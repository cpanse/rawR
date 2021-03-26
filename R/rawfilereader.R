.rawfileReaderDLLs <- function(){
  c('ThermoFisher.CommonCore.BackgroundSubtraction.dll',
    'ThermoFisher.CommonCore.Data.dll',
    'ThermoFisher.CommonCore.MassPrecisionEstimator.dll',
    'ThermoFisher.CommonCore.RawFileReader.dll')
}

.userRawfileReaderDLLsPath <- function(){
  libdir <- tools::R_user_dir("rawrr", which='data')
  f <- file.path(libdir, 'rawrrassembly')
  return(f)
}

.checkRawfileReaderDLLs <- function(){
  monoPath <- Sys.getenv("MONO_PATH", names=TRUE)

  rv <- vapply(.rawfileReaderDLLs(), function(dll){
    ff <- file.path(.userRawfileReaderDLLsPath(), dll)
    if (monoPath != ""){
      ff <- file.path(monoPath, dll)
      if (isTRUE(ff)) return (TRUE)
    }
    file.exists(ff)
  }, FALSE)
  if (isFALSE(all(rv))){
    warning("'ThermoFisher.CommonCore.*.dll' files are not complete.\n",
            "Run 'installRawfileReaderDLLs()' or setenv MONO_PATH to the location",
            " where the assemblies are located.\n",
            "For more information, type '?ThermoFisher'.")
  }
  all(rv)
}



#' Install the New RawFileReader from Thermo Fisher Scientific
#'
#' @param ... other parameter for \code{download.file}
#' @param sourceUrl url of New RawFileReader from Thermo Fisher Scientific
#' assemblies
#'
#' @aliases ThermoFisher
#'
#' @seealso \url{https://planetorbitrap.com/rawfilereader}
#'
#' @return An (invisible) vector of integer code, 0 for success and non-zero for
#' failure. For the "wget" and "curl" methods this is the status code returned
#' by the external program.
#'
#' @export installRawfileReaderDLLs
#' @importFrom utils download.file
#'
installRawfileReaderDLLs <- function(sourceUrl = "https://github.com/compomics/ThermoRawFileParser/raw/master/packages/mzLib.1.0.450/lib/netstandard2.0/", ...){

  rawfileReaderDLLsPath <- .userRawfileReaderDLLsPath()
  if (isFALSE(dir.exists(rawfileReaderDLLsPath))){
    dir.create(rawfileReaderDLLsPath, recursive = TRUE)
  }

  vapply(.rawfileReaderDLLs(), function(dll){
    download.file(file.path(sourceUrl, dll),
                  destfile=file.path(rawfileReaderDLLsPath, dll), ...)},
    0)
}


.buildRawrrExe <- function(){
  packagedir <- system.file(package = 'rawrr')
  if (Sys.which("msbuild") == "" && Sys.which("xbuild") == "")
  {
    warning ("could not find msbuild or xbuild in path; will not be able to use rDotNet unless corrected and rebuilt")
    return()
  }

  cwd <- getwd()
  setwd(file.path(packagedir, 'rawrrassembly'))

  cmd <- ifelse(Sys.which("msbuild") != "", "msbuild", "xbuild")
  cmdArgs <- sprintf("/p:OutputPath='%s/'", dirname(.rawrrAssembly()))
  rv <- system2 (cmd, cmdArgs, wait=TRUE, stderr=TRUE, stdout=TRUE)


  if (rv <- any(grepl("Build succeeded.", rv)) && file.exists(.rawrrAssembly())){
    message(sprintf("rawrr.exe successfully built\n'%s'.",
                    dirname(.rawrrAssembly())))
  }else{
    warning("rawrr.exe build failed.")
  }
  setwd(cwd)
  rv
}


.isRawFileReaderLicenseAccepted <- function(){
  licenseFile <- file.path(system.file(package = 'rawrr'), 'rawrrassembly', 'RawFileReaderLicense.txt')
  stopifnot(file.exists(licenseFile))

  eulaFile <- file.path(cachedir <- tools::R_user_dir("rawrr", which='cache'), "eula.txt")
  msg <- "# By changing the setting below to TRUE you are accepting the Thermo License agreement."

  if (!file.exists(eulaFile)){
    file.show(licenseFile)
    fmt <- "Do you accept the Thermo License agreement '%s'? [Y/n]: "
    prompt <- sprintf(fmt, licenseFile)
    response <- readline(prompt = prompt)
    if (tolower(response) == "y"){
      if (!dir.exists(cachedir)) { dir.create(cachedir, recursive = TRUE) }
      fileConn <- file(eulaFile)
      writeLines(paste(msg, paste0("# ", date()), "eula=true", sep="\n"), fileConn)
      close(fileConn)

      return(TRUE %in% grepl("eula=true", tolower(readLines(eulaFile))))
    }
  }else{
    return(TRUE %in% grepl("eula=true", tolower(readLines(eulaFile))))
  }

  stop("You have to accept the Thermo License agreement!")

  FALSE
}

