context("sparse vectors")
library(rawR)
library(Matrix)

test_that("conversion to sV works for sample data, scan 1", {
  S <- readSpectrum(rawfile = file.path(path.package(package = 'rawR'), 'extdata', 'sample.raw'), scan = 1)
  expect_s4_class(as_sparseVector(S[[1]], vType = "sV"), "dsparseVector")
  expect_s4_class(as_sparseVector(S[[1]], vType = "scV"), "dgCMatrix")
  expect_s4_class(as_sparseVector(S[[1]], vType = "srV"), "dgCMatrix")
})

test_that("conversion to sV works for all MS scans in sample data", {
  I <- readIndex(rawfile = file.path(path.package(package = 'rawR'), 'extdata', 'sample.raw'))
  S <- readSpectrum(rawfile = file.path(path.package(package = 'rawR'), 'extdata', 'sample.raw'), scan = 1:573)
  l <- lapply((S[I[I$MSOrder == "Ms", "scan"]]), as_sparseVector)
  expect_length(l, 27)
})

test_that("conversion to sV works with example_2 data", {
  expect_s4_class(as_sparseVector(rawRspectrum(sim = "example_2")), "dsparseVector")
  sV <- as_sparseVector(rawRspectrum(sim = "example_2"), mzBinRange = c(500, 800))
  expect_s4_class(sV, "dsparseVector")
  expect_length(sV, 301)
  expect_equal(sum(sV), 400)
  
})