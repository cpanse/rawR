context("sparse vectors")
library(rawR)
library(Matrix)

S <- readSpectrum(rawfile = sampleData(), scan = 1:573)
I <- readIndex(rawfile = sampleData())

test_that("conversion to sV works for sample data, scan 1", {
  expect_s4_class(as_sparseVector(S[[1]], vType = "sV"), "dsparseVector")
  #expect_s4_class(as_sparseVector(S[[1]], vType = "scV"), "dgCMatrix")
  expect_s4_class(as_sparseVector(S[[1]], vType = "scV"), "dgTMatrix")
  #expect_s4_class(as_sparseVector(S[[1]], vType = "srV"), "dgTMatrix")
})

test_that("conversion to sV works for all MS scans in sample data", {
  l <- lapply((S[I[I$MSOrder == "Ms", "scan"]]), as_sparseVector)
  expect_length(l, 27)
})

test_that("conversion to sV works across all scans in sample data", {
  sVset <- lapply(S, as_sparseVector)
  expect_length(sVset, length(S))
})

test_that("conversion to sV works using fixed mzBinRange", {
  x <- 50
  y <- 2000
  sVset <- lapply(S, as_sparseVector, mzBinRange = c(x, y))
  l <- lapply(sVset, length)
  expect_length(sVset, length(S))
  expect_setequal(l, expected = rep(y-x+1, length(S)))
})


test_that("conversion to sV works with example_2 data", {
  expect_s4_class(as_sparseVector(rawRspectrum(sim = "example_2")), "dsparseVector")
  sV <- as_sparseVector(rawRspectrum(sim = "example_2"), mzBinRange = c(500, 800))
  expect_s4_class(sV, "dsparseVector")
  expect_length(sV, 301)
  expect_equal(sum(sV), 400)
})

test_that("conversion to sV works with example_2 data and aggresive S/N filtering", {
  sV <- as_sparseVector(rawRspectrum(sim = "example_2"), StoNcutoff = 10, mzBinRange = c(500, 800))
  expect_s4_class(sV, "dsparseVector")
  expect_length(sV, 301)
  expect_equal(sum(sV), 0)
})


test_that("error when using spectrum without centroided data", {
  expect_error(as_sparseVector(rawRspectrum(sim = "example_1")))
})

test_that("normDotProd = 1 for self comparision", {
  expect_equal(normDotProd(as_sparseVector(S[[1]]), as_sparseVector(S[[1]])), 1)
})

test_that("normDotProd returns values in [0,1]", {
  x <- 50
  y <- 2000
  sVset <- lapply(S, as_sparseVector, mzBinRange = c(x, y))
  z <- unlist(lapply(sVset, normDotProd, y = sVset[[1]]))
  expect_equal(min(z, na.rm = TRUE), 0)
  expect_equal(max(z, na.rm = TRUE), 1)
})

