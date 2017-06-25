library(testthat)
library(farsSimple)
context("make_filename")

test_that("Creation of a filename",{
  expect_equal(farsSimple::make_filename(2013), "accident_2013.csv.bz2")
})
