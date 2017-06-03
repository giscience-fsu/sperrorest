context("sperrorest-misc.R")

pacman::p_load(sperrorest)

# dataset_distance Sat Jun  3 14:41:05 2017 ------------------------------

test_that("dataset_distance works correctly", {
  df <- data.frame(x = rnorm(100), y = rnorm(100))
  out <- dataset_distance(df, df) # == 0
  expect_equal(out, 0)
})
