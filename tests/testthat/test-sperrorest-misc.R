context("sperrorest-misc.R")

library(sperrorest)

# dataset_distance Sat Jun  3 14:41:05 2017 ------------------------------

test_that("dataset_distance works correctly", {
  df <- data.frame(x = rnorm(100), y = rnorm(100))
  out <- dataset_distance(df, df) # == 0
  expect_equal(out, 0)
})

# add.distance Sat Jun  3 14:45:34 2017 ------------------------------

test_that("add.distance works correctly", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  nsp_parti <- partition_cv(ecuador)
  nsp_parti <- add.distance(nsp_parti, ecuador)

  expect_length(nsp_parti[[1]], 10)
})
