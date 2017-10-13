context("sperrorest-resampling.R")

library(sperrorest)

# partition_cv() Mon Feb  6 21:57:08 2017 ------------------------------

test_that("partition_cv() output is of correct length", {

  skip_on_cran()

  data(ecuador)
  resamp <- partition_cv(ecuador, nfold = 5, repetition = 1:1)
  expect_equal(length(resamp[[1]]), 5)
})

# partition_cv.strat Mon Feb  6 21:58:20 2017 ------------------------------

test_that("partition_cv.strat() output is of correct length", {

  skip_on_cran()

  data(ecuador)
  parti <- partition_cv_strat(ecuador, strat = "slides", nfold = 5,
                              repetition = 1)
  expect_equal(length(parti[[1]]), 5)
})

# partition_factor() Mon Feb  6 22:01:05 2017 ------------------------------

test_that("partition_factor() output is of correct length", {

  skip_on_cran()

  data(ecuador)
  breaks <- quantile(ecuador$dem, seq(0, 1, length = 6))
  ecuador$zclass <- cut(ecuador$dem, breaks, include.lowest = TRUE)
  parti <- partition_factor(ecuador, fac = "zclass")
  expect_equal(length(parti[[1]]), 5)
})

# partition_factor_cv() Mon Feb  6 22:05:11 2017 ------------------------------

test_that("partition_factor_cv() output is of correct length", {

  skip_on_cran()

  breaks <- quantile(ecuador$dem, seq(0, 1, length = 6))
  ecuador$zclass <- cut(ecuador$dem, breaks, include.lowest = TRUE)
  parti <- partition_factor_cv(ecuador, fac = "zclass", nfold = 5)
  expect_equal(length(parti[[1]]), 5)
})

# partition_tiles() Mon Feb  6 22:15:15 2017 ------------------------------

test_that("partition_tiles() output is of correct length", {

  skip_on_cran()

  data(ecuador)
  parti <- partition_tiles(ecuador, nsplit = c(4, 2), reassign = FALSE)
  expect_equal(length(parti[[1]]), 8)
})

# partition_kmeans() Mon Feb  6 22:21:09 2017 ------------------------------

test_that("partition_kmeans() output is of correct length", {

  skip_on_cran()

  data(ecuador)
  resamp <- partition_kmeans(ecuador, nfold = 5, repetition = 1:1)
  expect_equal(length(resamp[[1]]), 5)
})

# partition_disc() Mon Feb  6 22:22:34 2017 ------------------------------

test_that("partition_disc() output is of correct length", {

  skip_on_cran()

  data(ecuador)
  parti <- partition_disc(ecuador, radius = 200, buffer = 200,
                          ndisc = 5, repetition = 1:1)
  expect_equal(length(parti[[1]]), 5)
})

# partition_loo() Mon Feb  6 22:22:34 2017 ------------------------------

test_that("partition_loo() output is of correct length", {

  skip_on_cran()

  data(ecuador)
  parti <- partition_loo(ecuador, buffer = 200,
                          ndisc = 5, repetition = 1:1)
  expect_equal(length(parti[[1]]), 5)
})

# represampling_bootstrap() Mon Feb  6 22:29:03 2017 ---------------------------

test_that("represampling_bootstrap() output is of correct length()", {

  skip_on_cran()

  data(ecuador)
  parti <- represampling_bootstrap(ecuador, repetition = 1)
  expect_equal(length(parti[[1]][[1]]), 2)
})


# represampling_factor_bootstrap() Mon Feb  6 22:36:10 2017 --------------------

test_that("represampling_factor_bootstrap() output is of correct length()", {

  skip_on_cran()

  data(ecuador)
  parti <- represampling_factor_bootstrap(ecuador,
                                          factor(floor(ecuador$dem / 100)),
                                          oob = TRUE)
  expect_equal(length(parti[[1]][[1]]), 2)
})

# represampling_tile_bootstrap() Mon Feb  6 22:39:26 2017 ----------------------

test_that("represampling_tile_bootstrap() output is of correct length()", {

  skip_on_cran()

  data(ecuador)
  parti <- represampling_tile_bootstrap(ecuador, nsplit = c(4, 2),
                                        reassign = FALSE)
  expect_equal(length(parti[[1]][[1]]), 2)
})

# represampling_kmeans_bootstrap() Mon Feb  6 22:44:53 2017 --------------------

test_that("represampling_kmeans_bootstrap() output is of correct length()", {

  skip_on_cran()

  data(ecuador)
  parti <- represampling_kmeans_bootstrap(ecuador, nsplit = c(4, 2),
                                          reassign = FALSE,
                                          nfold = 5)
  expect_equal(length(parti[[1]][[1]]), 2)
})

# represampling_disc_bootstrap() Mon Feb  6 22:52:28 2017 ----------------------

test_that("represampling_disc_bootstrap() output is of correct length()", {

  skip_on_cran()

  data(ecuador)
  parti <- represampling_disc_bootstrap(ecuador, radius = 200, nboot = 20,
                                        oob = FALSE)
  expect_equal(length(parti[[1]][[1]]), 2)
})

# plot.represampling() Mon Feb  6 22:54:18 2017 ------------------------------
# possible option to test base plots:
# http://stackoverflow.com/a/30286668/4185785

# test_that("plot.represampling is adjusting to repetitions", {
#   data(ecuador)
#   resamp <- partition_cv(ecuador, nfold = 1, repetition = 1:2)
#   p <- recordPlot()
#   plot(resamp, ecuador)
#   expect_equal(length(p[[1]][[1]]), 2)
# })
