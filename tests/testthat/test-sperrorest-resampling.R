context("sperrorest-resampling.R")

pacman::p_load(sperrorest, testthat)

# partition.cv() Mon Feb  6 21:57:08 2017 ------------------------------ 

test_that("partition.cv() output is of correct length", {
  data(ecuador)
  resamp <- partition.cv(ecuador, nfold = 5, repetition = 1:1)
  expect_equal(length(resamp[[1]]), 5)
})

# partition.cv.strat Mon Feb  6 21:58:20 2017 ------------------------------

test_that("partition.cv.strat() output is of correct length", {
  data(ecuador)
  parti <- partition.cv.strat(ecuador, strat = 'slides', nfold = 5, repetition = 1)
  expect_equal(length(parti[[1]]), 5)
})

# partition.factor() Mon Feb  6 22:01:05 2017 ------------------------------

test_that("partition.factor() output is of correct length", { 
  data(ecuador)
  breaks <- quantile(ecuador$dem, seq(0, 1, length = 6))
  ecuador$zclass <- cut(ecuador$dem, breaks, include.lowest = TRUE)
  parti <- partition.factor(ecuador, fac = 'zclass')
  expect_equal(length(parti[[1]]), 5)
})  

# partition.factor.cv() Mon Feb  6 22:05:11 2017 ------------------------------

test_that("partition.factor.cv() output is of correct length", {
  breaks <- quantile(ecuador$dem, seq(0, 1, length = 6))
  ecuador$zclass <- cut(ecuador$dem, breaks, include.lowest = TRUE)
  parti <- partition.factor.cv(ecuador, fac = 'zclass', nfold = 5)
  expect_equal(length(parti[[1]]), 5)
})

# partition.tiles() Mon Feb  6 22:15:15 2017 ------------------------------

test_that("partition.tiles() output is of correct length", {
  data(ecuador)
  parti <- partition.tiles(ecuador, nsplit = c(4,2), reassign = FALSE)
  expect_equal(length(parti[[1]]), 8)
})

# partition.kmeans() Mon Feb  6 22:21:09 2017 ------------------------------

test_that("partition.kmeans() output is of correct length", {
  data(ecuador)
  resamp <- partition.kmeans(ecuador, nfold = 5, repetition = 1:1)
  expect_equal(length(resamp[[1]]), 5)
})

# partition.disc() Mon Feb  6 22:22:34 2017 ------------------------------

test_that("partition.disc() output is of correct length", {
  data(ecuador)
  parti <- partition.disc(ecuador, radius = 200, buffer = 200, 
                          ndisc = 5, repetition = 1:1)
  expect_equal(length(parti[[1]]), 5)
})

# partition.loo() Mon Feb  6 22:22:34 2017 ------------------------------

test_that("partition.loo() output is of correct length", {
  data(ecuador)
  parti <- partition.loo(ecuador, buffer = 200, 
                          ndisc = 5, repetition = 1:1)
  expect_equal(length(parti[[1]]), 5)
})

# represampling.bootstrap() Mon Feb  6 22:29:03 2017 ------------------------------

test_that("represampling.bootstrap() output is of correct length()", {
  data(ecuador)
  parti <- represampling.bootstrap(ecuador, repetition = 1)
  expect_equal(length(parti[[1]][[1]]), 2)
})


# represampling.factor.bootstrap() Mon Feb  6 22:36:10 2017 ------------------------------

test_that("represampling.factor.bootstrap() output is of correct length()", {
  data(ecuador)
  parti <- represampling.factor.bootstrap(ecuador, 
                                          factor(floor(ecuador$dem / 100)), 
                                          oob = TRUE)
  expect_equal(length(parti[[1]][[1]]), 2)
})

# represampling.tile.bootstrap() Mon Feb  6 22:39:26 2017 ------------------------------

test_that("represampling.tile.bootstrap() output is of correct length()", {
  data(ecuador)
  parti <- represampling.tile.bootstrap(ecuador, nsplit = c(4,2), reassign = FALSE)
  expect_equal(length(parti[[1]][[1]]), 2)
})

# represampling.kmeans.bootstrap() Mon Feb  6 22:44:53 2017 ------------------------------

test_that("represampling.kmeans.bootstrap() output is of correct length()", {
  data(ecuador)
  parti <- represampling.kmeans.bootstrap(ecuador, nsplit = c(4,2), reassign = FALSE,
                                          nfold = 5)
  expect_equal(length(parti[[1]][[1]]), 2)
})

# represampling.disc.bootstrap() Mon Feb  6 22:52:28 2017 ------------------------------

test_that("represampling.disc.bootstrap() output is of correct length()", {
  data(ecuador)
  parti <- represampling.disc.bootstrap(ecuador, radius = 200, nboot = 20, oob = FALSE)
  expect_equal(length(parti[[1]][[1]]), 2)
})

# plot.represampling() Mon Feb  6 22:54:18 2017 ------------------------------
### possible option to test base plots: http://stackoverflow.com/a/30286668/4185785

# test_that("plot.represampling is adjusting to repetitions", {
#   data(ecuador)
#   resamp <- partition.cv(ecuador, nfold = 1, repetition = 1:2)
#   p <- recordPlot()
#   plot(resamp, ecuador)
#   expect_equal(length(p[[1]][[1]]), 2)
# })


