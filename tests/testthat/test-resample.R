context("resample_R")

pacman::p_load(sperrorest, testthat)

# resample_strat.uniform() Tue Feb 14 22:04:12 2017 ----------------------------

test_that("resample_start.uniform() produces correct output", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  d <- resample_strat_uniform(ecuador, 
                              param = list(strat = 'slides', nstrat = 100))
  expect_equal(nrow(d), 200) # == 200
  expect_equal(sum(d$slides == 'TRUE'), 100) # == 100
})


# resample_uniform() Tue Feb 14 22:07:03 2017 ------------------------------

test_that("resample_uniform() produces correct output", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  d <- resample_uniform(ecuador, param = list(strat = 'slides', n = 400))
  expect_equal(nrow(d), 400) # == 400
})

# resample_factor() Tue Feb 14 22:11:14 2017 ------------------------------

test_that("resample_uniform() produces correct output", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  d <- resample_uniform(ecuador, param = list(strat = 'slides', n = 200))
  expect_equal(nrow(d), 200) # == 400
})
