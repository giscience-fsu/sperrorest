context("sperrorest.R")

pacman::p_load(sperrorest, rpart, testthat)

# sperrorest() binary response Wed Feb  8 21:40:49 2017 ------------------------------

test_that("sperrorest() produces correct output for binary response", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                       model.fun = glm, model.args = list(family = "binomial"),
                       pred.fun = predict, pred.args = list(type = "response"),
                       smp.fun = partition.cv, 
                       smp.args = list(repetition = 1:2, nfold = 2),
                       notify = TRUE, benchmark = TRUE,
                       importance = TRUE, imp.permutations = 2)
  summary.rep <- summary(nspres$error.rep)                    
  summary.fold <- summary(nspres$error.fold)
  summary.resampling <- summary(nspres$represampling)
  
  expect_equal(length(nspres$error.rep[[1]]), 2) # reps
  expect_equal(length(nspres$error.fold[[1]]), 2) # folds
  expect_equal(length(summary.rep), 4) # binary response
  expect_equal(length(summary.fold), 4) # binary response
  expect_equal(length(summary.resampling), 2) # resampling summary
  expect_equal(length(nspres$importance[[1]]), 2) # import folds
  expect_equal(length(nspres$importance), 2) # import reps
  expect_equal(names(nspres$error.rep)[[1]], "train.auroc") # check for auroc existence
})


# sperorrest() continuous response Wed Feb  8 22:19:57 2017 ------------------------------

test_that("sperrorest() produces correct output for binary response", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                       model.fun = glm,
                       pred.fun = predict,
                       smp.fun = partition.cv, 
                       smp.args = list(repetition = 1:2, nfold = 2),
                       notify = TRUE, benchmark = TRUE, 
                       importance = TRUE, imp.permutations = 2)
  summary.rep <- summary(nspres$error.rep)                    
  summary.fold <- summary(nspres$error.fold)
  summary.resampling <- summary(nspres$represampling)
  
  expect_equal(length(nspres$error.rep[[1]]), 2) # reps
  expect_equal(length(nspres$error.fold[[1]]), 2) # folds
  expect_equal(length(summary.rep), 4) # binary response
  expect_equal(length(summary.fold), 4) # binary response
  expect_equal(length(summary.resampling), 2) # resampling summary
  expect_equal(length(nspres$importance[[1]]), 2) # import folds
  expect_equal(length(nspres$importance), 2) # import reps
  expect_equal(names(nspres$error.rep)[[1]], "train.bias") # check for bias existence
})

# pred.fun = NULL response Wed Feb  8 22:19:57 2017 ------------------------------

test_that("sperrorest() produces correct output for binary response", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                       model.fun = glm,
                       smp.fun = partition.cv, 
                       smp.args = list(repetition = 1:2, nfold = 2),
                       importance = TRUE, imp.permutations = 2)
  
  expect_equal(length(nspres$error.rep[[1]]), 2) # reps
})


# summary.sperroresterror() Thu Feb  9 22:10:15 2017 ------------------------------

test_that("summary.sperroresterror() produces correct output for binary response", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                       model.fun = glm,
                       pred.fun = predict,
                       smp.fun = partition.cv, 
                       smp.args = list(repetition = 1:2, nfold = 2))
  
  summary.rep1 <- summary(nspres$error.rep, pooled = FALSE)                    
  summary.fold1 <- summary(nspres$error.fold, pooled = FALSE)
  summary.rep <- summary(nspres$error.rep, pooled = TRUE)                    
  summary.fold <- summary(nspres$error.fold, pooled = TRUE)
  
  expect_equal(length(summary.rep), 4) # binary response
  expect_equal(length(summary.fold), 4) # binary response
})

# summary.sperrorestimportance() Thu Feb  9 22:17:15 2017 ------------------------------

test_that("summary.sperroresterror() with pooled = FALSE produces correct output for binary response", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                       model.fun = glm,
                       pred.fun = predict,
                       smp.fun = partition.cv, 
                       smp.args = list(repetition = 1:2, nfold = 2),
                       importance = TRUE, imp.permutations = 2,
                       do.try = TRUE)
  
  summary.imp <- summary(nspres$importance)        
  
  expect_equal(length(summary.imp), 28)
})

# sperrorest warnings Thu Feb  9 22:34:08 2017 ------------------------------

test_that("importance = T and err.fold = F", { 
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope
  
  expect_warning(sperrorest(data = ecuador, formula = fo,
                            model.fun = glm,
                            pred.fun = predict,
                            smp.fun = partition.cv, 
                            smp.args = list(repetition = 1:2, nfold = 2),
                            importance = TRUE, error.fold = FALSE))
  
  expect_warning(sperrorest(data = ecuador, formula = fo,
                          model.fun = glm,
                          pred.fun = predict,
                          smp.fun = partition.cv, 
                          smp.args = list(repetition = 1:2, nfold = 2),
                          someargument = NULL))
})

# sperrorest depr. args Thu Feb  9 22:42:48 2017 ------------------------------

test_that("deprecated args", { 
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope
  
  expect_error(sperrorest(data = ecuador, formula = fo,
                          model.fun = glm,
                          pred.fun = predict,
                          smp.fun = partition.cv, 
                          smp.args = list(repetition = 1:2, nfold = 2),
                          predfun = NULL))
  
  expect_error(sperrorest(data = ecuador, formula = fo,
                          model.fun = glm,
                          pred.fun = predict,
                          smp.fun = partition.cv, 
                          smp.args = list(repetition = 1:2, nfold = 2),
                          silent = NULL))
  
  expect_error(sperrorest(data = ecuador, formula = fo,
                          model.fun = glm,
                          pred.fun = predict,
                          smp.fun = partition.cv, 
                          smp.args = list(repetition = 1:2, nfold = 2),
                          err.pooled = NULL))
  expect_error(sperrorest(data = ecuador, formula = fo,
                          model.fun = glm,
                          pred.fun = predict,
                          smp.fun = partition.cv, 
                          smp.args = list(repetition = 1:2, nfold = 2),
                          err.unpooled = NULL))
})

# sperrorest() various non default arguments Fri Feb 10 19:09:01 2017 ------------------------------

# does not work with err.train = F and importance = T

test_that("sperrorest() produces correct output for binary response for non-default arguments", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  
  nspres <- sperrorest(data = ecuador, formula = fo,
                       model.fun = glm, model.args = list(family = "binomial"),
                       pred.fun = predict, pred.args = list(type = "response"),
                       smp.fun = partition.cv, 
                       smp.args = list(repetition = 1:2, nfold = 2),
                       notify = F, benchmark = F,
                       importance = TRUE, imp.permutations = 2,
                       do.try = TRUE, err.train = T, do.gc = 2)
  
  expect_equal(length(nspres$error.rep[[1]]), 2) # reps
  expect_equal(length(nspres$error.fold[[1]]), 2) # folds
  expect_equal(length(nspres$importance[[1]]), 2) # import folds
  expect_equal(length(nspres$importance), 2) # import reps
})


# summary.sperrorest() Sun Feb 12 11:56:13 2017 ------------------------------

test_that("summary.sperrorest() works correctly", {
  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slope ~ hcurv + vcurv + log.carea + cslope
  
  out <- sperrorest(data = ecuador, formula = fo,
                    model.fun = glm,
                    pred.fun = predict,
                    smp.fun = partition.cv, 
                    smp.args = list(repetition = 1:2, nfold = 2),
                    importance = T, imp.permutations = 2,
                    error.fold = T, 
                    benchmark = T)
  
  smry.out <- summary(out)
  
  expect_equal(length(smry.out), 6)
})
