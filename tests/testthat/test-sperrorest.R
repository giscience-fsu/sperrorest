set.seed(42)

test_that("output type (= list) for different logical combinations of
          error_rep and error_fold for par_mode = 'foreach' on LDA example", {
  library("MASS")

  lda_predfun <- function(object, newdata, fac = NULL) {
    library(nnet)
    majority <- function(x) {
      levels(x)[which.is.max(table(x))]
    }

    majority_filter <- function(x, fac) {
      for (lev in levels(fac)) {
        x[fac == lev] <- majority(x[fac == lev])
      }
      x
    }

    pred <- predict(object, newdata = newdata)$class
    if (!is.null(fac)) pred <- majority_filter(pred, newdata[, fac])
    return(pred)
  }

  fo <- croptype ~ b12 + b13 + b14 + b15 + b16 + b17 + b22 + b23 +
    b24 +
    b25 + b26 + b27 + b32 + b33 + b34 + b35 + b36 + b37 + b42 +
    b43 + b44 + b45 + b46 + b47 + b52 + b53 + b54 + b55 + b56 +
    b57 + b62 + b63 + b64 + b65 + b66 + b67 + b72 + b73 + b74 +
    b75 + b76 + b77 + b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
    ndvi02 + ndvi03 + ndvi04 + ndvi05 + ndvi06 + ndvi07 + ndvi08 +
    ndwi01 + ndwi02 + ndwi03 + ndwi04 + ndwi05 + ndwi06 + ndwi07 +
    ndwi08

  data(maipo)

  out <- sperrorest(fo,
    data = maipo, coords = c("utmx", "utmy"),
    model_fun = lda,
    pred_fun = lda_predfun,
    smp_fun = partition_cv,
    smp_args = list(repetition = 1:2, nfold = 4),
    benchmark = TRUE, progress = FALSE
  )

  expect_equal(typeof(out$error_rep), "list")
  expect_equal(typeof(out$error_fold), "list")
  # check that train_error is first
  expect_equal(names(out$error_rep)[[1]], "train_error")
})

test_that("output length of list is correct for par_mode = 'foreach' on rpart
          example", {

            library("rpart")
  data(ecuador)
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  mypred_rpart <- function(object, newdata) {
    predict(
      object,
      newdata
    )[, 2]
  }
  ctrl <- rpart.control(cp = 0.005) # show the effects of overfitting
  fit <- rpart(fo, data = ecuador, control = ctrl)

  # Non-spatial 5-repeated 10-fold cross-validation:
  mypred_rpart <- function(object, newdata) {
    predict(
      object,
      newdata
    )[, 2]
  }

  out <- sperrorest(
    data = ecuador, formula = fo,
    model_fun = rpart,
    model_args = list(control = ctrl),
    pred_fun = mypred_rpart,
    progress = FALSE,
    smp_fun = partition_cv,
    smp_args = list(
      repetition = 1:2,
      nfold = 2
    )
  )

  expect_equal(length(out$error_fold[[1]]), 2)
})

# variable importance Wed Feb  8 21:59:03 2017

test_that("sperrorest() variable importance (foreach)", {

  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- suppressWarnings(
    sperrorest(
      data = ecuador, formula = fo,
      model_fun = glm,
      model_args = list(family = "binomial"),
      pred_fun = predict,
      pred_args = list(type = "response"),
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:1, nfold = 4),
      benchmark = TRUE,
      importance = TRUE, imp_permutations = 10,
      progress = FALSE
    )
  )
  expect_equal(class(out$importance[[1]][[1]]), "data.frame")
})


# binary response Wed Feb  8 22:43:12 2017

test_that("sperrorest() produces correct output for binary response", {

  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- suppressWarnings(
    sperrorest(
      data = ecuador, formula = fo,
      model_fun = glm, model_args = list(family = "binomial"),
      pred_fun = predict, pred_args = list(type = "response"),
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 2),
      benchmark = TRUE,
      importance = FALSE,
      imp_permutations = 2,
      progress = FALSE
    )
  )

  summary_rep <- summary(out$error_rep)
  summary_fold <- summary(out$error_fold)
  summary_resampling <- summary(out$represampling)
  # check for train_auroc for binary response
  expect_equal(names(out$error_rep)[[1]], "train_auroc")
})

test_that("sperrorest() when pred_fun = NULL", {

  data(ecuador) # Muenchow et al. (2012), see ?ecuador
  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  out <- suppressWarnings(
    sperrorest(
      data = ecuador, formula = fo,
      model_fun = glm, model_args = list(family = "binomial"),
      pred_args = list(type = "response"),
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 4),
      benchmark = TRUE,
      importance = TRUE,
      imp_permutations = 10,
      progress = FALSE
    )
  )
  summary_rep <- summary(out$error_rep)
  summary_fold <- summary(out$error_fold)
  summary_resampling <- summary(out$represampling)
  summary_impo <- summary(out$importance)
  # check for train_auroc for binary response
  expect_equal(names(out$error_rep)[[1]], "train_auroc")
  # check for importance object
  expect_equal(class(out$importance[[1]][[1]]), "data.frame")
})

test_that("sperrorest correctly updates resampling object when using a
          sub-sample", {
  data(ecuador)

  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope
  out <- suppressWarnings(
    sperrorest(
      data = ecuador, formula = fo,
      model_fun = glm,
      model_args = list(family = binomial),
      test_fun = resample_strat_uniform,
      test_param = list(strat = "slides", nstrat = Inf),
      train_fun = resample_strat_uniform,
      train_param = list(strat = "slides", nstrat = Inf),
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 4),
      importance = FALSE,
      progress = FALSE
    )
  )

  expect_lt(length(out[["represampling"]][["1"]][["1"]][["test"]]), 150)
})



test_that("sperrorest gets consistent results when changing some of the settings", {

  library("MASS")
  data(ecuador)

  mypredfun <- function(object, newdata, ...) {
    prd <- predict(object = object, newdata = newdata, ...)
    # prd$posterior[, "TRUE"]
    prd$class
  }

  set.seed(123)
  sli <- ecuador$slides == "FALSE"
  sel <- c(which(sli),
           sample(which(!sli), size = sum(sli)))
  d <- ecuador[sel, ]

  fo <- slides ~ dem + slope + hcurv + vcurv + log.carea + cslope

  # # If you want to check that it works:
  # fit <- lda(fo, d)
  # mypredfun(fit, d)

  myerr <- function(obs, pred) {
    list(
      error = mean(obs != pred),
      accuracy = mean(obs == pred)
    )
  }

  out1 <- suppressWarnings(
    sperrorest(
      data = d, formula = fo,
      model_fun = MASS::lda,
      pred_fun = mypredfun,
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 4, seed1 = 321),
      importance = FALSE
    )
  )
  smry1r <- summary(out1$error_rep)
  smry1f <- summary(out1$error_fold)
  out2 <- suppressWarnings(
    sperrorest(
      data = d, formula = fo,
      model_fun = MASS::lda,
      pred_fun = mypredfun,
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 4, seed1 = 321),
      importance = FALSE,
      err_fun = myerr
    )
  )
  smry2r <- summary(out2$error_rep)
  smry2f <- summary(out2$error_fold)

  out3 <- suppressWarnings(
    sperrorest(
      data = d, formula = fo,
      model_fun = MASS::lda,
      pred_fun = mypredfun,
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 4, seed1 = 321),
      importance = TRUE, imp_permutations = 10,
      progress = 0
    )
  )
  smry3r <- summary(out3$error_rep)
  smry3f <- summary(out3$error_fold)

  expect_equal(smry1r["test_error", "mean"], 0.4332669,
               tol = 0.03)
  # expect_equal(smry1r["test_error", "mean"], 0.4332669,
  #              tol = 0.000001)
  # expect_equal(smry1r["test_error", "sd"], 0.0014085792,
  #              tol = 0.000001)

  expect_equal(smry1r["test_error", "mean"],
               smry2r["test_error", "mean"])
  expect_equal(smry1r["test_error", "mean"],
               smry3r["test_error", "mean"])
  expect_equal(smry1r["test_error", "mean"],
               1 - smry2r["test_accuracy", "mean"])
  expect_equal(smry1r["test_error", "mean"],
               1 - smry3r["test_accuracy", "mean"])
  expect_equal(smry1r["train_error", "mean"],
               smry2r["train_error", "mean"])
  expect_equal(smry1r["test_count", "mean"],
               smry3r["test_count", "mean"])

  expect_equal(smry1f["test_error", "mean"],
               smry2f["test_error", "mean"])
  expect_equal(smry1f["test_error", "mean"],
               smry3f["test_error", "mean"])
  expect_equal(smry1f["test_error", "mean"],
               1 - smry2f["test_accuracy", "mean"])
  expect_equal(smry1f["test_error", "mean"],
               1 - smry3f["test_accuracy", "mean"])
  expect_equal(smry1f["train_error", "mean"],
               smry2f["train_error", "mean"])
  expect_equal(smry1f["test_count", "mean"],
               smry3f["test_count", "mean"])
})



test_that("sperrorest gets consistent results when changing some of the settings: regression problem", {

  requireNamespace("sp", quietly = TRUE)
  data("meuse", package = "sp")

  set.seed(123)

  d <- meuse
  d$logZn <- log10(d$zinc)
  d$sqrt.dist <- sqrt(d$dist)
  fo <- logZn ~ sqrt.dist + elev

  # # If you want to check that it works:
  # fit <- lm(fo, d)
  # predict(fit, d)

  myerr <- function(obs, pred) {
    list(
      bias = mean(obs - pred),
      rmse = sqrt(mean((obs - pred)^2))
    )
  }

  out1 <- suppressWarnings(
    sperrorest(
      data = d, formula = fo,
      model_fun = lm,
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 4, seed1 = 321),
      importance = FALSE
    )
  )
  smry1r <- summary(out1$error_rep)
  smry1f <- summary(out1$error_fold)
  out2 <- suppressWarnings(
    sperrorest(
      data = d, formula = fo,
      model_fun = lm,
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 4, seed1 = 321),
      importance = FALSE,
      err_fun = myerr
    )
  )
  smry2r <- summary(out2$error_rep)
  smry2f <- summary(out2$error_fold)

  out3 <- suppressWarnings(
    sperrorest(
      data = d, formula = fo,
      model_fun = lm,
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 4, seed1 = 321),
      importance = TRUE, imp_permutations = 10,
      progress = 0
    )
  )
  smry3r <- summary(out3$error_rep)
  smry3f <- summary(out3$error_fold)

  expect_equal(smry1r["test_rmse", "mean"], 0.1695412,
               tol = 0.02)
  # expect_equal(smry1r["test_rmse", "mean"], 0.1695412,
  #              tol = 0.000001)
  # expect_equal(smry1r["test_rmse", "sd"], 0.0006948571,
  #              tol = 0.000001)

  expect_equal(smry1r["test_rmse", "mean"],
               smry2r["test_rmse", "mean"])
  expect_equal(smry1r["test_rmse", "mean"],
               smry3r["test_rmse", "mean"])
  expect_equal(smry1r["test_bias", "mean"],
               smry2r["test_bias", "mean"])
  expect_equal(smry1r["test_bias", "mean"],
               smry3r["test_bias", "mean"])
  expect_equal(smry1r["train_rmse", "mean"],
               smry2r["train_rmse", "mean"])
  expect_equal(smry1r["test_count", "mean"],
               smry3r["test_count", "mean"])

  expect_equal(smry1f["test_rmse", "mean"],
               smry2f["test_rmse", "mean"])
  expect_equal(smry1f["test_rmse", "mean"],
               smry3f["test_rmse", "mean"])
  expect_equal(smry1f["test_bias", "mean"],
               smry2f["test_bias", "mean"])
  expect_equal(smry1f["test_bias", "mean"],
               smry3f["test_bias", "mean"])
  expect_equal(smry1f["train_rmse", "mean"],
               smry2f["train_rmse", "mean"])
  expect_equal(smry1f["test_count", "mean"],
               smry3f["test_count", "mean"])
})


test_that("sperrorest gets consistent results regardless of parallelization settings: regression problem", {

  requireNamespace("future", quietly = TRUE)
  requireNamespace("future.apply", quietly = TRUE)

  requireNamespace("sp", quietly = TRUE)
  data("meuse", package = "sp")

  set.seed(123)

  d <- meuse
  d$logZn <- log10(d$zinc)
  d$sqrt.dist <- sqrt(d$dist)
  fo <- logZn ~ sqrt.dist + elev

  out1 <- suppressWarnings(
    sperrorest(
      data = d, formula = fo,
      model_fun = lm,
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 4, seed1 = 321),
      mode_rep = "loop",
      mode_fold = "loop",
      importance = FALSE
    )
  )
  smry1r <- summary(out1$error_rep)
  smry1f <- summary(out1$error_fold)

  out2 <- suppressWarnings(
    sperrorest(
      data = d, formula = fo,
      model_fun = lm,
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 4, seed1 = 321),
      mode_rep = "sequential",
      mode_fold = "future",
      importance = FALSE
    )
  )
  smry2r <- summary(out2$error_rep)
  smry2f <- summary(out2$error_fold)

  out3 <- suppressWarnings(
    sperrorest(
      data = d, formula = fo,
      model_fun = lm,
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 4, seed1 = 321),
      mode_rep = "future",
      mode_fold = "sequential",
      importance = FALSE
    )
  )
  smry3r <- summary(out3$error_rep)
  smry3f <- summary(out3$error_fold)

  out4 <- suppressWarnings(
    sperrorest(
      data = d, formula = fo,
      model_fun = lm,
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 4, seed1 = 321),
      mode_rep = "sequential",
      mode_fold = "sequential",
      importance = FALSE
    )
  )
  smry4r <- summary(out4$error_rep)
  smry4f <- summary(out4$error_fold)

  expect_equal(smry1r["test_rmse", "mean"],
               smry2r["test_rmse", "mean"])
  expect_equal(smry1r["test_rmse", "mean"],
               smry3r["test_rmse", "mean"])
  expect_equal(smry1r["test_rmse", "mean"],
               smry4r["test_rmse", "mean"])

  expect_equal(smry1f["test_rmse", "mean"],
               smry2f["test_rmse", "mean"])
  expect_equal(smry1f["test_rmse", "mean"],
               smry3f["test_rmse", "mean"])
  expect_equal(smry1f["test_rmse", "mean"],
               smry4f["test_rmse", "mean"])
})



test_that("sperrorest is able to handle formulas without predictors", {

  requireNamespace("sp", quietly = TRUE)
  data("meuse", package = "sp")

  set.seed(123)

  d <- meuse
  d$logZn <- log10(d$zinc)
  d$sqrt.dist <- sqrt(d$dist)
  fo <- logZn ~ 1
  fo0 <- logZn ~ sqrt.dist + elev

  # model uses none of the predictors:
  lm0 <- function(formula, data, ...) {
    formula <- as.formula(paste(all.vars(formula)[1], "~ 1"))
    lm(formula = formula, data = data, ...)
  }

  # LM with intercept only:
  out1 <- suppressWarnings(
    sperrorest(
      data = d, formula = fo,
      model_fun = lm,
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 4, seed1 = 321),
      importance = FALSE
    )
  )
  smry1r <- summary(out1$error_rep)

  # 'fake' LM that ignores predictors, uses only intercept:
  out2 <- suppressWarnings(
    sperrorest(
      data = d, formula = fo0,
      model_fun = lm0,
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 4, seed1 = 321),
      importance = FALSE
    )
  )
  smry2r <- summary(out2$error_rep)

  expect_equal(smry1r["test_rmse", "mean"],
               smry2r["test_rmse", "mean"])
})



test_that("sperrorest is able to handle formulas with only one predictor", {
  requireNamespace("sp", quietly = TRUE)
  data("meuse", package = "sp")

  set.seed(123)

  d <- meuse
  d$logZn <- log10(d$zinc)
  d$sqrt.dist <- sqrt(d$dist)
  fo <- logZn ~ sqrt.dist
  fo1 <- logZn ~ sqrt.dist + elev

  # model uses only first predictor:
  lm1 <- function(formula, data, ...) {
    formula <- as.formula(paste(all.vars(formula)[1], "~",
                                all.vars(formula)[2]))
    lm(formula = formula, data = data, ...)
  }

  out1 <- suppressWarnings(
    sperrorest(
      data = d, formula = fo,
      model_fun = lm,
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 4, seed1 = 321),
      importance = TRUE, imp_permutations = 10
    )
  )
  smry1r <- summary(out1$error_rep)
  smry1i <- summary(out1$importance)

  out2 <- suppressWarnings(
    sperrorest(
      data = d, formula = fo1,
      model_fun = lm1,
      smp_fun = partition_cv,
      smp_args = list(repetition = 1:2, nfold = 4, seed1 = 321),
      importance = TRUE, imp_permutations = 10
    )
  )
  smry2r <- summary(out2$error_rep)
  smry2i <- summary(out2$importance)

  expect_equal(smry1r["test_rmse", "mean"],
               smry2r["test_rmse", "mean"])
  expect_equal(smry1i["sqrt.dist", "mean.rmse"],
               smry2i["sqrt.dist", "mean.rmse"])
})
