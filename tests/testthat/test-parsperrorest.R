test_that("check list output of rep and folds for par.mode = 3", { 
  
  lda.predfun <- function(object, newdata, fac = NULL) {
    pred <- predict(object, newdata = newdata)$class
    if (!is.null(fac)) pred <- majority.filter(pred, newdata[,fac]) 
    return(pred)
  }
  
  library(doParallel)
  library(foreach)
  library(sperrorest)
  
  # create majority
  library(nnet)
  majority <- function(x) {
    levels(x)[which.is.max(table(x))]
  }
  
  # create majority.filter
  majority.filter <- function(x, fac) {
    for (lev in levels(fac)) {
      x[ fac == lev ] <- majority(x[ fac == lev ])
    }
    x
  }
  
  formula <- croptype ~ b12 + b13 + b14 + b15 + b16 + b17 + b22 + b23 + b24 + 
    b25 + b26 + b27 + b32 + b33 + b34 + b35 + b36 + b37 + b42 + 
    b43 + b44 + b45 + b46 + b47 + b52 + b53 + b54 + b55 + b56 + 
    b57 + b62 + b63 + b64 + b65 + b66 + b67 + b72 + b73 + b74 + 
    b75 + b76 + b77 + b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 + 
    ndvi02 + ndvi03 + ndvi04 + ndvi05 + ndvi06 + ndvi07 + ndvi08 + 
    ndwi01 + ndwi02 + ndwi03 + ndwi04 + ndwi05 + ndwi06 + ndwi07 + 
    ndwi08
  
  # err.rep = TRUE, err.fold = TRUE
  out <- parsperrorest(fo, data = maipo, coords = c("utmx","utmy"), 
                       model.fun = lda,
                       pred.fun = lda.predfun,
                       smp.fun = partition.cv,
                       smp.args = list(repetition = 1:2, nfold = 5),
                       par.args = list(par.mode = 3, par.units = 2, 
                                       lb = F, high = F),
                       err.rep = TRUE, err.fold = TRUE,
                       benchmark = TRUE, verbose = FALSE)
  
  expect_equal(typeof(out$error.rep), "list")
  expect_equal(typeof(out$error.fold), "list")
  
  # err.rep = TRUE, err.fold = FALSE 
  out <- parsperrorest(fo, data = maipo, coords = c("utmx","utmy"), 
                       model.fun = lda,
                       pred.fun = lda.predfun,
                       smp.fun = partition.cv,
                       smp.args = list(repetition = 1:2, nfold = 5),
                       par.args = list(par.mode = 3, par.units = 2, 
                                       lb = F, high = F),
                       err.rep = TRUE, err.fold = FALSE,
                       benchmark = TRUE, verbose = FALSE)
  
  expect_equal(typeof(out$error.rep), "list")
  expect_equal(typeof(out$error.fold), "NULL")
  
  # err.rep = FALSE, err.fold = TRUE 
  out <- parsperrorest(fo, data = maipo, coords = c("utmx","utmy"), 
                       model.fun = lda,
                       pred.fun = lda.predfun,
                       smp.fun = partition.cv,
                       smp.args = list(repetition = 1:2, nfold = 5),
                       par.args = list(par.mode = 3, par.units = 2, 
                                       lb = F, high = F),
                       err.rep = FALSE, err.fold = TRUE,
                       benchmark = TRUE, verbose = FALSE)
  
  expect_equal(typeof(out$error.rep), "NULL")
  expect_equal(typeof(out$error.fold), "list")
}) 
