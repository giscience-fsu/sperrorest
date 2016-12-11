## ---- echo=FALSE, cache=FALSE----------------------------------------------------------------------------------------------
library(knitr)
opts_chunk$set(cache = TRUE,
               eval = TRUE, 
               fig.align = "center",
               collapse = TRUE,
               fig.width = 7,
               fig.height = 5)
opts_knit$set(width = 125)

## ---- message=FALSE--------------------------------------------------------------------------------------------------------
library(rpart)
library(MASS)
library(ipred)
library(sperrorest)

## --------------------------------------------------------------------------------------------------------------------------
data("maipo", package = "sperrorest")

## --------------------------------------------------------------------------------------------------------------------------
predictors <- colnames(maipo)[5:ncol(maipo)]
# Construct a formula:
fo <- as.formula(paste("croptype ~", paste(predictors, collapse = "+")))

## --------------------------------------------------------------------------------------------------------------------------
co <- abs(cor(maipo[, predictors]) * 100)
co[co < 70] <- 0 # mask out weak/moderate correlations
head(round(co))

## --------------------------------------------------------------------------------------------------------------------------
fit <- MASS::lda(fo, data = maipo)

## --------------------------------------------------------------------------------------------------------------------------
pred <- predict(fit, newdata = maipo)$class
mean(pred != maipo$croptype)

## --------------------------------------------------------------------------------------------------------------------------
table(pred = pred, obs = maipo$croptype)

## --------------------------------------------------------------------------------------------------------------------------
fit <- rpart(fo, data = maipo)
#            control = rpart.control(cp=0.01))

## optional: view the classiciation tree
# par(xpd = TRUE)
# plot(fit)
# text(fit, use.n = TRUE)

## --------------------------------------------------------------------------------------------------------------------------
pred <- predict(fit, newdata = maipo, type = "class")
mean(pred != maipo$croptype)

## --------------------------------------------------------------------------------------------------------------------------
table(pred = pred, obs = maipo$croptype)

## --------------------------------------------------------------------------------------------------------------------------
summary(pred)
summary(maipo$croptype)

## --------------------------------------------------------------------------------------------------------------------------
fit <- bagging(fo, data = maipo, coob = TRUE)
fit

## --------------------------------------------------------------------------------------------------------------------------
pred <- predict(fit, newdata = maipo, type = "class")
mean(pred != maipo$croptype)

## --------------------------------------------------------------------------------------------------------------------------
table(pred = pred, obs = maipo$croptype)

## --------------------------------------------------------------------------------------------------------------------------
lda.predfun <- function(object, newdata, fac = NULL) {
  library(nnet)
  majority <- function(x) {
    levels(x)[which.is.max(table(x))]
  }
  
  majority.filter <- function(x, fac) {
    for (lev in levels(fac)) {
      x[ fac == lev ] <- majority(x[ fac == lev ])
    }
    x
  }
  
  pred <- predict(object, newdata = newdata)$class
  if (!is.null(fac)) pred <- majority.filter(pred, newdata[,fac]) 
  return(pred)
}

## --------------------------------------------------------------------------------------------------------------------------
res.lda.nsp <- res <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"), 
                                 model.fun = lda,
                                 pred.fun = lda.predfun, 
                                 pred.args = list(fac = "field"),
                                 smp.fun = partition.cv, 
                                 smp.args = list(repetition = 1:6, nfold = 5),
                                 error.rep = TRUE, error.fold = FALSE)
round(summary(res.lda.nsp$error.rep), 3)

## --------------------------------------------------------------------------------------------------------------------------
res.lda.sp <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"), 
                         model.fun = lda,
                         pred.fun = lda.predfun, 
                         pred.args = list(fac = "field"),
                         smp.fun = partition.factor.cv,
                         smp.args = list(fac = "field", repetition = 1:6, nfold = 5),
                         error.rep = TRUE, error.fold = FALSE, 
                         benchmark = TRUE)
res.lda.sp$benchmark$runtime.performance

## --------------------------------------------------------------------------------------------------------------------------
res.lda.sp.par <- parsperrorest(fo, data = maipo, coords = c("utmx","utmy"), 
                                model.fun = lda,
                                pred.fun = lda.predfun, 
                                pred.args = list(fac = "field"),
                                smp.fun = partition.factor.cv,
                                smp.args = list(fac = "field", repetition = 1:6, nfold = 5),
                                par.args = list(par.units = 2, par.mode = 2),
                                error.rep = TRUE, error.fold = TRUE,
                                benchmark = TRUE)
res.lda.sp.par$benchmark$runtime.performance

## --------------------------------------------------------------------------------------------------------------------------
res.lda.par <- parsperrorest(fo, data = maipo, coords = c("utmx","utmy"), 
                               model.fun = lda,
                               pred.fun = lda.predfun, 
                               pred.args = list(fac = "field"),
                               smp.fun = partition.cv,
                               smp.args = list(repetition = 1:6, nfold = 5),
                               par.args = list(par.units = 2, par.mode = 2),
                               error.rep = TRUE, error.fold = TRUE,
                               benchmark = TRUE)
res.lda.sp.par$benchmark$runtime.performance

## --------------------------------------------------------------------------------------------------------------------------
res.bagg.sp <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"), 
                          model.fun = bagging,
                          pred.args = list(fac = "field"),
                          smp.fun = partition.factor.cv,
                          smp.args = list(fac = "field",
                                          repetition = 1:6, nfold = 5),
                          error.rep = TRUE, error.fold = TRUE,
                          benchmark = TRUE, verbose = "rep")
res.bagg.sp$benchmark$runtime.performance

## --------------------------------------------------------------------------------------------------------------------------
res.bagg.sp.par <- parsperrorest(fo, data = maipo, coords = c("utmx","utmy"), 
                                 model.fun = bagging,
                                 pred.args = list(fac = "field"),
                                 smp.fun = partition.factor.cv,
                                 smp.args = list(fac = "field", 
                                                 repetition = 1:6, nfold = 5),
                                 par.args = list(par.units = 2, par.mode = 2),
                                 error.rep = TRUE, error.fold = TRUE,
                                 benchmark = TRUE)
res.bagg.sp.par$benchmarks$runtime.performance

