## ---- echo=FALSE, cache=FALSE--------------------------------------------
library(knitr)
opts_chunk$set(cache = TRUE,
               fig.align = "center",
               collapse = TRUE,
               fig.width = 7,
               fig.height = 5)
opts_knit$set(width = 125)

## ------------------------------------------------------------------------
res.lda.nsp <- res <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"), 
                                 model.fun = lda,
                                 pred.fun = lda.predfun, 
                                 pred.args = list(fac = "field"),
                                 smp.fun = partition.cv, 
                                 smp.args = list(repetition = 1:4, nfold = 5),
                                 err.rep = TRUE, err.fold = FALSE)
round(summary(res.lda.nsp$err.rep), 3)

## ------------------------------------------------------------------------
res.lda.sp <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"), 
                         model.fun = lda,
                         pred.fun = lda.predfun, 
                         pred.args = list(fac = "field"),
                         smp.fun = partition.factor.cv, 
                         smp.args = list(fac = "field", repetition = 1:4, nfold = 5),
                         err.rep = TRUE, err.fold = FALSE)
round(summary(res.lda.sp$err.rep), 3)

## ------------------------------------------------------------------------
res.rf.sp <- sperrorest(fo, data = maipo, coords = c("utmx","utmy"), 
                        model.fun = randomForest,
                        pred.fun = rf.predfun, 
                        pred.args = list(fac = "field"),
                        smp.fun = partition.factor.cv,
                        smp.args = list(fac = "field", 
                                        repetition = 1:4, nfold = 5),
                        err.rep = TRUE, err.fold = FALSE,
                        benchmark = TRUE, silent = FALSE)
res.rf.sp$benchmark

## ------------------------------------------------------------------------
res.rf.sp.par <- parsperrorest(fo, data = maipo, coords = c("utmx","utmy"), 
                               model.fun = randomForest,
                               pred.fun = rf.predfun, 
                               pred.args = list(fac = "field"),
                               smp.fun = partition.factor.cv,
                               smp.args = list(fac = "field", 
                                               repetition = 1:4, nfold = 5),
                               par.args = list(par.mode = 1, par.units = 2, 
                                               lb = T, high = TRUE),
                               err.rep = TRUE, err.fold = FALSE,
                               benchmark = TRUE, silent = FALSE)
res.rf.sp.par$benchmark

## ---- eval = FALSE-------------------------------------------------------
#  res.rf.sp.par1 <- sperrorest.par(fo, data = maipo, coords = c("utmx","utmy"),
#                                   model.fun = randomForest,
#                                   pred.fun = rf.predfun,
#                                   pred.args = list(fac = "field"),
#                                   smp.fun = partition.factor.cv,
#                                   smp.args = list(fac = "field",
#                                                   repetition = 1:4, nfold = 5),
#                                   par.units = 2, silent = 2,
#                                   err.fold = T, err.rep = T,
#                                   benchmark = TRUE)
#  res.rf.sp.par1$benchmark

