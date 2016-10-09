## ---- echo=FALSE, cache=FALSE--------------------------------------------
library(knitr)
opts_chunk$set(cache = TRUE,
               fig.align = "center",
               collapse = TRUE,
               fig.width = 10,
               fig.height = 10)
opts_knit$set(width = 125)

## ------------------------------------------------------------------------
res.rf.sp.par1 <- sperrorest.par(fo, data = d, coords = c("utmx","utmy"), 
                               model.fun = randomForest,
                               pred.fun = rf.predfun, 
                               pred.args = list(fac = "field"),
                               smp.fun = partition.factor.cv, silent = 2, 
                               smp.args = list(fac = "field", repetition = 1:4, nfold = 5),
                               err.pooled = TRUE, err.unpooled = FALSE,
                               benchmark = TRUE)
res.rf.sp.par1$benchmark

