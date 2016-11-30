test_that("check problems in RF example in vignettes", { 
  
  lda.predfun <- function(object, newdata, fac = NULL) {
    pred <- predict(object, newdata = newdata)$class
    if (!is.null(fac)) pred <- majority.filter(pred, newdata[,fac]) 
    return(pred)
  }
  
  library(doParallel)
  library(foreach)
  library(sperrorest)
  
  ### LDA
  data = maipo
  coords = c("utmx","utmy")
  model.fun = lda
  pred.fun = lda.predfun
  pred.args = list(fac = "field")
  smp.fun = partition.cv
  smp.args = list(repetition = 1:4, nfold = 5)
  par.args = list(par.mode = 3, par.units = 2, 
                  lb = FALSE, high = FALSE)
  err.rep = TRUE
  err.fold = FALSE
  err.train = TRUE
  importance = FALSE
  verbose = "all"
  progress = ""
  
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
  # create rf.predfun
  rf.predfun <- function(object, newdata, fac = NULL) {
    pred <- predict(object, newdata = newdata)
    if (!is.null(fac)) pred <- majority.filter(pred, newdata[,fac]) 
    return(pred)
  }
  
  formula <- croptype ~ b12 + b13 + b14 + b15 + b16 + b17 + b22 + b23 + b24 + 
    b25 + b26 + b27 + b32 + b33 + b34 + b35 + b36 + b37 + b42 + 
    b43 + b44 + b45 + b46 + b47 + b52 + b53 + b54 + b55 + b56 + 
    b57 + b62 + b63 + b64 + b65 + b66 + b67 + b72 + b73 + b74 + 
    b75 + b76 + b77 + b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 + 
    ndvi02 + ndvi03 + ndvi04 + ndvi05 + ndvi06 + ndvi07 + ndvi08 + 
    ndwi01 + ndwi02 + ndwi03 + ndwi04 + ndwi05 + ndwi06 + ndwi07 + 
    ndwi08
  
  # -------------- RF ----------------------------------------------------------
  data = maipo
  coords = c("utmx","utmy")
  model.fun = randomForest
  pred.fun = rf.predfun
  pred.args = list(fac = "field")
  smp.fun = partition.factor.cv
  smp.args = list(fac = "field", 
                  repetition = 1:4, nfold = 5)
  par.args = list(par.mode = 3, par.units = 2, 
                  lb = FALSE, high = FALSE)
  err.rep = TRUE
  err.fold = FALSE
  err.train = TRUE
  importance = FALSE
  verbose = "all"
  progress = ""
  
  # -------------- RF ----------------------------------------------------------
  
  # Name of response variable:
  response = as.character(attr(terms(formula),"variables"))[2]
  
  smp.args$data = data
  smp.args$coords = coords
  
  resamp = do.call(smp.fun, args = smp.args)
  if (distance)
    # Parallelize this function???
    resamp = add.distance(resamp, data, coords = coords, fun = mean)
  
  if (err.fold) {
    res = lapply(resamp, unclass)
    class(res) = "sperroresterror"
  } else res = NULL
  pooled.err = NULL
  # required to be able to assign levels to predictions if appropriate:
  is.factor.prediction = NULL
  
  ### Permutation-based variable importance assessment (optional):
  impo = NULL
  if (importance) {
    # Importance of which variables:
    if (is.null(imp.variables))
      imp.variables = strsplit(as.character(formula)[3]," + ",fixed = TRUE)[[1]]
    # Dummy data structure that will later be populated with the results:
    impo = resamp
    # Create a template that will contain results of variable importance 
    # assessment:
    imp.one.rep = as.list( rep(NA, length(imp.variables)) )
    names(imp.one.rep) = imp.variables
    tmp = as.list(rep(NA, imp.permutations))
    names(tmp) = as.character(1:imp.permutations)
    for (vnm in imp.variables) imp.one.rep[[vnm]] = tmp
    rm(tmp)
  }
    
    # suppress any progress output of workes if verbose = FALSE
    if (verbose == FALSE) {
      progress <- "/dev/null"
      if (Sys.info()["sysname"] == "Windows") {
        progress <- "nul:"
      } 
    }
    # special settings for Windows
    if (progress == "" & Sys.info()["sysname"] == "Windows")
      progress <- paste0(getwd(), "/sperrorest.progress.txt")
    
    cl <- makeCluster(par.args$par.units, outfile = progress)
    registerDoParallel(cl, cores = par.args$par.units)
    
    foreach.out <- foreach(i = 1:length(resamp), 
                           .packages = (.packages()), 
                           .errorhandling = "remove", 
                           .combine = rbind, .verbose = TRUE) %dopar% {
                             
                             # reset rep.err otherwise 
                             # duplicates are introduced
                             rep.err <- NULL
                             
                             if (verbose == "rep") {
                               cat(date(), "Repetition", 
                                   names(resamp)[i], "\n") 
                             }
                             if (err.train) {
                               pooled.obs.train = pooled.pred.train = c()
                               pooled.obs.test = pooled.pred.test = c()
                             }
                             for (j in 1:length(resamp[[i]])) {
                               if (verbose == "all") {
                                 cat(date(), "Repetition", 
                                     names(resamp)[i], "- Fold", j, "\n") 
                               }
                               nd <- data[resamp[[i]][[j]]$train, ]
                               if (!is.null(train.fun)) {
                                 nd <- train.fun(data = nd, param = train.param)
                               }
                               margs <- c(list(formula = formula, data = nd),
                                          model.args)
                               if (do.try) {
                                 fit <- try(do.call(model.fun, args = margs),
                                            silent = silent)
                                 if (class(fit) == "try-error") {
                                   fit <- NULL
                                   if (err.fold) {
                                     if (err.train) {
                                       res[[i]][[j]]$train <- NULL
                                     }
                                     res[[i]][[j]]$test <- NULL
                                     if (importance) {
                                       impo[[i]][[j]] = c()
                                     }
                                   }
                                   if (do.gc >= 2) {
                                     gc()
                                   }
                                   next
                                 }
                               }
                               else {
                                 fit <- do.call(model.fun, args = margs)
                               }
                               if (err.train) {
                                 pargs <- c(list(object = fit, newdata = nd),
                                            pred.args)
                                 if (is.null(pred.fun)) {
                                   pred.train <- do.call(predict, args = pargs)
                                 }
                                 else {
                                   pred.train <- do.call(pred.fun, 
                                                         args = pargs)
                                 }
                                 rm(pargs)
                                 if (err.fold) 
                                   if (do.try) {
                                     err.try <- try(err.fun(nd[, response],
                                                            pred.train), 
                                                    silent = silent)
                                     if (class(err.try) == "try-error") 
                                       err.try <- NULL
                                     res[[i]][[j]]$train <- err.try
                                   }
                                 else {
                                   res[[i]][[j]]$train <- err.fun(
                                     nd[, response], pred.train)
                                 }
                                 if (err.rep) {
                                   pooled.obs.train <- c(pooled.obs.train, 
                                                         nd[, response])
                                   pooled.pred.train <- c(pooled.pred.train,
                                                          pred.train)
                                 }
                               }
                               else {
                                 if (err.fold) {
                                   res[[i]][[j]]$train <- NULL
                                 }
                               }
                               nd <- data[resamp[[i]][[j]]$test, ]
                               if (!is.null(test.fun)) {
                                 nd <- test.fun(data = nd, param = test.param)
                               }
                               if (importance) {
                                 nd.bak <- nd
                               }
                               pargs <- c(list(object = fit, newdata = nd),
                                          pred.args)
                               if (is.null(pred.fun)) {
                                 pred.test <- do.call(predict, args = pargs)
                               }
                               else {
                                 pred.test <- do.call(pred.fun, args = pargs)
                               }
                               rm(pargs)
                               if (err.fold) {
                                 if (do.try) {
                                   err.try <- try(err.fun(
                                     nd[, response],pred.test), silent = silent)
                                   if (class(err.try) == "try-error") 
                                     err.try <- NULL
                                   res[[i]][[j]]$test <- err.try
                                 }
                                 else {
                                   res[[i]][[j]]$test <- err.fun(
                                     nd[, response], pred.test)
                                 }
                               }
                               if (err.rep) {
                                 pooled.obs.test <- c(pooled.obs.test, 
                                                      nd[, response])
                                 pooled.pred.test <- c(pooled.pred.test,
                                                       pred.test)
                                 is.factor.prediction <- is.factor(pred.test)
                               }
                               if (importance & err.fold) {
                                 if (is.null(res[[i]][[j]]$test)) {
                                   impo[[i]][[j]] <- c()
                                   if (verbose == "all" | verbose == "rep") 
                                     cat(date(), 
                                         "-- skipping variable importance\n")
                                 }
                                 else {
                                   if (verbose == "all" | verbose == "rep") {
                                     cat(date(), "-- Variable importance\n")
                                   }
                                   imp.temp <- imp.one.rep
                                   for (cnt in 1:imp.permutations) {
                                     if (verbose == "all" | verbose == "rep" & 
                                         (cnt > 1)) 
                                       if (log10(cnt) == floor(log10(cnt))) 
                                         cat(date(), "   ", cnt, "\n")
                                     permut <- sample(1:nrow(nd), 
                                                      replace = FALSE)
                                     for (vnm in imp.variables) {
                                       nd <- nd.bak
                                       nd[, vnm] <- nd[, vnm][permut]
                                       pargs <- c(list(object = fit, 
                                                       newdata = nd), 
                                                  pred.args)
                                       if (is.null(pred.fun)) {
                                         pred.test <- do.call(predict, 
                                                              args = pargs)
                                       }
                                       else {
                                         pred.test <- do.call(pred.fun, 
                                                              args = pargs)
                                       }
                                       rm(pargs)
                                       if (do.try) {
                                         permut.err <- try(err.fun(
                                           nd[, response], 
                                           pred.test), silent = silent)
                                         if (class(permut.err) == "try-error") {
                                           imp.temp[[vnm]][[cnt]] = c()
                                         }
                                         else {
                                           imp.temp[[vnm]][[cnt]] <- as.list(
                                             unlist(
                                               res[[i]][[j]]$test) - 
                                               unlist(permut.err))
                                         }
                                       }
                                       else {
                                         permut.err <- err.fun(nd[, response], 
                                                               pred.test)
                                         imp.temp[[vnm]][[cnt]] <- as.list(
                                           unlist(res[[i]][[j]]$test) - 
                                             unlist(permut.err))
                                       }
                                     }
                                   }
                                   impo[[i]][[j]] <- as.data.frame(
                                     t(sapply(imp.temp, 
                                              function(y) sapply(
                                                as.data.frame(t(sapply(
                                                  y, as.data.frame))), 
                                                function(x) mean(
                                                  unlist(x))))))
                                   rm(nd.bak, nd)
                                 }
                               }
                             } #end of each fold
                             if (err.rep) {
                               if (is.factor(data[, response])) {
                                 lev <- levels(data[, response])
                                 if (err.train) {
                                   pooled.obs.train <-
                                     factor(lev[pooled.obs.train], 
                                            levels = lev)
                                   pooled.obs.test <-
                                     factor(lev[pooled.obs.test], 
                                            levels = lev)
                                 }
                                 if (is.factor.prediction) {
                                   if (err.train) {
                                     pooled.pred.train <-
                                       factor(lev[pooled.pred.train], 
                                              levels = lev)
                                     pooled.pred.test <-
                                       factor(lev[pooled.pred.test], 
                                              levels = lev)
                                   }
                                 }
                                 rep.err.train <- NULL
                                 if (err.train) {
                                   rep.err.train <-
                                     err.fun(pooled.obs.train, 
                                             pooled.pred.train)
                                 }
                                 if (i == 1) {
                                   rep.err <- t(unlist(list(
                                     train = rep.err.train, 
                                     test = err.fun(pooled.obs.test,
                                                    pooled.pred.test))))
                                 }
                                 else {
                                   rep.err <- rbind(rep.err,
                                                    unlist(list(
                                                      train =
                                                        rep.err.train, 
                                                      test = err.fun(
                                                        pooled.obs.test,
                                                        pooled.pred.test))))
                                 }
                                 if (do.gc >= 2) {
                                   gc()
                                 }
                               }
                               if ((do.gc >= 1) & (do.gc < 2)) {
                                 gc()
                               }
                             }
                             
                             cat(date(), "Rep:", 
                                 names(resamp)[i], rep.err, "\n") 
                             
                             if (err.rep & err.fold) {
                               foreach.out <- list(rep.err, res)
                               return(foreach.out)
                             }
                             if (err.rep & !err.fold) {
                               foreach.out <- rep.err
                               return(foreach.out)
                             }
                             if (!err.rep & err.fold) {
                               foreach.out <- res
                               return(foreach.out)
                             }
                           }
    stopCluster(cl)
    
    return(foreach.out)
    
    if (err.rep & !err.fold) {
      rep.err <- as.data.frame(foreach.out)
    }
    if (err.rep & err.fold) {
      for (i in 1:length(resamp)) {
        foreach.out[[i]] <- as.data.frame(foreach.out[[i]])
      }
      for (i in 2:length(resamp)) {
        foreach.out[[1]] <- merge.data.frame(foreach.out[[1]], 
                                             foreach.out[[i]], all = TRUE)
      }
      i <- 2
      while (i <= length(resamp)) {
        foreach.out[[2]] <- NULL
        i <- i + 1
      }
    }
    
    if (verbose == "all" | verbose == "rep") {
      cat(date(), "Done.\n")
    }
    if (importance) {
      class(impo) <- "sperrorestimportance"
    }
    
    if (benchmark) {
      end.time = Sys.time()
      my.bench = list(system.info = Sys.info(),
                      t.start = start.time,
                      t.end = end.time,
                      cpu.cores = detectCores(),
                      par.mode = par.args$par.mode,
                      par.units = par.args$par.units,
                      runtime.performance = end.time - start.time)
      class(my.bench) = "sperrorestbenchmarks"
    }
    else my.bench = NULL
    
    if (err.rep & err.fold) {
      res <- foreach.out
      res[[1]] <- NULL
      class(res[[1]]) = "sperrorestfolderror"
      class(foreach.out[[1]]) = "sperrorestreperror"
      RES <- list(error.rep = foreach.out[[1]],
                  error.fold = res[[1]], 
                  represampling = resamp, 
                  importance = impo, 
                  benchmarks = my.bench,
                  package.version = packageVersion("sperrorest"))
      class(RES) <- "sperrorest"
      return(RES)
    }
    if (err.rep & !err.fold) {
      class(rep.err) = "sperrorestreperror"
      RES <- list(error.rep = rep.err,
                  error.fold = NULL, 
                  represampling = resamp, 
                  importance = impo, 
                  benchmarks = my.bench,
                  package.version = packageVersion("sperrorest"))
      class(RES) <- "sperrorest"
      return(RES)
    }
    if (!err.rep & err.fold) {
      class(foreach.out) = "sperrorestfolderror"
      RES <- list(error.rep = NULL,
                  error.fold = foreach.out, 
                  represampling = resamp, 
                  importance = impo, 
                  benchmarks = my.bench,
                  package.version = packageVersion("sperrorest"))
      class(RES) <- "sperrorest"
      return(RES)
    }
  }
  
  
}) 
