<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# sperrorest 3.0.5

---
- adjust handling of class name in response to R check 'Note'


# sperrorest 3.0.4.9000

- adjust handling of class name in response to R check 'Note'


# sperrorest 3.0.4

- implemented parallelization modes at fold and repetition levels
- more flexible permutation options (especially for LOO CV)
- handle formulas with 0 or 1 predictor(s), e.g. y~1
- updated vignette


# sperrorest 3.0.3.0001

- "fixed train_fun / test_fun issue
- more flexible parallelization of `add.distance`


# sperrorest 3.0.3

- added parallelization modes (arguments `mode_rep` and `mode_fold`)
- more flexible permutation (argument `imp_sample_from`)
- updated vignette


# sperrorest 3.0.2.9000

- `partition_disc()`: set default value of arg `buffer` to 0 instead of NULL, fixes #61


# sperrorest 3.0.2

- `partition_disc()`: set default value of arg `buffer` to 0 instead of NULL, fixes #61
- `partition_loo()`: Sequence along observations instead of columns.
  Before, the train set was only composed of `ncol` observation. (#60)


# sperrorest 3.0.1

- Resolve minor NAMESPACE issues for {future} on CRAN


# sperrorest 3.0.0

## Breaking

- All parallel modes got removed in favor of using parallelism via package {future}.
  This simplifies usage a lot and helps simplifying future maintenance.
  Also it gives users the freedom to choose the parallel backend on their own.
- `sperrorest()` run sequentially by default again rather than in parallel.

## Package infrastructure

- Removed the following packages from Imports: {glue}, {purrr}, {doFuture}, {gdata}, {magrittr}, {parallel}, {pbapply}, {pbmcapply}, {foreach}
- Moved the following packages from Imports to Suggests: {rpart}
- Removed the following packages from Suggests: {pacman}, {ipred}, {RSAGA}
- The "spatial-use-case" vignette now runs and is not read-only anymore.

# sperrorest 2.1.5 (20-Mar-2018)

## Bugfixes
  * Updated error message displayed to the user if `err_fun()` throws an error during performance calculation.
    An exemplary case would be a binary classification in which only one level of the response exists in the test data (due to spatial partitioning).

# sperrorest 2.1.4 (12-Feb-2018)

## Minor
  * import `future_lapply` from `future.apply` instead of `future`

# sperrorest 2.1.3 (08-Dec-2017)

## Minor
  * update CITATION file

# sperrorest 2.1.1 (15-Oct-2017)

## Bugfixes
  * `train_fun` and `test_fun` are now handled correctly and eventual sub-sampling is correctly reflected to the resulting 'resampling' object

# sperrorest 2.1.0 (25-Sep-2017)

## Features
  * error handling during model fitting & performance evaluation: If a model does not converge for some folds or an error occurs during performance calculation, results of this fold are set to `NA` and a message is printed to the console. `sperrorest()` will continue normally and uses the successful folds to calculate the repetition error. This helps to run CV with many repetitions using models which do not always converge like `maxnet()`, `gamm()` or `svm()`. 
  
## Bugfixes
  * Size of example data set `ecuador` has been adjusted to avoid exact duplicates of partitions when using `partition_kmeans()`. 
  

# sperrorest 2.0.1 (20-Jul-2017)

## Bugfixes
  * Fixes a bug which caused equal importance of all predictors when performing permutation-based variable importance assessment


# sperrorest 2.0.0 (12-Jun-2017)

## Major
  * integration of `parsperrorest()` into `sperrorest()`. 
  * by default, `sperrorest()` now runs in parallel using all available cores. 
  * `runfolds()` and `runreps()` are now doing the heavy lifting in the background. All modes are now running on the same code base. Before, all parallel modes were running on different code implementations. 
  * function and argument name changes to 'snake_case'
  
## Features
  * new (parallel) modes: 
    * `apply`: calls `pbmclapply()` on Unix and `pbapply()` on Windows.
    * `future`: calls `future_lapply()` with various `future` options (`multiprocess`, `multicore`, etc.). 
    * `foreach`: `foreach()` with various `future` options (`multiprocess`, `multicore`, etc.). Default option to `cluster`. This is also the overall default mode for `sperrorest()`.
    * `sequential`: sequential execution using `future` backend.
  * RMSE instead of MSE as error measure
  * You can now pass also single values to `repetition` argument of `sperrorest()`. Specifying a range like `repetition = 1:10` will also stay valid. 
  * New vignette `sperrorest::parallel-modes` comparing the various parallel modes.
  * New vignette `sperrorest::custom-pred-and-model-functions` explaining why and how custom defined model and predict functions are needed for some model setups.
    
## Misc
  * Limit workers to number of repetitions if number of cores > number of repetitions. This ensures that no unnecessary workers are started and increases robustness of parallel execuction.
  * documentation improvements.
  * `do_try` argument has been removed.
  * `error.fold`, `error.rep` and `err.train` arguments have been removed because they are all calculated by default now.

## Bugfixes
  * partial matching of arguments
  * account for factor levels only present in test data but missing in training data. Previously, {sperrorest} errored during the predict step when this occurred. Now, this is accounted for and an informative message is given. 

# sperrorest 1.0.0 (08-Mar-2017)

## New features
  * add `parsperrorest()`: This function lets you execute `sperrorest()` in parallel. It includes two modes (`par.mode = 1` and `par.mode = 2`) which use different   parallelization approaches in the background. See `?parsperrorest()` for more details.
  
  * add `partition.factor.cv()`: This resampling method enables partitioning based 
  on a given factor variable. This can be used, for example, to resample 
  agricultural data, that is grouped by fields, at the agricultural field level 
  in order to preserve spatial autocorrelation within fields.
  
  * `sperrorest()` and `parsperrorest()`: Add `benchmark` item to returned object giving information about execution time, used cores and other system details. 
  
Changes to functions: 
  * {sperrorest}(): Change argument naming. `err.unpooled` is now `error.fold` and `err.pooled` is now `error.rep`
  
  * `sperrorest()` and `parsperrorest()`: Change order and naming of returned object
      - class `sperrorestpoolederror` is now `sperrorestreperror`
      - returned {sperrorest} list is now ordered as follows: 
           1. error.rep
           2. error.fold
           3. importance
           4. benchmarks
           5. package.version  

## Package related
  * add package NEWS
  
  * add package vignette -> `vignette("sperrorest-vignette", package = "sperrorest")`
  
  * package is now ByteCompiled
  
  * Github repo of {sperrorest} now at 
  [https://github.com/giscience-fsu/sperrorest/](https://github.com/giscience-fsu/sperrorest/)
  
# sperrorest 0.2-1 (19 June 2012)

* First release on CRAN

# sperrorest 0.2-0 (19 June 2012)

* last pre-release version
* replaced Stoyan's data set with Jannes Muenchow's data, adapted examples


# sperrorest 0.1-5 (1 Mar 2012)

* made training set estimation optional
* robustified code using try()


# sperrorest 0.1-2 (29 Jan 2012)

* internal release 0.1-2
* some bug fixes, e.g. in err.* functions
* improved support of pooled versus unpooled error estimation
* changed some argument names
* this version was used for Angie's analyses

# sperrorest 0.1-1 (29 Dec 2011)

* built internal release 0.1-1

# sperrorest 0.1

* general code development (2009 - 2011)
* package project and documentation created (Oct-Dec 2011)
