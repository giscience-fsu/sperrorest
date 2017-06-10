# sperrorest 2.0.0

Major:
  * integration of `parsperrorest()` into `sperrorest()`. 
  * by default, `sperrorest()` now runs in parallel using all available cores. 
  * `runfolds()` and `runreps()` are now doing the heavy lifting in the background. All modes are now running on the same code base. Before, all parallel modes were running on different code implementations. 
  * function and argument name changes to 'snake_case'
  
Features:
  * new (parallel) modes: 
    * `apply`: calls `pbmclapply()` on Unix and `pbapply()` on Windows.
    * `future`: calls `future_lapply()` with various `future` options (`multiprocess`, `multicore`, etc.). 
    * `foreach`: `foreach()` with various `future` options (`multiprocess`, `multicore`, etc.). Default option to `cluster`. This is also the overall default mode for `sperrorest()`.
    * `sequential`: sequential execution using `future` backend.
  * RMSE instead of MSE as error measure
    
Misc: 
  * limit workers to number of repetitions if number of cores > number of repetitions. This ensures that no unnecessary workers are started and increases robustness of parallel execuction.
  * documentation improvements 

Bugfixes:
  * partial matching of arguments
  * account for factor levels only present in test data but missing in training data. Previously, `sperrorest` errored during the predict step when this case occured. Now, this is accounted for and an informative message is given. 

# sperrorest 1.0.0 (08-Mar-2017)

New features:
  * add `parsperrorest()`: This function lets you exexute `sperrorest()` in parallel. It includes two modes (`par.mode = 1` and `par.mode = 2`) which use different   parallelization approaches in the background. See `?parsperrorest()` for more details.
  
  * add `partition.factor.cv()`: This resampling method enables partitioning based 
  on a given factor variable. This can be used, for example, to resample 
  agricultural data, that is grouped by fields, at the agricultural field level 
  in order to preserve spatial autocorrelation within fields.
  
  * `sperrorest()` and `parsperrorest()`: Add `benchmark` item to returned object giving information about execution time, used cores and other system details. 
  
Changes to functions: 
  * `sperrorest`(): Change argument naming. `err.unpooled` is now `error.fold` and `err.pooled` is now `error.rep`
  
  * `sperrorest()` and `parsperrorest()`: Change order and naming of returned object
      - class `sperrorestpoolederror` is now `sperrorestreperror`
      - returned `sperrorest` list is now ordered as follows: 
           1. error.rep
           2. error.fold
           3. importance
           4. benchmarks
           5. package.version  

Package related:
  * add package NEWS
  
  * add package vignette -> `vignette("sperrorest-vignette", package = "sperrorest")`
  
  * package is now ByteCompiled
  
  * Github repo of `sperrorest` now at 
  [https://github.com/pat-s/sperrorest/](https://github.com/pat-s/sperrorest/)
  
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
