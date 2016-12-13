sperrorest 1.0.0 (11 Dec 2016)

* add `parsperrorest()`: This function lets you exexute `sperrorest()` in parallel. 

* add `partition.factor.cv()`: This resampling method enables partitioning based 
on a given factor variable. This can be used, for example, to resample 
agricultural data that is grouped by fields, at the agricultural field level 
in order to preserve spatial autocorrelation within fields.

* add `benchmark` item to returned `sperrorest()` and `parsperrorest()` object giving information about execution time, used cores and other system details. 

* create github repo of `sperrorest` at 
[https://github.com/pat-s/sperrorest/](https://github.com/pat-s/sperrorest/)

* change params: `err.unpooled` to `error.fold` and `err.pooled` to `error.rep`

* change order and naming of returned `sperrorest()` and `parsperrorest()` object
    - class `sperrorestpoolederror` is now `sperrorestreperror`
    - returned `sperrorest` object is now ordered as follows: 
         1. error.rep
         2. error.fold
         3. importance
         4. benchmarks
         5. package.version  
         
* add `notify` argument: Shows a notification badge once `sperrorest()` or `parsperrorest()` has finished.

* add package NEWS

* add package vignette

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
