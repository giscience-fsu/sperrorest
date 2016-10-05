
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/pat-s/sperrorest.svg?branch=master)](https://travis-ci.org/pat-s/sperrorest) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/sperrorest)](http://cran.r-project.org/package=sperrorest)

sperrorest
==========

Spatial Error Estimation and Variable Importance

This package implements spatial error estimation and permutation-based spatial variable importance using different spatial cross-validation and spatial block bootstrap methods. To cite `sperrorest` in publications, reference the paper by Brenning (2012).

Installation
------------

Get the released version from CRAN:

``` r
install.packages("sperrorest")
#> Installing package into '/usr/local/lib/R/3.3/site-library'
#> (as 'lib' is unspecified)
```

Or the development version from github:

``` r
devtools::install_github("pat-s/sperrorest")
#> Downloading GitHub repo pat-s/sperrorest@master
#> from URL https://api.github.com/repos/pat-s/sperrorest/zipball/master
#> Installing sperrorest
#> '/usr/local/Cellar/r/3.3.1_3/R.framework/Resources/bin/R' --no-site-file  \
#>   --no-environ --no-save --no-restore --quiet CMD INSTALL  \
#>   '/private/var/folders/5j/_1ts10x512sg_5q_3kzc_c1w0000gn/T/Rtmp2oaQA9/devtools8cca3a7a74b9/pat-s-sperrorest-142e633'  \
#>   --library='/usr/local/lib/R/3.3/site-library' --install-tests
#> 
```

References
==========

Brenning, A. 2012. Spatial cross-validation and bootstrap for the assessment of prediction rules in remote sensing: the R package 'sperrorest'. 2012 IEEE International Geoscience and Remote Sensing Symposium (IGARSS), 23-27 July 2012, p. 5372-5375.

Brenning, A. 2005. Spatial prediction models for landslide hazards: review, comparison and evaluation. Natural Hazards and Earth System Sciences, 5(6): 853-862.

Russ, G. & A. Brenning. 2010a. Data mining in precision agriculture: Management of spatial information. In 13th International Conference on Information Processing and Management of Uncertainty, IPMU 2010; Dortmund; 28 June - 2 July 2010. Lecture Notes in Computer Science, 6178 LNAI: 350-359.

Russ, G. & A. Brenning. 2010b. Spatial variable importance assessment for yield prediction in Precision Agriculture. In Advances in Intelligent Data Analysis IX, Proceedings, 9th International Symposium, IDA 2010, Tucson, AZ, USA, 19-21 May 2010. Lecture Notes in Computer Science, 6065 LNCS: 184-195.
