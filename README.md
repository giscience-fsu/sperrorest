
<!-- README.md is generated from README.Rmd. Please edit that file -->
#### General

[![Build Status](https://travis-ci.org/pat-s/sperrorest.svg?branch=master)](https://travis-ci.org/pat-s/sperrorest) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/pat-s/sperrorest?branch=master&svg=true)](https://ci.appveyor.com/project/pat-s/sperrorest) [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![codecov](https://codecov.io/gh/pat-s/sperrorest/branch/master/graph/badge.svg)](https://codecov.io/gh/pat-s/sperrorest) [![minimal R version](https://img.shields.io/badge/R%3E%3D-2.10-6666ff.svg)](https://cran.r-project.org/) [![Last-changedate](https://img.shields.io/badge/last%20change-2017--02--05-yellowgreen.svg)](/commits/master)

#### CRAN

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/sperrorest)](http://cran.r-project.org/package=sperrorest) [![Downloads](https://cranlogs.r-pkg.org/badges/sperrorest?color=brightgreen)](https://www.r-pkg.org/pkg/sperrorest) ![](https://cranlogs.r-pkg.org/badges/grand-total/sperrorest)

#### Github

[![packageversion](https://img.shields.io/badge/Package%20version-1.0.0.9000-orange.svg?style=flat-square)](commits/master)

sperrorest
==========

Spatial Error Estimation and Variable Importance

This package implements spatial error estimation and permutation-based spatial variable importance using different spatial cross-validation and spatial block bootstrap methods. To cite `sperrorest` in publications, reference the paper by Brenning (2012).

Installation
------------

Get the released version from CRAN:

``` r
install.packages("sperrorest")
```

Or the development version from Github:

``` r
devtools::install_github("pat-s/sperrorest", build_vignettes = TRUE)
```

References
==========

Brenning, A. 2005. “Spatial Prediction Models for Landslide Hazards: Review, Comparison and Evaluation.” *Natural Hazards and Earth System Science* 5 (6). Copernicus GmbH: 853–62. doi:[10.5194/nhess-5-853-2005](https://doi.org/10.5194/nhess-5-853-2005).

———. 2012. “Spatial Cross-Validation and Bootstrap for the Assessment of Prediction Rules in Remote Sensing: The R Package Sperrorest.” In *2012 IEEE International Geoscience and Remote Sensing Symposium*, 5372–5. doi:[10.1109/IGARSS.2012.6352393](https://doi.org/10.1109/IGARSS.2012.6352393).

Russ, Georg, and Alexander Brenning. 2010a. “Data Mining in Precision Agriculture: Management of Spatial Information.” In *Computational Intelligence for Knowledge-Based Systems Design: 13th International Conference on Information Processingand Management of Uncertainty, IPMU 2010, Dortmund, Germany, June 28 - July 2, 2010. Proceedings*, edited by Eyke Hüllermeier, Rudolf Kruse, and Frank Hoffmann, 350–59. Berlin, Heidelberg: Springer Berlin Heidelberg. doi:[10.1007/978-3-642-14049-5\_36](https://doi.org/10.1007/978-3-642-14049-5_36).

———. 2010b. “Spatial Variable Importance Assessment for Yield Prediction in Precision Agriculture.” In *Lecture Notes in Computer Science*, 184–95. Springer Science + Business Media. doi:[10.1007/978-3-642-13062-5\_18](https://doi.org/10.1007/978-3-642-13062-5_18).
