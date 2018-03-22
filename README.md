
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Deprecation warning

`partition_kmeans()`has been integrated into
[mlr](https://github.com/mlr-org/mlr) (see
[2ebd2f](https://github.com/mlr-org/mlr/commit/2ebd2f29bb8adcf5f865ca3a6b579c71ca96c072)).
`sperrorest` is currently not actively developed. We recommend to use
[mlr](https://github.com/mlr-org/mlr) for all future (spatial)
cross-validation work.

#### General

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/inactive.svg)](http://www.repostatus.org/#inactive)
[![DOI](https://zenodo.org/badge/69967610.svg)](https://zenodo.org/badge/latestdoi/69967610)

| Resource:     | CRAN                                                                                                                                                                       | Travis CI                                                                                                                                                 | Appveyor                                                                                                                                                                       |
| ------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| *Platforms:*  | *Multiple*                                                                                                                                                                 | *Linux & macOS*                                                                                                                                           | *Windows*                                                                                                                                                                      |
| R CMD check   | <a href="https://cran.r-project.org/web/checks/check_results_sperrorest.html"><img border="0" src="http://www.r-pkg.org/badges/version/sperrorest" alt="CRAN version"></a> | <a href="https://travis-ci.org/pat-s/sperrorest"><img src="https://travis-ci.org/pat-s/sperrorest.svg?branch=dev" alt="Build status"></a>                 | <a href="https://ci.appveyor.com/project/pat-s/sperrorest"><img src="https://ci.appveyor.com/api/projects/status/n4679ihnaixx86xv/branch/dev?svg=true" alt="Build status"></a> |
| Test coverage |                                                                                                                                                                            | <a href="https://codecov.io/gh/pat-s/sperrorest"><img src="https://codecov.io/gh/pat-s/sperrorest/branch/dev/graph/badge.svg" alt="Coverage Status"/></a> |                                                                                                                                                                                |

#### CRAN

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/sperrorest)](https://cran.r-project.org/package=sperrorest)
[![Downloads](https://cranlogs.r-pkg.org/badges/sperrorest?color=brightgreen)](https://www.r-pkg.org/pkg/sperrorest)
![](https://cranlogs.r-pkg.org/badges/grand-total/sperrorest)

# Description

*Spatial Error Estimation and Variable Importance*

This package implements spatial error estimation and permutation-based
spatial variable importance using different spatial cross-validation and
spatial block bootstrap methods. To cite `sperrorest` in publications,
reference the paper by Brenning (2012). To see the package in action,
please check [the
vignette](https://pat-s.github.io/sperrorest/articles/spatial-modeling-use-case.html).

## Installation

Get the released version from CRAN:

``` r
install.packages("sperrorest")
```

Or the development version from Github:

``` r
remotes::install_github("pat-s/sperrorest")
```

# References

<div id="refs" class="references">

<div id="ref-Brenning2005">

Brenning, A. 2005. “Spatial Prediction Models for Landslide Hazards:
Review, Comparison and Evaluation.” *Natural Hazards and Earth System
Science* 5 (6). Copernicus GmbH:853–62.
<https://doi.org/10.5194/nhess-5-853-2005>.

</div>

<div id="ref-Brenning2012">

———. 2012. “Spatial Cross-Validation and Bootstrap for the Assessment of
Prediction Rules in Remote Sensing: The R Package Sperrorest.” In *2012
IEEE International Geoscience and Remote Sensing Symposium*, 5372–5.
<https://doi.org/10.1109/IGARSS.2012.6352393>.

</div>

<div id="ref-Russ2010b">

Russ, Georg, and Alexander Brenning. 2010a. “Data Mining in Precision
Agriculture: Management of Spatial Information.” In *Computational
Intelligence for Knowledge-Based Systems Design: 13th International
Conference on Information Processingand Management of Uncertainty, IPMU
2010, Dortmund, Germany, June 28 - July 2, 2010. Proceedings*, edited by
Eyke Hüllermeier, Rudolf Kruse, and Frank Hoffmann, 350–59. Berlin,
Heidelberg: Springer Berlin Heidelberg.
<https://doi.org/10.1007/978-3-642-14049-5_36>.

</div>

<div id="ref-Russ2010a">

———. 2010b. “Spatial Variable Importance Assessment for Yield Prediction
in Precision Agriculture.” In *Lecture Notes in Computer Science*,
184–95. Springer Science + Business Media.
<https://doi.org/10.1007/978-3-642-13062-5_18>.

</div>

</div>
