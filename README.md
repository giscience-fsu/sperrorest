# sperrorest

<!-- badges: start -->
[![R CMD Check via {tic}](https://img.shields.io/github/workflow/status/giscience-fsu/sperrorest/R%20CMD%20Check%20via%20%7Btic%7D?logo=github&label=R%20CMD%20Check%20via%20{tic}&style=flat-square)](https://github.com/giscience-fsu/sperrorest/actions)
[![CRAN](https://www.r-pkg.org/badges/version/sperrorest)](https://cran.r-project.org/package=sperrorest)
[![lifecycle](https://img.shields.io/badge/lifecycle-retired-blue.svg)](https://www.tidyverse.org/lifecycle/#retired)
[![codecov](https://codecov.io/gh/giscience-fsu/sperrorest/branch/master/graph/badge.svg)](https://codecov.io/gh/giscience-fsu/sperrorest)
<!-- badges: end -->

{sperrorest} is currently not actively developed.
We recommend to use [mlr3spatiotempcv](https://github.com/mlr-org/mlr3spatiotempcv) for all future (spatial) cross-validation projects.

## Description

*Spatial Error Estimation and Variable Importance*

This package implements spatial error estimation and permutation-based spatial variable importance using different spatial cross-validation and spatial block bootstrap methods. 
To cite {sperrorest} in publications, reference the paper by @Brenning2012. 
To see the package in action, please check the vignette ["Spatial Modeling Use Case"](https://giscience-fsu.github.io/sperrorest/articles/spatial-modeling-use-case.html).

## Installation

```r
install.packages("sperrorest")
```

## References

Brenning, A. 2005. “Spatial Prediction Models for Landslide Hazards:
Review, Comparison and Evaluation.” *Natural Hazards and Earth System
Science* 5 (6). Copernicus GmbH:853–62.
https://doi.org/10.5194/nhess-5-853-2005.

Brenning, A. 2012. “Spatial Cross-Validation and Bootstrap for the Assessment of
Prediction Rules in Remote Sensing: The R Package Sperrorest.” In *2012
IEEE International Geoscience and Remote Sensing Symposium*, 5372–5.
https://doi.org/10.1109/IGARSS.2012.6352393.

Russ, Georg, and Alexander Brenning. 2010a. “Data Mining in Precision
Agriculture: Management of Spatial Information.” In *Computational
Intelligence for Knowledge-Based Systems Design: 13th International
Conference on Information Processingand Management of Uncertainty, IPMU
2010, Dortmund, Germany, June 28 - July 2, 2010. Proceedings*, edited by
Eyke Hüllermeier, Rudolf Kruse, and Frank Hoffmann, 350–59. Berlin,
Heidelberg: Springer Berlin Heidelberg.
https://doi.org/10.1007/978-3-642-14049-5_36.

Russ, Georg, and Alexander Brenning. 2010b. “Spatial Variable Importance Assessment for Yield Prediction
in Precision Agriculture.” In *Lecture Notes in Computer Science*,
184–95. Springer Science + Business Media.
https://doi.org/10.1007/978-3-642-13062-5_18.

