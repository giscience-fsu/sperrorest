# sperrorest

<!-- badges: start -->
[![R CMD Check via {tic}](https://github.com/giscience-fsu/sperrorest/workflows/R%20CMD%20Check%20via%20{tic}/badge.svg?branch=master)](https://github.com/giscience-fsu/sperrorest/actions)
[![CRAN](https://www.r-pkg.org/badges/version/sperrorest)](https://cran.r-project.org/package=sperrorest)
[![codecov](https://codecov.io/gh/giscience-fsu/sperrorest/branch/master/graph/badge.svg)](https://app.codecov.io/gh/giscience-fsu/sperrorest)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

## Description

*Spatial Error Estimation and Variable Importance*

This package implements spatial error estimation and permutation-based spatial variable importance using different spatial cross-validation and bootstrap methods.
Supported resampling methods include various types of block resampling, leave-one-out sampling with buffer, and resampling at the level of predefined groups; users can implement their own resampling functions.
To cite {sperrorest} in publications, reference the paper by @Brenning2012. 
To see the package in action, please check the vignette ["Spatial Modeling Use Case"](https://giscience-fsu.github.io/sperrorest/articles/spatial-modeling-use-case.html).

## Installation

CRAN release version

```r
install.packages("sperrorest")
```

Development version

```r
remotes::install_github("giscience-fsu/sperrorest")
```

## References

Brenning, A. 2005. Spatial Prediction Models for Landslide Hazards: Review, Comparison and Evaluation. *Natural Hazards and Earth System Sciences* 5 (6). Copernicus GmbH:853–62.
https://doi.org/10.5194/nhess-5-853-2005

Brenning, A. 2012. Spatial Cross-Validation and Bootstrap for the Assessment of Prediction Rules in Remote Sensing: The R Package Sperrorest. In *2012 IEEE International Geoscience and Remote Sensing Symposium*, 5372–5.
https://doi.org/10.1109/IGARSS.2012.6352393

Russ, Georg, and A. Brenning. 2010a. Data Mining in Precision Agriculture: Management of Spatial Information. In *Computational Intelligence for Knowledge-Based Systems Design: 13th International Conference on Information Processing and Management of Uncertainty, IPMU 2010, Dortmund, Germany, June 28 - July 2, 2010. Proceedings*, edited by Eyke Hüllermeier, Rudolf Kruse, and Frank Hoffmann, 350–59. Springer.
https://doi.org/10.1007/978-3-642-14049-5_36

Russ, G., and A. Brenning. 2010b. Spatial Variable Importance Assessment for Yield Prediction
in Precision Agriculture. In *Lecture Notes in Computer Science*,
184–95. 
https://doi.org/10.1007/978-3-642-13062-5_18

Schratz, P., Muenchow, J., Iturritxa, E., Richter, J., Brenning, A. (2019). Hyperparameter tuning and performance assessment of statistical and machine-learning algorithms using spatial data. *Ecological Modelling*, 406: 109-120.
https://doi.org/10.1016/j.ecolmodel.2019.06.002
