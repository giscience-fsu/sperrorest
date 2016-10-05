[![Build Status](https://travis-ci.org/hadley/sperrorest.svg?branch=master)](https://travis-ci.org/pat-s/sperrorest)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/sperrorest)](http://cran.r-project.org/package=sperrorest)

# sperrorest

Spatial Error Estimation and Variable Importance

This package implements spatial error estimation and permutation-based
spatial variable importance using different spatial cross-validation
and spatial block bootstrap methods. To cite `sperrorest` in publications,
reference the paper by Brenning (2012).

## Installation

Get the released version from CRAN:

```R
install.packages("sperrorest")
```
Or the development version from github:

```R
devtools::install_github("pat-s/sperrorest")
```

# References
Brenning, A. 2012. Spatial cross-validation and bootstrap for the
assessment of prediction rules in remote sensing: the R package 'sperrorest'.
2012 IEEE International Geoscience and Remote Sensing Symposium (IGARSS),
23-27 July 2012, p. 5372-5375.

Brenning, A. 2005. Spatial prediction models for landslide hazards:
review, comparison and evaluation. Natural Hazards and Earth System Sciences,
5(6): 853-862.

Russ, G. & A. Brenning. 2010a. Data mining in precision agriculture:
Management of spatial information. In 13th International Conference on
Information Processing and Management of Uncertainty, IPMU 2010; Dortmund;
28 June - 2 July 2010.  Lecture Notes in Computer Science, 6178 LNAI: 350-359.

Russ, G. & A. Brenning. 2010b. Spatial variable importance assessment for
yield prediction in Precision Agriculture. In Advances in Intelligent
Data Analysis IX, Proceedings, 9th International Symposium, IDA 2010,
Tucson, AZ, USA, 19-21 May 2010.
Lecture Notes in Computer Science, 6065 LNCS: 184-195.
