# Describe the data set:

#' J. Muenchow's Ecuador landslide data set
#'
#' Data set created by Jannes Muenchow, University of Erlangen-Nuremberg,
#' Germany.
#' These data should be cited as Muenchow et al. (2012) (see reference below).
#' This publication also contains additional information on data collection and
#' the geomorphology of the area. The data set provded here is (a subset of) the
#' one from the 'natural' part of the RBSF area and corresponds to landslide
#' distribution in the year 2000.
#' @name ecuador
#'
#' @keywords datasets
#' @keywords internal
#'
#' @docType data
#'
#' @format a `data.frame` with point samples of landslide and
#' non-landslide locations in a study area in the Andes of southern Ecuador.
#'
#' @references Muenchow, J., Brenning, A., Richter, M., 2012. Geomorphic process
#' rates of landslides along a humidity gradient in the tropical Andes.
#' *Geomorphology*, 139-140: 271-284.
#'
#' Brenning, A., 2005. Spatial prediction models for landslide hazards:
#' review, comparison and evaluation.
#' *Natural Hazards and Earth System Sciences*, 5(6): 853-862.
#'
#' @examples
#' data(ecuador)
#' str(ecuador)
#' library(rpart)
#' ctrl <- rpart.control(cp = 0.02)
#' fit <- rpart(slides ~ dem + slope + hcurv + vcurv +
#'   log.carea + cslope, data = ecuador, control = ctrl)
#' par(xpd = TRUE)
#' plot(fit, compress = TRUE, main = "Muenchows landslide data set")
#' text(fit, use.n = TRUE)
NULL

#' Fruit-tree crop classification: the Maipo dataset
#'
#' This dataset is from a case study on fruit-tree crop classification
#' using a satellite image time series. The dataset should be
#' cited as Pena & Brenning (2015), reference below. There are
#' 7713 grid cells from 400 fields in this dataset, which makes it
#' necessary to apply spatial cross-validation at the field level
#' for model assessment (see [partition_factor_cv()]).
#'
#' @name maipo
#' @docType data
#'
#' @keywords datasets
#' @keywords internal
#'
#' @format a `data.frame` with point samples (grid cells) of crop type
#' and Landsat-derived remote sensing features (spectral bands
#' and vegetation indices). In addition, UTM x/y coordinates and
#' a factor variable indicating which field a grid cell belongs to.
#' Spectral bands are coded as `bij` where `i` represents the image
#' date (early to late season, dates see paper), and `j` is the band
#' number.
#'
#' @references Pena, M.A., Brenning, A. (2015). Assessing fruit-tree
#' crop classification from Landsat-8 time series for the Maipo Valley,
#' Chile. *Remote Sensing of Environment*, 171: 234-244.
#'
NULL
