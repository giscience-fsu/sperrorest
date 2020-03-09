#' @title Partition the data for a (non-spatial) cross-validation
#'
#' @description `partition_cv` creates a [represampling] object for
#' `length(repetition)`-repeated `nfold`-fold cross-validation.
#'
#' @name partition_cv
#'
#' @param data `data.frame` containing at least the columns specified
#' by `coords`
#' @param coords (ignored by `partition_cv`)
#' @param nfold number of partitions (folds) in `nfold`-fold
#' cross-validation partitioning
#' @param repetition numeric vector: cross-validation repetitions
#' to be generated. Note that this is not the number of repetitions,
#' but the indices of these repetitions. E.g., use `repetition = c(1:100)`
#' to obtain (the 'first') 100 repetitions, and `repetition = c(101:200)`
#' to obtain a different set of 100 repetitions.
#' @param seed1 `seed1+i` is the random seed that will be used by
#' [set.seed] in repetition `i` (`i` in `repetition`)
#' to initialize the random number generator before sampling from the data set.
#' @param return_factor if `FALSE` (default), return a
#' [represampling] object; if `TRUE` (used internally by
#' other `sperrorest` functions), return a `list` containing factor
#' vectors (see Value)
#'
#' @details This function does not actually perform a cross-validation
#' or partition the data set itself; it simply creates a data structure
#' containing the indices of training and test samples.
#'
#' @return If `return_factor = FALSE` (the default), a
#' [represampling] object. Specifically, this is a (named) list of
#' `length(repetition)` `resampling` objects.
#' Each of these [resampling] objects is a list of length
#' `nfold` corresponding to the folds.
#' Each fold is represented by a list of containing the components `train`
#' and `test`, specifying the indices of training and test samples
#' (row indices for `data`).
#' If `return_factor = TRUE` (mainly used internally), a (named) list of
#' length `length(repetition)`.
#' Each component of this list is a vector of length `nrow(data)` of type
#' `factor`, specifying for each sample the fold to which it belongs.
#' The factor levels are `factor(1:nfold)`.
#'
#' @seealso [sperrorest], [represampling]
#'
#' @examples
#' data(ecuador)
#' ## non-spatial cross-validation:
#' resamp <- partition_cv(ecuador, nfold = 5, repetition = 5)
#' # plot(resamp, ecuador)
#' # first repetition, second fold, test set indices:
#' idx <- resamp[["1"]][[2]]$test
#' # test sample used in this particular repetition and fold:
#' ecuador[idx, ]
#' @export
partition_cv <- function(data, coords = c("x", "y"), nfold = 10, repetition = 1,
                         seed1 = NULL, return_factor = FALSE) {
  resampling <- list()

  if (length(repetition) < 2) {
    repetition <- seq(1, repetition, by = 1)
  }

  for (cnt in repetition) {
    if (!is.null(seed1)) {
      set.seed(seed1 + cnt) # nocov
    }
    resampler <- sample(rep(sample(nfold), length = nrow(data)),
      size = nrow(data)
    )
    resampler <- factor(resampler)
    if (!return_factor) {
      resampler <- as.resampling(resampler) # nolint
    }
    resampling[[as.character(cnt)]] <- resampler
  }
  if (!return_factor) {
    resampling <- as.represampling(resampling) # nolint
  }

  return(resampling)
}


#' Partition the data for a stratified (non-spatial) cross-validation
#'
#' \code{partition_cv_strat} creates a set of sample indices corresponding
#' to cross-validation test and training sets.
#'
#' @name partition_cv_strat
#' @inheritParams partition_cv
#'
#' @param coords vector of length 2 defining the variables in \code{data} that
#' contain the x and y coordinates of sample locations
#' @param strat character: column in \code{data} containing a factor variable
#' over which the partitioning should be stratified; or factor vector of length
#' \code{nrow(data)}: variable over which to stratify
#'
#' @return A \code{\link{represampling}} object, see also
#' \code{\link{partition_cv}}. \code{partition_strat_cv}, however,
#' stratified with respect to the variable \code{data[,strat]};
#' i.e., cross-validation partitioning is done within each set
#' \code{data[data[,strat]==i,]} (\code{i} in \code{levels(data[, strat])}), and
#' the \code{i}th folds of all levels are combined into one cross-validation
#' fold.
#'
#' @seealso \code{\link{sperrorest}}, \code{\link{as.resampling}},
#' \code{\link{resample_strat_uniform}}
#'
#' @examples
#' data(ecuador)
#' parti <- partition_cv_strat(ecuador,
#'   strat = "slides", nfold = 5,
#'   repetition = 1
#' )
#' idx <- parti[["1"]][[1]]$train
#' mean(ecuador$slides[idx] == "TRUE") / mean(ecuador$slides == "TRUE")
#' # always == 1
#' # Non-stratified cross-validation:
#' parti <- partition_cv(ecuador, nfold = 5, repetition = 1)
#' idx <- parti[["1"]][[1]]$train
#' mean(ecuador$slides[idx] == "TRUE") / mean(ecuador$slides == "TRUE")
#' # close to 1 because of large sample size, but with some random variation
#' @export
#' @noMd
partition_cv_strat <- function(data, coords = c("x", "y"), nfold = 10,
                               return_factor = FALSE, repetition = 1,
                               seed1 = NULL, strat) {
  repres <- list()

  if (length(repetition) < 2) {
    repetition <- seq(1, repetition, by = 1)
  }

  stopifnot((length(strat) == 1) | (length(strat) == nrow(data))) # nolint
  if (length(strat) == 1) {
    strat <- data[, strat]
  }
  stopifnot(is.factor(strat))
  # Can't split into nfold partitions if there are less than nfold samples
  # within a stratum:
  minstrat <- min(tapply(strat, strat, length))
  stopifnot(minstrat >= nfold)

  for (cnt in repetition) {
    if (!is.null(seed1)) {
      set.seed(seed1 + cnt) # nocov
    }
    fac <- rep(NA, nrow(data))
    for (lev in levels(strat)) {
      nstrat <- sum(sel <- (strat == lev))
      fac[sel] <- sample(rep(sample(nfold), length = nstrat), size = nstrat)
    }
    fac <- factor(fac)
    if (!return_factor) {
      fac <- as.resampling(fac) # nolint
    }
    repres[[as.character(cnt)]] <- fac # nolint
  }
  if (!return_factor) {
    repres <- as.represampling(repres) # nolint
  }

  return(repres)
}

#' Partition the data for a (non-spatial) leave-one-factor-out cross-validation
#' based on a given, fixed partitioning
#'
#' `partition_factor` creates a [represampling] object, i.e. a set of sample
#' indices defining cross-validation test and training sets.
#'
#' @inheritParams partition_cv
#'
#' @param coords vector of length 2 defining the variables in `data` that
#' contain the x and y coordinates of sample locations.
#' @param fac either the name of a variable (column) in `data`, or a vector
#' of type factor and length `nrow(data)` that contains the partitions
#' to be used for defining training and test samples.
#'
#' @return A [represampling] object,
#' see also [partition_cv] for details.
#'
#' @note In this partitioning approach, all `repetition`s are identical and
#' therefore pseudo-replications.
#'
#' @seealso [sperrorest], [partition_cv],
#' [as.resampling.factor]
#'
#' @examples
#' data(ecuador)
#' # I don't recommend using this partitioning for cross-validation,
#' # this is only for demonstration purposes:
#' breaks <- quantile(ecuador$dem, seq(0, 1, length = 6))
#' ecuador$zclass <- cut(ecuador$dem, breaks, include.lowest = TRUE)
#' summary(ecuador$zclass)
#' parti <- partition_factor(ecuador, fac = "zclass")
#' # plot(parti,ecuador)
#' summary(parti)
#' @export
partition_factor <- function(data, coords = c("x", "y"), fac,
                             return_factor = FALSE,
                             repetition = 1) {

  if (length(repetition) < 2) {
    repetition <- seq(1, repetition, by = 1)
  }

  if (length(fac) == 1 && is.character(fac)) {
    fac <- data[, fac]
  }
  fac <- factor(fac)
  if (!return_factor) {
    fac <- as.resampling(fac) # nolint
  }
  represmp <- list()
  for (cnt in repetition) {
    represmp[[as.character(cnt)]] <- fac
  }
  if (!return_factor) {
    represmp <- as.represampling(represmp) # nolint
  }
  return(represmp)
}

#' Partition the data for a (non-spatial) k-fold cross-validation at the group
#' level
#'
#' `partition_factor_cv` creates a [represampling] object,
#' i.e. a set of sample indices defining cross-validation test and
#' training sets, where partitions are obtained by resampling at the level of
#' groups of observations as defined by a given factor variable.
#' This can be used, for example, to resample agricultural data that is grouped
#' by fields, at the agricultural field level in order to preserve
#' spatial autocorrelation within fields.
#'
#' @inheritParams partition_cv
#'
#' @param coords vector of length 2 defining the variables in `data`
#' that contain the x and y coordinates of sample locations.
#' @param fac either the name of a variable (column) in `data`, or a
#' vector of type factor and length `nrow(data)` that defines groups or
#' clusters of observations.
#'
#' @return A [represampling] object,
#' see also [partition_cv] for details.
#'
#' @note In this partitioning approach, the number of factor levels in
#' `fac` must be large enough for this factor-level resampling to make
#' sense.
#'
#' @seealso [sperrorest], [partition_cv],
#' [partition_factor], [as.resampling.factor]
#'
#' @export
partition_factor_cv <- function(data, coords = c("x", "y"), fac, nfold = 10,
                                repetition = 1, seed1 = NULL,
                                return_factor = FALSE) {

  if (length(repetition) < 2) {
    repetition <- seq(1, repetition, by = 1)
  }

  if (length(fac) == 1 && is.character(fac)) {
    fac <- data[, fac]
  }
  fac <- factor(fac)
  if (nfold > nlevels(fac)) {
    # nocov start
    warning("'nfold' should be <= nlevels(fac); using nfold=nlevels(fac)\n")
    # nocov end
    nfold <- nlevels(fac) # nocov
  }
  resampling <- list()
  for (cnt in repetition) {
    if (!is.null(seed1)) {
      set.seed(seed1 + cnt) # nocov
    }
    fac_resampler <- sample(rep(sample(nfold), length = nlevels(fac)),
      size = nlevels(fac)
    )
    names(fac_resampler) <- levels(fac)
    resampler <- factor(fac_resampler[fac])
    if (!return_factor) {
      resampler <- as.resampling(resampler) # nolint
    }
    resampling[[as.character(cnt)]] <- resampler
  }
  if (!return_factor) {
    resampling <- as.represampling(resampling) # nolint
  }
  return(resampling)
}


#' Partition the study area into rectangular tiles
#'
#' `partition_tiles` divides the study area into a specified number of
#' rectangular tiles. Optionally small partitions can be merged with adjacent
#' tiles to achieve a minimum number or percentage of samples in each tile.
#'
#' @inheritParams partition_cv
#'
#' @param coords vector of length 2 defining the variables in `data` that
#' contain the x and y coordinates of sample locations
#'
#' @param dsplit optional vector of length 2: equidistance of splits in
#' (possibly rotated) x direction (`dsplit[1]`) and y direction
#' (`dsplit[2]`) used to define tiles. If `dsplit` is of length 1,
#' its value is recycled. Either `dsplit` or `nsplit` must be specified.
#'
#' @param nsplit optional vector of length 2: number of splits in
#' (possibly rotated) x direction (`nsplit[1]`) and y direction
#' (`nsplit[2]`) used to define tiles. If `nsplit` is of length 1,
#' its value is recycled.
#'
#' @param rotation indicates whether and how the rectangular grid should
#' be rotated; random rotation is only between `-45` and `+45` degrees.
#'
#' @param user_rotation if `rotation='user'`, angles (in degrees) by which
#' the rectangular grid is to be rotated in each repetition. Either a vector of
#' same length as `repetition`, or a single number that will be replicated
#' `length(repetition)` times.
#'
#' @param offset indicates whether and how the rectangular grid should be
#' shifted by an offset.
#'
#' @param user_offset if `offset='user'`, a list (or vector) of two
#' components specifying a shift of the rectangular grid in (possibly rotated) x
#' and y direction. The offset values are relative values, a value of `0.5`
#' resulting in a one-half tile shift towards the left, or upward.
#' If this is a list, its first (second) component refers to the rotated x (y)
#' direction, and both components must have same length as `repetition`
#' (or length 1). If a vector of length 2 (or list components have length 1),
#' the two values will be interpreted as relative shifts in (rotated) x and y
#' direction, respectively, and will therefore be recycled as needed
#' (`length(repetition)` times each).
#'
#' @param reassign logical (default `TRUE`): if `TRUE`, 'small' tiles
#' (as per `min_frac` and `min_n` arguments and
#' [get_small_tiles]) are merged with (smallest) adjacent tiles.
#' If `FALSE`, small tiles are 'eliminated', i.e. set to `NA`.
#'
#' @param min_frac numeric >=0, <1: minimum relative size of partition as
#' percentage of sample; argument passed to [get_small_tiles].
#' Will be ignored if `NULL`.
#'
#' @param min_n integer >=0: minimum number of samples per partition;
#' argument passed to [get_small_tiles].
#' Will be ignored if `NULL`.
#'
#' @param iterate argument to be passed to [tile_neighbors]
#'
#' @return A [represampling] object.
#' Contains `length(repetition)` [resampling] objects as
#' repetitions. The exact number of folds / test-set tiles within each
#' [resampling] objects depends on the spatial configuration of
#' the data set and possible cleaning steps (see `min_frac`, `min_n`).
#'
#' @note Default parameter settings may change in future releases.
#' This function, especially the rotation and shifting part of it and the
#' algorithm for cleaning up small tiles is still a bit experimental.
#' Use with caution.
#' For non-zero offsets (`offset!='none')`), the number of tiles may
#' actually be greater than `nsplit[1]*nsplit[2]` because of fractional
#' tiles lurking into the study region. `reassign=TRUE` with suitable
#' thresholds is therefore recommended for non-zero (including random) offsets.
#'
#' @seealso [sperrorest], [as.resampling.factor],
#' [get_small_tiles], [tile_neighbors]
#'
#' @examples
#' data(ecuador)
#' parti <- partition_tiles(ecuador, nsplit = c(4, 3), reassign = FALSE)
#' # plot(parti,ecuador)
#' # tile A4 has only 55 samples
#' # same partitioning, but now merge tiles with less than 100 samples to
#' # adjacent tiles:
#' parti2 <- partition_tiles(ecuador,
#'   nsplit = c(4, 3), reassign = TRUE,
#'   min_n = 100
#' )
#' # plot(parti2,ecuador)
#' summary(parti2)
#' # tile B4 (in 'parti') was smaller than A3, therefore A4 was merged with B4,
#' # not with A3
#' # now with random rotation and offset, and tiles of 2000 m length:
#' parti3 <- partition_tiles(ecuador,
#'   dsplit = 2000, offset = "random",
#'   rotation = "random", reassign = TRUE, min_n = 100
#' )
#' # plot(parti3, ecuador)
#' summary(parti3)
#' @export
partition_tiles <- function(data, coords = c("x", "y"), dsplit = NULL,
                            nsplit = NULL,
                            rotation = c("none", "random", "user"),
                            user_rotation, offset = c("none", "random", "user"),
                            user_offset, reassign = TRUE, min_frac = 0.025,
                            min_n = 5, iterate = 1,
                            return_factor = FALSE, repetition = 1,
                            seed1 = NULL) {

  if (length(repetition) < 2) {
    repetition <- seq(1, repetition, by = 1)
  }

  # Some basic argument checks:
  stopifnot(is.numeric(min_frac) && length(min_frac) == 1)
  stopifnot(is.numeric(min_n) && length(min_n) == 1)
  stopifnot(is.numeric(iterate) && length(iterate) == 1)
  stopifnot(!is.null(nsplit) | !is.null(dsplit))

  if (!is.null(nsplit)) {
    stopifnot(is.numeric(nsplit) && length(nsplit) <= 2)
  } else {
    stopifnot(is.numeric(dsplit) && length(dsplit) <= 2)
  }

  # Prepare rotation angles, if applicable:
  rotation <- match.arg(rotation)
  stopifnot(xor(rotation == "user", missing(user_rotation)))

  if (rotation == "none") {
    phi <- rep(0, length(repetition))
    # nocov start
  } else if (rotation == "random") {
    phi <- runif(-45, 45, n = length(repetition))
  } else if (rotation == "user") {
    if (length(user_rotation) == 1) {
      user_rotation <- rep(user_rotation, length(repetition))
    }
    stopifnot(length(user_rotation) == length(repetition))
    phi <- user_rotation # nocov end
  }
  names(phi) <- as.character(repetition)

  # This will make matrix multiplication (rotation) numerically better
  # conditioned:
  data[, coords[1]] <- data[, coords[1]] - mean(data[, coords[1]])
  data[, coords[2]] <- data[, coords[2]] - mean(data[, coords[2]])

  offset <- match.arg(offset)
  stopifnot(xor(offset == "user", missing(user_offset)))
  if (offset == "none") {
    x_shift <- y_shift <- rep(0, length(repetition))
  } else if (offset == "random") {
    x_shift <- runif(0, 1, n = length(repetition)) # nocov start
    y_shift <- runif(0, 1, n = length(repetition))
  } else if (offset == "user") {
    if (is.vector(user_offset) && length(user_offset) == 2) {
      user_offset <- list(user_offset[1], user_offset[2])
    }
    stopifnot(is.list(user_offset) && length(user_offset) == 2)
    # Recycle offsets as needed:
    if (length(user_offset[[1]]) == 1) {
      user_offset[[1]] <- rep(user_offset[[1]], length(repetition))
    }
    if (length(user_offset[[2]]) == 1) {
      user_offset[[2]] <- rep(user_offset[[2]], length(repetition))
    }
    # Got enough user_offsets?
    stopifnot(length(user_offset[[1]]) == length(repetition))
    stopifnot(length(user_offset[[2]]) == length(repetition))
    # Valid range, [0,1]?
    stopifnot(min(user_offset[[1]] >= 0 & max(user_offset[[1]]) <= 1))
    stopifnot(min(user_offset[[2]] >= 0 & max(user_offset[[2]]) <= 1))
    x_shift <- user_offset[[1]]
    y_shift <- user_offset[[2]] # nocov end
  }
  names(x_shift) <- as.character(repetition)
  names(y_shift) <- as.character(repetition)

  if (!is.null(nsplit)) {
    if (length(nsplit) == 1) {
      nsplit <- c(nsplit, nsplit) # nocov
    }
  }
  if (!is.null(dsplit)) {
    # nocov start
    if (length(dsplit) == 1) {
      dsplit <- c(dsplit, dsplit)
    } # nocov end
  }

  resampling <- list()
  for (cnt in repetition) {

    if (!is.null(seed1)) {
      set.seed(seed1 + cnt) # nocov
    }

    # Prepare the arguments and data:

    if (rotation != "none") {
      r <- phi[as.character(cnt)] * 180 / pi # nocov start
      r <- matrix(c(cos(r), -sin(r), sin(r), cos(r)), ncol = 2)
      xy <- r %*% t(data[, coords])
      x <- xy[1, ]
      y <- xy[2, ] # nocov end
    } else {
      x <- data[, coords[1]]
      y <- data[, coords[2]]
    }
    x_range <- range(x)
    y_range <- range(y)

    if (!is.null(nsplit)) {
      x_delta <- diff(x_range) / nsplit[1]
      y_delta <- diff(y_range) / nsplit[2]
      my_nsplit <- nsplit
    } else {
      # if !is.null(dsplit)
      x_delta <- dsplit[1] # nocov
      y_delta <- dsplit[2] # nocov
    }
    # Apply offsets:
    if (offset != "none") {
      # Widen the range and increase nsplit to allow for 'lurking' tiles:
      x_range[2] <- x_range[2] + x_delta # nocov start
      y_range[2] <- y_range[2] + y_delta
      x_range <- x_range - x_delta * (x_shift[as.character(cnt)])
      y_range <- y_range - y_delta * (y_shift[as.character(cnt)])
      if (is.null(dsplit)) {
        my_nsplit <- my_nsplit + 1
      } # nocov end
    }

    # Calculate x and y splits:
    if (is.null(dsplit)) {
      x_split <- seq(x_range[1], x_range[2], length = my_nsplit[1] + 1)
      y_split <- seq(y_range[1], y_range[2], length = my_nsplit[2] + 1)
    } else {
      # nocov start
      x_split <- seq(x_range[1], x_range[2] + x_delta, by = x_delta)
      y_split <- seq(y_range[1], y_range[2] + y_delta, by = y_delta)
      my_nsplit <- c(length(x_split) - 1, length(y_split) - 1) # nocov end
    }

    # Group data into tiles, i.e. assign tile labels to samples:
    tile <- rep(NA, nrow(data))

    for (ix in 1:my_nsplit[1]) {
      # Intervals are normally open to the left, except the first one:
      if (ix == 1) {
        sel_x <- (x >= x_split[ix]) & (x <= x_split[ix + 1])
      } else {
        sel_x <- (x > x_split[ix]) & (x <= x_split[ix + 1])
      }
      for (iy in 1:my_nsplit[2]) {
        if (iy == 1) {
          sel_y <- (y >= y_split[iy]) & (y <= y_split[iy + 1])
        } else {
          sel_y <- (y > y_split[iy]) & (y <= y_split[iy + 1])
        }
        # Assign tile name to samples:
        if (any(sel_x & sel_y)) {
          tile[sel_x & sel_y] <- as.character(as.tilename(c(ix, iy))) # nolint
        }
      }
    }
    tile <- factor(tile)

    # Identify and process small tiles:
    s_tiles <- get_small_tiles(tile, min_n = min_n, min_frac = min_frac) # nolint
    if (length(s_tiles) > 0) {
      # any small tiles?
      # nocov start
      if (reassign) {
        # Merge small tiles with neighbors:
        ignore <- c()
        # Repeat until no small tiles are left:
        while ((length(s_tiles) > 0) & (length(levels(tile)) > 1)) { # nolint
          # Start with smallest small tile:
          nbrs <- tile_neighbors(s_tiles[1],
            tileset = levels(tile),
            iterate = iterate
          )
          if (length(nbrs) == 0) {
            ignore <- c(ignore, as.character(s_tiles[1]))
          } else {
            # Merge tile with smallest neighbour to keep tile sizes balanced:
            n_tile <- tapply(tile, tile, length)
            s_nbr <- nbrs[which.min(n_tile[nbrs])]
            tile[tile == s_tiles[1]] <- s_nbr
            tile <- factor(as.character(tile))
          }
          # Update small tiles list:
          s_tiles <- get_small_tiles(tile,
            min_n = min_n, min_frac = min_frac,
            ignore = ignore
          )
        }
      } else {
        # Just eliminate small tiles:
        tile[tile %in% s_tiles] <- NA
        tile <- factor(as.character(tile))
      } # nocov end
    }

    if (!return_factor) {
      tile <- as.resampling(tile) # nolint
    }
    resampling[[as.character(cnt)]] <- tile
  }

  if (!return_factor) {
    resampling <- as.represampling(resampling) # nolint
  }

  return(resampling)
}




# Function 'partition_kmeans' --------------------------Uses k-means clustering
# to divide the samples into 'nfold' spatial clusters and perform spatial
# cross-validation.

#' Partition samples spatially using k-means clustering of the coordinates
#'
#' `partition_kmeans` divides the study area into irregularly shaped
#' spatial partitions based on \emph{k}-means ([kmeans]) clustering
#' of spatial coordinates.
#'
#' @inheritParams partition_cv
#'
#' @importFrom stats kmeans
#'
#' @param coords vector of length 2 defining the variables in `data` that
#' contain the x and y coordinates of sample locations.
#'
#' @param nfold number of cross-validation folds, i.e. parameter \emph{k} in
#' \emph{k}-means clustering.
#'
#' @param balancing_steps if `> 1`, perform `nfold`-means clustering
#' `balancing_steps` times, and pick the clustering that minimizes the Gini
#' index of the sample size distribution among the partitions. The idea is that
#' 'degenerate' partitions will be avoided, but this also has the side effect of
#' reducing variation among partitioning repetitions. More meaningful
#' constraints (e.g., minimum number of positive and negative samples within
#' each partition should be added in the future.
#'
#' @param order_clusters if `TRUE`, clusters are ordered by increasing x
#' coordinate of center point.
#'
#' @param ... additional arguments to [kmeans].
#'
#' @return A [represampling] object, see also
#' [partition_cv] for details.
#'
#' @note Default parameter settings may change in future releases.
#'
#' @references Brenning, A., Long, S., & Fieguth, P. (2012).
#' Detecting rock glacier flow structures using Gabor filters and IKONOS
#' imagery. Remote Sensing of Environment, 125, 227-237.
#' doi:10.1016/j.rse.2012.07.005
#'
#' Russ, G. & A. Brenning. 2010a. Data mining in precision agriculture:
#' Management of spatial information. In 13th International Conference on
#' Information Processing and Management of Uncertainty,
#' IPMU 2010; Dortmund; 28 June - 2 July 2010.
#' Lecture Notes in Computer Science, 6178 LNAI: 350-359.
#'
#' @seealso [sperrorest], [partition_cv],
#' [partition_disc], [partition_tiles],
#' [kmeans]
#'
#' @examples
#' data(ecuador)
#' resamp <- partition_kmeans(ecuador, nfold = 5, repetition = 2)
#' # plot(resamp, ecuador)
#' @export
partition_kmeans <- function(data, coords = c("x", "y"), nfold = 10,
                             repetition = 1, seed1 = NULL,
                             return_factor = FALSE, balancing_steps = 1,
                             order_clusters = TRUE, ...) {

  if (length(repetition) < 2) {
    repetition <- seq(1, repetition, by = 1)
  }

  balancing_steps <- max(1, balancing_steps)

  resampling <- list()
  for (cnt in repetition) {
    if (!is.null(seed1)) {
      set.seed(seed1 + cnt) # nocov
    }
    kms <- list()
    for (i in 1:balancing_steps) {
      kms[[i]] <- kmeans(data[, coords], centers = nfold, ...)
    }
    kmgini <- function(x) {
      p <- x$size / sum(x$size)
      return(1 - sum(p^2))
    }
    km <- kms[[which.max(sapply(kms, kmgini))]]
    # To do: add more meaningful selection criteria such as minimum number of
    # positives and negatives in each partition ???
    if (order_clusters) {
      o <- rank(km$centers[, 1], ties.method = "first")
      km$cluster <- o[km$cluster]
    }
    # The clusters are the partitions:
    tile <- factor(km$cluster)

    if (!return_factor) {
      tile <- as.resampling(tile) # nolint
    }
    resampling[[as.character(cnt)]] <- tile
  }
  if (!return_factor) {
    resampling <- as.represampling(resampling) # nolint
  }

  return(resampling)
}


#' Leave-one-disc-out cross-validation and leave-one-out cross-validation
#'
#' `partition_disc` partitions the sample into training and tests set by
#' selecting circular test areas (possibly surrounded by an exclusion buffer)
#' and using the remaining samples as training samples (leave-one-disc-out
#' cross-validation). `partition_loo` creates training and test sets for
#' leave-one-out cross-validation with (optional) buffer.
#'
#' @name partition_disc
#'
#' @inheritParams partition_cv
#'
#' @param coords vector of length 2 defining the variables in `data` that
#' contain the x and y coordinates of sample locations.
#'
#' @param radius radius of test area discs; performs leave-one-out resampling
#' if radius <0.
#'
#' @param buffer radius of additional 'neutral area' around test area discs
#' that is excluded from training and test sets; defaults to 0,
#' i.e. all samples are either in the test area or in the training area.
#'
#' @param ndisc Number of discs to be randomly selected; each disc constitutes
#' a separate test set. Defaults to `nrow(data)`, i.e. one disc around
#' each sample.
#'
#' @param return_train If `FALSE`, returns only test sample;
#' if `TRUE`, also the training area.
#'
#' @param prob optional argument to [sample].
#'
#' @param replace optional argument to [sample]: sampling with or
#' without replacement?
#'
#' @param repetition see `partition_cv`; however,
#' see Note below: `repetition` should normally be `= 1` in this function.
#'
#' @param ... arguments to be passed to `partition_disc`
#'
#' @return A [represampling] object.
#' Contains `length(repetition)` `resampling` objects.
#' Each of these contains `ndisc` lists with indices of test and
#' (if `return_train = TRUE`) training sets.
#'
#' @note Test area discs are centered at (random) samples, not at general
#' random locations. Test area discs may (and likely will) overlap independently
#' of the value of `replace`. `replace` only controls the replacement
#' of the center point of discs when drawing center points from the samples.
#'
#' `radius < 0` does leave-one-out resampling with an optional buffer.
#' `radius = 0` is similar except that samples with identical coordinates
#' would fall within the test area disc.
#'
#' @references Brenning, A. 2005. Spatial prediction models for landslide
#' hazards: review, comparison and evaluation. Natural Hazards and Earth System
#' Sciences, 5(6): 853-862.
#'
#' @seealso [sperrorest], [partition_cv],
#' [partition_kmeans]
#'
#' @examples
#' data(ecuador)
#' parti <- partition_disc(ecuador,
#'   radius = 200, buffer = 200,
#'   ndisc = 5, repetition = 1:2
#' )
#' # plot(parti,ecuador)
#' summary(parti)
#'
#' # leave-one-out with buffer:
#' parti.loo <- partition_loo(ecuador, buffer = 200)
#' summary(parti)
#' @export
partition_disc <- function(data, coords = c("x", "y"), radius, buffer = NULL,
                           ndisc = nrow(data), seed1 = NULL,
                           return_train = TRUE, prob = NULL, replace = FALSE,
                           repetition = 1) {

  if (length(repetition) < 2) {
    repetition <- seq(1, repetition, by = 1)
  }

  posbuf <- buffer
  if (is.null(buffer)) {
    # pospuf <- 0
  } else {
    stopifnot(buffer >= 0)
  }

  if (replace == FALSE & ndisc > nrow(data)) {
    stop("partition_disc: ndisc must be > nrow(data) if replace=FALSE") # nocov
  }

  resample <- list()

  # Loop for repetitions:
  for (cnt in repetition) {
    if (!is.null(seed1)) {
      set.seed(seed1 + cnt) # nocov
    }
    if (ndisc == nrow(data)) {
      index <- c(1:nrow(data)) # nocov
    } else {
      index <- sample.int(nrow(data),
        size = ndisc, replace = replace,
        prob = prob
      )
    }

    res <- list()
    for (i in index) {
      if (!is.null(buffer) | radius >= 0) {
        di <- sqrt((data[, coords[1]] - data[i, coords[1]])^2 + # nolint
          (data[, coords[2]] - data[i, coords[2]])^2) # nolint
      }
      train_sel <- numeric()
      if (radius >= 0) {
        # leave-disc-out with buffer:
        test_sel <- which(di <= radius)
        if (return_train) {
          train_sel <- which(di > (radius + posbuf))
        }
      } else {
        # leave-one-out with buffer:
        test_sel <- i
        if (return_train) {
          if (is.null(buffer)) {
            train_sel <- c(1:nrow(data))[-i] # nocov
          } else {
            train_sel <- which(di > posbuf)
          }
        }
      }
      if (return_train & (length(train_sel) == 0)) {
        warning(paste0(
          "empty training set in 'partition_disc': 'buffer'", # nocov  #nolint
          " and/or 'radius' too large?"
        )) # nocov
      }
      res[[as.character(i)]] <- list(train = train_sel, test = test_sel)
    }
    resample[[as.character(cnt)]] <- res
  }
  repres <- as.represampling(resample) # nolint

  return(repres)
}


#' @rdname partition_disc
#'
#' @name partition_loo
#'
#' @export
partition_loo <- function(data, ndisc = nrow(data), replace = FALSE, ...) {
  partition_disc(
    data = data, radius = -1, ndisc = ndisc,
    replace = replace, ...
  )
}

#' Non-spatial bootstrap resampling
#'
#' `represampling_bootstrap` draws a bootstrap random sample
#' (with replacement) from `data`.
#'
#' @inheritParams partition_cv
#'
#' @param coords vector of length 2 defining the variables in `data` that
#' contain the x and y coordinates of sample locations.
#' @param nboot Size of bootstrap sample
#' @param oob logical (default `FALSE`): if `TRUE`, use the out-of-bag
#' sample as the test sample; if `FALSE`, draw a second bootstrap sample of
#' size `nboot` independently to obtain a test sample.
#'
#' @return A [represampling] object. This is a (named) list
#' containing `length(repetition)`.
#' [resampling] objects. Each of these contains only one list with
#' indices of `train`ing and `test` samples.
#' Indices are row indices for `data`.
#'
#' @examples
#' data(ecuador)
#' # only 10 bootstrap repetitions, normally use >=100:
#' parti <- represampling_bootstrap(ecuador, repetition = 10)
#' # plot(parti, ecuador) # careful: overplotting occurs
#' # because some samples are included in both the training and
#' # the test sample (possibly even multiple times)
#' @export
represampling_bootstrap <- function(data, coords = c("x", "y"),
                                    nboot = nrow(data),
                                    repetition = 1, seed1 = NULL, oob = FALSE) {
  resample <- list()

  if (length(repetition) < 2) {
    repetition <- seq(1, repetition, by = 1)
  }

  for (cnt in repetition) {
    if (!is.null(seed1)) {
      set.seed(seed1 + cnt) # nocov
    }
    # Bootstrap training sample, drawn with replacement:
    train <- sample(nrow(data), nboot, replace = TRUE)
    if (oob) {
      # test set = out of bag sample:
      test <- c(1:nrow(data))[!(c(1:nrow(data)) %in% train)] # nocov
    } else {
      # test set = independently drawn bootstrap sample
      test <- sample(nrow(data), nboot, replace = TRUE)
    }
    resample[[as.character(cnt)]] <- list(`1` = list(
      train = train,
      test = test
    ))
  }
  return(as.represampling(resample)) # nolint
}

#' Bootstrap at an aggregated level
#'
#' `represampling_factor_bootstrap` resamples partitions defined by a
#' factor variable. This can be used for non-overlapping block bootstraps and
#' similar.
#'
#' @inheritParams represampling_bootstrap
#'
#' @param fac defines a grouping or partitioning of the samples in `data`;
#' three possible types:
#' (1) the name of a variable in `data` (coerced to factor if not already
#' a factor variable);
#' (2) a factor variable (or a vector that can be coerced to factor);
#' (3) a list of factor variables (or vectors that can be coerced to factor);
#' this list must be of length `length(repetition)`, and if it is named,
#' the names must be equal to `as.character(repetition)`; this list will
#' typically be generated by a `partition.*` function with
#' `return_factor = TRUE` (see Examples below)
#'
#' @param nboot number of bootstrap replications used for generating the
#' bootstrap training sample (`nboot[1]`) and the test sample
#' (`nboot[2]`); `nboot[2]` is ignored (with a warning) if
#' `oob = TRUE`. A value of `-1` will be substituted with the number
#' of levels of the factor variable, corresponding to an \emph{n} out of
#' \emph{n} bootstrap at the grouping level defined by `fac`.
#'
#' @param oob if `TRUE`, the test sample will be the out-of-bag sample;
#' if `FALSE` (default), the test sample is an independently drawn
#' bootstrap sample of size `nboot[2]`.
#'
#' @details `nboot` refers to the number of groups
#' (as defined by the factors) to be drawn with replacement from the set of
#' groups. I.e., if `fac` is a factor variable, `nboot` would normally
#' not be greater than `nlevels(fac)`, `nlevels(fac)` being the
#' default as per `nboot = -1`.
#'
#' @seealso [represampling_disc_bootstrap],
#' [represampling_tile_bootstrap]
#'
#' @examples
#' data(ecuador)
#' # a dummy example for demonstration, performing bootstrap
#' # at the level of an arbitrary factor variable:
#' parti <- represampling_factor_bootstrap(ecuador,
#'   factor(floor(ecuador$dem / 100)),
#'   oob = TRUE
#' )
#' # plot(parti,ecuador)
#' # using the factor bootstrap for a non-overlapping block bootstrap
#' # (see also represampling_tile_bootstrap):
#' fac <- partition_tiles(ecuador,
#'   return_factor = TRUE, repetition = c(1:3),
#'   dsplit = 500, min_n = 200, rotation = "random",
#'   offset = "random"
#' )
#' parti <- represampling_factor_bootstrap(ecuador, fac,
#'   oob = TRUE,
#'   repetition = c(1:3)
#' )
#' # plot(parti, ecuador)
#' @export
represampling_factor_bootstrap <- function(data, fac, repetition = 1,
                                           nboot = -1,
                                           seed1 = NULL, oob = FALSE) {

  if (length(repetition) < 2) {
    repetition <- seq(1, repetition, by = 1)
  }

  if (oob && length(nboot) > 1) {
    warning("nboot[2] ignored because 'oob = TRUE'") # nocov
  }
  if (is.list(fac)) {
    stopifnot(length(fac) == length(repetition))
    if (is.null(names(fac))) {
      names(fac) <- as.character(repetition) # nocov
    } else {
      stopifnot(all(as.character(repetition) %in% names(fac)))
    }
  } else {
    if (length(fac) == 1 && is.character(fac)) {
      fac <- data[, fac] # nocov
    } else {
      stopifnot(length(fac) == nrow(data))
    }
    fac <- factor(fac)
  }
  if (length(nboot) == 1) {
    nboot <- rep(nboot, 2)
  }

  resample <- list()

  for (cnt in repetition) {
    if (!is.null(seed1)) {
      set.seed(seed1 + cnt) # nocov
    }
    # what factor variable to resample?:
    if (is.list(fac)) {
      the_fac <- factor(fac[[as.character(cnt)]])
    } else {
      the_fac <- fac
    }
    # How many bootstrap samples (at the factor level)?:
    the_nboot <- nboot
    if (the_nboot[1] < 0) {
      the_nboot[1] <- nlevels(the_fac)
    }
    if (the_nboot[2] < 0) {
      the_nboot[2] <- nlevels(the_fac)
    }
    # Factor levels to be used in training sample:
    train <- sample(levels(the_fac), the_nboot[1], replace = TRUE)
    # Factor levels to be used for test sample: out-of-bag, i.e. factors that
    # are not used for the training sample:
    if (oob) {
      test <- levels(the_fac)[!(levels(the_fac) %in% train)]
    } else {
      # second, independently drawn bootstrap sample at the factor level:
      test <- sample(levels(the_fac), the_nboot[2], replace = TRUE)
    }
    # Turn factor levels into indices:
    train <- unlist(sapply(train, function(x) which(the_fac != x)),
      use.names = FALSE
    )
    test <- unlist(sapply(test, function(x) which(the_fac == x)),
      use.names = FALSE
    )
    # Compile training and test indices into a resampling object:
    resample[[as.character(cnt)]] <- as.resampling(list(
      `1` =
        list(
          train = train,
          test = test
        )
    ))
  }

  resample <- as.represampling(resample) # nolint
  return(resample)
}


#' Spatial block bootstrap using rectangular blocks
#'
#' `represampling_tile_bootstrap` performs a non-overlapping spatial
#' block bootstrap by resampling at the level of rectangular partitions or
#' 'tiles' generated by [partition_tiles].
#'
#' @inheritParams represampling_bootstrap
#'
#' @param nboot see [represampling_factor_bootstrap]
#' @param oob see [represampling_factor_bootstrap]
#' @param ... additional arguments to be passed to [partition_tiles]
#'
#' @export
represampling_tile_bootstrap <- function(data, coords = c("x", "y"),
                                         repetition = 1, nboot = -1,
                                         seed1 = NULL, oob = FALSE, ...) {

  if (length(repetition) < 2) {
    repetition <- seq(1, repetition, by = 1)
  }

  parti <- partition_tiles(
    data = data, coords = coords,
    repetition = repetition,
    seed1 = seed1, return_factor = TRUE, ...
  )
  repres <- represampling_factor_bootstrap(
    data = data, fac = parti,
    repetition = repetition,
    seed1 = seed1, nboot = nboot,
    oob = oob
  )
  return(repres)
}


#' Spatial block bootstrap at the level of spatial k-means clusters
#'
#' `represampling_kmeans_bootstrap` performs a non-overlapping spatial
#' block bootstrap by resampling at the level of irregularly-shaped partitions
#' generated by [partition_kmeans].
#'
#' @inheritParams represampling_bootstrap
#'
#' @param nfold see [partition_kmeans]
#' @param nboot see [represampling_factor_bootstrap]
#' @param oob see [represampling_factor_bootstrap]
#' @param ... additional arguments to be passed to [partition_kmeans]
#'
#' @export
represampling_kmeans_bootstrap <- function(data, coords = c("x", "y"),
                                           repetition = 1,
                                           nfold = 10, nboot = nfold,
                                           seed1 = NULL, oob = FALSE, ...) {

  if (length(repetition) < 2) {
    repetition <- seq(1, repetition, by = 1)
  }

  parti <- partition_tiles(
    data = data, coords = coords,
    repetition = repetition,
    seed1 = seed1, return_factor = TRUE, ...
  )
  repres <- represampling_factor_bootstrap(
    data = data, fac = parti,
    repetition = repetition,
    seed1 = seed1, nboot = nboot,
    oob = oob
  )
  return(repres)
}


#' Overlapping spatial block bootstrap using circular blocks
#'
#' `represampling_disc_bootstrap` performs a spatial block bootstrap by
#' resampling at the level of rectangular partitions or 'tiles' generated by
#' `partition_tiles`.
#'
#' @inheritParams represampling_bootstrap
#'
#' @param oob logical (default `FALSE`): if `TRUE`, use the out-of-bag
#' sample as the test sample (the complement of the `nboot[1]` test set
#' discs, minus the buffer area as specified in the `...` arguments to
#' [partition_disc]); if `FALSE`, draw a second bootstrap
#' sample of size `nboot` independently to obtain a test sample
#' (sets of overlapping discs drawn with replacement).
#' @param nboot number of bootstrap samples; you may specify different values
#' for the training sample (`nboot[1]`) and for the test sample
#' (`nboot[2]`).
#' @param ... additional arguments to be passed to [partition_disc];
#' note that a `buffer` argument has not effect if `oob=FALSE`;
#' see example below
#'
#' @note Performs `nboot` out of `nrow(data)` resampling of circular
#' discs. This is an \emph{overlapping} spatial block bootstrap where the
#' blocks are circular.
#'
#' @examples
#' data(ecuador)
#' # Overlapping disc bootstrap:
#' parti <- represampling_disc_bootstrap(ecuador,
#'   radius = 200, nboot = 20,
#'   oob = FALSE
#' )
#' # plot(parti, ecuador)
#' # Note that a 'buffer' argument would make no difference because boostrap
#' # sets of discs are drawn independently for the training and test sample.
#' #
#' # Overlapping disc bootstrap for training sample, out-of-bag sample as test
#' # sample:
#' parti <- represampling_disc_bootstrap(ecuador,
#'   radius = 200, buffer = 200,
#'   nboot = 10, oob = TRUE
#' )
#' # plot(parti,ecuador)
#' @export
represampling_disc_bootstrap <- function(data, coords = c("x", "y"), nboot,
                                         repetition = 1,
                                         seed1 = NULL, oob = FALSE, ...) {

  if (length(repetition) < 2) {
    repetition <- seq(1, repetition, by = 1)
  }

  if (oob && length(nboot) > 1) {
    warning("nboot[2] ignored because oob = TRUE") # nocov
  }
  if (length(nboot) == 1) {
    nboot <- c(nboot, nboot)
  }

  # nocov start
  if (oob) {
    resample <- list()
    for (cnt in repetition) {
      if (!is.null(seed1)) {
        set.seed(seed1 + cnt)
      }
      train <- partition_disc(
        data = data, coords = coords,
        repetition = c(1:nboot[1]),
        replace = TRUE, ndisc = 1, seed1 = NULL,
        return_train = TRUE, ...
      )
      test <- c(1:nrow(data))
      for (i in 1:nboot[1]) {
        test <- test[test %in% train[[i]][[1]]$train] # yes, $train!
        train[[i]][[1]]$train <- NULL
      }
      if (length(test) == 0) {
        warning("empty test set in 'partition_disc.bootstrap':\n'buffer'
                and/or 'radius' and/or 'nboot' too large?")
      }
      train <- unname(unlist(train))
      resample[[as.character(cnt)]] <- list(`1` = list(
        train = train,
        test = test
      ))
    } # nocov end
  } else {
    resample <- list()
    for (cnt in repetition) {
      if (!is.null(seed1)) {
        set.seed(seed1 + cnt) # nocov
      }
      train <- partition_disc(
        data = data, coords = coords,
        repetition = c(1:nboot[1]),
        seed1 = NULL, replace = TRUE, ndisc = 1,
        return_train = FALSE, ...
      )
      train <- unname(unlist(train))
      test <- partition_disc(
        data = data, coords = coords,
        repetition = c(1:nboot[2]),
        seed1 = NULL, replace = TRUE, ndisc = 1,
        return_train = FALSE, ...
      )
      test <- unname(unlist(test))
      resample[[as.character(cnt)]] <- list(`1` = list(
        train = train,
        test = test
      ))
    }
  }

  resample <- as.represampling(resample) # nolint
  return(resample)
}



#' Plot spatial resampling objects
#'
#' `plot.represampling` displays the partitions or samples corresponding
#' arising from the resampling of a data set.
#'
#' @method plot represampling
#'
#' @name plot.represampling
#'
#' @param x a [represampling] resp. [resampling] object.
#' @param data a `data.frame` of samples containing at least the x and y
#' coordinates of samples as specified by `coords`.
#' @param coords vector of length 2 defining the variables in `data` that
#' contain the x and y coordinates of sample locations.
#' @param pch point symbol (to be passed to [points]).
#' @param wiggle_sd 'wiggle' the point locations in x and y direction to avoid
#' overplotting of samples drawn multiple times by bootstrap methods;
#' this is a standard deviation (in the units of the x/y coordinates) of a
#' normal distribution and defaults to 0 (no wiggling).
#' @param ... additional arguments to [plot].
#'
#' @note This function is not intended for samples obtained by resampling with
#' replacement (e.g., bootstrap) because training and test points will be
#' overplotted in that case. The size of the plotting region will also limit
#' the number of maps that can be displayed at once, i.e., the number of rows
#' (repetitions) and fields (columns).
#'
#' @examples
#' data(ecuador)
#' # non-spatial cross-validation:
#' resamp <- partition_cv(ecuador, nfold = 5, repetition = 1:2)
#' # plot(resamp, ecuador)
#' # spatial cross-validation using k-means clustering:
#' resamp <- partition_kmeans(ecuador, nfold = 5, repetition = 1:2)
#' # plot(resamp, ecuador)
#' @export
plot.represampling <- function(x, data, coords = c("x", "y"), pch = "+",
                               wiggle_sd = 0, ...) {
  # nocov start
  if (missing(data)) {
    stop("'data' argument missing")
  }
  stopifnot(wiggle_sd >= 0)
  resample <- x
  nr <- length(resample)
  nc <- max(sapply(resample, length))
  if (nr > 5) {
    warning("Probably too many repetitions length(x) to be able to\n
            plot represampling object x. Trying anyway...")
  }
  if (nr > 7) {
    warning("Probably too many folds max(sapply(x,length)) to\n
            be able to plot represampling object x.
            Trying anyway...")
  }
  op <- par(no.readonly = TRUE)
  par(
    mfrow = c(nr, nc), mar = c(2, 2, 3, 0.5), mgp = c(2, 0.7, 0), tcl = -0.3,
    cex = 0.5
  )
  for (i in 1:length(resample)) {
    for (j in 1:length(resample[[i]])) {
      seltrain <- resample[[i]][[j]]$train
      seltest <- resample[[i]][[j]]$test
      main <- paste("Repetition ", names(resample)[i], ", Fold ", j)
      plot(data[, coords[1]], data[, coords[2]],
        pch = ".", type = "n",
        main = main,
        xlab = "", ylab = "", ...
      ) # xlab=coords[1], ylab=coords[2])
      wxtrain <- rnorm(length(seltrain), sd = wiggle_sd)
      wytrain <- rnorm(length(seltrain), sd = wiggle_sd)
      wxtest <- rnorm(length(seltest), sd = wiggle_sd)
      # wytest <- rnorm(length(seltest), sd = wiggle_sd)
      points(data[seltrain, coords[1]] + wxtrain, data[seltrain, coords[2]] +
        wytrain, pch = pch)
      points(data[seltest, coords[1]] + wxtest, data[seltest, coords[2]] +
        wxtest, pch = pch, col = "red")
    }
  }
  par(op)
}

#' @rdname plot.represampling
#' @name plot.resampling
#' @method plot resampling
plot.resampling <- function(x, ...) {
  x <- as.represampling(list(`1` = x)) # nolint
  plot.represampling(x) # nolint # nocov end
}
