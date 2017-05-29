#' Draw stratified random sample
#'
#' `resample_strat_uniform` draws a stratified random sample 
#' (with or without replacement) from the samples in `data`. 
#' Stratification is over the levels of `data[, param$response]`. 
#' The same number of samples is drawn within each level.
#' 
#' @param data a `data.frame`, rows represent samples
#' @param param a list with the following components: `strat` is either 
#' the name of a factor variable in `data` that defines the stratification
#'  levels, or a vector of type factor and length `nrow(data)`; 
#'  `n` is a numeric value specifying the size of the subsample; 
#'  `replace` determines if sampling is with or without replacement
#'  
#' @return a `data.frame` containing a subset of the rows of `data`.
#' 
#' @details If `param$replace=FALSE`, a subsample of size 
#' `min(param$n,nrow(data))` will be drawn from `data`. 
#' If `param$replace=TRUE`, the size of the subsample is `param$n`.
#' 
#' @seealso [resample_uniform()], [sample()]
#' 
#' @examples
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' d <- resample_strat_uniform(ecuador, 
#'                             param = list(strat = 'slides', nstrat = 100))
#' nrow(d) # == 200
#' sum(d$slides == 'TRUE') # == 100
#' 
#' @export
resample_strat_uniform <- function(data, param = list(strat = "class", 
                                                      nstrat = Inf, 
                                                      replace = FALSE))
{
  # Old version:
  if (!is.null(param$response)) {
    warning(paste0("'param$response' argument in 'resample_strat_uniform' ", # nocov
                   "renamed to 'strat';\n modify your code accordingly")) # nocov
    if (is.null(param$strat)) # nocov
      param$strat <- param$response # nocov
  } 
  
  # Use defaults if not specified:
  if (is.null(param$strat)) {
    param$strat <- "class"
  }
  if (is.null(param$nstrat)) {
    param$nstrat <- Inf
  }
  if (is.null(param$replace)) {
    param$replace <- FALSE
  }
  
  stopifnot((length(param$strat) == 1) | (length(param$strat) == nrow(data)))
  if (length(param$strat == 1)) {
    strat <- data[, param$strat]
  } else {
    strat <- param$strat
  }
  if (!is.factor(strat)) {
    stop("'strat' must either be a vector of factor type, or the name of 
         a factor variable in 'data'")
  }
  # Each factor level must have at least one sample, otherwise sampling within 
  # this level is impossible:
  minstrat <- min(tapply(strat, strat, length))
  stopifnot(minstrat >= 1)
  # might want to change this to a warning.???
  
  if (!param$replace) {
    param$nstrat <- min(param$nstrat, minstrat)
  }
  
  # Uniform sampling within each stratum:
  sel <- c()
  for (lev in levels(strat)) {
    wh <- sample(which(strat == lev), size = param$nstrat, 
                 replace = param$replace)
    sel <- c(sel, wh)
  }
  return(data[sel, ])
}
# To do: allow nstrat to be a named vector

#' Draw uniform random (sub)sample
#'
#' `resample_uniform` draws a random (sub)sample 
#' (with or without replacement) from the samples in `data`.
#' 
#' 
#' @param data a `data.frame`, rows represent samples
#' @param param a list with the following components: `n` is a numeric 
#' value specifying the size of the subsample; `replace` determines if 
#' sampling is with or without replacement
#' 
#' @return a `data.frame` containing a subset of the rows of `data`.
#' 
#' @details If `param$replace=FALSE`, a subsample of size 
#' `min(param$n,nrow(data))` will be drawn from `data`. 
#' If `param$replace=TRUE`, the size of the subsample is `param$n`.
#' 
#' @seealso [resample_strat_uniform()], [sample()]
#' 
#' @examples
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' d <- resample_uniform(ecuador, param = list(strat = 'slides', n = 200))
#' nrow(d) # == 200
#' sum(d$slides == 'TRUE')
#' 
#' @export
resample_uniform <- function(data, param = list(n = Inf, replace = FALSE)) {
  # Apply defaults if missing from parameter list:
  if (is.null(param$n)) {
    param$n <- Inf
  }
  if (is.null(param$replace)) {
    param$replace <- FALSE
  }
  
  if (!param$replace) {
    param$n <- min(param$n, nrow(data))
  }
  
  # Uniform sampling with or without replacement:
  sel <- sample(nrow(data), size = param$n, replace = param$replace)
  
  return(data[sel, ])
}

#' Draw uniform random (sub)sample at the group level
#'
#' `resample_factor` draws a random (sub)sample 
#' (with or without replacement) of the groups or clusters identified by 
#' the `fac` argument.
#' 
#' @param data a `data.frame`, rows represent samples
#' @param param a list with the following components: `fac` is a factor 
#' variable of length `nrow(data)` or the name of a factor variable 
#' in `data`; `n` is a numeric value specifying the size of the 
#' subsample (in terms of groups, not observations); `replace` determines 
#' if resampling of groups is to be done with or without replacement.
#' 
#' @return a `data.frame` containing a subset of the rows of `data`.
#' 
#' @details If `param$replace=FALSE`, a subsample of 
#' `min(param$n,nlevel(data[,fac]))` groups will be drawn from `data`. 
#' If `param$replace=TRUE`, the number of groups to be drawn is `param$n`.
#' 
#' @seealso [resample_strat_uniform()], [sample()]
#' 
#' @examples
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' d <- resample_uniform(ecuador, param = list(strat = 'slides', n = 200))
#' nrow(d) # == 200
#' sum(d$slides == 'TRUE')
#' 
#' @export
resample_factor <- function(data, param = list(fac = "class", n = Inf, 
                                               replace = FALSE)) {
  if (is.null(param$fac)) {
    param$fac <- "class"
  }
  if (is.null(param$replace)) {
    param$replace <- FALSE
  }
  stopifnot((length(param$fac) == 1) || (length(param$fac) == nrow(data)))
  if (length(param$fac == 1)) {
    fac <- data[, param$fac]
  } else {
    fac <- param$fac
  }
  if (!is.factor(fac)) {
    stop("'fac' must either be a vector of factor type, or the name of a 
         factor variable in 'data'")
  }
  fac <- factor(fac)
  if (is.null(param$n) || is.infinite(param$n)) {
    param$n <- nlevels(fac)
  }
  if (!param$replace) {
    param$n <- min(param$n, nrow(data))
  }
  sel <- sample(levels(fac), size = param$n, replace = param$replace)
  sel <- fac %in% sel
  return(data[sel, ])
}
