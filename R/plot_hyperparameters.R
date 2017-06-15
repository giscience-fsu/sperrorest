#' @title plot_hyper_svm
#' @description Plots tuning paramters cost and gamma of a tuned svm model
#' using an appropiate error measure.
#'
#' @param object a tuned object from [sptune_svm]
#'
#' @param error_measure an optional error measure to use which differs from the
#' default.
#'
#' @param gamma optional user-defined vector specyfing 'mtry' values to plot
#'
#' @param color_palette which color palette to use from [scale_colour_viridis]
#'
#' @import tibble
#' @import ggplot2
#' @import magrittr
#' @import hrbrthemes
#' @import viridis
#' @importFrom dplyr mutate filter arrange
#'
#' @export

plot_hyper_svm <- function(object = NULL, error_measure = NULL, gamma = NULL,
                           color_palette = NULL) {
  if (any(class(object$fit) == "svm")) {
    lib <- "e1071"
    # check kernel
    if (object$fit$kernel == 0) {
      kernel <- "linear"
    } else if (object$fit$kernel == 1) {
      kernel <- "polynomial"
    } else if (object$fit$kernel == 2) {
      kernel <- "radial"
    } else if (object$fit$kernel == 3) {
      kernel <- "sigmoid"
    }
  } else if (any(class(object$fit) == "ksvm")) {
    lib <- "kernlab"
    kernel <- class(object$fit@kernelf)[1]
  } else {
    stop("Passed 'svm_fun' is not supported.")
  }

  if (any(class(object$fit) == "svm")) {
    error_measure <- check_response_type(object$fit$fitted, error_measure)
  } else {
    error_measure <- check_response_type(object$fit@fitted, error_measure)
  }

  df <- tibble(cost = object$tune$all_costs, gamma = object$tune$all_gammas,
               var_error = round(object$tune$all_error_measures, 3))

  if (length(unique(df$gamma)) > 10) {
    message(paste0("Plotting of more than 10 'mtry' options is not supported.",
                   " Please specify your own vector of which 'mtry' values to",
                   " plot in argument 'mtry'.\n",
                   " Taking the first 10 values I can find."))

    # sort df
    df %>%
      arrange(gamma) -> df
    # get value of 10th index
    index <- unique(df$gamma)[10]

    # filter df
    df %>%
      filter(gamma <= index) -> df
  }

  # check for color palette
  if (is.null(color_palette)) {
    color_palette <- "viridis"
  }

  df %>%
    mutate(gamma = as.factor(gamma)) %>%
    ggplot(aes(x = cost, y = var_error, color = gamma, group = gamma)) +
    geom_point() +
    geom_line() +
    scale_color_viridis(discrete = TRUE, option = color_palette) +
    scale_x_continuous(trans = 'log10') +
    labs(x = "log10(cost)", y = error_measure,
         title = "Support Vector Machine hyperparameter tuning results",
         subtitle = sprintf(paste0("Total combinations: %s.",
                                   " Package: '%s'.",
                                   " Kernel: '%s'.",
                                   " Best '%s': %s. \n",
                                   "Optimal 'cost': %s.",
                                   " Optimal 'gamma': %s."),
                            length(object$tune$all_costs),
                            lib,
                            kernel,
                            toupper(error_measure),
                            round(object$tune$performances_best_run[[
                              error_measure]], 3),
                            object$tune$optimal_cost,
                            object$tune$optimal_gamma)) +
    theme_ipsum() +
    guides(color = guide_legend(title = "gamma"))
}

#' @title plot_hyper_rf
#' @description Plots tuning results of Random Forest
#'
#' @param object a fitted object of [randomForest] or [rfsrc].
#'
#' @param error_measure an optional error measure to use which differs from the
#' default.
#'
#' @param mtry optional user-defined numeric vector of 'mtry' values to plot.
#'
#' @param color_palette `option` argument from [scale_color_viridis].
#'
#' @import tibble
#' @import ggplot2
#' @import magrittr
#' @import hrbrthemes
#' @import viridis
#' @importFrom dplyr filter arrange mutate
#'
#' @examples
#' ##------------------------------------------------------------
#' ## regression
#' ##------------------------------------------------------------
#'
#' data(ecuador) # Muenchow et al. (2012), see ?ecuador
#' fo <- dem ~ slides + slope + hcurv + vcurv + log.carea + cslope
#'
#' out <- sptune_rf(fo, ecuador, step_factor = 16, nfold = 5,
#' partition_fun = "partition_kmeans", rf_fun = "randomForest")
#'
#' plot_hyper_rf(out)
#'
#' @export

plot_hyper_rf <- function(object = NULL, error_measure = NULL, mtry = NULL,
                          color_palette = NULL) {
  if (any(class(object$fit) == "randomForest")) {
    lib <- "randomForest"
  } else if (any(class(object$fit) == "rfsrc")) {
    lib <- "randomForestSRC"
  } else {
    stop("Passed 'rf_fun' is not supported.")
  }

  error_measure <- check_response_type(object$fit$predicted, error_measure)

  df <- tibble(ntrees = object$tune$all_ntrees, mtry = object$tune$all_mtrys,
               var_error = round(object$tune$all_error_measures, 3))

  if (length(unique(df$mtry)) > 10) {
    message(paste0("Plotting of more than 10 'mtry' options is not supported.",
                   " Please specify your own vector of which 'mtry' values to",
                   " plot in argument 'mtry'.\n",
                   " Taking the first 10 values I can find."))

    # sort df
    df %>%
      arrange(mtry) -> df
    # get value of 10th index
    index <- unique(df$mtry)[10]

    # filter df
    df %>%
      filter(mtry <= index) -> df
  }

  # check for color palette
  if (is.null(color_palette)) {
    color_palette <- "viridis"
  }

  # plot!
  df %>%
    mutate(mtry = as.factor(mtry)) %>%
    ggplot(aes(x = ntrees, y = var_error, color = mtry, group = mtry)) +
    geom_point() +
    geom_line() +
    scale_color_viridis(discrete = TRUE, option = color_palette) +
    labs(x = "ntrees", y = toupper(error_measure),
         title = "Random Forest hyperparameter tuning results",
         subtitle = sprintf(paste0("Total combinations: %s.",
                                   " Package: '%s'.",
                                   " Best '%s': %s.\n",
                                   "Optimal 'ntrees': %s.",
                                   " Optimal 'mtry: %s."),
                            length(object$tune$all_ntrees),
                            lib,
                            toupper(error_measure),
                            round(object$tune$performances_best_run[[
                              error_measure]], 3),
                            object$tune$optimal_ntree,
                            object$tune$optimal_mtry)) +
    theme_ipsum() +
    guides(color = guide_legend(title = "mtry"))
}
