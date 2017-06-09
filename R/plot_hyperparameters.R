#' @title plot_hyper_svm
#'
#' @import tibble
#' @import ggplot2
#' @import magrittr
#' @import hrbrthemes
#' @import viridis
#' @importFrom dplyr mutate
#'
#' @export

plot_hyper_svm <- function(object = NULL) {
  if (class(object) == "ksvm") {
    df <- tibble(cost = object@param$C_all, gamma = object@param$gamma_all,
                 auroc = round(object@param$auroc_all, 3))

    if (length(unique(df$gamma)) > 12) {
      stop(paste0("Too many 'gamma' levels (> 12) supplied for discrete",
                  " color scale."))
    }

    df %>%
      mutate(gamma = as.factor(gamma)) %>%
      ggplot(aes(x = cost, y = auroc, color = gamma, group = gamma)) +
      geom_point() +
      geom_line() +
      scale_color_ipsum() +
      # geom_text(aes(label = auroc), hjust = -0.1, vjust = 0) +
      labs(x = "cost", y = "auroc",
           title = "'kernlab::ksvm' hyperparameter tuning results",
           subtitle = sprintf(paste0("Number of combinations: %s. Kernel: '%s'."),
                              length(object@param$gamma_all),
                              class(out@kernelf))) +
      theme_ipsum() +
      guides(color = guide_legend(title = "gamma"))
  }
}

#' @title plot_hyper_rf
#' @description Plots tuning results of Random Forest
#'
#' @param object a fitted object of [randomForest] or [rfsrc].
#'
#' @param error_measure the error_measure which the model was tuned on.
#' Only needed if the default one from [sptune_rf] was changed.
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
#' out <- sptune_rf(fo, ecuador, accelerate = 16, nfold = 5,
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

  # binary classification
  if (is.factor(object$fit$predicted) &&
      length(object$fit$predicted) == 2) {
    if (is.null(error_measure)) {
      error_measure <- "auroc"
    }
  }
  # regression
  else if (is.numeric(object$fit$predicted)) {
    if (is.null(error_measure)) {
      error_measure <- "rmse"
    }
  }
  # multiclass classification
  else if (is.factor(object$fit$predicted) &&
           length(object$fit$predicted) > 2) {
    if (is.null(error_measure)) {
      error_measure <- "error"
    }
  }

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
    # get value of 12th index
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
    #scale_colour_manual(values = colorspace::sequential_hcl(12)) +
    labs(x = "ntrees", y = toupper(error_measure),
         title = "Random Forest hyperparameter tuning results",
         subtitle = sprintf(paste0("Total combinations: %s.",
                                   " Package: '%s'.",
                                   " Best '%s': %s.",
                                   " Optimal 'ntrees': %s.",
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
