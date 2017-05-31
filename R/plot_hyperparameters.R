#' @title plot_hyper_svm
#'
#' @import tibble
#' @import ggplot2
#' @import magrittr
#' @import hrbrthemes
#' @import viridis
#' @import dplyr
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
#'
#' @import tibble
#' @import ggplot2
#' @import magrittr
#' @import hrbrthemes
#' @import viridis
#' @import dplyr
#'
#' @export

plot_hyper_rf <- function(object = NULL) {
  if (any(class(object) == "randomForest")) {
    lib <- "randomForest"
  } else if (any(class(object) == "rfsrc")) {
    lib <- "randomForestSRC"
  } else {
    stop("Passed 'rf_fun' is not supported.")
  }
  df <- tibble(ntrees = object$all_ntrees, mtry = object$all_mtrys,
               auroc = round(object$all_auroc, 3))

  if (length(unique(df$mtry)) > 12) {
    stop(paste0("Too many 'ntrees' levels (> 12) supplied for discrete",
                " color scale."))
  }

  df %>%
    mutate(mtry = as.factor(mtry)) %>%
    ggplot(aes(x = ntrees, y = auroc, color = mtry, group = mtry)) +
    geom_point() +
    geom_line() +
    scale_color_viridis(discrete = TRUE) +
    labs(x = "ntrees", y = "auroc",
         title = "Random Forest hyperparameter tuning results",
         subtitle = sprintf(paste0("Number of combinations: %s.",
                                   " Package: '%s'.",
                                   " Best AUROC: %s.",
                                   " Optimal 'ntrees': %s.",
                                   " Optimal 'mtry: %s."),
                            length(object$all_ntrees),
                            lib,
                            round(object$best_auroc, 3),
                            object$optimal_ntree,
                            object$optimal_mtry)) +
    theme_ipsum() +
    guides(color = guide_legend(title = "mtry"))
}
