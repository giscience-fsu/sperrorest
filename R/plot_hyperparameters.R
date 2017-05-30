#' @title plot_hyper_svm
#'
#' @import tibble
#' @import ggplot2
#' @import magrittr
#' @import hrbrthemes
#'
#' @export

plot_hyper_svm <- function(object = NULL) {
  if (class(object) == "ksvm") {
    df <- tibble(cost = object@param$C_all, gamma = object@param$gamma_all,
                 auroc = round(object@param$auroc_all, 3))

    df %>%
      ggplot(aes(x = cost, y = gamma)) +
      geom_point() +
      geom_text(aes(label = auroc), hjust = -0.1, vjust = 0) +
      labs(x = "cost", y = "gamma",
           title = "'kernlab::ksvm' hyperparameter tuning results",
           subtitle = sprintf(paste0("Number of combinations: %s using '%s'.",
                                     " Auroc values attached to each point"),
                              length(object@param$gamma_all),
                              class(out@kernelf))) +
      theme_ipsum()
  }
}
