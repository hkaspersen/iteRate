#' Calculate mean value of distances
#'
#' Calculates the mean of distances per group
#'
#' @param df A data frame to run the analysis on
#' @param run The number of the specific iteration run
#'
#' @export
#'
#' @import dplyr
#'
mean_calc <- function(df, run) {
  df <- df %>%
    group_by(group1, group2) %>%
    mutate(metric = round(mean(value), 2)) %>%
    select(group1, group2, metric) %>%
    summarise_all(list(func_paste)) %>%
    mutate(iter = run,
           metric = as.numeric(metric))
}
