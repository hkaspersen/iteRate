#' Calculate median value of distances
#'
#' Calculates the median of observations for each group
#'
#' @param df A data frame to run the analysis on
#' @param run The number of the specific iteration run
#'
#' @export
#'
#' @import dplyr
#'
median_calc <- function(df, run) {
  df <- df %>%
    group_by(group1, group2) %>%
    mutate(metric = round(median(value), 2)) %>%
    select(group1, group2, metric) %>%
    summarise_all(list(func_paste)) %>%
    mutate(iter = run,
           metric = as.numeric(metric))
}
