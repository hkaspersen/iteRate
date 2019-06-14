#' Calculate sum of observations below distance value
#'
#' Calculates the sum of observations below dist_value and calculates the percentage of observed values per group
#'
#' @param df A data frame to run the analysis on
#' @param run The number of the specific iteration run
#' @param dist_value The distance value to calculate from
#'
#' @export
#'
#' @import dplyr
#' @importFrom tidyr spread
#'
below <- function(df,
                  run,
                  dist_value = 0.1) {
  df <- df %>%
    group_by(group1, group2) %>%
    mutate(n = value < dist_value) %>%
    group_by(group1, group2, n) %>%
    count() %>%
    ungroup() %>%
    mutate(n = ifelse(n == TRUE, "Below", "Above")) %>%
    spread(n, nn) %>%
    mutate(Below = ifelse(is.na(Below) == TRUE, 0, Below)) %>%
    mutate(Percent = round(Below / (Above + Below) * 100, 2),
           iter = run)
  return(df)
}
