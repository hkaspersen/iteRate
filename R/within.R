#' Calculate sum of observations between distance 1 and distance 2
#'
#' Calculates the sum of observations between dist_value1 and dist_value2, and calculates the percentage of observed values per group
#'
#' @param df A data frame to run the analysis on
#' @param run The number of the specific iteration run
#' @param dist_value1 The first distance value to calculate from, usually the lower value
#' @param dist_value2 The second distance value to calculate from, usually the higher value
#'
#' @export
#'
#' @import dplyr
#' @importFrom tidyr spread
#'
within <- function(df,
                   run,
                   dist_value1 = 0.1,
                   dist_value2 = 0.8) {
  df <- df %>%
    group_by(group1, group2) %>%
    mutate(n = value > dist_value1 & value < dist_value2) %>%
    group_by(group1, group2, n) %>%
    count() %>%
    ungroup() %>%
    mutate(n = ifelse(n == TRUE, "Within", "Without")) %>%
    spread(n, nn) %>%
    mutate(Percent = round(Within / (Within + Without) * 100, 2),
           iter = run)
  return(df)
}
