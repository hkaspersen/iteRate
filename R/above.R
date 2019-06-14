#' Calculate sum of observations above distance value
#'
#' Calculates the sum of observations above dist_value and calculates the percentage of observed values per group
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
above <- function(df,
                  run,
                  dist_value = 0.9) {
  df <- df %>%
    group_by(group1, group2) %>%
    mutate(n = value > dist_value) %>%
    group_by(group1, group2, n) %>%
    count() %>%
    ungroup() %>%
    mutate(n = ifelse(n == TRUE, "Above", "Below")) %>%
    spread(n, nn) %>%
    mutate(Above = ifelse(is.na(Above) == TRUE, 0, Above)) %>%
    mutate(Percent = round(Above / (Above + Below) * 100, 2),
           iter = run)
  return(df)
}
