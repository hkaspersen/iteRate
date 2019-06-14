#' Calculate quantile values of distances
#'
#' Calculates the quantile values of observations for each group
#'
#' @param df A data frame to run the analysis on
#' @param run The number of the specific iteration run
#'
#' @export
#'
#' @import dplyr
#'
quantile_calc <- function(df, run) {
  df <- df %>%
    group_by(group1, group2) %>%
    mutate(quant_0 = quantile(value)[1],
           quant_25 = quantile(value)[2],
           quant_50 = quantile(value)[3],
           quant_75 = quantile(value)[4],
           quant_100 = quantile(value)[5]) %>%
    select(group1, group2, contains("quant")) %>%
    summarise_all(list(func_paste)) %>%
    mutate_at(vars(contains("quant")),
              funs(as.numeric)) %>%
    mutate(iter = run)
}
