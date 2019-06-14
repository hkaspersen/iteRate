#' Iterate samples
#'
#' Specify iteration method and iterate over samples
#'
#' @param df A data frame to run the analysis on
#' @param run The number of the specific iteration run
#' @param method The summarizing method used, either "mean", "below", "above", "within", "without", "median", or "quantile"
#' @param dist_value1 The first distance value to calculate from, usually the lower value
#' @param dist_value2 The second distance value to calculate from, usually the higher value
#'
#' @export
#'
#' @importFrom dplyr mutate
#'
iterate_samples <- function(df, run, method = "mean", dist_value1, dist_value2) {

  # randomize the value column, regardless of groups
  df <- df %>%
    mutate(value = sample(value))

  # run specified function
  if (method == "mean") {
    df <- mean_calc(df, run)
  }

  if (method == "below") {
    df <- below(df, run, dist_value1)
  }

  if (method == "above") {
    df <- above(df, run, dist_value1)
  }

  if (method == "within") {
    df <- within(df, run, dist_value1, dist_value2)
  }

  if (method == "without") {
    df <- without(df, run, dist_value1, dist_value2)
  }

  if (method == "median") {
    df <- median_calc(df, run)
  }

  if (method == "quantile") {
    df <- quantile_calc(df, run)
  }

  return(df)
}
