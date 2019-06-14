#' Run iteration analysis
#'
#' Run iterations over given data frame with the selected method
#'
#' @param df A data frame to run the analysis on, with columns called "group1" and "group2" for the different groups, and "value" for the distances
#' @param runs How many iterations should be run?
#' @param seed Set seed number
#' @param method The summarizing method used, either "mean", "below", "above", "within", "without", "median", or "quantile"
#' @param dist_value1 The first distance value to calculate from, usually the lower value
#' @param dist_value2 The second distance value to calculate from, usually the higher value
#'
#' @export
#'
#' @importFrom dplyr bind_rows
#'
iterate <- function(df,
                    runs = 1000,
                    seed = 10,
                    method = "mean",
                    dist_value1,
                    dist_value2) {
  # set seed
  set.seed(seed)

  # create output list
  output <- list()

  # create expected values
  if (method == "mean") {
    orig <- mean_calc(df, 0)
  }

  if (method == "below") {
    orig <- below(df, 0, dist_value1)
  }

  if (method == "above") {
    orig <- above(df, 0, dist_value1)
  }

  if (method == "within") {
    orig <- within(df, 0, dist_value1, dist_value2)
  }

  if (method == "without") {
    orig <- without(df, 0, dist_value1, dist_value2)
  }

  if (method == "median") {
    orig <- median_calc(df, 0)
  }

  if (method == "quantile") {
    orig <- quantile_calc(df, 0)
  }

  output <- c(output, list(orig))

  # run iterations
  for (i in 1:runs) {
    result <- iterate_samples(df,
                              i,
                              method = method,
                              dist_value1 = dist_value1,
                              dist_value2 = dist_value2)
    output <- c(output, list(result))
  }

  output <- bind_rows(output)

  return(output)
}
