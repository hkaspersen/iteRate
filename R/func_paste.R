#' Paste unique values in column together
#'
#' Collapses column based on unique values in given column
#'
#' @param x A column name
#'
#' @export
#'
func_paste <- function(x) paste(unique(x[!is.na(x)]), collapse = ", ")
