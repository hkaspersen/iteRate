#' Plot results from iterations
#'
#' Plots the distribution of the expected values relative to the observed.
#'
#' @param df The data frame with results from the "iterate" function
#'
#' @export
#'
#' @import dplyr
#' @importFrom tidyr spread
#'
plot_dist <- function(df) {

  segment_data <- filter(df, iter == 0)

  p <- ggplot(df, aes(Percent)) +
    geom_histogram(
      aes(y = ..density..),
      binwidth = 0.2,
      color = "black",
      fill = "grey80"
    ) +
    stat_function(fun = dnorm,
                  args = with(df, c(
                    mean = mean(Percent), sd = sd(Percent)
                  )),
                  color = "red") +
    geom_segment(
      segment_data,
      mapping = aes(
        x = Percent,
        xend = Percent,
        y = 0.1,
        yend = 0,
        color = group1
      ),
      arrow = arrow(length = unit(0.3, "cm"),
                    type = "closed")
    ) +
    geom_text(segment_data,
              mapping = aes(label = group1,
                  x = Percent),
              y = 0.16,
              angle = 90) +
    geom_vline(xintercept = mean(df$Percent),
               size = 1) +
    guides(color = FALSE) +
    labs(x = "Percent (%) distances",
         y = "Density") +
    ggtitle(label = "Distribution of percentages",
            subtitle = paste("Number of iterations: ", max(df$iter))) +
    theme(plot.title = element_text(hjust = 0))

  return(p)
}
