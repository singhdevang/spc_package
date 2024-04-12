#' Create an SPC Chart with Custom Aesthetics
#'
#' This function generates a customized Statistical Process Control (SPC) chart using ggplot2.
#' The chart includes the main data line, control limits (CL, UCL, LCL) and points, with specific annotations
#' and a defined color palette. It returns a ggplot object ready for displaying or further customization.
#'
#' @param data A data frame containing the columns x, y, cl, lcl, and ucl.
#' @param chart_title Title of the chart to be displayed at the top.
#' @return A ggplot object representing the SPC chart.
#' @importFrom ggplot2 ggplot geom_line geom_point geom_text labs theme_minimal theme element_text element_blank panel_grid panel_background axis_title axis_text axis_ticks axis_line plot_caption
#' @importFrom dplyr tail
#' @importFrom grDevices rgb
#' @examples
#' data <- data.frame(
#'   x = 1:30,
#'   y = rnorm(30, 100, 15),
#'   cl = rep(100, 30),
#'   ucl = rep(115, 30),
#'   lcl = rep(85, 30)
#' )
#' make_chart(data, "Sample SPC Chart")
#' @export
make_chart <- function(data, chart_title) {
  # Define color palette for the chart
  colors <- list(
    y = rgb(74, 121, 134, maxColorValue = 255),
    cl = rgb(216, 159, 62, maxColorValue = 255),
    lcl_ucl = rgb(190, 190, 190, maxColorValue = 255),
    title = rgb(27, 87, 104, maxColorValue = 255),
    annotation = rgb(40, 40, 40, maxColorValue = 255)
  )

  # Get the last point of the cl, lcl, and ucl lines for annotation
  last_point <- tail(data[order(data$x), ], 1)

  # Create the plot with ggplot2
  p <- ggplot(data, aes(x = x)) +
    geom_line(aes(y = cl, group = 1), color = colors$cl, size = 1.25) +
    geom_line(aes(y = lcl, group = 1), color = colors$lcl_ucl, size = 1.25, alpha = 0.5) +
    geom_line(aes(y = ucl, group = 1), color = colors$lcl_ucl, size = 1.25, alpha = 0.5) +
    geom_line(aes(y = y, group = 1), color = colors$y, size = 1.25) +
    geom_point(aes(y = y), color = colors$y, fill = "white", shape = 21, size = 3) +
    geom_text(aes(x = last_point$x, y = last_point$cl), label = sprintf("%.1f", last_point$cl), color = colors$annotation, hjust = -0.2, vjust = 0, size = 3) +
    geom_text(aes(x = last_point$x, y = last_point$lcl), label = sprintf("%.1f", last_point$lcl), color = colors$annotation, hjust = -0.2, vjust = 0, size = 3) +
    geom_text(aes(x = last_point$x, y = last_point$ucl), label = sprintf("%.1f", last_point$ucl), color = colors$annotation, hjust = -0.2, vjust = 0, size = 3) +
    labs(title = chart_title) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(color = colors$title, size = 16, hjust = 0.5),
      plot.subtitle = element_text(color = colors$title, size = 16, hjust = 0.5),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, color = "darkgray"),
      axis.text.y = element_text(color = "darkgray"),
      axis.ticks = element_line(color = "darkgray"),
      axis.line = element_line(color = "darkgray"),
      plot.caption = element_text(size = 10, color = "gray", hjust = 1)
    )
  return(p)
}
