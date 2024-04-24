#' Create a Customized SPC Chart
#'
#' This function generates a statistical process control (SPC) chart from provided SPC data.
#' It ensures that date columns are treated as categorical variables, allowing each unique
#' date to be plotted without summarization. The function colors various components of the
#' chart and uses discrete scaling for the x-axis to display all unique dates.
#'
#' @param data A data frame containing the SPC data including at least the columns 'x' for dates,
#'        'y' for the measurement values, 'cl' for the centerline, 'lcl' for the lower control limit,
#'        and 'ucl' for the upper control limit.
#' @param chart_title A character string representing the title of the chart.
#' @param chart_title_size Numeric value defaulted at 14 but can be changed according to need
#' @param caption Character string that can be used to enter source of the data on bottom right
#' @param caption_size Numeric value that is defaulted at 8 but can be used to change size of caption
#' @return A ggplot object representing the SPC chart with categorical date handling and
#'         customized aesthetics.
#' @export
#' @importFrom ggplot2 ggplot geom_line geom_point aes labs scale_x_discrete theme_minimal theme element_text element_blank element_line
#' @importFrom lubridate parse_date_time
#' @importFrom grDevices rgb
#' @examples
#' data <- data.frame(
#'   x = seq(as.Date("2021-01-01"), by = "month", length.out = 12),
#'   y = rnorm(12, 100, 10),
#'   cl = 100,
#'   lcl = 85,
#'   ucl = 115
#' )
#' chart <- make_chart(data, "Monthly SPC Chart", 15, "Source: Imaginary database", 10)
#' print(chart)
make_chart <- function(data, chart_title = "", chart_title_size = 14, caption = "", caption_size = 8) {
  # Ensure that 'x' is a Date object if not already
  if (!inherits(data$x, "Date")) {
    data$x <- parse_date_time(data$x, orders = c("ymd_HMS", "ymd_HM", "ymd_H",
                                                 "mdy_HMS", "mdy_HM", "mdy_H", "mdy",
                                                 "dmy_HMS", "dmy_HM", "dmy_H", "dmy",
                                                 "ydm_HMS", "ydm_HM", "ydm_H", "ydm",
                                                 "ym"))
    # Force convert to Date if necessary
    if (!inherits(data$x, "Date")) {
      data$x <- as.Date(data$x)
    }
  }

  # Convert dates to character for categorical plotting
  data$x <- as.character(data$x)

  # Define color palette for the chart
  colors <- list(
    y = rgb(74, 121, 134, maxColorValue = 255),
    cl = rgb(216, 159, 62, maxColorValue = 255),
    lcl_ucl = rgb(190, 190, 190, maxColorValue = 255),
    title = rgb(27, 87, 104, maxColorValue = 255),
    annotation = rgb(40, 40, 40, maxColorValue = 255)
  )

  # Create the plot with ggplot2
  p <- ggplot(data, aes(x = x)) +
    geom_line(aes(y = cl, group = 1), color = colors$cl, size = 1.25) +
    geom_line(aes(y = lcl, group = 1), color = colors$lcl_ucl, size = 1.25, alpha = 0.5) +
    geom_line(aes(y = ucl, group = 1), color = colors$lcl_ucl, size = 1.25, alpha = 0.5) +
    geom_line(aes(y = y, group = 1), color = colors$y, size = 1.25) +
    geom_point(aes(y = y), color = colors$y, fill = "white", shape = 21, size = 3) +
    scale_x_discrete(name = "Date", breaks = unique(data$x), labels = unique(data$x)) +  # Explicitly set breaks and labels
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(color = colors$title, size = chart_title_size, hjust = 0.5),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, color = "darkgray"),
      axis.text.y = element_text(color = "darkgray"),
      axis.ticks = element_line(color = "darkgray"),
      axis.line = element_line(color = "darkgray"),
      plot.caption = element_text(size = caption_size, color = "darkgray", hjust = 1,family = "Arial"),
      plot.caption.position = "plot"
    )

  # Conditionally add chart title if provided
  if (chart_title != "") {
    p <- p + labs(title = chart_title)
  }

  # Conditionally add caption if provided
  if (caption != "") {
    p <- p + labs(caption = caption)
  }

  return(p)
}
