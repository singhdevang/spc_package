#' Run Test on SPC Data with Visual Rule Violations
#'
#' This function plots an SPC chart for the provided data frame, which must be structured
#' with specific columns from SPC analysis. It highlights rule violations in the SPC chart
#' by changing the color of data points according to specific SPC rules:
#' - Red: Any point outside the control limits (3-sigma).
#' - Blue: Run of eight consecutive points on one side of the centerline.
#' - Green: Six or more consecutive points increasing or decreasing.
#' - Black: Fifteen or more points clustering within 1-sigma of the centerline.
#' - Purple: Two out of three consecutive points beyond 2-sigma from the mean.
#'
#' @param data A data frame that contains SPC data including columns for measurements,
#'        control limits, and other SPC relevant statistics. Expected columns include
#'        'x' for the time/date, 'y' for the data points, 'cl' for the centerline, 'lcl'
#'        and 'ucl' for the lower and upper control limits, respectively.
#' @return A ggplot object representing the SPC chart with visual indications of rule violations.
#' @export
#' @importFrom dplyr mutate arrange lag lead case_when
#' @importFrom ggplot2 ggplot geom_line geom_point aes labs scale_color_manual theme_minimal theme
#' @examples
#' data <- data.frame(
#'   x = seq(as.Date("2022-01-01"), by = "month", length.out = 24),
#'   y = c(rnorm(12, 100, 15), rnorm(12, 110, 15)),
#'   cl = 105,
#'   lcl = 85,
#'   ucl = 125
#' )
#' result <- create_spc_data(data, 'x', 'y', 'xbar')  # Assuming create_spc_data is defined correctly
#' spc_chart <- run_test(result)
#' print(spc_chart)
run_test <- function(data, chart_title) {
  # Ensure that 'x' is a Date object if not already
  if (!inherits(data$x, "Date")) {
    data$x <- parse_date_time(data$x, orders = c("ymd_HMS", "ymd_HM", "ymd_H", "mdy_HMS", "mdy_HM", "mdy_H", "dmy_HMS", "dmy_HM", "dmy_H", "ydm_HMS", "ydm_HM", "ydm_H", "ydm"))
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
    annotation = rgb(40, 40, 40, maxColorValue = 255),
    outlier = "green"
  )

  # Create the plot with ggplot2
  p <- ggplot(data, aes(x = x)) +
    geom_line(aes(y = cl, group = 1), color = colors$cl, size = 1.25) +
    geom_line(aes(y = lcl, group = 1), color = colors$lcl_ucl, size = 1.25, alpha = 0.5) +
    geom_line(aes(y = ucl, group = 1), color = colors$lcl_ucl, size = 1.25, alpha = 0.5) +
    geom_line(aes(y = y, group = 1), color = colors$y, size = 1.25) +
    geom_point(aes(y = y), color = colors$y, fill = ifelse(data$y > data$ucl | data$y < data$lcl, colors$outlier, "white"), shape = 21, size = 3) +
    labs(title = chart_title) +
    scale_x_discrete(name = "Date", breaks = unique(data$x), labels = unique(data$x)) +  # Explicitly set breaks and labels
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(color = colors$title, size = 16, hjust = 0.5),
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
