make_chart <- function(data, chart_title) {
  # Ensure that 'x' is a Date object if not already
  if (!inherits(data$x, "Date")) {
    data$x <- parse_date_time(data$x, orders = c("ymd_HMS", "ymd_HM", "ymd_H", "ymd",
                                                 "mdy_HMS", "mdy_HM", "mdy_H", "mdy",
                                                 "dmy_HMS", "dmy_HM", "dmy_H", "dmy",
                                                 "ydm_HMS", "ydm_HM", "ydm_H", "ydm"))
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
