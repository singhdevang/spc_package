#' Create a Customized SPC Chart with Phase Handling and Annotations
#'
#' This function generates a statistical process control (SPC) chart from provided SPC data.
#' It ensures that date columns are treated as categorical variables, allowing each unique
#' date to be plotted without summarization. The function colors various components of the
#' chart and uses discrete scaling for the x-axis to display all unique dates. If phase changes
#' exist, it discontinues line connections between different phases. Additionally, it allows
#' for custom annotations on the chart.
#'
#' @param data A data frame containing the SPC data including at least the columns 'x' for dates,
#'        'y' for the measurement values, 'cl' for the centerline, 'lcl' for the lower control limit,
#'        'ucl' for the upper control limit, and optionally 'phase' for indicating phase changes.
#' @param chart_title A character string representing the title of the chart.
#' @param chart_title_size Numeric value defaulted at 14 but can be changed according to need.
#' @param caption Character string that can be used to enter the source of the data on the bottom right.
#' @param caption_size Numeric value that is defaulted at 8 but can be used to change the size of the caption.
#' @param annotations A data frame containing the annotations with columns 'row_number', 'label',
#'        'text_size', 'position_x', 'position_y'. The 'position_x' and 'position_y' columns
#'        specify the offset for the annotation text relative to the point.
#' @return A ggplot object representing the SPC chart with categorical date handling and
#'         customized aesthetics, including phase handling.
#' @export
#' @importFrom ggplot2 ggplot geom_line scale_y_continuous geom_point scale_x_date geom_text geom_segment aes labs scale_x_discrete theme_minimal theme element_text element_blank element_line
#' @importFrom lubridate parse_date_time
#' @importFrom grDevices rgb
#' @examples
#' data <- data.frame(
#'   x = seq(as.Date("2021-01-01"), by = "month", length.out = 12),
#'   y = rnorm(12, 100, 10),
#'   cl = 100,
#'   lcl = 85,
#'   ucl = 115,
#'   phase = rep(1:2, each = 6)
#' )
#' annotations <- data.frame(
#'   serial_number = c(3, 9),
#'   label = c("Annotation 1", "Annotation 2"),
#'   text_size = c(4, 4),
#'   position_x = c(0.2, 0),
#'   position_y = c(10, 10)
#' )
#' chart <- plot_spc_chart(data, "Monthly SPC Chart", 15, "Source: Imaginary database", 10, annotations)
#' print(chart)


plot_spc_chart <- function(data, chart_title = "", chart_title_size = 14, caption = "", caption_size = 8, annotations = NULL) {
  # Ensure that 'x' is a Date object if not already
  if (!inherits(data$x, "Date")) {
    data$x <- parse_date_time(data$x, orders = c("my", "ym", "ymd", "mdy", "dmy", "ydm",
                                                 "ymd_HMS", "ymd_HM", "ymd_H",
                                                 "mdy_HMS", "mdy_HM", "mdy_H",
                                                 "dmy_HMS", "dmy_HM", "dmy_H",
                                                 "ydm_HMS", "ydm_HM", "ydm_H"))
    # Force convert to Date if necessary
    if (!inherits(data$x, "Date")) {
      data$x <- as.Date(data$x)
    }
  }

  # Define color palette for the chart
  colors <- list(
    y = rgb(74, 121, 134, maxColorValue = 255),
    cl = rgb(216, 159, 62, maxColorValue = 255),
    lcl_ucl = rgb(190, 190, 190, maxColorValue = 255),
    title = rgb(27, 87, 104, maxColorValue = 255),
    annotation = rgb(0, 0, 0, maxColorValue = 255),
    annotation_line = rgb(169, 169, 169, maxColorValue = 255)  # Gray for annotation lines
  )

  # Prepare data by phase if phase column exists
  data_list <- split(data, data$phase)

  # Create the initial plot with ggplot2
  p <- ggplot(data, aes(x = x)) +
    scale_x_date(name = "Date") +  # Use scale_x_date for date handling
    theme_minimal() +
    theme(
      plot.title = element_text(color = colors$title, size = chart_title_size, hjust = 0.5),
      plot.background = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 0, vjust = 0.5, color = "darkgray"),  # Set angle to 0 for horizontal text
      axis.text.y = element_text(color = "darkgray"),
      axis.ticks = element_line(color = "darkgray"),
      axis.line = element_line(color = "darkgray"),
      plot.caption = element_text(size = caption_size, color = "darkgray", hjust = 1),
      plot.caption.position = "plot"
    )

  # Add phase-based layers
  for (df in data_list) {
    p <- p +
      geom_line(data = df, aes(y = cl, group = 1), color = colors$cl, size = 1.25) +
      geom_line(data = df, aes(y = lcl, group = 1), color = colors$lcl_ucl, size = 1.25, alpha = 0.5) +
      geom_line(data = df, aes(y = ucl, group = 1), color = colors$lcl_ucl, size = 1.25, alpha = 0.5)
  }

  # Add y line and points last so they're on top
  p <- p +
    geom_line(aes(y = y, group = 1), color = colors$y, size = 1.25) +
    geom_point(aes(y = y), color = colors$y, fill = "white", shape = 21, size = 3)

  # Conditionally add chart title if provided
  if (chart_title != "") {
    p <- p + labs(title = chart_title)
  }

  # Conditionally add caption if provided
  if (caption != "") {
    p <- p + labs(caption = caption)
  }

  # Conditionally add annotations if provided
  if (!is.null(annotations) && nrow(annotations) > 0) {
    # Add columns to annotations for plotting
    annotations$x <- data$x[annotations$serial_number]
    annotations$y <- data$y[annotations$serial_number]

    # Adjust x positions based on position_x
    annotations$label_x <- annotations$x + annotations$position_x

    # Adjust starting point of annotation line based on position_y
    point_radius <- 3 * 0.085  # Adjusted for size of the data point
    p <- p +
      geom_segment(data = annotations, aes(
        x = x,
        y = ifelse(position_y > 0, y + point_radius, y - point_radius),
        xend = label_x,
        yend = y + position_y
      ), color = colors$annotation_line, size = 0.5) +
      geom_text(data = annotations, aes(
        x = label_x,
        y = y + position_y,
        label = label
      ), color = colors$annotation, size = annotations$text_size, hjust = 0.5, vjust = ifelse(annotations$position_y > 0, -0.3, 1.3))
  }

  return(p)
}
