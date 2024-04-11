#Function 3 to create control charts
create_control_chart <- function(data, chart_title, start_date, end_date) {
  # Convert start_date and end_date to Date objects and format for subtitle
  start_date_obj <- ymd(start_date)
  end_date_obj <- ymd(end_date)
  subtitle_text <- sprintf("(%s - %s)",
                           format(start_date_obj, "%B %Y"),
                           format(end_date_obj, "%B %Y"))

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
    geom_line(aes(y = cl, group = 1), color = colors$cl, size = 1.25) +  # Plot the CL line
    geom_line(aes(y = lcl, group = 1), color = colors$lcl_ucl, size = 1.25, alpha = 0.5) +  # Plot the LCL line
    geom_line(aes(y = ucl, group = 1), color = colors$lcl_ucl, size = 1.25, alpha = 0.5) +  # Plot the UCL line
    geom_line(aes(y = y, group = 1), color = colors$y, size = 1.25) +  # Plot the main data line
    geom_point(aes(y = y), color = colors$y, fill = "white", shape = 21, size = 3) +  # Plot data points
    # Add annotations for cl, lcl, and ucl
    geom_text(aes(x = last_point$x, y = last_point$cl), label = sprintf("%.1f", last_point$cl), color = colors$annotation, hjust = -0.2, vjust = 0, size = 3) +
    geom_text(aes(x = last_point$x, y = last_point$lcl), label = sprintf("%.1f", last_point$lcl), color = colors$annotation, hjust = -0.2, vjust = 0, size = 3) +
    geom_text(aes(x = last_point$x, y = last_point$ucl), label = sprintf("%.1f", last_point$ucl), color = colors$annotation, hjust = -0.2, vjust = 0, size = 3) +
    # Add titles and labels
    labs(
      title = chart_title,
      subtitle = subtitle_text,
      caption = "Source: Hengoed Care Records"
    ) +

    # Customize the theme of the plot
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

#' Create Control Chart
#'
#' This function generates a control chart with customized themes using ggplot2. It is designed to work with data structured by `restructured_data_qic`. The function allows for customization of the chart title and the date range to be displayed in the subtitle. It utilizes a predefined color palette for different elements of the chart.
#'
#' @param data A data frame prepared by `restructured_data_qic` containing the columns for plotting, including control limits and measurement values.
#' @param chart_title The main title of the control chart.
#' @param start_date The start date for the data range to be included in the chart, formatted as "YYYY-MM-DD".
#' @param end_date The end date for the data range to be included in the chart, formatted as "YYYY-MM-DD".
#' @return A ggplot object representing the control chart, which can be further modified or directly plotted.
#' @export
#' @examples
#' # Assuming `data` has been prepared with `restructured_data_qic`
#' create_control_chart(data, "My Control Chart", "2021-01-01", "2021-12-31")
create_control_chart <- function(data, chart_title, start_date, end_date) {
  # function body remains the same
}

