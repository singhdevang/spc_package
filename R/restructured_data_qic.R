
#Function 2 to structure data using qic which will later be used for plotting
restructured_data_qic <- function(data, date_col, measure_col, chart_type) { # date_col for x axis and measure_col for y axis

  # Create qic object for the specified chart type using dynamic column names
  chart <- qic(data[[date_col]], data[[measure_col]], chart = chart_type)

  # Prepare data for plotting and ensure LCL values are not less than 0
  chart_data <- chart$data
  chart_data$lcl <- pmax(chart_data$lcl, 0)  # Replace negative LCL values with 0

  # Mutate the 'x' column to Date format within the data argument
  chart_data <- chart_data |>
    mutate(x = as.Date(x, format = "%Y-%m-%d"))

  return(chart_data)
}

#' Restructure Data for QIC
#'
#' This function restructures the data for quality improvement charts (QIC) and ensures LCL values are not less than 0.
#' @param data A data frame containing the data.
#' @param date_col The name of the column in `data` containing date values.
#' @param measure_col The name of the column in `data` containing measurement values.
#' @param chart_type The type of chart to be created.
#' @return A data frame prepared for plotting.
#' @export
#' @examples
#' restructured_data_qic(data, "date", "measure", "u-chart")
restructured_data_qic <- function(data, date_col, measure_col, chart_type) {
  # function body remains the same
}
