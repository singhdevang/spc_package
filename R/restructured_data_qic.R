#' Create SPC Data
#'
#' This function creates a Statistical Process Control chart data and ensures LCL values are not negative.
#' @param data Data frame containing the data
#' @param date_col Name of the date column
#' @param value_col Name of the value column
#' @param chart_type Type of SPC chart to create
#' @return Returns a data frame with SPC data including adjusted LCL values.
#' @export
#' @importFrom qicharts2 qic
#' @examples
#' data <- data.frame(date = seq(as.Date('2020-01-01'), by = 'month', length.out = 30),
#'                    value = rnorm(30, 100, 15))
#' create_spc_data(data, 'date', 'value', 'xbar')
create_spc_data <- function(data, date_col, value_col, chart_type) {
  # Create the control chart using qic function from qicharts2
  # Automatically plots, but we will extract data from the plot object for further manipulation
  chart <- qic(data[[date_col]], data[[value_col]], data = data, chart = chart_type)

  # Prepare data for plotting and ensure LCL values are not less than 0
  chart_data <- chart$data
  chart_data$lcl <- pmax(chart_data$lcl, 0)

  # Return the updated data frame
  return(chart_data)
}
