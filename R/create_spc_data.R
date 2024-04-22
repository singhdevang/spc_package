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
#' @importFrom lubridate parse_date_time
#' @examples
#' data <- data.frame(date = seq(as.Date('2020-01-01'), by = 'month', length.out = 30),
#'                    value = rnorm(30, 100, 15))
#' create_spc_data(data, 'date', 'value', 'xbar')
create_spc_data <- function(data, date_col, value_col, chart_type) {
  # Try to parse the date column using common date formats, now using uppercase for time components
  data[[date_col]] <- parse_date_time(data[[date_col]],
                                      orders = c("ymd_HMS", "ymd_HM", "ymd_H", "ymd",
                                                 "mdy_HMS", "mdy_HM", "mdy_H", "mdy",
                                                 "dmy_HMS", "dmy_HM", "dmy_H", "dmy",
                                                 "ydm_HMS", "ydm_HM", "ydm_H", "ydm"))

  # Proceed with creating the SPC chart using qic function from qicharts2
  chart <- qic(data[[date_col]], data[[value_col]], data = data, chart = chart_type)

  # Prepare data for plotting and ensure LCL values are not less than 0
  chart_data <- chart$data
  chart_data$lcl <- pmax(chart_data$lcl, 0)

  # Add shift column where shift is TRUE if y is less than cl
  chart_data$shift <- chart_data$y < chart_data$cl

  # Return the updated data frame
  return(chart_data)
}


