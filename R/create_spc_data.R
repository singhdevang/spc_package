#' Create SPC Data
#'
#' This function creates a Statistical Process Control (SPC) chart data and ensures LCL values are not negative.
#' It includes a 'shift' column indicating whether the data point's value is below or equal to the center line (CL).
#' Additionally, a 'trend' column is added to show whether each subsequent value of 'y' is greater than its predecessor.
#' Values in 'trend' are TRUE if the value is increasing, NA if constant, and FALSE if decreasing.
#'
#' @param data Data frame containing the data
#' @param date_col Name of the date column
#' @param value_col Name of the value column
#' @param chart_type Type of SPC chart to create
#' @return Returns a data frame with SPC data including adjusted LCL values, 'shift' and 'trend' status columns.
#' @export
#' @importFrom qicharts2 qic
#' @importFrom lubridate parse_date_time
#' @examples
#' data <- data.frame(date = seq(as.Date('2020-01-01'), by = 'month', length.out = 30),
#'                    value = rnorm(30, 100, 15))
#' create_spc_data(data, 'date', 'value', 'xbar')
create_spc_data <- function(data, date_col, value_col, chart_type) {
  # Try to parse the date column using common date formats
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

  # Add shift column where shift is TRUE if y < cl, NA if y == cl
  chart_data$shift <- ifelse(chart_data$y == chart_data$cl, NA, chart_data$y < chart_data$cl)

  # Calculate trend column comparing consecutive y values
  chart_data$trend <- c(NA, ifelse(diff(chart_data$y) > 0, TRUE, ifelse(diff(chart_data$y) == 0, NA, FALSE)))

  # Set the first entry of trend to be the same as the second entry
  if (!is.na(chart_data$trend[2])) {
    chart_data$trend[1] <- chart_data$trend[2]
  } else {
    # This handles the case if the second value is NA; the first value then defaults to FALSE
    chart_data$trend[1] <- FALSE
  }

  # Return the updated data frame
  return(chart_data)
}
