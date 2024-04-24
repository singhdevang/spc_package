#' Create SPC Data
#'
#' This function creates a Statistical Process Control (SPC) chart data and ensures LCL values are not negative.
#' It includes multiple statistical calculations such as shift detection, trend analysis, and sigma levels with control limits.
#'
#' @param data Data frame containing the data
#' @param date_col Name of the date column
#' @param value_col Name of the value column
#' @param chart_type Type of SPC chart to create
#' @return Returns a data frame with SPC data including adjusted LCL values, shift, trend, sigma calculations, and additional control limit assessments.
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
                                               "ydm_HMS", "ydm_HM", "ydm_H", "ydm",
                                               "ym"))

  # Create the SPC chart using qic function from qicharts2
  chart <- qic(data[[date_col]], data[[value_col]], data = data, chart = chart_type)

  # Prepare data for plotting and ensure LCL values are not less than 0
  chart_data <- chart$data
  chart_data$lcl <- pmax(chart_data$lcl, 0)

  # Add shift column where shift is TRUE if y < cl, NA if y == cl
  chart_data$shift <- ifelse(chart_data$y == chart_data$cl, NA, chart_data$y < chart_data$cl)

  # Calculate trend column comparing consecutive y values
  differences <- c(diff(chart_data$y), NA)  # Append NA for the last difference
  chart_data$trend <- ifelse(differences < 0, TRUE, ifelse(differences > 0, FALSE, NA))

  # Ensure the last value in the trend column copies the one above it
  chart_data$trend[length(chart_data$trend)] <- chart_data$trend[length(chart_data$trend) - 1]

  # Calculate sigma and sigma-based control limits
  chart_data$sigma <- (chart_data$ucl - chart_data$cl) / 3
  chart_data$cl_plus_1sigma <- chart_data$cl + chart_data$sigma
  chart_data$cl_plus_2sigma <- chart_data$cl + 2 * chart_data$sigma
  chart_data$cl_minus_1sigma <- chart_data$cl - chart_data$sigma
  chart_data$cl_minus_2sigma <- chart_data$cl - 2 * chart_data$sigma

  # Define fifteen_more based on sigma limits
  chart_data$fifteen_more <- ifelse(
    chart_data$shift,
    chart_data$y > chart_data$cl_minus_1sigma,  # Condition when shift is TRUE
    chart_data$y < chart_data$cl_plus_1sigma    # Condition when shift is FALSE
  )

  # Define two_more based on two sigma limits and control limits
  chart_data$two_more <- ifelse(chart_data$shift, chart_data$y > chart_data$cl_minus_2sigma & chart_data$y < chart_data$lcl,
                                ifelse(!chart_data$shift, chart_data$y > chart_data$cl_plus_2sigma & chart_data$y < chart_data$ucl, FALSE))

  # Return the updated data frame
  return(chart_data)
}
