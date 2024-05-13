#' Create SPC Data for P-Charts and U-Charts
#'
#' This function creates Statistical Process Control (SPC) chart data specifically for p-charts and u-charts. It ensures non-negative Lower Control Limits (LCL) and integrates additional statistical calculations such as trend analysis, shift detection, and sigma levels with control limits.
#'
#' @param data Data frame containing the data.
#' @param date_col Name of the date column.
#' @param num_col Name of the numerator column (defects or defectives).
#' @param den_col Name of the denominator column (sample size or units).
#' @param chart_type Type of SPC chart to create ('p' for p-chart, 'u' for u-chart).
#' @return Returns a data frame with SPC data including adjusted LCL values, shifts, trends, sigma calculations, and additional control limit assessments.
#' @export
#' @importFrom qicharts2 qic
#' @importFrom lubridate parse_date_time
#' @examples
#' data <- data.frame(date = seq(as.Date('2020-01-01'), by = 'month', length.out = 12),
#'                    defects = c(5, 6, 2, 8, 5, 9, 3, 4, 7, 1, 3, 2),
#'                    units = c(100, 90, 110, 100, 95, 105, 100, 95, 100, 90, 95, 105))
#' create_spc_data_pu(data, 'date', 'defects', 'units', 'u')

create_spc_data_pu <- function(data, date_col, num_col, den_col, chart_type) {
  # Parse the date column using ymd function from lubridate for yyyy-mm-dd format
  #data[[date_col]] <- ymd(data[[date_col]])
  data[[date_col]] <- parse_date_time(data[[date_col]],
                                      orders = c("ymd_HMS", "ymd_HM", "ymd_H", "ymd",
                                                 "mdy_HMS", "mdy_HM", "mdy_H", "mdy",
                                                 "dmy_HMS", "dmy_HM", "dmy_H", "dmy",
                                                 "ydm_HMS", "ydm_HM", "ydm_H", "ydm",
                                                 "ym"))
  # Create the SPC chart using qic function from qicharts2
  chart <- qic(data[[date_col]], data[[num_col]], data[[den_col]], data = data, chart = chart_type)

  # Adjust y values to be percentages
  chart$data$y <- chart$data$y * 100

  # Prepare data for plotting and ensure LCL values are not less than 0
  chart_data <- chart$data
  chart_data$lcl <- pmax(chart_data$lcl * 100, 0) # Adjust LCL as well
  chart_data$ucl <- chart_data$ucl * 100 # Adjust UCL as well
  chart_data$cl <- chart_data$cl * 100 # Adjust CL as well

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
