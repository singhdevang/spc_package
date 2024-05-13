#' Create SPC Data with Dynamic Phases
#'
#' This function creates Statistical Process Control (SPC) chart data and ensures LCL values are not negative.
#' It includes multiple statistical calculations such as shift detection, trend analysis, sigma levels with control limits,
#' and dynamic phase adjustments based on input.
#'
#' @param data Data frame containing the data
#' @param date_col Name of the date column
#' @param value_col Name of the value column
#' @param chart_type Type of SPC chart to create
#' @param phase A vector of numeric values specifying row numbers from which the phase value should incrementally increase.
#'              Each specified row starts a new phase, incrementing by 1 from the previous phase.
#'              If left blank, all phases default to 1.
#' @return Returns a data frame with SPC data including adjusted LCL values, shift, trend, sigma calculations,
#'         additional control limit assessments, and dynamic phase adjustments.
#' @export
#' @importFrom qicharts2 qic
#' @importFrom lubridate parse_date_time
#' @examples
#' data <- data.frame(date = seq(as.Date('2020-01-01'), by = 'month', length.out = 30),
#'                    value = rnorm(30, 100, 15))
#' create_spc_data(data, 'date', 'value', 'xbar')
create_spc_data <- function(data, date_col, value_col, chart_type, phase = numeric(0)) {
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

  # Initialize phase column based on the specified phase parameter
  if (length(phase) > 0) {
    phase_values <- rep(1, nrow(chart_data))
    current_phase <- 1
    for (p in sort(unique(phase))) {
      phase_values[p:nrow(chart_data)] <- current_phase + 1
      current_phase <- current_phase + 1
    }
    chart_data$phase <- phase_values
  } else {
    chart_data$phase <- 1  # Default to phase 1 if no phase is specified
  }

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

  # Define fifteen_more and two_more based on sigma limits and control limits
  chart_data$fifteen_more <- ifelse(
    chart_data$shift,
    chart_data$y > chart_data$cl_minus_1sigma,
    chart_data$y < chart_data$cl_plus_1sigma
  )
  chart_data$two_more <- ifelse(
    chart_data$shift,
    chart_data$y > chart_data$cl_minus_2sigma & chart_data$y < chart_data$lcl,
    ifelse(!chart_data$shift, chart_data$y > chart_data$cl_plus_2sigma & chart_data$y < chart_data$ucl, FALSE)
  )

  # Return the updated data frame
  return(chart_data)
}
