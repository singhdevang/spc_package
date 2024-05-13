#' Create SPC Data with Dynamic Phases and Recalculated Control Limits
#'
#' This function creates Statistical Process Control (SPC) chart data, ensuring LCL values are not negative, recalculates control
#' limits within each phase, and applies different statistical calculations based on the phase of the data. Calculations for
#' shift detection, trend analysis, and sigma levels with control limits are restarted and recalculated for each new phase.
#'
#' @param data Data frame containing the data
#' @param date_col Name of the date column
#' @param value_col Name of the value column
#' @param chart_type Type of SPC chart to create
#' @param phase A vector of numeric values specifying row numbers from which the phase value should incrementally increase.
#'              Each specified row starts a new phase, incrementing by 1 from the previous phase.
#'              If left blank, all phases default to 1.
#' @return Returns a data frame with SPC data including recalculated control limits, shift, trend, sigma calculations,
#'         additional control limit assessments, and dynamic phase adjustments.
#' @export
#' @importFrom qicharts2 qic
#' @importFrom lubridate parse_date_time
#' @examples
#' data <- data.frame(date = seq(as.Date('2020-01-01'), by = 'month', length.out = 30),
#'                    value = rnorm(30, 100, 15))
#' create_spc_data(data, 'date', 'value', 'xbar', c(10, 20))
create_spc_data <- function(data, date_col, value_col, chart_type, phase = numeric(0)) {
  # Try to parse the date column using common date formats
  data[[date_col]] <- parse_date_time(data[[date_col]],
                                      orders = c("ymd_HMS", "ymd_HM", "ymd_H", "ymd",
                                                 "mdy_HMS", "mdy_HM", "mdy_H", "mdy",
                                                 "dmy_HMS", "dmy_HM", "dmy_H", "dmy",
                                                 "ydm_HMS", "ydm_HM", "ydm_H", "ydm",
                                                 "ym"))

  # Initialize phase column
  chart_data <- data
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

  # Phase-dependent recalculations
  chart_data <- do.call(rbind, lapply(split(chart_data, chart_data$phase), function(subdata) {
    # Recreate the SPC chart for each phase
    spc_chart <- qic(subdata[[date_col]], subdata[[value_col]], data = subdata, chart = chart_type)

    subdata <- spc_chart$data
    subdata$lcl <- pmax(subdata$lcl, 0)

    subdata$shift <- ifelse(subdata$y == subdata$cl, NA, subdata$y < subdata$cl)

    # Calculate trend column comparing consecutive y values
    differences <- c(diff(subdata$y), NA)  # Append NA for the last difference
    subdata$trend <- ifelse(differences < 0, TRUE, ifelse(differences > 0, FALSE, NA))
    subdata$trend[length(subdata$trend)] <- subdata$trend[length(subdata$trend) - 1]

    # Calculate sigma and sigma-based control limits
    subdata$sigma <- (subdata$ucl - subdata$cl) / 3
    subdata$cl_plus_1sigma <- subdata$cl + subdata$sigma
    subdata$cl_plus_2sigma <- subdata$cl + 2 * subdata$sigma
    subdata$cl_minus_1sigma <- subdata$cl - subdata$sigma
    subdata$cl_minus_2sigma <- subdata$cl - 2 * subdata$sigma

    return(subdata)
  }))

  # Return the updated data frame
  return(chart_data)
}
