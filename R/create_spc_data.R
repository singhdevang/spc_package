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

  # Create a list to hold data frames split by phase
  split_data <- split(chart_data, chart_data$phase)

  # Phase-dependent recalculations
  results <- lapply(split_data, function(subdata) {
    # Recreate the SPC chart for each phase using qicharts2
    spc_chart <- qic(subdata[[date_col]], subdata[[value_col]], data = subdata, chart = chart_type)

    # Take the data from the SPC chart
    modified_data <- spc_chart$data
    modified_data$lcl <- pmax(modified_data$lcl, 0)

    # Calculate shift, trend, sigma, and control limits
    modified_data$shift <- ifelse(modified_data$y == modified_data$cl, NA, modified_data$y < modified_data$cl)
    differences <- c(diff(modified_data$y), NA)  # Append NA for the last difference
    modified_data$trend <- ifelse(differences < 0, TRUE, ifelse(differences > 0, FALSE, NA))
    modified_data$trend[length(modified_data$trend)] <- modified_data$trend[length(modified_data$trend) - 1]
    modified_data$sigma <- (modified_data$ucl - modified_data$cl) / 3
    modified_data$cl_plus_1sigma <- modified_data$cl + modified_data$sigma
    modified_data$cl_plus_2sigma <- modified_data$cl + 2 * modified_data$sigma
    modified_data$cl_minus_1sigma <- modified_data$cl - modified_data$sigma
    modified_data$cl_minus_2sigma <- modified_data$cl - 2 * modified_data$sigma

    # Ensure the phase column is carried over correctly
    modified_data$phase <- subdata$phase[1]  # Assign phase based on original subdata

    return(modified_data)
  })

  # Reassemble the data into a single frame
  final_data <- do.call(rbind, results)

  return(final_data)
}
