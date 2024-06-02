#' Create SPC Data for P-Charts, U-Charts, P-Prime Charts, and U-Prime Charts with Dynamic Phases
#'
#' This function creates Statistical Process Control (SPC) chart data specifically for p-charts, u-charts, p-prime charts, and u-prime charts.
#' It ensures non-negative Lower Control Limits (LCL) and integrates additional statistical calculations such as trend analysis,
#' shift detection, sigma levels with control limits, and dynamic phase adjustments based on input.
#'
#' @param data Data frame containing the data.
#' @param date_col Name of the date column.
#' @param num_col Name of the numerator column (defects or defectives).
#' @param den_col Name of the denominator column (sample size or units).
#' @param chart_type Type of SPC chart to create ('p' for p-chart, 'u' for u-chart, 'pp' for p-prime chart, 'up' for u-prime chart).
#' @param phase A vector of numeric values specifying row numbers from which the phase value should incrementally increase.
#'              Each specified row starts a new phase, incrementing by 1 from the previous phase.
#'              If left blank, all phases default to 1.
#' @return Returns a data frame with SPC data including adjusted LCL values, shifts, trends, sigma calculations,
#'         additional control limit assessments, and dynamic phase adjustments.
#' @export
#' @importFrom qicharts2 qic
#' @importFrom lubridate parse_date_time
#' @importFrom dplyr mutate row_number select everything group_by summarise ungroup
#' @examples
#' data <- data.frame(date = seq(as.Date('2020-01-01'), by = 'month', length.out = 12),
#'                    defects = c(5, 6, 2, 8, 5, 9, 3, 4, 7, 1, 3, 2),
#'                    units = c(100, 90, 110, 100, 95, 105, 100, 95, 100, 90, 95, 105))
#'
#' # Example for p-chart
#' create_spc_dataframe_pu(data, 'date', 'defects', 'units', 'p', phase = c(5, 10))
#'
#' # Example for u-chart
#' create_spc_dataframe_pu(data, 'date', 'defects', 'units', 'u', phase = c(5, 10))
#'
#' # Example for p-prime chart
#' create_spc_dataframe_pu(data, 'date', 'defects', 'units', 'pp', phase = c(5, 10))
#'
#' # Example for u-prime chart
#' create_spc_dataframe_pu(data, 'date', 'defects', 'units', 'up', phase = c(5, 10))
create_spc_dataframe_pu <- function(data, date_col, num_col, den_col, chart_type, phase = numeric(0)) {
  # Parse the date column using common date formats
  data[[date_col]] <- parse_date_time(data[[date_col]],
                                      orders = c("my", "ym", "ymd", "mdy", "dmy", "ydm",
                                                 "ymd_HMS", "ymd_HM", "ymd_H",
                                                 "mdy_HMS", "mdy_HM", "mdy_H",
                                                 "dmy_HMS", "dmy_HM", "dmy_H",
                                                 "ydm_HMS", "ydm_HM", "ydm_H"))


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
    # Identify the starting row for each subgroup in the original data
    subgroup_starts <- subdata |>
      group_by(grp = floor_date(!!sym(date_col), "month")) |>
      summarise(subgroup_start = min(row_number())) |>
      ungroup()

    # Create the SPC chart for each phase using qicharts2
    spc_chart <- qic(subdata[[date_col]], subdata[[num_col]], subdata[[den_col]], data = subdata, chart = chart_type)

    # Take the data from the SPC chart
    modified_data <- spc_chart$data

    # Adjust y, lcl, ucl, and cl values to percentages for p-charts and u-charts
    modified_data$y <- modified_data$y * 100
    modified_data$lcl <- pmax(modified_data$lcl * 100, 0) # Ensure LCL is not less than 0
    modified_data$ucl <- modified_data$ucl * 100
    modified_data$cl <- modified_data$cl * 100

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

    # Define fifteen_more based on sigma limits
    modified_data$fifteen_more <- ifelse(
      modified_data$shift,
      modified_data$y > modified_data$cl_minus_1sigma,  # Condition when shift is TRUE
      modified_data$y < modified_data$cl_plus_1sigma    # Condition when shift is FALSE
    )

    # Define two_more based on two sigma limits and control limits
    modified_data$two_more <- ifelse(modified_data$shift,
                                     modified_data$y > modified_data$cl_minus_2sigma & modified_data$y < modified_data$lcl,
                                     ifelse(!modified_data$shift, modified_data$y > modified_data$cl_plus_2sigma & modified_data$y < modified_data$ucl, FALSE))

    # Ensure the phase column is carried over correctly
    modified_data$phase <- subdata$phase[1]  # Assign phase based on original subdata

    # Map subgroup_number to modified_data
    modified_data <- modified_data |>
      mutate(subgroup_number = rep(subgroup_starts$subgroup_start, length.out = nrow(modified_data)))

    return(modified_data)
  })

  # Reassemble the data into a single frame
  final_data <- do.call(rbind, results)

  # Add row_number as the first column
  final_data <- final_data |>
    mutate(row_number = row_number()) |>
    select(row_number, subgroup_number, everything())

  return(final_data)
}
