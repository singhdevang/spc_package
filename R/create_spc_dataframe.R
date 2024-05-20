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
#' @importFrom lubridate parse_date_time floor_date
#' @importFrom dplyr mutate row_number select everything group_by summarise ungroup
#' @importFrom rlang sym
#' @examples
#' data <- data.frame(date = seq(as.Date('2020-01-01'), by = 'month', length.out = 30),
#'                    value = rnorm(30, 100, 15))
#' create_spc_data(data, 'date', 'value', 'xbar', phase = c(10, 20))
create_spc_dataframe <- function(data, date_col, value_col, chart_type, phase = numeric(0)) {
  # Ensure required packages are loaded


  # Try to parse the date column using common date formats
  data[[date_col]] <- parse_date_time(data[[date_col]],
                                      orders = c("ymd_HMS", "ymd_HM", "ymd_H", "ymd",
                                                 "mdy_HMS", "mdy_HM", "mdy_H", "mdy",
                                                 "dmy_HMS", "dmy_HM", "dmy_H", "dmy",
                                                 "ydm_HMS", "ydm_HM", "ydm_H", "ydm",
                                                 "ym"))

  # Initialize phase column
  chart_data <- data |>
    mutate(original_row_number = row_number())
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
      summarise(subgroup_start = min(original_row_number)) |>
      ungroup()

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
    # Define fifteen_more based on sigma limits
    modified_data$fifteen_more <- ifelse(
      modified_data$shift,
      modified_data$y > modified_data$cl_minus_1sigma,  # Condition when shift is TRUE
      modified_data$y < modified_data$cl_plus_1sigma    # Condition when shift is FALSE
    )

    # Define two_more based on two sigma limits and control limits
    modified_data$two_more <- ifelse(modified_data$shift, modified_data$y > modified_data$cl_minus_2sigma & modified_data$y < modified_data$lcl,
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
    select(row_number, subgroup_number,everything())

  return(final_data)
}
