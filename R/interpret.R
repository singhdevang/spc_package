#' Interpret SPC Data for Special Cause Variations
#'
#' This function analyzes SPC data generated from `create_spc_data` to detect special cause variations
#' such as shifts, trends, and excessive deviations. It identifies significant events based on predefined
#' criteria and displays these events in a tabular format indicating the type of variation and its
#' duration or specific occurrence point.
#'
#' @param data A data frame containing SPC data, expected to include the columns `x` for the time or date,
#'   `shift`, `trend`, `fifteen_more`, and `sigma.signal`, which are logical vectors indicating where these
#'   conditions are met.
#'
#' @return Returns a data frame with two columns: `SpecialCauseVariation`, which describes the type of
#'   variation detected, and `Duration`, which provides the duration or the specific point of occurrence
#'   for sigma signals. For interactive sessions, the results are also displayed using `View` or `print`.
#'
#' @importFrom stats rle
#' @examples
#' # Assuming 'spc_data' is a dataframe obtained from `create_spc_data` function
#' spc_data <- data.frame(
#'   x = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
#'   shift = c(rep(FALSE, 10), rep(TRUE, 8), rep(FALSE, 6)),
#'   trend = c(rep(FALSE, 5), rep(TRUE, 10), rep(FALSE, 9)),
#'   fifteen_more = c(rep(FALSE, 15), rep(TRUE, 5), rep(FALSE, 4)),
#'   sigma.signal = c(rep(FALSE, 20), rep(TRUE, 4))
#' )
#' interpreted_data <- interpret(spc_data)
#' print(interpreted_data)
#'
#' @export
interpret <- function(data) {
  # Helper function to compute RLE while skipping NAs, focusing on the specific column
  compute_rle_skip_na <- function(column) {
    na_positions <- is.na(column)
    clean_data <- column[!na_positions]
    rle_data <- rle(clean_data)
    actual_indices <- which(!na_positions)
    return(list(lengths = rle_data$lengths, values = rle_data$values, actual_indices = actual_indices))
  }

  # Initialize an empty data frame to store results
  results <- data.frame(SpecialCauseVariation = character(), Duration = character(), stringsAsFactors = FALSE)

  # Detect Shifts
  rle_shift <- compute_rle_skip_na(data$shift)
  for (i in seq_along(rle_shift$values)) {
    if (rle_shift$values[i] == TRUE && rle_shift$lengths[i] >= 8) {  # Custom condition for identifying significant shifts
      start_index <- sum(rle_shift$lengths[1:(i-1)]) + 1
      end_index <- sum(rle_shift$lengths[1:i])
      start_date <- data$x[rle_shift$actual_indices[start_index]]
      end_date <- data$x[rle_shift$actual_indices[end_index]]
      results <- rbind(results, data.frame(SpecialCauseVariation = "Shift", Duration = paste(start_date, end_date, sep = " - ")))
    }
  }

  # Detect Trends
  rle_trend <- compute_rle_skip_na(data$trend)
  for (i in seq_along(rle_trend$values)) {
    if (rle_trend$values[i] == TRUE && rle_trend$lengths[i] >= 6) {  # Custom condition for identifying significant trends
      start_index <- sum(rle_trend$lengths[1:(i-1)]) + 1
      end_index <- sum(rle_trend$lengths[1:i])
      start_date <- data$x[rle_trend$actual_indices[start_index]]
      end_date <- data$x[rle_trend$actual_indices[end_index]]
      results <- rbind(results, data.frame(SpecialCauseVariation = "Trend", Duration = paste(start_date, end_date, sep = " - ")))
    }
  }

  # Detect Long Runs of 15+
  rle_fifteen <- rle(data$fifteen_more)
  for (i in seq_along(rle_fifteen$values)) {
    if (rle_fifteen$values[i] == TRUE && rle_fifteen$lengths[i] >= 15) {
      start_index <- sum(rle_fifteen$lengths[1:(i-1)]) + 1
      end_index <- sum(rle_fifteen$lengths[1:i])
      start_date <- data$x[start_index]
      end_date <- data$x[end_index]
      results <- rbind(results, data.frame(SpecialCauseVariation = "15+", Duration = paste(start_date, end_date, sep = " - ")))
    }
  }
  # Detect Sigma Signals
  sigma_indices <- which(data$sigma.signal == TRUE)
  if (length(sigma_indices) > 0) {
    for (i in sigma_indices) {
      date_point <- data$x[i]
      results <- rbind(results, data.frame(SpecialCauseVariation = "Sigma Signal", Duration = as.character(date_point)))
    }
  }
  # Automatically view the results table in RStudio or similar
  if (interactive()) {
    View(results)
  } else {
    print(results)
  }
  return(results)
}
