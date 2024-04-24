#' Interpret SPC Data
#'
#' This function interprets the SPC data to detect special cause variations such as shifts, trends,
#' 15+ consecutive points, and sigma signals. It outputs a table summarizing these findings with
#' the range of occurrences.
#'
#' @param data A data frame containing the SPC data including at least the columns 'x' for dates,
#'        'y' for the measurement values, and additional calculated columns for analysis.
#' @return A data frame with columns "Special Cause Variation" and "Duration" indicating the type
#'         of variation and the corresponding range of dates.
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
