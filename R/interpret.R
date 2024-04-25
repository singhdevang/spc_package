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
    if (rle_shift$lengths[i] >= 8) {  # Check if the run length is 8 or more
      start_index = sum(rle_shift$lengths[1:(i-1)]) + 1
      end_index = sum(rle_shift$lengths[1:i])
      start_date = data$x[rle_shift$actual_indices[start_index]]
      end_date = data$x[rle_shift$actual_indices[end_index]]
      results <- rbind(results, data.frame(SpecialCauseVariation = "Shift",
                                           Duration = paste(start_date, end_date, sep = " - ")))
    }
  }
  # Function to compute RLE while skipping NAs for trend detection
  compute_rle_skip_na_trend <- function(differences) {
    na_positions <- is.na(differences)
    clean_differences <- differences[!na_positions]  # Remove NAs
    trends <- ifelse(clean_differences < 0, TRUE, ifelse(clean_differences > 0, FALSE, NA))
    rle_trends <- rle(trends)  # Compute RLE on cleaned trends data
    return(list(lengths = rle_trends$lengths, values = rle_trends$values, na_positions = na_positions))
  }

  # Apply the 'Two Out of Three' rule for detection
  if (length(data$two_more) >= 3) { # Ensure there are at least three data points to check
    for (i in 3:length(data$two_more)) {
      if (sum(data$two_more[(i-2):i]) >= 2) {
        start_date = data$x[i-2]
        end_date = data$x[i]
        results <- rbind(results, data.frame(SpecialCauseVariation = "Two Out of Three",
                                             Duration = paste(start_date, end_date, sep = " - ")))
      }
    }
  }

  # Compute differences and apply RLE skipping NA for trend analysis
  differences <- c(diff(data$y), NA)  # Calculate differences and append NA for the last value
  rle_trend <- compute_rle_skip_na_trend(differences)

  # Identify long trend runs (example: 6 or more consecutive)
  long_trend_runs <- which(rle_trend$lengths >= 5)  # Using 6 as per your requirement
  for (i in long_trend_runs) {
    indices <- sum(rle_trend$lengths[1:(i-1)]) + 1 : rle_trend$lengths[i]

    # Extend the trend marking to one additional point if possible
    if (length(differences) > max(indices)) {
      indices <- c(indices, max(indices) + 1)
    }

    # Convert indices back to include NAs
    actual_indices <- which(!rle_trend$na_positions)
    real_indices <- actual_indices[indices]

    # Find start and end dates including the extended point
    start_date <- data$x[real_indices[1]]
    end_date <- data$x[real_indices[length(real_indices)]]

    # Record the trend in results with extended end date
    results <- rbind(results, data.frame(SpecialCauseVariation = "Trend", Duration = paste(start_date, end_date, sep = " - ")))
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
  for (i in sigma_indices) {
    date_point <- data$x[i]
    results <- rbind(results, data.frame(SpecialCauseVariation = "Sigma Signal", Duration = as.character(date_point)))
  }

  if (nrow(results) == 0) {
    message("No special cause variation detected; process is stable")
    invisible(NULL)  # Suppress any output other than the message
  } else {
    # Print or view results if variations are detected
    if (interactive()) {
      View(results)
    } else {
      print(results)
    }
    return(results)
  }
}
