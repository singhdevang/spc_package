#' Interpret SPC Data for Special Cause Variations with Phase Handling
#'
#' This function analyzes SPC data generated from `create_spc_data` or `create_spc_data_pu` to detect special cause variations
#' such as shifts, trends, and excessive deviations. It identifies significant events based on predefined
#' criteria and displays these events in a tabular format indicating the type of variation, its duration or specific occurrence point,
#' and the phase in which it occurred.
#'
#' @param data A data frame containing SPC data, expected to include the columns `x` for the time or date,
#'   `shift`, `trend`, `fifteen_more`, `sigma.signal`, `two_more`, `phase`, and `row_number`.
#'
#' @return Returns a data frame with columns: `SpecialCauseVariation`, which describes the type of
#'   variation detected, `Duration`, which provides the duration or the specific point of occurrence
#'   for sigma signals, `Phase`, which indicates the phase in which the variation was detected, `RowNumber`, which indicates the row number(s) of the special cause variation,
#'   and `SubgroupNumber`, which indicates the subgroup number(s) of the special cause variation.
#'   For interactive sessions, the results are also displayed using `View` or `print`.
#' @examples
#' # Assuming 'spc_data' is a dataframe obtained from `create_spc_data` function
#' spc_data <- data.frame(
#'   x = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
#'   shift = c(rep(FALSE, 10), rep(TRUE, 8), rep(FALSE, 6)),
#'   trend = c(rep(FALSE, 5), rep(TRUE, 10), rep(FALSE, 9)),
#'   fifteen_more = c(rep(FALSE, 15), rep(TRUE, 5), rep(FALSE, 4)),
#'   sigma.signal = c(rep(FALSE, 20), rep(TRUE, 4)),
#'   two_more = c(rep(FALSE, 22), rep(TRUE, 2)),
#'   phase = c(rep(1, 12), rep(2, 12)),
#'   row_number = 1:24,
#'   subgroup_number = rep(1:4, each = 6)
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
  results <- data.frame(SpecialCauseVariation = character(), Duration = character(), Phase = integer(), RowNumber = integer(), SubgroupNumber = integer(), stringsAsFactors = FALSE)

  # Ensure 'subgroup_number' is present and handle missing values
  if (!"subgroup_number" %in% colnames(data)) {
    data$subgroup_number <- NA_integer_
  }

  # Split the data by phase
  data_list <- split(data, data$phase)

  # Function to interpret each phase separately
  interpret_phase <- function(subdata) {
    phase_results <- data.frame(SpecialCauseVariation = character(), Duration = character(), Phase = integer(), RowNumber = integer(), SubgroupNumber = integer(), stringsAsFactors = FALSE)
    phase <- unique(subdata$phase)

    # Shift detection logic
    rle_shift <- rle(subdata$shift)
    pos <- 1
    for (i in seq_along(rle_shift$lengths)) {
      if (!is.na(rle_shift$values[i]) && rle_shift$lengths[i] >= 8) {
        indices <- pos:(pos + rle_shift$lengths[i] - 1)
        start_index <- indices[1]
        end_index <- indices[length(indices)]
        start_date <- subdata$x[start_index]
        end_date <- subdata$x[end_index]
        row_number <- subdata$row_number[start_index]
        subgroup_number <- subdata$subgroup_number[start_index]
        phase_results <- rbind(phase_results, data.frame(SpecialCauseVariation = "Shift",
                                                         Duration = paste(start_date, end_date, sep = " - "),
                                                         Phase = phase, RowNumber = row_number, SubgroupNumber = ifelse(is.na(subgroup_number), -1, subgroup_number), stringsAsFactors = FALSE))
      }
      pos <- pos + rle_shift$lengths[i]
    }

    # Trend detection logic
    differences <- c(diff(subdata$y), NA)
    trends <- ifelse(differences < 0, TRUE, ifelse(differences > 0, FALSE, NA))
    rle_trend <- rle(trends)
    pos <- 1
    for (i in seq_along(rle_trend$lengths)) {
      if (rle_trend$lengths[i] >= 5) {
        indices <- pos:(pos + rle_trend$lengths[i] - 1)
        if (length(differences) >= max(indices) + 1) {
          indices <- c(indices, max(indices) + 1)
        }
        actual_indices <- which(!is.na(trends))
        real_indices <- actual_indices[indices]

        start_date <- subdata$x[real_indices[1]]
        end_date <- subdata$x[real_indices[length(real_indices)]]
        row_number <- subdata$row_number[real_indices[1]]
        subgroup_number <- subdata$subgroup_number[real_indices[1]]

        phase_results <- rbind(phase_results, data.frame(SpecialCauseVariation = "Trend",
                                                         Duration = paste(start_date, end_date, sep = " - "),
                                                         Phase = phase, RowNumber = row_number, SubgroupNumber = ifelse(is.na(subgroup_number), -1, subgroup_number), stringsAsFactors = FALSE))
      }
      pos <- pos + rle_trend$lengths[i]
    }

    # Detect Long Runs of 15+
    rle_fifteen <- rle(subdata$fifteen_more)
    pos <- 1
    print(paste("RLE Values:", toString(rle_fifteen$values)))
    print(paste("RLE Lengths:", toString(rle_fifteen$lengths)))

    for (i in seq_along(rle_fifteen$values)) {
      if (rle_fifteen$values[i] == TRUE && rle_fifteen$lengths[i] >= 15) {
        start_index <- if (i > 1) sum(rle_fifteen$lengths[1:(i-1)]) + 1 else 1
        end_index <- sum(rle_fifteen$lengths[1:i])
        print(paste("Dynamic Start Index:", start_index))
        print(paste("Dynamic End Index:", end_index))

        start_date <- subdata$x[start_index]
        end_date <- subdata$x[end_index]

        print(paste("Start Date:", start_date))
        print(paste("End Date:", end_date))

        row_number <- subdata$row_number[start_index]
        subgroup_number <- subdata$subgroup_number[start_index]

        # Additional debug prints
        print(paste("Row Number:", row_number))
        print(paste("Subgroup Number:", ifelse(is.na(subgroup_number), -1, subgroup_number)))
        print(paste("Phase:", phase))

        # Ensure all components are non-empty and have the correct lengths
        if (length(start_date) > 0 && length(end_date) > 0 && length(row_number) > 0) {
          phase_results <- rbind(phase_results, data.frame(SpecialCauseVariation = "15+",
                                                           Duration = paste(start_date, end_date, sep = " - "),
                                                           Phase = phase, RowNumber = row_number, SubgroupNumber = ifelse(is.na(subgroup_number), -1, subgroup_number), stringsAsFactors = FALSE))
        } else {
          print("Skipping due to empty components")
        }
      }
      pos <- pos + rle_fifteen$lengths[i]
    }

    # Detect Sigma Signals
    sigma_indices <- which(subdata$sigma.signal == TRUE)
    for (i in sigma_indices) {
      date_point <- subdata$x[i]
      row_number <- subdata$row_number[i]
      subgroup_number <- subdata$subgroup_number[i]
      phase_results <- rbind(phase_results, data.frame(SpecialCauseVariation = "Sigma Signal",
                                                       Duration = as.character(date_point),
                                                       Phase = phase, RowNumber = row_number, SubgroupNumber = ifelse(is.na(subgroup_number), -1, subgroup_number), stringsAsFactors = FALSE))
    }

    # Apply the 'Two Out of Three' rule
    if (length(subdata$two_more) >= 3) {
      for (i in 3:length(subdata$two_more)) {
        if (sum(subdata$two_more[(i-2):i]) >= 2) {
          start_date <- subdata$x[i-2]
          end_date <- subdata$x[i]
          row_number <- subdata$row_number[i-2]
          subgroup_number <- subdata$subgroup_number[i-2]
          phase_results <- rbind(phase_results, data.frame(SpecialCauseVariation = "Two Out of Three",
                                                           Duration = paste(start_date, end_date, sep = " - "),
                                                           Phase = phase, RowNumber = row_number, SubgroupNumber = ifelse(is.na(subgroup_number), -1, subgroup_number), stringsAsFactors = FALSE))
        }
      }
    }

    return(phase_results)
  }

  # Apply interpretation to each phase
  phase_results_list <- lapply(data_list, interpret_phase)
  results <- do.call(rbind, phase_results_list)

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
