% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpret.R
\name{interpret}
\alias{interpret}
\title{Interpret SPC Data for Special Cause Variations with Phase Handling}
\usage{
interpret(data)
}
\arguments{
\item{data}{A data frame containing SPC data, expected to include the columns \code{x} for the time or date,
\code{shift}, \code{trend}, \code{fifteen_more}, \code{sigma.signal}, \code{two_more}, \code{phase}, and \code{row_number}.}
}
\value{
Returns a data frame with columns: \code{SpecialCauseVariation}, which describes the type of
variation detected, \code{Duration}, which provides the duration or the specific point of occurrence
for sigma signals, \code{Phase}, which indicates the phase in which the variation was detected, \code{RowNumber}, which indicates the row number(s) of the special cause variation,
and \code{PhaseNumber}, which indicates the subgroup number(s) of the special cause variation.
For interactive sessions, the results are also displayed using \code{View} or \code{print}.
}
\description{
This function analyzes SPC data generated from \code{create_spc_dataframe} or \code{create_spc_dataframe_pu} to detect special cause variations
such as shifts, trends, and excessive deviations. It identifies significant events based on predefined
criteria and displays these events in a tabular format indicating the type of variation, its duration or specific occurrence point,
and the phase in which it occurred.
}
\examples{
# Assuming 'spc_data' is a dataframe obtained from `create_spc_dataframe` function
spc_data <- data.frame(
  x = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
  shift = c(rep(FALSE, 10), rep(TRUE, 8), rep(FALSE, 6)),
  trend = c(rep(FALSE, 5), rep(TRUE, 10), rep(FALSE, 9)),
  fifteen_more = c(rep(FALSE, 15), rep(TRUE, 5), rep(FALSE, 4)),
  sigma.signal = c(rep(FALSE, 20), rep(TRUE, 4)),
  two_more = c(rep(FALSE, 22), rep(TRUE, 2)),
  phase = c(rep(1, 12), rep(2, 12)),
  row_number = 1:24,
  phase_number = rep(1:4, each = 6)
)
interpreted_data <- interpret(spc_data)
print(interpreted_data)

}
