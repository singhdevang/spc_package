Improvement Cymru SPC Maker
Overview
The Improvement Cymru SPC Maker is an R package designed to facilitate the creation of Statistical Process Control (SPC) data frames, generate SPC charts adhering to Improvement Cymru brand guidelines, and perform advanced interpretation to highlight special causes of variation. This package is specifically tailored to assist analysts and healthcare professionals in monitoring, understanding, and improving healthcare processes.

Features
Dynamic Phase Handling: Allows for flexible phase changes within SPC charts to adapt to process shifts and changes.
Advanced Statistical Calculations: Includes shift detection, trend analysis, sigma levels with control limits, and additional control limit assessments.
Customizable SPC Charts: Provides functions to create and customize SPC charts with annotations and phase-specific adjustments.
Special Cause Variation Detection: Analyzes SPC data to detect and report on significant variations, making it easier to identify and act upon process improvements.
Integration with Popular R Packages: Leverages the capabilities of ggplot2, dplyr, lubridate, qicharts2, and rlang for powerful data manipulation and visualization.
Installation
To install the Improvement Cymru SPC Maker package, use the following commands in R:

R
Copy code
# Install the devtools package if not already installed
install.packages("devtools")

# Install the Improvement Cymru SPC Maker package from GitHub
devtools::install_github("your_github_username/icspc")
Usage
Creating SPC Data with Dynamic Phases
The create_spc_dataframe function creates SPC chart data and ensures Lower Control Limits (LCL) values are not negative. It includes multiple statistical calculations and dynamic phase adjustments based on input.

R
Copy code
library(icspc)
data <- data.frame(date = seq(as.Date('2020-01-01'), by = 'month', length.out = 30),
                   value = rnorm(30, 100, 15))
spc_data <- create_spc_dataframe(data, 'date', 'value', 'xbar', phase = c(10, 20))
Creating SPC Data for P-Charts and U-Charts
The create_spc_dataframe_pu function generates SPC chart data specifically for p-charts and u-charts, ensuring non-negative LCL values.

R
Copy code
data <- data.frame(date = seq(as.Date('2020-01-01'), by = 'month', length.out = 12),
                   defects = c(5, 6, 2, 8, 5, 9, 3, 4, 7, 1, 3, 2),
                   units = c(100, 90, 110, 100, 95, 105, 100, 95, 100, 90, 95, 105))
spc_data_pu <- create_spc_dataframe_pu(data, 'date', 'defects', 'units', 'u', phase = c(5, 10))
Plotting SPC Charts
The plot_spc_chart function generates SPC charts with phase handling and annotations, ensuring that date columns are treated as categorical variables for accurate plotting.

R
Copy code
annotations <- data.frame(
  row_number = c(3, 9),
  label = c("Annotation 1", "Annotation 2"),
  text_size = c(4, 4),
  position_x = c(0.2, 0),
  position_y = c(10, 10)
)
chart <- plot_spc_chart(spc_data, "Monthly SPC Chart", 15, "Source: Imaginary database", 10, annotations)
print(chart)
Running SPC Chart Tests
The plot_test_spc_chart function produces an SPC chart with advanced highlighting rules to emphasize special cause variations and trends.

R
Copy code
spc_data <- create_spc_dataframe(your_data_frame, 'date', 'value', 'xbar')
chart <- plot_test_spc_chart(spc_data, "Comprehensive Monthly SPC Chart", annotations = annotations)
print(chart)
Interpreting SPC Data
The interpret function analyzes SPC data to detect special cause variations, presenting the findings in a tabular format.

R
Copy code
spc_data <- data.frame(
  x = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
  shift = c(rep(FALSE, 10), rep(TRUE, 8), rep(FALSE, 6)),
  trend = c(rep(FALSE, 5), rep(TRUE, 10), rep(FALSE, 9)),
  fifteen_more = c(rep(FALSE, 15), rep(TRUE, 5), rep(FALSE, 4)),
  sigma.signal = c(rep(FALSE, 20), rep(TRUE, 4)),
  two_more = c(rep(FALSE, 22), rep(TRUE, 2)),
  phase = c(rep(1, 12), rep(2, 12)),
  row_number = 1:24,
  subgroup_number = rep(1:4, each = 6)
)
interpretation <- interpret(spc_data)
print(interpretation)
License
This package is licensed under the MIT License. See the LICENSE file for more details.

Contributing
We welcome contributions to enhance the Improvement Cymru SPC Maker package. If you would like to contribute, please fork the repository, create a new branch, and submit a pull request.

For any issues or suggestions, please open an issue on GitHub.

Contact
Improvement Cymru Analytics Team
Email: analytics-improvementcymru@wales.nhs.uk
