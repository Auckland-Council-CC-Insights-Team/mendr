
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mendr

<!-- badges: start -->

[![R-CMD-check](https://github.com/Auckland-Council-CC-Insights-Team/mendr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Auckland-Council-CC-Insights-Team/mendr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of mendr is to simplify the task of creating a calendar of
dates for a recurring process.

## Installation

You can install the development version of mendr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Auckland-Council-CC-Insights-Team/mendr")
```

## Example

Create a schedule for the first three months of 2023, allowing for
non-working days at Auckland Council.

``` r
library(mendr)
# Create a calendar of Auckland Council's known non-working days
create_calendar()

# Create a data frame of tasks and offsets. Alternatively, you could
# read in an Excel file here using the [readr](https://readr.tidyverse.org/)
# package.
 tasks <- data.frame(
   tasks = c("Send email reminder", "Extract data", "Run report"),
   offsets = c(-2, 0, 1)
   )

# Pass the data frame to `create_schedule` to create a schedule for the first
# three months of 2023
create_schedule(
  schedule_data = tasks,
  starting_month = "2023-01",
  iterations = 3
  )
#>                 tasks       date
#> 1 Send email reminder 2022-12-22
#> 2        Extract data 2023-01-09
#> 3          Run report 2023-01-10
#> 4 Send email reminder 2023-01-27
#> 5        Extract data 2023-02-01
#> 6          Run report 2023-02-02
#> 7 Send email reminder 2023-02-27
#> 8        Extract data 2023-03-01
#> 9          Run report 2023-03-02
```
