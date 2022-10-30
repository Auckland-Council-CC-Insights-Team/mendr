#' List all known standard non-working days at Auckland Council
#'
#' @description `auckland_council_holidays()` is an internal function that lists
#' the dates when kaimahi at Auckland Council are not expected to work, either
#' because of public holidays or because of an organisation-wide shutdown.
#'
#' @return A numeric vector containing dates, beginning on 24 October 2022.
#'
#' @noRd
auckland_council_holidays <- function() {
  labour_day <- c(ymd("2022-10-24"), ymd("2023-10-23"), ymd("2024-10-28"))
  christmas_shutdown <- c(
    seq(ymd("2022-12-26"), ymd("2023-01-06"), by = "days"),
    seq(ymd("2023-12-25"), ymd("2024-01-05"), by = "days")
  )
  auckland_anniversary <- c(ymd("2023-01-30"), ymd("2024-01-29"))
  waitangi_day <- c(ymd("2023-02-06"), ymd("2024-02-06"))
  good_friday <- c(ymd("2023-04-07"), ymd("2024-03-29"))
  easter_monday <- c(ymd("2023-04-10"), ymd("2024-04-01"))
  anzac_day <- c(ymd("2023-04-25"), ymd("2024-04-25"))
  kings_birthday <- c(ymd("2023-06-05"), ymd("2024-06-03"))
  matariki <- c(ymd("2023-07-14"), ymd("2024-06-28"))

  non_working_days <- c(
    labour_day,
    christmas_shutdown,
    auckland_anniversary,
    waitangi_day,
    good_friday,
    easter_monday,
    anzac_day,
    kings_birthday,
    matariki
  )

  return(non_working_days)
}

#' Create a calendar of non-working days
#'
#' @description `create_calendar` creates a calendar and stores it in the
#'   calendar register for use when calling `create_schedule`.
#'
#' @param holidays A vector of Dates which contains the non-working days. By
#'   default, this returns a list of non-working days for staff at Auckland
#'   Council.
#' @param weekend A character vector of days that are weekends. It accepts:
#'   `sunday`, `monday`, `tuesday`, `wednesday`, `thursday`, `friday`,
#'   `saturday.` By default, Saturday and Sunday are assumed to be the days of
#'   the weekend.
#'
#' @export
#'
#' @examples
#' # A calendar of non-working days for the first Quarter of 2023, with weekends
#' # set to the default (Saturdays and Sundays)
#' q1_2023 <- c(
#'   seq(as.Date("2023-01-02"), as.Date("2023-01-06"), by = "days"),
#'   as.Date("2023-01-30"),
#'   as.Date("2023-02-06")
#' )
#' create_calendar(holidays = q1_2023)
create_calendar <- function(holidays = auckland_council_holidays(), weekend = c("sunday", "saturday")) {
  bizdays::create.calendar(
    name = "NonWorkingDays",
    holidays = holidays,
    weekdays = weekend
  )
}

#' Create a schedule of tasks
#'
#' @description `create_schedule` accepts a data frame with at least two columns
#'   (tasks and offsets), and replaces the offsets with dates.
#'
#'   # Input data
#'
#'   The first column of your data frame should contain a list of tasks, for
#'   example "Send email reminder" or "Export data". The column doesn't have to
#'   be named `task`, but it does need to be the first column in the data frame.
#'
#'   The second column of your data frame should contain a list of integers,
#'   which might be positive or negative, that define the number of working days
#'   relative to the first working day of the month. For example, 0 is the first
#'   working day of the month, 1 is the second working day of the month and so
#'   on. Alternatively, -1 is the day before the first working day of the month,
#'   -2 is two days before the first working day of the month etc. The column
#'   doesn't have to be named `offset`, but it does need to be the second column
#'   in the data frame.
#'
#'   The third column in your data frame, which is optional, should contain a
#'   list of email addresses of individuals to whom the tasks are assigned. If a
#'   task is assigned to more than one individual, you should separate their
#'   email addresses with a semi-colon (`;`). The column doesn't have to be
#'   named `assigned_to`, but it does need to be the third column in the data
#'   frame should you choose to include it.
#'
#' @param schedule_data A data frame with at least two columns, the first of
#'   type character (for tasks) and the second of type integer (for offsets). If
#'   a third column is present, it should be of type character (for email
#'   addresses).
#' @param starting_month A character string of the format `yyyy-mm` providing
#'   the starting month for the schedule.
#' @param iterations An integer specifying the number of months you wish to
#'   schedule. The default is 1.
#' @param filepath A file name to write to
#'
#' @return A [tibble()] with three columns and length equal to the number of
#'   rows in your data frame multiplied by `iterations`.
#' @export
#'
#' @examples
#' # Create a calendar of Auckland Council's known non-working days
#' create_calendar()
#'
#' # Create a data frame of tasks and offsets
#' tasks <- data.frame(
#'   tasks = c("Send email reminder", "Extract data", "Run report"),
#'   offsets = c(-2, 0, 1)
#' )
#'
#' # Pass the data frame to `create_schedule` to create a schedule for the first
#' # three months of 2023
#' create_schedule(
#'   schedule_data = tasks,
#'   starting_month = "2023-01",
#'   iterations = 3
#' )
create_schedule <- function(schedule_data, starting_month, iterations = 1, filepath = NULL) {
  first_month <- ymd(paste0(starting_month, "-01"))
  final_month <- first_month |> lubridate::add_with_rollback(months(iterations - 1))
  starting_months <- seq(first_month, final_month, by = "months")

  schedule <- purrr::map_dfr(
    .x = starting_months,
    .f = ~ schedule_month(schedule_data, anchor_month = .x)
  )

  if (!is.null(filepath)) {
    write_xlsx(schedule, path = paste(filepath, ".xlsx"))
  }

  return(schedule)
}

#' Schedule dates for a single month
#'
#' @description Given a series of tasks, offsets relative to the first working day
#'   of the month, and a starting month, convert the offsets into actual dates.
#'
#' @param schedule_data A data frame with at least two columns, the first of
#'   type character (for tasks) and the second of type integer (for offsets). If
#'   a third column is present, it should be of type character (for email
#'   addresses).
#' @param anchor_month A date, where the day is the first calendar day of the month.
#'
#' @return A [tibble()] with three columns and length equal to the number of
#'   rows (i.e. tasks) passed via `schedule_data`.
#'
#' @noRd
schedule_month <- function(schedule_data, anchor_month) {
  anchor_date <- bizdays::getdate("first bizday", anchor_month - lubridate::days(1), "NonWorkingDays")

  processed_schedule <- schedule_data |>
    mutate(
      date = bizdays::add.bizdays(anchor_date, dplyr::cur_data()[[2]], "NonWorkingDays"),
      .keep = "unused"
    )

  return(processed_schedule)
}
