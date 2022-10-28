create_calendar <- function() {
  holidays <- c(
    ymd("2022-10-24"),
    seq(ymd("2022-12-26"), ymd("2023-01-06"), by = "days"),
    ymd("2023-01-30"),
    ymd("2023-02-06"),
    ymd("2023-04-07"),
    ymd("2023-04-10"),
    ymd("2023-04-25"),
    ymd("2023-06-05"),
    ymd("2023-07-14")
  )

  bizdays::create.calendar(
    name = "NonWorkingDays",
    holidays = holidays,
    weekdays = c("sunday", "saturday")
  )
}

create_schedule <- function(schedule_data, starting_month, scheduling_months, filename = NULL) {
  first_month <- ymd(paste0(starting_month, "-01"))
  final_month <- first_month %m+% months(scheduling_months-1)
  starting_months <- seq(first_month, final_month, by = "months")

  schedule <- purrr::map_dfr(
    .x = starting_months,
    .f = ~schedule_month(schedule_data, starting_month = .x)
  )

  if(!is.null(filename)) {
    write_xlsx(schedule, path = here(paste0(filename, ".xlsx")))
  }

  return(schedule)
}

schedule_month <- function(schedule_data, starting_month) {
  anchor_date <- bizdays::getdate("first bizday", starting_month - lubridate::days(1), "NonWorkingDays")

  processed_schedule <- schedule_data |>
    mutate(
      date =  bizdays::add.bizdays(anchor_date, offset, "NonWorkingDays")
    ) |>
    select(task = 1, date, assigned_to = 3)

  return(processed_schedule)
}
