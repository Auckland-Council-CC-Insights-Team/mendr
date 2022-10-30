test_that("we can retrieve a list of Auckland Council's non-working days", {
  expect_type(auckland_council_holidays(), "double")
  expect_equal(auckland_council_holidays() |> min(), as.Date("2022-10-24"))
  expect_equal(auckland_council_holidays() |> max(), as.Date("2024-10-28"))
})

test_that("we can create a calendar of non-working days", {
  holidays <- c(
    as.Date("2022-12-25"),
    as.Date("2022-12-26"),
    as.Date("2023-01-02"),
    as.Date("2023-01-03"),
    as.Date("2023-02-01")
  )

  create_calendar(holidays)

  expect_equal(bizdays::bizdays("2023-01-01", "2023-01-06", "NonWorkingDays"), 2)
})

test_that("we can create a schedule of tasks for several months", {
  create_calendar()

  tasks <- data.frame(
    tasks = c("Send email reminder", "Extract data", "Run report"),
    offsets = c(-2, 0, 1)
  )

  iterations <- 3

  schedule <- create_schedule(
    schedule_data = tasks,
    starting_month = "2023-01",
    iterations = iterations
  )

  # the number of rows in the schedule should equal the number of rows in our
  # dataset, multiplied by the number of iterations
  expect_equal(nrow(schedule), nrow(tasks) * iterations)
  expect_equal(max(schedule$date), as.Date("2023-03-02"))
})

test_that("we can create a schedule of tasks for one month", {
  create_calendar()

  unassigned_tasks <- data.frame(
    tasks = c("Send email reminder", "Extract data", "Run report"),
    offsets = c(-2, 0, 1)
  )

  assigned_tasks <- data.frame(
    tasks = c("Send email reminder", "Extract data", "Run report"),
    offsets = c(-2, 0, 1),
    assigned_to = c("test@test.com", NA, NA)
  )

  anchor_month <- as.Date("2023-01-01")

  expect_equal(
    schedule_month(unassigned_tasks, anchor_month) |> dplyr::pull(date) |> max(),
    as.Date("2023-01-10")
  )

  expect_equal(
    schedule_month(assigned_tasks, anchor_month) |> dplyr::pull(date) |> max(),
    as.Date("2023-01-10")
  )
})
