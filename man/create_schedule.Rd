% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mendr.R
\name{create_schedule}
\alias{create_schedule}
\title{Create a schedule of tasks}
\usage{
create_schedule(schedule_data, starting_month, iterations = 1, filepath = NULL)
}
\arguments{
\item{schedule_data}{A data frame with at least two columns, the first of
type character (for tasks) and the second of type integer (for offsets). If
a third column is present, it should be of type character (for email
addresses).}

\item{starting_month}{A character string of the format \code{yyyy-mm} providing
the starting month for the schedule.}

\item{iterations}{An integer specifying the number of months you wish to
schedule. The default is 1.}

\item{filepath}{A file name to write to}
}
\value{
A \code{\link[=tibble]{tibble()}} with three columns and length equal to the number of
rows in your data frame multiplied by \code{iterations}.
}
\description{
\code{create_schedule} accepts a data frame with at least two columns
(tasks and offsets), and replaces the offsets with dates.
}
\section{Input data}{
The first column of your data frame should contain a list of tasks, for
example "Send email reminder" or "Export data". The column doesn't have to
be named \code{task}, but it does need to be the first column in the data frame.

The second column of your data frame should contain a list of integers,
which might be positive or negative, that define the number of working days
relative to the first working day of the month. For example, 0 is the first
working day of the month, 1 is the second working day of the month and so
on. Alternatively, -1 is the day before the first working day of the month,
-2 is two days before the first working day of the month etc. The column
doesn't have to be named \code{offset}, but it does need to be the second column
in the data frame.

The third column in your data frame, which is optional, should contain a
list of email addresses of individuals to whom the tasks are assigned. If a
task is assigned to more than one individual, you should separate their
email addresses with a semi-colon (\verb{;}). The column doesn't have to be
named \code{assigned_to}, but it does need to be the third column in the data
frame should you choose to include it.
}

\examples{
# Create a calendar of Auckland Council's known non-working days
create_calendar()

# Create a data frame of tasks and offsets
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
}
