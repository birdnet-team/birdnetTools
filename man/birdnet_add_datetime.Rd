% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_column_editing.R
\name{birdnet_add_datetime}
\alias{birdnet_add_datetime}
\title{Add datetime-related columns from BirdNET output filenames}
\usage{
birdnet_add_datetime(data, tz = "UTC")
}
\arguments{
\item{data}{A data frame containing BirdNET output.}

\item{tz}{A character string specifying the time zone to assign when parsing datetime.
Defaults to \code{"UTC"}.}
}
\value{
A data frame with additional columns:
\describe{
\item{datetime}{POSIXct datetime parsed from the filename.}
\item{date}{Date portion of the datetime.}
\item{year}{Year of detection.}
\item{month}{Month of detection.}
\item{mday}{Day of the month.}
\item{yday}{Day of the year.}
\item{hour}{Hour of the day.}
\item{minute}{Minute of the hour.}
}
}
\description{
Extracts and parses datetime information from filenames in the file path column
(automatically detected), then adds several useful time-related columns
(e.g., \code{date}, \code{year}, \code{month}, \code{hour}) to the input data frame.
}
\details{
The function uses \link{birdnet_detect_columns} to find the column
containing file paths based on common name patterns (e.g., "file", "path").
Filenames are expected to contain a datetime string in the format
\code{"YYYYMMDD_HHMMSS"} or similar.
}
\examples{
\dontrun{
combined_data <- birdnet_combine("path/to/BirdNET/output")
data_with_time <- birdnet_add_datetime(combined_data)
}

}
