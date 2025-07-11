% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/birdnet_heatmap.R
\name{birdnet_heatmap}
\alias{birdnet_heatmap}
\title{Create a heatmap of BirdNET detections by hour and date}
\usage{
birdnet_heatmap(
  data,
  species = NULL,
  threshold = NULL,
  min_date = NULL,
  max_date = NULL,
  hour = NULL
)
}
\arguments{
\item{data}{A data frame containing BirdNET output. Relevant columns (e.g., \verb{common name},
\code{confidence}, \code{datetime}, \code{filepath}, etc.) are automatically detected by
\link{birdnet_detect_columns}. Must include columns like \code{common_name}, \code{confidence}, and \code{datetime}.}

\item{species}{Character scalar or vector specifying the common name(s) of species to visualize.
If \code{NULL}, no species filtering is applied.}

\item{threshold}{Either a numeric scalar between 0 and 1 (applied uniformly), or a data frame
with columns \code{common_name} and \code{threshold} for species-specific values. If \code{NULL}, no
threshold filtering is applied.}

\item{min_date}{Optional character scalar giving the earliest date to include (\code{"YYYY-MM-DD"} format).}

\item{max_date}{Optional character scalar giving the latest date to include (\code{"YYYY-MM-DD"} format).}

\item{hour}{Optional integer vector (0–23) specifying hours of the day to include in the heatmap.}
}
\value{
A \code{ggplot} object showing a heatmap of detections by date (x-axis) and hour (y-axis).
The fill color corresponds to detection counts.
}
\description{
Generates a heatmap visualizing the daily activity pattern of a specified species
detected in BirdNET output data. The heatmap shows detection counts by hour and date,
optionally filtered by species, confidence threshold, date range, and hours of the day.
}
\examples{
\dontrun{
birdnet_heatmap(
  data = birdnet_output,
  species = "Swainson's Thrush",
  threshold = 0.7,
  min_date = "2024-06-01",
  max_date = "2024-06-30",
  hour = 4:7
)
}
}
