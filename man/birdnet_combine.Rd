% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/birdnet_combine.R
\name{birdnet_combine}
\alias{birdnet_combine}
\title{Group BirdNET output files}
\usage{
birdnet_combine(path)
}
\arguments{
\item{path}{Character string. The directory path containing BirdNET output \code{.csv} files. Can include subdirectories.}
}
\value{
A data frame containing the combined BirdNET detection data from all valid CSV files, with columns:
\itemize{
\item \code{Start (s)}: Detection start time in seconds.
\item \code{End (s)}: Detection end time in seconds.
\item \code{Scientific name}: Scientific name of the detected species.
\item \code{Common name}: Common name of the detected species.
\item \code{Confidence}: Confidence score of the detection.
\item \code{File}: Name of the file where the detection was found.
}
}
\description{
Reads and combines multiple BirdNET output CSV files from a specified directory into a single data frame.
Files named with "analysis_params" or "CombinedTable" under the specified directory are automatically excluded.
}
\details{
This function is useful for aggregating BirdNET output from batch runs or large-scale deployments.
It uses a fixed column specification to ensure consistent parsing of each file. Files with incompatible
formats or errors are skipped and their names are printed with a warning.
}
\examples{
\dontrun{
combined_data <- group_BirdNET_output("path/to/BirdNET/output")
}

}
