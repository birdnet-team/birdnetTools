% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/birdnet_launch_validation.R
\name{birdnet_validation_server}
\alias{birdnet_validation_server}
\title{Server logic for BirdNET audio validation platform}
\usage{
birdnet_validation_server(input, output, session)
}
\arguments{
\item{input}{Shiny input object.}

\item{output}{Shiny output object.}

\item{session}{Shiny session object.}
}
\value{
Called for its side effects within a \code{shinyApp} context.
}
\description{
Defines the server-side logic for the BirdNET audio validation Shiny app within the
\code{birdnetTools} package. This function handles file imports, audio directory
selection, spectrogram generation, interactive table editing, and audio playback.
}
\details{
This internal server function powers the interactive audio validation
interface. Users can import a CSV of BirdNET detections, link to local audio
directories, view spectrograms, and manually validate detections through a
Shiny DataTable interface. Audio playback and validation column editing are supported.
}
\seealso{
\link{birdnet_validation_ui}, \link{birdnet_launch_validation}
}
\keyword{internal}
