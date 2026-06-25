#' Example BirdNET selection table from John Prince Research Forest
#'
#' A sample BirdNET selection table generated from audio recordings collected
#' from 5 sites, in John Prince Research Forest, British Columbia, Canada, during
#' May–June 2023. Recordings were scheduled continuously over 24 hours using a duty
#' cycle of 1 minute on, 4 minutes off. Audio files were analyzed using the BirdNET GUI
#' (model version 2.4) with a confidence threshold of 0.1; all other parameters were
#' kept at their default values.
#'
#' This dataset is useful for demonstrating typical BirdNET output and testing
#' workflows involving species filtering, thresholding, and temporal subsetting.
#'
#' @format ## `example_jprf_2023`
#' A data frame with 392,300 rows and 14 columns. Key columns include:
#' \describe{
#'   \item{filepath}{Full path to the audio file from which the detection was made}
#'   \item{start}{Start time of the detection in seconds}
#'   \item{end}{End time of the detection in seconds}
#'   \item{scientific_name}{Scientific name of the detected species or sound event}
#'   \item{common_name}{Common name of the detected species or sound event}
#'   \item{confidence}{BirdNET-assigned confidence score (0 to 1)}
#'   \item{lat}{Latitude of the recording location (set to -1 if not embedded)}
#'   \item{lon}{Longitude of the recording location (set to -1 if not embedded)}
#'   \item{week}{Week number of the year (set to -1 if not available)}
#'   \item{overlap}{Whether overlapping segments were analyzed (0 = no, 1 = yes)}
#'   \item{sensitivity}{Sensitivity parameter used during detection (default = 1)}
#'   \item{min_conf}{Minimum confidence threshold used during detection (e.g., 0.1)}
#'   \item{species_list}{Species list constraint used (e.g., "None" if not set)}
#'   \item{model}{Name of the BirdNET model used for detection}
#' }
#'
#' @source <https://sunnytseng.ca/>
"example_jprf_2023"



#' Example monitoring effort table from John Prince Research Forest
#'
#' A sample operational effort table mapping the active recording history of
#' Autonomous Recording Units (ARUs) deployed across 5 sites in John Prince
#' Research Forest, British Columbia, Canada, during May–June 2023. This data
#' documents the baseline monitoring effort, where a given location and date
#' combination is associated with an active ARU device if one or more audio files
#' were successfully recorded.
#'
#' This dataset acts as the operational counterpart to `example_jprf_2023` and is
#' useful for demonstrating workflow alignment between species detections and true
#' field effort, specifically for zero-filling non-detections in occupancy modeling.
#'
#' @details
#' This dataset was generated directly using the [birdnet_get_effort()] function.
#' For more details on the generation parameters, data constraints, and internal
#' file processing pipelines, please refer to the function documentation.
#'
#' @format ## `effort_jprf_2023`
#' A data frame with rows and columns detailing active recording days. Key columns include:
#' \describe{
#'   \item{site}{Character string indicating the unique identifier for the ARU deployment location}
#'   \item{date}{Date object representing the calendar day of monitoring effort}
#'   \item{n_files}{Integer representing the total number of audio files recorded at that location on that day}
#' }
#'
#' @source <https://sunnytseng.ca/>
"effort_jprf_2023"

