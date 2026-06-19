#' Generate Detection History Matrix and Summary for Occupancy Modeling
#'
#' Summarizes BirdNET detection data across specified survey intervals (occasions)
#' and aligns them with operational effort data. Returns both a zero-filled
#' site-by-occasion binary matrix for packages like `spOccupancy` and `unmarked`,
#' and a detailed long-format data frame summary.
#'
#' @details
#' The function groups continuous temporal data into distinct survey blocks using
#' `lubridate::floor_date()`. Detections are cross-referenced against your
#' `effort_data`: occasions where monitoring effort occurred but no target
#' species were detected are explicitly zero-filled. If an ARU was not operational
#' during a specific time block, it is preserved as an `NA` value to ensure
#' structural integrity for missing-visit designs.
#'
#' Values greater than 0 in the final matrix are collapsed to `1` to format
#' the output for binary presence/absence occupancy models.
#'
#' @param data A data frame containing BirdNET detections, including column matches
#'   for filepaths and prediction confidence scores.
#' @param effort_data A data frame containing monitoring operational effort,
#'   requiring at least `site` and `date` columns to indicate the active
#'   monitoring windows and locations of each ARU device. Users can generate
#'   this via [birdnet_get_effort()], which derives effort data from a directory
#'   of audio files by defining a site-date combination as "active" if at least
#'   one recording exists. If an `n_files` column is present, file counts will
#'   be aggregated per survey occasion block.
#' @param survey_interval A character string specifying the temporal unit for
#'   grouping survey occasions (e.g., `"1 day"`, `"1 week"`, `"7 days"`).
#'   Passed directly to \code{\link[lubridate:floor_date]{lubridate::floor_date()}}.
#' @param i An integer specifying the path hierarchy index for extracting site IDs.
#'   Passed directly to \code{\link{birdnet_add_site}}. Defaults to `-2`.
#'
#' @return A named `list` containing two components:
#' \describe{
#'   \item{detection_history}{A numeric base R `matrix` where rows represent
#'     unique sites (assigned as row names), columns represent chronological temporal
#'     occasions, and cells indicate binary occupancy integers (`1`, `0`,
#'     or `NA` for missing effort).}
#'   \item{detection_summary}{A data frame in long format containing the underlying
#'     aggregated metrics per site/occasion, including detection counts (`n_detections`),
#'     maximum verification confidence (`max_conf`), and the file path of the
#'     highest confidence detection (`max_conf_audio`).}
#' }
#'
#' @importFrom dplyr .data
#' @export
birdnet_detection_history <- function(data,
                                      effort_data,
                                      survey_interval,
                                      i = -2) {


  # argument check ----------------------------------------------------------


  # main function -----------------------------------------------------------

  cols <- birdnet_detect_columns(data)

  # 1. Summarize detections by site and occasion block
  detections_summarized <- data |>
    birdnet_add_site(i = i) |>
    birdnet_add_datetime() |>
    dplyr::mutate(occasion = lubridate::floor_date(x = .data$date,
                                                   unit = survey_interval)) |>
    dplyr::group_by(.data$site,
                    .data$occasion) |>
    dplyr::summarise(n_detections = dplyr::n(),
                     max_conf = max(.data[[cols$confidence]], na.rm = TRUE),
                     max_conf_audio = .data[[cols$filepath]][which.max(.data[[cols$confidence]])],
                     .groups = "drop")



  # 2. Process effort
  baseline_effort <- effort_data |>
    dplyr::mutate(occasion = lubridate::floor_date(x = .data$date,
                                                   unit = survey_interval))
  if ("n_files" %in% names(baseline_effort)) {
    # if n_files exists, aggregate the total file counts per site/occasion
    baseline_effort <- baseline_effort |>
      dplyr::group_by(.data$site, .data$occasion) |>
      dplyr::summarise(n_files = sum(.data$n_files, na.rm = TRUE), .groups = "drop")
  } else {
    # if n_files is missing, simply keep unique combinations of site and occasion
    baseline_effort <- baseline_effort |>
      dplyr::distinct(.data$site, .data$occasion)
  }



  # 3. join detections, fill zeros, and pivot wide
  detections_zero_filled <- baseline_effort |>
    # left join ensures we only evaluate occasions where the devices were running
    dplyr::left_join(detections_summarized, by = c("site", "occasion")) |>

    # differentiate true zeros from missing effort
    dplyr::mutate(n_detections = tidyr::replace_na(.data$n_detections, 0),
                  max_conf = tidyr::replace_na(.data$max_conf, 0),
                  max_conf_audio = tidyr::replace_na(.data$max_conf_audio, "none"))



  # 4. Creating matrix structure: pivot to wide format with sites as rows and occasions as columns

    # isolate matrix structure and shape wide for modeling packages (e.g., unmarked and spOccupancy)
  detection_history_df <- detections_zero_filled |>
    dplyr::select("site", "occasion", "n_detections") |>
    # manipulate n_detections column to make it 1 if it's larger than 0,
    # otherwise 0 (for occupancy modeling)
    dplyr::mutate(n_detections = ifelse(.data$n_detections > 0, 1, 0)) |>
    dplyr::arrange(.data$occasion, .data$site) |>
    tidyr::pivot_wider(id_cols = "site",
                       names_from = "occasion",
                       values_from = "n_detections",
                       values_fill = NA)

  detection_history <- as.matrix(detection_history_df[, -1])
  rownames(detection_history) <- detection_history_df$site



  # 5. Create the effort matrix for detection probability purpose
  baseline_effort <- baseline_effort |>
    dplyr::arrange(.data$occasion, .data$site)

  if ("n_files" %in% names(baseline_effort)) {
    # if n_files exists, we can use the file counts as a measure of effort
    wide_effort <- baseline_effort |>
      tidyr::pivot_wider(id_cols = "site",
                         names_from = "occasion",
                         values_from = "n_files",
                         values_fill = 0)
  } else {
    # if n_files is missing, we can only indicate presence (1) or absence (0) of effort
    wide_effort <- baseline_effort |>
      tidyr::pivot_wider(id_cols = "site",
                         names_from = "occasion",
                         values_from = "occasion",
                         values_fn = \(x) 1,
                         values_fill = 0)
  }

  # Strip the site column to create a clean matrix, keeping site names as rownames
  effort_matrix <- as.matrix(wide_effort[, -1])
  rownames(effort_matrix) <- wide_effort$site


  return(list("detection_history" = detection_history,
              "effort_matrix" = effort_matrix,
              "detection_summary" = detections_zero_filled))
}

