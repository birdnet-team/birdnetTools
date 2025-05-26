#' Generate a bird activity heatmap
#'
#' This function creates a heatmap of bird activity based on BirdNET detection data.
#'
#' @param data A data frame containing BirdNET output, including a file path column
#'   and a `common_name` column.
#' @param species A character string specifying the species name to filter by (from `common_name`).
#' @param type A character string indicating the type of heatmap to generate.
#'   Options are `"hour"` (default) for an hourly activity heatmap, or `"date"` for daily activity.
#'
#' @return A ggplot2 heatmap showing bird activity across time.
#' @export
#'
#' @examples
#' \dontrun{
#' birdnet_heatmap(data = birdnet_output, species = "Song Sparrow", type = "hour")
#' }
birdnet_heatmap <- function(data, species, type = "hour"){


# argument check ----------------------------------------------------------


# main function -----------------------------------------------------------

  # add datetime and filter species
  data_with_time <- data |>
    birdnet_clean_names() |>
    birdnet_add_datetime() |>
    dplyr::filter(common_name == species)


  # generate the heatmap based on the specified type
  plot <- switch(type,

         "hour" = data_with_time |>
           dplyr::group_by(date, hour) |>
           dplyr::summarise(activity = dplyr::n()) |>

           ggplot2::ggplot() +
           ggplot2::geom_tile(ggplot2::aes(y = hour, x = date, fill = activity)) +

           ggplot2::scale_fill_viridis_c(option = "A", direction = -1) +
           ggplot2::lims(y = c(0, 23)) +

           ggplot2::labs(title = species, x = "Date", y = "Hour") +
           ggplot2::theme(legend.position = "none",
                          axis.text = ggplot2::element_text(size = 12),
                          axis.title = ggplot2::element_text(
                            size = 14,
                            margin = ggplot2::margin(t = 15, r = 15))),

         "date" = data_with_time |>
           dplyr::group_by(date, hour) |>
           dplyr::summarise(activity = dplyr::n()) |>

           ggplot2::ggplot() +
           ggplot2::geom_tile(ggplot2::aes(y = hour, x = date, fill = activity)) +

           ggplot2::scale_fill_viridis_c(option = "A", direction = -1) +
           ggplot2::lims(y = c(0, 23)) +

           ggplot2::labs(title = species, x = "Date", y = "Hour") +
           ggplot2::theme(legend.position = "none",
                          axis.text = ggplot2::element_text(size = 12),
                          axis.title = ggplot2::element_text(
                            size = 14,
                            margin = ggplot2::margin(t = 15, r = 15)))
         )

  return(plot)
}

