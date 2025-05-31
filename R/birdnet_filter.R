birdnet_filter <- function(data,
                           species = NULL,
                           threshold = NULL,
                           year = NULL,
                           min_date = NULL,
                           max_date = NULL,
                           hour = NULL) {

  # Clean column names first (assumed all filters rely on this)
  data <- birdnet_clean_names(data)

  # Start with the full dataset
  data_filtered <- data

  # filter by species
  if (!is.null(species)) {
    data_filtered <- birdnet_filter_species(data_filtered,
                                            species)
  }

  # filter by threshold
  if (!is.null(threshold)) {
    data_filtered <- birdnet_filter_threshold(data_filtered,
                                              threshold)
  }

  # filter by year
  if (!is.null(year)) {
    data_filtered <- birdnet_filter_year(data_filtered,
                                         year)
  }

  # filter by date range
  if (!is.null(min_date) || !is.null(max_date)) {
    data_filtered <- birdnet_filter_date_range(data_filtered,
                                               min_date = min_date,
                                               max_date = max_date)
  }

  # filter by hour
  if (!is.null(hour)) {
    data_filtered <- birdnet_filter_hour(data_filtered,
                                         hour)
  }

  return(data_filtered)
}
