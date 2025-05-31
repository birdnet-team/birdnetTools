

birdnet_filter <- function(data,
                           species = NULL,
                           threshold = NULL,
                           year = NULL,
                           min_date = NULL,
                           max_date = NULL,
                           hour = NULL) {


# argument check ----------------------------------------------------------



# sub functions -----------------------------------------------------------

  birdnet_filter_species <- function(data, species){
    data_clean_name <- data |>
      birdnet_clean_names()

    data_filtered <- data_clean_name |>
      dplyr::filter(common_name %in% species)

    return(data_filtered)
  }



  birdnet_filter_threshold <- function(data, threshold){

    # argument check need to confirm that
    # threshold is either a value or a dataframe

    data_clean_name <- data |>
      birdnet_clean_names()

    if(length(threshold) == 1) {
      data_filtered <- data_clean_name |>
        dplyr::filter(confidence >= threshold)

    } else {
      data_filtered <- data_clean_name |>
        dplyr::left_join(threshold, scientific_name) |>
        dplyr::group_by(scientific_name) |>
        dplyr::filter(confidence >= threshold)
    }

    return(data_filtered)
  }



  birdnet_drop_datetime <- function(data) {

    # argument check need to confirm that data
    # has the following columns, otherwise not needed

    data_without_datetime <- data |>
      dplyr::select(-datetime, -date, -year, -month,
                    -mday, -yday, -hour, -minute)

    return(data_without_datetime)
  }


  birdnet_filter_year <- function(data, year){
    data_with_time <- data |>
      birdnet_clean_names() |>
      birdnet_add_datetime()

    data_filtered <- data_with_time |>
      dplyr::filter(year %in% year)

    data_without_time <- data_filtered |>
      birdnet_drop_datetime()

    return(data_without_time)
  }




# main function -----------------------------------------------------------

  # what if there is no filter argument supplied?

  # by species (one species or multiple species)
  if(!is.null(species)){
    data_filtered <- data |>
      birdnet_filter_species(species = species)
  }


  # by threshold (single value or species-specific)
  if(!is.null(threshold)){
    data_filtered <- data |>
      birdnet_filter_threshold(threshold = threshold)
  }


  # by year

  if(!is.null(year)){
    data_filtered <- data |>
      birdnet_filter_year(year = year)
  }



  #by date range

  #by hour

  return(data_filtered)
}
