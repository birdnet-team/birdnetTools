

birdnet_subsample <- function(data,
                              species,
                              n = 300,
                              method = c("stratified", "random", "top"),
                              save_to_file = FALSE) {


# argument check ----------------------------------------------------------

  checkmate::assert_data_frame(data)
  checkmate::assert_choice(method, choices = c("hour", "date", "month"))
  checkmate::assert_flag(save_to_file)


# main function -----------------------------------------------------------

  data_species <- data |>
    dplyr::filter(common_name == species)


  if (method == "stratified") {
    data_subsampled <- data_species |>
      dplyr::mutate(category = cut(confidence,
                                   breaks = seq(0.1, 1, by = 0.05),
                                   right = FALSE)) |>
      dplyr::slice_sample(n = round(n / 18), by = category) %>%
      dplyr::select(-category)

  } else if (method == "random") {
    data_subsampled <- data_species |>
      dplyr::slice_sample(n = n, replace = FALSE)


  } else if (method == "top") {
    data_subsampled <- data_species |>
      dplyr::slice_max(confidence, n = n, replace = FALSE)
  }


  if (save_to_file) {
    write.csv(data, "subsampled_data.csv", row.names = FALSE)
  }



  return(data_subsampled)


}
