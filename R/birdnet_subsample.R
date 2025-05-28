

birdnet_subsample <- function(data,
                              method = c("stratified", "random", "weighted"),
                              save_to_file = FALSE) {


# argument check ----------------------------------------------------------

  checkmate::assert_data_frame(data)
  checkmate::assert_choice(method, choices = c("hour", "date", "month"))
  checkmate::assert_flag(save_to_file)


# main function -----------------------------------------------------------





}
