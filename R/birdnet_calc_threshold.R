



birdnet_calc_threshold <- function (validated_data,
                                    full_data = NULL,

                                    method = c("precision", "probability"),

                                    probability = 0.9,
                                    precision = 0.9) {


  # argument check ----------------------------------------------------------

  method <- match.arg(method)

  # validated data has to have at least a column called "validation",
  # and "Y" and "N" as values to indicate true positive or false positive of the detection

  # method has to be either one of the two options



  # helper function ---------------------------------------------------------

  # find precision given a threshold
  threshold2precision <- function(probability_data,
                                  threshold_target){
    probability_data %>%
      filter(confidence >= threshold_target) %>%
      pull(probability) %>%
      mean()
  }

  # function to determine the threshold given specified precision level
  precision2threshold <- function(threshold_table,
                                  precision_target){
    threshold_table %>%
      filter(precision >= precision_target) %>%
      slice_min(threshold, with_ties = FALSE) %>%
      pull(threshold)
  }


  # main function -----------------------------------------------------------

  # filter out data to keep only interested species and find the species names
  species_list <- validated_data |>
    dplyr::distinct(common_name)


  # start the for loop across the species
  thresholds <- numeric(length(species_list))
  names(thresholds) <- species_list

  for (species_ind in species_list) {

    # get the species model ready
    species_data <- validated_data |>
      filter(common_name == species_ind)

    species_model <- species_data |>
      glm(formula = validation ~ confidence,
          family = binomial)



    if (method == "precision") {

      # use the built model to covert confidence to probability
      species_data_full <- full_data |>
        filter(common_name == species_ind)

      species_probability <- species_data_full |>
        mutate(probability = predict(species_model,
                                     newdata = species_data_full,
                                     type = "response"))

      # create a table of threshlds with precision
      threshold_table <- tibble(threshold = seq(0, 1, 0.001)) |>
        mutate(precision = map_dbl(.x = threshold,
                                   .f =~ threshold2precision(species_probability, .x)))

      # find the threshold that gives the target precision
      t_target <- precision2threshold(threshold_table = threshold_table,
                                      precision_target = precision)



    } else if (method == "probability") {

      # use the built model to convert confidence to probability (using validation dataset)
      t_target <- (log(probability/(1 - probability)) - species_model$coefficients[1])/species_model$coefficients[2]

    }

    # assign the threshold to the species
    thresholds[species_ind] <- t_target
  }

  return(thresholds)
}
