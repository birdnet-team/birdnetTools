## code to prepare `example_jprf_2023` dataset goes here

library(tidyverse)
library(here)
library(birdnetTools)

example_jprf_2023 <- read_csv(here("data-raw", "example_jprf_2023.csv")) %>%
  birdnet_filter(species = c("Swainson's Thrush",
                             "American Robin",
                             "Pacific-slope Flycatcher",
                             "Pacific Wren",
                             "Varied Thrush",
                             "American Crow",
                             "Yellow-rumped Warbler",
                             "White-throated Sparrow",
                             "Olive-sided Flycatcher",
                             "Wilson's Warbler",
                             "Orange-crowned Warbler",
                             "Red-breasted Nuthatch"))

usethis::use_data(example_jprf_2023, overwrite = TRUE)
