## code to prepare `example_jprf_2023` dataset goes here

library(tidyverse)
library(here)

example_jprf_2023 <- read_csv(here("data-raw", "example_jprf_2023.csv"))

usethis::use_data(example_jprf_2023, overwrite = TRUE)
