## code to prepare `effort_jprf_2023` dataset goes here


effort_jprf_2023 <- birdnet_get_effort("D:/Audio/2023_passerine") %>%
  # remove the "_1" suffix from site names, which indicates the recording session at each site
  mutate(site = str_replace(site, "_1$", "")) %>%
  # filter to the main breeding season (May 1 to August 31)
  filter(date >= "2023-05-01" & date <= "2023-08-31")


usethis::use_data(effort_jprf_2023, overwrite = TRUE)
