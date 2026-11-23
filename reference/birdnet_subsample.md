# Subsample BirdNET detections by species

Draws a subsample of `n` detections from a BirdNET output data frame,
for example for manual validation. Sampling is done across all rows of
`data`. To subsample one species, filter to that species first (see
Examples). Four methods are available: even or proportional stratified
sampling across confidence score bins, simple random sampling, or the
highest-confidence detections. Optionally saves the subsample to a CSV
file.

## Usage

``` r
birdnet_subsample(
  data,
  n,
  method = c("even_stratified", "proportional_stratified", "random", "top"),
  bin_width = 0.05,
  seed = NULL,
  save_to_file = FALSE,
  file = NULL,
  quiet = FALSE
)
```

## Arguments

- data:

  A data frame containing BirdNET output.

- n:

  Integer. Total number of detections to subsample. If `data` has `n` or
  fewer rows, all rows are returned.

- method:

  Character. Subsampling method to use. One of:

  "even_stratified"

  :   Samples evenly across confidence score bins. Bins with too few
      detections contribute all they have, and the remainder is shared
      among the other bins.

  "proportional_stratified"

  :   Samples across confidence score bins in proportion to the number
      of detections in each bin.

  "random"

  :   Simple random sample of `n` detections.

  "top"

  :   The `n` detections with the highest confidence.

  Defaults to `"even_stratified"`.

- bin_width:

  Numeric. Width of the confidence score bins used by the stratified
  methods. Defaults to `0.05`.

- seed:

  Integer or `NULL`. If given, the subsample is reproducible. The random
  number generator state of your session is left unchanged.

- save_to_file:

  Logical. If `TRUE`, saves the output to a CSV file. Defaults to
  `FALSE`. Automatically set to `TRUE` if `file` is provided.

- file:

  Character or `NULL`. File path to save the output CSV. If `NULL` and
  `save_to_file = TRUE`, saves as `"subsampled_data.csv"` in the working
  directory.

- quiet:

  Logical. If `TRUE`, suppresses the status messages about how many
  detections were returned and whether all of them were kept because
  `data` had no more than `n` rows. Defaults to `FALSE`.

  \#' @details **Choosing a method.**

  - `"random"` gives an unbiased sample, suited to estimating overall
    precision.

  - `"even_stratified"` gives balanced coverage across confidence
    scores, suited to relating precision to score or choosing a
    threshold.

  - `"proportional_stratified"` mirrors the confidence distribution of
    the detections, so it is dominated by low-confidence detections.

  - `"top"` is useful for confirming that a species is present, but it
    is biased for estimating precision.

  **Several species.** Sampling is not done per species. To subsample
  each species separately, apply the function to each species in turn
  (see Examples).

## Value

A data frame containing the subsampled observations.

## Examples

``` r
if (FALSE) { # \dontrun{
# one species: filter first, then subsample
my_data |>
  dplyr::filter(common_name == "Barred Owl") |>
  birdnet_subsample(n = 100, method = "even_stratified", seed = 1)

# several species: apply the function to each one
my_data |>
  split(my_data$common_name) |>
  purrr::map(birdnet_subsample, n = 100) |>
  dplyr::bind_rows()
} # }
```
