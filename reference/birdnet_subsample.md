# Subsample BirdNET detections by species

Subsamples a specified number of observations from a BirdNET output
dataset. Supports four subsampling methods: proportional stratified by
confidence score, random, or top confidence scores. Optionally saves the
subsampled data to a CSV file.

## Usage

``` r
birdnet_subsample(
  data,
  n,
  method = c("even_stratified", "proportional_stratified", "random", "top"),
  save_to_file = FALSE,
  file = NULL
)
```

## Arguments

- data:

  A data frame containing BirdNET output.

- n:

  Integer. Number of observations to subsample.

- method:

  Character. Subsampling method to use. One of:

  "proportional_stratified"

  :   Samples proportionally across confidence score strata.

  "even_stratified"

  :   Samples evenly across confidence score strata.

  "random"

  :   Randomly samples `n` observations per species.

  "top"

  :   Selects the top `n` observations with the highest confidence per
      species.

  Defaults to `"proportional_stratified"`.

- save_to_file:

  Logical. If `TRUE`, saves the output to a CSV file. Defaults to
  `FALSE`. Automatically set to `TRUE` if `file` is provided.

- file:

  Character or `NULL`. File path to save the output CSV. If `NULL` and
  `save_to_file = TRUE`, saves as `"subsampled_data.csv"` in the working
  directory.

## Value

A data frame containing the subsampled observations.

## Examples

``` r
if (FALSE) { # \dontrun{
birdnet_subsample(data = my_data, n = 300, method = "proportional_stratified")
birdnet_subsample(data = my_data, n = 100, method = "top", save_to_file = TRUE,
file = "top_samples.csv")
} # }
```
