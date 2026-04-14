# Filter BirdNET output data by species, confidence, date, and time

Applies one or more common filtering operations to a BirdNET output data
frame. Supports filtering by species name, confidence threshold
(universal or species-specific), year, date range, and hour of day.

## Usage

``` r
birdnet_filter(
  data,
  species = NULL,
  threshold = NULL,
  year = NULL,
  min_date = NULL,
  max_date = NULL,
  hour = NULL
)
```

## Arguments

- data:

  A data frame containing BirdNET output. Relevant columns (e.g.,
  `common name`, `confidence`, `datetime`) are automatically detected by
  [birdnet_detect_columns](https://birdnet-team.github.io/birdnetTools/reference/birdnet_detect_columns.md).

- species:

  Character vector. One or more common names of species to retain (e.g.,
  `c("Swainson's Thrush", "American Robin")`).

- threshold:

  Either a single numeric value between 0 and 1 (for a universal
  threshold), or a data frame with columns `common_name` and `threshold`
  for species-specific thresholds.

- year:

  Integer or vector of integers specifying year(s) to retain (e.g.,
  `2024:2025`).

- min_date, max_date:

  Optional. Character strings or `Date` objects specifying a date range
  in "YYYY-MM-DD" format. If only one is provided, filtering is
  open-ended on the other side.

- hour:

  Integer vector between 0 and 23 specifying hours of the day to retain
  (e.g., `4:7`).

## Value

A filtered data frame with an attribute `"filter_log"` containing the
applied filters.

## Details

This function uses
[birdnet_detect_columns](https://birdnet-team.github.io/birdnetTools/reference/birdnet_detect_columns.md)
to automatically identify the relevant columns (e.g., for species names,
confidence, datetime) based on common naming patterns.

All applied filter parameters are stored as an attribute called
`"filter_log"` attached to the returned data frame, which can be
accessed via `attr(x, "filter_log")`.

## Examples

``` r
if (FALSE) { # \dontrun{
filtered <- birdnet_filter(
  data = birdnet_combine("path/to/output"),
  species = "Swainson's Thrush",
  threshold = 0.75,
  year = 2025,
  min_date = "2025-06-01",
  hour = 4:8
)
attr(filtered, "filter_log")
} # }
```
