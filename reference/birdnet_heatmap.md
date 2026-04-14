# Create a heatmap of BirdNET detections by hour and date

Generates a heatmap visualizing the daily activity pattern of a
specified species detected in BirdNET output data. The heatmap shows
detection counts by hour and date, optionally filtered by species,
confidence threshold, date range, and hours of the day.

## Usage

``` r
birdnet_heatmap(
  data,
  species = NULL,
  threshold = NULL,
  min_date = NULL,
  max_date = NULL,
  hour = NULL
)
```

## Arguments

- data:

  A data frame containing BirdNET output. Must include columns like
  `common_name`, `confidence`, and `filepath`.

- species:

  Character scalar or vector specifying the common name(s) of species to
  visualize. If `NULL`, no species filtering is applied.

- threshold:

  Either a numeric scalar between 0 and 1 (applied uniformly), or a data
  frame with columns `common_name` and `threshold` for species-specific
  values. If `NULL`, no threshold filtering is applied.

- min_date:

  Optional character scalar giving the earliest date to include
  (`"YYYY-MM-DD"` format).

- max_date:

  Optional character scalar giving the latest date to include
  (`"YYYY-MM-DD"` format).

- hour:

  Optional numeric (typically integer) vector of hours (0–23) to include
  in the heatmap.

## Value

A `ggplot` object showing a heatmap of detection counts by date (x-axis)
and hour (y-axis). Fill color corresponds to the number of detections
per date-hour combination.

## Examples

``` r
if (FALSE) { # \dontrun{
birdnet_heatmap(
  data = birdnet_output,
  species = "Swainson's Thrush",
  threshold = 0.7,
  min_date = "2024-06-01",
  max_date = "2024-06-30",
  hour = 4:7
)
} # }
```
