# Generate Detection History Matrix, Effort Matrix, and Summary for Occupancy Modeling

Summarizes BirdNET detection data across specified survey intervals
(occasions), filters sites based on minimal detection persistence
thresholds, and aligns them with operational effort data. Returns a
zero-filled site-by-occasion binary matrix, an identical matching matrix
documenting sampling effort intensity for modeling detection probability
covariates, and a detailed long-format data frame summary.

## Usage

``` r
birdnet_detection_history(
  data,
  effort_data,
  survey_interval,
  i = -2,
  min_unique_days = 1
)
```

## Arguments

- data:

  A data frame containing BirdNET detections, including column matches
  for filepaths and prediction confidence scores.

- effort_data:

  A data frame containing monitoring operational effort, requiring at
  least `site` and `date` columns to indicate the active monitoring
  windows and locations of each ARU device. Users can generate this via
  [`birdnet_get_effort()`](https://birdnet-team.github.io/birdnetTools/reference/birdnet_get_effort.md),
  which derives effort data from a directory of audio files by defining
  a site-date combination as "active" if at least one recording exists.
  If an `n_files` column is present, file counts will be aggregated per
  survey occasion block.

- survey_interval:

  A character string specifying the temporal unit for grouping survey
  occasions (e.g., `"1 day"`, `"1 week"`, `"7 days"`). Passed directly
  to
  [`lubridate::floor_date()`](https://lubridate.tidyverse.org/reference/round_date.html).

- i:

  An integer specifying the path hierarchy index for extracting site
  IDs. Passed directly to
  [`birdnet_add_site`](https://birdnet-team.github.io/birdnetTools/reference/birdnet_add_site.md).
  Defaults to `-2`.

- min_unique_days:

  An integer specifying the threshold of unique calendar days a site
  must possess raw detections on to be kept. Sites with detections
  spanning fewer than `min_unique_days` are dropped early from
  compilation. Defaults to `1`.

## Value

A named `list` containing three components:

- detection_history:

  A numeric base R `matrix` where rows represent unique sites (assigned
  as row names), columns represent chronological temporal occasions, and
  cells indicate binary occupancy integers (`1`, `0`, or `NA` for
  missing effort).

- effort_matrix:

  A numeric base R `matrix` matching the exact dimensions and sorting
  order of `detection_history`. If `n_files` was present in the effort
  data, cells represent total file counts per site-occasion. Otherwise,
  cells contain binary integers indicating presence (`1`) or absence
  (`0`) of operational effort.

- detection_summary:

  A data frame in long format containing the underlying aggregated
  metrics per site/occasion, including detection counts
  (`n_detections`), maximum verification confidence (`max_conf`), and
  the file path of the highest confidence detection (`max_conf_audio`).

## Details

The function groups continuous temporal data into distinct survey blocks
using
[`lubridate::floor_date()`](https://lubridate.tidyverse.org/reference/round_date.html).
Detections are cross-referenced against your `effort_data`: occasions
where monitoring effort occurred but no target species were detected are
explicitly zero-filled. If an ARU was not operational during a specific
time block, it is preserved as an `NA` value in the detection history to
ensure structural integrity for missing-visit designs.

Values greater than 0 in the final detection matrix are collapsed to `1`
to format the output for binary presence/absence occupancy models (e.g.,
`spOccupancy`, `unmarked`).
