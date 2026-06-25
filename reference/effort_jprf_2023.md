# Example monitoring effort table from John Prince Research Forest

A sample operational effort table mapping the active recording history
of Autonomous Recording Units (ARUs) deployed across 5 sites in John
Prince Research Forest, British Columbia, Canada, during May–June 2023.
This data documents the baseline monitoring effort, where a given
location and date combination is associated with an active ARU device if
one or more audio files were successfully recorded.

## Usage

``` r
effort_jprf_2023
```

## Format

### `effort_jprf_2023`

A data frame with rows and columns detailing active recording days. Key
columns include:

- site:

  Character string indicating the unique identifier for the ARU
  deployment location

- date:

  Date object representing the calendar day of monitoring effort

- n_files:

  Integer representing the total number of audio files recorded at that
  location on that day

## Source

<https://sunnytseng.ca/>

## Details

This dataset acts as the operational counterpart to `example_jprf_2023`
and is useful for demonstrating workflow alignment between species
detections and true field effort, specifically for zero-filling
non-detections in occupancy modeling.

This dataset was generated directly using the
[`birdnet_get_effort()`](https://birdnet-team.github.io/birdnetTools/reference/birdnet_get_effort.md)
function. For more details on the generation parameters, data
constraints, and internal file processing pipelines, please refer to the
function documentation.
