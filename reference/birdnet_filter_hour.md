# Filter BirdNET data by hour of day

Filters a BirdNET output data frame to include only rows with detections
occurring during specified hours of the day (0–23). If the `hour` column
is missing, it will be extracted from available datetime columns.

## Usage

``` r
birdnet_filter_hour(data, hour_arg)
```

## Arguments

- data:

  A data frame containing BirdNET output. If the `hour` column is not
  present, the function will attempt to extract it using
  [birdnet_add_datetime](https://birdnet-team.github.io/birdnetTools/reference/birdnet_add_datetime.md).

- hour_arg:

  Integer vector of hours (from 0 to 23) to retain.

## Value

A data frame filtered to include only detections within the specified
hours.

## See also

[birdnet_add_datetime](https://birdnet-team.github.io/birdnetTools/reference/birdnet_add_datetime.md),
[birdnet_drop_datetime](https://birdnet-team.github.io/birdnetTools/reference/birdnet_drop_datetime.md)
