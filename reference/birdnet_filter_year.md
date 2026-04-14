# Filter BirdNET data by year

Filters a BirdNET output data frame based on one or more specified years
of detection. If the `year` column is missing, the function will extract
it from available datetime columns.

## Usage

``` r
birdnet_filter_year(data, year_arg)
```

## Arguments

- data:

  A data frame containing BirdNET output. If the `year` column is
  absent, the function will attempt to extract datetime information
  using
  [birdnet_add_datetime](https://birdnet-team.github.io/birdnetTools/reference/birdnet_add_datetime.md).

- year_arg:

  Integer vector. The year or years to retain in the filtered data.

## Value

A filtered data frame containing only rows with detections from the
specified years.

## See also

[birdnet_add_datetime](https://birdnet-team.github.io/birdnetTools/reference/birdnet_add_datetime.md),
[birdnet_drop_datetime](https://birdnet-team.github.io/birdnetTools/reference/birdnet_drop_datetime.md)
