# Filter BirdNET data by date range

Filters a BirdNET output data frame to include only rows with detections
within a specified date range. If the `date` column is missing, it will
be extracted from available datetime columns.

## Usage

``` r
birdnet_filter_date_range(data, min_date = NULL, max_date = NULL)
```

## Arguments

- data:

  A data frame containing BirdNET output. If the `date` column is not
  present, the function will attempt to extract it using
  `birdnet_add_datetime`.

- min_date:

  Date or character string coercible to Date. The earliest date to
  include (inclusive).

- max_date:

  Date or character string coercible to Date. The latest date to include
  (inclusive).

## Value

A data frame filtered to include only rows within the specified date
range.

## See also

`birdnet_add_datetime`, `birdnet_drop_datetime`
