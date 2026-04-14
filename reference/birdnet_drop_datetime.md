# Drop datetime-related columns from BirdNET output

Removes datetime-related columns (e.g., `datetime`, `date`, `year`,
`hour`, etc.) that may have been added using
[birdnet_add_datetime](https://birdnet-team.github.io/birdnetTools/reference/birdnet_add_datetime.md).

## Usage

``` r
birdnet_drop_datetime(data)
```

## Arguments

- data:

  A data frame containing BirdNET output, with datetime-related columns
  present.

## Value

A data frame with datetime-related columns removed, if present.
