# Filter BirdNET data by confidence threshold

Filters a BirdNET output data frame based on a confidence threshold,
using either a universal numeric value or species-specific thresholds.

## Usage

``` r
birdnet_filter_threshold(data, threshold_arg)
```

## Arguments

- data:

  A data frame containing BirdNET output.

- threshold_arg:

  Either:

  - A single numeric value specifying a universal confidence threshold,
    or

  - A data frame with columns `common_name` and `threshold`, specifying
    species-specific thresholds.

## Value

A data frame filtered to include only rows where confidence scores meet
the threshold criteria.

## Details

The function uses
[birdnet_detect_columns](https://birdnet-team.github.io/birdnetTools/reference/birdnet_detect_columns.md)
to automatically detect column names (e.g., "Confidence", "Scientific
Name") based on common patterns.

## See also

[birdnet_detect_columns](https://birdnet-team.github.io/birdnetTools/reference/birdnet_detect_columns.md)
