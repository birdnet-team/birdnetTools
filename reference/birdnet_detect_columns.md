# Detect standard BirdNET column names in a data frame

Helper function to identify the most likely columns representing key
BirdNET output variables such as start time, end time, scientific name,
common name, confidence, and filepath.

## Usage

``` r
birdnet_detect_columns(data)
```

## Arguments

- data:

  A data frame containing BirdNET output data.

## Value

A named list with elements:

- start:

  Column name for start time (or NA).

- end:

  Column name for end time (or NA).

- scientific_name:

  Column name for scientific name (or NA).

- common_name:

  Column name for common name (or NA).

- confidence:

  Column name for confidence score (or NA).

- filepath:

  Column name for file path or file name (or NA).

## Details

This function attempts to match user-supplied data frame column names to
expected BirdNET output columns by pattern matching. It returns a named
list with the detected column names or `NA` if no match is found.
