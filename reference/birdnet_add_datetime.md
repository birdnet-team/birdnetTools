# Add datetime-related columns from BirdNET output filenames

Extracts and parses datetime information from filenames in the file path
column (automatically detected), then adds several useful time-related
columns (e.g., `date`, `year`, `month`, `hour`) to the input data frame.

## Usage

``` r
birdnet_add_datetime(data, tz = "UTC")
```

## Arguments

- data:

  A data frame containing BirdNET output.

- tz:

  A character string specifying the time zone to assign when parsing
  datetime. Defaults to `"UTC"`.

## Value

A data frame with additional columns:

- datetime:

  POSIXct datetime parsed from the filename.

- date:

  Date portion of the datetime.

- year:

  Year of detection.

- month:

  Month of detection.

- mday:

  Day of the month.

- yday:

  Day of the year.

- hour:

  Hour of the day.

- minute:

  Minute of the hour.

## Details

The function uses
[birdnet_detect_columns](https://birdnet-team.github.io/birdnetTools/reference/birdnet_detect_columns.md)
to find the column containing file paths based on common name patterns
(e.g., "file", "path"). Filenames are expected to contain a datetime
string in the format `"YYYYMMDD_HHMMSS"` or similar.

## Examples

``` r
if (FALSE) { # \dontrun{
combined_data <- birdnet_combine("path/to/BirdNET/output")
data_with_time <- birdnet_add_datetime(combined_data)
} # }
```
