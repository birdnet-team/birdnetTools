# Add datetime-related columns from BirdNET output filenames

Extracts and parses datetime information from filenames in the file path
column (automatically detected), then adds several useful time-related
columns (e.g., `date`, `year`, `month`, `hour`) to the input data frame.
Several common formats are tried by default, and users can supply their
own with `formats` (see Details).

## Usage

``` r
birdnet_add_datetime(
  data,
  formats = c("%Y%m%dT%H%M%S", "%Y%m%d%H%M%S", "%Y%m%d_%H%M%S",
    "%Y%m%d_%H%M", "%d%m%Y_%H%M%S", "%m%d%Y_%H%M%S"),
  tz = "UTC",
  quiet = FALSE
)
```

## Arguments

- data:

  A data frame containing BirdNET output.

- formats:

  A character vector of datetime formats, using the same codes as
  [`strptime()`](https://rdrr.io/r/base/strptime.html) (see Details).
  Formats are tried in order, and the first one that successfully parses
  a file name is used. Defaults to common year-first formats, plus
  day-first and month-first variants. See Details for how ambiguous
  dates are handled.

- tz:

  A character string specifying the time zone to assign when parsing
  datetime. Defaults to `"UTC"`.

- quiet:

  Logical. If `TRUE`, suppresses the status messages about how many file
  names were parsed, failed, or were ambiguous. Warnings are still
  shown. Defaults to `FALSE`.

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

Rows whose file names can't be parsed get `NA` in all of these columns.
Existing columns with these names are overwritten, with a warning. If
all of these columns already exist, the data is returned unchanged.

## Details

**Defining your own formats.** If the default `formats` don't match your
file names, supply a format for each pattern in your data.

Supported codes:

- `%Y`: 4-digit year (1900-2099)

- `%y`: 2-digit year

- `%m`: 2-digit month

- `%d`: 2-digit day

- `%H`, `%M`, `%S`: 2-digit hour, minute, second

Any other characters (such as `_`, `-`, or `T`) are matched literally.
For example, `site1_2024-06-15_0530.wav` needs
`formats = "%Y-%m-%d_%H%M"`.

**Ambiguous dates.** The defaults include both day-first (`%d%m%Y`) and
month-first (`%m%d%Y`) formats. A file name such as `05062023_133500` is
valid as both, so the format listed first (day-first) is used and a
message lists the rows to check. If your data uses only one convention,
supply that format on its own to avoid the ambiguity.

## Examples

``` r
if (FALSE) { # \dontrun{
birdnet_add_datetime(example_jprf_2023) |>
glimpse()
} # }
```
