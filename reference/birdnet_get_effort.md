# Calculate recording effort by site and date

Scans a directory for all audio files, extracts site and datetime
metadata from their paths/filenames, and returns a unique timeline of
recording effort with file counts.

## Usage

``` r
birdnet_get_effort(path, i = -2)
```

## Arguments

- path:

  A character string specifying the path to the directory containing the
  audio files.

- i:

  An integer specifying the index of the path element to extract as the
  site identifier when split by slashes. Defaults to `-2`, which
  corresponds to the immediate parent directory of the file, passed
  directly to
  [`birdnet_add_site()`](https://birdnet-team.github.io/birdnetTools/reference/birdnet_add_site.md).
  Negative values count from the right-hand side.

## Value

A tibble (data frame) with three columns:

- site:

  The extracted site identifier.

- date:

  The date on which recording effort occurred.

- n_files:

  Integer. The total number of audio files recorded at that site on that
  specific date.

## Details

The function identifies audio files matching common extensions,
automatically detects site names via
[`birdnet_add_site()`](https://birdnet-team.github.io/birdnetTools/reference/birdnet_add_site.md),
parses dates via
[`birdnet_add_datetime()`](https://birdnet-team.github.io/birdnetTools/reference/birdnet_add_datetime.md),
and reduces the output to a unique combination of sites and dates,
summarizing total files recorded.

## Examples

``` r
if (FALSE) { # \dontrun{
effort_df <- birdnet_get_effort("path/to/audio/storage", i = -2)
head(effort_df)
} # }
```
