# Add site column from BirdNET output filenames

Extracts a specific directory level from the file path column
(automatically detected) to act as a site identifier, then adds a `site`
column to the input data frame.

## Usage

``` r
birdnet_add_site(data, i = -2)
```

## Arguments

- data:

  A data frame containing BirdNET output.

- i:

  An integer specifying the index of the path element to extract when
  split by slashes. Defaults to `-2`, which corresponds to the immediate
  parent directory of the file (e.g., extracting "Site-A" from
  "path/to/Site-A/audio.wav"). Negative values count from the right-hand
  side.

## Value

A data frame with an additional column:

- site:

  The extracted directory or folder name used as the site identifier.

## Details

The function uses
[birdnet_detect_columns](https://birdnet-team.github.io/birdnetTools/reference/birdnet_detect_columns.md)
to find the column containing file paths based on common name patterns.
By default, it looks at the immediate parent folder of the file.

## Examples

``` r
if (FALSE) { # \dontrun{
combined_data <- birdnet_combine("path/to/BirdNET/output")
data_with_site <- birdnet_add_site(combined_data, i = -2)
} # }
```
