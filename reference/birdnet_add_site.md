# Add site column from BirdNET output filenames

Extracts a specific directory level from the file path column
(automatically detected) to act as a site identifier, then adds a `site`
column to the input data frame. By default the whole directory name is
used, but `formats` lets you keep only part of it, such as the letters
or the digits (see Details).

## Usage

``` r
birdnet_add_site(data, i = -2, formats = ".*", quiet = FALSE)
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

- formats:

  A character vector of regular expressions (as used by
  [`stringr::str_extract()`](https://stringr.tidyverse.org/reference/str_extract.html))
  that describe which part of the extracted directory name to keep as
  the site. Expressions are tried in order, and the first one that
  matches is used. Defaults to `".*"`, which keeps the whole directory
  name. See Details.

- quiet:

  Logical. If `TRUE`, suppresses the status messages about how many file
  names were successfully matched to a site or could not be matched.
  Defaults to `FALSE`

## Value

A data frame with an additional column:

- site:

  The extracted directory or folder name (or the part of it matched by
  `formats`) used as the site identifier.

## Details

**Keeping only part of the name.** If your folder names combine several
pieces of information, supply a regular expression that matches only the
part you want. For a folder named `Pine05`:

- `".*"` (default): `"Pine05"` (the whole name)

- `"^[A-Za-z]+"`: `"Pine"` (the letters at the start only)

- `"[0-9]+"`: `"05"` (the digits only)

## Examples

``` r
if (FALSE) { # \dontrun{
combined_data <- birdnet_combine("path/to/BirdNET/output")
data_with_site <- birdnet_add_site(combined_data, i = -2)

# keep only the letters of the folder name (e.g. "Pine05" becomes "Pine")
birdnet_add_site(combined_data, formats = "^[A-Za-z]+")

# keep only the digits (e.g. "Pine05" becomes "05")
birdnet_add_site(combined_data, formats = "[0-9]+")
} # }
```
