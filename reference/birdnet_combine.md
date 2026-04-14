# Combine BirdNET output files from a directory

Reads and combines multiple BirdNET output `.csv` or `.txt` files from a
specified directory (including subdirectories) into a single data frame.

## Usage

``` r
birdnet_combine(path, add_filepath = FALSE)
```

## Arguments

- path:

  Character string. Path to a directory containing BirdNET output files.

- add_filepath:

  Logical. If `TRUE`, adds a column `filepath` to the output data frame
  indicating the source file for each row. Use with caution: if the
  input files already contain a column named `filepath`, it will be
  overwritten. Default is `FALSE`.

## Value

A data frame combining all readable BirdNET `.csv` or `.txt` files found
in the directory. The column structure depends on the input files and is
not standardized by this function.

## Details

Files named `"analysis_params"` or `"CombinedTable"` are automatically
excluded. Files that are empty or cannot be read are skipped. A summary
of how many files were successfully read and which files (if any) caused
errors is printed at the end.

This function is useful for aggregating large batches of BirdNET results
without requiring consistent formatting. All subdirectories are searched
recursively. Files are read using
[`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html),
and empty files are silently ignored.

If no valid files are found, or if all files are excluded or cause read
errors, the function aborts with an informative message.

## See also

[`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html),
[`list.files()`](https://rdrr.io/r/base/list.files.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Combine all BirdNET output files in a directory and its subfolders
data <- birdnet_combine(path = "path/to/BirdNET/output", add_filepath = FALSE)

# View a few rows of the result
head(data)
} # }
```
