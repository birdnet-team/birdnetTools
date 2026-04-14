# Clean and standardize column names

**DEPRECATED.** This function is no longer recommended for use. Consider
using
[birdnet_detect_columns](https://birdnet-team.github.io/birdnetTools/reference/birdnet_detect_columns.md)
instead for robust column detection and standardization.

Standardizes column names in a BirdNET-like data frame. This function
renames columns matching patterns like "start", "end", "scientific",
"common", "file", or "confidence" (case-insensitive) to consistent
names: `"start"`, `"end"`, `"scientific_name"`, `"common_name"`,
`"filepath"`, and `"confidence"`.

## Usage

``` r
birdnet_clean_names(data)
```

## Arguments

- data:

  A data frame containing BirdNET output or similar data with timestamp
  and species information.

## Value

A data frame with renamed, standardized column names.

## See also

[birdnet_detect_columns](https://birdnet-team.github.io/birdnetTools/reference/birdnet_detect_columns.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  Start.Time = 1:3,
  End.Time = 4:6,
  Scientific = c("Turdus migratorius", "Cyanocitta cristata", "Corvus brachyrhynchos"),
  Common = c("American Robin", "Blue Jay", "American Crow"),
  File.Name = c("file1.wav", "file2.wav", "file3.wav"),
  Confidence.Score = c(0.95, 0.87, 0.90)
)
birdnet_clean_names(df)
} # }
```
