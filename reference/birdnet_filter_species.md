# Filter BirdNET data by species

Filters a BirdNET output data frame for one or more specified species
using the column automatically detected as containing common species
names.

## Usage

``` r
birdnet_filter_species(data, species)
```

## Arguments

- data:

  A data frame containing BirdNET output.

- species:

  Character vector. One or more common names of species to keep.

## Value

A data frame filtered to include only the specified species.

## Details

This function uses
[birdnet_detect_columns](https://birdnet-team.github.io/birdnetTools/reference/birdnet_detect_columns.md)
to identify the appropriate column (e.g., "Common Name", "common_name",
etc.) containing common names of species, then filters the data frame to
retain only the rows matching the specified species.

## See also

[birdnet_detect_columns](https://birdnet-team.github.io/birdnetTools/reference/birdnet_detect_columns.md)
