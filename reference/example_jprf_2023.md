# Example BirdNET selection table from John Prince Research Forest

A sample BirdNET selection table generated from audio recordings
collected from 5 sites, in John Prince Research Forest, British
Columbia, Canada, during May–June 2023. Recordings were scheduled
continuously over 24 hours using a duty cycle of 1 minute on, 4 minutes
off. Audio files were analyzed using the BirdNET GUI (model version 2.4)
with a confidence threshold of 0.1; all other parameters were kept at
their default values.

## Usage

``` r
example_jprf_2023
```

## Format

### `example_jprf_2023`

A data frame with 392,300 rows and 14 columns. Key columns include:

- filepath:

  Full path to the audio file from which the detection was made

- start:

  Start time of the detection in seconds

- end:

  End time of the detection in seconds

- scientific_name:

  Scientific name of the detected species or sound event

- common_name:

  Common name of the detected species or sound event

- confidence:

  BirdNET-assigned confidence score (0 to 1)

- lat:

  Latitude of the recording location (set to -1 if not embedded)

- lon:

  Longitude of the recording location (set to -1 if not embedded)

- week:

  Week number of the year (set to -1 if not available)

- overlap:

  Whether overlapping segments were analyzed (0 = no, 1 = yes)

- sensitivity:

  Sensitivity parameter used during detection (default = 1)

- min_conf:

  Minimum confidence threshold used during detection (e.g., 0.1)

- species_list:

  Species list constraint used (e.g., "None" if not set)

- model:

  Name of the BirdNET model used for detection

## Source

<https://sunnytseng.ca/>

## Details

This dataset is useful for demonstrating typical BirdNET output and
testing workflows involving species filtering, thresholding, and
temporal subsetting.
