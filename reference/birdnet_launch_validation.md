# Launch the BirdNET audio validation app

Starts an interactive Shiny application for manually validating BirdNET
detections using audio clips and spectrograms.This tool is designed to
support manual review of BirdNET results.

## Usage

``` r
birdnet_launch_validation()
```

## Value

A `shiny.appobj` that runs the BirdNET audio validation platform in a
web browser.

## Details

This function launches a user-friendly Shiny app that allows you to:

- Import a CSV file of BirdNET detections (selection table format),

- Select the directory containing your audio files,

- View and validate each detection using spectrograms and audio
  playback,

- Edit validation results interactively and save the updated table.

Audio playback and spectrogram generation now support multiple audio
formats (e.g., WAV, MP3, FLAC, OGG) through the av package. Internally,
all files are converted to temporary WAV files with
[`av::av_audio_convert()`](https://docs.ropensci.org/av//reference/encoding.html)
before being read by
[`tuneR::readWave()`](https://rdrr.io/pkg/tuneR/man/readWave.html),
ensuring compatibility without quality loss. File names are displayed
without their extensions using base R functions.

## See also

[birdnet_validation_ui](https://birdnet-team.github.io/birdnetTools/reference/birdnet_validation_ui.md),
[birdnet_validation_server](https://birdnet-team.github.io/birdnetTools/reference/birdnet_validation_server.md)

## Examples

``` r
if (FALSE) { # \dontrun{
birdnet_launch_validation()
} # }
```
