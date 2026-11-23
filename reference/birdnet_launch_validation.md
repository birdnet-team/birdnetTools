# Launch the BirdNET audio validation app

Starts an interactive Shiny application for manually validating BirdNET
detections using audio clips and spectrograms. This tool is designed to
support manual review of BirdNET results.

## Usage

``` r
birdnet_launch_validation()
```

## Value

A `shiny.appobj` that runs the BirdNET audio validation platform in a
web browser.

## Details

This function launches a Shiny app for validating detections one by one.
The recommended workflow is:

1.  Subsample detections with
    [`birdnet_subsample()`](https://birdnet-team.github.io/birdnetTools/reference/birdnet_subsample.md),

2.  Cut audio clips with
    [`birdnet_extract_clips()`](https://birdnet-team.github.io/birdnetTools/reference/birdnet_extract_clips.md),

3.  Launch this app and select the folder that contains the clips and
    the table.

In the app you can view the spectrogram and play the audio of each
detection, and edit its validation. Changes are saved to the table in
the folder after every edit, so you can stop and resume at any time.

## See also

[birdnet_validation_ui](https://birdnet-team.github.io/birdnetTools/reference/birdnet_validation_ui.md),
[birdnet_validation_server](https://birdnet-team.github.io/birdnetTools/reference/birdnet_validation_server.md)

## Examples

``` r
if (FALSE) { # \dontrun{
birdnet_launch_validation()
} # }
```
