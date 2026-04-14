# Server logic for BirdNET audio validation platform

Defines the server-side logic for the BirdNET audio validation Shiny app
within the `birdnetTools` package. This function handles file imports,
audio directory selection, spectrogram generation, interactive table
editing, and audio playback.

## Usage

``` r
birdnet_validation_server(input, output, session)
```

## Arguments

- input:

  Shiny input object.

- output:

  Shiny output object.

- session:

  Shiny session object.

## Value

Called for its side effects within a `shinyApp` context.

## Details

This internal server function powers the interactive audio validation
interface. Users can import a CSV of BirdNET detections, link to local
audio directories, view spectrograms, and manually validate detections
through a Shiny DataTable interface.

## See also

[birdnet_validation_ui](https://birdnet-team.github.io/birdnetTools/reference/birdnet_validation_ui.md),
[birdnet_launch_validation](https://birdnet-team.github.io/birdnetTools/reference/birdnet_launch_validation.md)
