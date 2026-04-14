# User interface for BirdNET audio validation platform

Constructs the user interface layout for the BirdNET audio validation
platform within the `birdnetTools` package. This UI includes inputs for
file upload, audio folder selection, spectrogram settings, and outputs
for data display and spectrogram plotting.

## Usage

``` r
birdnet_validation_ui(request)
```

## Arguments

- request:

  Internal parameter for `{shiny}` applications. Typically not set by
  the user.

## Value

A Shiny UI definition built with
[`bslib::page_sidebar()`](https://rstudio.github.io/bslib/reference/page_sidebar.html).

## Details

This internal UI function is called by the Shiny validation app launched
through higher-level user-facing functions. It sets up sidebar inputs
for file import and settings, and main panel outputs for data and
spectrogram display.

## See also

[birdnet_validation_server](https://birdnet-team.github.io/birdnetTools/reference/birdnet_validation_server.md),
[birdnet_launch_validation](https://birdnet-team.github.io/birdnetTools/reference/birdnet_launch_validation.md)
