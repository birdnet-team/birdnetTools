
<!-- README.md is generated from README.Rmd. Please edit that file -->

# birdnetTools

<!-- badges: start -->

[![R-CMD-check](https://github.com/birdnet-team/birdnetTools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/birdnet-team/birdnetTools/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/birdnet-team/birdnetTools/graph/badge.svg)](https://app.codecov.io/gh/birdnet-team/birdnetTools)
<!-- badges: end -->

The `birdnetTools` is an R package designed to streamline the
post-processing and validation of output from BirdNET, an open-source
neural network developed by the [Cornell Lab of
Ornithology](https://www.birds.cornell.edu/home/) and [Chemnitz
University of Technology](https://www.tu-chemnitz.de/index.html.en) for
detecting and identifying bird sounds.

The goal of `birdnetTools` is to help researchers manage, explore, and
validate BirdNET results, which can be derived by [BirdNET
Analyzer](https://github.com/BirdNET-Team/BirdNET-Analyzer), or the
[birdnetR](https://birdnet-team.github.io/birdnetR/index.html) package.
It includes functions for filtering detections by species, confidence,
and date/time; visualizing temporal patterns; and validating detections
with an interactive Shiny app to support threshold-setting workflows.

## Installation

You can install the development version of birdnetTools from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("birdnet-team/birdnetTools")
```

## Example use

This is a basic example to show how to visualize BirdNET output data.
The example assumes you have a data frame with BirdNET results:

``` r
library(birdnetTools)

# to read data from a local path
data <- birdnet_combine("path/to/BirdNET/output")

# to filter data for given species, and time range
data_filtered <- birdnet_filter(data, 
                                species = "American Robin",
                                year = 2023,
                                date_range = c("2023-01-01", "2023-12-31"))

# to subsample dataset for validation purpose
data_subsampled <- birdnet_subsample(data_filtered, 
                                     n = 1000, 
                                     method = "stratified")

# data visualization
birdnet_heatmap(data_filtered)
  
  
```

## About this project

The development of `birdnetTools` is part of a visiting scholar program
supported by the [Michael Smith Foreign Study
Supplements](https://www.nserc-crsng.gc.ca/Students-Etudiants/PG-CS/CGSForeignStudy-BESCEtudeEtranger_eng.asp),
funded by the Natural Sciences and Engineering Research Council of
Canada (NSERC). I’m incredibly fortunate to have received this support,
which allowed me to collaborate in person with the BirdNET team in
Chemnitz, Germany.

This has been a valuable opportunity—not only for scientific
collaboration and networking, but also for memorable moments shared with
colleagues and the vibrant atmosphere of life in Europe.

## BirdNET funding and partners

BirdNET is supported by Jake Holshuh (Cornell class of ’69) and The
Arthur Vining Davis Foundations. Our work in the K. Lisa Yang Center for
Conservation Bioacoustics is made possible by the generosity of K. Lisa
Yang to advance innovative conservation technologies to inspire and
inform the conservation of wildlife and habitats.

The development of BirdNET is supported by the German Federal Ministry
of Education and Research through the project “BirdNET+” (FKZ
01\|S22072). The German Federal Ministry for the Environment, Nature
Conservation and Nuclear Safety contributes through the “DeepBirdDetect”
project (FKZ 67KI31040E). In addition, the Deutsche Bundesstiftung
Umwelt supports BirdNET through the project “RangerSound” (project
39263/01).

BirdNET is a joint effort of partners from academia and industry.
Without these partnerships, this project would not have been possible.

![](https://tuc.cloud/index.php/s/KSdWfX5CnSRpRgQ/download/box_logos.png)
