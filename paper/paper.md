---
title: 'birdnetTools: An R package for working with BirdNET output'
tags:
  - R
  - bird
  - ecology
  - birdnet
  - bioacoustics
authors:
  - name: Sunny Tseng
    orcid: 0000-0002-8621-2244
    corresponding: true # (This is how to denote the corresponding author)
    affiliation: 1 

  - name: Stefan Kahl
    orcid: 0000-0002-2411-8877
    affiliation: "2, 3" # (Multiple affiliations must be quoted)


affiliations:
 - name: Natural Resources and Environmental Studies, University of Northern British Columbia, Prince George, BC, Canada
   index: 1
 - name: K. Lisa Yang Center for Conservation Bioacoustics, Cornell Lab of Ornithology, Cornell University, Ithaca, NY, USA
   index: 2
 - name: Chemnitz University of Technology, Chemnitz, Germany
   index: 3
date: 09 September 2025
bibliography: paper.bib
---

# Summary

`birdnetTools` is an R package for post-processing outputs from BirdNET, an open-source neural network developed by the Cornell Lab of Ornithology and Chemnitz University of Technology for detecting and identifying bird species from audio recordings [@kahl:2021]. The `birdnetTools` package streamlines workflows for cleaning and combining multiple BirdNET selection tables, filtering detections by species, confidence, or date/time, visualizing temporal and spatial patterns, and validating results using an interactive Shiny app. It also supports species-specific and universal confidence thresholds, enabling reproducible threshold-setting workflows.

# Statement of need

# State of the field

Automated acoustic monitoring is increasingly used in ecology and conservation [@perezgranados:2023], with BirdNET being one of the most widely adopted tools for bird sound identification (e.g., @funosas:2024, @mcginn:2023, and @bota:2023). Although BirdNET was developed in Python, most of its primary users are ecologists who conduct analyses primarily in R. This language difference can limit accessibility for some research teams. While the `birdnetR` package [@günther:2025] enables R users to run BirdNET classifications, there is no dedicated framework in R for post-processing these outputs.

The `birdnetTools` R package fills this gap by providing functions built on workflows commonly used in published studies (e.g., @tseng:2024) and by incorporating threshold-setting and validation methods developed in recent research (i.e., @tseng:2025; @wood:2024). By consolidating these tools, `birdnetTools` streamlines analysis and lowers barriers for ecologists and conservation practitioners adopting BirdNET in large-scale monitoring projects (\autoref{fig:packages}).

::: {style="text-align: center;"}
![Integration of BirdNET R packages. birdnetR produces species detections from audio recordings, while birdnetTools provides tools for post-processing, data cleaning, and validation. \label{fig:packages}](./fig_packages.png){width="55%"}
:::

# Software design

The design of `birdnetTools` follows the natural analytical workflow used by ecologists: data import, data exploration, and detection validation. The package aims to streamline the post-processing of BirdNET outputs, which often exist in formats of Raven selection tables or CSV files. Our functions accommodate these various formats provided the core data columns are maintained.

The package architecture is highly integrated, with functions designed to pass information seamlessly from one stage of the pipeline to the next. Built on the `tidyverse` ecosystem, `birdnetTools` utilizes `dplyr` for data wrangling and `ggplot2` for visualizations. We implemented an interactive Shiny interface for audio validation. This modular approach allows users to either run a complete end-to-end pipeline or use specific functions, such as `birdnet_combine()`, to integrate BirdNET data into their existing custom R scripts. Furthermore, the package prioritizes data provenance by ensuring that temporal metadata and site identifiers are preserved throughout the filtering and validation process.

In summary, functions in `birdnetTools` fall into three categories: data import, data exploration, and detection validation (\autoref{fig:workflow}).

1.  Data import: `birdnet_combine()` integrates BirdNET outputs into R, supporting formats from the BirdNET GUI, Raven Pro, and the `birdnetR` package.

2.  Data exploration: `birdnet_filter()` enables filtering by species, threshold, or date/time; `birdnet_add_time()` extracts temporal metadata; and `birdnet_heatmap()` visualizes activity patterns.

3.  Detection validation: an R ShinyApp was developed, implementing threshold-setting approaches, including the precision-based method of @tseng:2025 and the probability-based method of @wood:2024.

![Workflow of the birdnetTools R package. \label{fig:workflow}](./fig_workflow.png)

# Research impact statement

While birdnetTools is developed primarily to support the core developer's research,

-   15 stars and 3 forks

# AI usage disclosure

AI tools, specifically ChatGPT (v5.3) and Gemini (v3), were used for the editorial correction of this manuscript and the associated function documentation. All AI-edited content was reviewed and verified by the authors, who take full responsibility for the final text.

# Acknowledgements

The `birdnetTools` project was supported by the Michael Smith Foreign Study Supplements, funded by the Natural Sciences and Engineering Research Council of Canada (NSERC). We thank all members of the BirdNET team for their valuable contributions, and we are grateful to Connor M. Wood and Trey Ursillo for their insightful feedback in improving the package.

The development of BirdNET is supported by the German Federal Ministry of Research, Technology and Space (FKZ 01\|S22072), the German Federal Ministry for the Environment, Climate Action, Nature Conservation and Nuclear Safety (FKZ 67KI31040E), the German Federal Ministry of Economic Affairs and Energy (FKZ 16KN095550), the Deutsche Bundesstiftung Umwelt (project 39263/01) and the European Social Fund.

# References
