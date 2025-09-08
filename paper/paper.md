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
 - name: University of Northern British Columbia, Prince George, BC, Canada
   index: 1
 - name: Center for Conservation Bioacoustics, Cornell Lab of Ornithology, Cornell University, Ithaca, NY, USA
   index: 2
 - name: Technische Universität Chemnitz, D-09111 Chemnitz, Germany
   index: 3
date: 09 September 2025
bibliography: paper.bib

---

# Summary
`birdnetTools` is an R package for post-processing outputs from BirdNET, an open-source neural network developed by the Cornell Lab of Ornithology and Chemnitz University of Technology for detecting and identifying bird species from audio recordings [@kahl:2021]. The `birdnetTools` package streamlines workflows for cleaning and combining multiple BirdNET selection tables, filtering detections by species, confidence, or date/time, visualizing temporal and spatial patterns, and validating results using an interactive Shiny app. It also supports species-specific and universal confidence thresholds, enabling reproducible threshold-setting workflows. 


# Statement of need
Automated acoustic monitoring is increasingly used in ecology and conservation, with BirdNET, created with python, being one of the most widely adopted tools for bird sound identification. While the `birdnetR` [@kahl:2025] package allows R users to run BirdNET classifications, there is no dedicated framework for post-processing these outputs within R.

The `birdnetTools` package fills this gap by providing functions to clean and wrangle BirdNET detections, apply species-specific or universal confidence thresholds, visualize results, and validate predictions through an interactive Shiny app. Its design is based on workflows commonly used in published studies (e.g., @tseng:2024) and incorporates methods for threshold setting and validation developed in recent research (i.e., @tseng:2025; @wood:2024). By consolidating these tools, birdnetTools streamlines analysis and lowers barriers for ecologists and conservation practitioners adopting BirdNET in large-scale monitoring projects.


# Key functionalities



# Acknowledgements
The `birdnetTools` project was supported by the Michael Smith Foreign Study Supplements, funded by the Natural Sciences and Engineering Research Council of Canada (NSERC). We thank all members of the BirdNET team for their valuable contributions, and we are grateful to Connor Wood and Trey Ursillo for their insightful feedback in improving the package.

# References
