---
title: Code for Steller's Eider (*Polysticta stelleri*) population and density estimates from the Arctic Coastal Plain and Utqiaġvik Triangle surveys
---

<!-- badges: start -->

<!-- For more info: https://usethis.r-lib.org/reference/badges.html -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

## Overview

Code here supports the results found in the report:  

> Osnas, E.E. 2024. Steller's Eider (*Polysticta stelleri*) population and density estimates from the Arctic Coastal Plain and Utqiaġvik Triangle surveys using generalized additive models. US Fish and Wildlife Service, Division of Migratory Bird Management, Anchorage, AK.

A brief description of each code file is below. They are listed in the order they should be run to produce the report. See comments in code files for more information. 

| File | Description |
| ----- | ----- |
| wrangle_ABR.R | This file documents the data manipulations and processing of the Triangle data obtained from ABR, Inc. It also fit the GAMs models, saves the workspace, and outputs transect segment data with birds counts. | 
| predict_ABR.R | This file reads in results from the Triangle models and produces several figures (images) and csv data files. | 
| predict_ACP.R | This file reads in data from the ACP, fits models, simulates posteriors, and writes results and figure images to files. | 
| STEI_report.qmd | This is the main Quarto file that produces the report. Code chucks read data objects, do analysis, and produce figure and tables. Code chucks document methods in the report. | 
| getdata2.R | A modified getdata.R function from the ACP-Mapping source code used for Triangle data. This file reads annual bird observations and returns segmentized transect data. | 
| eider_presentation.qmd | A Quarto presentation given to the eider team in October 2024. |
| power_ABR.R | Apply the R package dsims to estimate CV and other design properties of the TRiangle survey. Results used in eider_presentation.qmd. | 
| print_layers.R | Random code use to quality control and explore spatial data received from ABR/USFWS. Plots study areas and transects in a leaflet map for interactive exploration, among other things. | 
| STEI_functions.R | Random helper function for STEI analysis. Right now, only contains a function used to summarize the posterior simulations. | 


## Installation

No installation is necessary. Follow the instructions under [Usage](#usage). 

## Usage

Download individual files or clone this repository to a local directory. You will also need the data directory, which is hosted elsewhere (TBD). You will need R version 4.3.1 or greater. Note that some of the files above source code from the GitHub repository, [ACP-Mapping](https://github.com/USFWS/ACP-Mapping) and data for ACP is sourced from ScienceBase at [https://doi.org/10.7944/qtgn-y170](https://doi.org/10.7944/qtgn-y170). 

## Getting help

Contact the [project maintainer](emailto:erik_osnas@fws.gov) for help with this repository or to report issues. 

## Contribute

Contact the project maintainer for information about contributing to this repository. Submit a [GitHub Issue](https://github.com/USFWS/r7-repo-template/issues) to report a bug or request a feature or enhancement.

-----

![](https://i.creativecommons.org/l/by/4.0/88x31.png) This work is
licensed under a [Creative Commons Attribution 1.0 International
License](https://creativecommons.org/licenses/by/1.0/).
