
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ExcelToShiny

<!-- badges: start -->

[![R-CMD-check](https://github.com/IDEMSInternational/ExcelToShiny/workflows/R-CMD-check/badge.svg)](https://github.com/IDEMSInternational/ExcelToShiny/actions)
[![Codecov test
coverage](https://codecov.io/gh/IDEMSInternational/ExcelToShiny/branch/main/graph/badge.svg)](https://app.codecov.io/gh/IDEMSInternational/ExcelToShiny?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![license](https://img.shields.io/badge/license-LGPL%20(%3E=%203)-lightgrey.svg)](https://www.gnu.org/licenses/lgpl-3.0.en.html)
<!-- badges: end -->

A system for creating Shiny dashboards using Excel spreadsheets.

## Installation

You can install the development version of ExcelToShiny from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# devtools::install_github("IDEMSInternational/ExcelToShiny")
```

## Overview

ExcelToShiny enables users to define Shiny dashboards using Excel
spreadsheets. This approach makes it easy to specify dashboard
components, including:

- **Main Page**: Global dashboard settings.
- **Contents Page**: Defines the structure of the Shiny app.
- **Display Pages**: Creates different visualization components.
- **Tabbed Pages**: Organises pages into tabbed layouts.
- **Download Pages**: Allows data downloads with optional access
  control.

## Example Usage

Below is an example of how to load an Excel file and use it to generate
a Shiny dashboard:

``` r
library(ExcelToShiny)
library(readxl)
library(shiny)
library(shinydashboard)
library(dplyr)

# Load example Excel file
data_list <- rio::import_list("data/example_dashboard.xlsx")

# Build the Shiny dashboard
build_shiny(
  title = "Example Shiny Dashboard",
  data_list = data_list,
  key_var = "ID",
  status = "primary",
  colour = "blue"
)
```

## Contributing, Reporting Issues, and Seeking Support

We welcome contributions from the community to enhance the
`ExcelToShiny` package. If you would like to contribute, please follow
these guidelines:

- Contribute to the software: If you wish to contribute new features,
  fix bugs, or improve the documentation, please fork the repository,
  create a new branch for your changes, and submit a pull request.
  Ensure that your code follows the existing style and include tests
  where applicable.

- Report issues or problems: If you encounter a bug or have a feature
  request, please open an issue in the [GitHub
  Issues](https://github.com/IDEMSInternational/ExcelToShiny/issues)
  section. Provide as much detail as possible, including steps to
  reproduce the issue if applicable.

- Seek support: For any questions or help with using `ExcelToShiny`, you
  can reach out by opening a discussion in the GitHub Discussions, or
  open a new issue with a “Question” label. We aim to respond within a
  week.

We appreciate your contributions and feedback!
