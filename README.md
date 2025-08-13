---
editor_options:
  markdown:
    wrap: 72
output: word_document
---

# DTP Boost: An Interactive Tool for Modelling the Health and Economic Impact of Introducing DTPCV Booster Doses

## Introduction

DTP Boost is and interactive web-based decision-support tool allowing
users to design vaccination strategies to explore the health impact,
budget impact, and cost-effectiveness of introducing DTPCV booster doses
in a selected country. It focusing primarily on use for low- and
middle-income country settings that have not yet introduced DTPCV
boosters. Multiple different vaccination strategies can be designed,
allowing a detailed and interactive exploration of the relative costs
and benefits of introducing and routinely delivering different numbers
of DTPCV booster doses, vaccination coverage levels, introduction
approaches (e.g. simultaneous, phased), vaccine formulations and
vaccination delivery platforms (e.g. health facility, outreach site,
school-based).

**Background**

-   Routine infant vaccination has contributed to considerable reduction
    of cases and deaths from diphtheria, tetanus, and pertussis
    globally. However, these diseases continue to pose serious health
    risks. To increase duration of protection, the WHO recommends three
    DTPCV booster doses be offered in early childhood, childhood, and
    adolescence. Evidence to inform country-level decision-making about
    the introduction of DTPCV booster doses is needed, especially
    considering the availability of support for Gavi-eligible countries.

-   This project was a collaboration between the Modelling and
    Simulation Hub, Africa (MASHA) at the University of Cape Town (UCT),
    the African Field Epidemiology Network (AFENET), and the United
    States Centers for Disease Control and Prevention (CDC), with
    guidance from an expert technical advisory group and partnership
    with the Ugandan EPI Programme (UNEPI) for the pilot use-case.

**Approach (Decision tool)**

-   Interactive web-based decision-making tool - making modelling
    accessible (DTP Boost)

-   Single integrated mathematical (3 diseases) and economic models

-   Explores the health and economic impact (budget impact and
    cost-effectiveness)

-   Allows exploration and comparison of multiple approaches to
    introduction and routine delivery, and sensitivity analysis

-   Collaborative and multidisciplinary process involving end-users

## Key Features

-   **Configure model parameters**: Custom model parameters based on
    individual country epidemiological profiles, vaccination schedule,
    and costs
-   **Country-specific calibration**: Calibrate country data to model
    outputs by specifying the country parameters and finding
    transmission parameters which fit the data
-   **Scenario exploration**: Design and compare multiple vaccination
    strategies and implementation approaches
-   **Baseline comparison**: Compare intervention scenarios against
    current vaccination practices
-   **Model output visualisation**: Generate dynamic charts and graphs
    to visualise model outputs
-   **Export functionality**: Download results and visualisations for
    further analysis and reporting
-   **Session management**: Export your session so you can come back and
    work on it another day

## For End-Users

### Accessing the Application

The DTP Boost application is hosted and accessible at:
[**https://masha-app.shinyapps.io/DTPBoost**](https://masha-app.shinyapps.io/DTPBoost)

### How to Use the Application

**Step 1: Set Up Country Profile**

-   Select your country of interest from the dropdown menu

-   Review and adjust demographic parameters if necessary

-   Set current vaccination coverage and effectiveness estimates

-   Confirm baseline epidemiological and economic assumptions for your
    setting

**Step 2: Calibrate Model to Local Context**

-   Upload local surveillance data if available, or use default
    estimates from WHO or GBD

-   Adjust disease transmission parameters based on local
    epidemiological evidence and reasonable fit to data

**Step 3: Design Booster Strategy**

-   Define your proposed DTP booster vaccination strategy

-   Specify costs, target age groups and coverage targets

-   Set implementation timeline and roll-out approach

-   Configure vaccine characteristics (efficacy, duration of protection)

**Step 4: Run Scenarios and Analyse Results**

-   Execute model runs for your designed scenarios

-   Compare outcomes against the baseline (no booster) scenario

-   Review key metrics: population protected, cases prevented, deaths
    averted, cost-effectiveness

-   Explore many scenarios to understand uncertainty

**Step 5: Export and Share Results**

-   Download summary tables and visualisations

-   Generate reports for stakeholder presentations

-   Save scenario configurations for future reference

## For Developers

### Prerequisites

-   **R** (version 4.0 or higher)
-   **RStudio** (recommended for development)

### Installation

The application requires numerous R packages. You can install them using
one of the following methods:

**Option 1: Using pacman (recommended)**

``` r
# Install pacman if not already installed
if (!require("pacman")) install.packages("pacman")

# Install all required packages
pacman::p_load(dplyr, purrr, forcats, tidyr, stringr, shiny, 
               shinydashboard, shinydashboardPlus, here, shinyWidgets, 
               sortable, reactable, readxl, writexl, plotly, shinyjs, 
               shinycssloaders, rsconnect, httr, polite, glue, progressr, 
               tidyverse, deSolve, ggplot2, googledrive, googlesheets4, 
               data.table, lubridate, Rcpp, crayon, promises, future, 
               validate, gt, flextable, ggthemes, pryr, rlog, this.path, 
               bslib)
```

**Option 2: Using install.packages()**

``` r
# Install required packages
packages <- c("dplyr", "purrr", "forcats", "tidyr", "stringr", "shiny", 
              "shinydashboard", "shinydashboardPlus", "here", "shinyWidgets", 
              "sortable", "reactable", "readxl", "writexl", "plotly", "shinyjs", 
              "shinycssloaders", "rsconnect", "httr", "polite", "glue", "progressr", 
              "tidyverse", "deSolve", "ggplot2", "googledrive", "googlesheets4", 
              "data.table", "lubridate", "Rcpp", "crayon", "promises", "future", 
              "validate", "gt", "flextable", "ggthemes", "pryr", "rlog", "this.path", 
              "bslib")

install.packages(packages)
```

### Running the App Locally

To launch the application locally:

``` r
# Launch the Shiny app
shiny::runApp()
```

The app will open in your default web browser, typically at
`http://127.0.0.1:PORT` where PORT is automatically assigned.

### Advanced Configuration

#### Multiprocessing

The application supports multiprocessing to accelerate model runs. To
enable parallel processing:

``` r
# Enable multiprocessing (recommended for production)
library(future)
plan(multisession, workers = 4)  # Adjust workers based on your system

# To disable multiprocessing (useful for debugging)
plan(sequential)
```

**Note**: Multiprocessing is particularly beneficial as the three models
run in parallel, but it may complicate debugging or may not work well in
certain cloud environments.

#### Debugging Artifacts

For troubleshooting and development purposes, you can enable saving of
debugging artefacts in `app.R`:

``` r
shouldSaveAppObjects = TRUE  # or DEBUG=TRUE
```

When enabled, intermediate model outputs, error logs, and session
information will be saved as RDS files in the `cachedAppObjects/`
directory. This is particularly useful for:

-   Troubleshooting model convergence issues

-   Analysing unexpected results

-   Reproducing and reporting bugs

#### Development Mode

For active development, you may want to enable additional logging and
faster refresh:

``` r
# Enable granular logging
LOG <- makeLogger(default_level = LEVEL$TRACE)

# Enable development mode features
options(shiny.autoreload = TRUE)
options(shiny.trace = TRUE)
```

## Project Structure

```         
DTPShiny/
├── app.R                    # Main Shiny application file
├── DTPShiny.Rproj          # RStudio project file
├── deployApp.R             # Application deployment script
├── setShinyAppProperties.R # Shiny app configuration
├── models/                 # Mathematical model implementations
│   ├── R/                  # R model code and scripts
│   ├── data/               # Model-specific data files
│   ├── inputs/             # Model input parameters
│   └── parameters/         # Parameter configuration files
├── www/                    # Web assets and UI components
│   ├── R/                  # Shiny UI modules and utilities
│   │   ├── packages.R      # Required package definitions
│   │   ├── ui/             # UI component modules
│   │   └── utils/          # Utility functions
│   ├── markdown/           # Documentation markdown files
│   └── *.png, *.css, etc.  # Static assets (images, styles)
├── data/                   # Reference data and country parameters
├── cachedAppObjects/       # Debugging artifacts (when enabled)
├── cache/                  # Cached model outputs
└── testScripts/            # Testing and validation scripts
```

## Contributing

We welcome contributions to improve DTP Boost! Here's how you can help:

-   **Report Issues**: Use the GitHub issue tracker to report bugs or
    suggest enhancements
-   **Code Contributions**: Fork the repository, make your changes, and
    submit a pull request
-   **Documentation**: Help improve our documentation
-   **Testing**: Test the application with different country profiles,
    parameters and datasets and report any issues

Please ensure all contributions follow R coding best practices and
include appropriate documentation.

## Cite

The `CITATION.cff` file is included in this repository to provide
citation information for the DTP Boost application. If you use this tool
in your research or projects, please cite it appropriately.

## License

DTP Boost © 2025 by Modelling and Simulation Hub, Africa is licensed
under CC BY-NC-SA 4.0. To view a copy of this license, visit
<https://creativecommons.org/licenses/by-nc-sa/4.0/>

See the LICENSE.md file for full details.

------------------------------------------------------------------------

For questions, technical support, or collaboration inquiries, please
open an issue in this repository.
