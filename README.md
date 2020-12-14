
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinyfinance

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of shinyfinance is to illustrate Asset Management Principles
using R and Shiny dashboards.

You can try this app live on both Shiny and Heroku (free but not that
fast
    :D):

  - [https://o-o-o.shinyapps.io/finance](https://o-o-o.shinyapps.io/finance/)
  - [https://shinyfinance.herokuapp.com](https://shinyfinance.herokuapp.com/)

**Note:** shinyapps is a free service will redeploy a new server if the
app has not not being used recently. Please wait for the app to start
and download the first set of timeseries data.

## Usage

The application illustrates the key principles of portfolio
optimization.

The Portfolio allocation section allows you to chose financial
instruments and build a portfolio to be simulated between the selected
date range and a desired rebalancing schedule. The performance obtained
is then compared to a selected benchmark.

On the Allocation Comparison section, the portfolio is compared to two
possible optimized portfolios which had similar performances during the
same analysis period: a portfolio with the same return and lower risk,
and a portfolio with the same risk and higher return.

## IMPORTANT

Please be informed that information in this application is provided for
illustrative purposes only and does not constitute a financial advice.

## Installation

You can install the released version of shinyfinance from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("shinyfinance")
```

## Package design

This app uses {golem}, which is an opinionated framework for building
production-grade shiny applications. Please read the step-by-step guide
on [https://github.com/ThinkR-open/golem](github.com/ThinkR-open/golem)

## Running on RStudio

Open the project in RStudio and run the commands in ./dev/run\_dev.R

## File structure

``` 

#> ├── DESCRIPTION                  Package info and dependencies
#> ├── NAMESPACE                    Roxygen2 generated via devtools::document()
#> ├── Dockerfile                   Docker support for Azure, Heroku and ShinyApps deployments
#> ├── heroku.yml                   CI/CD pipeline for Heroku free hosting
#> ├── azure-pipelines.yml          CI/CD pipeline for build and container deploy to Azure Web App
#> ├── R
#> │   ├── app_config.R             golem R package config
#> │   ├── app_layout.R             overall shiny dashboard structure
#> │   ├── app.R                    entry point for the Shiny App
#> │   ├── fct_datasources.R        R functions for data access
#> │   ├── fct_portfolio.R          R functions for portifolio optimization
#> │   ├── mod_shiny_finance_portfolio
#> │   ├── ...                      shiny module for portifolio optimization
#> │   ├── mod_shiny_finance_theory
#> │   ├── ...                      shiny module with finance theory pages
#> │   ├── mod_shiny_utils          utility functions and widgets for shiny
#> │   └── run_app.R                used to start the app from R CLI
#> ├── dev
#> │   ├── 01_start.R
#> │   ├── 02_dev.R
#> │   ├── 03_deploy.R
#> │   └── run_dev.R                * use this file to start the app on RStudio *
#> ├── inst
#> │   ├── app
#> │   │   └── www                  static web assets
#> │   ├── extdata                  cache folder for downloaded timeseries
#> │   └── golem-config.yml
#> ├── tests
#> │   └── testthat                 functional and unit tests
#> └── man
#>     └── run_app.Rd               package documentation
```
