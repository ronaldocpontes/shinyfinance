#' @import shiny shinyWidgets
#' @importFrom xml2 read_html
#' @importFrom plotly plotlyOutput
NULL

#' shiny_finance_portfolio UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
mod_shiny_finance_portfolio_allocation_ui <- function(id, instrument_choices, instruments_selection, risk_free_choices, risk_free_selection, analysis_period) {
  ns <- NS(id)
  tagList(
    fluidRow(div(
      column(6, selectizeInput(ns("tickers"),
        label = "Instruments: (select or type a new one)",
        choices = instrument_choices,
        selected = instruments_selection,
        multiple = TRUE,
        options = list(create = TRUE, maxItems = 50)),),
      column(2, uiOutput(ns("benchmark"))),
      column(1),
      column(3, selectizeInput(ns("risk_free"),
        label = "Risk-Free Rate",
        choices = risk_free_choices,
        selected = risk_free_selection,
        multiple = FALSE,
        options = list(create = FALSE))))),
    fluidRow(div(
      column(6, h4("Portfolio Allocation:", align = "left"),),
      column(3, h4("Select Rebalance Schedule:", align = "left")),
      column(3, h4("Allocation", align = "left")))),
    fluidRow(
      column(6, uiOutput(ns("sliders"))),
      column(3,
        fluidRow(radioButtons(inputId = ns("rebalance"),
          label = NULL,
          choices = c("Monthly", "Quarterly", "Annually", "Never"),
          selected = "Never")),
        fluidRow(br(), br(), br())),
      column(3, div(plotlyOutput(ns("graph5")), align = "left", style = "height:250px"))),
    fluidRow(
      column(6,
        sliderTextInput(
          inputId = ns("date_range"), label = "", width = "95%",
          choices = analysis_period, selected = range(analysis_period),
          grid = TRUE, dragRange = FALSE),
        align = "right")),
    fluidRow(
      column(6, h4("Compound Return", align = "left")),
      column(6, h4("Performance Measures", align = "left"))),
    fluidRow(
      column(6, div(plotlyOutput(ns("graph6")), align = "center")),
      column(3, div(tableOutput(ns("port_measures1")), align = "center")),
      column(3, div(tableOutput(ns("port_measures2")), align = "center")))
  )
}

mod_shiny_finance_portfolio_optmisation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(4, h4("Your Allocation", align = "center")),
        column(4, h4("Similar Return", align = "center")),
        column(4, h4("Similar Risk", align = "center"))
        ),
    fluidRow(column(4, div(plotlyOutput(ns("graph7")), align = "center")),
        column(4, div(plotlyOutput(ns("graph8")), align = "center")),
        column(4, div(plotlyOutput(ns("graph9")), align = "center"))
        ),
    fluidRow(column(6, h4("Compound Return", align = "center")),
         column(6, h4("Performance Measures", align = "center"))),
    fluidRow(column(6, div(plotlyOutput(ns("graph10")), allign = "center")),
        column(6, div(tableOutput(ns("opt_measures1")), align = "center"))),
    fluidRow(column(6),
        column(6, div(tableOutput(ns("opt_measures2")), align = "center")))
    )
}

mod_shiny_finance_portfolio_how_to_use_ui <- function(id) {
  ns <- NS(id)
  fluidRow(column(8, div(htmlOutput(ns("how_to_use")))))
}
