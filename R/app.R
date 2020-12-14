#' @importFrom shiny tagList tags
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
NULL

source('./R/fct_datasources.R', local = TRUE)

#' PACKAGE CONSTANTS
MOD_SHINY_FINANCE_NAMESPACE = "mod_shiny_finance"
CACHE_FOLDER = "inst/extdata"
INSTRUMENTS = load_instruments(CACHE_FOLDER)

RISK_FREE_RATES = c("DGS3MO", "USD3MTD156N", "USDONTD156N", "GBP3MTD156N", "GBPONTD156N", "EUR3MTD156N", "EURONTD156N")
INITIAL_SELECTION = c("^GSPC", "^DJI", "^IXIC", "^FTSE", "^N100", "AAPL")
RISK_FREE = "DGS3MO"

ANALYSIS_PERIOD = seq(as.Date("2010-01-01"), Sys.Date(), by = "1 week")
START_DATE = first(ANALYSIS_PERIOD)
END_DATE = last(ANALYSIS_PERIOD)


#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
app_server = function(input, output, session) {

  mod_shiny_finance_portfolio_server(MOD_SHINY_FINANCE_NAMESPACE, RISK_FREE_RATES, START_DATE, END_DATE, CACHE_FOLDER)

  output$author = renderUI(includeHTML(
    system.file("./app/html/author.html", package = "shinyfinance")))
  output$disclaimer = renderUI(includeHTML(
    system.file("./app/html/disclaimer.html", package = "shinyfinance")))
}

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    app_ui = app_layout(MOD_SHINY_FINANCE_NAMESPACE, INSTRUMENTS, INITIAL_SELECTION, RISK_FREE_RATES, RISK_FREE, ANALYSIS_PERIOD)
  )
}

#' Add external Resources to the Application
#'
golem_add_external_resources <- function() {

  add_resource_path('www', app_sys('app/www'))
  add_resource_path('html', app_sys('app/html'))

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'shinyfinance'
    )
  )
}
