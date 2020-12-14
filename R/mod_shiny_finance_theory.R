#' @import shinyWidgets
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
NULL

#' mod_shiny_finance_theory_ui_risk_return UI Function
#'
#' @description Riks/Return theory pages.
#'
#' @param id namespace id for {shiny} module.
#'
#' @noRd
#'
mod_shiny_finance_theory_ui_risk_return <- function(id) {
  ns <- NS(id)
  fluidPage(h1("Risk/Return Ratio"),
    p("In 1952 Harry Markowitz suggested that assets should be evaluated based on their risk/return ratio.
			For the purposes of this app, I look at the asset returns measured by corresponding indices from 2000
			to the present day. "),
    p("The assets are:"),
      p(em("Equities:")),
        tags$div(tags$ul(
          tags$li("S&P 500"),
          tags$li("MSCI Europian Stock Index"),
          tags$li("MSCI Emerging Market Stock Index"))),
      p(em("Bonds:")),
        tags$div(tags$ul(
          tags$li("Barclays US Treasury Total Return Index"),
          tags$li("Barclays US Corporate Bonds Total Return Index"))),
      p(em("Real Estate:")),
        tags$div(tags$ul(
          tags$li("Dow Jones Real Estate Index"))),
    tabsetPanel(
      tabPanel("Whole Period", br(), plotlyOutput(ns("graph1"))),
      tabPanel("By Years", plotlyOutput(ns("graph2"))),
      tabPanel("Compound Return", plotlyOutput(ns("graph3"))))
  )
}

mod_shiny_finance_theory_ui_efficient_frontier <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(column(6, h1("Optimal portfolio"),
      p("Asset returns are not perferctly correlated. Therefore, we can combine assets into portfolios, and harverst
				the results of the diversification."),
      p("However, diversification is not limitless. For each expected risk there will be a portfolio with
				a maximum achievable return.The graph below shows risk/return profiles of simulated portfolios (gray) and
				a line (blue) depicting portfolios offering highest return for a given risk."),
      p("In Harry Markowitz (1952) framework, such line is called the Efficient Frontier. However, Markowitz' theory
				assumes that investors hold long-short portfolios. In our analysis, we limit ourselves to long-only portfolios,
				as it is the type of portfolios retail investors usually hold. Therefore, we will refer to portfolios on this line as
				'Optimal Portfolios', and the line itself as the 'Optimal Line'."),
      br(),
      plotlyOutput(ns("graph4"))))
  )
}

mod_shiny_finance_theory_ui_performance_measures <- function(id) {
  ns <- NS(id)
  fluidRow(column(8, div(htmlOutput(ns("measures")))))
}
