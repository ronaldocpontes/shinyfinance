#' @import shinydashboard
#' @importFrom tidyquant tq_get

app_layout = function(mod_shiny_finance_namespace, instrument_choices, instruments_selection, risk_free_choices, risk_free_selection, analysis_period) {

  sidebarWidth = 250

  dashboardPage(skin = "black",
    dashboardHeader(title = "Portfolio Allocation", titleWidth = sidebarWidth),
    dashboard_Sidebar(sidebarWidth),
    dashboard_body(mod_shiny_finance_namespace, instrument_choices, instruments_selection, risk_free_choices, risk_free_selection, analysis_period)
  )
}

dashboard_Sidebar = function(width) {
  dashboardSidebar(width = width,
    sidebarUserPanel("Ron Pontes", image = "www/avatar.png"),
    sidebarMenu(
      menuItem("Portfolio", tabName = "portfolio", icon = icon("line-chart"),
               menuSubItem("Portfolio Allocation", tabName = "portfolio_allocation"),
               menuSubItem("Allocation Comparison", tabName = "portfolio_optmisation")

               ),
      menuItem("Theory", tabName = "theory", icon = icon("graduation-cap"),
               menuSubItem("How to use the App", tabName = "how_to_use", icon = icon("book")),
               menuSubItem("Risk/Return Ratio", tabName = "risk_return_theory"),
               menuSubItem("Optimal Portfolio", tabName = "efficient_frontier_theory")
               ),
      menuItem("The Author", tabName = "author", icon = icon("user")),
      menuItem("Disclaimers", tabName = "discl", icon = icon("exclamation-triangle")))
  )
}

dashboard_body = function(mod_shiny_finance_namespace, instrument_choices, instruments_selection, risk_free_choices, risk_free_selection, analysis_period) {
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
      tabItem(tabName = "portfolio_allocation",
        mod_shiny_finance_portfolio_allocation_ui(mod_shiny_finance_namespace, instrument_choices, instruments_selection, risk_free_choices, risk_free_selection, analysis_period)),

      tabItem(tabName = "portfolio_optmisation",
        mod_shiny_finance_portfolio_optmisation_ui(mod_shiny_finance_namespace)),

      tabItem(tabName = "how_to_use",
        mod_shiny_finance_portfolio_how_to_use_ui(mod_shiny_finance_namespace)),

      tabItem(tabName = "risk_return_theory", mod_shiny_finance_theory_ui_risk_return(mod_shiny_finance_namespace)),
      tabItem(tabName = "efficient_frontier_theory", mod_shiny_finance_theory_ui_efficient_frontier(mod_shiny_finance_namespace)),

      tabItem(tabName = "author", htmlOutput("author")),
      tabItem(tabName = "discl", htmlOutput("disclaimer")))
  )
}
