#' @import dplyr PerformanceAnalytics plotly
#' @importFrom RColorBrewer brewer.pal
#' @importFrom xml2 read_html
#' @importFrom plotly plotly plotlyOutput renderPlotly
NULL

# source('./R/fct_datasources.R', local = TRUE)
# source('./R/fct_portfolio.R', local = TRUE)
# source('./R/utils_shiny.R', local = TRUE)
# source("./R/shiny/finance/portfolio/theory_server.R", local = TRUE)

HOW_TO_USE =
  renderUI(includeHTML("./R/mod_shiny_finance_portfolio_how_to_use.html"))

PALLETE = rev(brewer.pal(6, "Blues"))

PERFORMANCE_CHART_METHODS = c("ModifiedVaR", "ModifiedES")

#' shiny_finance_portfolio Server Functions
#' To be added to the shiny server function:
#' eg: mod_shiny_finance_portfolio_server("shiny_finance_portfolio_ui_1")
#'
#' @noRd
mod_shiny_finance_portfolio_server <- function(id, risk_free_choices, start_date, end_date, cache_folder) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    render_portfolio_optmisation(ns, input, output, session, risk_free_choices, start_date, end_date, cache_folder)
  })
}

render_portfolio_optmisation = function(ns, input, output, session, risk_free_choices, start_date, end_date, cache_folder) {
  react = c()
  output$how_to_use = HOW_TO_USE

  # Download instruments and render selectors
  rfr = load_fred_data(risk_free_choices, start_date, end_date) %>%
        fred_to_daily_rates() %>% as.xts()

  react$risk_free = reactive({
    rfr[, input$risk_free]
  })

  react$instruments = reactive({
    if (length(input$tickers > 2))
      load_tickers(toupper(input$tickers), start_date, end_date, cache_folder) %>% as.xts()
  })

  output$benchmark = renderUI({
    selectizeInput(ns("benchmark"),
    label = "Benchmark",
    choices = input$tickers,
    selected = input$tickers[1],
    multiple = FALSE)
  })

  output %>%
    monitor_instruments_selection(ns, input, session, react, rv) %>%
    render_theory_page(react$instruments())
}

monitor_instruments_selection = function(output, ns, input, session, react, rv) {
  instrument_changes <- reactive({
    list(input$tickers, input$benchmark)
  })

  observeEvent(instrument_changes(), {
    if (length(input$tickers > 2))
      render_portfolio_ui(output, ns, input, session, react)
  })

  output
}

render_portfolio_ui = function(output, ns, input, session, react, rv) {
  instrument_names = react$instruments() %>% colnames()
  ni = length(instrument_names)

  if (ni < 2 || is.null(input$benchmark) || nchar(input$benchmark) < 1) {
    return(output)
  }

  # Reactive Variables
  rv = reactiveValues(selectedWeights = rep(1 / ni, ni))
  names(rv$selectedWeights) = instrument_names

  react$bt_data = reactive({
    rv$selectedWeights
    input$date_range
    input$rebalance

    isolate(benchmark_portfolio(
      react$instruments(),
      rv$selectedWeights,
      input$rebalance,
      gsub("\\^", "X.", input$benchmark),
      as.Date(input$date_range[1]),
      as.Date(input$date_range[2])))
  })

  # Allocation Page
  render_sliders(ns, input, output, session, react, rv)
  render_portfolio_returns(input, output, session, react, rv)

  # Comparison Page
  render_optimised_allocations(input, output, session, react, rv)

  output
}


#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom purrr imap map
#' @importFrom glue glue
#' @noRd
render_sliders = function(ns, input, output, session, react, rv) {

  #A llocation sliders
  output$sliders = renderUI({
    sliders = names(rv$selectedWeights) %>%
            imap(~{ wghtsliderInput(ns(glue("{sliders_id}-{.}")), rv$selectedWeights[.y], label = .) }) %>%
      split(rep_len(1:2, length(.))) %>%
          map(~{ column(6, .) })

    tags$div(id = "sliders-container", sliders)
  })

  # Sliders Observers: If any of the sliders change, rebalance other weights so sum = 1
  sliders_id = "multi_slider"
  slider_observers = names(rv$selectedWeights) %>% imap(~{
    slider_input = glue("{sliders_id}-{.}")
    observeEvent(input[[slider_input]], {
      suspendMany(slider_observers)
      current = rv$selectedWeights[[.]]
      change = input[[slider_input]]
      if (!is_round_of(change, current)) {
        rv$selectedWeights = rebalance(rv$selectedWeights, input[[slider_input]], .)
      }
      resumeMany(slider_observers)
    })
  })
}

render_portfolio_returns = function(input, output, session, react, rv) {

  # Allocation pie chart
  output$graph5 = renderPlotly({ plot_allocation_pie_chart(rv$selectedWeights) })

  #  Returns chart
  output$graph6 = renderPlotly({
    react$bt_data() %>%
                charts.PerformanceSummary(Rf = react$risk_free(), geometric = TRUE, p = .95, plot.engine = "plotly", colors = PALLETE) %>%
    apply_plotly_styling()
  })

  # Performance metrics
  output$port_measures1 = renderTable(digits = 2, rownames = TRUE, {
    calculate_performance_metrics(react$bt_data(), react$risk_free(), input$date_range[1], input$date_range[2])
  })

  output$port_measures2 = renderTable(digits = 2, rownames = TRUE, {
    calculate_risk_metrics(react$bt_data(), react$risk_free(), input$date_range[1], input$date_range[2])
  })
}

render_optimised_allocations = function(input, output, session, react, rv) {

  react$opt_weights = reactive({
    react$bt_data()
    isolate(
      calculate_optimal_weights(react$instruments(), react$bt_data(), input$date_range[1], input$date_range[2]))
  })

  react$opt_data = reactive({
    react$opt_weights()
    isolate(calculate_optmised_returns(
                      react$instruments(),
      as.Date(input$date_range[1]
                    ),
      as.Date(input$date_range[2]),
      react$opt_weights(),
      react$bt_data()
                    ))
  })

  # Optimised allocation pie charts
  output$graph7 = renderPlotly({
    plot_allocation_pie_chart(rv$selectedWeights)
  })

  output$graph8 = renderPlotly({
    df = react$opt_weights()
    plot_allocation_pie_chart(df$SimilarReturn, rownames(df))
  })

  output$graph9 = renderPlotly({
    df = react$opt_weights()
    plot_allocation_pie_chart(df$SimilarRisk, rownames(df))
  })

  # Returns Chart
  output$graph10 = renderPlotly({
    react$opt_data() %>%
    charts.PerformanceSummary(Rf = react$risk_free(), geometric = TRUE, p = .95, plot.engine = "plotly", colors = PALLETE) %>%
    apply_plotly_styling()
  })

  # Performance metrics
  output$opt_measures1 = renderTable(digits = 2, rownames = TRUE, {
    calculate_performance_metrics(react$opt_data(), react$risk_free(), input$date_range[1], input$date_range[2])
  })

  output$opt_measures2 = renderTable(digits = 2, rownames = TRUE, {
    calculate_risk_metrics(react$opt_data(), react$risk_free(), input$date_range[1], input$date_range[2])
  })

}

plot_allocation_pie_chart = function(weights, labels = names(weights)) {

  alloc = data.frame(wght = weights, asset = labels)

  plot_ly(alloc, labels = ~asset, values = ~wght, type = 'pie',
      textposition = 'inside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#000'),
      hoverinfo = 'text',
      text = ~paste(round(wght, 4) * 100, ' %'),
      marker = list(line = list(color = '#FFFFFF', width = 1)), showlegend = FALSE, width = 250, height = 250) %>%
      apply_plotly_styling()
}

apply_plotly_styling = function(x, pallete = PALLETE) {
  layout(x,
    xaxis = list(title = "", zeroline = T, showline = F, showgrid = F, showticklabels = T,
      autotick = T, gridcolor = "#D3D3D3"),
    yaxis = list(title = "", zeroline = T, showline = F, showgrid = T, showticklabels = T,
      autotick = T, gridcolor = "#D3D3D3", tickformat = "%"),

    colorway = pallete,
    legend = list(orientation = "h", x = 0.1, y = 1.2),
    paper_bgcolor = 'rgba(0,0,0,0)',
    plot_bgcolor = 'rgba(0,0,0,0)',
    margin = list(b = 20, l = 20, t = 30))
}
