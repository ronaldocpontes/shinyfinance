#' @import ggplot2 ggthemes
#' @importFrom zoo index coredata
#' @importFrom plotly ggplotly
#' @importFrom scales percent_format
#' @importFrom lubridate year
#' @importFrom dplyr summarize
#' @importFrom tidyr drop_na spread gather

NULL

PERFORMANCE_MEASURES =
  renderUI(includeHTML("./R/mod_shiny_finance_theory_performance_measures.html"))

#' mod_shiny_finance_theory Server Functions
#'
#' @noRd
mod_shiny_finance_theory_server <- function(id, instruments) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    render_theory_page(output, instruments)
  })
}

render_theory_page = function(output, instruments) {

  output$measures = PERFORMANCE_MEASURES

  output$graph1 = renderPlotly({ plot_risk_return_ratios(instruments) })
  output$graph2 = renderPlotly({ plot_risk_return_by_years(instruments) })
  output$graph3 = renderPlotly({ plot_compound_returns(instruments) })
  output
}

plot_risk_return_ratios = function(df) {
  mean_ret = apply(df, 2, mean) * 252
  sd_ret = apply(df, 2, sd) * sqrt(252)

  df1 = data.frame(Asset = colnames(df), Return = mean_ret, Risk = sd_ret)

  g1 = ggplot(df1, aes(x = Risk, y = Return, label = Asset)) + geom_point(color = "steelblue3") +
    scale_y_continuous(limits = c(0, 0.10), labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(limits = c(0, 0.4), labels = scales::percent_format(accuracy = 1)) +
    xlab('Risk (standard deviation of returns, annualized)') + ylab('Average Returns, annualized') +
    theme_hc()

  g1 = ggplotly(g1, tooltip = c("x", "y"), width = 600) %>% add_annotations(x = df1$Risk, y = df1$Return,
    text = colnames(df), xref = "x", yref = "y", showarrow = TRUE, arrowhead = 4, arrowsize = .5, ax = 60, ay = -30)

  g1$x$data[[1]]$text = paste("Return:", round(df1$Return, 4) * 100, "%", "<br>",
                              "Risk:", round(df1$Risk, 4) * 100, "%")

  g1 = g1 %>% layout(margin = list(b = 50, l = 50, t = 100), title = "Risk/Return of Assets <br> (annualized) 2000 - Present")
}

plot_risk_return_by_years = function(df) {
  risk_ret_ann = summarize_anualised_risk_returns(df)

  g2 = ggplot(risk_ret_ann, aes(x = Risk, y = Return, text = paste(year, "<br>", "Return:",
                                                              round(Return, 4) * 100, "%", "<br>", "Risk:", round(Risk, 4) * 100, "%"))) +
    geom_point(color = "steelblue3") +
    xlab('Risk (standard deviation of returns, annualized)') +
    ylab('') +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_hc() + facet_wrap(~reorder(Asset, Risk, sd)) +
    theme(axis.title = element_text(hjust = 1, vjust = 1))

  g2 = ggplotly(g2, tooltip = c("text"), width = 600)

  g2[['x']][['layout']][['annotations']][[1]][['y']] = -0.1 #Move y-label lower

  g2 = g2 %>% layout(margin = list(b = 50, l = -50, t = 120), title = "Risk/Return of Assets By Years <br> (annualized) 2000 - Present",
                    yaxis = list(title = "Average Return, annualized", tickprefix = " "))
}

plot_compound_returns = function(df) {
  risk_ret_cum = summarize_compound_risk_returns(df)

  g3 = ggplot(risk_ret_cum, aes(x = as.Date(date), y = cumRet, text = paste(date, "<br>", "Compound return:", round(cumRet, 4) * 100, "%"), group = 1)) +
      geom_line(color = "steelblue3") + facet_wrap(~facet) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_x_date(date_breaks = "5 years", date_labels = "%y") +
      xlab('Years') + ylab('') + theme_hc()


  g3 = ggplotly(g3, tooltip = "text", width = 600)

  g3[['x']][['layout']][['annotations']][[1]][['y']] = -0.1 #Move y-label lower

  g3 = g3 %>% layout(margin = list(b = 50, l = 50, t = 120), title = "Compound Return <br> 2000 - Present",
                    yaxis = list(title = "Compound Return"))
}

plot_efficient_frontier = function(returns, rf) {
  mean_ret = apply(returns, 2, mean) * 252
  cov_matrix = cov(returns) * 252

  sim_port = simPortfolios(mean_ret, cov_matrix, nsim = 10000)

  #Calculate the EF line
  min_tret = sim_port[sim_port$Risk == min(sim_port$Risk), "Return"][[1]] #Usually a good starting point
  max_tret = max(sim_port$Return)

  tret_vector = seq(min_tret, max_tret, length.out = 20)

  ef_line = data.frame(Risk = rep(NA, length(tret_vector)), Return = rep(NA, length(tret_vector)),
                      Portfolio = rep(NA, length(tret_vector))) #Place holder
  i = 1 #counter

  for (ret in tret_vector) {
    ef_w = findEfficientFrontier.Return(returns, ret)
    tmp.Ret = calcPortPerformance(ef_w, mean_ret, cov_matrix)[[1]]
    tmp.Risk = calcPortPerformance(ef_w, mean_ret, cov_matrix)[[2]]

    ef_line[i, 'Return'] = tmp.Ret
    ef_line[i, 'Risk'] = tmp.Risk
    ef_line[i, 'Portfolio'] = paste(
      c(colnames(df)), paste(as.character(round(ef_w, 4) * 100), "%"), sep = ": ", collapse = "<br>")
    i = i + 1
  }

  g4 = ggplot(data = sim_port, aes(x = Risk, y = Return)) + geom_point(data = sim_port, aes(x = Risk, y = Return), color = 'gray', alpha = 0.5) +
    geom_line(data = ef_line, aes(x = Risk, y = Return, text = Portfolio, group = 1), color = 'steelblue3', size = 2, alpha = 0.5) +
    scale_y_continuous(limits = c(0, 0.10), labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(limits = c(0, 0.25), labels = scales::percent_format(accuracy = 1)) +
    theme_hc() + xlab('Risk (standard deviation of returns, annualized)') + ylab('') +
    theme(
      panel.background = element_rect(fill = "transparent") # bg of the panel
      , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
      , panel.grid.major = element_blank() # get rid of major grid
      , panel.grid.minor = element_blank() # get rid of minor grid
      , legend.background = element_rect(fill = "transparent") # get rid of legend bg
      , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    )

  g4 = ggplotly(g4, tooltip = "text", width = 600)

  g4 = g4 %>% layout(margin = list(b = 50, l = 50, t = 120), title = "Simulated Portfolios and the Optimal Line",
                    yaxis = list(title = 'Average Return, annualized'))
}

summarize_anualised_risk_returns = function(df, scale = 252) {
  data.frame(date = index(df), coredata(df)) %>%
    gather(key = "Asset", value = "Return", - date) %>%
    mutate(year = lubridate::year(date)) %>%
    group_by(Asset, year) %>%
    dplyr::summarize(av_ret = mean(Return) * scale, Risk = sd(Return) * sqrt(scale)) %>%
    rename(Return = av_ret)
}

summarize_compound_risk_returns = function(df) {
  risk_ret_ann = summarize_anualised_risk_returns(df)
  order = risk_ret_ann %>% group_by(Asset) %>% summarise(sd_SD = sd(Risk)) %>% arrange(sd_SD) %>% select(Asset)
  order = order[['Asset']]

  risk_ret_cum = data.frame(date = index(df), coredata(df)) %>%
    gather(key = "Asset", value = "Return", - date) %>%
    group_by(Asset) %>%
    arrange(date) %>%
    mutate(cumRet = cumprod(1 + Return) - 1)
  risk_ret_cum$facet = factor(risk_ret_cum$Asset, levels = c(order))
  risk_ret_cum
}
