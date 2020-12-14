#' @import PerformanceAnalytics
#' @importFrom xts as.xts
#' @importFrom zoo index coredata
#' @importFrom DEoptim DEoptim
NULL

### Helper functions for portfolio valuation
decimals = function(x) { min(which(x * 10 ^ (0:20) == floor(x * 10 ^ (0:20)))) - 1 }

is_round_of = function(x, y) { x == round(y, digits = decimals(x)) }

#' Rebalance weights vector given a change so the sum is 1
#'
#' @param weights, change, key parameters.
rebalance = function(weights, change, key) {

  current = weights[[key]]

  if (change > 1) stop("Error in change: value is greater than 1")

  if (is_round_of(change, current)) {
    return(weights)
  } else if (change == 1) {
    change = 0.9999
  }
  i = match(key, names(weights))
  delta = change - current
  weights = weights - (delta * weights / sum(weights[-i]))
  weights[[key]] = change
  weights
}

#' benchmark_portfolio
#'
#' @param weights, change, key parameters.
benchmark_portfolio = function(df, wght, rebalance, benchmark_column, from, to) {
  instruments = df %>% as.xts() %>% .[index(.) >= from & index(.) <= to]
  portfolio = portfolio_returns(instruments, from, to, wght, rebalance) %>% `names<-`("Portfolio")
  portfolio$Benchmark = instruments[, benchmark_column]
  portfolio
}

#' Calculates portfolio returns
#'
#' @param df, from, to, weights, rebalance, geometric parameters.
portfolio_returns = function(df, from, to, weights, rebalance, geometric = TRUE) {
  instruments = df %>% as.xts() %>% .[index(.) >= from & index(.) <= to]

  #Create repalace operator
  reb_op = ifelse(rebalance == "Never", NA,
                  ifelse(rebalance == "Annually", "years",
                         ifelse(rebalance == "Quarterly", "quarters",
                                "months")))

  Return.portfolio(instruments, weights = weights, geometric = geometric, rebalance_on = reb_op)
}

#' calculate_optimal_weights
#'
#' @param instruments, bt_df, from, to, scale parameters.
calculate_optimal_weights = function(instruments, bt_df, from, to, scale = 252) {

  instruments = instruments %>% as.xts() %>% .[index(.) >= from & index(.) <= to]
  bt_df = bt_df %>% as.xts() %>% .[index(.) >= from & index(.) <= to]

  #Calculate target risk and return
  target_ret = mean(bt_df$Portfolio) * scale
  target_risk = sd(bt_df$Portfolio) * sqrt(scale)

  # Calculate inputs for optimisation
  mean_ret = apply(instruments, 2, mean) * scale
  cov_matrix = cov(instruments) * scale

  #Find optimal weights
  opt_w_ret = findEfficientFrontier.ReturnALT(mean_ret, cov_matrix, target_ret)
  opt_w_risk = findEfficientFrontier.Risk(mean_ret, cov_matrix, target_risk)

  #Return a dataframe
  data.frame(SimilarReturn = opt_w_ret, SimilarRisk = opt_w_risk, row.names = names(instruments))
}


#' Build optimal portfolios returns
#'
#' @param instruments, from, to, opt_w, port_ret parameters.
calculate_optmised_returns = function(instruments, from, to, opt_w, port_ret) {
  instruments = instruments %>% as.xts() %>% .[index(.) >= from & index(.) <= to]
  port_ret = port_ret %>% as.xts() %>% .[index(.) >= from & index(.) <= to]

  #Same return portfolio
  port_ret$SimilarReturn = portfolio_returns(instruments, from, to, opt_w$SimilarReturn, rebalance = "Never", geometric = FALSE)
  port_ret$SimilarRisk = portfolio_returns(instruments, from, to, opt_w$SimilarRisk, rebalance = "Never", geometric = FALSE)
  port_ret
}

#' Calculates portfolio performance measures
#'
#' @param returns, risk_free, from, to, scale.
calculate_performance_metrics = function(returns, risk_free, from, to, scale = 252) {
  ret_df = returns %>% as.xts() %>% .[index(.) >= from & index(.) <= to]
  rf = risk_free %>% as.xts() %>% .[index(.) >= from & index(.) <= to]

  data.frame() %>%
  # rbind(CAPM.alpha(ret_df, ret_df$Benchmark, Rf = rf) * sqrt(scale)) %>%
  # rbind(CAPM.beta(ret_df, ret_df$Benchmark, Rf = rf)) %>%
  # rbind(CAPM.beta.bull(ret_df, ret_df$Benchmark, Rf = rf)) %>%
  # rbind(CAPM.beta.bear(ret_df, ret_df$Benchmark, Rf = rf)) %>%
  rbind(InformationRatio(ret_df, ret_df$Benchmark, scale = scale)) %>%
      rbind(SharpeRatio.annualized(ret_df, Rf = rf, scale = scale)) %>%
      rbind(SortinoRatio(ret_df, MAR = ret_df$Benchmark)) %>%
  # rbind(CalmarRatio(ret_df, scale=scale)) %>%
  # rbind(SterlingRatio(ret_df, scale=scale, excess = 0.1)) %>%
  rbind(TreynorRatio(ret_df, ret_df$Benchmark, Rf = rf, scale = scale, modified = T) %>%
        `rownames<-`("Modified Treynor Ratio")) %>%
      rbind(Omega(ret_df, ret_df$Benchmark, Rf = rf, method = "simple"))

}

#' Calculates portfolio performance measures
#'
#' @param returns, risk_free, from, to, scale.
calculate_risk_metrics = function(returns, risk_free, from, to, scale = 252) {
  ret_df = returns %>% as.xts() %>% .[index(.) >= from & index(.) <= to]
  rf = risk_free %>% as.xts() %>% .[index(.) >= from & index(.) <= to]

  data.frame() %>%
  # rbind(TotalRisk(ret_df, ret_df$Benchmark, Rf = rf))  %>%
  # rbind(SpecificRisk(ret_df, ret_df$Benchmark, Rf = rf))  %>%
  # rbind(SystematicRisk(ret_df, ret_df$Benchmark)) c
  # rbind(SkewnessKurtosisRatio(ret_df)) %>%
  rbind(maxDrawdown(ret_df)) %>%
      rbind(VaR(ret_df, p = 0.95, method = "modified") %>% `rownames<-`("Modified VaR") * sqrt(scale)) %>%
      rbind(VaR(ret_df, p = 0.95, method = "historical") %>% `rownames<-`("Historical VaR") * sqrt(scale)) %>%
      rbind(ES(ret_df, p = 0.95, method = "modified") %>% `rownames<-`("Modified Expected Shortfall") * sqrt(scale)) %>%
      rbind(ES(ret_df, p = 0.95, method = "historical") %>% `rownames<-`("Historical Expected Shortfall") * sqrt(scale))
}

#' Calculates portfolio return/risk given mean returns of assets and a covariance matrix
#'
#' @param returns, risk_free, from, to, scale.
calcPortPerformance = function(weights, mean_ret, cov_matrix) {
  portRet = t(weights) %*% mean_ret
  portRisk = sqrt(t(weights) %*% (cov_matrix %*% weights))
  return(list(portRet, portRisk))
}

#' Simulates portfolio risk/return given mean returns of assets and a covariance matrix
#'
#' @param returns, risk_free, from, to, scale.
simPortfolios = function(mean_ret, cov_matrix, nsim = 10000) {
  n_assets = length(mean_ret) #Get number of assets

  #Create empty DataFrame
  result = data.frame(Return = rep(NA, nsim), Risk = rep(NA, nsim))

  #Simulate portfolios performance and populate the resulting dataframe
  for (i in 1:nsim) {
    weights = runif(n_assets, 0, 1) #Simulate normal distribution
    weights = weights / sum(weights) #Make sure that weights add up to 1.0

    portRet = calcPortPerformance(weights, mean_ret, cov_matrix)[[1]]
    portRisk = calcPortPerformance(weights, mean_ret, cov_matrix)[[2]]

    result$Return[i] = portRet
    result$Risk[i] = portRisk
  }
  return(result)
}

#' Finds weights of assets on the efficient frontier by target return
#'
#' @param returns, risk_free, from, to, scale.
findEfficientFrontier.Return = function(returns, target_ret, short = FALSE, scale = 252) {
  #Calculate optimal weights
  opt.weights = portfolio.optim(returns, pm = target_ret / scale, shorts = short)$pw

  if (short == FALSE) {
    opt.weights = pmax(opt.weights, 0) #Correct approximation error
    opt.weights = opt.weights / sum(opt.weights)
  }
  opt.weights
}

#' By target risk
#'
#' @param returns, risk_free, from, to, scale.
findEfficientFrontier.Risk = function(mean_ret, cov_matrix, target_risk) {
  obj_func = function(w) {

    #To avoid NA
    if (sum(w) == 0) {
      w = w + 1e-10
    }

    #Balance to one
    w = w / sum(w)

    #Calculate negative return
    neg_ret = -t(w) %*% mean_ret
    p_risk = sqrt(t(w) %*% cov_matrix %*% w)

    (neg_ret + abs(p_risk - target_risk)) #Penalized optimisation
  }

  # Set parameters
  controlDE <- list(reltol = 1e-7, steptol = 100, itermax = 10000, trace = 5000, strategy = 6, c = 0)

  #Long only
  N = length(mean_ret)
  lower = rep(0, N)
  upper = rep(1, N)

  out <- DEoptim(fn = obj_func, lower = lower, upper = upper, control = controlDE)
  opt_w = out$optim$bestmem

  opt_w / sum(opt_w) #Sum up to 1
}

#' By target return ALTERNATIVE
#'
#' @param returns, risk_free, from, to, scale.
findEfficientFrontier.ReturnALT = function(mean_ret, cov_matrix, target_ret) {
  obj_func = function(w) {

    #To avoid NA
    if (sum(w) == 0) {
      w = w + 1e-10
    }

    #Balance to one
    w = w / sum(w)

    #Calculate negative return
    port_ret = t(w) %*% mean_ret
    port_risk = sqrt(t(w) %*% cov_matrix %*% w)

    return(port_risk + abs(port_ret - target_ret) * 10) #Penalized optimisation
  }

  # Set parameters
  controlDE <- list(reltol = 1e-7, steptol = 100, itermax = 10000, trace = 5000, strategy = 6, c = 0)
  #Long only
  N = length(mean_ret)
  lower = rep(0, N)
  upper = rep(1, N)

  out <- DEoptim(fn = obj_func, lower = lower, upper = upper, control = controlDE)

  opt_w = out$optim$bestmem
  opt_w / sum(opt_w) #Sum up to 1
}
