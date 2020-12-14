#' @import dplyr
#' @importFrom xml2 read_html
#' @importFrom xts as.xts
#' @importFrom BatchGetSymbols BatchGetSymbols GetSP500Stocks
#' @importFrom tidyquant tq_get
#' @importFrom tidyr drop_na spread
NULL

#' Load default list of instruments
#'
#' @param cache_folder folder for saving the downloaded data.
load_instruments = function(cache_folder) {
  c("^GSPC", "^DJI", "^IXIC", "^FTSE", "^N100", "^TNX") %>%
  append(GetSP500Stocks(do.cache = TRUE, cache.folder = cache_folder)$Tickers)
}

#' Load tickers from yahoo finance
#'
#' @param symbols, start_date, end_date, cache_folder tiker symbols and period.
load_tickers = function(symbols, start_date, end_date, cache_folder) {

  TICKERS = BatchGetSymbols(
    tickers = symbols,
    first.date = start_date,
    last.date = end_date,
    freq.data = "daily",
    type.return = "log",
    do.complete.data = FALSE,
    do.fill.missing.prices = TRUE,
    do.cache = TRUE,
    do.parallel = FALSE,
    cache.folder = cache_folder)

  df = TICKERS$df.tickers %>%
            select(c(ticker, ref.date, ret.adjusted.prices)) %>%
            spread(ticker, ret.adjusted.prices) %>%
            drop_na() %>%
            data.frame(row.names = .$ref.date) %>%
            select(-ref.date)
}

#' Load economic datasets from FRED website
#'
#' @param symbols, start_date, end_date Fred dataset and period.
load_fred_data = function(symbols, start_date, end_date) {
  tq_get(symbols, get = "economic.data", from = start_date, to = end_date) %>%
    spread(symbol, price) %>%
    drop_na() %>%
    data.frame(row.names = .$date) %>%
    select(-date)
}

#' Convert FRED rates to daily interest rates
#'
#' @param a FRED interest rate.
fred_to_daily_rates = function(a) {(a / 100 + 1) ^ (1 / 360) - 1 }
