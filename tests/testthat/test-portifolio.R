testPath = getwd()

if (stringr::str_detect(getwd(), ".Rcheck")) {
  setwd(glue(testPath, "/../../00_pkg_src/shinyfinance"))
} else {
  setwd(glue(testPath, "/../../"))
}

INSTRUMENTS = load_tickers(toupper(INITIAL_SELECTION), START_DATE, END_DATE, CACHE_FOLDER) %>% xts::as.xts()
setwd(testPath)

instrument_names = INSTRUMENTS %>% colnames()
ni = length(instrument_names)
EQUAL_WEIGHTS = rep(1 / ni, ni)
names(EQUAL_WEIGHTS) = instrument_names


test_that("portfolio benchmark is crerated", {

  rebalance = "Never"

  benchmarked = benchmark_portfolio(
    INSTRUMENTS,
    EQUAL_WEIGHTS,
    rebalance,
    gsub("\\^", "X.", INITIAL_SELECTION[1]),
    as.Date(START_DATE),
    as.Date(END_DATE))

  expect_named(benchmarked, c("Portfolio", "Benchmark"))
})
