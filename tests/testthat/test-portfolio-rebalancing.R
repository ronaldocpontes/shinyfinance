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

FIRST = names(EQUAL_WEIGHTS)[1]
LAST = names(EQUAL_WEIGHTS)[length(EQUAL_WEIGHTS)]

test_that("EQUAL_WEIGHTS works", {

  expect_true(all(EQUAL_WEIGHTS == EQUAL_WEIGHTS[1]))
  expect_named(EQUAL_WEIGHTS, colnames(INSTRUMENTS))
  expect_equal(sum(EQUAL_WEIGHTS), 1)

})

test_that("rebalancing works", {

  rebalanced = rebalance(EQUAL_WEIGHTS, 1, FIRST)
  expect_true(rebalanced[1] == 0.9999)
  expect_equal(sum(rebalanced), 1)
  rbt = tail(rebalanced, length(rebalanced) - 1)
  expect_true(all(rbt == rbt[1]))

  rebalanced = rebalance(EQUAL_WEIGHTS, 0.1, LAST)
  expect_equal(sum(rebalanced), 1)
  rbt = head(rebalanced, length(rebalanced) - 1)
  expect_true(all(rbt == rbt[1]))

  expect_error(rebalance(EQUAL_WEIGHTS, 2, LAST), "Error in change: value is greater than 1")
  expect_error(rebalance(EQUAL_WEIGHTS, .1, "Non existent Key"), "subscript out of bounds")

})
