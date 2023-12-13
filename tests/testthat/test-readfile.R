test_that("read_bassin returns a data frame with 7 lines", {
  bassin <- read_bassin()

  expect_equal(nrow(bassin), 7, info = "Number of rows is 7")
})
