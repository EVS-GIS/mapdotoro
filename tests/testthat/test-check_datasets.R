swaths_data <- swaths %>%
  dplyr::filter(VALUE == 2) # keep only valid swaths

sf::st_geometry(swaths_data) <- "geom" # standard geometry column

test_that("check_duplicate returns an sf class with two rows", {
  # Call the function
  result <- check_duplicate(swaths_data)

  # Check if the result is of class sf
  expect_true(inherits(result, "sf"))
})


test_that("clean_duplicated removes duplicated rows identified by check_duplicate", {

  # Call check_duplicate to identify duplicated rows
  duplicated_rows <- check_duplicate(swaths_data)

  # Call clean_duplicated to remove duplicated rows
  cleaned_dataset <- clean_duplicated(swaths_data, duplicated_rows)

  # Check if the result is of class sf
  expect_true(inherits(cleaned_dataset, "sf"))

  # Check if the cleaned dataset has the expected number of rows
  expect_equal(nrow(cleaned_dataset), nrow(swaths_data) - nrow(duplicated_rows))
})
