
test_that("get_result", {
  # Define the URL for the test
  url <- "https://www.parkrun.org.uk/wythenshawe/results/647/"

  result <- get_result(url)

  testthat::expect_s3_class(result, "parkrun_results")

  testthat::expect_true("493595" %in% result[["volunteers"]])
  testthat::expect_true("493595" %in% result[["results"]][["id"]])

  result <- get_result(event="wythenshawe", event_no=647)

  testthat::expect_s3_class(result, "parkrun_results")

  testthat::expect_true("493595" %in% result[["volunteers"]])
  testthat::expect_true("493595" %in% result[["results"]][["id"]])

  testthat::expect_error(
    get_result(),
    "Either 'url' or both 'event' and 'event_no' must be provided."
    )
   
  result <- get_result(url, event="wythenshawe", event_no=648)
  testthat::expect_true("493595" %in% result[["volunteers"]])
  testthat::expect_true("493595" %in% result[["results"]][["id"]])
})
