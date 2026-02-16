test_that("get_result", {
  # Define the URL for the test
  url <- "https://www.parkrun.org.uk/wythenshawe/results/647/"

  result <- get_result(url)

  testthat::expect_s3_class(result, "parkrun_results")

  testthat::expect_equal(
    result[["results"]] |>
      dplyr::filter(id == "493595", parkrunner == "Sebastian BATE") |>
      nrow(),
    1
  )

  testthat::expect_equal(
    result[["volunteers"]] |>
      dplyr::filter(id == "493595", parkrunner == "Sebastian BATE") |>
      nrow(),
    1
  )

  testthat::expect_equal(result[["date"]], "2026-01-03")
  testthat::expect_equal(class(result[["results"]][["time"]]), "character")
  testthat::expect_equal(class(result[["date"]]), "character")

  testthat::expect_equal(
    (result[["results"]] |>
      dplyr::filter(id == "493595", parkrunner == "Sebastian BATE"))$time,
    "24:24"
  )
  testthat::expect_equal(
    (result[["results"]] |>
      dplyr::filter(id == "493595", parkrunner == "Sebastian BATE"))$ag,
    52.94
  )

  result <- get_result(event = "wythenshawe", event_no = 647, as_hms = TRUE)

  testthat::expect_s3_class(result, "parkrun_results")

  testthat::expect_equal(
    result[["results"]] |>
      dplyr::filter(id == "493595", parkrunner == "Sebastian BATE") |>
      nrow(),
    1
  )
  testthat::expect_equal(class(result[["results"]][["time"]])[1], "hms")
  testthat::expect_equal(
    result[["volunteers"]] |>
      dplyr::filter(id == "493595", parkrunner == "Sebastian BATE") |>
      nrow(),
    1
  )

  result <- get_result(
    url,
    event = "wythenshawe",
    event_no = 648,
    as_Date = TRUE
  )

  testthat::expect_s3_class(result, "parkrun_results")
  testthat::expect_equal(
    result[["results"]] |>
      dplyr::filter(id == "493595", parkrunner == "Sebastian BATE") |>
      nrow(),
    1
  )

  testthat::expect_equal(
    result[["volunteers"]] |>
      dplyr::filter(id == "493595", parkrunner == "Sebastian BATE") |>
      nrow(),
    1
  )
  testthat::expect_equal(class(result[["date"]]), "Date")
  testthat::expect_error(
    get_result(),
    "Either 'url' or both 'event' and 'event_no' must be provided."
  )
})

test_that("get_result foreign", {
  testthat::expect_no_warning(
    get_result(
      url = "https://www.parkrun.pl/krakow/results/601/"
    )
  )

  testthat::expect_equal(
    get_result(
      url = "https://www.parkrun.pl/krakow/results/601/"
    )$results$ag[110],
    47.47
  )

  testthat::expect_no_warning(
    get_result(
      url = "https://www.parkrun.dk/faelledparken/results/677/"
    )
  )

  testthat::expect_error(
    get_result(
      event = "faelledparken",
      event_no = 677
    )
  )

  testthat::expect_no_error(
    get_result(
      event = "faelledparken",
      event_no = 677,
      domain = "parkrun.dk"
    )
  )

  testthat::expect_no_warning(
    get_result(
      url = "https://www.parkrun.jp/chuokoen/results/150"
    )
  )
})
