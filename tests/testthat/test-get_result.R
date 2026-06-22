test_that("get_result", {
  # Define the URL for the test
  url <- "https://www.parkrun.org.uk/wythenshawe/results/647/"

  result <- get_result(url)

  testthat::expect_s3_class(result, "parkrun_results")

  testthat::expect_equal(
    result[["results"]] |>
      dplyr::filter(id == "493595", parkrunner == "Seb BATE") |>
      nrow(),
    1
  )

  testthat::expect_equal(
    result[["volunteers"]] |>
      dplyr::filter(id == "493595", parkrunner == "Seb BATE") |>
      nrow(),
    1
  )

  testthat::expect_equal(result[["date"]], "2026-01-03")
  testthat::expect_equal(class(result[["results"]][["time"]]), "character")
  testthat::expect_equal(class(result[["date"]]), "character")

  testthat::expect_equal(
    (result[["results"]] |>
      dplyr::filter(id == "493595", parkrunner == "Seb BATE"))$time,
    "24:24"
  )
  testthat::expect_equal(
    (result[["results"]] |>
      dplyr::filter(id == "493595", parkrunner == "Seb BATE"))$ag,
    52.94
  )
  Sys.sleep(23)

  #event_no
  result <- get_result(event = "wythenshawe", event_no = 647, as_hms = TRUE)

  testthat::expect_s3_class(result, "parkrun_results")

  testthat::expect_equal(
    result[["results"]] |>
      dplyr::filter(id == "493595", parkrunner == "Seb BATE") |>
      nrow(),
    1
  )

  testthat::expect_false("finishes" %in% names(result[["results"]]))
  Sys.sleep(23)

  expect_no_error(
    result <- get_result(
      event = "wythenshawe",
      event_date = "2026-01-03",
      as_hms = TRUE
    )
  )
  testthat::expect_s3_class(result, "parkrun_results")

  testthat::expect_equal(
    result[["results"]] |>
      dplyr::filter(id == "493595", parkrunner == "Seb BATE") |>
      nrow(),
    1
  )

  testthat::expect_false("finishes" %in% names(result[["results"]]))

  Sys.sleep(23)

  result <- get_result(
    event = "wythenshawe",
    event_date = "03/01/2026",
    date_fmt = "%d/%m/%Y",
    as_hms = TRUE
  )

  testthat::expect_s3_class(result, "parkrun_results")

  testthat::expect_equal(
    result[["results"]] |>
      dplyr::filter(id == "493595", parkrunner == "Seb BATE") |>
      nrow(),
    1
  )

  testthat::expect_false("finishes" %in% names(result[["results"]]))

  Sys.sleep(23)

  #testing that no event date is provided and event_no is provided, the function will choose the event_no first
  result <- get_result(
    event = "wythenshawe",
    as_hms = TRUE
  )

  testthat::expect_s3_class(result, "parkrun_results")

  Sys.sleep(23)
  # chooses event no first
  expect_no_error(
    result <- get_result(
      event = "wythenshawe",
      event_no = 647,
      event_date = "2026-01-10",
      as_hms = TRUE
    )
  )

  testthat::expect_equal(
    result[["results"]] |>
      dplyr::filter(id == "493595", parkrunner == "Seb BATE") |>
      nrow(),
    1
  )
  Sys.sleep(23)
  # error on date that doesn't exist

  testthat::expect_error(
    get_result(
      event = "wythenshawe",
      event_date = "2026-01-09",
      as_hms = TRUE
    )
  )

  testthat::expect_error(
    result <- get_result(
      event = "wythenshawe",
      event_date = "01/03/2026",
      date_fmt = NULL,
      as_hms = TRUE
    )
  )
  testthat::expect_no_error(
    result <- get_result(
      event = "wythenshawe",
      event_date = "01/03/2026",
      date_fmt = "%m/%d/%Y",
      as_hms = TRUE
    )
  )
  testthat::expect_error(
    result <- get_result(
      event = "wythenshawe",
      event_date = "01/03/2026",
      date_fmt = "%d/%m/%Y",
      as_hms = TRUE
    )
  )
  testthat::expect_error(
    result <- get_result(
      event = "wythenshawe",
      event_date = "13/03/2026",
      date_fmt = "%m/%d/%Y",
      as_hms = TRUE
    )
  )
  #extra data
  result_extra <- get_result(url, extra_data = TRUE)

  testthat::expect_true(
    result_extra[["results"]] |>
      dplyr::filter(parkrunner == "Seb BATE") |>
      dplyr::pull("finishes") >=
      233
  )

  testthat::expect_true("finishes" %in% names(result_extra[["results"]]))

  testthat::expect_equal(class(result[["results"]][["time"]])[1], "hms")
  testthat::expect_equal(
    result[["volunteers"]] |>
      dplyr::filter(id == "493595", parkrunner == "Seb BATE") |>
      nrow(),
    1
  )
  Sys.sleep(23)
  result <- get_result(
    url,
    event = "wythenshawe",
    event_no = 648,
    as_Date = TRUE
  )

  testthat::expect_s3_class(result, "parkrun_results")
  testthat::expect_equal(
    result[["results"]] |>
      dplyr::filter(id == "493595", parkrunner == "Seb BATE") |>
      nrow(),
    1
  )

  testthat::expect_equal(
    result[["results"]] |>
      dplyr::filter(id == "493595", parkrunner == "Seb BATE") |>
      nrow(),
    1
  )

  testthat::expect_equal(
    result[["volunteers"]] |>
      dplyr::filter(id == "493595", parkrunner == "Seb BATE") |>
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
  Sys.sleep(23)

  testthat::expect_equal(
    get_result(
      url = "https://www.parkrun.pl/krakow/results/601/"
    )$results$ag[110],
    47.47
  )
  Sys.sleep(23)
  testthat::expect_no_warning(
    get_result(
      url = "https://www.parkrun.dk/faelledparken/results/677/"
    )
  )
  Sys.sleep(23)
  testthat::expect_error(
    get_result(
      event = "faelledparken",
      event_no = 677
    )
  )
  Sys.sleep(23)
  testthat::expect_no_error(
    get_result(
      event = "faelledparken",
      event_no = 677,
      domain = "parkrun.dk"
    )
  )
  Sys.sleep(23)
  testthat::expect_no_warning(
    get_result(
      url = "https://www.parkrun.jp/chuokoen/results/150"
    )
  )
})
