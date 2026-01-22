test_that("get_all_runs", {
  skip_on_cran()

  res <- get_all_runs("493595")

  expect_equal(res$name, "Sebastian BATE")

  last_row <- tail(res$results, 1)
  expect_equal(last_row$event_no, 86)
  expect_equal(last_row$short, "nonsuch")
  expect_equal(class(res[["results"]][["time"]]), "character")
  expect_equal(class(res[["results"]][["event_date"]]), "character")

  res <- get_all_runs("A493595", as_hms = TRUE, as_Date = TRUE)
  expect_equal(class(res[["results"]][["time"]])[1], "hms")
  expect_equal(class(res[["results"]][["event_date"]]), "Date")

  expect_equal(res$name, "Sebastian BATE")
  last_row <- tail(res$results, 1)
  expect_equal(last_row$event_no, 86)
  expect_equal(last_row$short, "nonsuch")

  res <- get_all_runs(url = "https://www.parkrun.org.uk/parkrunner/493595/")

  expect_equal(res$name, "Sebastian BATE")

  last_row <- tail(res$results, 1)
  expect_equal(last_row$event_no, 86)
  expect_equal(last_row$short, "nonsuch")

  res <- get_all_runs(
    id = 493595,
    url = "https://www.parkrun.org.uk/parkrunner/4087050/"
  )

  expect_equal(res$name, "Sebastian BATE")

  last_row <- tail(res$results, 1)
  expect_equal(last_row$event_no, 86)
  expect_equal(last_row$short, "nonsuch")
})
