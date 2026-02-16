# R
test_that("get_event_history returns expected values for bushy event_no 1000", {
  x = get_event_history(event = "bushy")
  expect_s3_class(x, "parkrun_event_history")

  df <- x[["history"]]
  expect_true("event_no" %in% names(df))
  row <- df[df$event_no == 1000, , drop = FALSE]
  expect_true(nrow(row) == 1)

  expect_equal(row$finishers[[1]], 6204)
  expect_equal(row$volunteers[[1]], 136)
  expect_equal(as.Date(row$date[[1]]), as.Date("2024-08-31"))

  #checking NAs
  last_row <- df[nrow(df), , drop = FALSE]
  expect_true(is.na(last_row$volunteers[[1]]))

  df <- get_event_history(
    "https://www.parkrun.org.uk/bushy/results/eventhistory/"
  )[["history"]]
  expect_true("event_no" %in% names(df))
  row <- df[df$event_no == 1000, , drop = FALSE]
  expect_true(nrow(row) == 1)

  expect_equal(row$finishers[[1]], 6204)
  expect_equal(row$volunteers[[1]], 136)
  expect_equal(as.Date(row$date[[1]]), as.Date("2024-08-31"))
})

test_that("get_event_history foreign", {
  df <- get_event_history(
    "https://www.parkrun.com.de/alstervorland/results/eventhistory/"
  )[["history"]]
  expect_true("event_no" %in% names(df))
  row <- df[nrow(df), , drop = FALSE]
  expect_true(nrow(row) == 1)

  expect_equal(row$finishers[[1]], 102)
  expect_equal(row$volunteers[[1]], 11)
  expect_equal(as.Date(row$date[[1]]), as.Date("2019-01-12"))
  expect_error(get_event_history(event = "alstervorland"))

  df <- get_event_history(
    event = "alstervorland",
    domain = "parkrun.com.de"
  )[["history"]]
  expect_true("event_no" %in% names(df))
  row <- df[nrow(df), , drop = FALSE]
  expect_true(nrow(row) == 1)

  expect_equal(row$finishers[[1]], 102)
  expect_equal(row$volunteers[[1]], 11)
  expect_equal(as.Date(row$date[[1]]), as.Date("2019-01-12"))

  expect_no_error(
    get_event_history("https://www.parkrun.jp/chuokoen/results/eventhistory/")
  )
})
