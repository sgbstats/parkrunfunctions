test_that("get_all_runs returns correct data for Sebastian BATE", {
  skip_on_cran()

  res <- get_all_runs("493595")

  expect_equal(res$name, "Sebastian BATE")

  last_row <- tail(res$results, 1)
  expect_equal(last_row$run_number, 86)
  expect_equal(last_row$short, "nonsuch")
})

test_that("get_all_runs returns correct data for Sebastian BATE", {
  skip_on_cran()

  res <- get_all_runs("A493595")

  expect_equal(res$name, "Sebastian BATE")

  last_row <- tail(res$results, 1)
  expect_equal(last_row$run_number, 86)
  expect_equal(last_row$short, "nonsuch")
})