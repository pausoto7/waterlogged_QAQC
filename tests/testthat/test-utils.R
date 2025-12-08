# Test utility functions

test_that("normalize_string works correctly", {
  expect_equal(normalize_string("Water Level"), "waterlevel")
  expect_equal(normalize_string("  DISSOLVED OXYGEN  "), "dissolvedoxygen")
  expect_equal(normalize_string("Air Temp"), "airtemp")
  expect_equal(normalize_string(c("DO", "WL")), c("do", "wl"))
})

test_that("convert_F_to_C works correctly", {
  expect_equal(convert_F_to_C(32), 0)
  expect_equal(convert_F_to_C(212), 100)
  expect_equal(round(convert_F_to_C(68), 1), 20)
  expect_equal(convert_F_to_C(c(32, 212)), c(0, 100))
})

test_that("escape_regex escapes special characters", {
  expect_equal(escape_regex("test.csv"), "test\\.csv")
  expect_equal(escape_regex("file[1-3].txt"), "file\\[1\\-3\\]\\.txt")
  expect_true(grepl(escape_regex("test.csv"), "test.csv", perl = TRUE))
})

test_that("find_na_runs identifies missing value runs", {
  # Create test data with NA runs
  test_data <- data.frame(
    timestamp = seq(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
                    by = "15 min", length.out = 100),
    airpress_kPa = c(rep(100, 20), rep(NA, 10), rep(101, 30), rep(NA, 5), rep(102, 35))
  )

  gaps <- find_na_runs(test_data, value_col = "airpress_kPa", add_step = FALSE)

  expect_s3_class(gaps, "data.frame")
  expect_equal(nrow(gaps), 2)  # Two NA runs
  expect_equal(gaps$n_missing, c(10, 5))
  expect_equal(names(gaps), c("gap_id", "gap_start", "gap_end", "n_missing", "duration"))
})

test_that("find_na_runs handles no missing values", {
  test_data <- data.frame(
    timestamp = seq(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
                    by = "15 min", length.out = 50),
    airpress_kPa = rep(100, 50)
  )

  gaps <- find_na_runs(test_data, value_col = "airpress_kPa")

  expect_equal(nrow(gaps), 0)
  expect_equal(names(gaps), c("gap_id", "gap_start", "gap_end", "n_missing", "duration"))
})

test_that("find_na_runs handles all missing values", {
  test_data <- data.frame(
    timestamp = seq(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
                    by = "15 min", length.out = 20),
    airpress_kPa = rep(NA_real_, 20)
  )

  gaps <- find_na_runs(test_data, value_col = "airpress_kPa")

  expect_equal(nrow(gaps), 1)
  expect_equal(gaps$n_missing, 20)
})
