# Test dissolved oxygen functions

# Helper to create test DO data
create_test_do_data <- function(n = 100) {
  data.frame(
    site_station_code = "TEST_DO",
    timestamp = seq(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
                    by = "15 min", length.out = n),
    do_mgl = runif(n, 7, 12),
    watertemp_C = runif(n, 5, 20),
    airtemp_C = runif(n, 3, 22),
    airpress_kPa = runif(n, 98, 103),
    sn = "12345678",
    metric = "dissolvedoxygen"
  )
}

test_that("convert_do_mgl_percsat creates do_percsat column", {
  do_data <- create_test_do_data()
  output_dir <- tempdir()

  result <- convert_do_mgl_percsat(
    do_data,
    output_dir = output_dir,
    version_label = "v0.3"
  )

  # Check that do_percsat column was added
  expect_true("do_percsat" %in% names(result))
  expect_type(result$do_percsat, "double")

  # Check that values are reasonable (should be around 80-120% at typical conditions)
  valid_percsat <- result$do_percsat[!is.na(result$do_percsat)]
  expect_gt(length(valid_percsat), 0)
  expect_true(all(valid_percsat > 0 & valid_percsat < 200))
})

test_that("convert_do_mgl_percsat handles missing airpress_kPa", {
  do_data <- create_test_do_data()
  do_data$airpress_kPa <- NULL

  expect_error(
    convert_do_mgl_percsat(do_data, output_dir = tempdir()),
    "airpress_kPa"
  )
})

test_that("convert_do_mgl_percsat writes v0.3 files", {
  do_data <- create_test_do_data()
  output_dir <- file.path(tempdir(), "test_do_convert")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  result <- convert_do_mgl_percsat(
    do_data,
    output_dir = output_dir,
    version_label = "v0.3"
  )

  # Check that files were created
  v03_files <- list.files(output_dir, pattern = "v0\\.3\\.csv$", recursive = TRUE)
  expect_gt(length(v03_files), 0)

  # Clean up
  unlink(output_dir, recursive = TRUE)
})

test_that("dissox_qaqc creates adjusted columns", {
  do_data <- create_test_do_data()
  # Add do_percsat column
  do_data$do_percsat <- runif(nrow(do_data), 80, 110)

  log_dir <- tempdir()

  result <- dissox_qaqc(
    input_data = do_data,
    select_station = "TEST_DO",
    log_root = log_dir
  )

  # Check adjusted columns exist
  expect_true("do_mgl_adj" %in% names(result))
  expect_true("do_percsat_adj" %in% names(result))
  expect_true("watertemp_C_do_adj" %in% names(result))

  # Check flag columns exist
  expect_true("flag_do_dry" %in% names(result))
  expect_true("temp_flag_ice" %in% names(result))
})

test_that("dissox_qaqc flags negative DO values", {
  do_data <- create_test_do_data(50)
  do_data$do_percsat <- runif(nrow(do_data), 80, 110)

  # Add some negative DO values
  do_data$do_mgl[1:5] <- c(-2, -1.5, -0.5, -0.1, 0.5)

  result <- dissox_qaqc(
    input_data = do_data,
    select_station = "TEST_DO",
    log_root = tempdir()
  )

  # Values < -1 should be removed (NA)
  expect_true(is.na(result$do_mgl_adj[1]))
  expect_true(is.na(result$do_mgl_adj[2]))

  # Values between -1 and 0 should be corrected to 0
  expect_equal(result$do_mgl_adj[3], 0)
  expect_equal(result$do_mgl_adj[4], 0)

  # Positive values should be unchanged
  expect_equal(result$do_mgl_adj[5], 0.5)

  # Check flags exist
  expect_true(result$do_neg_lt_minus1[1])
  expect_true(result$do_neg_between[3])
})

test_that("dissox_qaqc flags high DO outliers", {
  do_data <- create_test_do_data(50)
  do_data$do_percsat <- runif(nrow(do_data), 80, 110)

  # Add high outliers (threshold is hardcoded at 21 mg/L)
  do_data$do_mgl[1:3] <- c(25, 30, 22)

  result <- dissox_qaqc(
    input_data = do_data,
    select_station = "TEST_DO",
    log_root = tempdir()
  )

  # High values should be flagged
  expect_true(result$do_high[1])
  expect_true(result$do_high[2])
  expect_true(result$do_high[3])

  # Values > 21 should be NA
  expect_true(is.na(result$do_mgl_adj[1]))
  expect_true(is.na(result$do_mgl_adj[2]))
  expect_true(is.na(result$do_mgl_adj[3]))
})

test_that("dissox_qaqc detects logger error codes", {
  do_data <- create_test_do_data(50)
  do_data$do_percsat <- runif(nrow(do_data), 80, 110)

  # Add logger error code
  do_data$do_mgl[1:5] <- -888.88

  result <- dissox_qaqc(
    input_data = do_data,
    select_station = "TEST_DO",
    log_root = tempdir()
  )

  # Error codes should be flagged and removed
  expect_true(result$do_error[1])
  expect_true(all(is.na(result$do_mgl_adj[1:5])))
})

test_that("dissox_qaqc detects ice conditions", {
  do_data <- create_test_do_data(50)
  do_data$do_percsat <- runif(nrow(do_data), 80, 110)

  # Set water temperature to freezing (threshold is hardcoded at 0.3\u00B0C)
  do_data$watertemp_C[10:15] <- 0.1

  result <- dissox_qaqc(
    input_data = do_data,
    select_station = "TEST_DO",
    log_root = tempdir()
  )

  # Ice should be flagged (temp < 0.3\u00B0C)
  expect_true(any(result$temp_flag_ice[10:15]))
})

test_that("dissox_qaqc detects dry sensor conditions", {
  do_data <- create_test_do_data(50)
  do_data$do_percsat <- runif(nrow(do_data), 80, 110)

  # Create dry sensor conditions:
  # - High DO % saturation (>= 100%)
  # - Air and water temp close together (within 2\u00B0C)
  # Need 12+ consecutive points to trigger detection (hardcoded threshold)
  do_data$do_percsat[20:32] <- 105
  do_data$watertemp_C[20:32] <- 15
  do_data$airtemp_C[20:32] <- 15.5

  result <- dissox_qaqc(
    input_data = do_data,
    select_station = "TEST_DO",
    log_root = tempdir()
  )

  # Should detect dry conditions with 12-point window
  expect_true(any(result$flag_do_dry[20:32]))
})

test_that("dissox_qaqc writes log files", {
  do_data <- create_test_do_data(30)
  do_data$do_percsat <- runif(nrow(do_data), 80, 110)

  # Add some issues to log
  do_data$do_mgl[1:3] <- c(-2, 25, -888.88)

  log_dir <- file.path(tempdir(), "test_do_logs")
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(log_dir, "logs"), showWarnings = FALSE, recursive = TRUE)

  result <- dissox_qaqc(
    input_data = do_data,
    select_station = "TEST_DO",
    log_root = log_dir
  )

  # Check that log file was created
  log_file <- file.path(log_dir, "logs", "TEST_DO_DO_qaqc_log.csv")
  expect_true(file.exists(log_file))

  # Read log and verify contents
  log_data <- read.csv(log_file, stringsAsFactors = FALSE)
  expect_gt(nrow(log_data), 0)
  expect_true("station" %in% names(log_data))
  expect_true("action" %in% names(log_data))

  # Clean up
  unlink(log_dir, recursive = TRUE)
})

test_that("dissox_qaqc handles edge cases", {
  # Empty data
  expect_error(
    dissox_qaqc(
      input_data = data.frame(),
      select_station = "TEST",
      log_root = tempdir()
    ),
    "no rows"
  )

  # Missing required columns
  bad_data <- data.frame(
    site_station_code = "TEST",
    timestamp = Sys.time()
  )

  expect_error(
    dissox_qaqc(
      input_data = bad_data,
      select_station = "TEST",
      log_root = tempdir()
    ),
    "missing required column"
  )
})
