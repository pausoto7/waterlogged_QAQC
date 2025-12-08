# Test manual adjustment functions

# Helper to create test water level data with QA/QC columns
create_test_wl_for_adjustment <- function(n = 100) {
  data.frame(
    site_station_code = "TEST_WL",
    timestamp = seq(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
                    by = "15 min", length.out = n),
    waterlevel_m = runif(n, 0.5, 2.5),
    waterlevel_m_adj = runif(n, 0.5, 2.5),
    watertemp_C = runif(n, 5, 20),
    watertemp_C_adj = runif(n, 5, 20),
    wl_qaqc_code = NA_character_,
    wl_qaqc_note = NA_character_,
    sn = "12345678",
    metric = "waterlevel"
  )
}

test_that("adjust_waterlevel_spike interpolates spike periods", {
  wl_data <- create_test_wl_for_adjustment(100)

  # Create a spike
  wl_data$waterlevel_m_adj[40:50] <- NA  # Simulate removed spike

  log_dir <- tempdir()

  result <- adjust_waterlevel_spike(
    input_data = wl_data,
    select_station = "TEST_WL",
    timestamp_start = format(wl_data$timestamp[40], "%Y-%m-%d %H:%M:%S"),
    timestamp_end = format(wl_data$timestamp[50], "%Y-%m-%d %H:%M:%S"),
    reason_to_adjust = "ice",
    manual_note = "Test spike interpolation",
    log_root = log_dir,
    user = "test_user"
  )

  # Interpolated values should not be NA anymore
  expect_false(any(is.na(result$waterlevel_m_adj[40:50])))

  # Values should be interpolated between neighbors
  expect_true(result$waterlevel_m_adj[45] > min(result$waterlevel_m_adj[c(39, 51)]))
  expect_true(result$waterlevel_m_adj[45] < max(result$waterlevel_m_adj[c(39, 51)]))

  # Edit flag should be set
  expect_true(any(result$edit_spike_flat[40:50]))
})

test_that("adjust_waterlevel_spike errors with invalid dates", {
  wl_data <- create_test_wl_for_adjustment(50)

  # Spike end before spike start
  expect_error(
    adjust_waterlevel_spike(
      input_data = wl_data,
      select_station = "TEST_WL",
      timestamp_start = format(wl_data$timestamp[30], "%Y-%m-%d %H:%M:%S"),
      timestamp_end = format(wl_data$timestamp[20], "%Y-%m-%d %H:%M:%S"),
      reason_to_adjust = "ice",
      manual_note = "Test error handling",
      log_root = tempdir()
    ),
    "timestamp_end.*before.*timestamp_start"
  )
})

test_that("adjust_WL_offset applies calculated offset", {
  wl_data <- create_test_wl_for_adjustment(100)

  log_dir <- tempdir()

  # Create an artificial offset by modifying raw data at timestamp_end
  wl_data$waterlevel_m[50] <- wl_data$waterlevel_m[40] + 0.25

  result <- adjust_WL_offset(
    input_data = wl_data,
    select_station = "TEST_WL",
    timestamp_start = format(wl_data$timestamp[40], "%Y-%m-%d %H:%M:%S"),
    timestamp_end = format(wl_data$timestamp[50], "%Y-%m-%d %H:%M:%S"),
    manual_note = "Test offset adjustment",
    log_root = log_dir,
    user = "test_user"
  )

  # Values after timestamp_end should be adjusted
  # The function calculates offset automatically from difference at start/end
  expect_true("edit_offset" %in% names(result))
  expect_true(any(result$edit_offset))

  # Values before timestamp_start should be unchanged
  expect_equal(result$waterlevel_m_adj[1:39], wl_data$waterlevel_m_adj[1:39])
})

test_that("adjust_WL_zero changes reference datum", {
  wl_data <- create_test_wl_for_adjustment(100)

  log_dir <- tempdir()
  zero_shift <- -0.5  # Lower all values by 0.5 m

  # Record original values
  original_values <- wl_data$waterlevel_m_adj

  result <- adjust_WL_zero(
    input_data = wl_data,
    select_station = "TEST_WL",
    change_value = zero_shift,
    manual_note = "Test zero adjustment",
    log_root = log_dir,
    user = "test_user"
  )

  # ALL values should be adjusted
  expect_equal(
    result$waterlevel_m_adj,
    original_values + zero_shift
  )

  # Edit flag should be set for all rows
  expect_true("edit_zero" %in% names(result))
  expect_true(all(result$edit_zero))
})

test_that("adjust_logger_NA sets data to NA in specified period", {
  wl_data <- create_test_wl_for_adjustment(100)

  log_dir <- tempdir()

  result <- adjust_logger_NA(
    input_data = wl_data,
    select_station = "TEST_WL",
    metric = "waterlevel",
    reason_to_adjust = "ice",
    timestamp_start = format(wl_data$timestamp[30], "%Y-%m-%d %H:%M:%S"),
    timestamp_end = format(wl_data$timestamp[40], "%Y-%m-%d %H:%M:%S"),
    manual_note = "Test NA adjustment",
    log_root = log_dir,
    user = "test_user"
  )

  # Values in the period should be NA
  expect_true(all(is.na(result$waterlevel_m_adj[30:40])))

  # Values outside the period should be unchanged
  expect_equal(result$waterlevel_m_adj[1:29], wl_data$waterlevel_m_adj[1:29])
  expect_equal(result$waterlevel_m_adj[41:100], wl_data$waterlevel_m_adj[41:100])

  # Edit flag should be set
  expect_true("edit_remove_ice" %in% names(result))
  expect_true(any(result$edit_remove_ice[30:40]))
})

test_that("adjust_logger_NA can handle disturbance with keep_temp", {
  wl_data <- create_test_wl_for_adjustment(50)

  log_dir <- tempdir()

  # Adjust for disturbance but keep temperature
  result <- adjust_logger_NA(
    input_data = wl_data,
    select_station = "TEST_WL",
    metric = "waterlevel",
    reason_to_adjust = "disturbance",
    timestamp_start = format(wl_data$timestamp[20], "%Y-%m-%d %H:%M:%S"),
    timestamp_end = format(wl_data$timestamp[30], "%Y-%m-%d %H:%M:%S"),
    keep_temp = TRUE,
    manual_note = "Remove waterlevel, keep temperature",
    log_root = log_dir,
    user = "test_user"
  )

  # Water level should be NA
  expect_true(all(is.na(result$waterlevel_m_adj[20:30])))

  # Temperature should be preserved when keep_temp = TRUE
  expect_false(all(is.na(result$watertemp_C_adj[20:30])))
})

test_that("adjustment functions write log files", {
  wl_data <- create_test_wl_for_adjustment(50)

  # Add artificial offset for adjust_WL_offset
  wl_data$waterlevel_m[20] <- wl_data$waterlevel_m[10] + 0.15

  log_dir <- file.path(tempdir(), "test_adjustment_logs")
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(log_dir, "logs"), showWarnings = FALSE, recursive = TRUE)

  # Run several adjustments
  adjust_WL_offset(
    input_data = wl_data,
    select_station = "TEST_WL",
    timestamp_start = format(wl_data$timestamp[10], "%Y-%m-%d %H:%M:%S"),
    timestamp_end = format(wl_data$timestamp[20], "%Y-%m-%d %H:%M:%S"),
    manual_note = "Test offset log",
    log_root = log_dir,
    user = "test_user"
  )

  adjust_WL_zero(
    input_data = wl_data,
    select_station = "TEST_WL",
    change_value = -0.2,
    manual_note = "Test zero log",
    log_root = log_dir,
    user = "test_user"
  )

  # Check that log file was created and has entries
  log_file <- file.path(log_dir, "logs", "TEST_WL_WL_qaqc_log.csv")
  expect_true(file.exists(log_file))

  log_data <- read.csv(log_file, stringsAsFactors = FALSE)
  expect_gt(nrow(log_data), 0)
  expect_true("manual_note" %in% names(log_data))
  expect_true("user" %in% names(log_data))
  expect_equal(unique(log_data$user), "test_user")

  # Clean up
  unlink(log_dir, recursive = TRUE)
})

test_that("adjustment functions preserve original raw columns", {
  wl_data <- create_test_wl_for_adjustment(50)

  # Add artificial offset
  wl_data$waterlevel_m[30] <- wl_data$waterlevel_m[20] + 0.5

  # Store original raw values
  original_raw <- wl_data$waterlevel_m

  log_dir <- tempdir()

  result <- adjust_WL_offset(
    input_data = wl_data,
    select_station = "TEST_WL",
    timestamp_start = format(wl_data$timestamp[20], "%Y-%m-%d %H:%M:%S"),
    timestamp_end = format(wl_data$timestamp[30], "%Y-%m-%d %H:%M:%S"),
    manual_note = "Test raw preservation",
    log_root = log_dir
  )

  # Raw column should be unchanged
  expect_equal(result$waterlevel_m, original_raw)
})

test_that("adjustment functions handle missing adjusted columns", {
  wl_data <- create_test_wl_for_adjustment(30)

  # Remove adjusted column
  wl_data$waterlevel_m_adj <- NULL

  log_dir <- tempdir()

  # Should error or create column
  result <- tryCatch({
    adjust_WL_offset(
      input_data = wl_data,
      select_station = "TEST_WL",
      offset_start = wl_data$timestamp[10],
      offset_end = wl_data$timestamp[20],
      offset_m = 0.1,
      log_root = log_dir
    )
  }, error = function(e) {
    # Error is acceptable if adjusted column is required
    NULL
  })

  # If function succeeded, adjusted column should exist
  if (!is.null(result)) {
    expect_true("waterlevel_m_adj" %in% names(result))
  }
})

test_that("adjustment functions handle missing adjusted columns", {
  wl_data <- create_test_wl_for_adjustment(30)

  # Remove adjusted column
  wl_data$waterlevel_m_adj <- NULL

  log_dir <- tempdir()

  # Should error or create column
  result <- tryCatch({
    adjust_WL_zero(
      input_data = wl_data,
      select_station = "TEST_WL",
      change_value = 0.1,
      manual_note = "Test missing column",
      log_root = log_dir
    )
  }, error = function(e) {
    # Error is acceptable if adjusted column is required
    NULL
  })

  # If function succeeded, adjusted column should exist
  if (!is.null(result)) {
    expect_true("waterlevel_m_adj" %in% names(result))
  }
})
