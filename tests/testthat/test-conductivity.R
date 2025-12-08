# Test conductivity functions

# Helper to create test conductivity data
create_test_cond_data <- function(n = 100) {
  data.frame(
    site_station_code = "TEST_COND",
    timestamp = seq(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
                    by = "15 min", length.out = n),
    conduct_uScm = runif(n, 100, 500),
    watertemp_C = runif(n, 8, 18),
    airtemp_C = runif(n, 6, 20),
    sn = "12345678",
    metric = "conductivity"
  )
}

test_that("conductivity_qaqc creates adjusted columns", {
  cond_data <- create_test_cond_data()
  log_dir <- tempdir()

  result <- conductivity_qaqc(
    input_data = cond_data,
    select_station = "TEST_COND",
    log_root = log_dir
  )

  # Check adjusted columns exist
  expect_true("conduct_uScm_adj" %in% names(result))
  expect_true("watertemp_C_adj" %in% names(result))

  # Check QA/QC columns exist
  expect_true("cond_qaqc_code" %in% names(result))
  expect_true("cond_qaqc_note" %in% names(result))

  # Check flag columns exist
  expect_true("edit_cond_spike" %in% names(result))
  expect_true("edit_cond_dry" %in% names(result))
  expect_true("edit_temp_range" %in% names(result))
  expect_true("edit_ice" %in% names(result))
})

test_that("conductivity_qaqc detects temperature out of range", {
  cond_data <- create_test_cond_data(50)

  # Add out-of-range temperatures
  cond_data$watertemp_C[1:3] <- c(-5, -3, 45)

  result <- conductivity_qaqc(
    input_data = cond_data,
    select_station = "TEST_COND",
    log_root = tempdir(),
    temp_low_limit_C = -2,
    temp_high_limit_C = 40
  )

  # Out-of-range temps should be flagged
  expect_true(result$edit_temp_range[1])
  expect_true(result$edit_temp_range[2])
  expect_true(result$edit_temp_range[3])

  # Should have TEMP_LOW or TEMP_HIGH codes
  expect_true(any(result$cond_qaqc_code %in% c("TEMP_LOW", "TEMP_HIGH"), na.rm = TRUE))
})

test_that("conductivity_qaqc corrects negative temperatures near zero", {
  cond_data <- create_test_cond_data(50)

  # Add slightly negative temperatures
  cond_data$watertemp_C[1:5] <- c(-0.9, -0.5, -0.1, 0.05, 0.5)

  result <- conductivity_qaqc(
    input_data = cond_data,
    select_station = "TEST_COND",
    log_root = tempdir()
  )

  # Values between -1 and 0 should be corrected to 0
  expect_equal(result$watertemp_C_adj[1], 0)
  expect_equal(result$watertemp_C_adj[2], 0)
  expect_equal(result$watertemp_C_adj[3], 0)

  # Positive values should be unchanged
  expect_equal(result$watertemp_C_adj[4], 0.05)
  expect_equal(result$watertemp_C_adj[5], 0.5)

  # Should have NEGATIVE_WT code
  expect_true(any(result$cond_qaqc_code == "NEGATIVE_WT", na.rm = TRUE))
})

test_that("conductivity_qaqc detects ice conditions", {
  cond_data <- create_test_cond_data(50)

  # Set water temperature near freezing
  cond_data$watertemp_C[10:15] <- 0.05

  result <- conductivity_qaqc(
    input_data = cond_data,
    select_station = "TEST_COND",
    log_root = tempdir()
  )

  # Ice should be flagged
  expect_true(any(result$edit_ice[10:15]))
  expect_true(any(result$cond_qaqc_code[10:15] == "ICE", na.rm = TRUE))
})

test_that("conductivity_qaqc detects conductivity spikes", {
  cond_data <- create_test_cond_data(50)

  # Create a spike
  cond_data$conduct_uScm[25] <- 1000  # Jump from ~300 to 1000

  result <- conductivity_qaqc(
    input_data = cond_data,
    select_station = "TEST_COND",
    log_root = tempdir(),
    disturbance_threshold_uScm = 200
  )

  # Spike should be flagged
  expect_true(result$edit_cond_spike[25])
  expect_equal(result$cond_qaqc_code[25], "SPIKE")
})

test_that("conductivity_qaqc detects dry sensor - conductivity only", {
  cond_data <- create_test_cond_data(50)

  # Very low conductivity (dry sensor)
  cond_data$conduct_uScm[20:25] <- 5

  result <- conductivity_qaqc(
    input_data = cond_data,
    select_station = "TEST_COND",
    log_root = tempdir(),
    dry_threshold_uScm = 10
  )

  # Dry should be flagged
  expect_true(any(result$edit_cond_dry[20:25]))
  expect_true(any(result$cond_qaqc_code[20:25] == "DRY_COND_ONLY", na.rm = TRUE))
})

test_that("conductivity_qaqc detects dry sensor - with air/water temp", {
  cond_data <- create_test_cond_data(50)

  # Dry sensor: low conductivity AND air/water temps close
  cond_data$conduct_uScm[20:25] <- 5
  cond_data$watertemp_C[20:25] <- 15
  cond_data$airtemp_C[20:25] <- 15.5

  result <- conductivity_qaqc(
    input_data = cond_data,
    select_station = "TEST_COND",
    log_root = tempdir(),
    dry_threshold_uScm = 10,
    air_water_diff_threshold_C = 2
  )

  # Dry with air temp should be flagged
  expect_true(any(result$cond_qaqc_code[20:25] == "DRY_AIRTEMP", na.rm = TRUE))
})

test_that("conductivity_qaqc assigns single QA/QC code per row", {
  cond_data <- create_test_cond_data(50)

  # Create multiple issues in different rows
  cond_data$conduct_uScm[10] <- 5        # Dry
  cond_data$conduct_uScm[20] <- 1000     # Spike
  cond_data$watertemp_C[30] <- 0.05      # Ice
  cond_data$watertemp_C[40] <- -0.5      # Negative

  result <- conductivity_qaqc(
    input_data = cond_data,
    select_station = "TEST_COND",
    log_root = tempdir()
  )

  # Each flagged row should have only one code (no conflicts)
  flagged_rows <- !is.na(result$cond_qaqc_code)
  codes <- result$cond_qaqc_code[flagged_rows]

  # Check that we have different codes
  expect_true("DRY_COND_ONLY" %in% codes || "DRY_AIRTEMP" %in% codes)
  expect_true("ICE" %in% codes)
  expect_true("NEGATIVE_WT" %in% codes)
})

test_that("conductivity_temp_compensation calculates specific conductance", {
  cond_data <- create_test_cond_data(50)

  # First run QA/QC
  cond_qaqc <- conductivity_qaqc(
    input_data = cond_data,
    select_station = "TEST_COND",
    log_root = tempdir()
  )

  # Apply temperature compensation
  result <- conductivity_temp_compensation(cond_qaqc)

  # Check that SPC column was added
  expect_true("spc_uScm_adj" %in% names(result))
  expect_type(result$spc_uScm_adj, "double")

  # SPC should be different from conductivity (compensated to 25\u00B0C)
  valid_rows <- !is.na(result$spc_uScm_adj) & !is.na(result$conduct_uScm_adj)
  expect_true(sum(valid_rows) > 0)

  # At temperatures != 25\u00B0C, SPC should differ from conductivity
  non_25c <- valid_rows & abs(result$watertemp_C_adj - 25) > 0.5
  if (sum(non_25c) > 0) {
    expect_false(all(result$spc_uScm_adj[non_25c] == result$conduct_uScm_adj[non_25c]))
  }
})

test_that("conductivity_temp_compensation handles missing data", {
  cond_data <- create_test_cond_data(20)

  # First run QA/QC
  cond_qaqc <- conductivity_qaqc(
    input_data = cond_data,
    select_station = "TEST_COND",
    log_root = tempdir()
  )

  # Set some values to NA
  cond_qaqc$conduct_uScm_adj[1:5] <- NA
  cond_qaqc$watertemp_C_adj[10:12] <- NA

  result <- conductivity_temp_compensation(cond_qaqc)

  # SPC should be NA where inputs are NA
  expect_true(all(is.na(result$spc_uScm_adj[1:5])))
  expect_true(all(is.na(result$spc_uScm_adj[10:12])))

  # Non-NA inputs should produce non-NA SPC
  valid_rows <- !is.na(cond_qaqc$conduct_uScm_adj) & !is.na(cond_qaqc$watertemp_C_adj)
  expect_equal(
    is.na(result$spc_uScm_adj),
    !valid_rows
  )
})

test_that("conductivity_qaqc writes log files", {
  cond_data <- create_test_cond_data(30)

  # Add some issues to log
  cond_data$conduct_uScm[5] <- 1000  # Spike
  cond_data$watertemp_C[10] <- 0.05  # Ice

  log_dir <- file.path(tempdir(), "test_cond_logs")
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(log_dir, "logs"), showWarnings = FALSE, recursive = TRUE)

  result <- conductivity_qaqc(
    input_data = cond_data,
    select_station = "TEST_COND",
    log_root = log_dir
  )

  # Check that log file was created
  log_file <- file.path(log_dir, "logs", "TEST_COND_COND_qaqc_log.csv")
  expect_true(file.exists(log_file))

  # Read log and verify contents
  log_data <- read.csv(log_file, stringsAsFactors = FALSE)
  expect_gt(nrow(log_data), 0)
  expect_true("station" %in% names(log_data))
  expect_true("metric" %in% names(log_data))
  expect_equal(unique(log_data$station), "TEST_COND")
  expect_equal(unique(log_data$metric), "COND")

  # Clean up
  unlink(log_dir, recursive = TRUE)
})

test_that("conductivity_qaqc handles edge cases", {
  # Empty data after filtering
  expect_error(
    conductivity_qaqc(
      input_data = data.frame(site_station_code = "OTHER"),
      select_station = "TEST_COND",
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
    conductivity_qaqc(
      input_data = bad_data,
      select_station = "TEST",
      log_root = tempdir()
    ),
    "missing required column"
  )
})
