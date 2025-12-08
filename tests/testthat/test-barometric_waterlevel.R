# Test barometric and water level functions

# Helper to create test barometric data
create_test_baro_data <- function(n = 100) {
  data.frame(
    site_station_code = "TEST_BARO",
    timestamp = seq(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
                    by = "15 min", length.out = n),
    airpress_kPa = runif(n, 98, 103),
    airtemp_C = runif(n, -5, 25),
    sn = "12345678",
    metric = "barometric",
    latitude = 49.0,
    longitude = -123.0
  )
}

# Helper to create test water level data
create_test_wl_data <- function(n = 100) {
  data.frame(
    site_station_code = "TEST_WL",
    timestamp = seq(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
                    by = "15 min", length.out = n),
    waterpress_kPa = runif(n, 105, 115),
    watertemp_C = runif(n, 5, 20),
    airpress_kPa = runif(n, 98, 103),
    airtemp_C = runif(n, 3, 22),
    waterlevel_m = runif(n, 0.5, 2.5),  # Required by waterlevel_qaqc
    sn = "87654321",
    metric = "waterlevel",
    latitude = 49.0,
    longitude = -123.0
  )
}

test_that("barometric_qaqc creates adjusted columns", {
  baro_data <- create_test_baro_data()
  log_dir <- tempdir()

  result <- barometric_qaqc(
    input_data = baro_data,
    select_station = "TEST_BARO",
    log_root = log_dir
  )

  # Check adjusted columns exist
  expect_true("airpress_kPa_adj" %in% names(result))
  expect_true("airtemp_C_adj" %in% names(result))

  # Check edit flag columns exist
  expect_true("edit_press_high" %in% names(result))
  expect_true("edit_press_low" %in% names(result))
  expect_true("edit_temp_range" %in% names(result))
  expect_true("edit_spike" %in% names(result))
  expect_true("edit_flatline" %in% names(result))
})

test_that("barometric_qaqc detects out-of-range pressure", {
  baro_data <- create_test_baro_data(50)

  # Add out-of-range pressures
  baro_data$airpress_kPa[1:3] <- c(80, 110, 120)

  result <- barometric_qaqc(
    input_data = baro_data,
    select_station = "TEST_BARO",
    log_root = tempdir(),
    pressure_low_kpa = 85,
    pressure_high_kpa = 105
  )

  # Out-of-range pressures should be flagged and removed
  expect_true(is.na(result$airpress_kPa_adj[1]))
  expect_true(is.na(result$airpress_kPa_adj[2]))
  expect_true(is.na(result$airpress_kPa_adj[3]))

  # Edit flags should be set
  expect_true(result$edit_press_low[1])
  expect_true(result$edit_press_high[2])
  expect_true(result$edit_press_high[3])
})

test_that("barometric_qaqc detects out-of-range temperature", {
  baro_data <- create_test_baro_data(50)

  # Add out-of-range temperatures
  baro_data$airtemp_C[5:7] <- c(-50, 60, 70)

  result <- barometric_qaqc(
    input_data = baro_data,
    select_station = "TEST_BARO",
    log_root = tempdir(),
    temp_low_limit = -40,
    temp_high_limit = 50
  )

  # Out-of-range temps should be flagged and set to NA
  expect_true(is.na(result$airtemp_C_adj[5]))
  expect_true(is.na(result$airtemp_C_adj[6]))
  expect_true(is.na(result$airtemp_C_adj[7]))

  # Edit flag should be set
  expect_true(any(result$edit_temp_range[5:7]))
})

test_that("barometric_qaqc detects pressure spikes", {
  baro_data <- create_test_baro_data(50)

  # Create a spike
  baro_data$airpress_kPa[25] <- 103
  baro_data$airpress_kPa[26] <- 98  # Drop of 5 kPa

  result <- barometric_qaqc(
    input_data = baro_data,
    select_station = "TEST_BARO",
    log_root = tempdir(),
    spike_threshold_kpa = 1.5
  )

  # Spike should be detected
  expect_true(any(result$edit_spike))
})

test_that("barometric_qaqc detects flatlines", {
  baro_data <- create_test_baro_data(50)

  # Create a flatline
  baro_data$airpress_kPa[10:20] <- 100.5

  result <- barometric_qaqc(
    input_data = baro_data,
    select_station = "TEST_BARO",
    log_root = tempdir(),
    flatline_n = 6
  )

  # Flatline should be detected
  expect_true(any(result$edit_flatline))

  # All flatline values should be flagged
  flatline_indices <- which(result$edit_flatline)
  expect_gt(length(flatline_indices), 5)  # At least 6 consecutive
})

test_that("waterlevel_qaqc creates adjusted columns", {
  wl_data <- create_test_wl_data()
  log_dir <- tempdir()

  result <- waterlevel_qaqc(
    input_data = wl_data,
    select_station = "TEST_WL",
    log_root = log_dir
  )

  # Check adjusted columns exist
  expect_true("waterlevel_m_adj" %in% names(result))
  expect_true("watertemp_C_adj" %in% names(result))

  # Check flag columns exist
  expect_true("flag_disturbance" %in% names(result))
  expect_true("flag_ice" %in% names(result))
  expect_true("flag_wl_dry" %in% names(result))
})

test_that("waterlevel_qaqc detects disturbances", {
  wl_data <- create_test_wl_data(50)

  # Create a disturbance (large step change in water level)
  wl_data$waterlevel_m[25] <- 1.0
  wl_data$waterlevel_m[26] <- 1.5  # Jump of 0.5 m

  result <- waterlevel_qaqc(
    input_data = wl_data,
    select_station = "TEST_WL",
    log_root = tempdir()
  )

  # Disturbance should be detected
  expect_true(any(result$flag_disturbance))
})

test_that("waterlevel_qaqc detects ice conditions", {
  wl_data <- create_test_wl_data(50)

  # Set water temperature to freezing
  wl_data$watertemp_C[15:20] <- 0.1

  result <- waterlevel_qaqc(
    input_data = wl_data,
    select_station = "TEST_WL",
    log_root = tempdir()
  )

  # Ice should be flagged
  expect_true(any(result$flag_ice))
})

test_that("waterlevel_qaqc detects dry sensor conditions", {
  wl_data <- create_test_wl_data(50)

  # Create dry sensor conditions:
  # - Water pressure <= air pressure
  # - High water temperature
  # - Small air-water temp difference
  wl_data$waterpress_kPa[25:30] <- 100
  wl_data$airpress_kPa[25:30] <- 100.5
  wl_data$watertemp_C[25:30] <- 22
  wl_data$airtemp_C[25:30] <- 23

  result <- waterlevel_qaqc(
    input_data = wl_data,
    select_station = "TEST_WL",
    log_root = tempdir()
  )

  # Dry sensor should be detected
  expect_true(any(result$flag_wl_dry))
})

test_that("waterlevel_qaqc corrects negative temperatures", {
  wl_data <- create_test_wl_data(30)

  # Add slightly negative temperatures
  wl_data$watertemp_C[1:3] <- c(-0.8, -0.3, 0.2)

  result <- waterlevel_qaqc(
    input_data = wl_data,
    select_station = "TEST_WL",
    log_root = tempdir()
  )

  # Negative temps should be corrected to NA or removed
  expect_true(is.na(result$watertemp_C_adj[1]) || result$watertemp_C_adj[1] == 0)
  expect_true(is.na(result$watertemp_C_adj[2]) || result$watertemp_C_adj[2] == 0)

  # Positive values should be unchanged
  expect_equal(result$watertemp_C_adj[3], 0.2)
})

test_that("convert_waterlevel_kPa_m converts pressure to meters", {
  wl_data <- create_test_wl_data(50)

  output_dir <- file.path(tempdir(), "test_wl_convert")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Create a simple reference measurement
  ref_data <- data.frame(
    site_station_code = "TEST_WL",
    timestamp = wl_data$timestamp[25],
    depth_m = 1.0
  )

  ref_path <- file.path(output_dir, "test_ref.csv")
  write.csv(ref_data, ref_path, row.names = FALSE)

  result <- convert_waterlevel_kPa_m(
    input_data = wl_data,
    select_station = "TEST_WL",
    reference_data = ref_path,
    path_to_output_folder = output_dir
  )

  # Result is a list with 3 elements
  expect_type(result, "list")
  expect_equal(length(result), 3)

  # First element (site_wl) should have waterlevel_m column
  expect_true("waterlevel_m" %in% names(result[[1]]))
  expect_type(result[[1]]$waterlevel_m, "double")

  # Values should be reasonable (positive depths)
  valid_wl <- result[[1]]$waterlevel_m[!is.na(result[[1]]$waterlevel_m)]
  expect_gt(length(valid_wl), 0)

  # Clean up
  unlink(output_dir, recursive = TRUE)
})

test_that("barometric_qaqc_all processes multiple stations", {
  # Create data for multiple stations
  baro1 <- create_test_baro_data(30)
  baro1$site_station_code <- "BARO_01"

  baro2 <- create_test_baro_data(30)
  baro2$site_station_code <- "BARO_02"

  # Set up test directory structure
  test_root <- file.path(tempdir(), "test_baro_all")
  baro_data_dir <- file.path(test_root, "processed")
  output_dir <- file.path(test_root, "output")
  dir.create(baro_data_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Write barometric CSV files
  write.csv(baro1, file.path(baro_data_dir, "BARO_01_BARO_20250101_20250131_v0.1.csv"), row.names = FALSE)
  write.csv(baro2, file.path(baro_data_dir, "BARO_02_BARO_20250101_20250131_v0.1.csv"), row.names = FALSE)

  # Create metadata
  metadata <- data.frame(
    site_station_code = c("BARO_01", "BARO_02"),
    metric = c("barometric", "barometric"),
    latitude = c(49.0, 49.1),
    longitude = c(-123.0, -123.1)
  )
  metadata_path <- file.path(test_root, "metadata.csv")
  write.csv(metadata, metadata_path, row.names = FALSE)

  result <- barometric_qaqc_all(
    baro_data_path = baro_data_dir,
    metadata_path = metadata_path,
    path_to_output_folder = output_dir,
    log_root = output_dir
  )

  # Should have processed both stations
  stations <- unique(result$site_station_code)
  expect_equal(length(stations), 2)
  expect_true("BARO_01" %in% stations)
  expect_true("BARO_02" %in% stations)

  # Clean up
  unlink(test_root, recursive = TRUE)
})

test_that("barometric and waterlevel functions write log files", {
  baro_data <- create_test_baro_data(30)
  wl_data <- create_test_wl_data(30)

  # Add some issues to generate logs
  baro_data$airpress_kPa[5] <- 120  # Out of range
  wl_data$watertemp_C[10] <- 0.1    # Ice

  log_dir <- file.path(tempdir(), "test_baro_wl_logs")
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(log_dir, "logs"), showWarnings = FALSE, recursive = TRUE)

  barometric_qaqc(baro_data, "TEST_BARO", log_dir)
  waterlevel_qaqc(wl_data, "TEST_WL", log_dir)

  # Check log files exist
  baro_log <- file.path(log_dir, "logs", "TEST_BARO_BARO_qaqc_log.csv")
  wl_log <- file.path(log_dir, "logs", "TEST_WL_WL_qaqc_log.csv")

  expect_true(file.exists(baro_log))
  expect_true(file.exists(wl_log))

  # Clean up
  unlink(log_dir, recursive = TRUE)
})
