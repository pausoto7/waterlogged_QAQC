# Test output functions (write_clean_data, get_logger_data)

# Helper to create test data with QA/QC columns
create_test_output_data <- function(metric = "waterlevel", n = 100) {
  base_cols <- list(
    site_station_code = rep("TEST_SITE", n),
    timestamp = seq(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
                    by = "15 min", length.out = n),
    sn = rep("12345678", n),
    metric = rep(metric, n)
  )

  if (metric == "waterlevel") {
    data.frame(
      base_cols,
      waterlevel_m = runif(n, 0.5, 2.5),
      waterlevel_m_adj = runif(n, 0.5, 2.5),
      watertemp_C = runif(n, 5, 20),
      watertemp_C_adj = runif(n, 5, 20),
      wl_qaqc_code = sample(c(NA, "ICE", "DISTURBANCE"), n, replace = TRUE, prob = c(0.9, 0.05, 0.05)),
      wl_qaqc_note = NA_character_,
      flag_ice = FALSE,
      flag_disturbance = FALSE
    )
  } else if (metric == "dissolvedoxygen") {
    data.frame(
      base_cols,
      do_mgl = runif(n, 7, 12),
      do_mgl_adj = runif(n, 7, 12),
      do_percsat = runif(n, 80, 110),
      do_percsat_adj = runif(n, 80, 110),
      watertemp_C = runif(n, 5, 20),
      watertemp_C_adj = runif(n, 5, 20),
      do_qaqc_code = sample(c(NA, "ICE"), n, replace = TRUE, prob = c(0.95, 0.05)),
      do_qaqc_note = NA_character_,
      flag_ice = FALSE
    )
  } else {
    stop("Unsupported metric for test data")
  }
}

test_that("write_clean_data creates v1.0 files", {
  wl_data <- create_test_output_data("waterlevel", 100)

  output_dir <- file.path(tempdir(), "test_write_clean")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  result <- write_clean_data(
    input_data = wl_data,
    path_to_output_folder = output_dir,
    metric = "waterlevel"
  )

  # Check that v1.0 files were created
  clean_dir <- file.path(output_dir, "2025", "clean")
  expect_true(dir.exists(clean_dir))

  v10_files <- list.files(clean_dir, pattern = "v1\\.0\\.csv$", recursive = FALSE)
  expect_gt(length(v10_files), 0)

  # Clean up
  unlink(output_dir, recursive = TRUE)
})

test_that("write_clean_data splits by station and year", {
  # Create data spanning 2 stations
  wl_data1 <- create_test_output_data("waterlevel", 50)
  wl_data1$site_station_code <- "SITE_01"

  wl_data2 <- create_test_output_data("waterlevel", 50)
  wl_data2$site_station_code <- "SITE_02"

  all_data <- rbind(wl_data1, wl_data2)

  output_dir <- file.path(tempdir(), "test_write_multi")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  result <- write_clean_data(
    input_data = all_data,
    path_to_output_folder = output_dir,
    metric = "waterlevel"
  )

  # Should have 2 files (one per station)
  clean_dir <- file.path(output_dir, "2025", "clean")
  v10_files <- list.files(clean_dir, pattern = "v1\\.0\\.csv$")
  expect_equal(length(v10_files), 2)

  # File names should contain station codes
  expect_true(any(grepl("SITE_01", v10_files)))
  expect_true(any(grepl("SITE_02", v10_files)))

  # Clean up
  unlink(output_dir, recursive = TRUE)
})

test_that("write_clean_data includes flag and edit columns", {
  do_data <- create_test_output_data("dissolvedoxygen", 50)

  output_dir <- file.path(tempdir(), "test_write_flags")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  result <- write_clean_data(
    input_data = do_data,
    path_to_output_folder = output_dir,
    metric = "dissolvedoxygen"
  )

  # Read back the file
  clean_dir <- file.path(output_dir, "2025", "clean")
  v10_files <- list.files(clean_dir, pattern = "v1\\.0\\.csv$", full.names = TRUE)

  expect_gt(length(v10_files), 0)

  written_data <- read.csv(v10_files[1], stringsAsFactors = FALSE)

  # Should include QA/QC columns
  expect_true("do_qaqc_code" %in% names(written_data))

  # Should include flag columns
  expect_true(any(grepl("^flag_", names(written_data))))

  # Clean up
  unlink(output_dir, recursive = TRUE)
})

test_that("write_clean_data validates metric parameter", {
  wl_data <- create_test_output_data("waterlevel", 50)

  output_dir <- tempdir()

  # Invalid metric
  expect_error(
    write_clean_data(
      input_data = wl_data,
      path_to_output_folder = output_dir,
      metric = "invalid_metric"
    )
  )
})

test_that("write_clean_data handles missing required columns", {
  wl_data <- create_test_output_data("waterlevel", 20)

  # Remove required column
  wl_data$waterlevel_m_adj <- NULL

  output_dir <- tempdir()

  expect_error(
    write_clean_data(
      input_data = wl_data,
      path_to_output_folder = output_dir,
      metric = "waterlevel"
    ),
    "missing|required"
  )
})

test_that("get_logger_data retrieves data from v1.0 files", {
  skip("Requires actual v1.0 files in inst/extdata")

  # This test would need pre-existing v1.0 files
  # Or we would need to write them first
  data_dir <- system.file("extdata/testing/processed", package = "waterloggerQAQC")

  if (dir.exists(data_dir)) {
    result <- get_logger_data(
      data_folder = data_dir,
      metric = "waterlevel",
      data_processing = "v1.0"
    )

    expect_s3_class(result, "data.frame")
    expect_true("timestamp" %in% names(result))
    expect_true("site_station_code" %in% names(result))
  }
})

test_that("get_logger_data aggregates data temporally", {
  # Create and write test data
  wl_data <- create_test_output_data("waterlevel", 200)  # More data for aggregation

  output_dir <- file.path(tempdir(), "test_get_data")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  write_clean_data(
    input_data = wl_data,
    path_to_output_folder = output_dir,
    metric = "waterlevel"
  )

  # Retrieve and aggregate by day
  result <- get_logger_data(
    path_to_data = output_dir,
    metric = "waterlevel",
    data_processing = "v1.0",
    temporal_scale = "daily"
  )

  # Should have fewer rows than original (aggregated)
  expect_lt(nrow(result), nrow(wl_data))

  # Should have summary statistics columns
  expect_true(any(grepl("mean_", names(result))))
  expect_true(any(grepl("min_", names(result))))
  expect_true(any(grepl("max_", names(result))))

  # Clean up
  unlink(output_dir, recursive = TRUE)
})

test_that("get_logger_data handles different data_processing versions", {
  # Create test directory structure
  output_dir <- file.path(tempdir(), "test_get_versions")
  dir.create(file.path(output_dir, "2025/processed"), showWarnings = FALSE, recursive = TRUE)

  # Create v0.1 file
  wl_data <- create_test_output_data("waterlevel", 50)
  v01_file <- file.path(output_dir, "2025/processed", "TEST_SITE_WL_v0.1.csv")
  write.csv(wl_data, v01_file, row.names = FALSE)

  # Retrieve v0.1 data
  result <- get_logger_data(
    path_to_data = output_dir,
    metric = "waterlevel",
    data_processing = "v0.1"
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)

  # Clean up
  unlink(output_dir, recursive = TRUE)
})

test_that("get_logger_data validates temporal_scale parameter", {
  output_dir <- tempdir()

  # Invalid temporal scale
  expect_error(
    get_logger_data(
      path_to_data = output_dir,
      metric = "waterlevel",
      data_processing = "v1.0",
      temporal_scale = "invalid_scale"
    ),
    "temporal_scale"
  )
})

test_that("get_logger_data filters by station_code", {
  # Create data for multiple stations
  wl_data1 <- create_test_output_data("waterlevel", 50)
  wl_data1$site_station_code <- "SITE_01"

  wl_data2 <- create_test_output_data("waterlevel", 50)
  wl_data2$site_station_code <- "SITE_02"

  all_data <- rbind(wl_data1, wl_data2)

  output_dir <- file.path(tempdir(), "test_get_filter")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  write_clean_data(
    input_data = all_data,
    path_to_output_folder = output_dir,
    metric = "waterlevel"
  )

  # Retrieve only one station
  result <- get_logger_data(
    path_to_data = output_dir,
    metric = "waterlevel",
    data_processing = "v1.0",
    select_station = "SITE_01"
  )

  # Should only have SITE_01
  expect_equal(unique(result$site_station_code), "SITE_01")
  expect_false("SITE_02" %in% result$site_station_code)

  # Clean up
  unlink(output_dir, recursive = TRUE)
})

test_that("get_logger_data handles no files found", {
  empty_dir <- file.path(tempdir(), "empty_test_dir")
  dir.create(empty_dir, showWarnings = FALSE, recursive = TRUE)

  expect_error(
    get_logger_data(
      path_to_data = empty_dir,
      metric = "waterlevel",
      data_processing = "v1.0"
    ),
    "No.*files found|does not exist"
  )

  unlink(empty_dir, recursive = TRUE)
})
