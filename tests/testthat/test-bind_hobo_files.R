# Test bind_hobo_files and related functions

# Setup test paths
setup_test_paths <- function() {
  list(
    raw_do = system.file("extdata/testing/raw/DO", package = "waterloggerQAQC"),
    raw_cond = system.file("extdata/testing/raw/COND", package = "waterloggerQAQC"),
    metadata = system.file("extdata/testing/raw/testing_metadata.csv", package = "waterloggerQAQC"),
    output = tempdir()
  )
}

test_that("bind_hobo_files works with DO data", {
  skip_if_not(dir.exists(system.file("extdata/testing", package = "waterloggerQAQC")),
              "Test data not available")

  paths <- setup_test_paths()
  skip_if_not(file.exists(paths$metadata), "Metadata file not found")

  result <- bind_hobo_files(
    path_to_raw_folder = paths$raw_do,
    path_to_output_folder = paths$output,
    metadata_path = paths$metadata
  )

  # Check structure
  expect_type(result, "list")
  expect_length(result, 2)
  expect_s3_class(result[[1]], "data.frame")
  expect_s3_class(result[[2]], "gg")  # ggplot object

  # Check data
  data <- result[[1]]
  expect_gt(nrow(data), 0)
  expect_true("site_station_code" %in% names(data))
  expect_true("timestamp" %in% names(data))
  expect_true("do_mgl" %in% names(data))
  expect_true("watertemp_C" %in% names(data))

  # Check timestamp is POSIXct
  expect_s3_class(data$timestamp, "POSIXct")

  # Check no duplicate rows
  expect_equal(nrow(data), nrow(unique(data)))
})

test_that("bind_hobo_files works with conductivity data", {
  skip_if_not(dir.exists(system.file("extdata/testing", package = "waterloggerQAQC")),
              "Test data not available")

  paths <- setup_test_paths()

  suppressWarnings({
    result <- bind_hobo_files(
      path_to_raw_folder = paths$raw_cond,
      path_to_output_folder = paths$output,
      metadata_path = paths$metadata
    )
  })

  # Check structure
  expect_type(result, "list")
  data <- result[[1]]

  # Check conductivity-specific columns
  expect_true("conduct_uScm" %in% names(data))
  expect_true("watertemp_C" %in% names(data))

  # Check for range information if multi-range logger
  if ("conduct_range_used" %in% names(data)) {
    expect_type(data$conduct_range_used, "character")
  }
})

test_that("bind_hobo_files errors with missing metadata", {
  paths <- setup_test_paths()

  expect_error(
    bind_hobo_files(
      path_to_raw_folder = paths$raw_do,
      path_to_output_folder = paths$output,
      metadata_path = "nonexistent_file.csv"
    ),
    "File not found"
  )
})

test_that("bind_hobo_files errors with missing raw folder", {
  paths <- setup_test_paths()

  expect_error(
    bind_hobo_files(
      path_to_raw_folder = file.path(tempdir(), "nonexistent_folder"),
      path_to_output_folder = paths$output,
      metadata_path = paths$metadata
    ),
    "does not exist"
  )
})

test_that("bind_hobo_files writes v0.1 files correctly", {
  skip_if_not(dir.exists(system.file("extdata/testing", package = "waterloggerQAQC")),
              "Test data not available")

  paths <- setup_test_paths()
  output_dir <- file.path(tempdir(), "test_bind_output")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  result <- bind_hobo_files(
    path_to_raw_folder = paths$raw_do,
    path_to_output_folder = output_dir,
    metadata_path = paths$metadata
  )

  # Check that v0.1 files were created
  v01_files <- list.files(output_dir, pattern = "v0\\.1\\.csv$", recursive = TRUE)
  expect_gt(length(v01_files), 0)

  # Clean up
  unlink(output_dir, recursive = TRUE)
})

test_that("QAQC_metadata validates metadata correctly", {
  paths <- setup_test_paths()
  skip_if_not(file.exists(paths$metadata), "Metadata file not found")

  # This is an internal function but should work
  metadata <- QAQC_metadata(paths$metadata)

  expect_s3_class(metadata, "data.frame")
  expect_true("site_station_code" %in% names(metadata))
  expect_true("timestamp_deploy" %in% names(metadata))
  expect_true("timestamp_remove" %in% names(metadata))
  expect_true("sn" %in% names(metadata))
  expect_true("metric" %in% names(metadata))
  expect_true("latitude" %in% names(metadata))
  expect_true("longitude" %in% names(metadata))

  # Check timestamp parsing
  expect_s3_class(metadata$timestamp_deploy, "POSIXct")
  expect_s3_class(metadata$timestamp_remove, "POSIXct")
})

test_that("QAQC_reference_data validates reference data", {
  ref_path <- system.file("extdata/testing/raw/NT_manual_waterlevel_20251119.csv",
                          package = "waterloggerQAQC")
  skip_if_not(file.exists(ref_path), "Reference data file not found")

  ref_data <- QAQC_reference_data(ref_path)

  expect_s3_class(ref_data, "data.frame")
  expect_true("site_station_code" %in% names(ref_data))
  expect_true("timestamp" %in% names(ref_data))
  expect_s3_class(ref_data$timestamp, "POSIXct")

  # Should have at least one measurement column
  has_measurement <- any(c("stage_m", "depth_m") %in% names(ref_data))
  expect_true(has_measurement)
})
