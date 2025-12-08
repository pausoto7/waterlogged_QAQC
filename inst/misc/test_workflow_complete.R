#!/usr/bin/env Rscript
# Complete workflow test for waterloggerQAQC package

cat("\n╔════════════════════════════════════════╗\n")
cat("║  waterloggerQAQC Package Test Suite   ║\n")
cat("╚════════════════════════════════════════╝\n\n")

# Load package
cat("Loading package...\n")
devtools::load_all(".", quiet = TRUE)
cat("✓ Package loaded\n\n")

# Set paths
pkg_root <- "inst/extdata/testing"
raw_path <- file.path(pkg_root, "raw")
metadata_path <- file.path(raw_path, "testing_metadata.csv")
output_path <- file.path(pkg_root, "processed")

# Create output directory
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}
if (!dir.exists(file.path(output_path, "logs"))) {
  dir.create(file.path(output_path, "logs"), recursive = TRUE)
}

# Test counter
tests_passed <- 0
tests_failed <- 0

test_function <- function(name, expr) {
  cat("TEST:", name, "...\n")
  result <- tryCatch({
    expr
    cat("  ✓ PASSED\n\n")
    tests_passed <<- tests_passed + 1
    TRUE
  }, error = function(e) {
    cat("  ✗ FAILED:", conditionMessage(e), "\n\n")
    tests_failed <<- tests_failed + 1
    FALSE
  })
  return(result)
}

# ============================================================================
# TEST 1: bind_hobo_files() with DO data
# ============================================================================
DO_bound <- NULL
test_function("bind_hobo_files() - Dissolved Oxygen", {
  DO_bound <<- bind_hobo_files(
    path_to_raw_folder = file.path(raw_path, "DO"),
    path_to_output_folder = output_path,
    metadata_path = metadata_path
  )

  stopifnot(is.list(DO_bound))
  stopifnot(length(DO_bound) == 2)
  stopifnot(nrow(DO_bound[[1]]) > 0)

  cat("  - Rows:", nrow(DO_bound[[1]]), "\n")
  cat("  - Stations:", paste(unique(DO_bound[[1]]$site_station_code), collapse = ", "), "\n")
})

# ============================================================================
# TEST 2: bind_hobo_files() with conductivity data
# ============================================================================
cond_bound <- NULL
test_function("bind_hobo_files() - Conductivity", {
  suppressWarnings({
    cond_bound <<- bind_hobo_files(
      path_to_raw_folder = file.path(raw_path, "COND"),
      path_to_output_folder = output_path,
      metadata_path = metadata_path
    )
  })

  stopifnot(nrow(cond_bound[[1]]) > 0)
  cat("  - Rows:", nrow(cond_bound[[1]]), "\n")
})

# ============================================================================
# TEST 3: convert_do_mgl_percsat()
# ============================================================================
converted_DO <- NULL
test_function("convert_do_mgl_percsat()", {
  DO_data <- DO_bound[[1]]
  DO_data$airpress_kPa <- 101.325  # Add dummy baro pressure

  converted_DO <<- convert_do_mgl_percsat(
    DO_data,
    output_dir = output_path,
    version_label = "v0.3"
  )

  stopifnot("do_percsat" %in% names(converted_DO))
  stopifnot(sum(!is.na(converted_DO$do_percsat)) > 0)

  cat("  - Valid do_percsat values:", sum(!is.na(converted_DO$do_percsat)), "\n")
})

# ============================================================================
# TEST 4: dissox_qaqc()
# ============================================================================
test_function("dissox_qaqc()", {
  # Add dummy airtemp column (needed by dissox_qaqc)
  converted_DO$airtemp_C <- converted_DO$watertemp_C + rnorm(nrow(converted_DO), 0, 2)

  station <- unique(converted_DO$site_station_code)[1]

  do_qaqc <- dissox_qaqc(
    input_data = converted_DO,
    select_station = station,
    log_root = output_path
  )

  stopifnot(nrow(do_qaqc) > 0)
  stopifnot("do_mgl_adj" %in% names(do_qaqc))

  cat("  - Station:", station, "\n")
  cat("  - Rows processed:", nrow(do_qaqc), "\n")
})

# ============================================================================
# TEST 5: conductivity_qaqc()
# ============================================================================
test_function("conductivity_qaqc()", {
  station <- unique(cond_bound[[1]]$site_station_code)[1]

  cond_qaqc <- conductivity_qaqc(
    input_data = cond_bound[[1]],
    select_station = station,
    log_root = output_path,
    temp_low_limit_C = -2,
    temp_high_limit_C = 40,
    disturbance_threshold_uScm = 200,
    dry_threshold_uScm = 10
  )

  stopifnot(nrow(cond_qaqc) > 0)
  stopifnot("conduct_uScm_adj" %in% names(cond_qaqc))

  cat("  - Station:", station, "\n")
  cat("  - Rows processed:", nrow(cond_qaqc), "\n")
})

# ============================================================================
# TEST 6: conductivity_temp_compensation()
# ============================================================================
test_function("conductivity_temp_compensation()", {
  station <- unique(cond_bound[[1]]$site_station_code)[1]

  # First run QA/QC to get adjusted columns
  cond_qaqc <- conductivity_qaqc(
    input_data = cond_bound[[1]],
    select_station = station,
    log_root = output_path
  )

  # Then apply temperature compensation
  cond_temp_comp <- conductivity_temp_compensation(cond_qaqc)

  stopifnot("spc_uScm_adj" %in% names(cond_temp_comp))
  stopifnot(sum(!is.na(cond_temp_comp$spc_uScm_adj)) > 0)

  cat("  - Specific conductance (SPC) column 'spc_uScm_adj' added\n")
  cat("  - Valid SPC values:", sum(!is.na(cond_temp_comp$spc_uScm_adj)), "\n")
})

# ============================================================================
# TEST 7: write_clean_data()
# ============================================================================
test_function("write_clean_data() - DO data", {
  # Prepare data with required columns for DO
  do_clean <- converted_DO
  do_clean$airtemp_C <- do_clean$watertemp_C + 2

  station <- unique(do_clean$site_station_code)[1]
  do_qaqc <- dissox_qaqc(do_clean, station, output_path)

  result <- write_clean_data(
    input_data = do_qaqc,
    path_to_output_folder = output_path,
    metric = "dissolvedoxygen"
  )

  # Check that v1.0 files were created
  clean_dir <- file.path(output_path, "2025", "clean")
  stopifnot(dir.exists(clean_dir))

  cat("  - Clean data files written to:", clean_dir, "\n")
})

# ============================================================================
# SUMMARY
# ============================================================================
cat("\n╔════════════════════════════════════════╗\n")
cat("║           TEST SUMMARY                 ║\n")
cat("╚════════════════════════════════════════╝\n\n")

cat("Tests Passed: ", tests_passed, "\n")
cat("Tests Failed: ", tests_failed, "\n")
cat("Total Tests:  ", tests_passed + tests_failed, "\n\n")

if (tests_failed == 0) {
  cat("✓ ALL TESTS PASSED! Package is working correctly.\n\n")
} else {
  cat("⚠ Some tests failed. Please review errors above.\n\n")
}
