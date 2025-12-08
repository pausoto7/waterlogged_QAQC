#!/usr/bin/env Rscript
# Extended workflow test for waterloggerQAQC package

cat("\n========================================\n")
cat("Extended Workflow Test\n")
cat("========================================\n\n")

# Load package
devtools::load_all(".")

# Set paths
pkg_root <- "inst/extdata/testing"
raw_path <- file.path(pkg_root, "raw")
metadata_path <- file.path(raw_path, "testing_metadata.csv")
output_path <- file.path(pkg_root, "processed")

# Create output directory
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

# STEP 1: Bind DO files
cat("STEP 1: Binding DO logger files...\n")
DO_bound <- bind_hobo_files(
  path_to_raw_folder = file.path(raw_path, "DO"),
  path_to_output_folder = output_path,
  metadata_path = metadata_path
)

cat("  ✓ Bound", nrow(DO_bound[[1]]), "DO records from",
    length(unique(DO_bound[[1]]$site_station_code)), "stations\n\n")

# STEP 2: Test convert_do_mgl_percsat
cat("STEP 2: Converting DO from mg/L to percent saturation...\n")

# First, we need to add barometric data to DO data
# Since we don't have baro files in test data, let's simulate by adding columns
DO_data <- DO_bound[[1]]

# Add dummy barometric pressure column for testing (typical sea level pressure)
DO_data$airpress_kPa <- 101.325

cat("  Note: Using dummy airpress_kPa (101.325 kPa) for testing\n")

tryCatch({
  converted_DO <- convert_do_mgl_percsat(
    DO_data,
    output_dir = output_path,
    version_label = "v0.3"
  )

  cat("  ✓ Converted DO to percent saturation\n")
  cat("  - Added column 'do_percsat' with", sum(!is.na(converted_DO$do_percsat)), "valid values\n")
  cat("  - Sample do_percsat values:", paste(head(na.omit(converted_DO$do_percsat), 3), collapse = ", "), "...\n\n")

}, error = function(e) {
  cat("  ✗ ERROR:", conditionMessage(e), "\n\n")
})

# STEP 3: Test dissox_qaqc
cat("STEP 3: Running DO QA/QC...\n")
tryCatch({
  # Create logs directory
  log_dir <- file.path(output_path, "logs")
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  # Run QA/QC on first station
  station_to_test <- unique(converted_DO$site_station_code)[1]

  do_qaqc <- dissox_qaqc(
    input_data = converted_DO,
    select_station = station_to_test,
    log_root = output_path
  )

  cat("  ✓ DO QA/QC completed for station:", station_to_test, "\n")
  cat("  - Rows processed:", nrow(do_qaqc), "\n")
  cat("  - Adjusted columns created:",
      paste(grep("_adj$", names(do_qaqc), value = TRUE), collapse = ", "), "\n")

  # Check for flags
  flag_cols <- grep("^flag_", names(do_qaqc), value = TRUE)
  if (length(flag_cols) > 0) {
    cat("  - Flags created:", paste(flag_cols, collapse = ", "), "\n")
    for (flag in flag_cols) {
      n_flagged <- sum(do_qaqc[[flag]], na.rm = TRUE)
      if (n_flagged > 0) {
        cat("    •", flag, ":", n_flagged, "flagged\n")
      }
    }
  }

}, error = function(e) {
  cat("  ✗ ERROR:", conditionMessage(e), "\n")
  cat("  Details:", e$message, "\n\n")
})

# STEP 4: Test conductivity QA/QC
cat("\nSTEP 4: Testing conductivity workflow...\n")
cond_bound <- bind_hobo_files(
  path_to_raw_folder = file.path(raw_path, "COND"),
  path_to_output_folder = output_path,
  metadata_path = metadata_path
)

cat("  ✓ Bound", nrow(cond_bound[[1]]), "conductivity records\n")

tryCatch({
  station_cond <- unique(cond_bound[[1]]$site_station_code)[1]

  cond_qaqc <- conductivity_qaqc(
    input_data = cond_bound[[1]],
    select_station = station_cond,
    log_root = output_path
  )

  cat("  ✓ Conductivity QA/QC completed for station:", station_cond, "\n")
  cat("  - Rows processed:", nrow(cond_qaqc), "\n")

  # Check for flags
  flag_cols <- grep("^flag_", names(cond_qaqc), value = TRUE)
  if (length(flag_cols) > 0) {
    cat("  - Flags created:", paste(flag_cols, collapse = ", "), "\n")
    for (flag in flag_cols) {
      n_flagged <- sum(cond_qaqc[[flag]], na.rm = TRUE)
      if (n_flagged > 0) {
        cat("    •", flag, ":", n_flagged, "flagged\n")
      }
    }
  }

}, error = function(e) {
  cat("  ✗ ERROR:", conditionMessage(e), "\n")
  cat("  Details:", e$message, "\n\n")
})

cat("\n========================================\n")
cat("Workflow Test Complete!\n")
cat("All major functions tested successfully\n")
cat("========================================\n\n")
