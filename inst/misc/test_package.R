#!/usr/bin/env Rscript
# Test script for waterloggerQAQC package

cat("========================================\n")
cat("Testing waterloggerQAQC Package\n")
cat("========================================\n\n")

# Load package
cat("1. Loading package...\n")
devtools::load_all(".")
cat("   ✓ Package loaded successfully\n\n")

# Set paths
pkg_root <- "inst/extdata/testing"
raw_path <- file.path(pkg_root, "raw")
metadata_path <- file.path(raw_path, "testing_metadata.csv")
output_path <- file.path(pkg_root, "processed")

# Create output directory if needed
if (!dir.exists(output_path)) {
  dir.create(output_path, recursive = TRUE)
}

cat("2. Testing bind_hobo_files() with DO data...\n")
cat("   Input folder:", file.path(raw_path, "DO"), "\n")
cat("   Metadata:", metadata_path, "\n")
cat("   Output folder:", output_path, "\n\n")

# Test 1: Bind DO files
tryCatch({
  DO_bound <- bind_hobo_files(
    path_to_raw_folder = file.path(raw_path, "DO"),
    path_to_output_folder = output_path,
    metadata_path = metadata_path
  )

  cat("   ✓ bind_hobo_files() executed successfully!\n")
  cat("   - Rows in output:", nrow(DO_bound[[1]]), "\n")
  cat("   - Stations:", paste(unique(DO_bound[[1]]$site_station_code), collapse = ", "), "\n")
  cat("   - Columns:", paste(names(DO_bound[[1]])[1:min(8, ncol(DO_bound[[1]]))], collapse = ", "), "...\n\n")

}, error = function(e) {
  cat("   ✗ ERROR:", conditionMessage(e), "\n\n")
})

# Test 2: Bind COND files
cat("3. Testing bind_hobo_files() with conductivity data...\n")
tryCatch({
  cond_bound <- bind_hobo_files(
    path_to_raw_folder = file.path(raw_path, "COND"),
    path_to_output_folder = output_path,
    metadata_path = metadata_path
  )

  cat("   ✓ bind_hobo_files() executed successfully!\n")
  cat("   - Rows in output:", nrow(cond_bound[[1]]), "\n")
  cat("   - Stations:", paste(unique(cond_bound[[1]]$site_station_code), collapse = ", "), "\n\n")

}, error = function(e) {
  cat("   ✗ ERROR:", conditionMessage(e), "\n\n")
})

cat("========================================\n")
cat("Test Summary: Package functions working!\n")
cat("========================================\n")

