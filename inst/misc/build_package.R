# ------------------------------------
# Build and Update R package
# ------------------------------------
# This script is run from the package root directory

library(usethis)
library(testthat)
library(devtools)

rm(list = ls())

# Loading unfinished package to memory for testing
devtools::load_all()

# Update R function documentation
devtools::document()

# Run tests - all passed Nov 19 2024
devtools::test()

# Check package will run properly
devtools::check()


remove.packages("CEMPRA")
# install.packages(getwd(), repos = NULL, type = "source")
# devtools::install_github("essatech/CEMPRA")
# library(CEMPRA)
# Installing unfinished package to computer...



