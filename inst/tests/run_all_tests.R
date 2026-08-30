#!/usr/bin/env Rscript

#==============================================================================
# run_all_tests.R - Script to run all DeltaR tests
# 
# This script can be run from the command line:
#   Rscript inst/tests/run_all_tests.R
#==============================================================================

cat("========================================\n")
cat("     DeltaR Test Suite\n")
cat("========================================\n\n")

# Load the package
library(DeltaR)
cat("Package version:", packageVersion("DeltaR"), "\n\n")

# Source test script
source(system.file("tests", "test_delta.R", package = "DeltaR"))

# Run all tests
run_all_tests()