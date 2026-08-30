#==============================================================================
# test_load_delta.R - Tests for loading DELTA files
#==============================================================================

context("Loading DELTA files")

test_that("load_delta loads files correctly", {
  skip_if_no_example_files("viola")
  
  files <- get_example_files("viola")
  
  # Test loading without specifications
  delta <- load_delta(files$chars, files$items)
  expect_s3_class(delta, "Rcpp_DeltaParser")
  expect_true(delta$char_count() > 0)
  expect_true(delta$item_count() > 0)
  expect_false(delta$has_specifications())
  expect_true(delta$is_chars_parsed())
  expect_true(delta$is_items_parsed())
  
  # Test loading with specifications
  if (file.exists(files$specs)) {
    delta2 <- load_delta(files$chars, files$items, files$specs)
    expect_s3_class(delta2, "Rcpp_DeltaParser")
    expect_true(delta2$has_specifications())
    expect_true(delta2$is_specs_parsed())
  }
})

test_that("load_delta works with grass dataset", {
  skip_if_no_example_files("grass")
  
  files <- get_example_files("grass")
  delta <- load_delta(files$chars, files$items)
  
  expect_s3_class(delta, "Rcpp_DeltaParser")
  expect_true(delta$char_count() > 0)
  expect_true(delta$item_count() > 0)
})

test_that("load_delta errors on missing files", {
  expect_error(
    load_delta("nonexistent_chars.txt", "nonexistent_items.txt"),
    "Characters file not found"
  )
  
  expect_error(
    load_delta("nonexistent_chars.txt", "nonexistent_items.txt", 
               "nonexistent_specs.txt"),
    "Characters file not found"
  )
})

test_that("DeltaParser object has correct methods", {
  skip_if_no_example_files("viola")
  
  files <- get_example_files("viola")
  delta <- load_delta(files$chars, files$items)
  
  # Check that all expected methods exist
  expected_methods <- c(
    "char_count", "char_feature", "char_type", "char_type_name",
    "char_unit", "state_count", "state_name", "states",
    "set_char_type", "set_char_type_by_name",
    "item_count", "item_name", "item_names", "attribute_count",
    "attribute_string", "item_attributes", "item_data",
    "find_matching", "item_matches",
    "has_specifications", "implicit_value", "implicit_values",
    "dependent_count", "dependent_characters", "is_dependent",
    "version", "chars_filename", "items_filename", "specs_filename",
    "is_chars_parsed", "is_items_parsed", "is_specs_parsed"
  )
  
  for (method in expected_methods) {
    expect_true(method %in% names(delta), 
                paste("Method", method, "not found"))
  }
})