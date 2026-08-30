#==============================================================================
# test_error_handling.R - Tests for error handling
#==============================================================================

context("Error handling")

test_that("character methods handle invalid inputs", {
  skip_if_no_example_files("viola")
  
  delta <- load_test_dataset("viola", with_specs = FALSE)
  n_chars <- delta$char_count()
  
  # Invalid character number
  expect_error(delta$char_feature(0), "Character number out of range")
  expect_error(delta$char_feature(n_chars + 1), "Character number out of range")
  expect_error(delta$char_type(0))
  expect_error(delta$char_type(n_chars + 1))
  expect_error(delta$char_type_name(0))
  expect_error(delta$char_type_name(n_chars + 1))
  expect_error(delta$char_unit(0))
  expect_error(delta$char_unit(n_chars + 1))
  expect_error(delta$state_count(0))
  expect_error(delta$state_count(n_chars + 1))
  expect_error(delta$state_name(0, 1))
  expect_error(delta$state_name(n_chars + 1, 1))
  expect_error(delta$states(0))
  expect_error(delta$states(n_chars + 1))
})

test_that("item methods handle invalid inputs", {
  skip_if_no_example_files("viola")
  
  delta <- load_test_dataset("viola", with_specs = FALSE)
  n_items <- delta$item_count()
  
  # Invalid item number
  expect_error(delta$item_name(0), "Item number out of range")
  expect_error(delta$item_name(n_items + 1), "Item number out of range")
  expect_error(delta$attribute_count(0))
  expect_error(delta$attribute_count(n_items + 1))
  expect_error(delta$item_attributes(0))
  expect_error(delta$item_attributes(n_items + 1))
  expect_error(delta$item_data(0))
  expect_error(delta$item_data(n_items + 1))
})

test_that("search functions handle invalid inputs", {
  skip_if_no_example_files("viola")
  
  delta <- load_test_dataset("viola", with_specs = FALSE)
  n_items <- delta$item_count()
  
  # Invalid item number for matches
  if (n_items > 0) {
    expect_error(item_matches(delta, 0, 1, 1))
    expect_error(item_matches(delta, n_items + 1, 1, 1))
  }
})

test_that("attribute methods handle invalid inputs", {
  skip_if_no_example_files("viola")
  
  delta <- load_test_dataset("viola", with_specs = FALSE)
  
  if (delta$item_count() > 0 && delta$attribute_count(1) > 0) {
    attr_count <- delta$attribute_count(1)
    expect_error(delta$attribute_string(1, 0))
    expect_error(delta$attribute_string(1, attr_count + 1))
  }
})

test_that("load_delta handles malformed files gracefully", {
  # Create malformed file
  malformed_content <- c(
    "This is not a valid DELTA file",
    "No proper formatting"
  )
  tmp_file <- create_temp_delta_file(malformed_content, "txt")
  
  # Should error or warn
  expect_error(load_delta(tmp_file, tmp_file))
  
  # Clean up
  file.remove(tmp_file)
})

test_that("strip_comments handles malformed input", {
  # Unclosed comment should still work
  result <- strip_comments("Item <unclosed comment")
  expect_equal(result, "Item ")
  
  # Empty string
  result <- strip_comments("")
  expect_equal(result, "")
})