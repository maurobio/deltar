#==============================================================================
# test_characters.R - Tests for character functionality
#==============================================================================

context("Character functionality")

test_that("characters_df returns correct structure", {
  skip_if_no_example_files("viola")
  
  delta <- load_test_dataset("viola", with_specs = FALSE)
  chars_df <- characters_df(delta)
  
  # Check structure
  expect_s3_class(chars_df, "data.frame")
  expect_true(nrow(chars_df) > 0)
  expect_true(ncol(chars_df) >= 4)
  
  # Check column names
  expected_cols <- c("number", "feature", "type", "unit", "states")
  expect_true(all(expected_cols %in% colnames(chars_df)))
  
  # Check types
  expect_type(chars_df$number, "integer")
  expect_type(chars_df$feature, "character")
  expect_type(chars_df$type, "character")
  expect_type(chars_df$unit, "character")
  expect_true(is.list(chars_df$states))
})

test_that("character methods work correctly", {
  skip_if_no_example_files("viola")
  
  delta <- load_test_dataset("viola", with_specs = FALSE)
  n_chars <- delta$char_count()
  
  expect_true(n_chars > 0)
  
  # Test filename
  expect_true(nchar(delta$chars_filename()) > 0)
  expect_true(delta$is_chars_parsed())
  
  # Test each character
  for (i in 1:min(5, n_chars)) {
    # Feature should be non-empty
    feature <- delta$char_feature(i)
    expect_true(nchar(feature) > 0)
    
    # Type should be valid
    type <- delta$char_type(i)
    expect_true(type %in% c(2, 3, 4, 5, 8))  # CT_UM, CT_OM, CT_IN, CT_RN, CT_TE
    
    # Type name should be non-empty
    type_name <- delta$char_type_name(i)
    expect_true(nchar(type_name) > 0)
    expect_true(type_name %in% c("unordered_multistate", "ordered_multistate",
                                 "integer_numeric", "real_numeric", "text"))
    
    # If multistate, states should exist
    if (delta$char_type_name(i) == "unordered_multistate") {
      states <- delta$states(i)
      expect_true(length(states) > 0)
      
      # Test individual state retrieval
      state_count <- delta$state_count(i)
      expect_equal(length(states), state_count)
      
      if (state_count > 0) {
        first_state <- delta$state_name(i, 1)
        expect_true(nchar(first_state) > 0)
      }
    }
    
    # Test unit for numeric characters
    if (delta$char_type_name(i) %in% c("integer_numeric", "real_numeric")) {
      unit <- delta$char_unit(i)
      # Unit could be empty string or have a value
      expect_true(is.character(unit))
    }
  }
})

test_that("set_char_type works", {
  skip_if_no_example_files("viola")
  
  files <- create_minimal_test_dataset()
  delta <- load_delta(files$chars, files$items)
  
  # Original type should be CT_UM (2)
  expect_equal(delta$char_type(1), 2)
  
  # Set to CT_IN (4)
  delta$set_char_type(1, 4)
  expect_equal(delta$char_type(1), 4)
  
  # Set to CT_TE (8) by name
  delta$set_char_type_by_name(1, "text")
  expect_equal(delta$char_type(1), 8)
  
  # Invalid type name should error
  expect_error(delta$set_char_type_by_name(1, "invalid"))
  
  # Invalid character number should error
  expect_error(delta$set_char_type(999, 2))
  expect_error(delta$set_char_type(0, 2))
})

test_that("character type constants are correct", {
  types <- delta_char_types()
  
  expect_type(types, "integer")
  expect_true("unordered_multistate" %in% names(types))
  expect_true("ordered_multistate" %in% names(types))
  expect_true("integer_numeric" %in% names(types))
  expect_true("real_numeric" %in% names(types))
  expect_true("text" %in% names(types))
  
  # Check values
  expect_equal(types["unordered_multistate"], 2)
  expect_equal(types["ordered_multistate"], 3)
  expect_equal(types["integer_numeric"], 4)
  expect_equal(types["real_numeric"], 5)
  expect_equal(types["text"], 8)
})