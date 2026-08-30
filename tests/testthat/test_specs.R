#==============================================================================
# test_specs.R - Tests for specifications functionality
#==============================================================================

context("Specifications functionality")

test_that("specifications load correctly", {
  skip_if_no_example_files("viola")
  
  files <- get_example_files("viola")
  
  if (file.exists(files$specs)) {
    delta <- load_delta(files$chars, files$items, files$specs)
    
    expect_true(delta$has_specifications())
    expect_true(delta$is_specs_parsed())
    expect_true(nchar(delta$specs_filename()) > 0)
  }
})

test_that("implicit_values works correctly", {
  skip_if_no_example_files("viola")
  
  files <- get_example_files("viola")
  
  if (file.exists(files$specs)) {
    delta <- load_delta(files$chars, files$items, files$specs)
    n_chars <- delta$char_count()
    
    # Test single value
    if (n_chars > 0) {
      iv <- delta$implicit_value(1)
      expect_true(is.integer(iv) || is.na(iv))
    }
    
    # Test all values
    all_iv <- delta$implicit_values()
    expect_type(all_iv, "integer")
    expect_equal(length(all_iv), n_chars)
    
    # Test type 2
    all_iv2 <- delta$implicit_values(type = 2)
    expect_type(all_iv2, "integer")
    expect_equal(length(all_iv2), n_chars)
    
    # Invalid character number
    expect_error(delta$implicit_value(999))
    expect_error(delta$implicit_value(0))
  }
})

test_that("dependent_characters works correctly", {
  skip_if_no_example_files("viola")
  
  files <- get_example_files("viola")
  
  if (file.exists(files$specs)) {
    delta <- load_delta(files$chars, files$items, files$specs)
    n_chars <- delta$char_count()
    
    if (n_chars > 0) {
      # Test dependent_count
      dep_count <- delta$dependent_count(1, 1)
      expect_true(is.integer(dep_count))
      expect_true(dep_count >= 0)
      
      # Test dependent_characters
      deps <- delta$dependent_characters(1, 1)
      expect_type(deps, "integer")
      expect_equal(length(deps), dep_count)
      
      # Test is_dependent
      if (n_chars >= 2) {
        is_dep <- delta$is_dependent(2, 1, 1)
        expect_true(is.logical(is_dep))
      }
    }
    
    # Test with invalid inputs
    expect_error(delta$dependent_count(999, 1))
    expect_error(delta$dependent_characters(0, 1))
  }
})

test_that("specifications functions handle missing specs gracefully", {
  skip_if_no_example_files("viola")
  
  # Load without specifications
  delta <- load_test_dataset("viola", with_specs = FALSE)
  
  expect_false(delta$has_specifications())
  expect_false(delta$is_specs_parsed())
  expect_equal(delta$specs_filename(), "")
  
  # Functions should error when specs not available
  expect_error(delta$implicit_value(1))
  expect_error(delta$implicit_values())
  expect_error(delta$dependent_count(1, 1))
})