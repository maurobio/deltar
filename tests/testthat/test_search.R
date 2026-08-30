#==============================================================================
# test_search.R - Tests for search and matching functionality
#==============================================================================

context("Search and matching")

test_that("find_matching_items works correctly", {
  skip_if_no_example_files("viola")
  
  delta <- load_test_dataset("viola", with_specs = FALSE)
  n_chars <- delta$char_count()
  n_items <- delta$item_count()
  
  if (n_chars > 0 && n_items > 0) {
    # Test with single value
    matches <- find_matching_items(delta, 1, 1)
    expect_type(matches, "integer")
    expect_true(length(matches) >= 0)
    
    # Test with multiple values
    matches2 <- find_matching_items(delta, 1, c(1, 2))
    expect_type(matches2, "integer")
    expect_true(length(matches2) >= length(matches))
    
    # Test with numeric vector from R
    values <- c(1, 2, 3)
    matches3 <- find_matching_items(delta, 1, values)
    expect_type(matches3, "integer")
  }
})

test_that("find_matching_items handles strict vs non-strict", {
  # Create minimal dataset for controlled testing
  files <- create_minimal_test_dataset()
  delta <- load_delta(files$chars, files$items)
  
  # Strict matching
  matches_strict <- find_matching_items(delta, 1, 1, strict = TRUE)
  # Non-strict matching (unknown values match)
  matches_nonstrict <- find_matching_items(delta, 1, 1, strict = FALSE)
  
  # Non-strict should return at least as many as strict
  expect_true(length(matches_nonstrict) >= length(matches_strict))
})

test_that("item_matches works correctly", {
  skip_if_no_example_files("viola")
  
  delta <- load_test_dataset("viola", with_specs = FALSE)
  n_items <- delta$item_count()
  n_chars <- delta$char_count()
  
  if (n_items > 0 && n_chars > 0) {
    # Test with valid item
    result <- item_matches(delta, 1, 1, 1)
    expect_true(is.logical(result))
    
    # Test with multiple values
    result2 <- item_matches(delta, 1, 1, c(1, 2))
    expect_true(is.logical(result2))
    
    # Test with different parameters
    result_strict <- item_matches(delta, 1, 1, 1, strict = TRUE)
    result_nonstrict <- item_matches(delta, 1, 1, 1, strict = FALSE)
    expect_true(is.logical(result_strict))
    expect_true(is.logical(result_nonstrict))
    
    # Invalid item number should error
    expect_error(item_matches(delta, 999, 1, 1))
    expect_error(item_matches(delta, 0, 1, 1))
  }
})

test_that("find_matching_items with invalid inputs", {
  skip_if_no_example_files("viola")
  
  delta <- load_test_dataset("viola", with_specs = FALSE)
  
  # Invalid character number should not crash
  expect_error(find_matching_items(delta, 999, 1))
  expect_error(find_matching_items(delta, 0, 1))
})

test_that("search results are reproducible", {
  skip_if_no_example_files("viola")
  
  delta <- load_test_dataset("viola", with_specs = FALSE)
  
  if (delta$char_count() > 0 && delta$item_count() > 0) {
    # Same search should give same results
    matches1 <- find_matching_items(delta, 1, 1)
    matches2 <- find_matching_items(delta, 1, 1)
    expect_identical(matches1, matches2)
  }
})