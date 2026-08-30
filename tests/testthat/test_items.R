#==============================================================================
# test_items.R - Tests for item functionality
#==============================================================================

context("Item functionality")

test_that("items_df returns correct structure", {
  skip_if_no_example_files("viola")
  
  delta <- load_test_dataset("viola", with_specs = FALSE)
  items_df_result <- items_df(delta)
  
  # Check structure
  expect_s3_class(items_df_result, "data.frame")
  expect_true(nrow(items_df_result) > 0)
  expect_true(ncol(items_df_result) >= 3)
  
  # Check column names
  expected_cols <- c("number", "name", "attribute_count")
  expect_true(all(expected_cols %in% colnames(items_df_result)))
  
  # Check types
  expect_type(items_df_result$number, "integer")
  expect_type(items_df_result$name, "character")
  expect_type(items_df_result$attribute_count, "integer")
})

test_that("item methods work correctly", {
  skip_if_no_example_files("viola")
  
  delta <- load_test_dataset("viola", with_specs = FALSE)
  n_items <- delta$item_count()
  
  expect_true(n_items > 0)
  
  # Test filename
  expect_true(nchar(delta$items_filename()) > 0)
  expect_true(delta$is_items_parsed())
  
  # Test each item
  for (i in 1:min(5, n_items)) {
    # Name should be non-empty
    name <- delta$item_name(i)
    expect_true(nchar(name) > 0)
    
    # Name with comments should work
    name_with_comments <- delta$item_name(i, TRUE)
    expect_true(is.character(name_with_comments))
    
    # Should have at least some attributes
    attr_count <- delta$attribute_count(i)
    expect_true(attr_count >= 0)
    
    # Get item data
    item_data <- get_item(delta, i)
    expect_type(item_data, "list")
    expect_true("item_num" %in% names(item_data))
    expect_true("item_name" %in% names(item_data))
    expect_true("attributes" %in% names(item_data))
    
    # If there are attributes, check them
    if (attr_count > 0) {
      attr_string <- delta$attribute_string(i, 1)
      expect_true(nchar(attr_string) > 0)
      
      # Get all attributes as data frame
      attrs <- delta$item_attributes(i)
      expect_s3_class(attrs, "data.frame")
      expect_true(nrow(attrs) == attr_count)
      expect_true("character" %in% colnames(attrs))
      expect_true("attribute" %in% colnames(attrs))
    }
  }
})

test_that("item_names returns all names", {
  skip_if_no_example_files("viola")
  
  delta <- load_test_dataset("viola", with_specs = FALSE)
  n_items <- delta$item_count()
  
  names_all <- delta$item_names()
  expect_type(names_all, "character")
  expect_equal(length(names_all), n_items)
  
  names_with_comments <- delta$item_names(TRUE)
  expect_type(names_with_comments, "character")
  expect_equal(length(names_with_comments), n_items)
})

test_that("get_item works correctly", {
  skip_if_no_example_files("viola")
  
  delta <- load_test_dataset("viola", with_specs = FALSE)
  
  if (delta$item_count() > 0) {
    item_data <- get_item(delta, 1)
    
    expect_type(item_data, "list")
    expect_true("item_num" %in% names(item_data))
    expect_true("item_name" %in% names(item_data))
    expect_true("attributes" %in% names(item_data))
    
    # With comments
    item_data_comments <- get_item(delta, 1, TRUE)
    expect_type(item_data_comments, "list")
  }
})