#==============================================================================
# test_utils.R - Tests for utility functions
#==============================================================================

context("Utility functions")

test_that("strip_comments works correctly", {
  # Test simple comment
  result <- strip_comments("Item name <with comment>")
  expect_equal(result, "Item name ")
  
  # Test nested comment
  result <- strip_comments("Character <nested <comment>> feature")
  expect_equal(result, "Character  feature")
  
  # Test string without comments
  result <- strip_comments("No comments here")
  expect_equal(result, "No comments here")
  
  # Test multiple comments
  result <- strip_comments("<first>text<second>")
  expect_equal(result, "text")
  
  # Test empty string
  result <- strip_comments("")
  expect_equal(result, "")
})

test_that("parse_delta_attribute works correctly", {
  # Test simple attribute
  result <- parse_attribute("1,2/3")
  expect_equal(result$character, 1)
  expect_equal(length(result$alternatives), 2)
  expect_equal(result$alternatives[1], "2")
  expect_equal(result$alternatives[2], "3")
  
  # Test attribute with comment
  result <- parse_attribute("2,1<comment>/2")
  expect_equal(result$character, 2)
  expect_equal(length(result$alternatives), 2)
  expect_equal(result$alternatives[1], "1<comment>")
  expect_equal(result$alternatives[2], "2")
  
  # Test attribute with special values
  result <- parse_attribute("3,-")
  expect_equal(result$character, 3)
  expect_equal(result$alternatives[1], "-")
  
  result <- parse_attribute("4,V")
  expect_equal(result$character, 4)
  expect_equal(result$alternatives[1], "V")
  
  result <- parse_attribute("5,U")
  expect_equal(result$character, 5)
  expect_equal(result$alternatives[1], "U")
  
  # Test invalid attribute (no comma)
  result <- parse_attribute("invalid")
  expect_true(is.na(result$character))
  expect_equal(length(result$alternatives), 1)
  expect_equal(result$alternatives[1], "invalid")
})

test_that("parse_attribute handles edge cases", {
  # Test empty string
  result <- parse_attribute("")
  expect_true(is.na(result$character))
  expect_equal(result$values, "")
  
  # Test only comma
  result <- parse_attribute(",")
  expect_true(is.na(result$character))
  expect_equal(result$values, "")
})

test_that("strip_comments and parse_attribute work together", {
  # Parse an attribute with comments, then strip comments
  attr <- "2,1<comment>/2"
  parsed <- parse_attribute(attr)
  stripped <- strip_comments(parsed$alternatives[1])
  expect_equal(stripped, "1")
})

test_that("delta_char_types returns correct values", {
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