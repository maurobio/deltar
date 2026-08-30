#==============================================================================
# helper.R - Helper functions for DeltaR tests
#==============================================================================

#' Get example file paths for testing
#'
#' @param dataset Character string: "viola" or "grass"
#' @return List with file paths
get_example_files <- function(dataset = "viola") {
  if (dataset == "viola") {
    list(
      chars = system.file("extdata", "chars_viola", package = "DeltaR"),
      items = system.file("extdata", "items_viola", package = "DeltaR"),
      specs = system.file("extdata", "specs_viola", package = "DeltaR")
    )
  } else if (dataset == "grass") {
    list(
      chars = system.file("extdata", "chars_grass", package = "DeltaR"),
      items = system.file("extdata", "items_grass", package = "DeltaR"),
      specs = system.file("extdata", "specs_grass", package = "DeltaR")
    )
  } else {
    stop("Unknown dataset: ", dataset)
  }
}

#' Check if example files exist
#'
#' @param dataset Character string: "viola" or "grass"
#' @return Logical indicating if all files exist
check_example_files <- function(dataset = "viola") {
  files <- get_example_files(dataset)
  all(file.exists(unlist(files)))
}

#' Skip test if example files are missing
#'
#' @param dataset Character string: "viola" or "grass"
skip_if_no_example_files <- function(dataset = "viola") {
  if (!check_example_files(dataset)) {
    skip(paste("Example files for", dataset, "not found"))
  }
}

#' Load test dataset
#'
#' @param dataset Character string: "viola" or "grass"
#' @param with_specs Logical, load specifications if available
#' @return DeltaParser object
load_test_dataset <- function(dataset = "viola", with_specs = TRUE) {
  files <- get_example_files(dataset)
  if (!file.exists(files$chars) || !file.exists(files$items)) {
    stop("Example files not found for dataset: ", dataset)
  }
  
  if (with_specs && file.exists(files$specs)) {
    load_delta(files$chars, files$items, files$specs)
  } else {
    load_delta(files$chars, files$items)
  }
}

#' Create a temporary DELTA file for testing
#'
#' @param content Character vector of file content
#' @param ext File extension
#' @return Path to temporary file
create_temp_delta_file <- function(content, ext = "txt") {
  tmp <- tempfile(fileext = paste0(".", ext))
  writeLines(content, tmp)
  tmp
}

#' Create a minimal test DELTA dataset
#'
#' @return List of temporary file paths
create_minimal_test_dataset <- function() {
  chars_content <- c(
    "#1. Character 1 /",
    "  1, state 1 /",
    "  2, state 2 /",
    "#2. Character 2 /",
    "  1, state 1 /",
    "  2, state 2 /"
  )
  
  items_content <- c(
    "#1. Item 1 /",
    "  1,1/2",
    "  2,1",
    "#2. Item 2 /",
    "  1,2",
    "  2,2"
  )
  
  specs_content <- c(
    "*CHARACTER TYPES 1-2, UM",
    "*IMPLICIT VALUES 1,1",
    "*DEPENDENT CHARACTERS 1,2:1"
  )
  
  list(
    chars = create_temp_delta_file(chars_content, "txt"),
    items = create_temp_delta_file(items_content, "txt"),
    specs = create_temp_delta_file(specs_content, "txt")
  )
}