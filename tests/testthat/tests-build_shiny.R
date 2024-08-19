library(testthat)
library(PLHShiny)

test_that("create_shiny_dashboard creates a Shiny app", {
  # Test with a valid Excel file path
  excel_file <- system.file("extdata", "example_dashboard.xlsx", package = "PLHShiny")
  shiny_app <- build_shiny(excel_file)
  
  expect_true(is.list(shiny_app))
  expect_true("ui" %in% names(shiny_app))
  expect_true("server" %in% names(shiny_app))
  
  # Check that the UI and server functions are returned
  expect_true(is.function(shiny_app$ui))
  expect_true(is.function(shiny_app$server))
})

test_that("create_shiny_dashboard handles errors", {
  # Test with an invalid file path
  expect_error(create_shiny_dashboard("nonexistent_file.xlsx"))
})
