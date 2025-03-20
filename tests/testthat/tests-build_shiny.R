# library(testthat)
# library(ExcelToShiny)
# 
#test_that("create_shiny_dashboard creates a Shiny app", {
#   # Test with a valid Excel file path
#   excel_file <- system.file("extdata", "example_dashboard.xlsx", package = "PLHShiny")
#   shiny_app <- build_shiny(excel_file)
#   
#   expect_true(is.list(shiny_app))
#   expect_true("ui" %in% names(shiny_app))
#   expect_true("server" %in% names(shiny_app))
#   
#   # Check that the UI and server functions are returned
#   expect_true(is.function(shiny_app$ui))
#   expect_true(is.function(shiny_app$server))
# })
# 
test_that("create_shiny_dashboard handles errors", {
  # Test with an invalid file path
  expect_error(build_shiny("nonexistent_file.xlsx"))
})

library(NHANES)
# Load the NHANES dataset
data(NHANES)

# Prepare the data by selecting individual records
NHANES_by_ind <- NHANES %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(count = 1:dplyr::n()) %>%
  dplyr::filter(count == 1) %>%
  dplyr::ungroup()

# Ensure that the ID column is in character format
NHANES_by_ind$ID <- as.character(NHANES_by_ind$ID)
example_excel <- rio::import_list("testdata/nhanes_data.xlsx")

test_that("create_shiny_dashboard runs successfully", {
  shiny_dashboard <- build_shiny(title = "New Dashboard", 
                                 data_list = example_excel,
                                 data_fram = NHANES,
                                 status = "primary",
                                 colour = "blue",
                                 key_var = "ID",
                                 deploy_shiny = FALSE)
  expect_equal(class(shiny_dashboard), "list")
})