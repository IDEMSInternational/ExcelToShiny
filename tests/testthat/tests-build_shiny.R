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


test_that("create_shiny_dashboard runs successfully", {
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
  NHANES$ID <- as.character(NHANES$ID)
  NHANES_by_ind$ID <- as.character(NHANES_by_ind$ID)
  example_excel <- rio::import_list("testdata/nhanes_data.xlsx")
  
  shiny_dashboard <- build_shiny(title = "New Dashboard", 
                                 data_list = example_excel,
                                 data_fram = NHANES,
                                 status = "primary",
                                 colour = "blue",
                                 key_var = "ID",
                                 deploy_shiny = FALSE)
  expect_equal(class(shiny_dashboard), "list")
  
  # # Manually inspect reactives
  # server_env <- environment(shiny_dashboard$server)
  # print(ls(server_env))  # List variables in the server function
  
  # Define the app-running background process
  project_dir <- here::here()
  shiny_process <- callr::r_bg(
    function(project_path) {
      setwd(project_path)
      library(shiny)
      library(shinydashboard)
      library(dplyr)
      library(stringdist)
      library(plotly)
      library(rio)
      library(NHANES)
      devtools::load_all()
      
      # Load data inside background session too!
      data(NHANES)
      # Prepare the data by selecting individual records
      NHANES_by_ind <- NHANES %>%
        dplyr::group_by(ID) %>%
        dplyr::mutate(count = 1:dplyr::n()) %>%
        dplyr::filter(count == 1) %>%
        dplyr::ungroup()
      
      # Ensure that the ID column is in character format
      NHANES$ID <- as.character(NHANES$ID)
      NHANES_by_ind$ID <- as.character(NHANES_by_ind$ID)
      
      credentials_data <- data.frame(
        user = c("admin"),
        password = c("password"),
        stringsAsFactors = FALSE
      )  
      
      example_excel <- rio::import_list("tests/testthat/testdata/nhanes_data.xlsx")
      
      app <- build_shiny(
        title = "Test Dashboard",
        data_list = example_excel,
        data_frame = NHANES,
        key_var = "ID",
        deploy_shiny = FALSE
      )
      
      shiny::runApp(shinyApp(ui = app$ui, server = app$server), launch.browser = FALSE)
    },
    args = list(project_dir)
  )
  
  # Give it time to start
  Sys.sleep(5)
  
  # Check that it started correctly
  expect_true(shiny_process$is_alive() || shiny_process$get_exit_status() == 0)
  
  # Optional: peek at logs if it failed
  if (!shiny_process$is_alive()) {
    cat("STDOUT:\n", shiny_process$read_all_output())
    cat("STDERR:\n", shiny_process$read_all_error())
  }
  
  # Kill the process to clean up
  shiny_process$kill()
  
})