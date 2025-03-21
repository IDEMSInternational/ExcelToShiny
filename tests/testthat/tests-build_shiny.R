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
  project_dir <- Sys.getenv("GITHUB_WORKSPACE", unset = getwd())
  
  shiny_process <- callr::r_bg(
    function(project_path) {
      setwd(project_path)
      message("Working directory in background: ", getwd())
      
      # Load your package
      devtools::load_all(project_path)
      
      # Load libraries
      library(shiny)
      library(shinydashboard)
      library(dplyr)
      library(stringdist)
      library(plotly)
      library(rio)
      library(NHANES)
      
      # Load data
      data(NHANES)
      NHANES_by_ind <- NHANES %>%
        group_by(ID) %>%
        mutate(count = 1:n()) %>%
        filter(count == 1) %>%
        ungroup()
      
      NHANES$ID <- as.character(NHANES$ID)
      NHANES_by_ind$ID <- as.character(NHANES_by_ind$ID)
      
      credentials_data <- data.frame(
        user = "admin",
        password = "password",
        stringsAsFactors = FALSE
      )
      
      example_excel <- rio::import_list("tests/testthat/testdata/nhanes_data.xlsx")
      
      # Your shiny app
      app <- ExcelToShiny::build_shiny(
        title = "Test Dashboard",
        data_list = example_excel,
        data_frame = NHANES,
        key_var = "ID",
        deploy_shiny = FALSE
      )
      
      shiny::runApp(shinyApp(ui = app$ui, server = app$server), launch.browser = TRUE)
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

test_app_runs_ok <- function(run_app_fn, delay_sec = 5, label = "Unnamed test") {
  shiny_process <- callr::r_bg(
    function(run_app_code) {
      eval(parse(text = run_app_code))
    },
    args = list(run_app_code = deparse(body(run_app_fn)))
  )
  
  Sys.sleep(delay_sec)
  
  if (!shiny_process$is_alive() && shiny_process$get_exit_status() != 0) {
    cat(paste0("\n❌ ", label, " failed:\n"))
    cat("STDOUT:\n", shiny_process$read_all_output(), "\n")
    cat("STDERR:\n", shiny_process$read_all_error(), "\n")
    shiny_process$kill()
    return(FALSE)
  }
  
  shiny_process$kill()
  cat(paste0("✅ ", label, " passed.\n"))
  return(TRUE)
}

test_that("build_shiny launches app correctly", {
  result <- test_app_runs_ok(function() {
    setwd(here::here())
    devtools::load_all()
    
    library(shiny)
    library(dplyr)
    library(rio)
    library(NHANES)
    
    data(NHANES)
    NHANES$ID <- as.character(NHANES$ID)
    example_excel <- rio::import_list("tests/testthat/testdata/nhanes_data.xlsx")
    
    app <- build_shiny(
      title = "Test Dashboard",
      data_list = example_excel,
      data_frame = NHANES,
      key_var = "ID",
      deploy_shiny = FALSE
    )
    
    shiny::runApp(shinyApp(ui = app$ui, server = app$server), launch.browser = FALSE)
  }, label = "build_shiny app test")
  
  expect_true(result)
})
