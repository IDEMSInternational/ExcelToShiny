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
  
  credentials_data <- data.frame(
    user = c("admin"),
    password = c("password"),
    stringsAsFactors = FALSE
  )  
  
  # Try launching the app
  # Create the app
  app <- build_shiny(
    title = "Test Dashboard",
    data_list = example_excel,
    data_frame = NHANES,
    key_var = "ID",
    deploy_shiny = TRUE
  )

  expect_silent({
    print("Starting app test...")  # Debugging print
    runApp(app, launch.browser = FALSE)  # Start app
    stopApp(app)  # Stop app immediately
    print("App ran successfully!")  # Debugging print
  })
  

  # Run the app in the background
  p <- processx::process$new(
    "Rscript",
    c("-e", "shiny::runApp('app')"),
    stderr = "|", stdout = "|"
  )
  
  Sys.sleep(5)  # Give it a few seconds to check if it runs
  
  # Check if the app is still running
  expect_true(p$is_alive())
  
  # Stop the app
  p$kill()
})