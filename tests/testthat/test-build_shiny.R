library(NHANES)
library(ggplot2)
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
  example_excel$main_page <- rbind(example_excel$main_page, example_excel$main_page[2,])
  example_excel$main_page$variable_value[2] <- NA
  example_excel$main_page$variable_value[7] <- "MALE"
  
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
      
      example_excel <- rio::import_list("testdata/nhanes_data.xlsx")
      
      # Your shiny app
      app <- build_shiny(
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

test_that("build_shiny throws error for incorrect filter type", {
    data(economics)
    economics_data_list <- rio::import_list("testdata/economics_singledate_data.xlsx")
    economics_data_list$main_page$value <- c("mean_box", "mean_box", "date")
    expect_error(build_shiny(title = "New Dashboard", 
                                   data_list = economics_data_list,
                                   data_fram = economics,
                                   status = "primary",
                                   colour = "blue",
                                   key_var = "date",
                                   deploy_shiny = FALSE))
})


# test_that("build_shiny corrects for incorrect filter type if choices is given", {
#   data(NHANES)
#   NHANES_by_ind <- NHANES %>%
#     dplyr::group_by(ID) %>%
#     dplyr::mutate(count = 1:n()) %>%
#     dplyr::filter(count == 1) %>%
#     dplyr::ungroup()
# 
#   NHANES$ID <- as.character(NHANES$ID)
#   NHANES_by_ind$ID <- as.character(NHANES_by_ind$ID)
# 
#   credentials_data <- data.frame(
#     user = "admin",
#     password = "password",
#     stringsAsFactors = FALSE
#   )
# 
#   example_excel <- rio::import_list("testdata/nhanes_data.xlsx")
#   example_excel$main_page$value[5] <- "check"
#   # Your shiny app
#   expect_warning(build_shiny(
#     title = "Test Dashboard",
#     data_list = example_excel,
#     data_frame = NHANES,
#     key_var = "ID",
#     deploy_shiny = FALSE),
#     "Cannot read value check on main_page for filter_box. Setting as checkbox_group.")
# })


test_that("create_shiny_dashboard runs successfully for filter by date_range", {
  library(ggplot2)
  data(economics)
  economics_data_list <- rio::import_list("testdata/economics_data.xlsx")
  app <- build_shiny(title = "New Dashboard", 
                     data_list = economics_data_list,
                     data_fram = economics,
                     status = "primary",
                     colour = "blue",
                     key_var = "date",
                     deploy_shiny = FALSE)
  expect_equal(class(app), "list")
  
  #and with a range
  economics_data_list$main_page[3,]$value <- "date_range_group"
  economics_data_list$main_page[3,]$parameter_list <- 
    "label = \"date goes from\", start = \"1968-07-01\""
  app <- build_shiny(title = "New Dashboard", 
                                     data_list = economics_data_list,
                                     data_fram = economics,
                                     status = "primary",
                                     colour = "blue",
                                     key_var = "date",
                                     deploy_shiny = FALSE)
  expect_equal(class(app), "list")
})

test_that("create_shiny_dashboard runs successfully for filter by date_range", {
  library(ggplot2)
  data(economics)
  economics_data_list <- rio::import_list("testdata/economics_data.xlsx")
  shiny_dashboard <- build_shiny(title = "New Dashboard", 
                                 data_list = economics_data_list,
                                 data_fram = economics,
                                 status = "primary",
                                 colour = "blue",
                                 key_var = "date",
                                 deploy_shiny = FALSE)
  expect_equal(class(shiny_dashboard), "list")
  
  # when we don' have min/max/value given:
  economics_data_list$main_page$value[3] <- "date_group"
  economics_data_list$main_page$parameter_list[3] <- "label = \"Date\""
  shiny_dashboard <- build_shiny(title = "New Dashboard", 
                                 data_list = economics_data_list,
                                 data_fram = economics,
                                 status = "primary",
                                 colour = "blue",
                                 key_var = "date",
                                 deploy_shiny = FALSE)
  expect_equal(class(shiny_dashboard), "list")
  
  economics_data_list$main_page$value[3] <- "date_range_group"
  economics_data_list$main_page$parameter_list[3] <- "label = \"Date\", start = \"1968-07-01\", end = \"1967-07-01\", min = \"1967-07-01\", max = \"2015-04-01\""
  app <- build_shiny(title = "New Dashboard", 
                     data_list = economics_data_list,
                     data_fram = economics,
                     status = "primary",
                     colour = "blue",
                     key_var = "date",
                     deploy_shiny = FALSE)
  expect_equal(class(app), "list")
})


test_that("build_shiny runs successfully for incorrect values", {
  library(ggplot2)
  data(economics)
  economics_data_list <- rio::import_list("testdata/economics_data.xlsx")
  economics_data_list$main_page <- economics_data_list$main_page[1:2,]
  
  economics_data_list1 <- economics_data_list
  economics_data_list1$demographics$parameter_list[1] <- "text = \"Personal Consumption Expenditure (in billions of dollars)\""
  expect_warning(build_shiny(title = "New Dashboard", 
                                 data_list = economics_data_list1,
                                 data_fram = economics,
                                 status = "primary",
                                 colour = "lime",
                                 key_var = "date",
                                 deploy_shiny = FALSE),
                 "Valid colours are: blue, green, light blue, orange, red. Setting colour to 'blue'.")
  
  economics_data_list_1 <- economics_data_list
  economics_data_list_1$contents$type <- "none"
  expect_error(build_shiny(title = "New Dashboard", 
                                 data_list = economics_data_list_1,
                                 data_fram = economics,
                                 status = "primary",
                                 colour = "blue",
                                 key_var = "date",
                                 deploy_shiny = FALSE),
               "Cannot read contents type: none, Should be one of Display, Tabbed_display, Download,")

  economics_data_list_1 <- economics_data_list
  economics_data_list_1$main_page$type[1] <- "unknown_box"
  expect_error(build_shiny(title = "New Dashboard", 
                           data_list = economics_data_list_1,
                           data_fram = economics,
                           status = "primary",
                           colour = "blue",
                           key_var = "date",
                           deploy_shiny = FALSE),
               "Cannot read type: unknown_box, on main_page. Should be one of value_box, filter_box, group_by_box,")
  
  economics_data_list_1 <- economics_data_list
  economics_data_list_1$main_page <- rbind(economics_data_list_1$main_page, economics_data_list_1$main_page, economics_data_list_1$main_page)
  economics_data_list_1$main_page$value[1] <- "average_box"
    app <- build_shiny(title = "New Dashboard", 
                     data_list = economics_data_list_1,
                     data_fram = economics,
                     status = "primary",
                     colour = "blue",
                     key_var = "date",
                     deploy_shiny = FALSE)
  expect_equal(class(app), "list")
  
  # runs when there is no main_page
  economics_data_list_1 <- economics_data_list
  economics_data_list_1$main_page <- NULL
  app <- build_shiny(title = "New Dashboard", 
                     data_list = economics_data_list_1,
                     data_fram = economics,
                     status = "primary",
                     colour = "blue",
                     key_var = "date",
                     deploy_shiny = FALSE)
  expect_equal(class(app), "list")
})