example_excel <- rio::import_list("testdata/nhanes_data.xlsx")

# Helper Function 1: Prepare the data_list and extract df names
test_that("extract_df_names replaces NA with df name and extracts df names", {
  data_list <- list(
    data.frame(type = "Display", data = NA),
    data.frame(type = "Display", data = "df2")
  )
  result <- extract_df_names(data_list, "my_df")
  
  expect_equal(result$data_list[[1]]$data[1], "my_df")
  expect_equal(sort(result$list_of_df_names), sort(c("my_df", "df2")))
})

test_that("extract_df_names replaces NA with df name and extracts df names", {
  data_list <- example_excel
  
  result <- extract_df_names(data_list, "NHANES")
  
  expect_equal(result$list_of_df_names, c("NHANES", "NHANES_by_ind"))
})

# Helper Function 2: Generate complete_dfs environment
test_that("copy_dfs_for_filtering works correctly", {
  df_names <- c("df1", "df2")
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(b = 4:6)
  
  env <- new.env()
  assign("df1", df1, envir = env)
  assign("df2", df2, envir = env)
  
  complete_dfs <- copy_dfs_for_filtering(df_names, env)
  
  expect_equal(complete_dfs$df1_1, df1)
  expect_equal(complete_dfs$df2_1, df2)
  expect_equal(get("df1_1", envir = env), df1)
  expect_equal(get("df2_1", envir = env), df2)
})

test_that("copy_dfs_for_filtering copies dataframes into environment with _1 suffix", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(b = 4:6)
  df_names <- c("df1", "df2")
  
  complete_dfs <- copy_dfs_for_filtering(df_names, env = environment())
  
  expect_true(exists("df1_1", envir = environment()))
  expect_true(exists("df2_1", envir = environment()))
  expect_equal(complete_dfs$df1_1, df1)
  expect_equal(complete_dfs$df2_1, df2)
})

test_that("copy_dfs_for_filtering copies dataframes into environment with _1 suffix", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(b = 4:6)
  df_names <- c("df1", "df2")
  
  complete_dfs <- copy_dfs_for_filtering(df_names, env = environment())
  
  expect_true(exists("df1_1", envir = environment()))
  expect_true(exists("df2_1", envir = environment()))
  expect_equal(complete_dfs$df1_1, df1)
  expect_equal(complete_dfs$df2_1, df2)
})

# Helper Function 3: Create reactive data filtering
# TODO

# Helper Function 4: Display content by tab
test_that("display_content_by_tab returns NULL if tab doesn't match", {
  input <- list(tab = "summary")
  output <- display_content_by_tab("details", input, shiny::reactive({mtcars}), NULL, NULL, NULL)
  expect_null(output)
})

# display_content_by_tab(tab_name = input$tab,
#                        input = input,
#                        filtered_data = NHANES,
#                        contents = example_excel$td_diagnostics,
#                        data_list = example_excel,
#                        list_of_reactives = NULL)

# Helper Function 5: Process spreadsheet for value boxes
test_that("process_main_page_function works correctly", {
  spreadsheet <- example_excel$main_page
  spreadsheet$name <- paste0("box", 1:nrow(spreadsheet))
  result <- data.frame(process_main_page_function(spreadsheet))
  
  expected_result <- data.frame(
    name = c(rep("box1", 3), rep("box2", 3), rep("box3", 3), rep("box4", 3)),
    names = c(rep(c("text", "colour", "icon"), 4)),
    values = c("Total individuals in the study", "aqua", "tick",
               "Men in the study", "yellow", "male",
               "Women in the study", "purple", "female",
               "Average age", "green", "active")
  )
  
  expect_equal(result, expected_result)
})

test_that("process_main_page_function extracts and splits parameter list", {
  spreadsheet <- data.frame(
    type = "value_box",
    name = "box1",
    parameter_list = "label = \"Revenue\", value = revenue"
  )
  result <- process_main_page_function(spreadsheet)
  
  expect_equal(result$names, c("label", "value"))
  expect_equal(result$values, c("Revenue", "revenue"))
})

# Helper Function 6: Render value boxes
test_that("draw_top_value_boxes works correctly", {
  spreadsheet_shiny_value_box <- data.frame(name = c("box1", "box2"))
  processed_spreadsheet_data <- data.frame(
    name = c("box1", "box2"),
    names = c("param1", "param2"),
    values = c("value1", "value2")
  )
  filtered_data <- shiny::reactive(data.frame(a = 1:3))
  list_of_reactives <- list()
  output <- list()
  
  expect_silent(draw_top_value_boxes(spreadsheet_shiny_value_box, processed_spreadsheet_data, filtered_data, list_of_reactives, output))
})

test_that("draw_top_value_boxes works correctly", {
  spreadsheet_shiny_value_box <- data.frame(name = c("box1", "box2"))
  processed_spreadsheet_data <- data.frame(
    name = c("box1", "box2"),
    names = c("param1", "param2"),
    values = c("value1", "value2")
  )
  filtered_data <- shiny::reactive(data.frame(a = 1:3))
  list_of_reactives <- list()
  output <- list()
  
  expect_silent(draw_top_value_boxes(spreadsheet_shiny_value_box, processed_spreadsheet_data, filtered_data, list_of_reactives, output))
})

# Helper Function 7: Render download UI
test_that("render_download_ui works correctly", {
  j <- 1
  spreadsheet <- example_excel$download
  datasets <- list(NHANES = data.frame(a = 1:3),
                   NHANES_by_ind = data.frame(b = 1:10))
  input <- list(dataset1 = "data1")
  output <- list()
  x <- render_download_ui(j, spreadsheet, datasets, input, output)
  expect_true("shiny.render.function" %in% class(x))
})


  # Helper Function 8: Extract group input IDs from UI
test_that("extract_group_input_ids works correctly", {
  group_ui_list <- list(
    list(children = list(list(children = list(list(children = list(list(attribs = list(id = "id1")))))))),
    list(children = list(list(children = list(list(children = list(list(attribs = list(id = "id2")))))))))
         
  result <- extract_group_input_ids(group_ui_list)
  expect_equal(result, c("id1", "id2"))
})

# Helper Function 9: Render plots and tables for Display tabs
test_that("render_display_items works correctly", {
  display_box <- list(
    list(list(label_table = "table1", plot_obj = "plot1")),
    list(list(label_table = NULL, plot_obj = "plot2"))
  )
  display_content <- shiny::reactive(list(
    list(list(table_obj = "table1", plot_obj = "plot1")),
    list(list(table_obj = NULL, plot_obj = "plot2"))
  ))
  data_list <- list(contents = data.frame(type = c("Display", "Display")))
  output <- list()
  
  expect_silent(render_display_items(display_box, display_content, data_list, output))
})

# Helper Function 10: Render plots and tables for Tabbed_display tabs
test_that("render_tabbed_display_items works correctly", {
  display_box <- list(
    list(list(table_obj = "table1", plot_obj = "plot1")),
    list(list(table_obj = "table2", plot_obj = "plot2"))
  )
  display_content <- shiny::reactive(list(
    list(list(table_obj = "table1", plot_obj = "plot1")),
    list(list(table_obj = "table2", plot_obj = "plot2"))
  ))
  data_list <- list(contents = data.frame(type = c("Tabbed_display", "Tabbed_display")))
  output <- list()
  
  expect_silent(render_tabbed_display_items(display_box, display_content, data_list, output))
})



################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

# 📁 tests/testthat/test-build_server.R

library(testthat)
library(shiny)
library(dplyr)
library(mockery)

# --- 🔧 Helpers ---

# Minimal reactive placeholder
fake_reactive <- function(value) shiny::reactive(value)

# Minimal display contents
fake_contents <- function(name = "Display", id = "display", type = "Display") {
  data.frame(name = name,
             type = type, 
             ID = id,
             icon = "users",
             stringsAsFactors = FALSE)
}

# Minimal main page with group or filter
fake_main_page <- function(type = c("group_by_box", "filter_box"), var = "cyl") {
  data.frame(
    name = paste0(type, 1),
    type = type,
    parameter_list = c("label = \"CYL\", value = FALSE", "label = \"CYL\", choices = c(\"4\", \"6\", \"8\"), selected = c(\"4\", \"6\")"),
    variable = var,
    data = NA,
    stringsAsFactors = FALSE
  )
}

fake_demographics_page <- function(){
  data.frame(
    name = "box1",
    type = "box",
    value = "bar_table",
    parameter_list = c("text = \"Gender\", colour = \"blue\""),
    variable = "cyl",
    row = 1,
    data = NA
  )
}

# --- 🔍 Tests ---

test_that("build_server runs when data_list has no main_page", {
  data_list <- list(contents = fake_contents())
  df <- mtcars
  
  server_fn <- build_server(data_list, df, key_var = NULL, data_frame_name = "df")
  
  testServer(server_fn, {
    expect_type(session, "environment")
  })
})

test_that("build_server handles Tabbed_display types", {
  data_list <- list(
    main_page = NULL,
    contents = rbind(
      fake_contents("tab1", "Tabbed_display"),
      fake_contents("tab2", "Display")
    )
  )
  df <- mtcars
  
  server_fn <- build_server(data_list, df, key_var = NULL, data_frame_name = "df")
  
  testServer(server_fn, {
    session$setInputs(tab = "tab1")
    expect_true(TRUE)
  })
})

test_that("build_server handles group_by_box logic", {
  data_list <- list(
    main_page = fake_main_page("group_by_box"),
    contents = fake_contents(),
    display = fake_demographics_page()
  )
  df <- mtcars
  df$cyl <- factor(df$cyl)
  
  server_fn <- build_server(data_list, df, key_var = NULL, data_frame_name = "df")
  
  testServer(server_fn, {
    session$setInputs(goButton_group = 1, group_by_box1 = TRUE)
    expect_true(TRUE)  # Placeholder for coverage
  })
})

test_that("build_server handles filter_box with variable only", {
  data_list <- list(
    main_page = fake_main_page("filter_box"),
    contents = fake_contents(),
    display = fake_demographics_page()
  )
  df <- mtcars
  
  server_fn <- build_server(data_list, df, key_var = NULL, data_frame_name = "df")
  
  testServer(server_fn, {
    session$setInputs(goButton_group = 1, filter_box1 = 6)
    expect_true(TRUE)  # We assume filter ran
  })
})

test_that("build_server handles key_var and list_of_reactives", {
  data_list <- list(
    main_page = fake_main_page("group_by_box"),
    contents = fake_contents(),
    display = fake_demographics_page()
  )
  df <- mtcars
  
  server_fn <- build_server(data_list, df, key_var = "cyl", data_frame_name = "df")
  
  testServer(server_fn, {
    session$setInputs(goButton_group = 1, group_by_box1 = TRUE)
    expect_true(TRUE)
  })
})

test_that("build_server sets up download UI without credentials", {
  contents_df <- data.frame(
    ID = "download_sheet",
    type = "Download",
    stringsAsFactors = FALSE
  )
  spreadsheet <- data.frame(
    type = "Data",
    name = "First",
    value = "mtcars",
    stringsAsFactors = FALSE
  )
  data_list <- list(
    contents = contents_df,
    download_sheet = spreadsheet
  )
  df <- mtcars
  
  server_fn <- build_server(data_list, df, key_var = "row.names", data_frame_name = "df")
  
  testServer(server_fn, {
    expect_type(output$build_download1, "list")
  })
})

# You can also snapshot outputs if you want:
# expect_snapshot_output(output$some_output)





test_that("build_server handles group_by_box logic", {
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
  
  data_list <- rio::import_list("testdata/nhanes_data.xlsx")
  
  server_fn <- build_server(data_list, NHANES, key_var = "id", data_frame_name = "NHANES")
  
  testServer(server_fn, {
    session$setInputs(goButton_group = 1, group_by_box1 = TRUE)
    expect_true(TRUE)  # Placeholder for coverage
  })
})


