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





test_that("display_content_by_tab returns NULL if tab doesn't match", {
  input <- list(tab = "summary")
  output <- display_content_by_tab("details", input, reactive({mtcars}), NULL, NULL, NULL)
  expect_null(output)
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

# Helper Function 6: Render value boxes
test_that("draw_top_value_boxes works correctly", {
  spreadsheet_shiny_value_box <- data.frame(name = c("box1", "box2"))
  processed_spreadsheet_data <- data.frame(
    name = c("box1", "box2"),
    names = c("param1", "param2"),
    values = c("value1", "value2")
  )
  filtered_data <- reactive(data.frame(a = 1:3))
  list_of_reactives <- list()
  output <- list()
  
  expect_silent(draw_top_value_boxes(spreadsheet_shiny_value_box, processed_spreadsheet_data, filtered_data, list_of_reactives, output))
})

# Helper Function 7: Render download UI
# test_that("render_download_ui works correctly", {
#   j <- 1
#   spreadsheet <- data.frame(
#     type = c("Data label", "Download label", "Data"),
#     name = c("label1", "download1", "data1")
#   )
#   datasets <- list(data1 = data.frame(a = 1:3))
#   input <- list(dataset1 = "data1")
#   output <- list()
#   
#   expect_silent(render_download_ui(j, spreadsheet, datasets, input, output))
# })

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
  display_content <- reactive(list(
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
  display_content <- reactive(list(
    list(list(table_obj = "table1", plot_obj = "plot1")),
    list(list(table_obj = "table2", plot_obj = "plot2"))
  ))
  data_list <- list(contents = data.frame(type = c("Tabbed_display", "Tabbed_display")))
  output <- list()
  
  expect_silent(render_tabbed_display_items(display_box, display_content, data_list, output))
})
