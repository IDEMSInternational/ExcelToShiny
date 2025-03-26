# Testing server_display_contents
test_that("server_display_contents handles Display type correctly", {
  contents <- data.frame(ID = "box1", type = "Display", stringsAsFactors = FALSE)
  spreadsheet <- data.frame(name = "box1", type = "box", value = "bar_table", variable = "gear")
  
  data_list <- list(box1 = spreadsheet)
  dummy_df <- mtcars
  dummy_reactives <- list(df = function() dummy_df)
  
  result <- suppressWarnings(server_display_contents(contents1 = contents, data_frame = dummy_df,
                                    data_list = data_list, list_of_reactives = dummy_reactives,
                                    id_name = "box1"))
  
  expect_type(result, "list")
  expect_true(!is.null(result[[1]]))
})

test_that("server_display_contents handles Tabbed_display correctly", {
  contents <- data.frame(ID = "tab1", type = "Tabbed_display", stringsAsFactors = FALSE)
  tab_contents <- data.frame(ID = "box2", type = "Display", name = "box2", value = "bar_table", variable = "gear")
  
  data_list <- list(tab1 = tab_contents, box2 = tab_contents)
  dummy_df <- mtcars
  dummy_reactives <- list(df = function() dummy_df)
  
  result <- server_display_contents(contents1 = contents, data_frame = dummy_df,
                                    data_list = data_list, list_of_reactives = dummy_reactives,
                                    id_name = "tab1", k = 1)
  
  expect_type(result, "list")
  expect_true(!is.null(result[[1]]))
})

test_that("server_display_contents handles Download type", {
  contents <- data.frame(ID = "dl1", type = "Download", stringsAsFactors = FALSE)
  result <- server_display_contents(contents1 = contents, data_frame = mtcars,
                                    data_list = list(), list_of_reactives = list(),
                                    id_name = "dl1")
  expect_null(result)
})

test_that("server_display_contents stops on unknown type", {
  contents <- data.frame(ID = "bad1", type = "Unknown", stringsAsFactors = FALSE)
  expect_error(
    server_display_contents(contents1 = contents, data_frame = mtcars,
                            data_list = list(), list_of_reactives = list(),
                            id_name = "bad1"),
    "contents_type unrecognised"
  )
})

# Testing server_display_contents_tabbed
test_that("server_display_contents_tabbed processes multiple rows", {
  contents <- data.frame(
    ID = c("box1", "box2"),
    type = c("Display", "Display"),
    name = c("box1", "box2"),
    value = c("bar_table", "bar_table"),
    variable = c("gear", "carb"),
    stringsAsFactors = FALSE
  )
  
  data_list <- list(box1 = contents[1, ], box2 = contents[2, ])
  dummy_df <- mtcars
  dummy_reactives <- list(df = function() dummy_df)
  
  result <- server_display_contents_tabbed(contents1 = contents, data_frame = dummy_df,
                                           data_list = data_list, list_of_reactives = dummy_reactives, k = 1)
  expect_length(result, 2)
  expect_type(result, "list")
})

# Testing server_display_sheet_setup
test_that("server_display_sheet_setup generates boxes for type == 'box'", {
  spreadsheet_data <- data.frame(
    name = c("box1", "box2"),
    type = c("box", "box"),
    value = c("bar_table", "bar_table"),
    variable = c("gear", "carb"),
    stringsAsFactors = FALSE
  )
  
  dummy_df <- mtcars
  dummy_reactives <- list(df = function() dummy_df)
  
  result <- suppressWarnings(server_display_sheet_setup(spreadsheet_data = spreadsheet_data,
                                       data_frame = dummy_df,
                                       j = 1,
                                       loop = NULL,
                                       list_of_reactives = dummy_reactives))
  
  expect_type(result, "list")
  expect_length(result, 2)
})

# Testing server_box_function
test_that("server_box_function handles basic bar_table value", {
  spreadsheet <- data.frame(
    name = "box1",
    type = "box",
    value = "bar_table",
    variable = "gear",
    stringsAsFactors = FALSE
  )
  dummy_df <- mtcars
  dummy_reactives <- list(df = function() dummy_df)
  
  result <- suppressWarnings(server_box_function(data_frame = dummy_df,
                                spreadsheet = spreadsheet,
                                unique_ID = "box1",
                                list_of_reactives = dummy_reactives))
  
  expect_type(result, "list")
  expect_true("table_obj" %in% names(result))
  expect_s3_class(result$table_obj, "data.frame")
  expect_s3_class(result$plot_obj, "gg")
})

test_that("server_box_function returns NULL for unknown ID", {
  spreadsheet <- data.frame(
    name = "box1",
    type = "box",
    value = "bar_table",
    variable = "gear",
    stringsAsFactors = FALSE
  )
  dummy_df <- mtcars
  dummy_reactives <- list(df = function() dummy_df)
  
  result <- suppressWarnings(server_box_function(data_frame = dummy_df,
                                spreadsheet = spreadsheet,
                                unique_ID = "box_not_there",
                                list_of_reactives = dummy_reactives))
  
  expect_null(result)
})

test_that("server_box_function returns data_frame for 'data_frame' value", {
  spreadsheet <- data.frame(
    name = "box1",
    type = "box",
    value = "data_frame",
    variable = NA,
    stringsAsFactors = FALSE
  )
  dummy_df <- mtcars
  dummy_reactives <- list(df = function() dummy_df)
  
  result <- server_box_function(data_frame = dummy_df,
                                spreadsheet = spreadsheet,
                                unique_ID = "box1",
                                list_of_reactives = dummy_reactives)
  
  expect_equal(result$table_obj, dummy_df)
  expect_null(result$plot_obj)
})

test_that("server_box_function errors on unknown value", {
  spreadsheet <- data.frame(
    name = "box1",
    type = "box",
    value = "not_valid",
    variable = "gear",
    stringsAsFactors = FALSE
  )
  dummy_df <- mtcars
  dummy_reactives <- list(df = function() dummy_df)
  
  expect_error(server_box_function(data_frame = dummy_df,
                                   spreadsheet = spreadsheet,
                                   unique_ID = "box1",
                                   list_of_reactives = dummy_reactives),
               "Invalid value type.")
})
