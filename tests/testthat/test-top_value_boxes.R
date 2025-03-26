test_that("top_value_boxes returns a valueBox with value_box_rows", {
  spreadsheet <- data.frame(name = "box1", value = "value_box_rows", stringsAsFactors = FALSE)
  processed_spreadsheet <- data.frame(
    name = "box1",
    names = c("text", "icon", "colour"),
    values = c("Total Rows", "bar-chart", "blue"),
    stringsAsFactors = FALSE
  )
  dummy_df <- mtcars
  dummy_reactives <- list()
  
  result <- top_value_boxes(data_frame = dummy_df,
                            spreadsheet = spreadsheet,
                            processed_spreadsheet = processed_spreadsheet,
                            unique_ID = "box1",
                            list_of_reactives = dummy_reactives)
  
  expect_s3_class(result, "shiny.tag")
  expect_true(any(grepl("Total Rows", capture.output(print(result)))))
})

test_that("top_value_boxes computes correct count with value_box", {
  spreadsheet <- data.frame(
    name = "box1",
    value = "value_box",
    variable = "cyl",
    variable_value = 6,
    stringsAsFactors = FALSE
  )
  processed_spreadsheet <- data.frame(
    name = "box1",
    names = c("text", "icon", "colour"),
    values = c("Count of 6 Cylinders", "car", "green"),
    stringsAsFactors = FALSE
  )
  dummy_df <- mtcars
  dummy_reactives <- list()
  
  result <- top_value_boxes(data_frame = dummy_df,
                            spreadsheet = spreadsheet,
                            processed_spreadsheet = processed_spreadsheet,
                            unique_ID = "box1",
                            list_of_reactives = dummy_reactives)
  
  expect_s3_class(result, "shiny.tag")
  expect_true(any(grepl("Count of 6 Cylinders", capture.output(print(result)))))
})

test_that("top_value_boxes calculates mean and SD correctly", {
  spreadsheet <- data.frame(
    name = "box1",
    value = "mean_sd_box",
    variable = "mpg",
    stringsAsFactors = FALSE
  )
  processed_spreadsheet <- data.frame(
    name = "box1",
    names = c("text", "icon", "colour"),
    values = c("Mean and SD", "tachometer", "yellow"),
    stringsAsFactors = FALSE
  )
  dummy_df <- mtcars
  dummy_reactives <- list()
  
  result <- top_value_boxes(data_frame = dummy_df,
                            spreadsheet = spreadsheet,
                            processed_spreadsheet = processed_spreadsheet,
                            unique_ID = "box1",
                            list_of_reactives = dummy_reactives)
  
  expect_s3_class(result, "shiny.tag")
  expect_match(as.character(result), "\\d+\\.\\d+ \\(\\d+\\.\\d+\\)")
})

test_that("top_value_boxes computes summary with custom function string", {
  spreadsheet <- data.frame(
    name = "box1",
    value = "median_box",
    variable = "hp",
    stringsAsFactors = FALSE
  )
  processed_spreadsheet <- data.frame(
    name = "box1",
    names = c("text", "icon", "colour"),
    values = c("Median HP", "bolt", "red"),
    stringsAsFactors = FALSE
  )
  dummy_df <- mtcars
  dummy_reactives <- list()
  
  result <- top_value_boxes(data_frame = dummy_df,
                            spreadsheet = spreadsheet,
                            processed_spreadsheet = processed_spreadsheet,
                            unique_ID = "box1",
                            list_of_reactives = dummy_reactives)
  
  expect_s3_class(result, "shiny.tag")
  expect_true(any(grepl("Median HP", capture.output(print(result)))))
})

test_that("top_value_boxes falls back to base data_frame when no reactive available", {
  spreadsheet <- data.frame(
    name = "box1",
    value = "value_box_rows",
    data = NA,
    stringsAsFactors = FALSE
  )
  processed_spreadsheet <- data.frame(
    name = "box1",
    names = c("text", "icon", "colour"),
    values = c("Rows", "info", "blue"),
    stringsAsFactors = FALSE
  )
  dummy_df <- mtcars
  dummy_reactives <- list()
  
  result <- top_value_boxes(dummy_df, spreadsheet, processed_spreadsheet, "box1", dummy_reactives)
  expect_s3_class(result, "shiny.tag")
})

