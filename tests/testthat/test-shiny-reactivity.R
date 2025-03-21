test_that("prepare_data_list replaces NA with df name and extracts df names", {
  data_list <- list(
    data.frame(type = "Display", data = NA),
    data.frame(type = "Display", data = "df2")
  )
  result <- prepare_data_list(data_list, "my_df")
  
  expect_equal(result$data_list[[1]]$data[1], "my_df")
  expect_equal(sort(result$list_of_df_names), sort(c("my_df", "df2")))
})

test_that("generate_complete_dfs copies dataframes into environment with _1 suffix", {
  df1 <- data.frame(a = 1:3)
  df2 <- data.frame(b = 4:6)
  df_names <- c("df1", "df2")
  
  complete_dfs <- generate_complete_dfs(df_names, env = environment())
  
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

test_that("process_spreadsheet_function extracts and splits parameter list", {
  spreadsheet <- data.frame(
    type = "value_box",
    name = "box1",
    parameter_list = "label = \"Revenue\", value = revenue"
  )
  result <- process_spreadsheet_function(spreadsheet)
  
  expect_equal(result$names, c("label", "value"))
  expect_equal(result$values, c("Revenue", "revenue"))
})