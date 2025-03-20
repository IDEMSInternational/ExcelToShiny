# test set 1
#' add_na_variable
#' check_unknown_variables
#' check_variables_existence
#' get_parameter_value
#' naming_conventions
#' spreadsheet_finder
#' tagAssert
#' hasCSSclass

test_that("add_na_variable adds missing variables with NA", {
  df <- data.frame(a = 1:3, b = 4:6)
  df_new <- suppressWarnings(add_na_variable(df, c("b", "c")))
  
  expect_true("c" %in% colnames(df_new))
  expect_true(all(is.na(df_new$c)))
  expect_true("b" %in% colnames(df_new))
  expect_warning(add_na_variable(df, c("d")), "d does not exist. Adding NAs")
})

test_that("check_unknown_variables detects unknown variables", {
  df <- data.frame(variable = c("var1", "var2"), results = c(TRUE, FALSE), data = c("df1", "df2"))
  expect_error(check_unknown_variables(df), "Unknown variables in data frames")
  
  df_all_known <- data.frame(variable = c("var1", "var2"), results = c(TRUE, TRUE), data = c("df1", "df2"))
  expect_equal(check_unknown_variables(df_all_known), "No unknown variables.")
})

test_that("check_variables_existence identifies missing variables", {
  df <- data.frame(variable = c("a", "c"), data = c("df1", NA))
  df1 <- data.frame(a = 1:3, b = 4:6)
  assign("df1", df1, envir = .GlobalEnv)
  
  missing_vars <- check_variables_existence(df, "df1")
  expect_true("c" %in% missing_vars$variable)
  expect_true(all(missing_vars$results == FALSE))
})

test_that("get_parameter_value extracts correct values", {
  param_string <- 'label = "Value"'
  expect_equal(get_parameter_value(param_string), "Value")
  
  param_list <- 'options = c("Option1", "Option2", "Option3")'
  expect_equal(get_parameter_value(param_list, name = "options", list = TRUE), c("Option1", "Option2", "Option3"))
  
  param_logical <- 'is_active = TRUE'
  expect_equal(get_parameter_value(param_logical, name = "is_active", logical = TRUE), TRUE)
  
  param_missing <- 'other_param = 42'
  expect_null(get_parameter_value(param_missing))
  
  param_date <- 'date_value = as.Date("2025-03-20")'
  expect_equal(get_parameter_value(param_date, name = "date_value", date = TRUE), "2025-03-20")
})

test_that("naming_conventions standardizes names", {
  names <- c("prefix_var1_suffix", "prefix_var2_suffix")
  standardized <- naming_conventions(names, replace = "prefix_", replace_after = "_suffix")
  expect_equal(standardized, c("Var1", "Var2"))
  
  names_no_replace <- c("var1", "var2")
  standardized_no_replace <- naming_conventions(names_no_replace)
  expect_equal(standardized_no_replace, c("Var1", "Var2"))
})

test_that("spreadsheet_finder returns correct values", {
  data <- data.frame(names = c("col1", "col2"), values = c(10, 20))
  expect_equal(spreadsheet_finder(data, "col1"), 10)
  expect_equal(spreadsheet_finder(data, "col2"), 20)
  expect_null(spreadsheet_finder(data, "col3"))
})

test_that("tag_assert verifies tag properties", {
  tag <- shiny::tags$div(class = "test-class")
  expect_error(tag_assert(tag, type = "span"), "Expected tag to be of type")
  expect_error(tag_assert(tag, class = "wrong-class"), "Expected tag to have class")
  expect_silent(tag_assert(tag, type = "div", class = "test-class"))
  expect_error(tag_assert("not a tag"), "Expected an object with class 'shiny.tag'.")
})

test_that("hasCssClass detects class presence", {
  tag <- shiny::tags$div(class = "test-class another-class")
  expect_true(hasCssClass(tag, "test-class"))
  expect_true(hasCssClass(tag, "another-class"))
  expect_false(hasCssClass(tag, "nonexistent-class"))
  tag_no_class <- shiny::tags$div()
  expect_false(hasCssClass(tag_no_class, "test-class"))
})
