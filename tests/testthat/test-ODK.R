
test_that("create_box_ODK generates correct data frame", {
  result <- create_box_ODK(
    type = "box",
    value = "boxplot_summary",
    parameter_list = "color=red",
    variable = "test_var",
    row = 2
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 5)
  expect_equal(result$type, "box")
  expect_equal(result$value, "boxplot_summary")
  expect_equal(result$parameter_list, "color=red")
  expect_equal(result$variable, "test_var")
  expect_equal(result$row, 2)
})

# Test create_from_type_ODK

test_that("create_from_type_ODK correctly processes metadata", {
  metadata <- data.frame(
    type = c("integer", "decimal", "select_one"),
    label = c("Age", "Height", "Gender"),
    name = c("age", "height", "gender"),
    stringsAsFactors = FALSE
  )
  
  result <- create_from_type_ODK(metadata)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(result$value %in% c("boxplot_summary", "bar_table")))
  expect_true(all(result$type == "box"))
  expect_true(all(!is.na(result$variable)))
})

# Test create_display_pages_from_metadata_ODK

test_that("create_display_pages_from_metadata_ODK filters correctly", {
  input_data <- data.frame(
    group = c("Identification", "Identification", "Other"),
    type = c("integer", "select_one", "decimal"),
    label = c("ID Number", "Gender", "Weight"),
    name = c("id", "gender", "weight"),
    stringsAsFactors = FALSE
  )
  
  result <- create_display_pages_from_metadata_ODK(input_data, data.frame(), group_name = "Identification")
  
  expect_s3_class(result, "data.frame")
  expect_true(all(result$variable %in% c("id", "gender")))
  expect_true(nrow(result) > 0)
})

# Test generate_data_list

test_that("generate_data_list creates structured list", {
  # Read in the data:
  ODK_sample_survey <- readxl::read_excel("testdata/ODK_test_survey.xlsx")
  #ODK_sample_survey_choices <- read_excel("C:/Users/lclem/Downloads/ODK_sample_survey.xlsx", sheet = "choices")
  our_survey_data <- readxl::read_excel("testdata/ODK_test_data.xlsx")
  
  # just for some sample rows
  ODK_sample_survey_1 <- ODK_sample_survey
  
  result <- generate_data_list(input_data = ODK_sample_survey_1, output_data = our_survey_data)

  expect_type(result, "list")
  expect_true("contents" %in% names(result))
  expect_true("main_page" %in% names(result))
  expect_s3_class(result$contents, "data.frame")
  expect_s3_class(result$main_page, "data.frame")
  expect_true(length(result) > 2)  # Ensures additional display pages exist
})

# Additional edge cases

test_that("create_from_type_ODK handles empty metadata", {
  metadata <- data.frame(type = character(), label = character(), name = character(), stringsAsFactors = FALSE)
  result <- create_from_type_ODK(metadata)
  expect_null(result)
})

test_that("create_display_pages_from_metadata_ODK handles no matching group", {
  input_data <- data.frame(
    group = c("Other"),
    type = c("integer"),
    label = c("Weight"),
    name = c("weight"),
    stringsAsFactors = FALSE
  )
  result <- create_display_pages_from_metadata_ODK(input_data, data.frame(), group_name = "Identification")
  expect_null(result)
})
