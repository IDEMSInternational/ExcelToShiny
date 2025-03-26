# 
# test_that("get_data_to_download evaluates strings to objects and names them correctly", {
#   
#   # Create example objects
#   df1 <- data.frame(a = 1:3)
#   df2 <- data.frame(b = letters[1:3])
#   
#   # Simulate data_to_download input
#   input <- data.frame(
#     name = c("First", "Second"),
#     value = c("df1", "df2"),
#     stringsAsFactors = FALSE
#   )
#   
#   result <- get_data_to_download(input, i = 1)
#   expect_type(result, "list")
#   expect_named(result, c("First", "Second"))
#   expect_equal(result$First, df1)
#   expect_equal(result$Second, df2)
# })

test_that("get_data_to_download returns empty list if input is empty", {
  input <- data.frame(name = character(), value = character(), stringsAsFactors = FALSE)
  result <- get_data_to_download(input, i = 1)
  expect_equal(result, list())
})

test_that("get_data_to_download handles invalid expression gracefully", {
  input <- data.frame(name = "Broken", value = "non_existent_object", stringsAsFactors = FALSE)
  expect_error(get_data_to_download(input, i = 1), "object 'non_existent_object' not found")
})
