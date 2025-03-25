library(dplyr)
library(ggplot2)

test_data <- mtcars
test_data$cyl <- factor(test_data$cyl)
test_data$gear <- factor(test_data$gear)

test_that("bar_table returns correct structure for freq", {
  out <- suppressWarnings(bar_table(data = test_data, variable = "gear", type = "freq", spreadsheet = list(), grouped_vars = "cyl"))
  expect_type(out, "list")
  expect_named(out, c("table", "plot"))
  expect_s3_class(out$plot, "gg")
  expect_s3_class(out$table, "data.frame")
})

test_that("bar_table handles list input", {
  dummy <- list(gear = head(test_data))
  out <- bar_table(data = dummy, variable = "gear", spreadsheet = list(), type = "freq")
  expect_s3_class(out$plot, "gg")
  expect_equal(out$table, dummy$gear)
})

test_that("bar_table handles spreadsheet commands", {
  spreadsheet <- list(data_manip = "%>% filter(mpg > 20)", graph_manip = "ggplot2::theme_minimal()")
  out <- suppressWarnings(bar_table(data = test_data, variable = "gear", spreadsheet = spreadsheet, grouped_vars = "cyl"))
  expect_s3_class(out$plot, "gg")
})

test_that("boxplot_table creates valid output", {
  out <- boxplot_table(data = test_data, variable = "mpg", spreadsheet = list(), grouped_vars = "cyl")
  expect_named(out, c("table", "plot"))
  expect_s3_class(out$plot, "gg")
})

test_that("scatter_table works with two numeric vars", {
  spreadsheet <- list(variable = "mpg, hp", graph_manip = NULL)
  out <- scatter_table(data = test_data, variable = NULL, spreadsheet = spreadsheet, grouped_vars = NULL)
  expect_named(out, c("table", "plot"))
  expect_s3_class(out$table, "data.frame")
  expect_s3_class(out$plot, "gg")
})

test_that("specify_plot handles minimal spreadsheet", {
  ss <- list(data_manip = "%>% filter(mpg > 20)", graph_manip = "geom_histogram(aes(x = mpg), bins = 10)")
  out <- specify_plot(test_data, ss)
  expect_s3_class(out$plot, "gg")
  expect_type(out$table, "character") # Can be "No Table Given"
})

test_that("specify_table works with basic table_manip", {
  ss <- list(table_manip = "%>% summarise(mean_mpg = mean(mpg, na.rm = TRUE))")
  out <- specify_table(test_data, ss)
  expect_s3_class(out$table, "data.frame")
})

test_that("bar_table handles invalid manipulation gracefully", {
  bad_ss <- list(data_manip = "%>% unknown_function()")
  expect_message(
    suppressWarnings(bar_table(data = test_data, variable = "gear", spreadsheet = bad_ss)),
    "Ignoring manipulations"
  )
})

