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

test_that("boxplot_table handles list input and returns early", {
  dummy_data <- list(mpg = mtcars[1:3, ])
  result <- boxplot_table(data = dummy_data, variable = "mpg", spreadsheet = list())
  
  expect_type(result, "list")
  expect_s3_class(result$plot, "gg")
  expect_equal(result$table, dummy_data$mpg)
})

test_that("boxplot_table runs data manipulation if spreadsheet$data_manip is not NULL or NA", {
  spreadsheet <- list(data_manip = "%>% dplyr::filter(mpg > 20)")
  result <- boxplot_table(data = mtcars, variable = "mpg", spreadsheet = spreadsheet)
  
  expect_s3_class(result$plot, "gg")
  expect_s3_class(result$table, "data.frame")
  expect_true(all(result$table$Median >= median(mtcars$mpg[mtcars$mpg > 20])))
})

test_that("boxplot_table produces freq table if type is freq", {
  spreadsheet <- list()
  result <- boxplot_table(data = mtcars, variable = "cyl", type = "freq", spreadsheet = spreadsheet)
  
  expect_s3_class(result$table, "data.frame")
  expect_true(any(grepl("Count", names(result$table))) || any(grepl("cyl", names(result$table))))
})

test_that("boxplot_table skips table if spreadsheet$table_manip == 'none'", {
  spreadsheet <- list(table_manip = "none")
  result <- boxplot_table(data = mtcars, variable = "mpg", spreadsheet = spreadsheet)
  
  expect_equal(result$table, "No Table Given")
})

test_that("boxplot_table applies graph_manip if present", {
  spreadsheet <- list(graph_manip = "ggplot2::theme_minimal()")
  result <- boxplot_table(data = mtcars, variable = "mpg", spreadsheet = spreadsheet)
  
  expect_s3_class(result$plot, "gg")
})

test_that("boxplot_table handles invalid data_manip or graph_manip safely", {
  spreadsheet <- list(data_manip = "%>% mutate(wrong = logg(mpg))",
                      graph_manip = "ggtitle(TitleDoesNotWorkBecauseMissingQuote)")
  expect_message({
    result <- boxplot_table(data = mtcars, variable = "mpg", spreadsheet = spreadsheet)
  }, "Ignoring manipulations")
  
  expect_s3_class(result$plot, "gg")
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

test_that("bar_table handles list input and exits early", {
  dummy <- list(gear = mtcars[1:3, ])
  out <- bar_table(data = dummy, variable = "gear", spreadsheet = list())
  expect_s3_class(out$plot, "gg")
  expect_equal(out$table, dummy$gear)
})

test_that("bar_table performs data manipulation if spreadsheet$data_manip is provided", {
  spreadsheet <- list(data_manip = "%>% dplyr::filter(mpg > 25)")
  out <- suppressWarnings(bar_table(data = mtcars, variable = "gear", spreadsheet = spreadsheet))
  expect_true(nrow(out$table) <= nrow(mtcars))
})

test_that("bar_table adds graph manipulation if spreadsheet$graph_manip is provided", {
  spreadsheet <- list(graph_manip = "ggplot2::theme_minimal()")
  out <- suppressWarnings(bar_table(data = mtcars, variable = "gear", spreadsheet = spreadsheet))
  expect_s3_class(out$plot, "gg")
})

test_that("scatter_table handles spreadsheet$data_manip correctly", {
  ss <- list(variable = "mpg, hp", data_manip = "%>% dplyr::filter(mpg > 25)")
  out <- scatter_table(mtcars, variable = NULL, spreadsheet = ss)
  expect_s3_class(out$table, "data.frame")
})

test_that("scatter_table applies spreadsheet$graph_manip correctly", {
  ss <- list(variable = "mpg, hp", graph_manip = "ggplot2::theme_minimal()")
  out <- scatter_table(mtcars, variable = NULL, spreadsheet = ss)
  expect_s3_class(out$plot, "gg")
})

# test_that("specify_plot exits early if data is a list", {
#   spreadsheet <- list(graph_manip = "geom_point()", table_manip = NULL, data_manip = NULL)
#   out <- specify_plot(mtcars, spreadsheet)
#   expect_s3_class(out$plot, "gg")
# })

test_that("specify_plot applies spreadsheet$data_manip correctly", {
  spreadsheet <- list(
    data_manip = "%>% dplyr::filter(mpg > 25)",
    graph_manip = "geom_point(aes(x = mpg, y = hp))"
  )
  out <- specify_plot(mtcars, spreadsheet)
  expect_s3_class(out$plot, "gg")
})

test_that("specify_plot applies spreadsheet$graph_manip correctly", {
  spreadsheet <- list(
    graph_manip = "geom_histogram(aes(x = mpg), bins = 5)",
    data_manip = NULL
  )
  out <- specify_plot(mtcars, spreadsheet)
  expect_s3_class(out$plot, "gg")
  expect_true("GeomBar" %in% class(out$plot$layers[[1]]$geom))
})

test_that("specify_table exits early if data is a list", {
  spreadsheet <- list(table_manip = "%>% summarise(mean_mpg = mean(mpg))")
  out <- specify_table(mtcars, spreadsheet)
  expect_equal(out$table, mtcars %>% summarise(mean_mpg = mean(mpg)))
})

test_that("specify_table applies spreadsheet$table_manip correctly", {
  spreadsheet <- list(table_manip = "%>% summarise(mean_mpg = mean(mpg))")
  out <- specify_table(mtcars, spreadsheet)
  expect_s3_class(out$table, "data.frame")
  expect_true("mean_mpg" %in% names(out$table))
})

test_that("specify_table falls back to spreadsheet$data_manip if table_manip is missing", {
  spreadsheet <- list(
    table_manip = NULL,
    data_manip = "%>% summarise(med_mpg = median(mpg))"
  )
  out <- suppressWarnings(specify_table(mtcars, spreadsheet))
  expect_s3_class(out$table, "data.frame")
  expect_true("med_mpg" %in% names(out$table))
  
  expect_warning(specify_table(mtcars, spreadsheet), "Manipulations for specify_table are given in data_manip. These should be in table_manip.")
})

test_that("bar_table catches and handles error in data manipulation", {
  spreadsheet <- list(data_manip = "%>% non_existent_function()")
  expect_message(
    result <- suppressWarnings(bar_table(data = mtcars, variable = "gear", spreadsheet = spreadsheet)),
    "Ignoring manipulations"
  )
  expect_s3_class(result$plot, "gg")
  expect_s3_class(result$table, "data.frame")
})

test_that("bar_table catches and handles error in graph manipulation", {
  spreadsheet <- list(graph_manip = "this is not valid R code")
  expect_message(
    result <- suppressWarnings(bar_table(data = mtcars, variable = "gear", spreadsheet = spreadsheet)),
    "Error in evaluating graph manipulation"
  )
  expect_s3_class(result$plot, "gg")
})

test_that("scatter_table catches error in data manipulation", {
  ss <- list(variable = "mpg, hp", data_manip = "%>% dplyr::not_a_function()")
  expect_message(
    result <- scatter_table(mtcars, variable = NULL, spreadsheet = ss),
    "Ignoring manipulations"
  )
  expect_s3_class(result$table, "data.frame")
})

test_that("scatter_table catches error in graph manipulation", {
  ss <- list(variable = "mpg, hp", graph_manip = "broken + code + here")
  expect_message(
    result <- scatter_table(mtcars, variable = NULL, spreadsheet = ss),
    "Error in evaluating graph manipulation"
  )
  expect_s3_class(result$plot, "gg")
})


test_that("specify_plot catches error in data manipulation", {
  spreadsheet <- list(data_manip = "%>% invalid_code()", graph_manip = "geom_point(aes(x = mpg, y = hp))")
  expect_message(
    result <- specify_plot(mtcars, spreadsheet),
    "Ignoring manipulations"
  )
  expect_s3_class(result$plot, "gg")
})

test_that("specify_table handles invalid table_manip", {
  spreadsheet <- list(table_manip = "%>% doesnotexist()")
  expect_message(
    result <- specify_table(mtcars, spreadsheet),
    "Ignoring manipulations"
  )
  expect_type(result$table, "NULL")  # Falls back to unmodified
  
  spreadsheet <- list(table_manip = "doesnotexist()")
  expect_message(
    result <- specify_table(mtcars, spreadsheet),
    "Ignoring manipulations"
  )
  expect_type(result$table, "NULL")  # Falls back to unmodified
})

test_that("specify_plot catches error in graph manipulation", {
  spreadsheet <- list(table_manip = "%>% doesnotexist()")
  expect_message(
    result <- specify_plot(mtcars, spreadsheet),
    "Ignoring manipulations"
  )
  expect_type(result$table, "NULL")  # Falls back to unmodified
  
  spreadsheet <- list(table_manip = "doesnotexist()")
  expect_message(
    result <- specify_plot(mtcars, spreadsheet),
    "Ignoring manipulations"
  )
  expect_type(result$table, "NULL")  # Falls back to unmodified
  
  spreadsheet <- list(graph_manip = "geom_point(,,)", data_manip = NULL)
    result <- specify_plot(mtcars, spreadsheet)
    
  expect_s3_class(result$plot, "gg")
})

test_that("summary_calculation handles mean summaries with margins", {
  data <- mtcars
  data$cyl <- as.factor(data$cyl)  # factor is expected for grouping
  result <- summary_calculation(data = data,
                                factors = cyl,
                                columns_to_summarise = "mpg",
                                summaries = "mean",
                                include_margins = TRUE)
  
  expect_s3_class(result, "data.frame")
  expect_true("Total" %in% result$cyl)
  expect_true("mpg" %in% names(result))
  expect_gt(nrow(result), 1)
})

test_that("summary_calculation handles frequency summaries with margins", {
  data <- mtcars
  data$cyl <- as.factor(data$cyl)
  data$gear <- as.factor(data$gear)
  
  result <- summary_calculation(data = data,
                                factors = cyl,
                                columns_to_summarise = "gear",
                                summaries = "frequencies",
                                include_margins = TRUE,
                                together = FALSE)
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("gear", "cyl", "n", "perc") %in% names(result)))
  expect_true(any(result$cyl == "Total"))
  expect_true(any(result$gear == "Total"))
})


test_that("summary_table handles mean summaries with naming_convention", {
  data <- mtcars
  data$cyl <- as.factor(data$cyl)
  
  result <- summary_table(data = data,
                          factors = cyl,
                          columns_to_summarise = "mpg",
                          summaries = "mean",
                          naming_convention = TRUE)
  
  expect_s3_class(result, "data.frame")
  expect_true("cyl" %in% tolower(names(result)) || any(grepl("cyl", names(result))))
})

test_that("summary_table returns gt table when display_table = TRUE and summaries = frequencies", {
  data <- mtcars
  data$cyl <- as.factor(data$cyl)
  data$gear <- as.factor(data$gear)
  
  result <- summary_table(data = data,
                          factors = cyl,
                          columns_to_summarise = "gear",
                          summaries = "frequencies",
                          display_table = TRUE,
                          together = FALSE)
  
  expect_s3_class(result, "gt_tbl")
})

test_that("summary_table pivots wider when wider_table is TRUE and factors ≠ columns_to_summarise", {
  data <- mtcars
  data$cyl <- as.factor(data$cyl)
  data$gear <- as.factor(data$gear)
  
  result <- summary_table(data = data,
                          factors = cyl,
                          columns_to_summarise = "gear",
                          summaries = "frequencies",
                          display_table = FALSE,
                          wider_table = TRUE,
                          together = TRUE)
  
  expect_true(any(grepl("Cyl", names(result))))
})

test_that("summary_table relocates 'Total' column to the end if present", {
  data <- mtcars
  data$cyl <- as.factor(data$cyl)
  data$gear <- as.factor(data$gear)
  
  result <- summary_table(data = data,
                          factors = cyl,
                          columns_to_summarise = "gear",
                          summaries = "frequencies",
                          include_margins = TRUE,
                          together = TRUE)
  
  # Only meaningful if the pivot creates a "Total" column
  expect_true("Total" %in% names(result))
  expect_equal(names(result)[length(result)], "Total")
})

