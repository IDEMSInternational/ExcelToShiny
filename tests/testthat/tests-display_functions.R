# testing:
# box_function
# display_contents
# display_sheet_setup
library(NHANES)
# Load the NHANES dataset
data(NHANES)

# Prepare the data by selecting individual records
NHANES_by_ind <- NHANES %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(count = 1:dplyr::n()) %>%
  dplyr::filter(count == 1) %>%
  dplyr::ungroup()

# Ensure that the ID column is in character format
NHANES_by_ind$ID <- as.character(NHANES_by_ind$ID)
example_excel <- rio::import_list("testdata/nhanes_data.xlsx")

test_that("display_contents processes and returns display boxes", {
  contents <- example_excel$contents
  data_list <- example_excel
  data_frame <- NHANES
  
  expect_error(display_contents(contents, data_frame, data_list), "Non-unique names given in `name` column")
})

test_that("display_sheet_setup filters and creates labels correctly", {
  spreadsheet_data <- example_excel$demographics
  spreadsheet_data$name <- 1:nrow(spreadsheet_data)
  data_frame <- NHANES
  
  display_boxes <- display_sheet_setup(spreadsheet_data, data_frame, j = 1, loop = NULL)
  expect_length(display_boxes, 7)
  
  spreadsheet_data_box <- spreadsheet_data[1,]
  
  result <- box_function(data_frame, spreadsheet_data_box, "1", "label_table", "label_plot")
  expect_equal(result$label_plot, "label_plot")
})





