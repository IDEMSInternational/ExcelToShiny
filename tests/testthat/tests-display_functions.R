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
data_frame <- NHANES

test_that("display_contents processes and returns display boxes", {
  contents <- example_excel$contents
  data_list <- example_excel
  
  expect_error(display_contents(contents, data_frame, data_list), "Non-unique names given in `name` column")
})

test_that("display_sheet_setup and box_function filter and creates labels correctly", {
  spreadsheet_data <- example_excel$demographics
  spreadsheet_data$name <- 1:nrow(spreadsheet_data)
  data_frame <- NHANES
  
  display_boxes <- display_sheet_setup(spreadsheet_data, data_frame, j = 1, loop = NULL)
  expect_length(display_boxes, 7)
  
  spreadsheet_data_box <- spreadsheet_data[1,]
  
  result <- box_function(data_frame, spreadsheet_data_box, "1", "label_table", "label_plot")
  expect_equal(result$label_plot, "label_plot")
})

test_that("display_sheet generates tabItem correctly", {
  data_list <- list(contents = data.frame(ID = c("sheet1"), name = c("Sheet 1")))
  spreadsheet <- data.frame(name = "row1", row = 1)
  data_list[["sheet1"]] <- spreadsheet
  d_box <- list(list(ID = "row1", gui_obj = "Box"))
  
  tab <- display_sheet(data_list, "sheet1", d_box)
  expect_true(inherits(tab, "shiny.tag"))
})

test_that("download_sheet generates download tab correctly", {
  data_list <- list(contents = data.frame(ID = c("download1"), name = c("Download Tab"), icon = c("download")))
  spreadsheet <- data.frame(name = "Download Data", type = "Data")
  data_list[["download1"]] <- spreadsheet
  
  tab <- download_sheet(data_list, "download1")
  expect_true(inherits(tab, "shiny.tag"))
})


# Setting up (pre-UI and pre-server items) --------------------------------
# Check the types in contents are all valid types (display, tabbed_display, and download)
example_excel$contents <- example_excel$contents %>%
  dplyr::mutate(type = ifelse(stringdist::stringdist(type, "Display", method = "lv") <= 2, "Display",
                              ifelse(stringdist::stringdist(type, "Tabbed_display", method = "lv") <= 3, "Tabbed_display",
                                     ifelse(stringdist::stringdist(type, "Download", method = "lv") <= 3, "Download",
                                            type))))
valid_contents_type <- c("Display", "Tabbed_display", "Download")
if (!all(example_excel$contents$type %in% valid_contents_type)){
  invalid_contents_type <- example_excel$contents %>%
    dplyr::filter(!type %in% valid_contents_type) %>%
    dplyr::pull(type)
  stop("Cannot read contents type: ", paste0(invalid_contents_type, sep = ", "), "Should be one of ", paste0(valid_contents_type, sep = ", "))
}
contents <- example_excel$contents

for (i in 1:length(example_excel)){
  if (is.null(example_excel[[i]]$name)){
    example_excel[[i]]$name <- paste0("box", 1:nrow(example_excel[[i]]))
  }
}

# list of sheets
# check the variables exist
for (df_name in names(example_excel)){
  sheet <- example_excel[[df_name]]
  data_frame_name <- deparse(substitute(NHANES))
  results <- NULL
  results <- check_variables_existence(sheet, data_frame = data_frame_name)
  if (!is.null(results) && nrow(results) > 0) check_unknown_variables(results)
}

test_that("create_tab_items generates tab items correctly", {
  # Populate items for the tabs ------------------------------------------------
  display_box <- display_contents(data_frame = NHANES, contents = contents, data_list = example_excel, k = which(example_excel$contents$type == "Tabbed_display"))
  expect_equal(class(display_box), "list")
  expect_equal(length(display_box), 2)
  
  my_tab_items <- create_tab_items(data_list = data_list,
                                   d_box = display_box,
                                   status = "primary",
                                   colour = "blue")
  expect_length(display_box[[1]], 7)
  expect_length(display_box[[2]], 2)
  expect_length(display_box, 2)
})