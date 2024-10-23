#' Setup Display Sheets for Shiny Application
#'
#' This function sets up display sheets for a Shiny application by creating boxes for each relevant item in the provided spreadsheet data. It handles the creation of labels for tables and plots, and filters the data to include only those entries marked as "box" type.
#'
#' @param spreadsheet_data A data frame containing the data for the spreadsheet, which includes the information required to set up the display boxes.
#' @param data_frame The main data frame containing the data to be displayed in the boxes.
#' @param j An integer index used for creating unique labels for tables and plots within the Shiny app.
#' @param loop An optional parameter used to handle nested loops for label creation. If not provided, the function will create labels without a loop suffix.
#'
#' @return A list of display boxes (`d_box`) ready to be integrated into a Shiny application.
display_sheet_setup <- function(spreadsheet_data, data_frame, j, loop) {
  # Filter the data once and store it in a variable
  filtered_spreadsheet_data <- spreadsheet_data %>% dplyr::filter(type == "box")
  
  # Define a helper function to create label names
  create_labels <- function(index) {
    suffix <- if (is.null(loop)) "" else paste0(loop, "_")
    list(
      label_table = paste0(suffix, "table_", j, "_", index),
      label_plot = paste0(suffix, "plot_", j, "_", index)
    )
  }
  
  # Use lapply for more concise code
  d_box <- lapply(seq_len(nrow(filtered_spreadsheet_data)), function(i) {
    ID <- filtered_spreadsheet_data[i,]$name
    labels <- create_labels(i)
      box_function(data_frame = data_frame, 
                   spreadsheet = filtered_spreadsheet_data,
                   unique_ID = ID,
                   label_table = labels$label_table,
                   label_plot = labels$label_plot)
  })
  return(d_box)
}