#' Server Display Sheet Setup
#'
#' This function sets up the content and layout for display sheets within a Shiny server. It processes the spreadsheet data and prepares it for visualisation in a Shiny app.
#'
#' @param spreadsheet_data A data frame containing the metadata and instructions for creating the display.
#' @param data_frame The main data frame from which the content will be generated.
#' @param j An integer or index for identifying the specific display sheet.
#' @param loop Optional. A vector of loop indices for handling nested displays.
#' @param list_of_reactives A list of reactive expressions used to dynamically update content in Shiny.
#'
#' @return A list of display boxes for the specified content type.
server_display_sheet_setup <- function(spreadsheet_data, data_frame, j, loop, list_of_reactives) {
  # Filter the data once and store it in a variable
  filtered_spreadsheet_data <- spreadsheet_data %>% dplyr::filter(type %in% c("box"))
  
  # Define a helper function to create label names
  create_labels <- function(index) {
    suffix <- if (is.null(loop)) "" else paste0(loop, "_")
    list(
      label_table = paste0(suffix, "table_", j, "_", index),
      label_plot = paste0(suffix, "plot_", j, "_", index)
    )
  }
  
  # Use lapply for more concise code
  s_box <- lapply(seq_len(nrow(filtered_spreadsheet_data)), function(i) {
    ID <- filtered_spreadsheet_data[i,]$name
    labels <- create_labels(i)
    server_box_function(data_frame = data_frame, 
                        spreadsheet = filtered_spreadsheet_data,
                        unique_ID = ID,
                        list_of_reactives = list_of_reactives)
  })
  
  return(s_box)
}
