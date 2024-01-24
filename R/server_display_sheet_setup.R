#' Server Display Sheet Setup
#'
#' This function sets up display sheets for a Shiny application server. It processes and filters spreadsheet data, creating display boxes for specific types of content, such as bar charts, box plots, and summaries.
#'
#' @param spreadsheet_data A data frame containing spreadsheet data.
#' @param data_frame The data frame used for creating display sheets.
#' @param j An index or identifier for the current display sheet.
#' @param loop A vector of loop indices, used for nested displays. Default is NULL.
#'
#' @return A list of display boxes for the specified content type.
#'
#' @export
#'
#' @examples
#' # todo
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
