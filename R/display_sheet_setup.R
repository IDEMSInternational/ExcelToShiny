#' Title
#'
#' @param spreadsheet_data todo
#' @param data_frame  todo
#' @param j  todo
#'
#' @return todo
#' @export
#'
#' @examples #todo
#' 
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