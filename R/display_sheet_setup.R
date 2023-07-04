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
display_sheet_setup <- function(spreadsheet_data, data_frame, j, loop){
  # read in 
  spreadsheet_shiny_box <- spreadsheet_data %>% dplyr::filter(type %in% c("bar_table", "boxplot_table"))
  d_box <- NULL
  if (is.null(loop)){
    for (i in 1:nrow(spreadsheet_shiny_box)) {
      ID <- spreadsheet_shiny_box[i,]$name
      d_box[[i]] <- box_function(data_frame = data_frame, 
                                 spreadsheet = spreadsheet_data,
                                 unique_ID = ID,
                                 label_table = paste0("table_", j, "_", i),
                                 label_plot = paste0("plot_", j, "_", i))
    }
  } else {
    for (i in 1:nrow(spreadsheet_shiny_box)) {
      ID <- spreadsheet_shiny_box[i,]$name
      d_box[[i]] <- box_function(data_frame = data_frame, 
                                 spreadsheet = spreadsheet_data,
                                 unique_ID = ID,
                                 label_table = paste0(loop, "_table_", j, "_", i),
                                 label_plot = paste0(loop, "_plot_", j, "_", i))
    }
  }
  print(names(d_box[[1]]))
  return(d_box)
}
