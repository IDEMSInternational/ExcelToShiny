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
#' # Create and set up display sheets using server_display_sheet_setup
#' spreadsheet_data <- data.frame(...)  # Define your spreadsheet data
#' data_frame <- data.frame(...)  # Define your data frame
#' display_boxes <- server_display_sheet_setup(spreadsheet_data, data_frame, j = 1)
#'
#' # Render the display boxes in your Shiny app UI
#' shiny::uiOutput("display_boxes")
server_display_sheet_setup <- function(spreadsheet_data, data_frame, j, loop, list_of_reactives){
  # read in 
  spreadsheet_shiny_server <- spreadsheet_data %>% dplyr::filter(type %in% c("box"))
  s_box <- NULL
  if (is.null(loop)){
    for (i in 1:nrow(spreadsheet_shiny_server)) {
      # for each row in my sheet, create a box containing information.
      ID <- spreadsheet_shiny_server[i,]$name
      s_box[[i]] <- server_box_function(data_frame = data_frame, 
                                        spreadsheet = spreadsheet_data,
                                        unique_ID = ID,
                                        list_of_reactives = list_of_reactives)
    }
  } else {
    for (i in 1:nrow(spreadsheet_shiny_server)) {
      ID <- spreadsheet_shiny_server[i,]$name
      s_box[[i]] <- server_box_function(data_frame = data_frame, 
                                        spreadsheet = spreadsheet_data,
                                        unique_ID = ID,
                                        list_of_reactives = list_of_reactives)
    }
  }
  return(s_box)
}
