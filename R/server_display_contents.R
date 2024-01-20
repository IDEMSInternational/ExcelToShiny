#' Server Display Contents
#'
#' This function processes and displays contents, including display and tabbed display sheets, in a Shiny application server. It allows you to organize and present data frames and data lists in a structured way.
#'
#' @param contents1 A list containing information about the content to be displayed.
#' @param data_frame The data frame used for displaying content.
#' @param data_list A list containing data associated with the content.
#' @param loop A vector of loop indices, used for nested tabbed displays. Default is NULL.
#' @param k An integer specifying the initial value for looping through tabbed displays. Default is 1.
#'
#' @return A list of display boxes, each containing the content to be displayed.
#'
#' @export
#'
#' @examples
#' # Create and display content using server_display_contents
#' content_list <- list(
#'   ID = c("content1", "content2"),
#'   type = c("Display", "Tabbed_display")
#'   # Add other content details here
#' )
#' data_frame <- data.frame(...)  # Define your data frame
#' data_list <- list(...)  # Define your data list
#' display_boxes <- server_display_contents(contents1 = content_list, data_frame = data_frame, data_list = data_list)
#'
#' # Render the display boxes in your Shiny app UI
#' shiny::uiOutput("display_boxes")
server_display_contents <- function(contents1 = contents, data_frame, data_list, loop = NULL, list_of_reactives, id_name, k = 1){
  
  # Contents to display
  i <- which(contents1$ID == id_name)
  
  # Display type sheets ---
  contents_type <- contents1[i,][["type"]]
  
  # Tabbed-display type sheets
  #sheets_to_td <- contents1 %>% dplyr::filter(type == "Tabbed_display")
  
  # Display and tabbed display contents:
  display_box <- NULL
  if (contents_type == "Display"){
    spreadsheet <- data_list[[id_name]]
    # read our display tab/sheet into server_display_sheet_setup
    display_box[[i]] <- server_display_sheet_setup(spreadsheet_data = spreadsheet,
                                                   data_frame = data_frame,
                                                   j = i,
                                                   loop = loop,
                                                   list_of_reactives = list_of_reactives)
  } else if (contents_type == "Tabbed_display"){
    k_orig <- dplyr::first(k)
    spreadsheet <- data_list[[id_name]]
    # todo: can we have a tabbed display in a tabbed display - if so, looping it.
    
    # TODO: we want to do lazy loading with the tabs
    # I think this means that this function is just for "Display" type
    # a "Tabbed Display" type runs this "Display" type depending on what tab is checked.
    # and we can check on plh_data function if it is display type or tabbed type.
    display_box[[i]] <- server_display_contents_tabbed(contents1 = spreadsheet,
                                                       data_frame = data_frame,
                                                       data_list = data_list,
                                                       loop = k_orig,
                                                       list_of_reactives = list_of_reactives)
    k <- k[-1]
  } else {
    stop("contents_type unrecognised")
  }
  return(display_box)
}
