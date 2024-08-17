#' Server Display Contents
#'
#' This function processes and displays content within a Shiny application, including handling display sheets and tabbed displays. It organises data into a structured format suitable for visualisation.
#'
#' @param contents1 A list or data frame containing metadata and information about the content to be displayed.
#' @param data_frame The primary data frame used to generate the displayed content.
#' @param data_list A list of additional data frames used in various parts of the display.
#' @param loop Optional. A vector of loop indices for nested tabbed displays.
#' @param list_of_reactives A list of reactive expressions used to update content dynamically in Shiny.
#' @param id_name A string specifying the ID of the content to be displayed.
#' @param k Optional. An integer index for looping through tabbed displays.
#'
#' @return A list of display boxes that can be rendered in a Shiny UI.
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
  } else if (contents_type == "Download"){
    display_box <- NULL
  } else {
    stop("contents_type unrecognised")
  }
  return(display_box)
}
