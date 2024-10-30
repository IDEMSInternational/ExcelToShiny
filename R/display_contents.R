#' Display Contents for Shiny Application
#'
#' This function processes and displays contents within a Shiny application based on the specifications provided in the `contents` data frame. It supports different types of displays, including standard "Display" sheets and "Tabbed_display" sheets.
#'
#' @param contents A data frame containing details about the contents to display. It should include columns such as `ID` for unique identifiers and `type` for the type of display.
#' @param data_frame The primary data frame containing the data to be displayed.
#' @param data_list A list containing the data for each content sheet, referenced by the `ID` from `contents`.
#' @param loop An optional parameter used for handling nested "Tabbed_display" types. Default is `NULL`.
#' @param k An integer or vector that controls the iteration for nested tabbed displays. Default is `1`.
#'
#' @return A list of display boxes ready to be integrated into a Shiny application.
#' 
display_contents <- function(contents = contents, data_frame, data_list, loop = NULL, k = 1){
  # Contents to display
  names_display <- contents[["ID"]]
  
  # Display type sheets ---
  sheets_to_display <- contents %>% dplyr::filter(type == "Display")
  
  # Tabbed-display type sheets
  sheets_to_td <- contents %>% dplyr::filter(type == "Tabbed_display")
  
  # Display and tabbed display contents:
  display_box <- NULL
  x <- NULL
  contents_type <- contents[["type"]]
  for (i in 1:nrow(contents)){
    spreadsheet <- data_list[[names_display[[i]]]]
    
    # # check unique names
    if (length(unique(spreadsheet$name)) != nrow(spreadsheet)){
      if (contents_type[[i]] != "Download"){
        stop(paste0("Non-unique names given in `name` column in ", names_display[[i]]))
      }
    }

    if (contents_type[[i]] == "Display"){
      display_box[[i]] <- display_sheet_setup(spreadsheet_data = spreadsheet,
                                              data_frame = data_frame,
                                              j = i,
                                              loop = loop)
    }
    if (contents_type[[i]] == "Tabbed_display"){
      k_orig <- dplyr::first(k)
      # todo: can we have a tabbed display in a tabbed display - if so, looping it.
      display_box[[i]] <- display_contents(contents = spreadsheet, data_frame = data_frame, data_list = data_list, loop = k_orig)
      k <- k[-1]
    }
  }
  return(display_box)
}