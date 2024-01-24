#' Title TODO
#'
#' @param contents1  TODO
#' @param data_frame TODO
#' @param data_list TODO
#' @param loop TODO
#' @param k TODO
#'
#' @return TODO
#' @export
#'
#' @examples #todo
display_contents <- function(contents1 = contents, data_frame, data_list, loop = NULL, k = 1){
  # Contents to display
  names_display <- contents1[["ID"]]
  
  # Display type sheets ---
  sheets_to_display <- contents1 %>% dplyr::filter(type == "Display")
  
  # Tabbed-display type sheets
  sheets_to_td <- contents1 %>% dplyr::filter(type == "Tabbed_display")
  
  # Display and tabbed display contents:
  display_box <- NULL
  x <- NULL
  contents_type <- contents1[["type"]]
  for (i in 1:nrow(contents1)){
    if (contents_type[[i]] == "Display"){
      spreadsheet <- data_list[[names_display[[i]]]]
      display_box[[i]] <- display_sheet_setup(spreadsheet_data = spreadsheet,
                                              data_frame = data_frame,
                                              j = i,
                                              loop = loop)
    }
    if (contents_type[[i]] == "Tabbed_display"){
      k_orig <- dplyr::first(k)
      spreadsheet <- data_list[[names_display[[i]]]]
      # todo: can we have a tabbed display in a tabbed display - if so, looping it.
      display_box[[i]] <- display_contents(contents1 = spreadsheet, data_frame = data_frame, data_list = data_list, loop = k_orig)
      k <- k[-1]
    }
  }
  return(display_box)
}