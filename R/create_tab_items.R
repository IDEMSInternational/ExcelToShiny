#' Create Tab Items for Shiny Application
#'
#' This function generates tab items for a Shiny application based on the type of content specified in the `data_list`. It supports different types of sheets such as "Display", "Download", and "Tabbed_display".
#'
#' @param data_list A list containing the contents to be displayed in the tabs. It should include a `contents` data frame with columns such as `type` and `ID`.
#' @param d_box A list of box objects to be used in the display. These boxes correspond to the items in `data_list`.
#' @param status A character string indicating the status of the tab items, used for styling. Default is `"primary"`.
#' @param colour A character string specifying the colour theme for the tab items. Default is `"blue"`.
#'
#' @return A list of tab items ready to be integrated into a Shiny application.
create_tab_items <- function(data_list, d_box, status = "primary", colour = "blue"){
  my_tab_items <- NULL
  i_disp <- 1
  i_tb_disp <- 1

  for (i in 1:nrow(data_list$contents)){
    if (data_list$contents$type[[i]] == "Display"){
      my_tab_items[[i]] <- display_sheet(data_list = data_list,
                                         spreadsheet_name = data_list$contents$ID[[i]],
                                         d_box = d_box[[i_disp]],
                                         status = status,
                                         colour = colour,
                                         j = i)
      i_disp <- i_disp + 1
    } else if (data_list$contents$type[[i]] == "Download"){
      my_tab_items[[i]] <- download_sheet(data_list = data_list,
                                          spreadsheet_name = data_list$contents$ID[[i]],
                                          status = status,
                                          colour = colour,
                                          j = i)
    } else if (data_list$contents$type[[i]] == "Tabbed_display"){
      my_tab_items[[i]] <- tabbed_sheet(data_list = data_list,
                                        spreadsheet_name = data_list$contents$ID[[i]],
                                        d_box = d_box[[i]],
                                        status = status,
                                        colour = colour,
                                        j = i)
      
    }
  }
  return(my_tab_items)
}