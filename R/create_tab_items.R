# what kind of sheet is it?
#' Title
#'
#' @param data_list   todo
#' @param d_box   todo
#' @param status   todo
#' @param colour   todo
#'
#' @return  todo
#' @export
#'
#' @examples  #todo
create_tab_items <- function(data_list, d_box, status, colour){
  my_tab_items <- NULL
  i_disp <- 1
  
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
                                          j = i)
    }
  }
  
  return(my_tab_items)
}