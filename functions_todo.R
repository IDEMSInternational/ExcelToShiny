menu_items <- function(contents_list = data_list$contents){
  add_menu <- NULL
  for (i in 1:nrow(contents_list)){
    add_menu[[i]] <- menuItem(contents_list$name[[i]], tabName = contents_list$ID[[i]], icon = icon(contents_list$icon[[i]]))
  }
  return(add_menu)
}

# for (i in 1:length(data_list$contents)){
#   sidebarMenu(menu_items(data_list$contents)[[1]],
#               menu_items(data_list$contents)[[2]])
# }

            



# d_box <- display_sheet_setup(spreadsheet_data = data_l$demographics, data_frame = srh_df, j = 1)
# data_list <- data_l
# status <- "primary"
# colour <- "blue"
#   j <- 1
#   
#   
#   data_l$demographics
#   
# 
#   
# 
# 
# 
#   shiny::fluidRow(shiny::column(12,
#                                 align = "center",
#                                 split_row[[1]]),
#                   width = 10)
#   
#   
#      





# tab_items <- function(i = 2, aa = a, x = htmltools::tagList(a[[1]], a[[2]])){
#   if (length(aa) > i) {
#     i <- i + 1
#     x <- htmltools::tagList(x, aa[[i]])
#     if (length(aa) > i){
#       test_fun(i = i, x = x)
#     } else {
#       return(x)
#     }
#   } else {
#     return(x)
#   }
# }