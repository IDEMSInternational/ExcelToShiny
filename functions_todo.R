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

            

display_sheet_setup <- function(spreadsheet_data, data_frame, j){
  spreadsheet_shiny_box <- spreadsheet_data %>% dplyr::filter(type == "box")
  d_box <- NULL
  for (i in 1:nrow(spreadsheet_shiny_box)) {
    ID <- spreadsheet_shiny_box[i,]$name
    d_box[[i]] <- box_function(data_frame = data_frame, 
                                   spreadsheet = spreadsheet_data,
                                   unique_ID = ID,
                                   label_table = paste0("table_", j, "_", i),
                                   label_plot = paste0("plot_", j, "_", i))
  }
  if (i < 9) {
    for (i in (i + 1):9) {
      d_box[[i]] <- c(list(""), list(""), list(""))
    }
  }
  return(d_box)
}

display_sheet <- function(data_list = data_list, d_box, status, colour, j = 1){
  
  tab_item <- shinydashboard::tabItem(tabName = data_list$contents$ID[[j]],
    
    # Stuff for the top of the tab
    shiny::fluidRow(shiny::column(12,
                                  align = "center",
                                  shinydashboard::box(shiny::splitLayout(shiny::h2(data_list$contents$name[[1]]), 
                                                                         shiny::icon(data_list$contents$icon[[1]], "fa-6x"),
                                                                         cellArgs = list(style = "vertical-align: top"), 
                                                                         cellWidths = c("80%", "20%")),
                                                  status = status,
                                                  background = colour,
                                                  width = 10,
                                                  title = NULL,
                                                  collapsible = FALSE,
                                                  solidHeader = TRUE,
                                                  height = "95px"))),
    
    # Tab contents
    shiny::fluidRow(shiny::column(12,
                                  align = "center",
                                  shiny::splitLayout(d_box[[1]][[1]],
                                                     d_box[[2]][[1]], 
                                                     d_box[[3]][[1]],
                                                     cellWidths = c("33.3%", "33.3%", "33.3%"),
                                                     cellArgs = list(style = "vertical-align: top"))), width = 10),
    shiny::fluidRow(shiny::column(12, 
                                  align = "center",
                                  shiny::splitLayout(d_box[[4]][[1]], 
                                                     d_box[[5]][[1]],
                                                     d_box[[6]][[1]], 
                                                     cellWidths = c("33.3%", "33.3%", "33.3%"), 
                                                     cellArgs = list(style = "vertical-align: top"))), width = 10),
    shiny::fluidRow(shiny::column(12, 
                                  align = "center",
                                  shiny::splitLayout(d_box[[7]][[1]], 
                                                     d_box[[8]][[1]],
                                                     d_box[[9]][[1]], 
                                                     cellWidths = c("33.3%", "33.3%", "33.3%"), 
                                                     cellArgs = list(style = "vertical-align: top"))), width = 10)
  )
  return(tab_item)
  }

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

tab_items <- function(...) {
  lapply(..., shinydashboard:::tagAssert, class = "tab-pane")
  div(class = "tab-content", ...)
}

create_tab_items <- function(data_list, d_box, status, colour){
  my_tab_items <- NULL
  i_disp <- 1
  
  for (i in 1:nrow(data_list$contents)){
    if (data_list$contents$type[[i]] == "Display"){
      my_tab_items[[i]] <- display_sheet(data_list = data_list,
                                         d_box = d_box[[i_disp]],
                                         status = status,
                                         colour = colour,
                                         j = i)
      i_disp <- i_disp + 1
    } # if it is another type of sheet then ... etc
  }
  return(my_tab_items)
}
