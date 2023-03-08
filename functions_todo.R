menu_items <- function(contents_list = data_list$contents){
  add_menu <- NULL
  for (i in 1:nrow(contents_list)){
    add_menu[[i]] <- menuItem(contents_list$name[[i]], tabName = contents_list$ID[[i]], icon = icon(contents_list$icon[[i]]))
  }
  return(add_menu)
}

display_sheet_setup <- function(spreadsheet_data, data_frame){
  spreadsheet_shiny_box <- spreadsheet_data %>% dplyr::filter(type == "box")
  row_1_box <- NULL
  for (i in 1:nrow(spreadsheet_shiny_box)) {
    ID <- spreadsheet_shiny_box[i,]$name
    row_1_box[[i]] <- box_function(data_frame = data_frame, 
                                   spreadsheet = spreadsheet_data,
                                   unique_ID = ID,
                                   label_table = paste0("table_1_", i),
                                   label_plot = paste0("plot_1_", i))
  }
  if (i < 9) {
    for (i in (i + 1):9) {
      row_1_box[[i]] <- c(list(""), list(""), list(""))
    }
  }
  return(row_1_box)
}

display_sheet <- function(data_list = data_list, row_1_box, status, colour){
  tab_item <-
    shinydashboard::tabItem(tabName = data_list$contents$ID[[1]],
    
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
                                  shiny::splitLayout(row_1_box[[1]][[1]],
                                                     row_1_box[[2]][[1]], 
                                                     row_1_box[[3]][[1]],
                                                     cellWidths = c("33.3%", "33.3%", "33.3%"),
                                                     cellArgs = list(style = "vertical-align: top"))), width = 10),
    shiny::fluidRow(shiny::column(12, 
                                  align = "center",
                                  shiny::splitLayout(row_1_box[[4]][[1]], 
                                                     row_1_box[[5]][[1]],
                                                     row_1_box[[6]][[1]], 
                                                     cellWidths = c("33.3%", "33.3%", "33.3%"), 
                                                     cellArgs = list(style = "vertical-align: top"))), width = 10),
    shiny::fluidRow(shiny::column(12, 
                                  align = "center",
                                  shiny::splitLayout(row_1_box[[7]][[1]], 
                                                     row_1_box[[8]][[1]],
                                                     row_1_box[[9]][[1]], 
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

create_tab_items <- function(data_list, row_1_box, status, colour){
  my_tab_items <- NULL
  i_disp <- 1
  
  for (i in 1:nrow(data_list$contents)){
    
    if (data_list$contents$type[[i]] == "Display"){
      my_tab_items[[1]] <- display_sheet(data_list = data_list,
                                         row_1_box = row_1_box[[i_disp]],
                                         status = status,
                                         colour = colour)
      i_disp <- i_disp + 1
    } # if it is another type of sheet then ... etc
  }
  return(my_tab_items)
}
