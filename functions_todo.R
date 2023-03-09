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
  spreadsheet_shiny_box <- spreadsheet_data %>% dplyr::filter(type == "bar_table")
  d_box <- NULL
  for (i in 1:nrow(spreadsheet_shiny_box)) {
    ID <- spreadsheet_shiny_box[i,]$name
    d_box[[i]] <- bar_table(data_frame = data_frame, 
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

split_layout <- function(..., cellWidths = NULL, cellArgs = list()){
  children <- (...)
  childIdx <- !nzchar(names(children) %||% character(length(children)))
  attribs <- children[!childIdx]
  children <- children[childIdx]
  count <- length(children)
  if (length(cellWidths) == 0 || isTRUE(is.na(cellWidths))) {
    cellWidths <- sprintf("%.3f%%", 100/count)
  }
  cellWidths <- rep(cellWidths, length.out = count)
  cellWidths <- sapply(cellWidths, validateCssUnit)
  do.call(tags$div, c(list(class = "shiny-split-layout"), 
                      attribs, mapply(children, cellWidths, FUN = function(x, 
                                                                           w) {
                        do.call(tags$div, c(list(style = sprintf("width: %s;", 
                                                                 w)), cellArgs, list(x)))
                      }, SIMPLIFY = FALSE)))
}


display_sheet <- function(data_list, spreadsheet_name, d_box, status, colour, j = 1){
  
  # Create the "div" for each row.
  # each row is stored in a list, split_row[[j]] (j = row)
  spreadsheet <- data_list[[spreadsheet_name]]
  split_row_j <- NULL
  k <- 1
  for (i in 1:length(d_box)){
    split_row_j[[i]] <- d_box[[i]][[1]]
    k <- k + 1
  }
  
  # TODO: get following to work
  # split_A<- NULL
  # for (j in 1:3){
  #   test_hi <- NULL
  #   row_j <- (spreadsheet %>% filter(row == j))$name
  #   k <- 1
  #   for (i in 1:length(d_box)){
  #     if (d_box[[i]]$ID %in% row_j){
  #       test_hi[[k]] <- split_row_j[[i]]
  #       k <- k + 1
  #     } else {
  #       print("no")
  #     }
  #   }
  # split_A[[j]] <- split_layout(test_hi)
  # }
  
  
  # TODO: generalise this for all j's from 1 to max(row) value in the data frame
  
  test_hi <- NULL
  split_2 <- NULL
  split_3 <- NULL
  
  row_1 <- (spreadsheet %>% filter(row == 1))$name
  row_1_set <- NULL
  k <- 1
  for (i in 1:length(d_box)){
    if (d_box[[i]]$ID %in% row_1){
      row_1_set[k] <- i
      k <- k + 1
    }
  }
  for (i in row_1_set){
    test_hi[[i]] <- split_row_j[[i]]
  }
  test_hi <- split_layout(test_hi)
  k <- 1
  for (i in 4:6){
    split_2[[k]] <- split_row_j[[i]]
    k <- k + 1
  }
  split_2 <- split_layout(split_2)
  k <- 1
  for (i in 7:9){
    split_3[[k]] <- split_row_j[[i]]
    k <- k + 1
  }
  split_3 <- split_layout(split_3)
  
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
                                                                    test_hi),
                                                      width = 10),
                                      shiny::fluidRow(shiny::column(12,
                                                                    align = "center",
                                                                    split_2),
                                                      width = 10),
                                      shiny::fluidRow(shiny::column(12,
                                                                    align = "center",
                                                                    split_3),
                                                      width = 10)
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
                                         spreadsheet_name = data_list$contents$ID[[i]],
                                         d_box = d_box[[i_disp]],
                                         status = status,
                                         colour = colour,
                                         j = i)
      i_disp <- i_disp + 1
    } # if it is another type of sheet then ... etc
  }
  return(my_tab_items)
}
