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
  # read in 
  spreadsheet_shiny_box <- spreadsheet_data %>% dplyr::filter(type %in% c("bar_table", "boxplot_table"))
  d_box <- NULL
  for (i in 1:nrow(spreadsheet_shiny_box)) {
    ID <- spreadsheet_shiny_box[i,]$name
    d_box[[i]] <- box_function(data_frame = data_frame, 
                                   spreadsheet = spreadsheet_data,
                                   unique_ID = ID,
                                   label_table = paste0("table_", j, "_", i),
                                   label_plot = paste0("plot_", j, "_", i))
  }
  print(names(d_box[[1]]))
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

  tab_item_objects <- NULL
  for (l in 1:max(spreadsheet[["row"]])){
    row_l_set <- list()
    row_l <- (spreadsheet %>% filter(row == l))$name
    k <- 1
    for (i in 1:length(d_box)){
      if (d_box[[i]]$ID %in% row_l){
        row_l_set[[k]] <- split_row_j[[i]]
        k <- k + 1
      }
    }
    tab_item_objects[[l]] <- split_layout(row_l_set)
  }
  
  for (l in 1:length(tab_item_objects)){
    tab_item_objects[[l]] <- shiny::fluidRow(shiny::column(12,
                                  align = "center",
                                  tab_item_objects[[l]]),
                    width = 10)
  }
  
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
                                      tab_item_objects

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

# what kind of sheet is it?
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


download_sheet <- function(data_list, spreadsheet_name, j = 1){
  # this is jth sheet on downloading data - does this work for multiple sheets?
  # what about multiple downloads on one page?
  spreadsheet <- data_list[[spreadsheet_name]]
  data_label <- (spreadsheet %>% filter(type == "Data label"))$name
  download_label <- (spreadsheet %>% filter(type == "Download label"))$name
  data_names <- (spreadsheet %>% filter(type == "Data"))$name
  
  # be able to edit choices, format (csv, etc), table name.
  tab_item_objects <- fluidRow(
    box(width = 6, 
        selectInput(paste0("dataset", j), data_label,
                    choices = data_names),
        # Button
        downloadButton(paste0("downloadData", j), download_label)),
    fluidRow(box(width = 12,
                 dataTableOutput(paste0("table", j))))
  )
  
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
                                      tab_item_objects
  )
  
  return(tab_item)
}

get_data_download <- function(data_to_download, i){
  return(eval(parse(text = data_to_download$value[i])))
}