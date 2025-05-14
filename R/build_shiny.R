#' PLH Shiny Function - Create and Launch a Shiny Dashboard
#'
#' The PLH_shiny function generates a Shiny dashboard application based on the provided datasets and specifications. This function is designed to create a dynamic and interactive user interface that includes value boxes, filters, groupings, plots, tables, and downloadable content, all customisable through the input parameters.
#'
#' @param title A string specifying the title of the dashboard.
#' @param data_list A list of data frames and associated metadata that define the contents and structure of the dashboard. Each entry in the list corresponds to different sections or components within the dashboard.
#' @param data_frame A data frame that serves as the main dataset for the dashboard. It is used to generate the main page content and handle filters or groupings.
#' @param status A string specifying the status of the dashboard elements. The default is "primary", but it can also be set to "success", "info", "warning", or "danger" depending on the chosen colour.
#' @param colour A string specifying the skin colour of the Shiny App. Valid options include "blue", "green", "light blue", "orange", and "red". Each colour corresponds to a different status.
#' @param date_from A string representing the initial date to filter data from, in the format "YYYY-MM-DD". The default is "2021-10-14".
#' @param key_var A string specifying the name of the key variable that links different data frames together. If NULL, no key-based linking is performed.
#' @param deploy_shiny Default TRUE. Boolean denoting if the function should deploy the shiny dashboard.
#'
#' @return A Shiny App object that, when run, launches the interactive dashboard.
#' @export
#'
#' @details
#' This function facilitates the creation of a Shiny dashboard tailored to the specified datasets and requirements. The dashboard includes:
#' - Sidebar Menu: Dynamically generated based on the provided data_list.
#' - Value Boxes: Display key metrics at the top of the dashboard.
#' - Filters and Grouping: Allows users to filter and group data on the main page.
#' - Tabs and Content Display: Customisable tabs and content displays based on the data_list configuration.
#' - Downloadable Content: Enables users to download specific datasets as CSV files.
#'
#' @examples
#' \dontrun{
#' # Example of using the PLH_shiny function to create a dashboard
#' # TODO: give this as a reproducible example
#' data_list <- list(
#' contents = data.frame(
#' ID = c("main_page", "tab_1", "tab_2"),
#' type = c("Tabbed_display", "Display", "Download"),
#' stringsAsFactors = FALSE
#' ),
#' main_page = data.frame(
#' name = c("value_1", "value_2"),
#' type = c("value_box", "filter_box"),
#' variable = c("metric1", "metric2"),
#' stringsAsFactors = FALSE
#' )
#' )
#' data_frame <- data.frame(
#' metric1 = rnorm(100),
#' metric2 = rnorm(100),
#' stringsAsFactors = FALSE
#' )
#'
#' # Run the dashboard
#' shinyApp(ui = build_shiny("My Dashboard", data_list, data_frame), server = function(input, output) {})
#' }
#'
#' @note Ensure that the data_list and data_frame parameters are correctly formatted to match the expected structure for the dashboard to function properly.
build_shiny <- function (title, data_list, data_frame, status = "primary", colour = "blue", date_from = "2021-10-14", key_var = NULL, deploy_shiny = TRUE){
  colour <- tolower(colour)
  
  status_map <- c(
    "blue" = "primary",
    "green" = "success",
    "light blue" = "info",
    "orange" = "warning",
    "red" = "danger"
  )
  
  status <- status_map[colour]
  
  if (is.na(status)) {
    warning("Valid colours are: blue, green, light blue, orange, red. Setting colour to 'blue'.")
    status <- "primary"
    colour <- "blue"
  }
  
  # Setting up (pre-UI and pre-server items) --------------------------------
  # Check the types in contents are all valid types (display, tabbed_display, and download)
  data_list$contents <- data_list$contents %>%
    dplyr::mutate(type = ifelse(stringdist::stringdist(type, "Display", method = "lv") <= 2, "Display",
                         ifelse(stringdist::stringdist(type, "Tabbed_display", method = "lv") <= 3, "Tabbed_display",
                                ifelse(stringdist::stringdist(type, "Download", method = "lv") <= 3, "Download",
                                       type))))
  valid_contents_type <- c("Display", "Tabbed_display", "Download")
  if (!all(data_list$contents$type %in% valid_contents_type)){
    invalid_contents_type <- data_list$contents %>%
      dplyr::filter(!type %in% valid_contents_type) %>%
      dplyr::pull(type)
    stop("Cannot read contents type: ", paste0(invalid_contents_type, sep = ", "), "Should be one of ", paste0(valid_contents_type, sep = ", "))
  }
  contents <- data_list$contents
  
  for (i in 1:length(data_list)){
    if (is.null(data_list[[i]]$name)){
      data_list[[i]]$name <- paste0("box", 1:nrow(data_list[[i]]))
    }
  }
  
  # list of sheets
  # check the variables exist
  for (df_name in names(data_list)){
    sheet <- data_list[[df_name]]
    data_frame_name <- deparse(substitute(data_frame))
    results <- NULL
    results <- check_variables_existence(sheet, data_frame = data_frame_name)
    if (!is.null(results) && nrow(results) > 0) check_unknown_variables(results)
  }

  # Populate items for the tabs ------------------------------------------------
  display_box <- display_contents(data_frame = data_frame, contents = contents, data_list = data_list, k = which(data_list$contents$type == "Tabbed_display"))

  my_tab_items <- create_tab_items(data_list = data_list,
                                   d_box = display_box,
                                   status = status,
                                   colour = colour)
  
  # value box for main page -------------------------------------------------------------------------------
  # check type is one of value_box, filter_box, or group_by_box
  shiny_top_box_i <- NULL
  if (!is.null(data_list$main_page)){
    data_list$main_page <- data_list$main_page %>%
      dplyr::mutate(type = ifelse(stringdist::stringdist(type, "value_box", method = "lv") <= 2, "value_box",
                                  ifelse(stringdist::stringdist(type, "filter_box", method = "lv") <= 3, "filter_box",
                                         ifelse(stringdist::stringdist(type, "group_by_box", method = "lv") <= 3, "group_by_box",
                                                type))))
    valid_boxes <- c("value_box", "filter_box", "group_by_box")
    if (!all(data_list$main_page$type %in% valid_boxes)){
      invalid_type <- data_list$main_page %>%
        dplyr::filter(!type %in% valid_boxes) %>%
        dplyr::pull(type)
      stop("Cannot read type: ", paste0(invalid_type, sep = ", "), "on main_page. Should be one of ", paste0(valid_boxes, sep = ", "))
    }
    
    # value boxes
    spreadsheet_shiny_value_box <- data_list$main_page %>% dplyr::filter(type %in% c("value_box"))
    for (i in 1:nrow(spreadsheet_shiny_value_box)){
      if (nrow(spreadsheet_shiny_value_box) <= 4){
        shiny_top_box_i[[i]] <- shinydashboard::valueBoxOutput(spreadsheet_shiny_value_box[i,]$name, width = 12/nrow(spreadsheet_shiny_value_box))
      } else {
        shiny_top_box_i[[i]] <- shinydashboard::valueBoxOutput(spreadsheet_shiny_value_box[i,]$name)
      }
    } 
    
    # Filters
    checkbox_data <- data_list$main_page %>% dplyr::filter(type %in% c("filter_box"))
    if (nrow(checkbox_data) > 0) {
      filter_on_main_page <- main_page_filter(spreadsheet = checkbox_data)
    } else {
      filter_on_main_page <- NULL
    }
    
    # Group bys
    group_by_data_box <- data_list$main_page %>% dplyr::filter(type %in% c("group_by_box"))
    if (nrow(group_by_data_box) > 0) {
      group_on_main_page <- main_page_group(spreadsheet = group_by_data_box)
    } else {
      group_on_main_page <- NULL
    }
  } else {
    filter_on_main_page <- NULL
    group_on_main_page <- NULL
  }
  
  # Assuming menu_items(data_list$contents) returns a list of menuItem objects
  menu_items_list <- menu_items(data_list$contents)
  
  # Add the 'id' to the list of arguments
  args <- c(list(id = "tab"), menu_items_list)
  
  # Create the sidebar menu with do.call
  sidebar_menu <- do.call(shinydashboard::sidebarMenu, args)
  
  #sidebar_menu <- do.call(shinydashboard::sidebarMenu, menu_items(data_list$contents))
  # Set up UI -------------------------------------------------------
  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    shinydashboard::dashboardPage(
      # 
      header = shinydashboard::dashboardHeader(title = paste(title, "Dashboard")),
      skin = colour,
      
      # todo: fix up this function to allow N items (rather than having to tell it how many)
      sidebar = shinydashboard::dashboardSidebar(sidebar_menu),
      
      shinydashboard::dashboardBody(
        #value input boxes
        shiny::fluidRow(shiny_top_box_i),
        
        # tabs info
        shiny::column(12, align = "center",
                      filter_on_main_page,
                      group_on_main_page
                      # shiny::splitLayout(shiny::textInput(inputId = "datefrom_text", 
                      #                                     label = "Date from:", value = date_from), 
                      #                    cellArgs = list(style = "vertical-align: top"),
                      #                    cellWidths = c("80%", "20%")))
        ),
        tab_items(my_tab_items)
        
      )
    )
  )
  
  server <- build_server(data_list, data_frame, key_var, data_frame_name = deparse(substitute(data_frame)))
  if (deploy_shiny) shiny::shinyApp(ui = ui, server = server)
  else return(list(ui = ui, server = server))
}