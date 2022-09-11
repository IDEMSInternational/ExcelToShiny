#' Function to create Shiny
#'
#' @param title Title of the dashboard.
#' @param spreadsheet Spreadsheet that contains meta information to put in the box.
#' @param data_frame Spreadsheet that contains information to put in the box.
#' @param colour Skin colour of the Shiny App.
#' @param date_from Initial date to filter from.
#'
#' @return Shiny App
#' @export
#'
PLH_shiny <- function (title, spreadsheet, data_frame, colour = "blue", date_from = "2021-10-14") 
  colour <- tolower(colour)
{
  if (colour == "blue") {
    status = "primary"
  }
  else if (colour == "green") {
    status = "success"
  }
  else if (colour == "light blue") {
    status = "info"
  }
  else if (colour == "orange") {
    status = "warning"
  }
  else if (colour == "red") {
    status = "danger"
  }
  else {
    warning("Valid colours are blue, green, light blue, orange, red")
    status = "primary"
  }
  spreadsheet_shiny_box <- spreadsheet %>% dplyr::filter(location == 
                                                           "box")
  box_titles <- as.character(spreadsheet_shiny_box$box_title)
  box_colours <- as.character(spreadsheet_shiny_box$colour)
  box_variables <- as.character(spreadsheet_shiny_box$variable)
  spreadsheet_shiny_value_box <- spreadsheet %>% dplyr::filter(location == 
                                                                 "value_box")
  row_1_box <- NULL
  for (i in 1:length(box_titles)) {
    row_1_box[[i]] <- box_function1(data_frame = data_frame, 
                                    text = box_titles[i], colour = box_colours[i], variable = box_variables[i], 
                                    label_table = paste0("table_1_", i), label_plot = paste0("plot_1_", 
                                                                                             i))
  }
  if (i < 9) {
    for (i in (i + 1):9) {
      row_1_box[[i]] <- c(list(""), list(""), list(""))
    }
  }
  ui <- shinydashboard::dashboardPage(header = shinydashboard::dashboardHeader(title = paste(title, 
                                                                                             "Dashboard")), skin = colour, sidebar = shinydashboard::dashboardSidebar(shinydashboard::sidebarMenu(shinydashboard::menuItem("Demographics", 
                                                                                                                                                                                                                           tabName = "demographics", icon = shiny::icon("users")))), 
                                      shinydashboard::dashboardBody(shiny::fluidRow(shinydashboard::valueBoxOutput("myvaluebox1", 
                                                                                                                   width = 3), shinydashboard::valueBoxOutput("myvaluebox2", 
                                                                                                                                                              width = 3), shinydashboard::valueBoxOutput("myvaluebox3", 
                                                                                                                                                                                                         width = 3), shinydashboard::valueBoxOutput("myvaluebox4", 
                                                                                                                                                                                                                                                    width = 3)), shiny::column(6, align = "center", 
                                                                                                                                                                                                                                                                               shinydashboard::box(width = NULL, collapsible = FALSE, 
                                                                                                                                                                                                                                                                                                   solidHeader = TRUE, shiny::splitLayout(shiny::textInput(inputId = "datefrom_text", 
                                                                                                                                                                                                                                                                                                                                                           label = "Date from:", value = date_from), 
                                                                                                                                                                                                                                                                                                                                          cellArgs = list(style = "vertical-align: top"), 
                                                                                                                                                                                                                                                                                                                                          cellWidths = c("80%", "20%")))), shinydashboard::tabItems(shinydashboard::tabItem(tabName = "demographics", 
                                                                                                                                                                                                                                                                                                                                                                                                                            shiny::fluidRow(shiny::column(12, align = "center", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                          shinydashboard::box(shiny::splitLayout(shiny::h2("Demographics"), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 shiny::icon("users", "fa-6x"), cellArgs = list(style = "vertical-align: top"), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 cellWidths = c("80%", "20%")), status = status, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              background = colour, width = 10, title = NULL, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                              collapsible = FALSE, solidHeader = TRUE, height = "95px"))), 
                                                                                                                                                                                                                                                                                                                                                                                                                            shiny::fluidRow(shiny::column(12, align = "center", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                          shiny::splitLayout(row_1_box[[1]][[1]], row_1_box[[2]][[1]], 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                             row_1_box[[3]][[1]], cellWidths = c("33.3%", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "33.3%", "33.3%"), cellArgs = list(style = "vertical-align: top"))), 
                                                                                                                                                                                                                                                                                                                                                                                                                                            width = 10), shiny::fluidRow(shiny::column(12, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       align = "center", shiny::splitLayout(row_1_box[[4]][[1]], 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            row_1_box[[5]][[1]], row_1_box[[6]][[1]], 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            cellWidths = c("33.3%", "33.3%", "33.3%"), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            cellArgs = list(style = "vertical-align: top"))), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         width = 10), shiny::fluidRow(shiny::column(12, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    align = "center", shiny::splitLayout(row_1_box[[7]][[1]], 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         row_1_box[[8]][[1]], row_1_box[[9]][[1]], 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         cellWidths = c("33.3%", "33.3%", "33.3%"), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         cellArgs = list(style = "vertical-align: top"))), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      width = 10)))))
  server <- function(input, output) {
    output$table_1_1 <- shiny::renderTable({
      (row_1_box[[1]][[2]])
    }, striped = TRUE)
    output$plot_1_1 <- plotly::renderPlotly({
      row_1_box[[1]][[3]]
    })
    output$plot_1_2 <- plotly::renderPlotly({
      row_1_box[[2]][[3]]
    })
    output$table_1_2 <- shiny::renderTable({
      (row_1_box[[2]][[2]])
    }, striped = TRUE)
    output$plot_1_3 <- plotly::renderPlotly({
      row_1_box[[3]][[3]]
    })
    output$table_1_3 <- shiny::renderTable({
      (row_1_box[[3]][[2]])
    }, striped = TRUE)
    output$table_1_4 <- shiny::renderTable({
      (row_1_box[[4]][[2]])
    }, striped = TRUE)
    output$plot_1_4 <- plotly::renderPlotly({
      row_1_box[[4]][[3]]
    })
    output$plot_1_5 <- plotly::renderPlotly({
      row_1_box[[5]][[3]]
    })
    output$table_1_5 <- shiny::renderTable({
      (row_1_box[[5]][[2]])
    }, striped = TRUE)
    output$plot_1_6 <- plotly::renderPlotly({
      row_1_box[[6]][[3]]
    })
    output$table_1_6 <- shiny::renderTable({
      (row_1_box[[6]][[2]])
    }, striped = TRUE)
    output$plot_1_7 <- plotly::renderPlotly({
      row_1_box[[7]][[3]]
    })
    output$table_1_7 <- shiny::renderTable({
      (row_1_box[[7]][[2]])
    }, striped = TRUE)
    output$plot_1_8 <- plotly::renderPlotly({
      row_1_box[[8]][[3]]
    })
    output$table_1_8 <- shiny::renderTable({
      (row_1_box[[8]][[2]])
    }, striped = TRUE)
    output$plot_1_9 <- plotly::renderPlotly({
      row_1_box[[9]][[3]]
    })
    output$table_1_9 <- shiny::renderTable({
      (row_1_box[[9]][[2]])
    }, striped = TRUE)
    spreadsheet_shiny_value_box_text <- as.character(spreadsheet_shiny_value_box$box_title)
    spreadsheet_shiny_value_box_factor <- as.character(spreadsheet_shiny_value_box$variable)
    spreadsheet_shiny_value_box_icon <- as.character(spreadsheet_shiny_value_box$icon)
    spreadsheet_shiny_value_box_value_to_display <- as.character(spreadsheet_shiny_value_box$value_to_display)
    spreadsheet_shiny_value_box_colour <- as.character(spreadsheet_shiny_value_box$colour)
    output$myvaluebox1 <- shinydashboard::renderValueBox({
      top_value_boxes(data_frame = data_frame, variable = spreadsheet_shiny_value_box_factor[1], 
                      value_to_display = spreadsheet_shiny_value_box_value_to_display[1], 
                      colour = spreadsheet_shiny_value_box_colour[1], 
                      icon_pic = spreadsheet_shiny_value_box_icon[1], 
                      text = spreadsheet_shiny_value_box_text[1])
    })
    output$myvaluebox2 <- shinydashboard::renderValueBox({
      top_value_boxes(data_frame = data_frame, variable = spreadsheet_shiny_value_box_factor[2], 
                      value_to_display = spreadsheet_shiny_value_box_value_to_display[2], 
                      colour = spreadsheet_shiny_value_box_colour[2], 
                      icon_pic = spreadsheet_shiny_value_box_icon[2], 
                      text = spreadsheet_shiny_value_box_text[2])
    })
    output$myvaluebox3 <- shinydashboard::renderValueBox({
      top_value_boxes(data_frame = data_frame, variable = spreadsheet_shiny_value_box_factor[3], 
                      value_to_display = spreadsheet_shiny_value_box_value_to_display[3], 
                      colour = spreadsheet_shiny_value_box_colour[3], 
                      icon_pic = spreadsheet_shiny_value_box_icon[3], 
                      text = spreadsheet_shiny_value_box_text[3])
    })
    output$myvaluebox4 <- shinydashboard::renderValueBox({
      top_value_boxes(data_frame = data_frame, variable = spreadsheet_shiny_value_box_factor[4], 
                      value_to_display = spreadsheet_shiny_value_box_value_to_display[4], 
                      colour = spreadsheet_shiny_value_box_colour[4], 
                      icon_pic = spreadsheet_shiny_value_box_icon[4], 
                      text = spreadsheet_shiny_value_box_text[4])
    })
  }
  shiny::shinyApp(ui = ui, server = server)
  }