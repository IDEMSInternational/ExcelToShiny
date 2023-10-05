#' Checkbox Group Input
#'
#' This function generates a set of checkbox group input elements based on the provided checkbox data.
#'
#' @param checkbox_data A data frame containing information about the checkbox groups to be created.
#'
#' @return A list containing checkbox group input elements and an action button.
#'
#' @export
#'
#' @examples
#' ## Not Run 
#' checkbox_group_data2 <- data.frame(type = c("checkbox_group", "checkbox_group"),
#'                                    name = c("variable", "variable2"),
#'                                    parameter_list = c('label = "variable", choices = c("Cylinders" = "cyl", "Transmission" = "am", "Gears" = "gear")',
#'                                                       'label = "variable2", choices = c("Cyls" = "Sepal.Length", "Gers" = "Sepal.Width")'))
#' ui <- shiny::fluidPage(
#'  main_page_filter(checkbox_group_data2),
#'    shiny::tableOutput("data"),
#'    shiny::tableOutput("data2")
#'  )
#'server <- function(input, output, session) {
#'  output$data <- renderTable({
#'  mtcars[, c("mpg", input$variable), drop = FALSE]
#'  }, rownames = TRUE)
#'  output$data2 <- renderTable({
#'  iris[, c("Species", input$variable2), drop = FALSE]
#'  }, rownames = TRUE)
#'}
#'
#'#NOT RUN:
#'#shiny::shinyApp(ui, server)
main_page_filter <- function(spreadsheet){
  # For the group input
  checkbox_group_input <- NULL
  checkbox_group_data <- spreadsheet %>% dplyr::filter(type == "checkbox_group")
  if (nrow(checkbox_group_data) > 0){
    for (i in 1:nrow(checkbox_group_data)){
      checkbox_group_data_i <- checkbox_group_data[i,]
      name <- checkbox_group_data_i$name
      spreadsheet_parameters <- checkbox_group_data_i$parameter_list
      label <- get_parameter_value(spreadsheet_parameters, name = "label")
      choices <- get_parameter_value(spreadsheet_parameters, name = "choices", TRUE)
      selected <- get_parameter_value(spreadsheet_parameters, name = "selected", TRUE)
      width <- get_parameter_value(spreadsheet_parameters, name = "width")
      choiceNames <- get_parameter_value(spreadsheet_parameters, name = "choiceNames", TRUE)
      choiceValues <- get_parameter_value(spreadsheet_parameters, name = "choiceValues", TRUE)
      # todo - set up for inline (TRUE/FALSE parameter)
      
      checkbox_group_input[[i]] <- shiny::checkboxGroupInput(inputId = paste0(name),
                                                      label = label, width = width,
                                                      choices = choices, selected = selected,
                                                      choiceNames = choiceNames, choiceValues = choiceValues)
    }
  }
  
  # todo:
  return(shinydashboard::box(width = 6,
             checkbox_group_input,
             # then run the actionButton
             # this means currently only checkbox filtering on the main page.
             shiny::actionButton(paste0("goButton_group"), "Submit", class = "btn-success")))
}
