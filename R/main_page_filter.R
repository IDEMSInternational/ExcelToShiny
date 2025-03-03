#' Generate Checkbox Group and Date Input Elements
#'
#' This function generates a set of input elements (checkbox group and date input) based on the provided data, and returns them within a Shiny dashboard box along with a submit action button.
#'
#' @param spreadsheet A data frame containing information about the input elements to be created. The data frame should include columns such as `type`, `name`, and `parameter_list` that define the input elements. Supported input types are "checkbox_group" for checkbox groups and "date" for date inputs.
#'
#' @return A `shinydashboard::box` object containing the generated input elements (checkbox group and/or date input) and an action button for submission.
#'
#' @details
#' This function supports two types of inputs:
#' 1. **Checkbox Group Inputs**: Created when `type` is "checkbox_group". The `parameter_list` should include parameters like `label`, `choices`, `selected`, `width`, `choiceNames`, and `choiceValues` to customize the checkbox group.
#' 2. **Date Inputs**: Created when `type` is "date". The `parameter_list` should include parameters such as `label`, `value`, `min`, `max`, `format`, `startview`, and `weekstart` to customise the date input.
#' 3. **Date Range Inputs**: Created when `type` is "date_range". The `parameter_list` should include parameters such as `label`, `start`, `end`, `min`, `max`, `format`, `startview`, and `weekstart` to customise the date input.
#'
#' Both input types are returned within a `shinydashboard::box` alongside a submit button for triggering the action based on the selected filters.
main_page_filter <- function(spreadsheet){
  # For the group input
  filter_input <- NULL
  filter_data <- spreadsheet %>% dplyr::filter(type == "filter_box")

  # if statement
  filter_data <- filter_data %>%
    mutate(value = ifelse(stringdist::stringdist(value, "checkbox_group", method = "lv") <= 3, "checkbox_group",
                         ifelse(stringdist::stringdist(value, "date_range_group", method = "lv") <= 3, "date_range_group",
                                ifelse(stringdist::stringdist(value, "date_group", method = "lv") <= 3, "date_group",
                                       value))))
  valid_filter_boxes <- c("checkbox_group", "date_range_group", "date_group")
  if (!all(filter_data$value %in% valid_filter_boxes)){
    invalid_filter_data <- filter_data %>%
      dplyr::filter(!value %in% valid_filter_boxes)
    invalid_type <- invalid_filter_data %>%
      dplyr::pull(value)

    # if we can't read it, we can check if it has a choices option to see if it is a checkbox_group
    for (i in 1:nrow(invalid_filter_data)){
      invalid_filter_data_i <- invalid_filter_data[i,]
      spreadsheet_parameters <- invalid_filter_data_i$parameter_list
      choices <- get_parameter_value(spreadsheet_parameters, name = "choices", TRUE)
      if (!is.null(choices)){
        row_name <- invalid_filter_data_i$name
        warning("Cannot read value ", invalid_filter_data_i$value, " on main_page for filter_box. Setting as checkbox_group.")
        filter_data[filter_data$name == row_name, "value"] <- "checkbox_group" 
      } else {
        stop("Cannot read value: ", paste0(invalid_type, sep = ", "), "on main_page for filter_box. Should be one of ", paste0(valid_filter_boxes, sep = ", "))
      }
    }
  }

  # for value == "checkbox_group"
  if (nrow(filter_data) > 0){
    for (i in 1:nrow(filter_data)){
      filter_data_i <- filter_data[i,]
      name <- filter_data_i$name
      if (filter_data_i$value == "checkbox_group"){
        spreadsheet_parameters <- filter_data_i$parameter_list
        label <- get_parameter_value(spreadsheet_parameters, name = "label")
        choices <- get_parameter_value(spreadsheet_parameters, name = "choices", TRUE)
        selected <- get_parameter_value(spreadsheet_parameters, name = "selected", TRUE)
        width <- get_parameter_value(spreadsheet_parameters, name = "width")
        choiceNames <- get_parameter_value(spreadsheet_parameters, name = "choiceNames", TRUE)
        choiceValues <- get_parameter_value(spreadsheet_parameters, name = "choiceValues", TRUE)
        # todo - set up for inline (TRUE/FALSE parameter)
        
        filter_input[[i]] <- shiny::checkboxGroupInput(inputId = paste0(name),
                                                       label = label, width = width,
                                                       choices = choices, selected = selected,
                                                       choiceNames = choiceNames, choiceValues = choiceValues)
      } else if (filter_data_i$value %in% c("date", "date_group")) {
        spreadsheet_parameters <- filter_data_i$parameter_list
        label <- get_parameter_value(spreadsheet_parameters, name = "label")
        width <- get_parameter_value(spreadsheet_parameters, name = "width")
        value <- get_parameter_value(spreadsheet_parameters, name = "value")
        min <- get_parameter_value(spreadsheet_parameters, name = "min")
        max <- get_parameter_value(spreadsheet_parameters, name = "max")
        format <- get_parameter_value(spreadsheet_parameters, name = "format")
        startview <- get_parameter_value(spreadsheet_parameters, name = "startview")
        weekstart <- get_parameter_value(spreadsheet_parameters, name = "weekstart")
        if (!is.null(value) && is.na(value)) value <- Sys.Date()
        if (!is.null(min) && is.na(min)) min <- Sys.Date()
        if (!is.null(max) && is.na(max)) max <- Sys.Date()
        if (is.null(format)) format <- "yyyy-mm-dd"
        if (is.null(startview)) startview <- "month"
        if (is.null(weekstart)) weekstart <- 0
        filter_input[[i]] <- shiny::dateInput(inputId = paste0(name),
                                              label = label, value = value, width = width,
                                              min = min, max = max, format = format, startview = startview,
                                              weekstart = weekstart)
      } else if (filter_data_i$value %in% c("date_range", "date_range_group")) {
        spreadsheet_parameters <- filter_data_i$parameter_list
        label <- get_parameter_value(spreadsheet_parameters, name = "label")
        start <- get_parameter_value(spreadsheet_parameters, name = "start", date = TRUE)
        end <- get_parameter_value(spreadsheet_parameters, name = "end", date = TRUE)
        min <- get_parameter_value(spreadsheet_parameters, name = "min", date = TRUE)
        max <- get_parameter_value(spreadsheet_parameters, name = "max", date = TRUE)
        format <- get_parameter_value(spreadsheet_parameters, name = "format")
        startview <- get_parameter_value(spreadsheet_parameters, name = "startview")
        weekstart <- get_parameter_value(spreadsheet_parameters, name = "weekstart")
        separator <- get_parameter_value(spreadsheet_parameters, name = "separator")
        if (is.null(format)) format <- "yyyy-mm-dd"
        if (is.null(startview)) startview <- "month"
        if (is.null(weekstart)) weekstart <- 0
        if (is.null(separator)) separator <- " to "
        filter_input[[i]] <- shiny::dateRangeInput(inputId = paste0(name),
                                              label = label, start = as.Date(start), end = as.Date(end),
                                              min = as.Date(min), max = as.Date(max), format = format, startview = startview,
                                              weekstart = weekstart, separator = separator)
      }
    }
  }
  return(shinydashboard::box(width = 6,
                             filter_input,
                             # then run the actionButton
                             # this means currently only checkbox filtering on the main page.
                             shiny::actionButton(paste0("goButton_group"), "Submit", class = "btn-success")))
}
