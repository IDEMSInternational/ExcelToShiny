#' Creating box to be used in `PLH_shiny` function
#'
#' @param data_frame Data frame that contains the data to analyse.
#' @param spreadsheet Spreadsheet that contains the template.
#' @param unique_ID Unique identifier.
#' @param label_table ID for the table.
#' @param label_plot ID for the plot.
#'
#' @return Box for use in `Shiny`
#' @export
bar_table <- function(data_frame, spreadsheet, unique_ID, label_table, label_plot){
  all_return <- NULL
  
  # we repeat for each row later in the plh_shiny function
  # for now, just get the data
  #spreadsheet <- testing_shiny
  spreadsheet <- spreadsheet %>% dplyr::filter(name == unique_ID)
  spreadsheet_parameters <- spreadsheet$parameter_list
  spreadsheet_parameters <- data.frame(stringr::str_split(spreadsheet_parameters, ", ", simplify = TRUE))
  spreadsheet_parameters_names <- sub("\\= .*", "", spreadsheet_parameters)
  spreadsheet_parameters_values <- gsub(".*= ", "", spreadsheet_parameters)
  spreadsheet_parameters_values <- stringr::str_remove_all(spreadsheet_parameters_values, stringr::fixed("\""))
  values <- spreadsheet_parameters_values
  names <- spreadsheet_parameters_names
  spreadsheet_df <- data.frame(names, values)

  #repeat for all variables like text, etc. so make into a function?
  text <- spreadsheet_finder(data = spreadsheet_df, "text ")
  width <- spreadsheet_finder(data = spreadsheet_df, "width ")
  colour <- spreadsheet_finder(data = spreadsheet_df, "colour ")
  colour <- tolower(colour)
  if (colour == "blue") {
    status = "primary"
  } else if (colour == "green") {
    status = "success"
  } else if (colour == "light blue") {
    status = "info"
  } else if (colour == "orange") {
    status = "warning"
  } else if (colour == "red") {
    status = "danger"
  } else {
    warning("Valid colours are blue, green, light blue, orange, red")
    status = "primary"
  }
  variable <- spreadsheet$variable
  if (!variable %in% names(data_frame)) stop(paste0(variable, " not in data."))
  
  #label_ID <- (stringr::str_split(spreadsheet$value, ", ", simplify = TRUE))
  #label_ID <- paste0(spreadsheet$name, "_", label_ID)
  #label_table <- label_ID[stringr::str_which(label_ID, "table")]
  #label_plot <- label_ID[stringr::str_which(label_ID, "plot")]
  
  all_return[[1]] <- shinydashboard::box(width=NULL,
                                         collapsible = FALSE,
                                         title = text,
                                         status = status, # primary, success, info, warning, danger
                                         solidHeader = TRUE,
                                         plotly::plotlyOutput(outputId = label_plot, height = "240"),
                                         shiny::tableOutput(label_table))
  plot_to_return <- ggplot2::ggplot()
  if (is.numeric(data_frame[[variable]])){
    plot_to_return <- plot_to_return +
      ggplot2::geom_boxplot(data = data_frame, ggplot2::aes(y = .data[[variable]])) +
      ggplot2::labs(x = "Count")
    table_to_return <- data_frame %>%
      dplyr::summarise(Mean = round(mean(data_frame[[variable]], na.rm = TRUE), 2),
                SD = round(stats::sd(data_frame[[variable]], na.rm = TRUE), 2))
  } else {
    table_to_return <- summary_table(data = data_frame,
                                     factors = .data[[variable]],
                                     include_margins = TRUE,
                                     replace = NULL)
    plot_to_return <- plot_to_return +
      ggplot2::geom_histogram(data = data_frame, ggplot2::aes(x = .data[[variable]]), stat = "count")  +
      viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
      ggplot2::labs(y = "Count", x = naming_conventions(variable))
  }
  all_return[[2]] <- table_to_return
  all_return[[3]] <- plot_to_return
  all_return[[4]] <- label_table
  all_return[[5]] <- label_plot
  all_return[[6]] <- unique_ID
  names(all_return) <- c("gui_obj", "table_obj", "plot_obj", "label_table", "label_plot", "ID")
  return(all_return)
}
