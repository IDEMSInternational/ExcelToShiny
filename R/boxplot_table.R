#' Creating boxplot to be used in `PLH_shiny` function
#'
#' @param data_frame Data frame that contains the data to analyse.
#' @param spreadsheet Spreadsheet that contains the template.
#' @param unique_ID Unique identifier.
#' @param label_table ID for the table.
#' @param label_plot ID for the plot.
#'
#' @return Box for use in `Shiny`
#' @export
boxplot_table <- function(data, variable, type = c("summary", "freq"), spreadsheet, grouped_vars = NULL){
  type <- match.arg(type)
  all_return <- list(table = NULL, plot = NULL)
  
  if (class(data) == "list") {
    all_return$table <- data[[variable]]
    all_return$plot <- ggplot2::ggplot()
    return(all_return)
  }
  
  if (!is.null(grouped_vars) && (grouped_vars %in% variable)) grouped_vars <- NULL
  
  # Check if data manipulation command is not null or NA
  if (!is.null(spreadsheet$data_manip) && !is.na(spreadsheet$data_manip)) {
    # Command string from the spreadsheet
    command_string <- spreadsheet$data_manip
    
    # Construct the full command
    if (!is.null(grouped_vars)){
      full_command <- paste0("data %>% group_by(", grouped_vars, ")", command_string)
    } else {
      full_command <- paste0("data ", command_string)
    }
    
    # Evaluate the command
    data <- tryCatch({
      eval(parse(text = full_command))
    }, error = function(e) {
      message("Ignoring manipulations. Error in evaluating data manipulation command: ", e$message)
      data  # Return NULL or handle the error as appropriate
    })
  }
  
  plot_to_return <- ggplot2::ggplot()
  plot_to_return <- plot_to_return +
    ggplot2::geom_boxplot(data = data, ggplot2::aes(y = .data[[variable]])) +
    ggplot2::labs(x = "Count")
  if (!is.null(grouped_vars)){
    plot_to_return <- plot_to_return + facet_wrap(grouped_vars)
  }
  
  if (!is.null(spreadsheet$graph_manip) && !is.na(spreadsheet$graph_manip)) {
    plot_command <- paste0("plot_obj + ", spreadsheet$graph_manip)
    plot_to_return <- tryCatch({
      eval(parse(text = plot_command))
    }, error = function(e) {
      message("Error in evaluating graph manipulation code: ", e$message)
      plot_to_return  # Return the original plot object in case of an error
    })
  }
  
  if (type == "freq"){
    if (!is.null(grouped_vars)){
      table_data <- summary_table(data = data, factors = c(.data[[variable]], .data[[grouped_vars]]), include_margins = FALSE)
    } else {
      table_data <- summary_table(data = data, factors = .data[[variable]], include_margins = FALSE)
    }
  } else {
    table_data <- dplyr::filter(data, !is.na(data[[variable]]))
    
    if (!is.null(grouped_vars)){
      table_data <- table_data %>% group_by(!!sym(grouped_vars))
    }
    table_data <- table_data %>%
      dplyr::summarise(Median = round(median(!!sym(variable), na.rm = TRUE), 2),
                       SD = round(stats::sd(!!sym(variable), na.rm = TRUE), 2),
                       N = length(!is.na(!!sym(variable))))
  }
  all_return[[1]] <- table_data
  all_return[[2]] <- plot_to_return
  return(all_return)
}
