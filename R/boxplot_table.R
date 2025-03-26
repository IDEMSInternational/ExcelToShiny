#' Creating a Boxplot to be Used in the `PLH_shiny` Function
#'
#' This function generates a summary table and a boxplot for use in `Shiny` applications, specifically within the `PLH_shiny` function. It processes data according to the specifications in the provided spreadsheet.
#'
#' @param data Data frame that contains the data to analyse.
#' @param variable Character string specifying the variable of interest within the data.
#' @param type Character vector specifying the type of table to generate. Options are `"summary"` for summary statistics (median, SD) or `"freq"` for frequency tables. Default is `"summary"`.
#' @param spreadsheet List containing commands and templates for data manipulation and plotting.
#' @param grouped_vars Optional character string specifying the variable to group by in the table and plot.
#'
#' @return A list containing a summary table and a ggplot boxplot object for use in `Shiny`.
boxplot_table <- function(data, variable, type = c("summary", "freq"), spreadsheet, grouped_vars = NULL){
  type <- match.arg(type)
  all_return <- list(table = NULL, plot = NULL)
  
  if (inherits(data, "list")) {
    all_return$table <- data[[variable]]
    all_return$plot <- ggplot2::ggplot()
    return(all_return)
  }
  
  #if (!is.null(grouped_vars) && (grouped_vars %in% variable)) grouped_vars <- NULL
  
  # Check if data manipulation command is not null or NA
  if (!is.null(spreadsheet$data_manip) && !is.na(spreadsheet$data_manip)) {
    # Command string from the spreadsheet
    command_string <- spreadsheet$data_manip
    
    # Clean and construct the full command
    # Remove extra whitespace or control characters from command_string
    command_string <- gsub("\r\n", "\n", command_string)  # Replace CRLF with LF for consistency
    command_string <- gsub("^%>%\\s*", "", command_string) # Remove leading %>% if present
    
    # Construct the full command
    if (!is.null(grouped_vars)){
      full_command <- paste0("data %>% dplyr::group_by(", grouped_vars, ") %>%", command_string)
    } else {
      full_command <- paste0("data %>%", command_string)
    }
    
    # Evaluate the command
    data <- tryCatch({
      eval(parse(text = full_command))
    }, error = function(e) {
      message("Ignoring manipulations. Error in evaluating data manipulation command: ", e$message)
      data  # Return NULL or handle the error as appropriate
    })
  }
  
  # Refactor the boxplot into a separate function
  create_boxplot <- function(data, variable, grouped_vars) {
    plot_to_return <- ggplot2::ggplot(data, ggplot2::aes(x = 1, y = .data[[variable]])) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(x = "Count", y = naming_conventions(variable))
    if (!is.null(grouped_vars)){
      plot_to_return <- plot_to_return +
        ggplot2::facet_wrap(grouped_vars)
    }
    return(plot_to_return)
  }
  
  if (!is.null(spreadsheet$table_manip) && !is.na(spreadsheet$table_manip) && spreadsheet$table_manip == "none"){
    all_return$table <- "No Table Given"
  } else {
    if (type == "freq"){
      if (!is.null(grouped_vars)){
        all_return$table <- summary_table(data = data, factors = c(variable, grouped_vars), include_margins = FALSE)
      } else {
        all_return$table <- summary_table(data = data, factors = variable, include_margins = FALSE)
      }
    } else {
      table_data <- dplyr::filter(data, !is.na(data[[variable]]))
      
      if (!is.null(grouped_vars)){
        table_data <- table_data %>% dplyr::group_by(!!!rlang::syms(grouped_vars))
      }
      all_return$table <- table_data %>%
        dplyr::summarise(Median = round(stats::median(!!rlang::sym(variable), na.rm = TRUE), 2),
                         SD = round(stats::sd(!!rlang::sym(variable), na.rm = TRUE), 2)) # temp. remove N
      # N = length(!is.na(!!sym(variable))))
    }
  }
  
  plot_obj <- create_boxplot(data, variable, grouped_vars)
  if (!is.null(spreadsheet$graph_manip) && !is.na(spreadsheet$graph_manip)) {
    command_string <- spreadsheet$graph_manip
    
    # Clean and construct the full command
    # Remove extra whitespace or control characters from command_string
    command_string <- gsub("\r\n", "\n", command_string)  # Replace CRLF with LF for consistency
    command_string <- gsub("^+\\s*", "", command_string) # Remove leading %>% if present
    plot_command <- paste0("plot_obj + ", command_string)
    
    plot_obj <- tryCatch({
      eval(parse(text = plot_command))
    }, error = function(e) {
      message("Error in evaluating graph manipulation code: ", e$message)
      plot_obj  # Return the original plot object in case of an error
    })
  }
  all_return$plot <- plot_obj
  return(all_return)
}
