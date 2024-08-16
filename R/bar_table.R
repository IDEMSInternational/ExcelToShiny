#' Create a Box for Use in Shiny Dashboards
#'
#' This function generates a list containing a summary table and a plot for use in `Shiny` applications, specifically designed for integration with the `PLH_shiny` function. It can handle grouped data and apply custom data manipulations based on the provided spreadsheet.
#'
#' @param data A data frame or a list. If a list, the table is extracted from the list element specified by `variable`.
#' @param variable A character string specifying the variable of interest within the data.
#' @param type A character vector specifying the type of table to generate. Options are `"freq"` for frequency tables and `"summary"` for summary statistics (median, SD). Default is `"freq"`.
#' @param spreadsheet A list containing data manipulation commands and options for customising the generated plot and table. It should include elements like `data_manip`, `table_manip`, and `graph_manip`.
#' @param grouped_vars An optional character string specifying the variable to group by in the table and plot.
#'
#' @return A list with two elements: `table`, containing the generated summary table, and `plot`, containing the corresponding ggplot object.
#'
#' @examples
#' # Grouped summary table
#' result <- bar_table(data = mtcars, variable = "mpg", type = "summary", 
#'                    spreadsheet = list(), grouped_vars = "cyl")
#' print(result$table)
#' print(result$plot)
#'
bar_table <- function(data, variable, type = c("freq", "summary"), spreadsheet, grouped_vars = NULL) {
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

  # Refactor the histogram plotting into a separate function
  create_histogram_plot <- function(data, variable, grouped_vars) {
    if (!is.null(grouped_vars)){
      ggplot2::ggplot(data, ggplot2::aes(x = .data[[variable]])) +
        ggplot2::geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        ggplot2::labs(y = "Count", x = naming_conventions(variable)) +
        facet_wrap(grouped_vars)
    } else {
      ggplot2::ggplot(data, ggplot2::aes(x = .data[[variable]])) +
        ggplot2::geom_histogram(stat = "count") +
        viridis::scale_fill_viridis(discrete = TRUE, na.value = "navy") +
        ggplot2::labs(y = "Count", x = naming_conventions(variable)) 
    }
  }
  
  if (!is.null(spreadsheet$table_manip) && !is.na(spreadsheet$table_manip) && spreadsheet$table_manip == "none"){
    #all_return$table <- NA
    all_return$table <- "No Table Given"
  } else {
    if (type == "freq") {
      if (!is.null(grouped_vars)){
        all_return$table <- summary_table(data = data, factors = c(variable, grouped_vars), include_margins = FALSE)
      } else {
        all_return$table <- summary_table(data = data, factors = variable, include_margins = FALSE)
      }
    } else {
      table_data <- dplyr::filter(data, !is.na(data[[variable]]))
      
      if (!is.null(grouped_vars)){
        table_data <- table_data %>% group_by(!!sym(grouped_vars))
      }
      all_return$table <- table_data %>%
        dplyr::summarise(Median = round(median(!!sym(variable), na.rm = TRUE), 2),
                         SD = round(stats::sd(!!sym(variable), na.rm = TRUE), 2))
      #N = length(!is.na(!!sym(variable)))) '# remove n for now
    }
  }

  plot_obj <- create_histogram_plot(data, variable, grouped_vars)
  if (!is.null(spreadsheet$graph_manip) && !is.na(spreadsheet$graph_manip)) {
    plot_command <- paste0("plot_obj + ", spreadsheet$graph_manip)
    
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
