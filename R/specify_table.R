#' Create a Table Based on Data Manipulation Instructions
#'
#' This function generates a  table from a dataset based on manipulation instructions provided in a `spreadsheet` object. 
#' The function supports optional grouping and allows for dynamic data manipulation and plot creation.
#'
#' @param data A dataset to manipulate and plot. Can be a list or a data frame.
#' @param spreadsheet An object containing data manipulation and graph manipulation instructions.
#' @param grouped_vars Optional. Variables to group the data by, provided as a character vector.
#'
#' @return A list containing two elements:
#' \describe{
#'   \item{`table`}{A table generated from the data based on the `spreadsheet$table_manip` instructions.}
#' }
specify_table <- function(data, spreadsheet, grouped_vars = NULL) {
  all_return <- list(table = NULL, plot_obj = NULL)
  # if (any(class(data) %in% "list")) {
  #   all_return$table <- data[[variable]]
  #   return(all_return)
  # }
  
  # TODO: need to fix this up to work for grouped_vars.
  # but if I have facets, it removes them. I think (see facilitator mexico, with the df having grouped var)
  #if (!is.null(grouped_vars)) grouped_vars <- NULL
  if (!is.null(grouped_vars)){
    if (!all(grouped_vars %in% names(data))) {
      grouped_vars <- grouped_vars[which(grouped_vars %in% names(data))]
    }
    group_cmd <- paste0("%>% dplyr::group_by(", grouped_vars, ")")
  } else {
    group_cmd <- paste0("")
  }
  
  # Table manipulation
  if (!is.null(spreadsheet$table_manip) && !is.na(spreadsheet$table_manip)) {
    # Command string from the spreadsheet
    command_string <- spreadsheet$table_manip
    
    # Clean and construct the full command
    # Remove extra whitespace or control characters from command_string
    command_string <- gsub("\r\n", "\n", command_string)   # Replace CRLF with LF for consistency
    #command_string <- gsub("^%>%\\s*", "", command_string) # Remove leading %>% if present

    if (startsWith(trimws(spreadsheet$table_manip), "%>%")){
      all_return$table <- tryCatch({
        eval(parse(text = paste0("data ", group_cmd, command_string)))
      }, error = function(e) {
        message("Ignoring manipulations. Error in `specify_table`: ", e$message)
        NULL  # Return NULL or handle the error as appropriate
      })
    } else{
      all_return$table <- tryCatch({
        eval(parse(text = command_string))
      }, error = function(e) {
        message("Ignoring manipulations. Error in `specify_table`: ", e$message)
        NULL  # Return NULL or handle the error as appropriate
      })
    }
  } else {
    if (!is.null(spreadsheet$data_manip) && !is.na(spreadsheet$data_manip)) {
      warning("Manipulations for specify_table are given in data_manip. These should be in table_manip.")
      # Command string from the spreadsheet
      command_string <- spreadsheet$data_manip
      
      # Clean and construct the full command
      # Remove extra whitespace or control characters from command_string
      command_string <- gsub("\r\n", "\n", command_string)  # Replace CRLF with LF for consistency
      command_string <- gsub("^%>%\\s*", "", command_string) # Remove leading + if present
      
      # Construct the full command
      if (!is.null(grouped_vars)){
        full_command <- paste0("data %>% dplyr::group_by(", grouped_vars, ") %>%", command_string)
      } else {
        full_command <- paste0("data %>%", command_string)
      }
      # Evaluate the command
      full_command <- tryCatch({
        eval(parse(text = full_command))
      }, error = function(e) {
        message("Ignoring manipulations. Error in evaluating data manipulation command: ", e$message)
        data  # Return NULL or handle the error as appropriate
      })
      all_return$table <- full_command 
    } else {
      all_return$table <- "No Table Given"      
    }
  }
  all_return$plot_obj <- NULL
  
  #print(all_return$table)
  
  return(all_return)
}