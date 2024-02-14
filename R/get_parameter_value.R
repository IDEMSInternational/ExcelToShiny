#' Get Parameter Value
#'
#' This function extracts a parameter value from a string that represents a set of spreadsheet parameters.
#'
#' @param spreadsheet_parameters A character string containing the spreadsheet parameters.
#' @param name The name of the parameter whose value you want to extract. Default is "label".
#' @param list Logical value indicating whether the parameter value should be treated as a list.
#'
#' @return The value of the specified parameter. If the parameter is not found, it returns NULL.
#'
#' @export
#'
#' @examples
#' # Extract a parameter value as a string
#' param_string <- 'label = "Value"'
#' get_parameter_value(param_string)  # Returns "Value"
#'
#' # Extract a parameter value as a list
#' param_list <- 'options = c("Option1", "Option2", "Option3")'
#' get_parameter_value(param_list, "options", list = TRUE)  # Returns a character vector c("Option1", "Option2", "Option3")
#'
#' # Parameter not found
#' param_not_found <- 'other_param = 42'
#' get_parameter_value(param_not_found)
get_parameter_value <- function(spreadsheet_parameters, name = "label", list = FALSE, logical = FALSE){
  #spreadsheet_parameters <- trimws(spreadsheet_parameters)
  if (stringr::str_detect(spreadsheet_parameters, paste0(name, " ="))) {
    if (stringr::str_detect(spreadsheet_parameters, paste0(name, " = "))) {
      label_form = paste0(name, " = ")
    } else {
      label_form = paste0(name, " =")
    }
  } else if (stringr::str_detect(spreadsheet_parameters, paste0(name, "="))){
    if (stringr::str_detect(spreadsheet_parameters, paste0(name, "= "))) {
      label_form = paste0(name, "= ")
    } else {
      label_form = paste0(name, "=")
    }
  } else {
    label_form = NULL
  }
  
  if (!is.null(label_form)){
    if (list == FALSE){
      if (logical == FALSE){
        label_pattern <- paste0(label_form, '"(.*?)"')
        label_match <- regmatches(spreadsheet_parameters, regexec(label_pattern, spreadsheet_parameters))[[1]][1]
        label <- gsub(paste0(label_form, '"|"'), '', label_match)
      } else {
        label <- as.logical(trimws(gsub(paste0(".*", label_form), "", spreadsheet_parameters)))
      }
    } else {
      label_pattern <- paste0(label_form, 'c\\((.*?)\\)')
      label_match <- regmatches(spreadsheet_parameters, regexec(label_pattern, spreadsheet_parameters))[[1]][1]
      label <- gsub(paste0(label_form), '', label_match)
      label <- eval(parse(text = label))
    }
  } else {
    label <- NULL
  }
  return(label)
}
