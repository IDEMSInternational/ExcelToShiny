#' Get Parameter Value from Spreadsheet Parameters
#'
#' This function extracts a parameter value from a string that represents a set of spreadsheet parameters. The function supports extracting values as strings, lists, or logical values.
#'
#' @param spreadsheet_parameters A character string containing the spreadsheet parameters, formatted as key-value pairs.
#' @param name The name of the parameter whose value you want to extract. Default is `"label"`.
#' @param list Logical value indicating whether the parameter value should be treated as a list. Default is `FALSE`.
#' @param logical Logical value indicating whether the parameter value should be treated as a logical (`TRUE` or `FALSE`). Default is `FALSE`.
#' @param date Logical value indicating whether the parameter value should be treated as a date (`TRUE` or `FALSE`). Default is `FALSE`.
#' @return The value of the specified parameter. The return type depends on the `list` and `logical` arguments:
#' \itemize{
#'   \item If `list = FALSE` and `logical = FALSE`, it returns the parameter as a string.
#'   \item If `list = TRUE`, it returns the parameter as a character vector (list).
#'   \item If `logical = TRUE`, it returns the parameter as a logical value.
#' }
#' If the parameter is not found, the function returns `NULL`.
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
#' get_parameter_value(param_list, name = "options", list = TRUE)
#'
#' # Extract a logical parameter value
#' param_logical <- 'is_active = TRUE'
#' get_parameter_value(param_logical, name = "is_active", logical = TRUE) 
#'
#' # Parameter not found
#' param_not_found <- 'other_param = 42'
#' get_parameter_value(param_not_found)  # Returns NULL
get_parameter_value <- function(spreadsheet_parameters, name = "label", list = FALSE, logical = FALSE, date = FALSE) {
  param_string <- spreadsheet_parameters
  
  # Pattern: name = value (with optional spaces)
  if (date) {
    # Match date as string or wrapped in as.Date()
    date_pattern <- paste0(name, "\\s*=\\s*(?:as.Date\\()?[\"'](.*?)[\"']\\)?")
    match <- stringr::str_match(param_string, date_pattern)[, 2]
    return(if (!is.na(match)) match else NULL)
  }
  
  if (logical) {
    logic_pattern <- paste0(name, "\\s*=\\s*(TRUE|FALSE)")
    match <- stringr::str_match(param_string, logic_pattern)[, 2]
    return(if (!is.na(match)) as.logical(match) else NULL)
  }
  
  if (list) {
    list_pattern <- paste0(name, "\\s*=\\s*c\\((.*?)\\)")
    match <- stringr::str_match(param_string, list_pattern)[, 2]
    if (!is.na(match)) {
      list_str <- paste0("c(", match, ")")
      return(eval(parse(text = list_str)))
    } else {
      return(NULL)
    }
  }
  
  # Default: quoted string
  str_pattern <- paste0(name, "\\s*=\\s*['\"](.*?)['\"]")
  match <- stringr::str_match(param_string, str_pattern)[, 2]
  return(if (!is.na(match)) match else NULL)
}
