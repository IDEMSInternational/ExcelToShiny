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
  matches <- stringr::str_extract(spreadsheet_parameters, paste0(name, "\\s*=\\s*"))
  label_form <- unique(matches[!is.na(matches)])
  
  if (length(label_form) == 0) label_form <- NULL
  
  if (!is.null(label_form)){
    if (date == TRUE){
      label_pattern <- paste0(label_form, '"(.*?)"')  # Match as.Date("value")
      label_match <- regmatches(spreadsheet_parameters, regexec(label_pattern, spreadsheet_parameters))[[1]][1]
      
      # if it's NA, check in case as.Date has been added:
      if (is.na(label_match)){
        # Match for both simple and as.Date formats
        label_form <- paste0(label_form, "as.Date\\(")  # Adjust for as.Date() structure
        label_pattern <- paste0(label_form, '"(.*?)"\\)')  # Match as.Date("value")
        
        # Extract the full match
        label_match <- regmatches(spreadsheet_parameters, regexec(label_pattern, spreadsheet_parameters))[[1]][1]
      }
      # Clean up to extract the date (removes name=as.Date( and the surrounding quotes)
      label <- gsub(paste0(label_form, '"|"|\\)'), '', label_match)
    } else if (list == FALSE){
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
