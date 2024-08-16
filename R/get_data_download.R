#' Prepare Data for Download in Shiny Application
#'
#' This function processes and retrieves data sets for download in a Shiny application. It evaluates the specified data expressions and returns the corresponding data sets.
#'
#' @param data_to_download A data frame containing the names and expressions for the data sets to be prepared for download. It should include columns `name` (the names of the data sets) and `value` (the expressions to be evaluated to retrieve the data).
#' @param i An integer index for iteration, typically used within a loop context. Default is `1`.
#'
#' @return A named list of data sets prepared for download. The names of the list elements correspond to the `name` column in `data_to_download`.
get_data_to_download <- function(data_to_download, i){
  data_names <- data_to_download$name
  data_to_download <- data_to_download$value
  data_sets <- NULL
  for (i in 1:length(data_to_download)){
    data_sets[[i]] <- eval(parse(text = data_to_download[i]))
  }
  names(data_sets) <- data_names
  return(data_sets)
}
