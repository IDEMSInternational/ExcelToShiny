#' Title
#'
#' @param data_to_download todo
#' @param i   todo
#'
#' @return  todo
#' @export
#'
#' @examples  #todo
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
