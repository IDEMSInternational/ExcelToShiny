#' Title
#'
#' @param data_to_download todo
#' @param i   todo
#'
#' @return  todo
#' @export
#'
#' @examples  #todo
get_data_download <- function(data_to_download, i){
  return(eval(parse(text = data_to_download$value[i])))
}