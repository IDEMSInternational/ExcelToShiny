#' Get notification data from metabase site
#'
#' @param site Connection to database to get original data (using DBI::dbConnect).
#'
#' @return Notification data from Metabase
#' @export
#'
#' @examples #TODO
get_nf_data <- function(site){
  df <- postgresr::get_nf_data(site = site)
  return(df)
}