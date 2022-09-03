#' Get data from postgres site
#' 
#' @description Call ParentApp data from postgres
#'
#' @param site Connection to database to get original data (using DBI::dbConnect).
#' @param name Data to call from connection. Default `"app_users"`, but also takes `"app_notification_interaction"`.
#'
#' @return Data from postgres
#' @export
#'
#' @examples # TODO
get_postgres_data <- function(site, name = "app_users"){
  df <- postgresr::get_postgres_data(site = site, name = name)
  return(df)
}