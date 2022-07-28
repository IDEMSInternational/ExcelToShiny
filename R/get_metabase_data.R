#' Get data from metabase site
#' 
#' @description Call ParentApp data from Metabase
#'
#' @param site Connection to database to get original data (using DBI::dbConnect).
#' @param name Data to call from connection. Default `"app_users"`, but also takes `"app_notification_interaction"`.
#'
#' @return Data from Metabase site
#' @export
#'
#' @examples # TODO
get_metabase_data <- function(site, name = "app_users"){
  plh_tables <- DBI::dbListTables(site)
  df <- DBI::dbReadTable(conn = site,
                         name = name)
  return(df)
}