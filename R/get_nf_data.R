#' Get notification data from metabase site
#'
#' @param site Connection to database to get original data (using DBI::dbConnect).
#'
#' @return Notification data from Metabase
#' @export
#'
#' @examples #TODO
get_nf_data <- function(site){
  df <- get_metabase_data(site = site, name = "app_notification_interaction")
  appdata_df <- list()
  
  for (i in 1:nrow(df)) {
    if (!is.na(df$notification_meta[i])){
      appdata_df[[i]] <- data.frame(jsonlite::fromJSON(df$notification_meta[i]))
    } else {
      appdata_df[[i]] <- data.frame(i)
    }
  }
  # combine the list into a data frame 
  appdata <- plyr::ldply(appdata_df)
  
  # bind into main data frame
  plhdata <- dplyr::bind_cols(df, appdata) %>% dplyr::select(-c("i"))
  
  return(plhdata)
}