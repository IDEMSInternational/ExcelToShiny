#' Get user data from metabase site
#'
#' @param site Connection to database to get original data (using DBI::dbConnect).
#' @param date_from Date to read in data from.
#' @param date_to Date to read in data to.
#' @param format_date Character string giving a date-time format as used by \code{\link[base]{strptime}}.
#' @param tzone_date Time zone specification to be used for the conversion, if one is required.
#' "UTC" by default. System-specific (see \code{\link[base]{as.POSIXlt}}), but "" is the current time zone, and "GMT" is UTC (Universal Time, Coordinated).
#' Invalid values are most commonly treated as UTC, on some platforms with a warning.
#' @param include_UIC_data logical. Default `TRUE`. Whether to merge UIC data.
#' @param merge_check logical. Default `TRUE`. Whether to display the close matches when merging UIC data in.
#' @param app_user_id Default "app_user_id". If `include_UIC_data = TRUE`, variable in the metabase data to merge the UIC data by.
#' @param UIC_Tracker If `include_UIC_data = TRUE`, UIC tracker data. 
#' @param join_UIC Default "UIC". If `include_UIC_data = TRUE`, variable to merge the UIC data by.
#' @param max_dist Default `5`. Maximum string distance when merging UIC Tracker data.
#'
#' @return User data from Metabase
#' @export
#' @importFrom utils capture.output
#'
#' @examples # TODO
get_user_data <- function(site, date_from, date_to = NULL, format_date = "%Y-%m-%d", tzone_date = "UTC", include_UIC_data = TRUE, merge_check = TRUE, UIC_Tracker, app_user_id = "app_user_id", join_UIC = "UIC", max_dist = 5){ # ideally would have flatten = FALSE in there, but seems that this isn't an option from metabase.
  df <- get_metabase_data(site = site, name = "app_users")
  
  # create empty list to store the data frames
  appdata_df <- list()
  
  # for each row of the data, get the contact_fields
  for (i in 1:nrow(df)) {
    appdata_df[[i]] <- data.frame(jsonlite::fromJSON(df$contact_fields[i]))
  }
  
  # combine the list into a data frame 
  appdata <- plyr::ldply(appdata_df)
  
  # bind into main data frame
  plhdata <- dplyr::bind_cols(df, appdata)
  
  # add UIC data?
  if (include_UIC_data){
    plhdata_org_fuzzy <- fuzzyjoin::stringdist_full_join(x = plhdata, y = UIC_Tracker, by = c(app_user_id = join_UIC[1]), max_dist = max_dist)
    
    #check the fuzzy matches 
    plhdata_org_fuzzy_comp <- plhdata_org_fuzzy %>% 
      dplyr::filter(!is.na(.data[[join_UIC]])) %>% 
      dplyr::filter(app_user_id!=.data[[join_UIC]] | is.na(app_user_id)) %>% 
      dplyr::select(app_user_id, .data[[join_UIC]])
    
    if (merge_check){
      if (yesno::yesno2("Fuzzy matches are:\n",
                        paste0(capture.output(plhdata_org_fuzzy_comp), collapse = "\n"),
                        "\nDo you want to merge these changes in?") == TRUE){
        return_data <- plhdata_org_fuzzy
      } else {
        warning("merging in fuzzy matches:\n", paste0(capture.output(plhdata_org_fuzzy_comp %>% dplyr::filter(stats::complete.cases(app_user_id))), collapse = "\n"))
        return_data <- plhdata_org_fuzzy
      }
    }else{
      return_data <- dplyr::full_join(x=plhdata, y=UIC_Tracker, by=c(app_user_id = join_UIC))
    }
    
  } else {
    return_data <- plhdata
  }
  
  if (!missing(date_from)){
    return_data <- return_data %>% dplyr::filter(as.POSIXct(date_from, format = format_date, tzone = tzone_date) < as.POSIXct(return_data$createdAt, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  if (!missing(date_to)){
    return_data <- return_data %>% dplyr::filter(as.POSIXct(date_to, format = format_date, tzone = tzone_date) > as.POSIXct(return_data$createdAt, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  
  return(return_data)
}