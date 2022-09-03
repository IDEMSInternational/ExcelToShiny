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
get_postgres_user_data <- function(site, date_from, date_to = NULL, format_date = "%Y-%m-%d", tzone_date = "UTC", include_UIC_data = TRUE, merge_check = TRUE, UIC_Tracker, app_user_id = "app_user_id", join_UIC = "UIC", max_dist = 5){ # ideally would have flatten = FALSE in there, but seems that this isn't an option from metabase.
  df <- postgresr::get_postgres_data(site = site, date_from = date_from, date_to = date_to, format_date = format_date, tzone_date = tzone_date, include_UIC_data = include_UIC_data, merge_check = merge_check, 
                                     UIC_Tracker = UIC_Tracker, app_user_id = app_user_id, join_UIC = join_UIC, max_dist = max_dist)
  return(df)
}