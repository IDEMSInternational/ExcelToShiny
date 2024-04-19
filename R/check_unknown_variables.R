#' Check Unknown Variables
#'
#' Check for unknown variables in the provided data frame.
#'
#' @param df A data frame containing information about variables and data frames.
#'
#' @return A message indicating any unknown variables found.
#'
#' @examples
#' check_unknown_variables(df = my_df)
#'
#' @export
check_unknown_variables <- function(df) {
  unknown_vars <- df$variable[!df$results]
  unknown_dataframes <- df$data[!df$results]
  if (length(unknown_vars) == 0) {
    return("No unknown variables.")
  } else {
    message <- "Unknown variables in data frames: "
    for (i in seq_along(unknown_vars)) {
      message <- paste0(message, "'", unknown_vars[i], "' in '", unknown_dataframes[i], "' ")
    }
    stop(message)
  }
}
