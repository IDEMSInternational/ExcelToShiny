#' Check for Unknown Variables in a Data Frame
#'
#' This function checks for any unknown variables in the provided data frame and returns a message indicating any issues found.
#'
#' @param df A data frame containing information about variables and data frames, including a column `variable` for variable names and a logical column `results` indicating whether each variable is known.
#'
#' @return A message indicating if any unknown variables were found. If unknown variables are detected, the function raises an error listing these variables and their associated data frames.
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
