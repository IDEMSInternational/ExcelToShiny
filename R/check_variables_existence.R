#' Add NA Variable
#'
#' Add NA values to the specified variables in the data frame if they do not exist.
#'
#' @param data A data frame. Default is `contacts_unflat`.
#' @param variable A character vector specifying the variable(s) to add NA values to.
#'
#' @return The modified data frame with NA values added.
#'
#' @examples
#' add_na_variable(data = mtcars, variable = c("cyl", "var2"))
#'
#' @export
add_na_variable <- function(data = contacts_unflat, variable){
  for (names in variable) {
    if (!names %in% colnames(data)) {
      data[, names] <- NA
      warning(paste(names, "does not exist. Adding NAs"))
    }
  }
  return(data)
}

#' Check Variables Existence
#'
#' Check the existence of variables in the specified data frames.
#'
#' @param df A data frame containing the variable names and data frame names.
#' @param data_frame The default data frame to check against if data frame names are not provided in df.
#'
#' @return A data frame containing the results of variable existence checks.
#'
#' @examples
#' #check_variables_existence(df = mtcars, data_frame = "my_default_df")
#'
#' @export
check_variables_existence <- function(df, data_frame) {
  results <- vector("logical", nrow(df))
  
  if (!is.null(df$variable)){
    if (is.null(df[["data"]]) || is.na(df[["data"]])) df$data <- data_frame
    
    for (i in 1:nrow(df)) {
      variable <- df$variable[i]
      data_name <- df[["data"]][i]
      
      if (!is.na(variable)){
        if (is.na(data_name)){
          data_frame_obj <- get(data_frame)
        } else {
          data_frame_obj <- get(data_name)
        }
        
        if (variable %in% names(data_frame_obj)) {
          results[i] <- TRUE
        } else {
          results[i] <- FALSE
        }
      } else {
        results[i] <- NA
      }
    }
    
    df$results <- results
    df <- df %>% dplyr::filter(results == FALSE)
    df <- df %>% dplyr::mutate(data = tidyr::replace_na(data, data_frame))
    return(df)
  }
}

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
