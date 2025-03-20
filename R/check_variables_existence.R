#' Check Variables Existence in Specified Data Frames
#'
#' This function checks the existence of specified variables within designated data frames. It returns a data frame indicating which variables are missing from their respective data frames.
#'
#' @param df A data frame containing at least two columns: `variable`, which holds the names of the variables to check, and `data`, which contains the names of the data frames where these variables should be found. If the `data` column is not provided or contains `NA`, the function uses `data_frame` as the default data frame.
#' @param data_frame The default data frame to check against if data frame names are not provided in `df`.
#'
#' @return A data frame containing only the rows where the specified variables were not found in the corresponding data frames. The returned data frame includes a `results` column indicating the success of the check.
check_variables_existence <- function(df, data_frame) {
  results <- vector("logical", nrow(df))
  if (!is.null(df$variable)){
    if (is.null(df[["data"]]) || any(is.na(df[["data"]]))) {
      if (is.null(df[["data"]])){
        df$data <- data_frame
      } else {
        df$data[is.na(df$data)] <- data_frame
      }
      #df$data <- data_frame
    }
    for (i in 1:nrow(df)) {
      variable <- df$variable[i]
      data_name <- df[["data"]][i]
      
      if (!is.na(variable)){
        if (is.na(data_name)){
          data_frame_obj <- get(data_frame, envir = parent.frame(2))
        } else {
          data_frame_obj <- get(data_name, envir = parent.frame(2))
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