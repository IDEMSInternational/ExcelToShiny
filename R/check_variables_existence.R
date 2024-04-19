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