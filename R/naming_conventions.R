#' Standardise Naming Conventions for Variables
#'
#' This function standardises the naming conventions for a given set of variable names by removing specified patterns and capitalising the first letter.
#'
#' @param x A vector of strings containing variable names to standardise.
#' @param replace A string indicating the pattern to remove from the start of the variable names.
#' @param replace_after A string indicating the pattern to remove from the end of the variable names.
#'
#' @return A vector of strings containing the standardized variable names.
#' @export
#'
#' @examples
#' # Example usage:
#' data_hp_started <- c("rp.contact.field.w_1on1_hp_review_started",
#'                      "rp.contact.field.w_praise_hp_review_started",
#'                      "rp.contact.field.w_instruct_hp_review_started",
#'                      "rp.contact.field.w_stress_hp_review_started")
#' naming_conventions(data_hp_started, replace = "rp.contact.field.w_",
#'                    replace_after = "_hp_review_started")
naming_conventions <- function(x, replace, replace_after) {
  if (!missing(replace)){
    x <- gsub(paste("^.*?", replace, ".*", sep = ""), "", x)
  }
  if (!missing(replace_after)){
    x <- gsub(paste(replace_after, "$", sep = ""), "", x)
  }
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x <- gsub("_", " ", x)
  x
}