#' Change naming in tables/data sets
#' @description Quick and simple way to remove strings in a variable name, and have consistent naming structure in the package.
#'
#' @param x String containing variable name(s) to change.
#' @param replace String of which values to remove before the final string.
#' @param replace_after String of which values to remove after the final string.
#'
#' @return String containing variable name(s) in consistent naming convention
#' @export
#'
#' @examples
#' # String of values we want to change the naming convention in
#' data_hp_started <- c("rp.contact.field.w_1on1_hp_review_started",
#'                      "rp.contact.field.w_praise_hp_review_started",
#'                      "rp.contact.field.w_instruct_hp_review_started",
#'                      "rp.contact.field.w_stress_hp_review_started")
#' # Want to remove "rp.contact.field.w_" and "_hp_review_started"
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
