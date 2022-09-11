#' Creating top value boxes to be used in `PLH_shiny` function
#'
#' @param data_frame Data frame that contains the data to analyse.
#' @param text Variable containing titles to use in each value box.
#' @param colour Variable containing colours to use in each value box.
#' @param icon_pic Variable containing icons to use in each value box.
#' @param variable Variable containing the variable to use in each value box.
#' @param value_to_display Variable containing the value from the variable to display.
#'
#' @return Value box for use in `Shiny`.
#' @export
top_value_boxes <- function(data_frame, variable, value_to_display, colour, text, icon_pic){
  df_box <- summary_table(data_frame, factors = .data[[variable]], wider_table = TRUE, together = FALSE, naming_convention = FALSE)
  df_box <- df_box %>% dplyr::mutate(group = .data[[variable]], count = n) %>%
    dplyr::select(c(group, count))
  value <- (df_box %>% dplyr::filter(group == value_to_display))$count
  shinydashboard::valueBox(value, subtitle = text, icon = shiny::icon(icon_pic), color = colour)
}
