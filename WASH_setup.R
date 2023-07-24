### FUNCTIONS
# get names
get_var_names <- function(data, field){
  var_names <- names(data %>% dplyr::select(starts_with(paste0("rp.contact.field.", field))))
  return(var_names)
}
# started a module?
started_module <- function(data, var_list = "introduction"){
  #var_list <- paste0(var, "_all_started")
  data <- data %>%
    dplyr::select(c(app_user_id, all_of(var_list))) %>%
    mutate(x = ifelse(rowSums(!is.na(across(var_list))) == 0, "No", "Yes")) %>%
    mutate(x = fct_relevel(x, c("Yes", "No")))
  return(data %>% dplyr::pull(x))
}

### DATA 
# this bit will eventually be in a data manipulation code file
our_data <- readRDS(file="C:/Users/lclem/OneDrive/Documents/GitHub/ParentAppDataScripts/WASHapp20230710.RDS")
our_data$rp.contact.field.app_launch_count <- as.numeric(our_data$rp.contact.field.app_launch_count)
our_data$rp.contact.field.max_days_between_app_launches <- as.numeric(our_data$rp.contact.field.max_days_between_app_launches)

module_names <- c("introduction", "handwashing_with_soap", "when_to_wash_your_hands",
                  "safe_drinking_water",
                  "waste", "celebration", "safe_food",  "bathing", "healthy_families",
                  "clean_toilets", "healthy_homes", "how_to_wash_your_hands")
our_data <- our_data %>%
  mutate(across(ends_with("_completed"), ~ifelse(. == "true", "Yes",
                                                ifelse(. == "false", "No",
                                                       "NA")))) %>%
  mutate(across(ends_with("_completed"), ~fct_relevel(., c("Yes", "No"))))

var_names <- purrr::map(.x = module_names, .f = ~ get_var_names(our_data, .x))
names(var_names) <- paste0(module_names, "_all")
var_names_started <- purrr::map(.x = names(var_names),
                                .f = ~var_names[[.x]][grepl("_card_click_history", var_names[[.x]])])
names(var_names_started) <- paste0(module_names, "_started")
x <- purrr::map(.x = var_names_started, .f = ~ started_module(our_data, var_list = .x))
our_data <- dplyr::bind_cols(our_data, x)

