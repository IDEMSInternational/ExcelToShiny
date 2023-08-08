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
    dplyr::mutate(x = ifelse(rowSums(!is.na(across(var_list))) == 0, "No", "Yes")) %>%
    dplyr::mutate(x = fct_relevel(x, c("Yes", "No")))
  return(data %>% dplyr::pull(x))
}

completed_module <- function(data, var_list = "introduction"){
  #var_list <- paste0(var, "_all_started")
  data <- data %>%
    dplyr::select(c(app_user_id, all_of(var_list))) %>%
    dplyr::mutate(x = ifelse(rowSums(!is.na(across(var_list))) == length(var_list), "Yes", "No")) %>%
    dplyr::mutate(x = fct_relevel(x, c("Yes", "No")))
  return(data %>% dplyr::pull(x))
}

module_completion_level <- function(data, var_list = "introduction", incl_first = TRUE){
  if (incl_first == FALSE) var_list <- var_list[-1]
  data <- data %>%
    dplyr::select(c(app_user_id, all_of(var_list))) %>% 
    mutate(across(var_list,
                  #~str_count(., ";") + 1)) %>%
                  ~!is.na(.))) %>%
    pivot_longer(cols = var_list) %>%
    group_by(name) %>%
    mutate(nrow = n()) %>%
    group_by(name, value, nrow) %>%
    summarise(prop = n()) %>%
    filter(value == TRUE) %>%
    mutate(started = prop / nrow * 100)
  return(data)
}

### DATA 
# this bit will eventually be in a data manipulation code file
our_data <- readRDS(file="C:/Users/lclem/OneDrive/Documents/GitHub/ParentAppDataScripts/WASHData_20230725.RDS")
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
var_names$how_to_wash_your_hands_all[which(var_names$how_to_wash_your_hands_all == "rp.contact.field.how_to_wash_your_hands_when_handwash_card_click_history")] <- ""
# variables related to started
var_names_non_toggle <- purrr::map(.x = names(var_names),
                                .f = ~var_names[[.x]][grepl("_card_click_history", var_names[[.x]])])
names(var_names_non_toggle) <- paste0(module_names, "_started_not_toggle")
var_names_non_toggle$introduction_started_not_toggle <- "rp.contact.field.introduction_card_click_history"
var_names_non_toggle$celebration_started_not_toggle <- "rp.contact.field.celebration_card_click_history"
start_non_tog <- purrr::map(.x = var_names_non_toggle, .f = ~ started_module(our_data, var_list = .x))
names(var_names_non_toggle) <- paste0(module_names, "_completed_not_toggle")
var_names_non_toggle$introduction_completed_not_toggle <- "rp.contact.field.introduction_card_click_history"
var_names_non_toggle$celebration_completed_not_toggle <- "rp.contact.field.celebration_card_click_history"
complete_non_tog <- purrr::map(.x = var_names_non_toggle, .f = ~ completed_module(our_data, var_list = .x))

var_names_toggle <- purrr::map(.x = names(var_names), .f = ~var_names[[.x]][grepl("_completed_history", var_names[[.x]])])
names(var_names_toggle) <- paste0(module_names, "_started_toggle")
start_tog <- purrr::map(.x = var_names_toggle, .f = ~ started_module(our_data, var_list = .x))
names(var_names_toggle) <- paste0(module_names, "_completed_toggle")
complete_tog <- purrr::map(.x = var_names_toggle, .f = ~ completed_module(our_data, var_list = .x))

start_non_tog$introduction_started_not_toggle

our_data <- dplyr::bind_cols(our_data, start_non_tog)
our_data <- dplyr::bind_cols(our_data, complete_non_tog)
our_data <- dplyr::bind_cols(our_data, start_tog)
our_data <- dplyr::bind_cols(our_data, complete_tog)

# 

# started a module
# add data column in shiny sheet, but it's a list. 
# state that it is a list, then we read in the list name (e..g, "handwashing_with_soap_started_not_toggle")
mod_compl_nt <- purrr::map(.x = var_names_non_toggle[c(2:5, 7:12)], .f = ~ module_completion_level(our_data, .x, FALSE))
mod_compl_t <- purrr::map(.x = var_names_toggle[c(2:5, 7:12)], .f = ~ module_completion_level(our_data, .x))





#saveRDS(our_data, "WASHData_with_toggles_20230725.RDS")
# View(our_data %>% dplyr::select(var_names$how_to_wash_your_hands_all[1:8],
#                                 "how_to_wash_your_hands_started_not_toggle",
#                                 "how_to_wash_your_hands_completed_not_toggle",
#                                 "how_to_wash_your_hands_started_toggle",
#                                 "how_to_wash_your_hands_completed_toggle"))
# 
# View(our_data %>% dplyr::select(var_names$introduction_all,
#                                 "introduction_started_not_toggle",
#                                 "introduction_completed_not_toggle",
#                                 "introduction_started_toggle",
#                                 "introduction_completed_toggle"))
# 
# # and for completed
# 
# 
# 
# 
# # our_data %>% filter(healthy_homes_started_not_toggle != healthy_homes_started_toggle) %>% View()
# 
# # if these are all NOT NA, then they have "completed" it in terms of the parentapp completion
# # if at least one NA, then they have "completed" it in terms of the parentapp completion
# our_data$rp.contact.field.waste_how_card_click_history
# our_data$rp.contact.field.waste_why_card_click_history
# our_data$rp.contact.field.waste_what_card_click_history
# our_data$rp.contact.field.waste_reduce_card_click_history
