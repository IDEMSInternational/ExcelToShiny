WASH_data <- readRDS("C:/Users/lclem/source/repos/RInstat/instat/static/InstatObject/R/our_data.RDS")

ggplot(WASH_data, aes(y = rp.contact.field.app_launch_count)) + geom_boxplot()
median(plhdata_org_clean$rp.contact.field.app_launch_count, na.rm = TRUE)

# Module Names:
module_names <- c("introduction", "handwashing_with_soap", "when_to_wash_your_hands",
                  "how_to_wash_your_hands",  "healthy_families", "clean_toilets", "healthy_homes",
                  "safe_drinking_water","safe_food", "waste", "bathing", "celebration")

completed_modules <- paste0(module_names, "_completed_not_toggle")
completed_modules_toggle <- paste0(module_names, "_completed_toggle")

# 589 individuals have manually said they've completed all the sections in the introduction:
WASH_data %>% 
  group_by(rp.contact.field.introduction_completed) %>%
  summarise(n())
# 613 individuals have not manually said completed
WASH_data %>% 
  group_by(introduction_completed_not_toggle) %>%
  summarise(n())

completed_modules <- as_factor(completed_modules)
completed_modules_toggle <- as_factor(completed_modules_toggle)

WASH_data_not_tog <- WASH_data %>%
  dplyr::select(c(app_user_id, completed_modules)) %>%
  pivot_longer(cols = completed_modules,
               names_to = "Module", 
               values_to = "Value") %>%
  group_by(Module, Value) %>%
  summarise(value = n()) %>%
  group_by(Module) %>%
  mutate(sum_total = sum(value)) %>%
  filter(Value == "Yes") %>%
  mutate(sum_completed = value,
         perc_complete = sum_completed /sum_total,
         Data = "WASH")

WASH_data_toggle <- WASH_data %>%
  dplyr::select(c("app_user_id", completed_modules_toggle)) %>%
  pivot_longer(cols = completed_modules_toggle,
               names_to = "Module", 
               values_to = "Value") %>%
  group_by(Module, Value) %>%
  summarise(value = n()) %>%
  group_by(Module) %>%
  mutate(sum_total = sum(value)) %>%
  filter(Value == "Yes") %>%
  mutate(sum_completed = value,
         perc_complete = sum_completed /sum_total,
         Data = "WASH")

WASH_data_not_tog$Module <- factor(WASH_data_not_tog$Module)
WASH_data_not_tog$Module <- fct_relevel(WASH_data_not_tog$Module,
                                                levels(completed_modules))
levels(WASH_data_not_tog$Module )
WASH_data_not_tog$Module <- as.numeric(WASH_data_not_tog$Module )
  
WASH_data_summary <- WASH_data_not_tog %>%
  dplyr::select(`.id` = Module, sum_completed, sum_total, perc_complete, Data)

##################### PLH DATA ########################
summary_table_baseline_build <- summary_table_base_build(opt_factors = "ClusterName",
                                                         data = plhdata_org_clean,
                                                         columns_to_summarise = data_completion_level,
                                                         replace = "rp.contact.field.w_",
                                                         replace_after = "_completion_level")
summary_table_baseline_build <- summary_table_baseline_build %>%
  purrr::map(.f =~.x %>% mutate_all(~replace(., is.na(.), 0)))
summary_table_baseline_build <- summary_table_baseline_build %>% purrr::map(.f =~.x %>% janitor::adorn_totals(c("row", "col")))


# Number started, Number Completed, Avg workshop completion

summary_table_completion_level <- summary_table_baseline_build
for (i in 1:length(summary_table_completion_level)){
  if (!"100" %in% names(summary_table_completion_level[[i]])){
    summary_table_completion_level[[i]]$`100` <- 0
  }
}
select_items <- c("ClusterName", "n_started", "perc_started", "n_completed", "perc_completed")

relative_perc_completed <- imap(summary_table_completion_level, ~.x %>%
                                  mutate(Total = Total - `NA`,
                                         n_started = Total - `0`,
                                         perc_started = round(n_started/Total * 100, 1),
                                         total = Total,
                                         n_completed = `100`) %>%
                                  select(total, n_completed, ClusterName))
relative_perc_completed <- plyr::ldply(relative_perc_completed)

head(relative_perc_completed)

relative_perc_completed_summary <- relative_perc_completed %>%
  group_by(`.id`) %>%
  summarise(sum_completed = sum(n_completed),
            sum_total = sum(total)) %>%
  mutate(Data = "ParentApp") %>%
  mutate(perc_complete = sum_completed/sum_total)

data_completion_level_naming <- naming_conventions(data_completion_level, replace = "rp.contact.field.w_", replace_after = "_completion_level")

data_completion_level_naming <- as_factor(data_completion_level_naming)

relative_perc_completed_summary <- relative_perc_completed_summary %>%
  mutate(`.id` = fct_relevel(`.id`, levels(data_completion_level_naming))) 
levels(relative_perc_completed_summary$`.id`) <- c(levels(relative_perc_completed_summary$`.id`))
relative_perc_completed_summary$`.id` <- as.numeric(relative_perc_completed_summary$`.id`)
head(relative_perc_completed_summary)

parent_app_completion <- relative_perc_completed_summary

parent_app_completion <- parent_app_completion %>%
  mutate(`.id` = ifelse(`.id` == 9, 0,
                        ifelse(`.id` >= 6 & `.id` < 9, `.id` + 1,
                               `.id`))) %>%
  mutate(`.id` = ifelse(`.id` == 0, 6, `.id`))

###################################################

completion_data <- bind_rows(parent_app_completion, WASH_data_summary)

ggplot(completion_data, aes(x = factor(`.id`), y = perc_complete, fill = Data)) +
  geom_histogram(stat = "identity", position = "dodge") +
  labs(x = "Workshop Week", y = "Percentage Completed") +
  ggthemes::scale_fill_colorblind()

###################################################
  

#### STARTED A WORKSHOP??? ################################################################

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
var_names_started <- purrr::map(.x = names(var_names),
                                .f = ~var_names[[.x]][grepl("_card_click_history", var_names[[.x]])])
names(var_names_started) <- paste0(module_names, "_started")
x <- purrr::map(.x = var_names_started, .f = ~ started_module(our_data, var_list = .x))
our_data <- dplyr::bind_cols(our_data, x)



####
# our_data$rp.contact.field.waste_card_click_history
# 
# # if these are all NOT NA, then they have "completed" it in terms of the parentapp completion
# # if at least one NA, then they have "completed" it in terms of the parentapp completion
# our_data$rp.contact.field.waste_how_card_click_history
# our_data$rp.contact.field.waste_why_card_click_history
# our_data$rp.contact.field.waste_what_card_click_history
# our_data$rp.contact.field.waste_reduce_card_click_history


  
  
  

 
# WASH PLOT





ggplot(plhdata_org_clean_longer, aes(x = Module)) +
  geom_bar()

naming_conventions <- function(x, replace, replace_after, rename = TRUE) {
  if (!missing(replace)){
    x <- gsub(paste("^.*?", replace, ".*", sep = ""), "", x)
  }
  if (!missing(replace_after)){
    x <- gsub(paste(replace_after, "$", sep = ""), "", x)
  }
  if (rename){
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x <- gsub("_", " ", x) 
  }
  x
}

plhdata_org_clean_longer$Module <- naming_conventions(plhdata_org_clean_longer$Module, replace = "rp.contact.field.", replace_after = "_completed")

module_order <- as_factor(naming_conventions(module_names))
#levels(module_order)
#?as.factor
ggplot(plhdata_org_clean_longer, aes(x = Module)) +
  geom_bar() +
  scale_x_discrete(limits = module_order) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#WASH_data <- plhdata_org_clean_longer

# Get the same thing, but with PA data - 


data_completion_level

plhdata_org_clean$rp.contact.field.w_self_care_completion_level
