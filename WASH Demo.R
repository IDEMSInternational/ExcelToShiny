WASH_data <- readRDS("C:/Users/lclem/source/repos/RInstat/instat/static/InstatObject/R/our_data.RDS")

ggplot(WASH_data, aes(y = rp.contact.field.app_launch_count)) + geom_boxplot()
median(plhdata_org_clean$rp.contact.field.app_launch_count, na.rm = TRUE)

# Module Names:
module_names <- c("introduction", "handwashing_with_soap", "when_to_wash_your_hands",
                  "how_to_wash_your_hands",  "healthy_families", "clean_toilets", "healthy_homes",
                  "safe_drinking_water","safe_food", "waste", "bathing", "celebration")

completed_modules <- paste0(module_names, "_completed_not_toggle")
completed_modules_toggle <- paste0(module_names, "_completed_toggle")
started_modules <- paste0(module_names, "_started_not_toggle")
started_modules_toggle <- paste0(module_names, "_started_toggle")
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

WASH_data_toggle$Module <- factor(WASH_data_toggle$Module)
WASH_data_toggle$Module <- fct_relevel(WASH_data_toggle$Module,
                                       levels(completed_modules_toggle))
WASH_data_toggle$Module <- as.numeric(WASH_data_toggle$Module )

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
select_items <- c("ClusterName", "n_started", "n_completed", "total")

relative_perc_completed <- imap(summary_table_completion_level, ~.x %>%
                                  mutate(Total = Total - `NA`,
                                         n_started = Total - `0`,
                                         n_completed = `100`,
                                         total = Total) %>%
                                  select(select_items))
relative_perc_completed <- plyr::ldply(relative_perc_completed)

head(relative_perc_completed)

relative_perc_completed_summary <- relative_perc_completed %>%
  group_by(`.id`) %>%
  summarise(sum_started = sum(n_started),
            sum_completed = sum(n_completed),
            sum_total = sum(total)) %>%
  mutate(Data = "ParentApp") %>%
  mutate(perc_complete = sum_completed/sum_total,
         perc_started = sum_started/sum_total)

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
  labs(x = "Workshop Week", y = "Percentage Completed",
       title = "Proportion of users who have completed a workshop",
       subtitle = "Non-toggle for WASH App") +
  ggthemes::scale_fill_colorblind() +
  ylim(c(0, 1))

###################################################
  


# STARTED A WORKSHOP ---------------------------------------
started_modules <- as_factor(started_modules)
started_modules_toggle <- as_factor(started_modules_toggle)

WASH_data_not_tog <- WASH_data %>%
  dplyr::select(c(app_user_id, started_modules)) %>%
  pivot_longer(cols = started_modules,
               names_to = "Module", 
               values_to = "Value") %>%
  group_by(Module, Value) %>%
  summarise(value = n()) %>%
  group_by(Module) %>%
  mutate(sum_total = sum(value)) %>%
  filter(Value == "Yes") %>%
  mutate(sum_started = value,
         perc_started = sum_started /sum_total,
         Data = "WASH")

WASH_data_toggle <- WASH_data %>%
  dplyr::select(c("app_user_id", started_modules_toggle)) %>%
  pivot_longer(cols = started_modules_toggle,
               names_to = "Module", 
               values_to = "Value") %>%
  group_by(Module, Value) %>%
  summarise(value = n()) %>%
  group_by(Module) %>%
  mutate(sum_total = sum(value)) %>%
  filter(Value == "Yes") %>%
  mutate(sum_started = value,
         perc_started = sum_started /sum_total,
         Data = "WASH")

WASH_data_toggle$Module <- factor(WASH_data_toggle$Module)
WASH_data_toggle$Module <- fct_relevel(WASH_data_toggle$Module,
                                       levels(started_modules_toggle))
WASH_data_toggle$Module <- as.numeric(WASH_data_toggle$Module )

WASH_data_not_tog$Module <- factor(WASH_data_not_tog$Module)
WASH_data_not_tog$Module <- fct_relevel(WASH_data_not_tog$Module,
                                        levels(started_modules))
levels(WASH_data_not_tog$Module )
WASH_data_not_tog$Module <- as.numeric(WASH_data_not_tog$Module )

WASH_data_summary <- WASH_data_not_tog %>%
  dplyr::select(`.id` = Module, sum_started, sum_total, perc_started, Data)

started_data <- bind_rows(parent_app_completion, WASH_data_summary)

ggplot(started_data, aes(x = factor(`.id`), y = perc_started, fill = Data)) +
  geom_histogram(stat = "identity", position = "dodge") +
  labs(x = "Workshop Week", y = "Percentage Started",
       title = "Proportion of users who have started a workshop",
       subtitle = "Non-toggle for WASH App") +
  ggthemes::scale_fill_colorblind() +
  ylim(c(0, 1))


# FOR PLH


