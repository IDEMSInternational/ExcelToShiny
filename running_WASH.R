library(rio)
library(plhR)
library(readxl)
library(tidyverse)
library(shiny)
library(shinydashboard)

## Testing with WASH data
data_l <- import_list("~/GitHub/plhR/WASH_shiny.xlsx")
#source("our_data_manipulation.R")

# this bit will eventually be in a data manipulation code file
our_data <- readRDS(file="C:/Users/lclem/OneDrive/Documents/GitHub/ParentAppDataScripts/WASHapp20230710.RDS")
our_data$rp.contact.field.app_launch_count <- as.numeric(our_data$rp.contact.field.app_launch_count)
our_data$rp.contact.field.max_days_between_app_launches <- as.numeric(our_data$rp.contact.field.max_days_between_app_launches)

# Then actually running it!
status = "primary"
colour = "blue"
data_list = data_l
data_frame = our_data
PLH_shiny(title = "Testing Shiny Dashboard",
          data_list = data_l,
          data_frame = our_data)

#'A lot to do:
#'Manipulation of variables - changing their names, removing bits (replace before, after, etc)
#'Manipulation of data frames - adding new data in new shapes. Add a "data" option in the excel sheet, which is default "global" - the main dta frame defined. But you can define it as something else, e.g. a manipulated data frame - see below - instead

# module_names <- c("introduction", "safe_food", "handwashing_with_soap", "safe_drinking_water",
#                   "waste", "celebration", "bathing", "healthy_families", "when_to_wash_your_hands",
#                   "clean_toilets", "healthy_homes", "how_to_wash_your_hands")
# 
# # Number who have completed each of these, in a single plot I think
# completed_modules <- paste0("rp.contact.field.", module_names, "_completed")
# module_names_order <- levels(as_factor(naming_conventions(completed_modules, replace = "rp.contact.field.",replace_after = "_completed")))
# # Completed Modules plot
# our_data %>% dplyr::select(all_of(completed_modules))
# summary_table_baseline_build <- summary_table_base_build(opt_factors = "ClusterName",
#                                                          data = our_data,
#                                                          columns_to_summarise = completed_modules,
#                                                          replace = "rp.contact.field.",
#                                                          replace_after = "_completed")
# perc_completed <- imap(summary_table_baseline_build, ~.x %>%
#                          mutate(n_completed = True,
#                                 perc_completed = round(True/(False + True + `NA`) * 100, 1)) %>%
#                          select(ClusterName, n_completed, perc_completed) %>%
#                          mutate(completion = paste0(n_completed, " (", perc_completed, "%)")))
# 
# perc_completed <- dplyr::bind_rows(perc_completed, .id = "Workshop")
# perc_completed_wider <- perc_completed %>%
#   pivot_wider(id_cols = "ClusterName", names_from = "Workshop", values_from = "completion")
# 
# ggplot(perc_completed, aes(x = Workshop, y = n_completed)) +
#   geom_histogram(stat = "identity") +
#   scale_x_discrete(limits = module_names_order)
