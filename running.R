
# Template file to run PLHR changes.

library(rio)
library(plhR)
library(readxl)
library(tidyverse)
library(shiny)
library(shinydashboard)
source("functions_todo.R")
#excel_template_shiny <- read_excel("~/GitHub/plhR/excel_template_shiny.xlsx",
#                                   sheet = "demographics")

## Testing with WASH data
data_l <- import_list("~/GitHub/plhR/WASH_shiny.xlsx")
source("~/GitHub/WASH_serifat/WASH/WASH_setup.R")
flow_checkin_data <- readRDS("C:/Users/lclem/OneDrive/Documents/GitHub/ParentText-data-analysis/R Code/flow_checkin_data.RDS")
df <- readRDS("C:/Users/lclem/OneDrive/Documents/GitHub/ParentText-data-analysis/R Code/df.RDS")
# Then actually running it!
#data_list = data_l
#data_frame = our_data
our_data <- our_data[1:489,]
our_data <- bind_cols(df, our_data)
#data_l$main_page <- NULL #data_l$contents[1:3,]
our_data$ID <- our_data$id...1

#data_l$main_page <- data_l$main_page %>% filter(type != "checkbox_group")
PLH_shiny1(title = "Testing Shiny Dashboard",
          data_list = data_l,
          data_frame = our_data,
          status = "primary",
          colour = "blue",
          key_var = "ID") # currently only one key variable ok.

# todo: build in that key_var determines updating other data frames. 
# so we need to run through the other sheets, check names of other data frames, get those data frame names
# and filter in them to ID == ... if it exists.




# TODO: for when we put it into PLH_shiny
checkbox_group_data <- (spreadsheet %>% filter(type == "checkbox_group"))
if (nrow(checkbox_group_data) > 1){
  checkbox_group_filtered <- eventReactive(ifelse(input$goButton_group == 0, 1, input$goButton_group), {
    variable <- checkbox_group_data$variable
    name <- checkbox_group_data$name
    filtered_data <- data %>%
      dplyr::filter(variable %in% c((input[[paste0("checkbox", name)]])))  # Org = variable in the row,
    return(filtered_data)
  })
}


# then we use checkbox_group_filtered instead of data in the rest of the places where we
# curretnly use data_frame in the PLH_shiny server
# and update the data in display_box?




# bar_table, box_function

# with changes in untitled 21, we never get
# [1] 2
# we should though.
# So is it just taking ages?


