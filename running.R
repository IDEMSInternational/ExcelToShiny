
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
#data_l$contents <- data_l$contents[1:3,]

PLH_shiny1(title = "Testing Shiny Dashboard",
          data_list = data_l,
          data_frame = our_data,
          status = "primary",
          colour = "blue")

# bar_table, box_function

# with changes in untitled 21, we never get
# [1] 2
# we should though.
# So is it just taking ages?


