library(rio)
library(plhR)
library(readxl)
library(tidyverse)
library(shiny)
library(shinydashboard)
source("functions_todo.R")
#excel_template_shiny <- read_excel("~/GitHub/plhR/excel_template_shiny.xlsx",
#                                   sheet = "demographics")

data_l <- import_list("~/GitHub/plhR/excel_template_shiny.xlsx")
our_data <- readRDS("srh_df.RDS")

status = "primary"
colour = "blue"
data_list = data_l
data_frame = our_data
PLH_shiny(title = "Testing Shiny Dashboard",
          data_list = data_l,
          data_frame = our_data)

# bar_table, box_function

# with changes in untitled 21, we never get
# [1] 2
# we should though.
# So is it just taking ages?


