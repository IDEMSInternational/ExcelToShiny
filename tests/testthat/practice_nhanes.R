library(rio)
library(plhR)
library(readxl)
library(here)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(NHANES)

# load data sets and excel spreadsheet in
data_l <- import_list("nhanes_data.xlsx")
data(NHANES)

NHANES_by_ind <- NHANES %>%
  group_by(ID) %>%
  mutate(count = 1:n()) %>%
  filter(count == 1)

NHANES_by_ind$ID <- as.character(NHANES_by_ind$ID)

# run code
PLH_shiny1(title = "Testing Shiny Dashboard",
          data_list = data_l,
          data_frame = NHANES_by_ind,
          status = "primary",
          colour = "blue",
          key_var = "ID")
