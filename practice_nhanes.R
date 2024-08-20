library(rio)
#library(plhR)
library(readxl)
library(here)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(NHANES)


# load data sets and excel spreadsheet in
example_excel <- rio::import_list("vignettes/data/nhanes_data.xlsx")

View(NHANES)

build_shiny(title = "Testing Shiny Dashboard",
            data_list = example_excel,
            data_frame = NHANES_by_ind,
            status = "primary",
            colour = "blue",
            key_var = "ID")

NHANES %>% filter(!is.na(Depressed))



x <- NHANES_by_ind %>% filter(Diabetes == "Yes")

# DiabetesAge

ggplot(x) + geom_boxplot(aes(y = DiabetesAge))

