# Template file to run PLHR changes.
library(rio)
library(plhR)
library(readxl)
library(tidyverse)
library(shiny)
library(shinydashboard)
source("functions_todo.R")

## Testing with WASH data
data_l <- import_list("~/GitHub/plhR/WASH_shiny1.xlsx")
source("~/GitHub/WASH_serifat/WASH/WASH_setup.R")
flow_checkin_data <- readRDS("C:/Users/lclem/OneDrive/Documents/GitHub/ParentText-data-analysis/R Code/flow_checkin_data.RDS")
df <- readRDS("C:/Users/lclem/OneDrive/Documents/GitHub/ParentText-data-analysis/R Code/df.RDS")
# Then actually running it!
#data_list = data_l
#data_frame = our_data
our_data <- our_data[1:489,]
our_data <- bind_cols(df, our_data)
# data_l$main_page <- data_l$main_page %>% filter(type != "checkbox_group") #data_l$contents[1:3,]
our_data$uuid <- our_data$id...1

#save(our_data, flow_checkin_data, file = "data_for_shiny_example.rds")

getwd()
#load("data_for_shiny_example.rds")
head(our_data)
#flow_checkin_data$uuid <- NULL
#our_data <- head(our_data)
# data_l[[10]] <- NULL
# data_l[[9]] <- NULL
# data_l$contents <- data_l$contents %>% filter(name != "Modules") #data_l$contents[1:3,]
PLH_shiny(title = "Testing Shiny Dashboard",
          data_list = data_l,
          data_frame = our_data,
          status = "primary",
          colour = "blue",
          key_var = "uuid") # currently only one key variable.

# TODO have a "linked" column to accompany the data column in the spreadsheet data 
# that column then means you link it with the key_var column (we can essentially just rename the linked col to be the key_var (e.g., "uuid") col in that df.)


# need to fix up the filtering code.

# 
# data(mtcars)
# data(iris)
# mtcars$ID <- rep(c(1, 2), 16)
# iris$ID <- rep(c(1, 2), 75)
# key_var <- "ID"
# valid_ids <- 2
# first_load <- TRUE
# 
# list_of_df_names <- c("mtcars", "iris")
# 
# # then do this for every df_name
# if (first_load){
#   for (df_name in list_of_df_names){
#     new_name <- paste0(df_name, "_1")
#     assign(new_name, get(df_name, envir = .GlobalEnv), envir = .GlobalEnv)
#     
#     filtered_df <- get(df_name, envir = .GlobalEnv) %>%
#       dplyr::filter(.data[[key_var]] %in% valid_ids)
#     assign(df_name, filtered_df, envir = .GlobalEnv)
#   }
#   first_load <- FALSE
# } else {
#   print("A")
#   for (df_name in list_of_df_names){
#     
#     filtered_df <- get(paste0(df_name, "_1"), envir = .GlobalEnv) %>%
#       dplyr::filter(.data[[key_var]] %in% valid_ids)
#     assign(df_name, filtered_df, envir = .GlobalEnv)
#   }
# }
