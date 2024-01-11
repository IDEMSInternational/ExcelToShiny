
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
our_data$uuid <- our_data$id...1

#data_l$main_page <- data_l$main_page %>% filter(type != "checkbox_group")
PLH_shiny1(title = "Testing Shiny Dashboard",
          data_list = data_l,
          data_frame = our_data,
          status = "primary",
          colour = "blue",
          key_var = "uuid") # currently only one key variable ok.


# THIS IS TO UPDATE A DATA FRAME, NOT TO GIVE A VALUE BOX
# LOOK AT PARENTAPP FOR HELP ON THAT.


# run through the other sheets and get all data frame names
list_of_df_names <- NULL
for (i in 1:length(data_list)){
  data_l_dfs <- data_list[[i]]$data
  if (!is.null(data_l_dfs)){
    list_of_df_names[[i]] <- unique(data_l_dfs)
  }
}
list_of_df_names <- unlist(list_of_df_names)

function_B <- function(i, first_load){
  if (first_load){
    filtered_df <- get(df_name, envir = .GlobalEnv) %>%
      dplyr::filter(.data[[key_var]] %in% valid_ids())
    assign(df_name, filtered_df, envir = .GlobalEnv)  
  } else {
    filtered_df <- get(paste0(df_name, "_1"), envir = .GlobalEnv) %>%
      dplyr::filter(.data[[key_var]] %in% valid_ids())
    assign(df_name, filtered_df, envir = .GlobalEnv)
  }
  return(get(df_name))
}

# TOMORROW: Try this code (the above relevant code is in the doc already)
# i've put it in on line 132. Try it tomorrow :) 
for (df_name in list_of_df_names){
  new_name <- paste0(df_name, "_1")
  assign(new_name, eventReactive(ifelse(input$goButton_group == 0, 1, input$goButton_group), {
    df_name
  }))
  assign(df_name, eventReactive(ifelse(input$goButton_group == 0, 1, input$goButton_group), {
    function_B(df_name, first_load = first_load)
  }))
}
first_load <- FALSE


# or try this: (which is just with a first_load spread out more)
if (first_load){
  for (df_name in list_of_df_names){
    new_name <- paste0(df_name, "_1")
    assign(new_name, eventReactive(ifelse(input$goButton_group == 0, 1, input$goButton_group), {
      df_name
    }))
    assign(df_name, eventReactive(ifelse(input$goButton_group == 0, 1, input$goButton_group), {
      function_B(df_name, first_load = TRUE)
    }))
  }
  first_load <- FALSE
} else {
  for (df_name in list_of_df_names){
    assign(df_name, eventReactive(ifelse(input$goButton_group == 0, 1, input$goButton_group), {
      function_B(df_name, first_load = FALSE)
    }))
  }
}

#####
#####
#####

# TRY 2:
if (first_load){
  for (df_name in list_of_df_names){
    #new_name <- paste0(df_name, "_1")
    #assign(new_name, get(df_name, envir = .GlobalEnv), envir = .GlobalEnv)
    
    mydataname_1 <- eventReactive(ifelse(input$goButton_group == 0, 1, input$goButton_group), {
      df_name
      #new_name <- paste0(df_name, "_1")
      #assign(new_name, get(df_name, envir = .GlobalEnv), envir = .GlobalEnv)
    })
    mydataname <- eventReactive(ifelse(input$goButton_group == 0, 1, input$goButton_group), {
      function_B(i = df_name, first_load = TRUE)
    })
  }
  first_load <- FALSE
} else {
  for (df_name in list_of_df_names){
    mydataname <- eventReactive(ifelse(input$goButton_group == 0, 1, input$goButton_group), {
      function_B(i = df_name, first_load = FALSE)
    })
  }
}



#




### option two - just all the code together instead of functino_B
if (first_load){
  for (df_name in list_of_df_names){
    new_name <- paste0(df_name, "_1")
    assign(new_name, get(df_name, envir = .GlobalEnv), envir = .GlobalEnv)
    
    filtered_df <- get(df_name, envir = .GlobalEnv) %>%
      dplyr::filter(.data[[key_var]] %in% valid_ids)
    assign(df_name, filtered_df, envir = .GlobalEnv)
  }
  first_load <- FALSE
} else {
  for (df_name in list_of_df_names){
    
    filtered_df <- get(paste0(df_name, "_1"), envir = .GlobalEnv) %>%
      dplyr::filter(.data[[key_var]] %in% valid_ids)
    assign(df_name, filtered_df, envir = .GlobalEnv)
  }
}




# a function like our reactive one, which won't work though.


if (nrow(data_list$main_page %>% filter(...))>0){
  # run through the other sheets and get all data frame names
  list_of_df_names <- NULL
  for (i in 1:length(data_l)){
    data_l_dfs <- data_l[[i]]$data
    if (!is.null(data_l_dfs)){
      list_of_df_names[[i]] <- unique(data_l_dfs)
    }
  }
  list_of_df_names <- unlist(list_of_df_names)
  
  # then for every df name, we save it as _1 first time around to avoid any overwrites when re-filtering
  # and we then return(filtered_df) - our filtered data frame to the key_var (ID) to be valid ids
  function_B <- function(df_name = df_name, first_load){
    if (first_load){
      filtered_df <- get(df_name, envir = .GlobalEnv) %>%
        dplyr::filter(.data[[key_var]] %in% valid_ids())
      assign(df_name, filtered_df, envir = .GlobalEnv)  
    } else {
      filtered_df <- get(paste0(df_name, "_1"), envir = .GlobalEnv) %>%
        dplyr::filter(.data[[key_var]] %in% valid_ids())
      assign(df_name, filtered_df, envir = .GlobalEnv)
    }
    return(get(df_name))
  }
  
  function_c <- function(df_name, first_load){
    if (first_load){
      new_name <- paste0(df_name, "_1")
      assign(new_name, get(df_name, envir = .GlobalEnv), envir = .GlobalEnv)
      function_B(df_name = df_name, first_load = TRUE)
      ### have to have our output[[ID]] <- render... here.
    } else {
      function_B(i = df_name, first_load = FALSE)
    }
  }
  for (df_name in list_of_df_names){
    function_c(df_name = df_name, first_load = first_load)
  }
  first_load <- FALSE
}




# then for every df name, we save it as _1 first time around to avoid any overwrites when re-filtering
# and we then return(filtered_df) - our filtered data frame to the key_var (ID) to be valid ids
# TODO: is to set valid_ids as valid_ids()
# TODO: is to then run this like we run our display_value_boxes - needs to be saved and run for them all.
function_B <- function(i, first_load){
  if (first_load){
    filtered_df <- get(df_name, envir = .GlobalEnv) %>%
      dplyr::filter(.data[[key_var]] %in% valid_ids())
    assign(df_name, filtered_df, envir = .GlobalEnv)  
  } else {
    filtered_df <- get(paste0(df_name, "_1"), envir = .GlobalEnv) %>%
      dplyr::filter(.data[[key_var]] %in% valid_ids())
    assign(df_name, filtered_df, envir = .GlobalEnv)
  }
  return(get(df_name))
}

if (first_load){
  for (df_name in list_of_df_names){
    new_name <- paste0(df_name, "_1")
    assign(new_name, get(df_name, envir = .GlobalEnv), envir = .GlobalEnv)
    function_B(i = df_name, first_load = TRUE)
  }
  first_load <- FALSE
} else {
  for (df_name in list_of_df_names){
    function_B(i = df_name, first_load = FALSE)
  }
}










if (first_load){
  for (df_name in list_of_df_names){
    new_name <- paste0(df_name, "_1")
    assign(new_name, get(df_name, envir = .GlobalEnv), envir = .GlobalEnv)
    filtered_df <- get(df_name, envir = .GlobalEnv) %>%
      dplyr::filter(.data[[key_var]] %in% valid_ids)
    assign(df_name, filtered_df, envir = .GlobalEnv)
  }
  first_load <- FALSE
} else {
  for (df_name in list_of_df_names){
    filtered_df <- get(paste0(df_name, "_1"), envir = .GlobalEnv) %>%
      dplyr::filter(.data[[key_var]] %in% valid_ids)
    assign(df_name, filtered_df, envir = .GlobalEnv)
  }
}
# return(filtered_df)



# function_A <- function(i){
#     new_name <- paste0(df_name, "_1")
#     assign(new_name, get(df_name, envir = .GlobalEnv), envir = .GlobalEnv)
#   return(get(new_name))
# }

function_B <- function(i, first_load){
  if (first_load){
    filtered_df <- get(df_name, envir = .GlobalEnv) %>%
      dplyr::filter(.data[[key_var]] %in% valid_ids)
    assign(df_name, filtered_df, envir = .GlobalEnv)  
  } else {
    filtered_df <- get(paste0(df_name, "_1"), envir = .GlobalEnv) %>%
      dplyr::filter(.data[[key_var]] %in% valid_ids)
    assign(df_name, filtered_df, envir = .GlobalEnv)
  }
  return(get(df_name))
}

if (first_load){
  for (df_name in list_of_df_names){
    new_name <- paste0(df_name, "_1")
    assign(new_name, get(df_name, envir = .GlobalEnv), envir = .GlobalEnv)
    function_B(i = df_name, first_load = TRUE)
  }
  first_load <- FALSE
} else {
  for (df_name in list_of_df_names){
    function_B(i = df_name, first_load = FALSE)
  }
}
#









# then we need to say that filtered_df is now mtcars






data(mtcars)
data(iris)
mtcars$ID <- rep(c(1, 2), 16)
iris$ID <- rep(c(1, 2), 75)
key_var <- "ID"
valid_ids <- 1
first_load <- TRUE

list_of_df_names <- c("mtcars", "iris")

# then do this for every df_name
if (first_load){
  for (df_name in list_of_df_names){
    new_name <- paste0(df_name, "_1")
    assign(new_name, get(df_name, envir = .GlobalEnv), envir = .GlobalEnv)
    
    filtered_df <- get(df_name, envir = .GlobalEnv) %>%
      dplyr::filter(.data[[key_var]] %in% valid_ids)
    assign(df_name, filtered_df, envir = .GlobalEnv)
  }
  first_load <- FALSE
} else {
  print("A")
  for (df_name in list_of_df_names){
    
    filtered_df <- get(paste0(df_name, "_1"), envir = .GlobalEnv) %>%
      dplyr::filter(.data[[key_var]] %in% valid_ids)
    assign(df_name, filtered_df, envir = .GlobalEnv)
  }
}
