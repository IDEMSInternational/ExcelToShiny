library(rio)
library(plhR)
library(readxl)
excel_template_shiny <- read_excel("~/GitHub/plhR/excel_template_shiny.xlsx",
                                   sheet = "demographics")
data_l <- import_list("~/GitHub/plhR/excel_template_shiny.xlsx")
names(data_l$demographics)[[1]] <- "type"

#all_data <- update_data(consent_only = FALSE)
#srh_df <- all_data[[1]]
#srh_df <- srh_df %>% mutate(consent = "Yes")
srh_df <- readRDS("srh_df.RDS")

PLH_shiny(title = "Testing Shiny Dashboard",
          data_list = data_l,
          data_frame = srh_df)