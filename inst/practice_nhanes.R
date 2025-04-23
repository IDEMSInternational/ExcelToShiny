setwd("C:/Users/lclem/OneDrive/Documents/GitHub/ExcelToShiny")
devtools::load_all()

# load data sets and excel spreadsheet in
example_excel <- rio::import_list("vignettes/data/nhanes_data.xlsx")

data(NHANES)
NHANES_by_ind <- NHANES %>%
  group_by(ID) %>%
  mutate(count = 1:n()) %>%
  filter(count == 1) %>%
  ungroup()

NHANES$ID <- as.character(NHANES$ID)
NHANES_by_ind$ID <- as.character(NHANES_by_ind$ID)

credentials_data <- data.frame(
  user = "admin",
  password = "password",
  stringsAsFactors = FALSE
)

# Your shiny app
build_shiny(
  title = "Test Dashboard",
  data_list = example_excel,
  data_frame = NHANES,
  key_var = "ID",
)

