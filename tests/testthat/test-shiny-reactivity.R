test_that("build_shiny app works with shinytest2", {
  skip_on_cran()
  
  library(shinytest2)
  library(NHANES)
  library(rio)
  
  data(NHANES)
  NHANES$ID <- as.character(NHANES$ID)
  data_list <- rio::import_list(testthat::test_path("testdata/nhanes_data.xlsx"))
  
  app_obj <- ExcelToShiny::build_shiny(
    title = "Test Dashboard",
    data_list = data_list,
    data_frame = NHANES,
    key_var = "ID",
    deploy_shiny = FALSE
  )
  
  app <- AppDriver$new(
    app = shiny::shinyApp(ui = app_obj$ui, server = app_obj$server),
    load_timeout = 10000,
    shiny_args = list(launch.browser = FALSE)
  )
  app$wait_for_idle()
  
  # 👇 Adjust this to match visible elements in your app
  app$expect_ui_text(c("value_1", "value_2"))
  
  app$stop()
  

})
