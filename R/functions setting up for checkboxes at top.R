# 
# # Testing it out
# 
# 
# 
# spreadsheet <- data.frame(
#   type = c("checkbox_group", "checkbox_group"),
#   name = c("A", "B"), # ID
#   value = c(1,2),
#   parameter_list = c('label = "hi"', 'label = "Site", choices = c("Mwanza" = "Mwanza", "Mwanza 2" = "Mwanza 2", "Shinyanga" = "Shinyanga", "Unknown" = "Unknown"), selected = c("Mwanza", "Mwanza 2", "Shinyanga", "Unknown")'),
#   variable = c(1,2),
#   variable_value = c(1,2)
# )
# 
# 
# checkbox_group_data <- (spreadsheet %>% filter(type == "checkbox_group"))
# if (nrow(checkbox_group_data) > 1){
#   checkbox_group_input(checkbox_group_data)
# }
# 
# bb <- checkbox_group_input(checkbox_group_data)
# identical(bb, aa)
# 
# 
# ########################################################################################
# 
# # TODO: for when we put it into PLH_shiny
# checkbox_group_data <- (spreadsheet %>% filter(type == "checkbox_group"))
# if (nrow(checkbox_group_data) > 1){
#   checkbox_group_input(inputId, checkbox_group_data)
# }
# 
# # and in the server:
# checkbox_group_data <- (spreadsheet %>% filter(type == "checkbox_group"))
# if (nrow(checkbox_group_data) > 1){
#   #...
# }
# 
# checkbox_group_filtered <- eventReactive(ifelse(input$goButton_group == 0, 1, input$goButton_group), {
#   variable <- checkbox_group_data$variable
#   name <- checkbox_group_data$name
#   filtered_data <- data %>%
#     dplyr::filter(variable %in% c((input[[paste0("checkbox", name)]])))  # Org = variable in the row,
#   return(filtered_data)
# })
# # then we use checkbox_group_filtered instead of data in the rest of the places where we
# # curretnly use data_frame in the PLH_shiny server
# # and update the data in display_box?
# 
# 
# 
# 
# 
# 
# #
# # checkbox_text_input <- function(inputId, spreadsheet){
# #   return(box(width = 12,
# #              checkboxInput(inputId = "select_cluster",
# #                            label = "All clusters",
# #                            value = TRUE),
# #              textInput(inputId = "opt_cluster",
# #                        label = "Cluster",
# #                        placeholder = "Enter values separated by a comma..."),
# #              actionButton(paste0("goButton", inputId), "Submit", class = "btn-success")))
# # }
# 
# 
# 
# # If Checkbox
# observe({
#   if(input$select_cluster){
#     shinyjs::disable("opt_cluster")
#   } else {
#     shinyjs::enable("opt_cluster")
# 
#   }
# })
# 
# selected_data_dem <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
#   if (country == "Tanzania"){
#     if (study %in% c("RCT", "WASH")) {
#       if(input$select_cluster){
#         opt_cluster_vals <- unique(UIC_Tracker_Use$ClusterName)
#       } else {
#         opt_cluster_vals <- input$opt_cluster
#       }
#       plhdata_checkgroup <- plhdata_org_clean %>%
#         dplyr::filter(ClusterName %in% c(opt_cluster_vals))
#       print(opt_cluster_vals)
#     } else {
#       plhdata_checkgroup <- plhdata_org_clean
#     }
#   } else {
#     plhdata_checkgroup <- plhdata_org_clean %>% dplyr::filter(Org %in% c((input$OrgDem)))
#   }
#   return(plhdata_checkgroup)
# })
# 
# opt_factors <- eventReactive(ifelse(input$goButton == 0, 1, input$goButton), {
#   if (country == "Tanzania"){
#   } if (study %in% c("RCT", "WASH")){
#     opt_factors <- c("ClusterName")
#   } else {
#     opt_factors <- c("Org")
#   }
# } else {
#   opt_factors <- c("Org")
# }
# return(opt_factors)
# })
