all_shiny_screw_options_long_df <- all_implants_constructed_df %>%
  select(level, vertebral_number, side, object) %>%
  filter(str_detect(object, "screw")) %>%
  filter(side == "left" | side == "right") %>%
  mutate(level_side_object = paste(side, level, object,sep = "_")) %>%
  mutate(level_side_object = str_to_lower(level_side_object)) %>%
  mutate(object_diameter_label = paste(level_side_object, "diameter", sep = "_")) %>%
  mutate(object_length_label = paste(level_side_object, "length", sep = "_")) %>%
  arrange(vertebral_number) %>%
  select(level, vertebral_number, object, side, level_side_object) %>%
  mutate(diameter_label = paste(level_side_object, "_diameter", sep = "")) %>%
  mutate(length_label = paste(level_side_object, "_length", sep = "")) %>%
  mutate(type_label = paste(level_side_object, "_type", sep = "")) %>%
  mutate(diameter_input = map(.x = diameter_label, .f = ~numericInput(inputId = .x, label = NULL, value = 0,
                                                                      width = '75%'))) %>%
  mutate(length_input = map(.x = length_label, .f = ~numericInput(inputId = .x, label = NULL, value = 0,
                                                                  width = '75%'))) %>%
  mutate(type_input = map(.x = type_label,
                          .f = ~ radioGroupButtons( #"option2",  
                            inputId = .x,
                            label = NULL, 
                            choices = c("M", "U", "P", "Red", "Offset"),
                            selected = "P",
                            checkIcon = list(yes = icon("wrench")),
                            size = "xs", direction = "horizontal",
                            justified = TRUE,
                            width = "95%"
                          )
  ))

left_input_options_df <- all_shiny_screw_options_long_df %>%
  filter(side == "left") %>%
  select(level, 
         left_diameter_label = diameter_label,
         left_diameter_input = diameter_input, 
         left_length_label = length_label,
         left_length_input = length_input,
         left_type_label = type_label,
         left_type_input = type_input) %>%
  mutate(count = row_number())


right_input_options_df <- all_shiny_screw_options_long_df %>%
  filter(side == "right") %>%
  select(level, 
         right_diameter_label = diameter_label, 
         right_diameter_input = diameter_input, 
         right_length_label = length_label,
         right_length_input = length_input,
         right_type_label = type_label,
         right_type_input = type_input)%>%
  mutate(count = row_number())



all_possible_screw_input_labels_wide_df <- left_input_options_df %>%
  left_join(right_input_options_df) %>%
  mutate(left_object = str_remove_all(left_diameter_label, "_diameter")) %>%
  mutate(right_object = str_remove_all(right_diameter_label, "_diameter")) %>%
  mutate(object = str_remove_all(right_object, "right_"))%>%
  mutate(object_label = str_to_title(str_replace_all(object, "_", " "))) %>%
  select(level, object, left_object, right_object, everything())%>%
  mutate(count = row_number())

## LEFT
left_input_diameter_list <- all_possible_screw_input_labels_wide_df$left_diameter_input
names(left_input_diameter_list) <- all_possible_screw_input_labels_wide_df$left_diameter_label

left_input_length_list <- all_possible_screw_input_labels_wide_df$left_length_input
names(left_input_length_list) <- all_possible_screw_input_labels_wide_df$left_length_label



### RIGHT
right_input_diameter_list <- all_possible_screw_input_labels_wide_df$right_diameter_input
names(right_input_diameter_list) <- all_possible_screw_input_labels_wide_df$right_diameter_label

right_input_length_list <- all_possible_screw_input_labels_wide_df$right_length_input
names(right_input_length_list) <- all_possible_screw_input_labels_wide_df$right_length_label


screw_size_input_tables_list <- pmap(.l = 
                         list(
                           ..1 = all_possible_screw_input_labels_wide_df$level, 
                           ..2 = all_possible_screw_input_labels_wide_df$object_label,
                           ..3 = left_input_diameter_list,
                           ..4 = left_input_length_list,
                           ..5 = right_input_diameter_list,
                           ..6 = right_input_length_list
                         ), 
                       .f = ~ tags$table(
                         tags$tr(width = "100%", 
                                 tags$td(width = "30%", 
                                         div(style = "font-size:12px; font-weight:bold; text-align:center; padding-bottom:10px", paste(..2))),
                                 tags$td(width = "15%",
                                         ..3
                                 ),
                                 tags$td(width = "15%", 
                                         ..4  
                                 ),
                                 tags$td(width = "15%",
                                         ..5
                                 ),
                                 tags$td(width = "15%",
                                         ..6
                                 )
                         )
                       )
)

names(screw_size_input_tables_list) <- all_possible_screw_input_labels_wide_df$object

rm(right_input_diameter_list, right_input_length_list, left_input_diameter_list, left_input_length_list)

left_input_type_list <- all_possible_screw_input_labels_wide_df$left_type_input
names(left_input_type_list) <- all_possible_screw_input_labels_wide_df$left_type_label

right_input_type_list <- all_possible_screw_input_labels_wide_df$right_type_input
names(right_input_type_list) <- all_possible_screw_input_labels_wide_df$right_type_label

screw_type_input_tables_list <- pmap(.l = 
                                       list(
                                         ..1 = all_possible_screw_input_labels_wide_df$level, 
                                         ..2 = all_possible_screw_input_labels_wide_df$object_label,
                                         ..3 = left_input_type_list,
                                         ..4 = right_input_type_list
                                       ), 
                                     .f = ~ tags$table(
                                       tags$tr(width = "100%", 
                                               tags$td(width = "7%", 
                                                       div(style = "font-size:14px; font-weight:bold; text-align:center; padding-bottom:10px",  
                                                                         ..2)),
                                               tags$td(width = "45%",
                                                       ..3
                                               ),
                                               tags$td(width = "45%",
                                                       ..4
                                               )
                                       )
                                     )
)

names(screw_type_input_tables_list) <- all_possible_screw_input_labels_wide_df$object

rm(left_input_type_list, right_input_type_list)


### this works:
# ui <- shinyUI(basicPage(
#   
#   column(width = 12,
#          pickerInput(inputId = "screw_options", 
#                      label = "Select Screws", 
#                      choices = names(table_row_list),
#                      multiple = TRUE), 
#          switchInput(inputId = "test", label = "select to show"),
#          br(), 
#          conditionalPanel(condition = "input.test == true",
#                           pickerInput(inputId = "screw_options_for_verification", selected = NULL, multiple = TRUE, 
#                                       choices = names(table_row_list))
#          ),
#          # uiOutput(outputId = "screw_size_input"),
#          hr(),
#          map(.x = c(1:length(screw_size_input_tables_list)),
#              .f = ~ conditionalPanel(
#                condition = glue("input.screw_options_for_verification.indexOf('{names(screw_size_input_tables_list[.x])}') > -1"),
#                screw_size_input_tables_list[[.x]]
#              )
#          )
#   )
#   
#   
#   
#   
# ))
# 
# server <- function(input, output, session) {
#   
#   observeEvent(input$screw_options, {
#     updatePickerInput(session = session, 
#                       inputId = "screw_options_for_verification", 
#                       selected = input$screw_options)
#   })
#   
#   output$screw_choices_ui <- renderUI({
#     
#     tags$table(
#       map(.x = input$screw_options, .f = ~ table_row_list[[.x]])
#       
#     )
#     
#   })
#   
# }
# 
# 
# shinyApp(ui = ui, server = server)