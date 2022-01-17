

all_screw_size_type_inputs_df <-  all_implants_constructed_df %>%
  filter(vertebral_number < 23.9) %>%
  union_all(l6_all_implants_constructed_df) %>%
  select(level, vertebral_number, side, object) %>%
  filter(str_detect(object, "screw")) %>%
  filter(side == "left" | side == "right") %>%
  arrange(vertebral_number) %>%
  select(-vertebral_number) %>%
  group_by(level, side, object) %>%
  mutate(count = row_number()) %>%
  ungroup() %>%
  mutate(level_object = str_to_lower(paste(level, object, sep = "_"))) %>%
  mutate(level_object_label = str_to_title(paste(str_replace_all(level_object, "_", " ")))) %>%
  mutate(level_object_label = str_replace_all(level_object_label, "S2ai", "S2AI")) %>%
  mutate(level_object_label = str_replace_all(level_object_label, "Occiput Occ", "Occ")) %>%
  select(-side) %>%
  distinct() %>%
  mutate(left_object = paste("left", level_object, sep = "_"))%>%
  mutate(right_object = paste("right", level_object, sep = "_")) %>%
  mutate(implant_row_id = row_number()) %>%
  select(implant_row_id, everything()) %>%
  select(implant_row_id, level_object_label, left_object, right_object) %>%
  mutate(left_diameter_input = map(.x = left_object, 
                                   .f = ~numericInput(inputId = paste0(.x, "_diameter"),
                                                      label = NULL, 
                                                      value = "",
                                                      width = '85%'))) %>%
  mutate(left_length_input = map(.x = left_object, 
                                 .f = ~numericInput(inputId = paste0(.x, "_length"),
                                                    label = NULL, 
                                                    value = "",
                                                    width = '85%'))) %>%
  mutate(right_diameter_input = map(.x = right_object, 
                                    .f = ~numericInput(inputId = paste0(.x, "_diameter"),
                                                       label = NULL, 
                                                       value = "",
                                                       width = '85%'))) %>%
  mutate(right_length_input = map(.x = right_object, 
                                  .f = ~numericInput(inputId = paste0(.x, "_length"),
                                                     label = NULL, 
                                                     value = "",
                                                     width = '85%'))) %>%
  mutate(left_type_input = map(.x = left_object, 
                               .f = ~radioGroupButtons( #"option2",  
                                 inputId = paste0(.x, "_type"),
                                 label = NULL, 
                                 choices = c("M", "U", "P", "Red", "Offset"),
                                 selected = "P",
                                 checkIcon = list(yes = icon("wrench")),
                                 size = "xs", direction = "horizontal",
                                 justified = TRUE,
                                 width = "95%"
                               ))) %>%
  mutate(right_type_input = map(.x = right_object, 
                                .f = ~radioGroupButtons( #"option2",  
                                  inputId = paste0(.x, "_type"),
                                  label = NULL, 
                                  choices = c("M", "U", "P", "Red", "Offset"),
                                  selected = "P",
                                  checkIcon = list(yes = icon("wrench")),
                                  size = "xs", direction = "horizontal",
                                  justified = TRUE,
                                  width = "95%"
                                ))) 




jh_generate_df_for_screening_screw_inputs_function <- function(all_implants_df){
  
  level_object_df <- all_implants_df %>%  
    select(level, vertebral_number, side, object) %>%
    mutate(level_object_label = str_to_title(str_replace_all(paste(level, object), "_", " "))) %>%
    mutate(level_object_label = str_replace_all(level_object_label, "S2ai", "S2AI"))
  
  level_object_label_df <- level_object_df %>%
    select(level, level_object_label) %>%
    distinct()
  
  left_objects_df <- level_object_df %>%
    filter(side == "left") %>%
    mutate(left_object = str_to_lower(str_replace_all(paste(side, level_object_label, sep = "_"), " ", "_"))) %>%
    select(level_object_label, left_object)
  
  right_objects_df <- level_object_df %>%
    filter(side == "right") %>%
    mutate(right_object = str_to_lower(str_replace_all(paste(side, level_object_label, sep = "_"), " ", "_"))) %>%
    select(level_object_label, right_object)
  
  level_object_label_df %>%
    left_join(left_objects_df) %>%
    left_join(right_objects_df) %>%
    replace_na(list(left_object = "no_screw", right_object = "no_screw"))
  
  
}


