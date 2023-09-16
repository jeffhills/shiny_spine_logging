

# all_screw_size_type_inputs_df <-  all_implants_constructed_df %>%

anterior_plate_screws_df <- all_implants_constructed_df %>%
  filter(vertebral_number < 23.9) %>%
  # union_all(l6_all_implants_constructed_df) %>%
  bind_rows(l6_all_implants_constructed_df) %>%
  select(level, vertebral_number, side, object) %>%
  filter(str_detect(object, "screw")) %>%
  filter(side == "left" | side == "right") %>%
  arrange(vertebral_number) %>%
  select(-vertebral_number) %>%
  filter(object == "pedicle_screw") %>%
  mutate(object = str_replace_all(object, "pedicle", "anterior_plate"))

all_screw_size_type_inputs_df <- all_implants_constructed_df %>%
  filter(vertebral_number < 23.9) %>%
  bind_rows(l6_all_implants_constructed_df) %>%
  select(level, vertebral_number, side, object) %>%
  filter(str_detect(object, "screw")) %>%
  filter(side == "left" | side == "right") %>%
  arrange(vertebral_number) %>%
  select(-vertebral_number) %>%
  group_by(level, side, object) %>%
  bind_rows(anterior_plate_screws_df) %>% ### added this NEW for ANTERIOR PLATE SCREWS
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
  select(implant_row_id, level, level_object_label, left_object, right_object) %>%
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
                                .f = ~ radioGroupButtons( #"option2",
                                  inputId = paste0(.x, "_type"),
                                  label = NULL,
                                  choices = c("M", "U", "P", "Red", "Offset"),
                                  selected = "P",
                                  checkIcon = list(yes = icon("wrench")),
                                  size = "xs", direction = "horizontal",
                                  justified = TRUE,
                                  width = "95%"
                                )))


  anterior_plate_screws_all_df <- all_screw_size_type_inputs_df %>%
    filter(str_detect(string = left_object, pattern = "anterior_plate")) %>%
    mutate(left_type_input = map(.x = left_object,
                                 .f = ~radioGroupButtons( #"option2",
                                   inputId = paste0(.x, "_type"),
                                   label = NULL,
                                   choices = c("Anterior"),
                                   selected = "Anterior",
                                   checkIcon = list(yes = icon("wrench")),
                                   size = "xs", direction = "horizontal",
                                   justified = TRUE,
                                   width = "95%"
                                 ))) %>%
    mutate(right_type_input = map(.x = right_object,
                                  .f = ~radioGroupButtons( #"option2",
                                    inputId = paste0(.x, "_type"),
                                    label = NULL,
                                    choices = c("Anterior"),
                                    selected = "Anterior",
                                    checkIcon = list(yes = icon("wrench")),
                                    size = "xs", direction = "horizontal",
                                    justified = TRUE,
                                    width = "95%"
                                  )))
  
  all_screw_size_type_inputs_df <- all_screw_size_type_inputs_df %>%
    filter(str_detect(string = left_object, pattern = "anterior_plate", negate = TRUE)) %>%
    bind_rows(anterior_plate_screws_all_df) 
    # separate(col = level_object_label, into = c("level"), remove = FALSE)



jh_generate_df_for_screening_screw_inputs_function <- function(all_implants_df){
  
  ## new for anterior plate screws
  if(any(all_implants_df$object == "anterior_plate")){
    anterior_plate_df <- all_implants_df %>%
      filter(object == "anterior_plate") %>%
      select(level, vertebral_number, side, object)%>%
      separate(col = level, into = c("cranial_level", "caudal_level"), sep = "-") %>%
      mutate(side_left = "left", side_right = "right") %>%
      select(-side, vertebral_number) %>%
      pivot_longer(cols = c(cranial_level, caudal_level), names_to = "cranial_caudal", values_to = "level") %>%
      select(level, object, side_left, side_right) %>%
      pivot_longer(cols = c(side_left, side_right), names_to = "remove", values_to = "side") %>%
      distinct() %>%
      mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
      select(level, vertebral_number, side, object) %>%
      mutate(object = "anterior_plate_screw")
  }else{
    anterior_plate_df <- tibble(level = character(), vertebral_number = double(), side = character(), object = character())
  }
  
  
  # level_object_df <- all_implants_df %>%
  #   filter(object != "anterior_plate") %>%
  #   select(level, vertebral_number, side, object) %>%
  #   union_all(anterior_plate_df) %>% ### NEW
  #   mutate(level_object_label = str_to_title(str_replace_all(paste(level, object), "_", " "))) %>%
  #   mutate(level_object_label = str_replace_all(level_object_label, "S2ai", "S2AI"))
  # 
  # level_object_label_df <- level_object_df %>%
  #   select(level, level_object_label) %>%
  #   distinct()
  # 
  # left_objects_df <- level_object_df %>%
  #   filter(side == "left") %>%
  #   mutate(left_object = str_to_lower(str_replace_all(paste(side, level_object_label, sep = "_"), " ", "_"))) %>%
  #   select(level_object_label, left_object)
  # 
  # right_objects_df <- level_object_df %>%
  #   filter(side == "right") %>%
  #   mutate(right_object = str_to_lower(str_replace_all(paste(side, level_object_label, sep = "_"), " ", "_"))) %>%
  #   select(level_object_label, right_object)
  # 
  # level_object_label_df %>%
  #   left_join(left_objects_df) %>%
  #   left_join(right_objects_df) %>%
  #   replace_na(list(left_object = "no_screw", right_object = "no_screw"))
  
  level_object_df <- all_implants_df %>%
    filter(object != "anterior_plate") %>%
    select(level, vertebral_number, side, object) %>%
    bind_rows(anterior_plate_df) %>% ### NEW
    mutate(level_object_label = str_to_title(str_replace_all(paste(level, object), "_", " "))) %>%
    mutate(level_object_label = str_replace_all(level_object_label, "S2ai", "S2AI"))
  
  level_objects_wide_df <- level_object_df %>%
    mutate(objects = str_to_lower(paste(side, level, object, sep = "_"))) %>%
    mutate(level_object_label = str_to_title(str_replace_all(paste(level, object), "_", " "))) %>%
    select(level, side, objects, level_object_label) %>%
    distinct() %>%
    mutate(side = paste0(side, "_object")) %>%
    pivot_wider(names_from = side, values_from = objects) 
  
  level_objects_wide_df
  
  if(str_detect(string = toString(names(level_objects_wide_df)), pattern = "left") == FALSE){
    level_objects_wide_df <- level_objects_wide_df %>%
      mutate(left_object = "no_screw")
  }
  if(str_detect(string = toString(names(level_objects_wide_df)), pattern = "right") == FALSE){
    level_objects_wide_df <- level_objects_wide_df %>%
      mutate(right_object = "no_screw")
  }
  
  level_objects_wide_df %>%
    select(level, level_object_label, left_object, right_object) %>%
    mutate(level_object_label = str_replace_all(level_object_label, "S2ai", "S2AI")) %>%
    replace_na() %>%
    replace_na(list(left_object = "no_screw", right_object = "no_screw"))
  
  
}


