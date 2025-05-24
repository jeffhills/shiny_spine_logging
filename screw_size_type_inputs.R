

# all_screw_size_type_inputs_df <-  all_implants_constructed_df %>%

jh_make_screw_details_inputs_df_function <- function(all_objects = tibble(), return_shiny_inputs_df = FALSE){
  
  if(nrow(all_objects)>0){
    if(any(str_detect(all_objects$object, "anterior_plate"))){
      
      no_screws_level_vector <- all_objects %>%
        filter(str_detect(object, "corpectomy")) %>%
        select(level) %>%
        distinct() %>%
        as_vector()
      # if(any(str_detect(all_objects$object, "corpectomy"))){
      # no_screws_level <- all_objects %>%
      #   filter(str_detect(object), "corpectomy") %>%
      #   select(level) %>%
      #   distinct() %>%
      #   as.vector()
      #   }else{
      #   
      # }
      
      anterior_plate_input_names_wide_df <- all_objects %>%
        filter(object == "anterior_plate") %>%
        select(level, vertebral_number, side, object) %>%
        mutate(side = "left", side2 = "right") %>%
        pivot_longer(cols = c(side, side2), names_to = "discard", values_to = "side") %>%
        select(-discard)  %>%
        separate(col = level, into = c("proximal", "distal"), sep = "-") %>%
        pivot_longer(cols = c(proximal, distal), names_to = "discard", values_to = "level") %>%
        select(level, side, object) %>%
        filter(level %in% no_screws_level_vector == FALSE) %>%
        distinct() %>%
        group_by(level, side, object) %>%
        mutate(count = row_number()) %>%
        ungroup() %>%
        mutate(level_object = str_to_lower(paste(level, object, "screw", sep = "_"))) %>%
        mutate(level_object_label = str_to_title(paste(str_replace_all(level_object, "_", " ")))) %>%
        select(-side) %>%
        distinct() %>%
        mutate(left_object = paste("left", level_object, sep = "_"))%>%
        mutate(right_object = paste("right", level_object, sep = "_")) %>%
        mutate(implant_row_id = row_number()) %>%
        select(implant_row_id, everything()) 
      
      anterior_plate_input_names_long_df <- anterior_plate_input_names_wide_df %>%
        select(level) %>%
        mutate(left = "anterior_plate_screw", right = "anterior_plate_screw") %>% 
        pivot_longer(cols = -level, names_to = "side", values_to = "object") %>%
        mutate(side_level_object = str_to_lower(paste0(side, "_", level, "_", object)),
               screw_diameter_input_name = str_to_lower(paste0(side, "_", level, "_", object, "_diameter")), 
               screw_length_input_name = str_to_lower(paste0(side, "_", level, "_", object, "_length")), 
               screw_type_input_name = str_to_lower(paste0(side, "_", level, "_", object, "_type"))
        ) %>%
        mutate(approach = "anterior") %>%
        select(level, side, approach, object, everything())
      
      if(return_shiny_inputs_df == TRUE){
        anterior_plate_inputs_df <- anterior_plate_input_names_wide_df %>%
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
                                         choices = c("Anterior", "Fixed Angle", "Variable Angle"),
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
                                          choices = c("Anterior", "Fixed Angle", "Variable Angle"),
                                          selected = "Anterior",
                                          checkIcon = list(yes = icon("wrench")),
                                          size = "xs", direction = "horizontal",
                                          justified = TRUE,
                                          width = "95%"
                                        ))) 
      }else{
        anterior_plate_inputs_df <- tibble(level = character())
      }
    }else{
      anterior_plate_input_names_long_df <- tibble(level = character(), 
                                                   side = character(), 
                                                   approach = character(),
                                                   object = character(),
                                                   side_level_object = character(),
                                                   screw_diameter_input_name = character(), 
                                                   screw_length_input_name = character(), 
                                                   screw_type_input_name = character())
      
      anterior_plate_inputs_df <- tibble(implant_row_id = integer(),
                                         level = character(),
                                         level_object_label = character(),
                                         left_object = character(),
                                         right_object = character(),
                                         left_diameter_input = list(),
                                         left_length_input = list(),
                                         right_diameter_input = list(),
                                         right_length_input = list(),
                                         left_type_input = list(),
                                         right_type_input = list())
      
    }
    
    if(nrow(
      all_objects %>%
      filter(approach == "posterior") %>%
      filter(str_detect(object, "screw"))
    )>0){
      step_1 <- all_objects %>%
        filter(approach == "posterior") %>%
        filter(str_detect(object, "screw")) %>%
        select(level, vertebral_number, side, object) %>%
        filter(side == "left" | side == "right") %>%
        arrange(vertebral_number) %>%
        select(-vertebral_number) %>%
        group_by(level, side, object) %>%
        mutate(count = row_number()) %>%
        ungroup()
      
      if(any(step_1$side == "left") == FALSE){
        step_1 <- step_1 %>%
          bind_rows(tibble(level = "x", side = "left", object = "x", count = 1))
      }
      if(any(step_1$side == "right") == FALSE){
        step_1 <- step_1 %>%
          bind_rows(tibble(level = "x", side = "right", object = "x", count = 1))
      }
      
      posterior_screw_input_names_wide_df <-  step_1 %>%
        mutate(level_object_label = str_to_title(paste(level, str_replace_all(object, "_", " ")))) %>%
        pivot_wider(names_from = side, values_from = object) %>%
        replace_na(list(left = "no_screw", right = c("no_screw"))) %>%
        mutate(implant_row_id = row_number()) %>%
        mutate(left_object = paste0("left_", str_to_lower(level), "_", left), 
               right_object = paste0("right_",str_to_lower(level), "_",  right))
      
      posterior_screw_input_names_long_df <- posterior_screw_input_names_wide_df %>%
        select(level, left, right) %>%
        pivot_longer(cols = c(left, right), names_to = "side", values_to = "object") %>%
        filter(object != "no_screw")  %>%
        mutate(side_level_object = str_to_lower(paste0(side, "_", level, "_", object)),
               screw_diameter_input_name = str_to_lower(paste0(side, "_", level, "_", object, "_diameter")), 
               screw_length_input_name = str_to_lower(paste0(side, "_", level, "_", object, "_length")), 
               screw_type_input_name = str_to_lower(paste0(side, "_", level, "_", object, "_type"))
        )%>%
        mutate(approach = "posterior") %>%
        select(level, side, approach, object, everything())
      
      if(return_shiny_inputs_df == TRUE){
        posterior_screw_size_type_inputs_df <- posterior_screw_input_names_wide_df %>%
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
                                         choices = c("M", "U", "P", "Red", "Offset", "Dual"),
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
                                          choices = c("M", "U", "P", "Red", "Offset", "Dual"),
                                          selected = "P",
                                          checkIcon = list(yes = icon("wrench")),
                                          size = "xs", direction = "horizontal",
                                          justified = TRUE,
                                          width = "95%"
                                        ))) 
      }else{
        posterior_screw_size_type_inputs_df <- tibble(level = character())
      }
    }else{
      posterior_screw_input_names_long_df <- tibble(level = character(), 
                                                    side = character(), 
                                                    object = character(),
                                                    side_level_object = character(),
                                                    screw_diameter_input_name = character(), 
                                                    screw_length_input_name = character(), 
                                                    screw_type_input_name = character())
      
      posterior_screw_size_type_inputs_df <- tibble(implant_row_id = integer(),
                                                    level = character(),
                                                    level_object_label = character(),
                                                    left_object = character(),
                                                    right_object = character(),
                                                    left_diameter_input = list(),
                                                    left_length_input = list(),
                                                    right_diameter_input = list(),
                                                    right_length_input = list(),
                                                    left_type_input = list(),
                                                    right_type_input = list())
    }
    
  }else{
    all_screw_inputs_df <- tibble(implant_row_id = integer(),
                                  level = character(),
                                  level_object_label = character(),
                                  left_object = character(),
                                  right_object = character(),
                                  left_diameter_input = list(),
                                  left_length_input = list(),
                                  right_diameter_input = list(),
                                  right_length_input = list(),
                                  left_type_input = list(),
                                  right_type_input = list())
  }
  
  if(return_shiny_inputs_df == TRUE){
    
    all_screw_inputs_df <- posterior_screw_size_type_inputs_df %>%
      bind_rows(anterior_plate_inputs_df)
    return(all_screw_inputs_df) 
  }else{
    all_screw_input_names_df <- posterior_screw_input_names_long_df %>%
      bind_rows(anterior_plate_input_names_long_df)
    return(all_screw_input_names_df)
  }
  
}

# 
# 
# 
# jh_generate_df_for_screening_screw_inputs_function <- function(all_implants_df){
# 
#   ## new for anterior plate screws
#   if(any(all_implants_df$object == "anterior_plate")){
#     anterior_plate_df <- all_implants_df %>%
#       filter(object == "anterior_plate") %>%
#       select(level, vertebral_number, side, object)%>%
#       separate(col = level, into = c("cranial_level", "caudal_level"), sep = "-") %>%
#       mutate(side_left = "left", side_right = "right") %>%
#       select(-side, vertebral_number) %>%
#       pivot_longer(cols = c(cranial_level, caudal_level), names_to = "cranial_caudal", values_to = "level") %>%
#       select(level, object, side_left, side_right) %>%
#       pivot_longer(cols = c(side_left, side_right), names_to = "remove", values_to = "side") %>%
#       distinct() %>%
#       mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
#       select(level, vertebral_number, side, object) %>%
#       mutate(object = "anterior_plate_screw")
#   }else{
#     anterior_plate_df <- tibble(level = character(), vertebral_number = double(), side = character(), object = character())
#   }
# 
# 
# 
#   level_object_df <- all_implants_df %>%
#     filter(object != "anterior_plate") %>%
#     select(level, vertebral_number, side, object) %>%
#     bind_rows(anterior_plate_df) %>% ### NEW
#     mutate(level_object_label = str_to_title(str_replace_all(paste(level, object), "_", " "))) %>%
#     mutate(level_object_label = str_replace_all(level_object_label, "S2ai", "S2AI"))
# 
#   level_objects_wide_df <- level_object_df %>%
#     mutate(objects = str_to_lower(paste(side, level, object, sep = "_"))) %>%
#     mutate(level_object_label = str_to_title(str_replace_all(paste(level, object), "_", " "))) %>%
#     select(level, side, objects, level_object_label) %>%
#     distinct() %>%
#     mutate(side = paste0(side, "_object")) %>%
#     pivot_wider(names_from = side, values_from = objects)
# 
#   level_objects_wide_df
# 
#   if(str_detect(string = toString(names(level_objects_wide_df)), pattern = "left") == FALSE){
#     level_objects_wide_df <- level_objects_wide_df %>%
#       mutate(left_object = "no_screw")
#   }
#   if(str_detect(string = toString(names(level_objects_wide_df)), pattern = "right") == FALSE){
#     level_objects_wide_df <- level_objects_wide_df %>%
#       mutate(right_object = "no_screw")
#   }
# 
#   level_objects_wide_df %>%
#     select(level, level_object_label, left_object, right_object) %>%
#     mutate(level_object_label = str_replace_all(level_object_label, "S2ai", "S2AI")) %>%
#     replace_na() %>%
#     replace_na(list(left_object = "no_screw", right_object = "no_screw"))
# 
# 
# }
# 

