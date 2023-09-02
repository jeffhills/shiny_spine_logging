jh_make_op_note_test_df_function <- function(posterior_or_anterior = "posterior", object = c("pre_selected"), spine_region = "lumbar"){
  
  surgical_objects <- as_vector(object)
  
  if(spine_region == "lumbar"){
    object_levels <- c("L4", "L5", "S1", "Iliac", "S2AI", "L3-L4", "L4-L5")
    object_list <- c("pedicle_screw", "pelvic_screw_1", "sublaminar_decompression", "anterior_interbody_implant", "diskectomy_fusion", "anterior_plate")
  }else{
    object_levels <- c("C2", "T1", "T2", "C3", "C4", "C5", "C6", "C3-C4", "C4-C5", "C5-C6")
    object_list <- c("pars_screw", "pedicle_screw", "lateral_mass_screw", "laminectomy", "anterior_interbody_implant", "decompression_diskectomy_fusion", "anterior_plate")
  }
  
  if(surgical_objects[[1]] == "pre_selected"){
    
    testing_df <- all_implants_constructed_df %>%
      filter(object %in% object_list) %>%
      filter(approach == posterior_or_anterior) %>%
      group_by(object) %>%
      filter(level %in% object_levels) %>%
      ungroup()
  }else{
    testing_df <- all_implants_constructed_df %>%
      filter(object %in% surgical_objects) %>%
      group_by(object) %>%
      filter(level %in% object_levels) %>%
      ungroup()
  }
  
  testing_df
  
  testing_df <- testing_df %>%
    mutate(screw_size_type = "", implant_statement = "") %>%
    select(level, vertebral_number, body_interspace, approach, category, implant, object, side, screw_size_type, implant_statement)
  
  return(testing_df)
}


levels_vector <- c('Occiput', 'O-C1', 'C1', 'C1-C2', 'C2', 'C2-C3', 'C3', 'C3-C4', 'C4', 'C4-C5', 'C5', 'C5-C6', 'C6', 'C6-C7', 'C7', 'C7-T1', 'T1', 'T1-T2', 'T2', 'T2-T3', 'T3', 'T3-T4', 'T4', 'T4-T5', 'T5', 'T5-T6', 'T6', 'T6-T7', 'T7', 'T7-T8', 'T8', 'T8-T9', 'T9', 'T9-T10', 'T10', 'T10-T11', 'T11', 'T11-T12', 'T12', 'T12-L1', 'L1', 'L1-L2', 'L2', 'L2-L3', 'L3', 'L3-L4', 'L4', 'L4-L5', 'L5', 'L5-S1', 'S1', 'Sacro-iliac', 'Iliac', 'S2AI')

vertebral_numbers_vector <- c(0,0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5, 11, 11.5, 12, 12.5, 13, 13.5, 14, 14.5, 15, 15.5, 16, 16.5, 17, 17.5, 18, 18.5, 19, 19.5, 20, 20.5, 21, 21.5, 22, 22.5, 23, 23.5, 24, 24.5, 25, 25.5, 26, 27)

levels_numbered_df <- tibble(level = levels_vector, vertebral_number = vertebral_numbers_vector)


vertebral_bodies_vector <- c('C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'T1', 'T2', 'T3', 'T4', 'T5', 'T6', 'T7', 'T8', 'T9', 'T10', 'T11', 'T12', 'L1', 'L2', 'L3', 'L4', 'L5', 'L6', 'S1')

interspaces_vector <- c('O-C1', 'C1-C2', 'C2-C3', 'C3-C4', 'C4-C5', 'C5-C6', 'C6-C7', 'C7-T1', 'T1-T2', 'T2-T3', 'T3-T4', 'T4-T5', 'T5-T6', 'T6-T7', 'T7-T8', 'T8-T9', 'T9-T10', 'T10-T11', 'T11-T12', 'T12-L1', 'L1-L2', 'L2-L3', 'L3-L4', 'L4-L5', 'L5-S1', 'L4-S1', 'L5-L6', 'L6-S1', 'Sacro-iliac')

jh_reorder_levels_function <- function(level_vector){
  levels <- tibble(level = as.character(level_vector)) %>%
    mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
    arrange(vertebral_number) %>%
    select(level) %>%
    as_vector() 
  return(levels)
} 

# ## for screw sizes
# screw_levels_vector <- c('Occiput', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'T1', 'T2', 'T3', 'T4', 'T5', 'T6', 'T7', 'T8', 'T9', 'T10', 'T11', 'T12', 'L1', 'L2', 'L3', 'L4', 'L5', 'L6', 'S1', 'Iliac', 'S2AI')
# 
# screw_input_labels_df <- tibble(level = str_to_lower(screw_levels_vector), diameter_text = "_screw_diameter", length_text = "_screw_length") %>%
#   mutate(screw_diameter_label = str_to_lower(paste0(level, diameter_text))) %>%
#   mutate(screw_length_label = str_to_lower(paste0(level, length_text))) %>%
#   select(level, screw_diameter_label, screw_length_label)
screw_label_vector <- c('occiput_left_occipital_screw', 'occiput_right_occipital_screw', 'c1_left_lateral_mass_screw', 'c1_right_lateral_mass_screw', 'c1_left_translaminar_screw', 'c1_right_translaminar_screw', 'c2_left_pars_screw', 'c2_right_pars_screw', 'c2_left_pedicle_screw', 'c2_right_pedicle_screw', 'c2_left_transarticular_screw', 'c2_right_transarticular_screw', 'c2_left_translaminar_screw', 'c2_right_translaminar_screw', 'c2_central_screw_washer', 'c3_left_lateral_mass_screw', 'c3_right_lateral_mass_screw', 'c3_left_pedicle_screw', 'c3_right_pedicle_screw', 'c3_left_translaminar_screw', 'c3_right_translaminar_screw', 'c3_central_screw_washer', 'c4_left_lateral_mass_screw', 'c4_right_lateral_mass_screw', 'c4_left_pedicle_screw', 'c4_right_pedicle_screw', 'c4_left_translaminar_screw', 'c4_right_translaminar_screw', 'c4_central_screw_washer', 'c5_left_lateral_mass_screw', 'c5_right_lateral_mass_screw', 'c5_left_pedicle_screw', 'c5_right_pedicle_screw', 'c5_left_translaminar_screw', 'c5_right_translaminar_screw', 'c5_central_screw_washer', 'c6_left_lateral_mass_screw', 'c6_right_lateral_mass_screw', 'c6_left_pedicle_screw', 'c6_right_pedicle_screw', 'c6_left_translaminar_screw', 'c6_right_translaminar_screw', 'c6_central_screw_washer', 'c7_left_lateral_mass_screw', 'c7_right_lateral_mass_screw', 'c7_left_pedicle_screw', 'c7_right_pedicle_screw', 'c7_left_translaminar_screw', 'c7_right_translaminar_screw', 'c7_central_screw_washer', 't1_left_pedicle_screw', 't1_right_pedicle_screw', 't1_left_translaminar_screw', 't1_right_translaminar_screw', 't1_central_screw_washer', 't2_left_pedicle_screw', 't2_right_pedicle_screw', 't2_left_translaminar_screw', 't2_right_translaminar_screw', 't2_central_screw_washer', 't3_left_pedicle_screw', 't3_right_pedicle_screw', 't3_left_translaminar_screw', 't3_right_translaminar_screw', 't3_central_screw_washer', 't4_left_pedicle_screw', 't4_right_pedicle_screw', 't4_left_translaminar_screw', 't4_right_translaminar_screw', 't4_central_screw_washer', 't5_left_pedicle_screw', 't5_right_pedicle_screw', 't5_left_translaminar_screw', 't5_right_translaminar_screw', 't5_central_screw_washer', 't6_left_pedicle_screw', 't6_right_pedicle_screw', 't6_left_translaminar_screw', 't6_right_translaminar_screw', 't6_central_screw_washer', 't7_left_pedicle_screw', 't7_right_pedicle_screw', 't7_left_translaminar_screw', 't7_right_translaminar_screw', 't7_central_screw_washer', 't8_left_pedicle_screw', 't8_right_pedicle_screw', 't8_left_translaminar_screw', 't8_right_translaminar_screw', 't8_central_screw_washer', 't9_left_pedicle_screw', 't9_right_pedicle_screw', 't9_left_translaminar_screw', 't9_right_translaminar_screw', 't9_central_screw_washer', 't10_left_pedicle_screw', 't10_right_pedicle_screw', 't10_left_translaminar_screw', 't10_right_translaminar_screw', 't10_central_screw_washer', 't11_left_pedicle_screw', 't11_right_pedicle_screw', 't11_left_translaminar_screw', 't11_right_translaminar_screw', 't11_central_screw_washer', 't12_left_pedicle_screw', 't12_right_pedicle_screw', 't12_left_translaminar_screw', 't12_right_translaminar_screw', 't12_central_screw_washer', 'l1_left_pedicle_screw', 'l1_right_pedicle_screw', 'l1_left_translaminar_screw', 'l1_right_translaminar_screw', 'l1_central_screw_washer', 'l2_left_pedicle_screw', 'l2_right_pedicle_screw', 'l2_left_translaminar_screw', 'l2_right_translaminar_screw', 'l2_central_screw_washer', 'l3_left_pedicle_screw', 'l3_right_pedicle_screw', 'l3_left_translaminar_screw', 'l3_right_translaminar_screw', 'l3_central_screw_washer', 'l4_left_pedicle_screw', 'l4_right_pedicle_screw', 'l4_left_translaminar_screw', 'l4_right_translaminar_screw', 'l4_central_screw_washer', 'l5_left_pedicle_screw', 'l5_right_pedicle_screw', 'l5_left_translaminar_screw', 'l5_right_translaminar_screw', 'l5_central_screw_washer', 's1_left_pedicle_screw', 's1_right_pedicle_screw', 's1_central_screw_washer', 'iliac_left_pelvic_screw', 'iliac_right_pelvic_screw', 's2ai_left_pelvic_screw', 's2ai_right_pelvic_screw')

screw_label_level_vector <- c('Occiput', 'Occiput', 'C1', 'C1', 'C1', 'C1', 'C2', 'C2', 'C2', 'C2', 'C2', 'C2', 'C2', 'C2', 'C2', 'C3', 'C3', 'C3', 'C3', 'C3', 'C3', 'C3', 'C4', 'C4', 'C4', 'C4', 'C4', 'C4', 'C4', 'C5', 'C5', 'C5', 'C5', 'C5', 'C5', 'C5', 'C6', 'C6', 'C6', 'C6', 'C6', 'C6', 'C6', 'C7', 'C7', 'C7', 'C7', 'C7', 'C7', 'C7', 'T1', 'T1', 'T1', 'T1', 'T1', 'T2', 'T2', 'T2', 'T2', 'T2', 'T3', 'T3', 'T3', 'T3', 'T3', 'T4', 'T4', 'T4', 'T4', 'T4', 'T5', 'T5', 'T5', 'T5', 'T5', 'T6', 'T6', 'T6', 'T6', 'T6', 'T7', 'T7', 'T7', 'T7', 'T7', 'T8', 'T8', 'T8', 'T8', 'T8', 'T9', 'T9', 'T9', 'T9', 'T9', 'T10', 'T10', 'T10', 'T10', 'T10', 'T11', 'T11', 'T11', 'T11', 'T11', 'T12', 'T12', 'T12', 'T12', 'T12', 'L1', 'L1', 'L1', 'L1', 'L1', 'L2', 'L2', 'L2', 'L2', 'L2', 'L3', 'L3', 'L3', 'L3', 'L3', 'L4', 'L4', 'L4', 'L4', 'L4', 'L5', 'L5', 'L5', 'L5', 'L5', 'S1', 'S1', 'S1', 'Iliac', 'Iliac', 'S2AI', 'S2AI')


screw_label_object <- c('occipital_screw', 'occipital_screw', 'lateral_mass_screw', 'lateral_mass_screw', 'translaminar_screw', 'translaminar_screw', 'pars_screw', 'pars_screw', 'pedicle_screw', 'pedicle_screw', 'transarticular_screw', 'transarticular_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'lateral_mass_screw', 'lateral_mass_screw', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'lateral_mass_screw', 'lateral_mass_screw', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'lateral_mass_screw', 'lateral_mass_screw', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'lateral_mass_screw', 'lateral_mass_screw', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'lateral_mass_screw', 'lateral_mass_screw', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'screw_washer', 'pelvic_screw', 'pelvic_screw', 'pelvic_screw', 'pelvic_screw')

# screw_size_labels_df <- tibble(level = screw_label_level_vector, screw_label = screw_label_vector, object = screw_label_object) %>%
#   filter(str_detect(screw_label, "central", negate = TRUE)) %>%
#   mutate(side = if_else(str_detect(screw_label, "left"), "left", "right")) %>%
#   mutate(screw_diameter_label = paste(screw_label, "diameter", sep = "_")) %>%
#   mutate(screw_length_label = paste(screw_label, "length", sep = "_"))  %>%
#   mutate(value = 0)

# screw_label_level_long_df <- tibble(level = screw_label_level_vector, screw_label = screw_label_vector) %>%
#   filter(str_detect(screw_label, "central", negate = TRUE)) %>%
#   mutate(side = if_else(str_detect(screw_label, "left"), "left", "right")) %>%
#   mutate(screw_diameter_label = paste(screw_label, "diameter", sep = "_")) %>%
#   mutate(screw_length_label = paste(screw_label, "length", sep = "_")) 


screw_size_labels_df <- tibble(level = screw_label_level_vector, screw_label = screw_label_vector) %>%
  mutate(screw_diameter_label = paste(screw_label, "diameter", sep = "_")) %>%
  mutate(screw_length_label = paste(screw_label, "length", sep = "_"))


#################################
jh_convert_body_levels_to_interspace_vector_function <- function(vertebral_bodies_vector){
  levels_vector <- unique(
    append(
      map(.x = vertebral_bodies_vector, 
          .f = ~ jh_get_cranial_caudal_interspace_body_list_function(level = .x)$cranial_interspace), 
      map(.x = vertebral_bodies_vector, 
          .f = ~ jh_get_cranial_caudal_interspace_body_list_function(level = .x)$caudal_interspace)))
  discard(levels_vector, .p = ~ is.na(.x) | .x == "Sacro-iliac")
  
  return(jh_reorder_levels_function(level_vector = discard(levels_vector, .p = ~ is.na(.x) | .x == "Sacro-iliac")))
}

jh_convert_interspace_to_body_vector_function <- function(interspaces_vector){
  body_df <- tibble(interspaces = interspaces_vector) %>%
    separate(col = interspaces, into = c("proximal", "distal")) %>%
    pivot_longer(cols = c(proximal, distal)) %>%
    select(value) %>%
    distinct()
  
  return(body_df$value)
  
}


#################################
jh_check_body_or_interspace_function <- function(level){
  result <- case_when(level %in% vertebral_bodies_vector ~ "body",
                      level %in% interspaces_vector ~ "interspace")
  
  result
}


#################################
jh_get_exiting_nerve_root_function <- function(interspace){
  if(interspace == "xx"){
    exiting_root <- "none"
  }else{
    if(str_detect(string = interspace, pattern = "-")){
      exiting_root <- case_when(
        interspace == 'O-C1' ~ "C1",
        interspace ==  'C1-C2' ~ 'C2', 
        interspace == 'C2-C3' ~ 'C3', 
        interspace == 'C3-C4' ~ 'C4', 
        interspace == 'C4-C5' ~ 'C5', 
        interspace == 'C5-C6' ~ 'C6', 
        interspace == 'C6-C7' ~ 'C7', 
        interspace == 'C7-T1' ~ 'C8', 
        interspace == 'T1-T2' ~ 'T1', 
        interspace == 'T2-T3' ~ 'T2', 
        interspace == 'T3-T4' ~ 'T3', 
        interspace == 'T4-T5' ~ 'T4', 
        interspace == 'T5-T6' ~ 'T5', 
        interspace == 'T6-T7' ~ 'T6', 
        interspace == 'T7-T8' ~ 'T7', 
        interspace == 'T8-T9' ~ 'T8', 
        interspace == 'T9-T10' ~ 'T9', 
        interspace == 'T10-T11' ~ 'T10', 
        interspace == 'T11-T12' ~ 'T11', 
        interspace == 'T12-L1' ~ 'T12', 
        interspace == 'L1-L2' ~ 'L1', 
        interspace == 'L2-L3' ~ 'L2', 
        interspace == 'L3-L4' ~ 'L3', 
        interspace == 'L4-L5' ~ 'L4', 
        interspace == 'L5-S1' ~ 'L5'
      )
    }else{
      exiting_root <- case_when(
        str_starts(string = interspace, pattern = "O") ~ "C1",
        interspace == "C1" ~ "C2",
        interspace == "C2" ~ "C3",
        interspace == "C3" ~ "C4",
        interspace == "C4" ~ "C5",
        interspace == "C5" ~ "C6",
        interspace == "C6" ~ "C7",
        interspace == "C7" ~ "C8",
        str_starts(string = interspace, pattern = "T") ~ interspace,
        str_starts(string = interspace, pattern = "L") ~ interspace,
        str_starts(string = interspace, pattern = "S1") ~ interspace,
        interspace == "S2AI" ~ "none",
        interspace == "Iliac" ~ "none"
      )
    } 
  }
  return(exiting_root)
}

#################################
jh_get_vertebral_number_function <- function(level_to_get_number){
  vert_number <-
    case_when(
      level_to_get_number == 'Occiput' ~ 0,
      level_to_get_number == 'O-C1' ~ 0.5,
      level_to_get_number == 'C1' ~ 1,
      level_to_get_number == 'C1-C2' ~ 1.5,
      level_to_get_number == 'C2' ~ 2,
      level_to_get_number == 'C2-C3' ~ 2.5,
      level_to_get_number == 'C3' ~ 3,
      level_to_get_number == 'C3-C4' ~ 3.5,
      level_to_get_number == 'C4' ~ 4,
      level_to_get_number == 'C4-C5' ~ 4.5,
      level_to_get_number == 'C5' ~ 5,
      level_to_get_number == 'C5-C6' ~ 5.5,
      level_to_get_number == 'C6' ~ 6,
      level_to_get_number == 'C6-C7' ~ 6.5,
      level_to_get_number == 'C7' ~ 7,
      level_to_get_number == 'C7-T1' ~ 7.5,
      level_to_get_number == 'T1' ~ 8,
      level_to_get_number == 'T1-T2' ~ 8.5,
      level_to_get_number == 'T2' ~ 9,
      level_to_get_number == 'T2-T3' ~ 9.5,
      level_to_get_number == 'T3' ~ 10,
      level_to_get_number == 'T3-T4' ~ 10.5,
      level_to_get_number == 'T4' ~ 11,
      level_to_get_number == 'T4-T5' ~ 11.5,
      level_to_get_number == 'T5' ~ 12,
      level_to_get_number == 'T5-T6' ~ 12.5,
      level_to_get_number == 'T6' ~ 13,
      level_to_get_number == 'T6-T7' ~ 13.5,
      level_to_get_number == 'T7' ~ 14,
      level_to_get_number == 'T7-T8' ~ 14.5,
      level_to_get_number == 'T8' ~ 15,
      level_to_get_number == 'T8-T9' ~ 15.5,
      level_to_get_number == 'T9' ~ 16,
      level_to_get_number == 'T9-T10' ~ 16.5,
      level_to_get_number == 'T10' ~ 17,
      level_to_get_number == 'T10-T11' ~ 17.5,
      level_to_get_number == 'T11' ~ 18,
      level_to_get_number == 'T11-T12' ~ 18.5,
      level_to_get_number == 'T12' ~ 19,
      level_to_get_number == 'T12-L1' ~ 19.5,
      level_to_get_number == 'L1' ~ 20,
      level_to_get_number == 'L1-L2' ~ 20.5,
      level_to_get_number == 'L2' ~ 21,
      level_to_get_number == 'L2-L3' ~ 21.5,
      level_to_get_number == 'L3' ~ 22,
      level_to_get_number == 'L3-L4' ~ 22.5,
      level_to_get_number == 'L4' ~ 23,
      level_to_get_number == 'L4-L5' ~ 23.5,
      level_to_get_number == 'L5' ~ 24,
      level_to_get_number == 'L5-S1' ~ 24.5,
      level_to_get_number == 'S1' ~ 25,
      level_to_get_number == "Sacro-iliac" ~ 25.5,
      level_to_get_number == 'Iliac' ~ 26,
      level_to_get_number == 'Sacro-iliac' ~ 26.5,
      level_to_get_number == 'S2AI' ~ 27
    )
  return(vert_number)
}
#################################
jh_get_vertebral_level_function <- function(number) {
  level = case_when(
    number == 0 ~ 'Occiput',
    number == 0.5 ~ 'O-C1',
    number == 1 ~ 'C1',
    number == 1.5 ~ 'C1-C2',
    number == 2 ~ 'C2',
    number == 2.5 ~ 'C2-C3',
    number == 3 ~ 'C3',
    number == 3.5 ~ 'C3-C4',
    number == 4 ~ 'C4',
    number == 4.5 ~ 'C4-C5',
    number == 5 ~ 'C5',
    number == 5.5 ~ 'C5-C6',
    number == 6 ~ 'C6',
    number == 6.5 ~ 'C6-C7',
    number == 7 ~ 'C7',
    number == 7.5 ~ 'C7-T1',
    number == 8 ~ 'T1',
    number == 8.5 ~ 'T1-T2',
    number == 9 ~ 'T2',
    number == 9.5 ~ 'T2-T3',
    number == 10 ~ 'T3',
    number == 10.5 ~ 'T3-T4',
    number == 11 ~ 'T4',
    number == 11.5 ~ 'T4-T5',
    number == 12 ~ 'T5',
    number == 12.5 ~ 'T5-T6',
    number == 13 ~ 'T6',
    number == 13.5 ~ 'T6-T7',
    number == 14 ~ 'T7',
    number == 14.5 ~ 'T7-T8',
    number == 15 ~ 'T8',
    number == 15.5 ~ 'T8-T9',
    number == 16 ~ 'T9',
    number == 16.5 ~ 'T9-T10',
    number == 17 ~ 'T10',
    number == 17.5 ~ 'T10-T11',
    number == 18 ~ 'T11',
    number == 18.5 ~ 'T11-T12',
    number == 19 ~ 'T12',
    number == 19.5 ~ 'T12-L1',
    number == 20 ~ 'L1',
    number == 20.5 ~ 'L1-L2',
    number == 21 ~ 'L2',
    number == 21.5 ~ 'L2-L3',
    number == 22 ~ 'L3',
    number == 22.5 ~ 'L3-L4',
    number == 23 ~ 'L4',
    number == 23.5 ~ 'L4-L5',
    number == 24 ~ 'L5',
    number == 24.5 ~ 'L5-S1',
    number == 25 ~ 'S1',
    number == 25.5 ~ "Sacro-iliac",
    number == 26 ~ 'Iliac',
    number == 26.5 ~ 'Sacro-iliac',
    number == 27 ~ 'S2AI'
  )
  return(level)
}

###################################

jh_get_level_range_vector_function <- function(object_df, interspace_or_body_or_all = "body"){
  range_df <- object_df %>%
    filter(vertebral_number == min(vertebral_number) | vertebral_number == max(vertebral_number)) %>%
    arrange(vertebral_number) %>%
    select(level, vertebral_number) %>%
    mutate(level = if_else(level == "S2AI", "S1", level)) %>%
    mutate(level = if_else(str_detect(string = level, pattern = "Iliac"), "S1", level)) %>%
    mutate(level = if_else(level == "Occiput", "C1", level)) %>%
    select(level) %>%
    mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level))
  
  cranial_level_type <- jh_check_body_or_interspace_function(level = head(range_df$level, 1)) 
  caudal_level_type <- jh_check_body_or_interspace_function(level = tail(range_df$level, 1)) 
  
  if(cranial_level_type == "body"){
    cranial_body_number <-  head(range_df$vertebral_number, 1)
    cranial_interspace_number <-  head(range_df$vertebral_number, 1) - 0.5
  }else{
    cranial_body_number <-  head(range_df$vertebral_number, 1) - 0.5
    cranial_interspace_number <-  head(range_df$vertebral_number, 1)
  }
  
  if(caudal_level_type == "body"){
    caudal_body_number <-  tail(range_df$vertebral_number, 1)
    caudal_interspace_number <-  tail(range_df$vertebral_number, 1) + 0.5
  }else{
    caudal_body_number <-  tail(range_df$vertebral_number, 1) + 0.5
    caudal_interspace_number <-  tail(range_df$vertebral_number, 1)
  }
  
  if(interspace_or_body_or_all == "body"){
    range_number_vector <- seq(cranial_body_number, caudal_body_number, by = 1)
  }
  
  if(interspace_or_body_or_all == "interspace"){
    range_number_vector <- seq(cranial_interspace_number, caudal_interspace_number, by = 1)
  }
  
  if(interspace_or_body_or_all == "all"){
    range_number_vector <- seq(cranial_interspace_number, caudal_interspace_number, by = 0.5)
  }
  levels_range_vector <- jh_get_vertebral_level_function(number = range_number_vector)
  
  return(levels_range_vector)
}

#################################

jh_get_cranial_caudal_interspace_body_list_function <- function(level){
  
  if(str_detect(level, "-")){
    interspace_number <- jh_get_vertebral_number_function(level)
    cranial_interspace_number <- interspace_number - 1
    caudal_interspace_number <- interspace_number + 1
    
    cranial_level_number <- interspace_number - 0.5
    caudal_level_number <- interspace_number + 0.5
  }else{
    vertebral_number <- jh_get_vertebral_number_function(level)
    cranial_level_number <- vertebral_number - 1
    caudal_level_number <- vertebral_number + 1
    
    caudal_interspace_number <- vertebral_number + 0.5
    cranial_interspace_number <- vertebral_number - 0.5
  }
  
  return(list(cranial_interspace = jh_get_vertebral_level_function(number = cranial_interspace_number),
              cranial_level = jh_get_vertebral_level_function(number = cranial_level_number),
              caudal_interspace = jh_get_vertebral_level_function(number = caudal_interspace_number),
              caudal_level = jh_get_vertebral_level_function(number = caudal_level_number)
  ))
  
}

#################################
jh_filter_objects_by_y_range_function <- function(y_min = 0, y_max = 1, object_vector, full_y_range_df = all_objects_y_range_df){
  
  filtered_df <- full_y_range_df %>%
    filter(between(y, y_min, y_max))
  
  filtered_vector <- discard(.x = object_vector, .p = ~ .x %in% filtered_df$object == FALSE)
  
  return(filtered_vector)
  
}
#################################
jh_filter_osteotomies_function <- function(full_df_to_filter){
  objects_in_sequence_df <- full_df_to_filter %>%
    bind_rows(tibble(level = "x", object = c("grade_1", "grade_2", "grade_3", "grade_4", "grade_5"))) %>%
    mutate(order_number = row_number())
  
  grade_5_osteotomy_df <- objects_in_sequence_df %>%
    filter(object == "grade_5") 
  
  grade_4_osteotomy_df <- objects_in_sequence_df %>%
    filter(object == "grade_4") 
  
  grade_3_osteotomy_df <- objects_in_sequence_df %>%
    filter(object == "grade_3") 
   
  grade_2_osteotomy_df <- objects_in_sequence_df %>%
    filter(object == "grade_2") 
    # separate(col = level, into = c("proximal_level", "distal_level"), remove = FALSE)
  
  grade_1_osteotomy_df <- objects_in_sequence_df %>%
    filter(object == "grade_1") 
  
  if(nrow(grade_1_osteotomy_df) > 0){
    grade_1_osteotomy_df <- grade_1_osteotomy_df %>%
      mutate(distal_level = map(.x = level, .f =  ~ jh_get_cranial_caudal_interspace_body_list_function(level = .x)$caudal_level)) %>%
      unnest(distal_level) %>%
      distinct() %>%
      # mutate(distal_level = jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level) %>%
      mutate(keep_remove = if_else(level %in% grade_2_osteotomy_df$level | 
                                     # level %in% grade_2_osteotomy_df$proximal_level |
                                     level %in% grade_3_osteotomy_df$level |
                                     level %in% grade_4_osteotomy_df$level |
                                     level %in% grade_5_osteotomy_df$level |       
                                     distal_level %in% grade_3_osteotomy_df$level |
                                     distal_level %in% grade_4_osteotomy_df$level |
                                     distal_level %in% grade_5_osteotomy_df$level, "remove", "keep"))
  }
  
  if(nrow(grade_2_osteotomy_df)>0){
    grade_2_osteotomy_df <- grade_2_osteotomy_df %>%
      mutate(keep_remove = if_else(level %in% grade_3_osteotomy_df$level |
                                     level %in% grade_4_osteotomy_df$level | level %in% grade_5_osteotomy_df$level, "remove", "keep"))
    
    # grade_2_osteotomy_df <- grade_2_osteotomy_df %>%
    #   mutate(keep_remove = if_else(proximal_level %in% grade_3_osteotomy_df$level |
    #                                  proximal_level %in% grade_4_osteotomy_df$level |
    #                                  proximal_level %in% grade_5_osteotomy_df$level |
    #                                  distal_level %in% grade_3_osteotomy_df$level |
    #                                  distal_level %in% grade_4_osteotomy_df$level |
    #                                  distal_level %in% grade_5_osteotomy_df$level, "remove", "keep")) %>%
    #   select(-proximal_level, -distal_level)
  }
  
  if(nrow(grade_3_osteotomy_df)>0){
    grade_3_osteotomy_df <- grade_3_osteotomy_df %>%
      mutate(keep_remove = if_else(level %in% grade_4_osteotomy_df$level |
                                     level %in% grade_5_osteotomy_df$level, "remove", "keep"))
  }
  if(nrow(grade_4_osteotomy_df)>0){
    grade_4_osteotomy_df <- grade_4_osteotomy_df %>%
      mutate(keep_remove = if_else(level %in% grade_5_osteotomy_df$level, "remove", "keep"))
  }
  
  if(nrow(grade_5_osteotomy_df)>0){
    grade_5_osteotomy_df <- grade_5_osteotomy_df %>%
      mutate(keep_remove = "keep")
  }
  
  
  all_grades_df <- grade_1_osteotomy_df %>%
    bind_rows(grade_2_osteotomy_df) %>%
    bind_rows(grade_3_osteotomy_df) %>%
    bind_rows(grade_4_osteotomy_df) %>%
    bind_rows(grade_5_osteotomy_df) %>%
    select(level, order_number, side, object, keep_remove)
  
  final_filtered_full_df <- objects_in_sequence_df %>%
    left_join(all_grades_df)%>%
    select(order_number, level, keep_remove, everything()) %>%
    replace_na(list(keep_remove = "keep")) %>%
    filter(keep_remove == "keep") %>%
    select(-keep_remove, -order_number) %>%
    filter(level != "x") %>%
    filter(level != "x-x")
  
  return(final_filtered_full_df)
}


jh_make_bmp_ui_function <-  function(anterior_posterior){
  fluidRow(  
    column(12, 
           tags$table(
             tags$tr(
               tags$td(width = "20%",
                       div(style = "font-size:18px; font-weight:bold; text-align:left;", "Add BMP Kits:"),
                       actionBttn(
                         inputId = glue("reset_{anterior_posterior}_bmp"),
                         label = "Reset",
                         style = "simple", 
                         size = "xs",
                         color = "danger", 
                         icon = icon("undo-alt")
                       )
               ),
               tags$td(width = "10%",
                       actionBttn(
                         inputId = glue("add_{anterior_posterior}_xxs_bmp_button"),
                         label = "XXS",
                         style = "simple", size = "sm",block = TRUE,
                         color = "success", 
                         icon = icon("thumbs-up")
                       )
               ),
               tags$td(width = "1%"),
               tags$td(width = "10%",
                       actionBttn(
                         inputId = glue("add_{anterior_posterior}_xs_bmp_button"),
                         label = "XS",
                         style = "simple", size = "sm",block = TRUE,
                         color = "success", 
                         icon = icon("thumbs-up")
                       )
               ),
               tags$td(width = "1%"),
               tags$td(width = "10%",
                       actionBttn(
                         inputId = glue("add_{anterior_posterior}_sm_bmp_button"),
                         label = "Sm",
                         style = "simple", size = "sm",block = TRUE,
                         color = "success", 
                         icon = icon("thumbs-up")
                       )
               ),
               tags$td(width = "1%"),
               tags$td(width = "10%",
                       actionBttn(
                         inputId = glue("add_{anterior_posterior}_m_bmp_button"),
                         label = "M",
                         style = "simple", size = "sm",block = TRUE,
                         color = "success", 
                         icon = icon("thumbs-up")
                       )
               ),
               tags$td(width = "1%"),
               tags$td(width = "10%",
                       actionBttn(
                         inputId = glue("add_{anterior_posterior}_l_bmp_button"),
                         label = "L",
                         style = "simple", size = "sm",block = TRUE,
                         color = "success", 
                         icon = icon("thumbs-up")
                       )
               ),
               tags$td(width = "1%"),
               tags$td(width = "12%",
                       htmlOutput(outputId = glue("{anterior_posterior}_bmp_kits"))
               ),
               tags$td(with = "10%",
                       htmlOutput(outputId = glue("{anterior_posterior}_bmp_dosage"))
               )
             )
           )
    )
  )
}


jh_make_shiny_table_row_function <- function(required_option = FALSE,
                                             left_column_label,
                                             left_column_percent_width = 30,
                                             font_size = 14, 
                                             input_type,
                                             input_id,
                                             initial_value_selected = 0,
                                             min = 0,
                                             max = 50000,
                                             step = 100, 
                                             choices_vector = c("picker_choice"),
                                             choose_multiple = TRUE,
                                             switch_input_on_label = "Yes", 
                                             switch_input_off_label = "No",
                                             checkboxes_inline = FALSE,
                                             button_size = "sm",
                                             return_as_full_table = TRUE,
                                             text_align = "left", 
                                             top_margin = "auto",
                                             initial_height = 100,
                                             bottom_margin = "auto", 
                                             status_color = "success",
                                             individual_buttons = FALSE,
                                             justified_radio_buttons  = TRUE,
                                             check_icon_for_radiogroupbuttons = list(
                                               yes = tags$i(class = "fa fa-check-square", 
                                                            style = "color: steelblue"),
                                               no = tags$i(class = "fa fa-square-o", 
                                                           style = "color: steelblue"))){
  
  right_column_percent_width <- 100 - left_column_percent_width
  
  label_style <- glue("font-size:{paste(font_size)}px; font-weight:bold; text-align:{text_align}; margin-top:{top_margin}; margin-bottom:{bottom_margin}")
  
  middle_column_spacer <- tags$td(width = "5%", tags$div(style = label_style, "   "))
  
  if(required_option == TRUE){
    left_column_percent_width <- left_column_percent_width - 3
    required_label_style <- glue("font-size:{paste(font_size)}px; color:red; font-weight:bold; text-align:{text_align}; margin-top:{top_margin}; margin-bottom:{bottom_margin}")
  }
  
  # tags$div(style = "font-size:xxxpx; font-weight:bold; text-align:xxx; margin-top:xxx; margin-bottom:xxx", "xxxx")
  
  if(input_type == "numeric"){
    row <- tags$tr(width = "100%",
                   if(required_option == TRUE){
                     tags$td(width = "3%", tags$div(style = required_label_style, "***"))
                   },
                   tags$td(width = paste0(left_column_percent_width, "%"), tags$div(style = label_style, paste(left_column_label))),
                   tags$td(width = paste0(right_column_percent_width, "%"), numericInput(inputId = input_id, label = NULL,value = initial_value_selected, min = min, max = max, step = step))
    )  
  }
  if(input_type == "text"){
    row <- tags$tr(width = "100%",
                   if(required_option == TRUE){
                     tags$td(width = "3%", tags$div(style = required_label_style, "***"))
                   },
                   tags$td(width = paste0(left_column_percent_width, "%"), tags$div(style = label_style, paste(left_column_label))),
                   tags$td(width = paste0(right_column_percent_width, "%"), textInput(inputId = input_id, label = NULL, value = initial_value_selected))
    )  
  }
  if(input_type == "textAreaInput"){
    row <- tags$tr(width = "100%",
                   if(required_option == TRUE){
                     tags$td(width = "3%", tags$div(style = required_label_style, "***"))
                   },
                   tags$td(width = paste0(left_column_percent_width, "%"), tags$div(style = label_style, paste(left_column_label))),
                   tags$td(width = paste0(right_column_percent_width, "%"), textAreaInput(inputId = input_id, label = NULL, 
                                                                                          value = initial_value_selected,
                                                                                          width = "100%", height = initial_height))
    )  
  }
  
  # textAreaInput(inputId = "operative_note_text", label = "Operative Note:", width = "100%", height = 750),
  if(input_type == "picker"){
    row <- tags$tr(width = "100%",
                   if(required_option == TRUE){
                     tags$td(width = "3%", tags$div(style = required_label_style, "***"))
                   },
                   tags$td(width = paste0(left_column_percent_width, "%"), tags$div(style = label_style, paste(left_column_label))),
                   tags$td(width = paste0(right_column_percent_width, "%"), pickerInput(inputId = input_id, label = NULL, choices = choices_vector, selected = initial_value_selected, multiple = choose_multiple))
    )  
  }
  
  if(input_type == "switch"){
    row <- tags$tr(width = "100%",
                   if(required_option == TRUE){
                     tags$td(width = "3%", tags$div(style = required_label_style, "***"))
                   },
                   tags$td(width = paste0(left_column_percent_width, "%"), tags$div(style = label_style, paste(left_column_label))),
                   tags$td(width = paste0(right_column_percent_width, "%"), switchInput(inputId = input_id, label = NULL, value = initial_value_selected, onLabel = switch_input_on_label, offLabel = switch_input_off_label))
    )  
  }
  
  if(input_type == "checkbox"){
    row <- tags$tr(width = "100%",
                   if(required_option == TRUE){
                     tags$td(width = "3%", tags$div(style = required_label_style, "***"))
                   },
                   tags$td(width = paste0(left_column_percent_width, "%"), tags$div(style = label_style, paste(left_column_label))),
                   if(text_align == "right"){middle_column_spacer},
                   tags$td(width = paste0(right_column_percent_width, "%"), awesomeCheckboxGroup(inputId = input_id,
                                                                                                 label = NULL, 
                                                                                                 choices = choices_vector, 
                                                                                                 selected = initial_value_selected, 
                                                                                                 inline = checkboxes_inline))
    )  
  }
  if(input_type == "checkboxGroupButtons"){
    row <- tags$tr(width = "100%",
                   if(required_option == TRUE){
                     tags$td(width = "3%", tags$div(style = required_label_style, "***"))
                   },
                   tags$td(width = paste0(left_column_percent_width, "%"), tags$div(style = label_style, paste(left_column_label))),
                   if(text_align == "right"){middle_column_spacer},
                   tags$td(width = paste0(right_column_percent_width, "%"), checkboxGroupButtons(inputId = input_id,
                                                                                                 label = NULL, 
                                                                                                 justified = TRUE, 
                                                                                                 choices = choices_vector, 
                                                                                                 selected = initial_value_selected, 
                                                                                                 checkIcon = list(
                                                                                                   yes = tags$i(class = "fas fa-check",
                                                                                                                style = "color: steelblue")),
                                                                                                 direction = if_else(checkboxes_inline == TRUE, "horizontal", "vertical")))
    )  
  }
  
  if(input_type == "prettyCheckboxGroup"){
    row <- tags$tr(width = "100%",
                   if(required_option == TRUE){
                     tags$td(width = "3%", tags$div(style = required_label_style, "***"))
                   },
                   tags$td(width = paste0(left_column_percent_width, "%"), tags$div(style = label_style, paste(left_column_label))),
                   if(text_align == "right"){middle_column_spacer},
                   tags$td(width = paste0(right_column_percent_width, "%"), prettyCheckboxGroup(inputId = input_id,
                                                                                                 label = NULL, 
                                                                                                 choices = choices_vector,
                                                                                                status = status_color,
                                                                                                 selected = initial_value_selected, 
                                                                                                icon = icon("check"), 
                                                                                                bigger = TRUE, 
                                                                                                 inline = checkboxes_inline))
    )  
  }
  
  
  if(input_type == "radioGroupButtons"){
    row <- tags$tr(width = "100%",
                   if(required_option == TRUE){
                     tags$td(width = "3%", tags$div(style = required_label_style, "***"))
                   },
                   tags$td(width = paste0(left_column_percent_width, "%"), tags$div(style = label_style, paste(left_column_label))),
                   if(text_align == "right"){middle_column_spacer},
                   tags$td(width = paste0(right_column_percent_width, "%"), 
                           radioGroupButtons(inputId = input_id, label = NULL, choices = choices_vector, selected = initial_value_selected, direction = if_else(checkboxes_inline == TRUE, "horizontal", "vertical"), checkIcon = check_icon_for_radiogroupbuttons,
                                             individual = individual_buttons))
    )  
  }

  if(input_type == "awesomeRadio"){
    row <- tags$tr(width = "100%",
                   if(required_option == TRUE){
                     tags$td(width = "3%", tags$div(style = required_label_style, "***"))
                   },
                   tags$td(width = paste0(left_column_percent_width, "%"), tags$div(style = label_style, paste(left_column_label))),
                   if(text_align == "right"){middle_column_spacer},
                   tags$td(width = paste0(right_column_percent_width, "%"), awesomeRadio(inputId = input_id, 
                                                                                         label = NULL,
                                                                                         choices = choices_vector, 
                                                                                         selected = initial_value_selected,
                                                                                         inline = checkboxes_inline, 
                                                                                         status = status_color))
    )  
  }
  
  if(input_type == "prettyRadioButtons"){
    row <- tags$tr(width = "100%",
                   if(required_option == TRUE){
                     tags$td(width = "3%", tags$div(style = required_label_style, "***"))
                   },
                   tags$td(width = paste0(left_column_percent_width, "%"), tags$div(style = label_style, paste(left_column_label))),
                   if(text_align == "right"){middle_column_spacer},
                   tags$td(width = paste0(right_column_percent_width, "%"), prettyRadioButtons(inputId = input_id, 
                                                                                         label = NULL,
                                                                                         choices = choices_vector, 
                                                                                         selected = initial_value_selected,
                                                                                         inline = checkboxes_inline, 
                                                                                         icon = icon("check"), 
                                                                                         bigger = TRUE,
                                                                                         status = status_color))
    )  
  }

  if(input_type == "date"){
    row <- tags$tr(width = "100%",
                   if(required_option == TRUE){
                     tags$td(width = "3%", tags$div(style = required_label_style, "***"))
                   },
                   tags$td(width = paste0(left_column_percent_width, "%"), tags$div(style = label_style, paste(left_column_label))),
                   tags$td(width = paste0(right_column_percent_width, "%"), dateInput(inputId = input_id, label = NULL, format = "mm-dd-yyyy", value = "", max = Sys.Date(), autoclose = TRUE, startview = "decade"))
    )  
  }
  # return(row)
  
  if(return_as_full_table == TRUE){
    return(tags$table(width = "100%",
                      row))
  }else{
    return(row)
  }
  
}

jh_make_supplemental_rod_ui_function <- function(rod_type, input_label){
  
  left_input_identifier <- as.character(glue("add_left_{rod_type}"))
  right_input_identifier <- as.character(glue("add_right_{rod_type}"))
  rod_material_vector <- c("NA", "Titanium", "Cobalt Chrome", "Stainless Steel")
  rod_size_vector <- c("NA", "Transition", "3.5mm", "4.0mm", "4.5mm", "4.75mm", "5.5mm", "6.0mm", "6.35mm/0.25 in")
  
  left_table <- tags$table(
    tags$tr(
      tags$td(width = "35%",
              awesomeCheckbox(
                inputId = left_input_identifier,
                label = input_label,
                value = FALSE,
                status = "success")
      ),
      tags$td(width = "35%",
              conditionalPanel(condition = glue("input.{left_input_identifier} == true"),
                               prettyRadioButtons(inputId = as.character(glue("left_{rod_type}_material")), 
                                                  label = NULL, 
                                                  choices = rod_material_vector, 
                                                  selected = "NA", 
                                                  outline = TRUE, 
                                                  shape = "round", 
                                                  inline = FALSE,
                                                  status = "primary",
                                                  width = "90%"
                               )
              )
      ),
      tags$td(width = "30%",
              conditionalPanel(condition = glue("input.{left_input_identifier} == true"),
                               pickerInput(inputId = as.character(glue("left_{rod_type}_size")), 
                                           label = "Size:", 
                                           choices = rod_size_vector, 
                                           selected = "NA", 
                                           multiple = FALSE, 
                                           width = "90%"
                               )
              )
      )
    )
  )
  
  right_table <- tags$table(
    tags$tr(
      tags$td(width = "35%",
              awesomeCheckbox(
                inputId = right_input_identifier,
                label = input_label,
                value = FALSE,
                status = "success")
      ),
      tags$td(width = "35%",
              conditionalPanel(condition = glue("input.{right_input_identifier} == true"),
                               prettyRadioButtons(inputId = as.character(glue("right_{rod_type}_material")), 
                                                  label = NULL, 
                                                  choices = rod_material_vector, 
                                                  selected = "NA", 
                                                  outline = TRUE, 
                                                  shape = "round", 
                                                  inline = FALSE, 
                                                  status = "primary", 
                                                  width = "90%")
              )
      ),
      tags$td(width = "30%",
              conditionalPanel(condition = glue("input.{right_input_identifier} == true"),
                               pickerInput(inputId = as.character(glue("right_{rod_type}_size")), 
                                           label = "Size:", 
                                           choices = rod_size_vector, 
                                           selected = "NA", 
                                           multiple = FALSE, 
                                           width = "90%"
                               )
              )
      )
    )
  )
  
  full_ui <- column(width = 12, 
                    fixedRow(
                      column(6, 
                             conditionalPanel(condition = "input.left_supplemental_rods_eligible == true", 
                                              left_table
                             )
                      ),
                      column(6, 
                             conditionalPanel(condition = "input.right_supplemental_rods_eligible == true",
                                              right_table
                             )
                      )
                    ),
                    if(rod_type == "intercalary_rod"){
                      fixedRow(
                        column(4, 
                               conditionalPanel(condition = glue("input.{left_input_identifier} == true"), 
                                                sliderTextInput(inputId = as.character(glue("left_{rod_type}")),
                                                                label = NULL, 
                                                                choices = c("a", "b"),
                                                                selected = c("a", "b"), 
                                                                width = "90%")
                               )
                        ),
                        column(2, 
                               conditionalPanel(condition = glue("input.{left_input_identifier} == true"), 
                                                pickerInput(inputId = "left_intercalary_rod_junction",
                                                            label = "Junction:",
                                                            choices = c("a", "b"),
                                                            width = "fit")
                               )
                        ),
                        column(4, 
                               conditionalPanel(condition = glue("input.{right_input_identifier} == true"), 
                                                sliderTextInput(inputId = as.character(glue("right_{rod_type}")), 
                                                                label = NULL, 
                                                                choices = c("a", "b"), 
                                                                selected = c("a", "b"), 
                                                                width = "90%")
                               )
                        ),
                        conditionalPanel(condition = glue("input.{right_input_identifier} == true"), 
                                         pickerInput(inputId = "right_intercalary_rod_junction",
                                                     label = "Junction:",
                                                     choices = c("a", "b"),
                                                     width = "fit")
                        )
                      ) 
                    }else{
                      fixedRow(
                        column(6, 
                               conditionalPanel(condition = glue("input.{left_input_identifier} == true"), 
                                                sliderTextInput(inputId = as.character(glue("left_{rod_type}")), 
                                                                label = NULL,
                                                                choices = c("a", "b"), 
                                                                selected = c("a", "b"), 
                                                                width = "90%")
                               )
                        ),
                        column(6, 
                               conditionalPanel(condition = glue("input.{right_input_identifier} == true"), 
                                                sliderTextInput(inputId = as.character(glue("right_{rod_type}")), 
                                                                label = NULL,
                                                                choices = c("a", "b"), 
                                                                selected = c("a", "b"),
                                                                width = "90%")
                               )
                        )
                      ) 
                    }
  ) 
  
  return(full_ui)
}





jh_make_shiny_table_column_function <- function(input_type,
                                                left_input_id = NULL,
                                                left_label = NULL,
                                                left_condition_statement = "x",
                                                right_input_id= NULL,
                                                right_label = NULL,
                                                right_condition_statement = "x",
                                                left_column_percent_width = 50,
                                                right_column_percent_width = 50,
                                                initial_value_selected = 0,
                                                min = 0,
                                                max = 50000,
                                                step = 100, 
                                                picker_choose_multiple = TRUE,
                                                choices_vector = c("picker_choice"), 
                                                switch_input_on_label = "Yes", 
                                                switch_input_off_label = "No",
                                                checkboxes_inline = FALSE,
                                                button_size = "sm",
                                                font_size = 14, 
                                                status = NULL,
                                                table_percent_width = 100,
                                                text_align = "left",
                                                bottom_margin = "auto",
                                                top_margin = "auto"
){
  
  
  if(input_type == "title"){
    left_shiny_input <-  div(style = glue("font-size:{font_size}px; font-weight:bold; text-align:{text_align}; margin-bottom:{bottom_margin}; margin-top:{top_margin}"), left_label)
    
    right_shiny_input <-div(style = glue("font-size:{font_size}px; font-weight:bold; text-align:{text_align}; margin-bottom:{bottom_margin}; margin-top:{top_margin}"), right_label)
  }
  
  
  if(input_type == "numericInput"){
    left_shiny_input <-numericInput(inputId = left_input_id, 
                                    label = left_label, 
                                    value = initial_value_selected, 
                                    min = min,
                                    max = max,
                                    step = step
    )
    
    right_shiny_input <-numericInput(inputId = right_input_id, 
                                     label = right_label, 
                                     value = initial_value_selected, min = min, max = max, step = step
    )
  }
  
  if(input_type == "textInput"){
    left_shiny_input <-textInput(inputId = left_input_id, 
                                 label = left_label, 
                                 value = initial_value_selected 
    )
    
    right_shiny_input <-textInput(inputId = right_input_id, 
                                  label = right_label, 
                                  value = initial_value_selected
    )
  }
  
  if(input_type == "pickerInput"){
    left_shiny_input <-pickerInput(inputId = left_input_id, 
                                   label = left_label, 
                                   choices = choices_vector,
                                   selected = initial_value_selected, 
                                   multiple = picker_choose_multiple 
    )
    
    right_shiny_input <-pickerInput(inputId = right_input_id, 
                                    label = right_label, 
                                    choices = choices_vector,
                                    selected = initial_value_selected,
                                    multiple = picker_choose_multiple )
    
  }
  
  if(input_type == "switchInput"){
    left_shiny_input <-switchInput(inputId = left_input_id, 
                                   label = left_label, 
                                   onLabel = switch_input_on_label, 
                                   offLabel = switch_input_off_label
    )
    
    right_shiny_input <-switchInput(inputId = right_input_id, 
                                    label = right_label, 
                                    onLabel = switch_input_on_label, 
                                    offLabel = switch_input_off_label
    )
    
  }
  
  if(input_type == "awesomeCheckboxGroup"){
    left_shiny_input <-awesomeCheckboxGroup(inputId = left_input_id, 
                                            label = left_label, 
                                            choices = choices_vector,
                                            selected = initial_value_selected, 
                                            inline = checkboxes_inline, 
                                            status = status)
    
    right_shiny_input <-awesomeCheckboxGroup(inputId = right_input_id, 
                                             label = right_label, 
                                             choices = choices_vector,
                                             selected = initial_value_selected,
                                             inline = checkboxes_inline, 
                                             status = status)
    
  }
  if(input_type == "radioGroupButtons"){
    left_shiny_input <-radioGroupButtons(inputId = left_input_id, 
                                         label = left_label, 
                                         choices = choices_vector,
                                         selected = initial_value_selected, 
                                         direction = if_else(checkboxes_inline == TRUE, "horizontal", "vertical"),
                                         size = button_size, 
                                         status = status, 
                                         checkIcon = list(
                                           yes = tags$i(class = "fas fa-check",
                                                        style = "color: steelblue")))
    
    right_shiny_input <-radioGroupButtons(inputId = right_input_id, 
                                          label = right_label, 
                                          choices = choices_vector,
                                          selected = initial_value_selected,
                                          direction = if_else(checkboxes_inline == TRUE, "horizontal", "vertical"),
                                          size = button_size, 
                                          status = status, 
                                          checkIcon = list(
                                            yes = tags$i(class = "fas fa-check",
                                                         style = "color: steelblue")))
    
  }
  if(input_type == "awesomeRadio"){
    left_shiny_input <-awesomeRadio(inputId = left_input_id, 
                                    label = left_label, 
                                    choices = choices_vector,
                                    selected = initial_value_selected, 
                                    inline = checkboxes_inline, 
                                    status = status)
    
    right_shiny_input <-awesomeRadio(inputId = right_input_id, 
                                     label = right_label, 
                                     choices = choices_vector,
                                     selected = initial_value_selected, 
                                     inline = checkboxes_inline, 
                                     status = status)
    
  }
  if(input_type == "awesomeCheckbox"){
    left_shiny_input <-  awesomeCheckbox(
      inputId = left_input_id,
      label = left_label,
      value = FALSE,
      status = status
    )
    
    right_shiny_input <-awesomeCheckbox(
      inputId = right_input_id,
      label = right_label,
      value = FALSE,
      status = status
    )
  }
  
  if(input_type == "sliderTextInput"){
    left_shiny_input <- sliderTextInput(
      inputId = left_input_id,
      label = left_label,
      choices = choices_vector,
      selected = initial_value_selected
    )
    right_shiny_input <- sliderTextInput(
      inputId = right_input_id,
      label = right_label,
      choices = choices_vector,
      selected = initial_value_selected
    )
  }
  
  if(str_detect(string = left_condition_statement, pattern = "input") | str_detect(string = right_condition_statement, pattern = "input")){
    table_column <- tags$table(width = paste0(table_percent_width, "%"),
                               tags$tr(
                                 tags$td(width = "5%"),
                                 tags$td(width = paste0(left_column_percent_width- 7.5, "%"),
                                         conditionalPanel(condition = paste(left_condition_statement),
                                                          left_shiny_input)
                                 ),
                                 tags$td(width = "5%"),
                                 tags$td(width = paste0(right_column_percent_width- 7.5, "%"),
                                         conditionalPanel(condition = paste(right_condition_statement),
                                                          right_shiny_input)
                                 ),
                                 tags$td(width = "5%")
                               )
    )
  }else{
    table_column <- tags$table(width = paste0(table_percent_width, "%"),
                               tags$tr(
                                 tags$td(width = "5%"),
                                 tags$td(width = paste0(left_column_percent_width - 7.5, "%"),
                                         left_shiny_input
                                 ),
                                 tags$td(width = "5%"),
                                 tags$td(width = paste0(right_column_percent_width - 7.5, "%"),
                                         right_shiny_input)
                               ),
                               tags$td(width = "5%"),
    )
  }
  
  
  
  return(table_column)
}


##########################################  DETERMINE ANTERIOR FUSION LEVELS ##################### #####################

fusion_levels_df_function <- function(all_objects_to_add_df){
  if(any(all_objects_to_add_df$fusion == "yes")){
    fusion_range_df <- all_objects_to_add_df %>%
      filter(fusion == "yes") %>%
      select(level, body_interspace) %>%
      mutate(level = if_else(level == "S2AI", "S1", level)) %>%
      mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
      arrange(vertebral_number) 
    
    if(any(fusion_range_df$body_interspace == "body")){
      fusion_bodies_df <- fusion_range_df %>%
        filter(body_interspace == "body") %>%
        filter(vertebral_number == min(vertebral_number) | vertebral_number == max(vertebral_number)) %>%
        select(vertebral_number) %>%
        distinct() %>%
        mutate(vertebral_number = if_else(vertebral_number == min(vertebral_number), vertebral_number + 0.5, vertebral_number - 0.5))
      
      fusions_levels_df <- tibble(vertebral_number = seq(from = min(fusion_bodies_df$vertebral_number), to = max(fusion_bodies_df$vertebral_number), by = 1)) %>%
        mutate(level = jh_get_vertebral_level_function(number = vertebral_number))  %>%
        bind_rows(fusion_range_df %>%
                    filter(body_interspace == "interspace") %>%
                    select(level, vertebral_number) %>%
                    distinct()) %>%
        arrange(vertebral_number) %>%
        distinct()
    }else{
      fusions_levels_df <- tibble(vertebral_number = seq(from = min(fusion_range_df$vertebral_number), to = max(fusion_range_df$vertebral_number), by = 1)) %>%
        mutate(level = jh_get_vertebral_level_function(number = vertebral_number))  %>%
        arrange(vertebral_number) %>%
        distinct()
    }
    
  }else{
    fusions_levels_df <- tibble(level = character(), vertebral_number = double(), category = character())
  }
  
  
  
  return(fusions_levels_df)
}

jh_fusion_category_function <- function(fusion_vector, all_objects_df){
  
  fusions_ranked_df <- all_objects_df %>%
    filter(body_interspace == "body") %>%
    select(vertebral_number, approach, object) %>%
    distinct() %>%
    mutate(vertebral_number = vertebral_number - 0.5) %>%
    bind_rows(all_objects_df %>%
                filter(body_interspace == "body") %>%
                select(vertebral_number, approach, object) %>%
                distinct() %>%
                mutate(vertebral_number = vertebral_number + 0.5)) %>%
    mutate(level = jh_get_vertebral_level_function(number = vertebral_number)) %>%
    filter(!is.na(level)) %>%
    distinct() %>%
    bind_rows(all_objects_df %>% filter(body_interspace == "interspace")) %>%
    arrange(vertebral_number) %>%
    select(level, vertebral_number, approach, object) %>%
    mutate(fusion_category = if_else(approach == "anterior", "anterior_interbody_fusion",
                                     case_when(
                                       str_detect(object, "screw") | str_detect(object, "hook") |str_detect(object, "wire") ~ "posterolateral_fusion",
                                       str_detect(object, "lif") | str_detect(object, "interbody") |str_detect(object, "cage")| str_detect(object, "extracav")  ~ "posterior_interbody_fusion",
                                     ))) %>%
    mutate(fusion_rank = if_else(str_detect(string = fusion_category, pattern = "interbody"), 1, 2)) 
  
  if(length(fusion_vector)>0){
    # fusion_df <- tibble(level = fusion_vector) %>%
    #   left_join(fusions_ranked_df) %>%
    fusion_df <- fusions_ranked_df %>%
      filter(level %in% fusion_vector) %>%
      mutate(fusion_rank = if_else(is.na(fusion_rank), 9, fusion_rank)) %>%
      group_by(level, approach) %>%
      filter(fusion_rank == min(fusion_rank)) %>%
      select(level, vertebral_number, object = fusion_category) %>%
      distinct() %>%
      ungroup() %>%
      mutate(category = "fusion")
  }else{
    fusion_df <- tibble(level = character(), vertebral_number = double(), object = character())
  }
  return(fusion_df)
}

# anterior_fusion_levels_function <- function(all_objects_added_df){
#   anterior_fusion_implants_df <- all_objects_added_df %>%
#     filter(approach == "anterior") %>%
#     select(level, vertebral_number, object) %>%
#     filter(object != "screw_washer") %>%
#     filter(object != "anterior_disc_arthroplasty") %>%
#     distinct()
#   
#   corpectomy_df <- anterior_fusion_implants_df %>%
#     filter(str_detect(string = object, pattern = "corpectomy")) %>%
#     select(level, object) %>%
#     distinct() %>%
#     mutate(level = map(.x = level, .f = ~ jh_get_cranial_caudal_interspace_body_list_function(level = .x))) %>%
#     unnest() %>%
#     unnest() %>%
#     filter(str_detect(string = level, pattern = "-")) %>%
#     mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
#     mutate(object = "fusion") %>%
#     distinct()
#   
#   fusion_levels_df <- anterior_fusion_implants_df %>%
#     union_all(corpectomy_df) %>%
#     filter(str_detect(level, pattern = "-")) %>%
#     mutate(object = "fusion") %>%
#     distinct() %>%
#     arrange(vertebral_number)
#   
#   return(fusion_levels_df)
#   
# }




##########################################  MAKE UI's FOR INTERBODY ##################### ##################### 
##########################################  MAKE UI's FOR INTERBODY ##################### ##################### 

make_interbody_ui_function <-  function(level = NULL){
  level_input_id <- str_to_lower(string = str_replace_all(string = level, pattern = "-", replacement = "_"))
  
  fluidRow(
    hr(),
    column(width = 3,
           tags$div(style = "font-size:20px; font-weight:bold; text-align:center", paste0(level, ":"))
    ),
    column(9, 
           fixedRow(
             column(width = 5, 
                    pickerInput(
                      inputId = glue("{level_input_id}_interbody_composition"),
                      label = NULL,
                      inline = "auto",
                      options = list(
                        title = "Choose Implant Type"),
                      choices = c("Allograft",
                                  "Autograft",
                                  "Carbon Fiber",
                                  "Coated PEEK",
                                  "PEEK", "Hybrid",
                                  "Titanium",
                                  "3D/Porous Titanium",
                                  "Other"),
                    ), 
             ), 
             column(width = 4,
                    textInput(inputId = glue("{level_input_id}_interbody_device_name"),
                              label = NULL, placeholder = "Cage Name")
             ), 
             column(width = 3, 
                    numericInput(inputId = glue("{level_input_id}_interbody_height"),
                                 label = NULL, value = 8,min = 5, max = 30,step = 1))
           ),
           fixedRow(
             column(width = 3, 
                    h5("Other Comments")),
             column(width = 6,
                    textInput(inputId = glue("{level_input_id}_interbody_other"),
                              label = NULL,
                              placeholder = "Details",
                              width = "100%")
             ),
             column(width = 3, 
                    awesomeCheckbox(
                      inputId = glue("{level_input_id}_interbody_integrated_fixation"),
                      label = "Integrated Fixation",
                      value = FALSE,
                      status = "danger"
                    ),
                    # prettyCheckboxGroup(
                    #   inputId = glue("{level_input_id}_interbody_integrated_fixation"),
                    #   label = NULL, choices = c("Integrated Fixation"),
                    #   outline = TRUE,
                    #   # value = FALSE,
                    #   status = "danger",
                    #   shape = "curve"
                    # ), 
                    # conditionalPanel(condition = glue("input.{level_input_id}_interbody_integrated_fixation.indexOf('Integrated Fixation')>-1"),
                    conditionalPanel(condition = glue("input.{level_input_id}_interbody_integrated_fixation"),
                                     h4("Leave 0 for no screw"), 
                                     textInput(inputId = glue("{level_input_id}_interbody_cranial_screw_1_size"), 
                                               label = "Cranial Screw 1 size:", 
                                               value = "0",
                                               placeholder = "W x L"
                                                 ),
                                     textInput(inputId = glue("{level_input_id}_interbody_cranial_screw_2_size"), 
                                               label = "Cranial Screw 2 size:", 
                                               value = "0",
                                               placeholder = "W x L"
                                     ),
                                     textInput(inputId = glue("{level_input_id}_interbody_caudal_screw_1_size"), 
                                               label = "Caudal Screw 1 size:", 
                                               value = "0",
                                               placeholder = "W x L"
                                     ),
                                     textInput(inputId = glue("{level_input_id}_interbody_caudal_screw_2_size"), 
                                               label = "Caudal Screw 2 size:", 
                                               value = "0",
                                               placeholder = "W x L"
                                     )
                    ),
                    # prettyCheckboxGroup(
                    #   inputId = glue("{level_input_id}_interbody_expandable"),
                    #   label = NULL,
                    #   choices = c("Expandable"),
                    #   outline = TRUE,
                    #   status = "danger",
                    #   shape = "curve"
                    # )
                    awesomeCheckbox(
                      inputId = glue("{level_input_id}_interbody_expandable"),
                      label = "Expandable",
                      value = FALSE,
                      status = "danger"
                    )
             )
           )
    ),
    hr()
  )
}

##########################################  MAKE UI's FOR screw ##################### ##################### 
##########################################  MAKE UI's FOR screw ##################### ##################### 
make_screw_sizes_ui_function <-  function(level = NULL, left_screw = "no_screw", right_screw = "no_screw", left_diameter = " ", left_length = " ", right_diameter = " ", right_length = " "){
  if(left_screw != "no_screw"){
    left_diameter <- numericInput(inputId = glue("left_{str_to_lower(level)}_screw_diameter"), label = NULL, value = left_diameter, min = 1, max = 12, step = 0.5
    ) 
    left_length <- numericInput(inputId = glue("left_{str_to_lower(level)}_screw_length"), label = NULL, value = left_length, min = 1, max = 140, step = 5
    ) 
  }else{
    left_diameter <- NULL
    left_length <- NULL
  }
  
  if(right_screw != "no_screw"){
    right_diameter <- numericInput(inputId = glue("right_{str_to_lower(level)}_screw_diameter"), label = NULL, value = right_diameter, min = 1, max = 12, step = 0.5
    ) 
    right_length <- numericInput(inputId = glue("right_{str_to_lower(level)}_screw_length"), label = NULL, value = right_length, min = 1, max = 140, step = 5
    ) 
  }else{
    right_diameter <- NULL
    right_length <- NULL
  }
  
  tags$tr(width = "100%", 
          tags$td(width = "10%", div(style = "font-size:14px; font-weight:bold; text-align:center; padding-bottom:10px", paste(level))),
          tags$td(width = "10%", div(
            left_diameter
          )
          ),
          tags$td(width = "10%", div(
            left_length
          )
          ),
          tags$td(width = "10%", div( 
            right_diameter
          )
          ),
          tags$td(width = "10%", div(
            right_length
          )
          )
  )
}

make_screw_types_function <-  function(level = NULL, left_screw_level = "no_screw", right_screw_level = "no_screw", left_selected = "P", right_selected = "P"){
  if(left_screw_level != "no_screw"){
    left_ui <- radioGroupButtons(   #"option2",
      inputId = glue("left_{str_to_lower(level)}_screw_type"),
      label = NULL,
      choices = c("M", "U", "P", "Red", "Offset"),
      selected = left_selected,
      checkIcon = list(yes = icon("wrench")),
      size = "xs",
      justified = TRUE,
      width = "95%"
    )
  }else{
    left_ui <- NULL
  }
  if(right_screw_level != "no_screw"){
    right_ui <- radioGroupButtons(   #"option2",
      inputId = glue("right_{str_to_lower(level)}_screw_type"),
      label = NULL,
      choices = c("M", "U", "P", "Red", "Offset"),
      selected = right_selected,
      checkIcon = list(yes = icon("wrench")),
      justified = TRUE,
      width = "95%",
      size = "xs"
    )
  }else{
    right_ui <- NULL
  }
  tags$tr(width = "100%", 
          tags$td(width = "7%", div(style = "font-size:14px; font-weight:bold; text-align:center; padding-bottom:10px",   paste(level))),
          tags$td(width = "45%",
                  left_ui
                  # div(id = "my_small_button_input",
                  #     left_ui)
          ),
          tags$td(width = "45%",
                  right_ui
                  # div(id = "my_small_button_input",
                  #     right_ui)
          )
  )
}
# make_screw_sizes_ui_function <-  function(level = NULL, left_screw_level = "no_screw", right_screw_level = "no_screw", left_selected = "Unknown", right_selected = "Unknown"){
#   if(left_screw_level != "no_screw"){
#     left_diameter <- textInput(inputId = glue("left_{str_to_lower(level)}_screw_diameter"), 
#                                label = NULL, 
#                                placeholder = "D",
#                                width = "90%")
#     left_length <- textInput(inputId = glue("left_{str_to_lower(level)}_screw_length"),
#                              label = NULL,
#                              placeholder = "L",
#                              width = "90%")
#   }else{
#     left_diameter <- NULL
#     left_length <- NULL
#   }
#   if(right_screw_level != "no_screw"){
#     right_diameter <- textInput(inputId = glue("right_{str_to_lower(level)}_screw_diameter"), 
#                                 label = NULL, 
#                                 placeholder = "D",
#                                 width = "90%")
#     right_length <- textInput(inputId = glue("right_{str_to_lower(level)}_screw_length"),
#                               label = NULL,
#                               placeholder = "L",
#                               width = "90%")
#   }else{
#     right_diameter <- NULL
#     right_length <- NULL
#   }
#   
#   tags$tr(width = "100%", 
#           tags$td(width = "10%", div(style = "font-size:14px; font-weight:bold; text-align:center; padding-bottom:10px", paste(level))),
#           tags$td(width = "10%", div(id = "my_small_text_input", 
#                                      left_diameter
#           )
#           ),
#           tags$td(width = "10%", div(id = "my_small_text_input", 
#                                      left_length
#           )
#           ),
#           tags$td(width = "10%", div(id = "my_small_text_input", 
#                                      right_diameter
#           )
#           ),
#           tags$td(width = "10%", div(id = "my_small_text_input", 
#                                      right_length
#           )
#           )
#   )
# }


##########################################  MAKE UI's FOR screw type ##################### ##################### 
##########################################  MAKE UI's FOR screw type ##################### ##################### 
make_screw_types_function <-  function(level = NULL, left_screw_level = "no_screw", right_screw_level = "no_screw", left_selected = "P", right_selected = "P"){
  if(left_screw_level != "no_screw"){
    left_ui <- radioGroupButtons(   #"option2",
      inputId = glue("left_{str_to_lower(level)}_screw_type"),
      label = NULL,
      choices = c("M", "U", "P", "Red", "Offset"),
      selected = left_selected,
      checkIcon = list(yes = icon("wrench")),
      size = "xs",
      justified = TRUE,
      width = "95%"
    )
  }else{
    left_ui <- NULL
  }
  if(right_screw_level != "no_screw"){
    right_ui <- radioGroupButtons(   #"option2",
      inputId = glue("right_{str_to_lower(level)}_screw_type"),
      label = NULL,
      choices = c("M", "U", "P", "Red", "Offset"),
      selected = right_selected,
      checkIcon = list(yes = icon("wrench")),
      justified = TRUE,
      width = "95%",
      size = "xs"
    )
  }else{
    right_ui <- NULL
  }
  
  tags$tr(width = "100%", 
          tags$td(width = "7%", div(style = "font-size:14px; font-weight:bold; text-align:center; padding-bottom:10px",   paste(level))),
          tags$td(width = "45%",
                  div(id = "my_small_button_input",
                      left_ui)
          ),
          tags$td(width = "45%",
                  div(id = "my_small_button_input",
                      right_ui)
          )
  )
  
}




jh_cranial_and_caudal_list_for_supplementary_rods_function <- function(all_objects_df, osteotomy_site = NULL){
  
  implant_df <- all_objects_df %>%
    filter(str_detect(string = object, pattern = "screw") | str_detect(string = object, pattern = "hook") | str_detect(string = object, pattern = "wire")) %>%
    # select(level, vertebral_number) %>%
    arrange(vertebral_number)
  
  if(nrow(implant_df)> 2){
    ############## ACCESSORY ROD ###################
    cranial_point_accessory <- tail(head(implant_df$level, n=2), n=1) 
    caudal_point_accessory <- head(tail(implant_df$level, n=2), n=1)
    
    if(cranial_point_accessory == caudal_point_accessory){
      accessory_vector <- c(head(implant_df$level, n=1), caudal_point_accessory)
    }else{
      accessory_vector <- c(cranial_point_accessory, caudal_point_accessory)
    }
  }else{
    accessory_vector <-  c("a", "b")
  }
  
  ############## Satellite ROD ###################
  
  if(nrow(implant_df) > 3 & !is.null(osteotomy_site)){
    
    osteotomy_df <- tibble(level = osteotomy_site) %>%
      mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level))
    
    
    cranial_satellite_df <- implant_df %>%
      mutate(satellite_cranial_number = min(osteotomy_df$vertebral_number) - 1) %>%
      filter(vertebral_number <= satellite_cranial_number)
    
    caudal_satellite_df <- implant_df %>%
      mutate(satellite_caudal_number = max(osteotomy_df$vertebral_number) + 1) %>%
      filter(vertebral_number >= satellite_caudal_number)
    
    cranial_point_satellite <- jh_get_vertebral_level_function(number = (max(cranial_satellite_df$vertebral_number)))
    caudal_point_satellite <- jh_get_vertebral_level_function(number = (min(caudal_satellite_df$vertebral_number)))
    
    
    satellite_vector <- c(cranial_point_satellite, caudal_point_satellite)
    # satellite_vector <- c("WRONG", "WRONG")
  }else if(nrow(implant_df) > 3){
    satellite_vector <- c( head(tail(implant_df$level, n=3), n=1),  head(tail(implant_df$level, n=2), n=1))
  }else{
    satellite_vector <- c("a", "b")
  }
  if(satellite_vector[[1]] == satellite_vector[[2]]){
    satellite_vector <- c("a", "b")  
  }
  
  
  ############## Intercalary ROD ###################
  if(nrow(implant_df) > 3 & !is.null(osteotomy_site)){
    
    osteotomy_df <- tibble(level = osteotomy_site) %>%
      mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level))

    
    cranial_intercalary_df <- implant_df %>%
      mutate(intercalary_cranial_number = min(osteotomy_df$vertebral_number) - 1) %>%
      filter(vertebral_number <= intercalary_cranial_number)
    
    caudal_intercalary_df <- implant_df %>%
      mutate(intercalary_caudal_number = max(osteotomy_df$vertebral_number) + 1) %>%
      filter(vertebral_number >= intercalary_caudal_number)
    
    cranial_point_intercalary <- jh_get_vertebral_level_function(number = (max(cranial_intercalary_df$vertebral_number)))
    caudal_point_intercalary <- jh_get_vertebral_level_function(number = (min(caudal_intercalary_df$vertebral_number)))
    
    intercalary_vector <- c(cranial_point_intercalary, caudal_point_intercalary)
    
  }else if(nrow(implant_df) > 3){
    intercalary_vector <- c( head(tail(implant_df$level, n=3), n=1),  head(tail(implant_df$level, n=2), n=1))
  }else{
    intercalary_vector <- c("a", "b")
  }
  if(intercalary_vector[[1]] == intercalary_vector[[2]]){
    intercalary_vector <- c("a", "b")  
  }
  
  if(nrow(implant_df) > 3){
    if(nrow(implant_df) > 7){
      linked_vector <- c(head(tail(implant_df$level, n=5), n=1), 
                         head(tail(implant_df$level, n=3), n=1))
    }else{
      linked_vector <- c(head(tail(implant_df$level, n=4), n=1), 
                         head(tail(implant_df$level, n=2), n=1))
    }
    
  }else{
    linked_vector <- c("a", "b")
  }
  if(linked_vector[[1]] == linked_vector[[2]]){
    linked_vector <- c("a", "b")  
  }
  
  return(list(accessory_starts = accessory_vector, 
              satellite_starts = satellite_vector, 
              intercalary_starts = intercalary_vector,
              linked_starts = linked_vector,
              all_levels = implant_df$level
  )
  )
  
}


####### ROD FUNCTION

build_unilateral_rods_list_function <- function(accessory_rod_vector = c("a", "b"), 
                                                satellite_rods_vector = c("a", "b"),
                                                intercalary_rods_vector = c("a", "b"), 
                                                linked_rods_vector = c("a", "b"),
                                                revision_rods_retained_df = tibble(level = character(), vertebral_number = double(), x = double(), y = double()),
                                                unilateral_full_implant_df, 
                                                intercalary_rod_junction = "T12", 
                                                prior_rod_overlap_connectors = c(""), 
                                                rod_side = "left"){
  
  if(!is.null(unilateral_full_implant_df) && nrow(unilateral_full_implant_df)>0){
    rods_list <- list()
    connector_list <- list()
    all_levels_vector <- c("Occiput", "C1", "C1-C2", "C2", "C2-C3", "C3", "C3-C4", "C4", "C4-C5", "C5", "C5-C6", "C6", "C6-C7", "C7", "C7-T1", "T1", "T1-T2", "T2", "T2-T3", "T3", "T3-T4", "T4", "T4-T5", "T5", "T5-T6", "T6", "T6-T7", "T7", "T7-T8", "T8", "T8-T9", "T9", "T9-T10", "T10", "T10-T11", "T11", "T11-T12", "T12", "T12-L1", "L1", "L1-L2", "L2", "L2-L3", "L3", "L3-L4", "L4", "L4-L5", "L5", "L5-S1", "S1", "Iliac", "S2AI")
    
    if(nrow(revision_rods_retained_df) > 0){
      unilateral_full_implant_df <- revision_rods_retained_df %>% 
        filter(prior_rod_connected == "no") %>%
        select(level, vertebral_number, x, y) %>%
        bind_rows(unilateral_full_implant_df)%>%
        arrange(y)
      
      revision_implants_retained_df <- revision_rods_retained_df %>% 
        filter(prior_rod_connected == "yes")  

      # if(any(revision_implants_retained_df$old_rod_connected_to_new_rod == "yes")){
      #   if(nrow(revision_implants_retained_df)>0){
      #     if(min(unilateral_full_implant_df$y) > max(revision_implants_retained_df$y)){
      #       connector_matrix <- revision_implants_retained_df %>%
      #         select(x, y) %>%
      #         filter(y == max(y)) %>%
      #         union_all(unilateral_full_implant_df %>% select(x, y) %>% filter(y == min(y))) %>%
      #         as.matrix()
      # 
      #       connector_list$prior_rod_connector <- st_buffer(st_linestring(connector_matrix), dist = 0.0045, endCapStyle = "FLAT")
      # 
      #     }else{
      #       connector_matrix <- revision_implants_retained_df %>%
      #         select(x, y) %>%
      #         filter(y == min(y)) %>%
      #         union_all(unilateral_full_implant_df %>% select(x, y) %>% filter(y == max(y))) %>%
      #         as.matrix()
      # 
      #       connector_list$prior_rod_connector <- st_buffer(st_linestring(connector_matrix), dist = 0.0045, endCapStyle = "FLAT")
      #     }
      #   }
      # }
      
      if(length(prior_rod_overlap_connectors)>0){
        revision_rod_overlap <- all_implants_constructed_df %>%
          filter(level %in% prior_rod_overlap_connectors, 
                 object == "pedicle_screw", 
                 side == rod_side) %>%
          mutate(connector_count = row_number()) %>%
          select(connector_count, x, y) %>%
          mutate(y = y - 0.005)
        
        prior_rod_connector_matrix_list <-  map(.x = revision_rod_overlap$connector_count, .f =  ~ revision_rod_overlap %>%
                                        filter(connector_count == .x) %>%
                                        bind_rows(revision_rod_overlap %>%
                                                    filter(connector_count == .x) %>%
                                                    mutate(x = if_else(x < 0.5, x + 0.01, x - 0.01))) %>%
                                        select(x, y) %>%
                                        remove_missing() %>%
                                        as.matrix())
        
        if(length(prior_rod_connector_matrix_list) == 1){
          connector_list$connector_1 <-  st_buffer(st_linestring(prior_rod_connector_matrix_list[[1]]), dist = 0.0045, endCapStyle = "FLAT")
        } 
        if(length(prior_rod_connector_matrix_list) == 2){
          connector_list$connector_1 <-  st_buffer(st_linestring(prior_rod_connector_matrix_list[[1]]), dist = 0.0045, endCapStyle = "FLAT")
          connector_list$connector_2 <-  st_buffer(st_linestring(prior_rod_connector_matrix_list[[2]]), dist = 0.0045, endCapStyle = "FLAT")
        }
        if(length(prior_rod_connector_matrix_list) == 3){
          connector_list$connector_1 <-  st_buffer(st_linestring(prior_rod_connector_matrix_list[[1]]), dist = 0.0045, endCapStyle = "FLAT")
          connector_list$connector_2 <-  st_buffer(st_linestring(prior_rod_connector_matrix_list[[2]]), dist = 0.0045, endCapStyle = "FLAT")
          connector_list$connector_3 <-  st_buffer(st_linestring(prior_rod_connector_matrix_list[[3]]), dist = 0.0045, endCapStyle = "FLAT")
        }
        if(length(prior_rod_connector_matrix_list) == 4){
          connector_list$connector_1 <-  st_buffer(st_linestring(prior_rod_connector_matrix_list[[1]]), dist = 0.0045, endCapStyle = "FLAT")
          connector_list$connector_2 <-  st_buffer(st_linestring(prior_rod_connector_matrix_list[[2]]), dist = 0.0045, endCapStyle = "FLAT")
          connector_list$connector_3 <-  st_buffer(st_linestring(prior_rod_connector_matrix_list[[3]]), dist = 0.0045, endCapStyle = "FLAT")
          connector_list$connector_4 <-  st_buffer(st_linestring(prior_rod_connector_matrix_list[[4]]), dist = 0.0045, endCapStyle = "FLAT")
        }
      }
    
    }
    
    implant_levels_vector <- unilateral_full_implant_df$level
    
    if(length(prior_rod_overlap_connectors)>0){
      revision_rod_overlap <- all_implants_constructed_df %>%
        filter(level %in% prior_rod_overlap_connectors, 
               object == "pedicle_screw", 
               side == rod_side) %>%
        select(x, y) %>%
        mutate(y = y - 0.01)
      
    }else{
      revision_rod_overlap <- tibble(x = double(), 
                                     y = double())
    }
    

    if(nrow(unilateral_full_implant_df) >1){
      main_rod_df <- unilateral_full_implant_df %>%
        mutate(y = if_else(y == max(y), y + 0.005, y)) %>%
        mutate(y = if_else(y == min(y), y - 0.005, y)) 
      
      main_rod_matrix <- main_rod_df %>%
        select(x, y) %>%
        bind_rows(revision_rod_overlap) %>%
        arrange(rev(y)) %>%
        distinct() %>%
        remove_missing() %>%
        select(x, y) %>%
        as.matrix()
      
      rods_list$main_rod_sf <- st_buffer(st_linestring(main_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
    }
    
    if(satellite_rods_vector[1] %in% implant_levels_vector & satellite_rods_vector[2] %in% implant_levels_vector){
      # satellite_rods_vector_df <- tibble(level = satellite_rods_vector) %>%
      #   left_join(unilateral_full_implant_df) %>%
      satellite_rods_vector_df <- unilateral_full_implant_df %>%
        filter(level %in% satellite_rods_vector) %>%
        select(x, y) %>%
        arrange(rev(y)) %>%
        distinct()
      
      satellite_rod_matrix <- satellite_rods_vector_df %>%
        mutate(x = if_else(x < 0.5, x - 0.005, x + 0.005)) %>%
        mutate(y = if_else(y == max(y), y + 0.01, y)) %>%
        mutate(y = if_else(y == min(y), y - 0.01, y)) %>%
        remove_missing() %>%
        select(x, y) %>%
        as.matrix()
      
      top_connector_matrix <- satellite_rods_vector_df %>%
        mutate(x = if_else(x < 0.5, x - 0.005, x + 0.005)) %>%
        filter(y == max(y)) %>%
        remove_missing() %>%
        select(x, y) %>%
        as.matrix()
      
      bottom_connector_matrix <- satellite_rods_vector_df %>%
        mutate(x = if_else(x < 0.5, x - 0.005, x + 0.005)) %>%
        filter(y == min(y)) %>%
        remove_missing() %>%
        select(x, y) %>%
        as.matrix()
      
      connector_list$satellite_rod_top_connector <- st_buffer(st_point(top_connector_matrix), dist = 0.005, endCapStyle = "ROUND")
      connector_list$satellite_rod_bottom_connector <- st_buffer(st_point(bottom_connector_matrix), dist = 0.005, endCapStyle = "ROUND")
      
      rods_list$satellite_rod_sf <- st_buffer(st_linestring(satellite_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
      
      main_rod_df <- main_rod_df %>%
        anti_join(y = satellite_rods_vector_df) %>%
        mutate(x = if_else(x < 0.5, x + 0.003, x - 0.003))
      
      main_rod_matrix <- main_rod_df %>%
        select(x, y) %>%
        arrange(rev(y)) %>%
        distinct() %>%
        remove_missing() %>%
        select(x, y) %>%
        as.matrix()
      
      rods_list$main_rod_sf <- st_buffer(st_linestring(main_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
    }
    
    if(linked_rods_vector[1] %in% implant_levels_vector & linked_rods_vector[2] %in% implant_levels_vector){
      main_rod_df <- unilateral_full_implant_df %>%
        select(level, vertebral_number, x, y) %>%
        mutate(y = if_else(y == max(y), y + 0.005, y)) %>%
        mutate(y = if_else(y == min(y), y - 0.005, y)) 
      
      linked_rods_overlap_df <- tibble(level = linked_rods_vector) %>%
        left_join(main_rod_df)
      
      top_linked_rod_matrix <- main_rod_df %>%
        filter(y >= min(linked_rods_overlap_df$y)) %>%
        mutate(x = if_else(x < 0.5, x - 0.004, x + 0.004)) %>%
        remove_missing() %>%
        select(x, y) %>%
        as.matrix()
      
      bottom_linked_rod_matrix <- main_rod_df %>%
        filter(y <= max(linked_rods_overlap_df$y)) %>%
        mutate(x = if_else(x < 0.5, x + 0.004, x - 0.004)) %>% 
        remove_missing() %>%
        select(x, y) %>%
        as.matrix()
      
      top_left_connect_df <- linked_rods_overlap_df %>%
        filter(y == max(y)) %>%
        select(x, y) %>%
        distinct() %>%
        mutate(x = if_else(x < 0.5, x + 0.004, x - 0.004))
      
      top_connector_matrix <- linked_rods_overlap_df %>%
        filter(y == max(y)) %>%
        select(x, y) %>%
        distinct() %>%
        mutate(x = if_else(x < 0.5, x - 0.004, x + 0.004)) %>%
        bind_rows(top_left_connect_df) %>%
        select(x, y) %>%
        distinct() %>%
        as.matrix()
      
      
      bottom_left_connect_df <- linked_rods_overlap_df %>%
        filter(y == min(y)) %>%
        select(x, y) %>%
        distinct() %>%
        mutate(x = if_else(x < 0.5, x + 0.004, x - 0.004))
      
      bottom_connector_matrix <- linked_rods_overlap_df %>%
        filter(y == min(y)) %>%
        select(x, y) %>%
        distinct() %>%
        mutate(x = if_else(x < 0.5, x - 0.004, x + 0.004)) %>%
        bind_rows(bottom_left_connect_df) %>%
        select(x, y) %>%
        distinct() %>%
        as.matrix()
      
      
      connector_list$linked_rod_top_connector <- st_buffer(st_linestring(top_connector_matrix), dist = 0.003, endCapStyle = "ROUND")
      connector_list$linked_rod_bottom_connector <- st_buffer(st_linestring(bottom_connector_matrix), dist = 0.003, endCapStyle = "ROUND")
      
      rods_list$linked_top_rod_sf <- st_buffer(st_linestring(x = top_linked_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
      rods_list$main_rod_sf <- st_buffer(st_linestring(x = bottom_linked_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
    }
    
    
    if(intercalary_rods_vector[1] %in% implant_levels_vector & intercalary_rods_vector[2] %in% implant_levels_vector){
      intercalary_rod_df <- tibble(level = intercalary_rods_vector) %>%
        left_join(main_rod_df) %>%
        select(x, y) %>%
        arrange(rev(y)) %>%
        distinct() %>%
        mutate(x = if_else(x < 0.5, x - 0.01, x + 0.01))
      
      top_connector_matrix <- intercalary_rod_df %>%
        filter(y == max(y)) %>%
        mutate(x = if_else(x < 0.5, x + 0.012, x - 0.012)) %>%
        bind_rows(intercalary_rod_df %>%
                    filter(y == max(y))) %>%
        mutate(y = y - 0.01) %>%
        remove_missing() %>%
        select(x, y) %>%
        as.matrix()
      
      bottom_connector_matrix <- intercalary_rod_df %>%
        filter(y == min(y)) %>%
        mutate(x = if_else(x < 0.5, x + 0.012, x - 0.012)) %>%
        bind_rows(intercalary_rod_df %>%
                    filter(y == min(y))) %>%
        mutate(y = y + 0.01) %>%
        remove_missing() %>%
        select(x, y) %>%
        as.matrix()
      
      intercalary_rod_matrix <- intercalary_rod_df %>%
        select(x, y) %>%
        as.matrix()
      
      top_intercalary_rod_matrix <- main_rod_df %>%
        filter(vertebral_number < jh_get_vertebral_number_function(level_to_get_number = intercalary_rod_junction)) %>%
        remove_missing() %>%
        select(x, y) %>%
        mutate(y = if_else(y == min(y), y - 0.015, y)) %>%
        as.matrix()
      
      bottom_intercalary_rod_matrix <- main_rod_df %>%
        filter(vertebral_number >= jh_get_vertebral_number_function(level_to_get_number = intercalary_rod_junction)) %>%
        mutate(y = if_else(y == max(y), y + 0.015, y)) %>%
        remove_missing() %>%
        select(x, y) %>%
        as.matrix()
      
      connector_list$intercalary_rod_top_connector <- st_buffer(st_linestring(top_connector_matrix), dist = 0.003, endCapStyle = "ROUND")
      connector_list$intercalary_rod_bottom_connector <- st_buffer(st_linestring(bottom_connector_matrix), dist = 0.003, endCapStyle = "ROUND")
      
      
      rods_list$intercalary_rod_sf <- st_buffer(st_linestring(intercalary_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
      rods_list$intercalary_top_rod_sf <- st_buffer(st_linestring(top_intercalary_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
      rods_list$main_rod_sf <- st_buffer(st_linestring(bottom_intercalary_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
      
    }
    
    if(accessory_rod_vector[1] %in% implant_levels_vector & accessory_rod_vector[2] %in% implant_levels_vector){
      accessory_rod_df <- tibble(level = accessory_rod_vector) %>%
        left_join(unilateral_full_implant_df) %>%
        select(x, y) %>%
        arrange(rev(y)) %>%
        distinct() %>%
        mutate(x = if_else(x < 0.5, x + 0.01, x - 0.01))
      
      accessory_rod_connections_df <- tibble(level = accessory_rod_vector) %>%
        left_join(unilateral_full_implant_df) %>%
        select(x, y) %>%
        arrange(rev(y)) %>%
        distinct()
      
      top_connector_matrix <- accessory_rod_connections_df %>%
        bind_rows(accessory_rod_df) %>%
        filter(y == max(y)) %>%
        mutate(y = y - 0.01) %>%
        remove_missing() %>%
        select(x, y) %>%
        as.matrix()
      
      bottom_connector_matrix <- accessory_rod_connections_df %>%
        bind_rows(accessory_rod_df) %>%
        filter(y == min(y)) %>%
        mutate(y = y + 0.01) %>%
        remove_missing() %>%
        select(x, y) %>%
        as.matrix()
      
      accessory_rod_matrix <- accessory_rod_df %>%
        remove_missing() %>%
        select(x, y) %>%
        as.matrix()
      
      rods_list$accessory_rod <- st_buffer(st_linestring(accessory_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
      
      connector_list$accessory_rod_top_connector <- st_buffer(st_linestring(top_connector_matrix), dist = 0.003, endCapStyle = "ROUND")
      connector_list$accessory_rod_bottom_connector <- st_buffer(st_linestring(bottom_connector_matrix), dist = 0.003, endCapStyle = "ROUND")
      
    }
    return(list(rod_list = rods_list, 
                connector_list = connector_list))
  }else{
    list(rod_list = NULL, 
         connector_list = NULL)
  }
  
}











