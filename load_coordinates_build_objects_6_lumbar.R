l6_levels_vector <- c('Occiput', 'O-C1', 'C1', 'C1-C2', 'C2', 'C2-C3', 'C3', 'C3-C4', 'C4', 'C4-C5', 'C5', 'C5-C6', 'C6', 'C6-C7', 'C7', 'C7-T1', 'T1', 'T1-T2', 'T2', 'T2-T3', 'T3', 'T3-T4', 'T4', 'T4-T5', 'T5', 'T5-T6', 'T6', 'T6-T7', 'T7', 'T7-T8', 'T8', 'T8-T9', 'T9', 'T9-T10', 'T10', 'T10-T11', 'T11', 'T11-T12', 'T12', 'T12-L1', 'L1', 'L1-L2', 'L2', 'L2-L3', 'L3', 'L3-L4', 'L4', 'L4-L5', 'L5', "L5-L6", "L6", 'L6-S1', 'S1', 'Sacro-iliac', 'Iliac', 'S2AI')

l6_vertebral_numbers_vector <- c(0,0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5, 11, 11.5, 12, 12.5, 13, 13.5, 14, 14.5, 15, 15.5, 16, 16.5, 17, 17.5, 18, 18.5, 19, 19.5, 20, 20.5, 21, 21.5, 22, 22.5, 23, 23.5, 24, 24.5, 25, 25.5, 26, 26.5, 26, 27)

l6_levels_numbered_df <- tibble(level = l6_levels_vector, vertebral_number = l6_vertebral_numbers_vector)

l6_jh_get_vertebral_number_function <- function(level_to_get_number){
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
      level_to_get_number == 'L5-L6' ~ 24.5,
      level_to_get_number == 'L6' ~ 25,
      level_to_get_number == 'L6-S1' ~ 25.5,
      level_to_get_number == 'S1' ~ 26,
      level_to_get_number == "Sacro-iliac" ~ 26.5,
      level_to_get_number == 'Iliac' ~ 27,
      level_to_get_number == 'S2AI' ~ 28
    )
  return(vert_number)
}

l6_jh_get_vertebral_level_function <- function(number) {
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
    number == 24.5 ~ 'L5-L6',
    number == 25 ~ 'L6',
    number == 25.5 ~ 'L6-S1',
    number == 26 ~ 'S1',
    number == 26.5 ~ "Sacro-iliac",
    number == 27 ~ 'Iliac',
    number == 28 ~ 'S2AI'
  )
  return(level)
}


left_label_x <- 0.32
right_label_x <- 1 - left_label_x

l6_labels_df <- l6_levels_numbered_df %>%
  filter(str_detect(level, pattern = "-") == FALSE) %>%
  mutate(y = c(0.925,
               0.88,
               0.865,
               0.847,
               0.828,
               0.811,
               0.793,
               0.779,
               0.757, 
               0.733,
               0.711, 
               0.682, 
               0.653,
               0.623,
               0.593,
               0.561,
               0.53, 
               0.491,
               0.457,
               0.421,
               0.385, 
               0.353, 
               0.311,
               0.271,
               0.238,
               0.20, 
               .175, 
               0.15, 
               .13))

l6_interbody_levels_df <- l6_levels_numbered_df %>%
  filter(str_detect(level, pattern = "-"))


#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############


#############-----------------------   LOAD DATA  ----------------------###############

l6_spine_png <- image_read(path = "spine_posterior_6_lumbar_vert.png")

posterior_spine_plot_l6 <- ggdraw() +
  draw_image(
    l6_spine_png,
    scale = 1,
    y = 0,
    valign = 0,
    x = 0,
    height = 1
    # width = 1
  ) 

l6_anterior_spine_png <- image_read(path = "spine_anterior_6_lumbar_vert.png")


jh_change_object_df_to_l6_function <- function(all_objects_input_df = tibble(level = character())){
  l5_to_l6_shifted_down_df <- all_object_ids_df %>%
    select(-x, -y) %>%
    left_join(imported_coordinates) %>%
    filter(vertebral_number > 23.9) %>%
    mutate(level = str_replace_all(string = level, pattern = "L5", replacement = "L6")) %>%
    mutate(vertebral_number = vertebral_number + 1) %>%
    mutate(y = y - 0.04) %>%
    mutate(level = str_replace_all(string = object_id, pattern = "L5", replacement = "L6")) 
  
  
  
  l5_l6_matrices_df <- all_object_ids_df %>%
    select(-x, -y) %>%
    left_join(imported_coordinates) %>%
    filter(vertebral_number > 23.9) %>%
    filter(vertebral_number < 24.7) %>%
    mutate(level = str_replace_all(string = level, pattern = "S1", replacement = "L6")) %>%
    mutate(level = str_replace_all(string = object_id, pattern = "S1", replacement = "L6")) %>%
    union_all(l5_to_l6_shifted_down_df) 
  
  l6_implants_constructed_df <- l5_l6_matrices_df %>%
    filter(str_detect(object_id, "arthroplasty") == FALSE) %>%
    select(object_id, x, y) %>%
    group_by(object_id) %>%
    # filter(is.na(y)) %>%
    nest() %>%
    mutate(object_constructed = map(.x = data, .f = ~ st_linestring(as.matrix(.x)))) %>%
    select(object_id, object_constructed)
  
  l5_to_l6_shifted_down_df_for_linking <- all_object_ids_df %>%
    filter(vertebral_number > 23.9) %>%
    mutate(level = str_replace_all(string = level, pattern = "L5", replacement = "L6")) %>%
    mutate(vertebral_number = vertebral_number + 1) %>%
    mutate(y = y - 0.04) %>%
    mutate(level = str_replace_all(string = object_id, pattern = "L5", replacement = "L6")) 
  
  
  l5_l6_for_linking <- all_object_ids_df %>%
    # select(-x, -y) %>%
    # left_join(imported_coordinates) %>%
    filter(vertebral_number > 23.9) %>%
    filter(vertebral_number < 24.7) %>%
    mutate(level = str_replace_all(string = level, pattern = "S1", replacement = "L6")) %>%
    mutate(level = str_replace_all(string = object_id, pattern = "S1", replacement = "L6")) %>%
    union_all(l5_to_l6_shifted_down_df_for_linking) 
  
  
  l6_implants_constructed_df <- l5_l6_for_linking %>%
    distinct() %>%
    filter(str_detect(object_id, "arthroplasty") == FALSE) %>%
    left_join(l6_implants_constructed_df)
  
  l6_all_implants_constructed_df <- all_objects_input_df %>%
    filter(vertebral_number < 23.9) %>%
    bind_rows(l6_implants_constructed_df)
  
  l6_revision_implants_df <- l6_all_implants_constructed_df %>%
    filter(object == "pedicle_screw" | object == "pelvic_screw" | object == "occipital_screw") %>%
    filter(approach == "posterior") %>%
    arrange(vertebral_number) %>%
    distinct() %>%
    group_by(level, object, side) %>%
    filter(y == max(y)) %>%
    ungroup()%>%
    remove_empty(which = c("rows", "cols"))
  
  return(list(l6_implants_constructed_df = l6_all_implants_constructed_df, 
              l6_revision_implants_df = l6_revision_implants_df))
  
}


