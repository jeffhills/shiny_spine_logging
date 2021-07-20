#############-----------------------   Short Shiny Functions  ----------------------###############

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


#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############


#############-----------------------   LOAD DATA  ----------------------###############

# spine_png <- image_read(path = "posterior_spine_figure.png")
l6_spine_png <- image_read(path = "posterior_spine_6_lumbar_vert.png")

# spine_png <- image_read(path = "posterior_spine_6_lumbar_vert.png")
# posterior_spine_6_lumbar_vert
l6_anterior_spine_png <- image_read(path = "anterior_spine_6_lumbar_vert.png")

l6_implant_starts_lower_only_df <- read_csv(file = "full_coordinates_df.csv") %>%
  filter(!is.na(x)) %>%
  filter(vertebral_number > 23.9)

l5_to_l6_shifted_down_df <-l6_implant_starts_lower_only_df %>%
  mutate(level = str_replace_all(string = level, pattern = "L5", replacement = "L6")) %>%
  mutate(vertebral_number = vertebral_number + 1) %>%
  mutate_at(.vars = vars("y", 
                         "superior_y", 
                         "inferior_y", 
                         "superior_vert_inferior_pedicle_y", 
                         "superior_lamina_y",
                         "inferior_lamina_y", 
                         "inferior_pedicle_y",
                         "body_center_y", 
                         "superior_tp_y", 
                         "inferior_tp_y", 
                         "inferior_facet_superior_border_y",
                         "inferior_facet_inferior_border_y",
                         "inferior_endplate_y",
                         "superior_endplate_y", 
                         "superior_endplate_inferior_body_y",
                         "inferior_endplate_superior_body_y"), .funs = ~ .x - 0.04)


l6_implant_starts_df <- l6_implant_starts_lower_only_df %>%
  filter(vertebral_number < 24.7) %>%
  mutate(level = str_replace_all(string = level, pattern = "S1", replacement = "L6")) %>%
  union_all(l5_to_l6_shifted_down_df) %>%
  # union_all(l6_implant_starts_unchanged_df) %>%
  arrange(vertebral_number) 

# l6_implant_starts_df <- s1_to_l6_changed_df %>%
#   union_all(shifted_down_df)

#############-----------------------   Build Key Dataframes  ----------------------###############

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


#############-------#############-----------------------   POSTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   POSTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   POSTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   POSTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   POSTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   POSTERIOR  ----------------------###############-------###############

#############-----------------------   Build Implants  ----------------------###############

l6_implants_constructed_df <- l6_implant_starts_df %>%
  filter(category == "implant") %>%
  mutate(sublaminar_band_length = length) %>%
  mutate(length_for_tether = length) %>%
  mutate(object_constructed = pmap(.l =  list(..1 = object, 
                                              ..2 = x, 
                                              ..3 = y, 
                                              ..4 = angle,
                                              ..5 = length, 
                                              ..6 = width,
                                              ..7 = sublaminar_band_length,
                                              ..8 = length_for_tether, 
                                              ..9 = superior_y, 
                                              ..10 = inferior_y), .f = ~ screw_hook_implant_function(implant_type = ..1, 
                                                                                                     start_x = ..2,
                                                                                                     y = ..3,
                                                                                                     angle = ..4,
                                                                                                     screw_length_mod = ..5,
                                                                                                     screw_width_mod = ..6, 
                                                                                                     sublaminar_band_length = ..7, 
                                                                                                     length_for_tether = ..8, 
                                                                                                     y_superior = ..9, 
                                                                                                     y_inferior = ..10)))


#############-----------------------   Build Osteotomies  ----------------------###############

l6_osteotomy_df <- l6_implant_starts_df %>%
  filter(category == "osteotomy") %>%
  # remove_empty() %>%
  mutate(object_constructed = pmap(list(..1 = level, 
                                        ..2 = object,
                                        ..3 = left_x,
                                        ..4 = superior_y,
                                        ..5 = right_x,
                                        ..6 = inferior_y,
                                        ..7 = superior_vert_lateral_pars_x, 
                                        ..8 = superior_vert_inferior_pedicle_y,
                                        ..9 = superior_lamina_y,
                                        ..10 = lateral_tp_x, 
                                        ..11 = inferior_lamina_y,
                                        ..12 = lateral_pars_x,
                                        ..13 = inferior_pedicle_y,
                                        ..14 = superior_tp_y, 
                                        ..15 = inferior_facet_lateral_border_x,
                                        ..16 = inferior_facet_medial_border_x,
                                        ..17 = inferior_facet_superior_border_y,
                                        ..18 = inferior_facet_inferior_border_y,
                                        ..19 = x), .f = ~build_osteotomy_function(level = ..1,
                                                                                  x_click = ..19,
                                                                                  osteotomy_grade = ..2,
                                                                                  left_x = ..3,
                                                                                  superior_y = ..4,
                                                                                  right_x = ..5, 
                                                                                  inferior_y = ..6,
                                                                                  superior_vert_lateral_pars_x = ..7, 
                                                                                  superior_vert_inferior_pedicle_y = ..8, 
                                                                                  superior_lamina_y = ..9,
                                                                                  lateral_tp_x = ..10, 
                                                                                  inferior_lamina_y = ..11, 
                                                                                  lateral_pars_x = ..12, 
                                                                                  inferior_pedicle_y = ..13, 
                                                                                  superior_tp_y = ..14,
                                                                                  inferior_facet_lateral_border_x = ..15,
                                                                                  inferior_facet_medial_border_x = ..16,
                                                                                  inferior_facet_superior_border_y = ..17, 
                                                                                  inferior_facet_inferior_border_y = ..18)))

#############-----------------------   Build Decompressions  ----------------------###############

l6_decompression_df <- l6_implant_starts_df %>%
  filter(approach == "posterior") %>%
  filter(category == "decompression") %>%
  mutate(object_constructed = pmap(list(..1 = left_x,
                                        ..2 = superior_y,
                                        ..3 = right_x,
                                        ..4 = inferior_y,
                                        ..5 = width, 
                                        ..6 = object,
                                        ..7 = lateral_pars_x,
                                        ..8 = superior_tp_y,
                                        ..9 = side, 
                                        ..10 = inferior_pedicle_y), 
                                   .f = ~ build_decompression_function(left_x = ..1, right_x = ..3, superior_y = ..2, inferior_y = ..4, top_width = ..5, object = ..6, x_lateral_pars = ..7, y_inferior_tp = ..8, side = ..9, inferior_pedicle_y = ..10)))

#############-----------------------   Build Open Canal df  ----------------------###############

l6_open_canal_df <- l6_decompression_df %>%
  filter(object == "laminectomy") %>%
  mutate(category = "revision")

#############-----------------------   Build Interbody Fusions  ----------------------###############

l6_interbody_df <- l6_implant_starts_df %>%
  filter(category == "interbody") %>%
  filter(approach == "posterior") %>%
  filter(!is.na(width)) %>%
  replace_na(list(superior_endplate_y = 0, inferior_endplate_y = 0, inferior_facet_lateral_border_x = 0, inferior_facet_medial_border_x = 0, inferior_facet_superior_border_y = 0, inferior_facet_inferior_border_y = 0)) %>%
  mutate(object_constructed = pmap(list(..1 = object,
                                        ..2 = x,
                                        ..3 = right_x,
                                        ..4 = left_x,
                                        ..5 = superior_endplate_y,
                                        ..6 = inferior_endplate_y, 
                                        ..7 = width, 
                                        ..8 = inferior_facet_lateral_border_x,
                                        ..9 = inferior_facet_medial_border_x, 
                                        ..10 = inferior_facet_superior_border_y,
                                        ..11 = inferior_facet_inferior_border_y), 
                                   .f = ~ build_interbody_function(object = ..1, 
                                                                   x_click = ..2, 
                                                                   left_x = ..3, 
                                                                   right_x = ..4,
                                                                   y_superior_endplate = ..5,
                                                                   y_inferior_endplate = ..6,
                                                                   top_width = ..7,
                                                                   inferior_facet_lateral_border_x = ..8, 
                                                                   inferior_facet_medial_border_x = ..9,
                                                                   inferior_facet_superior_border_y = ..10,
                                                                   inferior_facet_inferior_border_y = ..11)))


l6_llif_interbody_df <- l6_interbody_df %>%
  filter(object == "tlif") %>%
  mutate(object = "llif")

l6_plif_interbody_df <- l6_interbody_df %>%
  filter(object == "tlif") %>%
  mutate(object = "plif") 

l6_all_interbody_df <- l6_interbody_df %>%
  union_all(l6_llif_interbody_df) %>%
  union_all(l6_plif_interbody_df) %>%
  distinct()

rm(l6_llif_interbody_df, l6_interbody_df, l6_plif_interbody_df)

#############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############

l6_anterior_df <- l6_implant_starts_df %>%
  filter(str_detect(string = category, pattern = "anterior")) 

l6_labels_anterior_df <- l6_anterior_df %>%
  filter(object == "corpectomy") %>%
  select(level, x, y) %>%
  distinct() 

l6_anterior_objects_df <- l6_anterior_df %>%
  mutate(object_constructed = pmap(list(..1 = object,
                                        ..2 = width,
                                        ..3 = inferior_endplate_y,
                                        ..4 = superior_endplate_y, 
                                        ..5 = superior_endplate_inferior_body_y, 
                                        ..6 = inferior_endplate_superior_body_y,
                                        ..7 = y,
                                        ..8 = direction),
                                   .f = ~ anterior_implant_function(object_type = ..1, 
                                                                    body_width = ..2,
                                                                    inferior_endplate_y = ..3,
                                                                    superior_endplate_y = ..4,
                                                                    superior_endplate_inferior_body_y = ..5, 
                                                                    inferior_endplate_superior_body_y = ..6, y_click = ..7, direction = ..8)))



#############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
#############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
#############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
#############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
#############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
#############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############

l6_all_points_all_implants_constructed_df <- l6_implants_constructed_df %>%
  union_all(l6_osteotomy_df) %>%
  union_all(l6_decompression_df) %>%
  union_all(l6_anterior_objects_df) %>% 
  union_all(l6_all_interbody_df) %>%
  select(level, vertebral_number, everything())

l6_revision_implants_df <- l6_all_points_all_implants_constructed_df %>%
  filter(object == "pedicle_screw" | object == "pelvic_screw" | object == "occipital_screw") %>%
  filter(approach == "posterior") %>%
  arrange(vertebral_number) %>%
  distinct() %>%
  group_by(level, object, side) %>%
  filter(y == max(y)) %>%
  ungroup()

l6_all_implants_constructed_df <- l6_all_points_all_implants_constructed_df %>%
  select(-ends_with("_x"), -ends_with("_y"))
  # select(level, body_interspace, vertebral_number, approach, category,implant, object, side, x, y, fusion,interbody_fusion, direction, fixation_uiv_liv, object_constructed)

rm(l6_osteotomy_df, l6_decompression_df, l6_all_interbody_df)

