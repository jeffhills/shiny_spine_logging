#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############


#############-----------------------   LOAD DATA  ----------------------###############


spine_png <- image_read(path = "spine_posterior.png", density = 5)

posterior_spine_plot <- ggdraw() +
  draw_image(
    spine_png,
    scale = 1,
    y = 0,
    valign = 0,
    x = 0,
    height = 1
    # width = 1
  ) 

anterior_spine_jpg <- image_read(path = "spine_anterior.jpg")
 
implant_starts_df <- fread(file = "full_coordinates_df.csv ") %>%
  filter(!is.na(x)) 

#############-----------------------   Build Key Dataframes  ----------------------###############

left_label_x <- 0.32
right_label_x <- 1 - left_label_x

labels_df <- levels_numbered_df %>%
  filter(str_detect(level, pattern = "-") == FALSE) %>%
  mutate(y = c(0.925, 0.88, 0.865, 0.847, 0.828, 0.811, 0.793, 0.779, 0.757, 0.733, 0.711, 0.682, 0.653, 0.623, 0.593, 0.561, 0.53, 0.491, 0.457, 0.421, 0.385, 0.353, 0.311, 0.271, 0.238, 0.21, 0.19, 0.17))

labels_anterior_df <- implant_starts_df %>%
  filter(object == "corpectomy") %>%
  select(level, x, y) %>%
  distinct()  %>%
  mutate(y = case_when(
    level == "C3" ~ y + 0.005,
    level == "C4" ~ y + 0.01,
    level == "C5" ~ y + 0.01,
    level == "C6" ~ y + 0.01,
    level == "C7" ~ y + 0.01,
    level == "T1" ~ y + 0.01,
    level == "T2" ~ y + 0.01,
    level == "T3" ~ y + 0.01,
    level == "T4" ~ y + 0.01,
    level == "T5" ~ y + 0.01,
    TRUE ~ y
  ))


interbody_levels_df <- levels_numbered_df %>%
  filter(str_detect(level, pattern = "-"))

all_objects_y_range_df <- implant_starts_df %>%
  select(object, y) %>%
  distinct()

# To move all objects down a certain amount, if creating L6 vertebrae.
# move_polygon_sf <- function(object_vector_to_move, row_number_to_move, y_to_move){
#   st_polygon(list(as_tibble(object_vector_to_move[[row_number_to_move]][[1]], .name_repair = make_clean_names) %>%
#                     rename(y = x_2) %>%
#                     mutate(y = y - y_to_move) %>%
#                     as.matrix()))
# }
# 
# lower_vert_df$lowered <- map2(.x = nrow(lower_vert_df), .y = 0.02, .f = ~ move_polygon_sf(object_vector_to_move = lower_vert_df$object_constructed, row_number_to_move = .x, y_to_move = .y))


#############-------#############-----------------------   POSTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   POSTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   POSTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   POSTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   POSTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   POSTERIOR  ----------------------###############-------###############

#############-----------------------   Build Revision Implants  ----------------------###############
revision_implants_df <- implant_starts_df %>%
  filter(category == "implant") %>%
  filter(object == "pedicle_screw" | str_detect(object, "pelvic_screw") | object == "occipital_screw" | object == "lateral_mass_screw" | object == "pars_screw" | str_detect(object, "hook")) %>%
  filter(approach == "posterior") %>%
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
                                                                                                     y_inferior = ..10)))%>%
  arrange(vertebral_number) %>%
  distinct() %>%
  # group_by(level, side) %>%
  group_by(level, object, side) %>%
  filter(y == max(y)) %>%
  ungroup() %>%
  select(-ends_with("_x"), -ends_with("_y"))

revision_screws_df <- revision_implants_df %>%
  filter(str_detect(object, "hook") == FALSE) %>%
  filter((str_detect(string = level, pattern = "C1|C3|C4|C5|C6") & object == "pedicle_screw") == FALSE) %>%
  group_by(level, side) %>%
  filter(y == max(y)) %>%
  ungroup() %>%
  union_all(revision_implants_df %>% filter(level == "Iliac"))%>%
  filter(object != "pelvic_screw_2") 


#############-----------------------   Build Implants  ----------------------###############
construct_objects_live_function <- function(all_procedures_selected_df){
  
  assembled_df <- implant_starts_df %>%
    filter(level == "xx")
  
  if(nrow(all_procedures_selected_df) > 0){
    
    if(any(all_procedures_selected_df$category == "implant")){
      assembled_df <- assembled_df %>%
        union_all(all_procedures_selected_df %>%
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
                                                                                                                       y_inferior = ..10))))
    }
    
    if(any(all_procedures_selected_df$category == "osteotomy")){
      #############-----------------------   Build Osteotomies  ----------------------###############
      
      assembled_df <- assembled_df %>%
        union_all(all_procedures_selected_df %>%
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
                                                                                                    inferior_facet_inferior_border_y = ..18))))
    }
    
    posterior_df <- all_procedures_selected_df %>%
      filter(approach == "posterior")
    
    #############-----------------------   Build Decompressions  ----------------------###############
    if(any(posterior_df$category == "decompression" | posterior_df$category == "tumor")){
      assembled_df <- assembled_df %>%
        union_all(posterior_df %>%
                    filter(category == "decompression" | category == "tumor") %>%
                    mutate(object_constructed = pmap(list(..1 = left_x,
                                                          ..2 = superior_y,
                                                          ..3 = right_x,
                                                          ..4 = inferior_y,
                                                          ..5 = width, 
                                                          ..6 = object,
                                                          ..7 = lateral_pars_x,
                                                          ..8 = superior_tp_y,
                                                          ..9 = side, 
                                                          ..10 = inferior_pedicle_y,
                                                          ..11 = inferior_facet_superior_border_y), 
                                                     .f = ~ build_decompression_function(left_x = ..1,
                                                                                         superior_y = ..2,
                                                                                         right_x = ..3, 
                                                                                         inferior_y = ..4, 
                                                                                         top_width = ..5, 
                                                                                         object = ..6, 
                                                                                         x_lateral_pars = ..7, 
                                                                                         y_inferior_tp = ..8,
                                                                                         side = ..9, 
                                                                                         inferior_pedicle_y = ..10,
                                                                                         inferior_facet_superior_border_y = ..11))))
      
    }
    
    if(any(posterior_df$category == "incision_drainage")){
      
      assembled_df <- assembled_df %>%
        union_all(all_procedures_selected_df %>%
                    filter(approach == "posterior") %>%
                    filter(category == "incision_drainage") %>%
                    mutate(object_constructed = pmap(list(..1 = left_x,
                                                          ..2 = superior_y,
                                                          ..3 = right_x,
                                                          ..4 = inferior_y,
                                                          ..5 = width, 
                                                          ..6 = object,
                                                          ..7 = lateral_pars_x,
                                                          ..8 = superior_tp_y,
                                                          ..9 = side, 
                                                          ..10 = inferior_pedicle_y,
                                                          ..11 = inferior_facet_superior_border_y), 
                                                     .f = ~ build_incision_drainage_function(left_x = ..1,
                                                                                             superior_y = ..2,
                                                                                             right_x = ..3, 
                                                                                             inferior_y = ..4, 
                                                                                             top_width = ..5, 
                                                                                             object = ..6, 
                                                                                             x_lateral_pars = ..7, 
                                                                                             y_inferior_tp = ..8,
                                                                                             side = ..9, 
                                                                                             inferior_pedicle_y = ..10,
                                                                                             inferior_facet_superior_border_y = ..11))))
    }
    
    #############-----------------------   Build Interbody Fusions  ----------------------###############
    if(any(posterior_df$category == "interbody")){
      
      assembled_df <- assembled_df %>%
        union_all(all_procedures_selected_df %>%
                    filter(category == "interbody") %>%
                    filter(approach == "posterior") %>%
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
                                                                                     inferior_facet_inferior_border_y = ..11))))
    }
    
    
    #############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
    #############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
    #############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
    #############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
    #############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
    #############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
    anterior_df <- all_procedures_selected_df %>%
      filter(approach == "anterior") %>%
      remove_empty()

    if(nrow(anterior_df) > 0){
      assembled_df <- assembled_df %>%
        union_all(anterior_df %>%
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
                                                                                      inferior_endplate_superior_body_y = ..6, y_click = ..7, direction = ..8))))
    }



    #############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
    #############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
    #############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
    #############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
    #############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
    #############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
    
    
    objects_constructed_df <- assembled_df%>%
      select(level, vertebral_number, everything())%>%
      select(-ends_with("_x"), -ends_with("_y"))
    
    return(objects_constructed_df)
    
  }
}

implants_constructed_df <- implant_starts_df %>%
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

osteotomy_df <- implant_starts_df %>%
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

decompression_df <- implant_starts_df %>%
  filter(approach == "posterior") %>%
  filter(category == "decompression" | category == "tumor") %>%
  mutate(object_constructed = pmap(list(..1 = left_x,
                                        ..2 = superior_y,
                                        ..3 = right_x,
                                        ..4 = inferior_y,
                                        ..5 = width,
                                        ..6 = object,
                                        ..7 = lateral_pars_x,
                                        ..8 = superior_tp_y,
                                        ..9 = side, 
                                        ..10 = inferior_pedicle_y,
                                        ..11 = inferior_facet_superior_border_y),
                                   .f = ~ build_decompression_function(left_x = ..1,
                                                                       superior_y = ..2,
                                                                       right_x = ..3,
                                                                       inferior_y = ..4,
                                                                       top_width = ..5,
                                                                       object = ..6,
                                                                       x_lateral_pars = ..7,
                                                                       y_inferior_tp = ..8,
                                                                       side = ..9,
                                                                       inferior_pedicle_y = ..10,
                                                                       inferior_facet_superior_border_y = ..11)))



incision_drainage_df <- implant_starts_df %>%
  filter(approach == "posterior") %>%
  filter(category == "incision_drainage") %>%
  mutate(object_constructed = pmap(list(..1 = left_x,
                                        ..2 = superior_y,
                                        ..3 = right_x,
                                        ..4 = inferior_y,
                                        ..5 = width,
                                        ..6 = object,
                                        ..7 = lateral_pars_x,
                                        ..8 = superior_tp_y,
                                        ..9 = side,
                                        ..10 = inferior_pedicle_y,
                                        ..11 = inferior_facet_superior_border_y),
                                   .f = ~ build_incision_drainage_function(left_x = ..1,
                                                                           superior_y = ..2,
                                                                           right_x = ..3,
                                                                           inferior_y = ..4,
                                                                           top_width = ..5,
                                                                           object = ..6,
                                                                           x_lateral_pars = ..7,
                                                                           y_inferior_tp = ..8,
                                                                           side = ..9,
                                                                           inferior_pedicle_y = ..10,
                                                                           inferior_facet_superior_border_y = ..11)))


#############-----------------------   Build Open Canal df  ----------------------###############

open_canal_df <- decompression_df %>%
  filter(object == "laminectomy") %>%
  mutate(category = "revision")

#############-----------------------   Build Interbody Fusions  ----------------------###############


all_interbody_df <- implant_starts_df %>%
  filter(category == "interbody") %>%
  filter(approach == "posterior") %>%
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


# #############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
# #############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
# #############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
# #############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
# #############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
# #############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
# 
anterior_df <- implant_starts_df %>%
  filter(approach == "anterior") %>%
  remove_empty(which = c("rows", "cols"))


anterior_objects_df <- anterior_df %>%
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
                                                                    inferior_endplate_superior_body_y = ..6,
                                                                    y_click = ..7,
                                                                    direction = ..8)))



#############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
#############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
#############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
#############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
#############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
#############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############

all_implants_constructed_df <- implants_constructed_df %>%
  bind_rows(osteotomy_df) %>%
  bind_rows(decompression_df) %>%
  bind_rows(incision_drainage_df) %>%
  bind_rows(anterior_objects_df) %>%
  bind_rows(all_interbody_df) %>%
  select(level, vertebral_number, body_interspace, approach, category, implant, object, side, x, y, fusion, interbody_fusion, fixation_uiv_liv, direction, object_constructed) %>%
  group_by(level, side, object) %>%
  mutate(object_id = paste0(level, "_", side, "_", object, "_", row_number())) %>%
  ungroup() %>%
  mutate(level = if_else(str_detect(level, "Iliac|S2AI") & object == "pelvic_screw_2", paste0(level, "_2"), level)) %>%
  mutate(vertebral_number = if_else(str_detect(level, "Iliac|S2AI") & object == "pelvic_screw_2", vertebral_number + 0.5, vertebral_number)) 


all_objects_y_range_df <- all_implants_constructed_df %>%
  select(object, y) %>%
  distinct()

implant_levels_numbered_df <- all_implants_constructed_df %>%
  filter(implant == "yes") %>%
  select(level, vertebral_number) %>%
  distinct() %>%
  arrange(vertebral_number)

all_screw_coordinates_df <- all_implants_constructed_df %>%
  filter(str_detect(object, "lateral_mass_screw|occipital_screw"), str_detect(level, "Occiput|C1")) %>%
  select(level, vertebral_number, side, x, y) %>%
  distinct() %>%
  union_all(
    all_implants_constructed_df %>%
      filter(str_detect(object, "pedicle_screw|pelvic_screw")) %>%
      select(level, vertebral_number, side, x, y) %>%
      distinct()
  ) %>%
  arrange(vertebral_number, rev(y))

all_screw_coordinates_df <- all_screw_coordinates_df %>%
  mutate(level = map(.x = level, .f = ~ jh_get_cranial_caudal_interspace_body_list_function(level = .x)$caudal_interspace)) %>%
  unnest(level) %>%
  filter(!is.na(level)) %>%
  mutate(vertebral_number = map(.x = level, .f = ~ jh_get_vertebral_number_function(.x))) %>%
  unnest(vertebral_number) %>%
  union_all(all_screw_coordinates_df) %>%
  arrange(vertebral_number) %>%
  filter(str_detect(level, " 2") == FALSE) %>%
  distinct()

all_cages_df <- all_implants_constructed_df %>%
  filter(
      object == "anterior_interbody_implant" |
      object == "tlif" |
      object == "plif" |
      object == "llif" |
      object == "anterior_disc_arthroplasty" |
      object == "corpectomy_cage") %>%
  select(level, side, vertebral_number, object, approach) %>%
  # union_all(intervertebral_cage_df) %>%
  distinct() %>%
  arrange(vertebral_number) %>%
  mutate(cage_id = paste(str_to_lower(str_replace_all(level, "-", "_")), side, object, sep = "_")) 

# all_implants_constructed_df

rm(osteotomy_df,
   decompression_df,
   incision_drainage_df,
   anterior_objects_df,
   all_interbody_df,
   # all_points_all_implants_constructed_df,
   implants_constructed_df)

