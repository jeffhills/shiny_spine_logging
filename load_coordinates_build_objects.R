#############-----------------------   xxxxxxxxxxxx  ----------------------###############

# rcon <- redcapConnection(url = 'https://redcap.wustl.edu/redcap/api/', token = "4B2AA28A4D6EFBC6CA1F49EC0FB385F7")

#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############


#############-----------------------   LOAD DATA  ----------------------###############
spine_icd10_codes_df <- read_csv(file = "spine_icd_codes.csv")%>%
  mutate(spine_category = if_else(spine_category == "Deformity" & str_detect(diagnosis, pattern = "Infant|Juveni|Adolescent"), "Pediatric Deformity", spine_category)) %>%
  mutate(category_number = if_else(spine_category == "Pediatric Deformity", 1.5, category_number)) %>% 
  select(spine_category, category_number, site, diagnosis, icd10_code, site_number)

spine_categories_vector <- unique(spine_icd10_codes_df$spine_category)

create_spine_icd_list_by_site_list_by_category_function <- function(spine_region, category){
  dx_df <- spine_icd10_codes_df %>%
    filter(site == spine_region) %>%
    filter(spine_category == category)  %>%
    select(diagnosis) 
  
  dx_df$diagnosis
}

lumbar_list <- map(.x = spine_categories_vector, .f = ~create_spine_icd_list_by_site_list_by_category_function(spine_region = "lumbar", category = .x))
names(lumbar_list) <- spine_categories_vector

####
thoracolumbar_list <- map(.x = spine_categories_vector, .f = ~create_spine_icd_list_by_site_list_by_category_function(spine_region = "thoracolumbar", category = .x))
names(thoracolumbar_list) <- spine_categories_vector

####
thoracic_list <- map(.x = spine_categories_vector, .f = ~create_spine_icd_list_by_site_list_by_category_function(spine_region = "thoracic", category = .x))
names(thoracic_list) <- spine_categories_vector

####
cervicothoracic_list <- map(.x = spine_categories_vector, .f = ~create_spine_icd_list_by_site_list_by_category_function(spine_region = "cervicothoracic", category = .x))
names(cervicothoracic_list) <- spine_categories_vector

####
cervical_list <- map(.x = spine_categories_vector, .f = ~create_spine_icd_list_by_site_list_by_category_function(spine_region = "cervical", category = .x))
names(cervical_list) <- spine_categories_vector

####
all_list <- map(.x = spine_categories_vector, .f = ~create_spine_icd_list_by_site_list_by_category_function(spine_region = "all", category = .x))
names(all_list) <- spine_categories_vector


spine_icd_list_by_region <- list(all_list, lumbar_list, thoracolumbar_list, thoracic_list, cervicothoracic_list, cervical_list)

names(spine_icd_list_by_region) <- c("all_regions", "Lumbar", "Thoracolumbar", "Thoracic", "Cervicothoracic", "Cervical")

jh_create_diagnosis_list_vector_function <- function(category_input, site_input){
  spine_options_list <- list()
  
  sites_to_include_vector <- append(site_input, "all")
  
  if(any(category_input == "Degenerative")){
    spine_options_list$Degenerative <- unlist(map(.x = sites_to_include_vector ,.f = ~ spine_icd_list_by_region[[.x]]$Degenerative))
  }
  
  if(any(category_input == "Pediatric Deformity")){
    spine_options_list$`Pediatric Deformity` <- unlist(map(.x = sites_to_include_vector ,.f = ~ spine_icd_list_by_region[[.x]]$`Pediatric Deformity`))
  }
  
  if(any(category_input == "Deformity")){
    spine_options_list$Deformity <- unlist(map(.x = sites_to_include_vector ,.f = ~ spine_icd_list_by_region[[.x]]$Deformity))
  }
  
  
  if(any(category_input == "Trauma")){
    spine_options_list$Trauma <- unlist(map(.x = sites_to_include_vector ,.f = ~ spine_icd_list_by_region[[.x]]$Trauma))
  }
  
  if(any(category_input == "Infection")){
    spine_options_list$Infection <- unlist(map(.x = sites_to_include_vector ,.f = ~ spine_icd_list_by_region[[.x]]$Infection))
  }
  
  if(any(category_input == "Tumor")){
    spine_options_list$Tumor <- unlist(map(.x = sites_to_include_vector ,.f = ~ spine_icd_list_by_region[[.x]]$Tumor))
  }
  
  if(any(category_input == "Congenital")){
    spine_options_list$Congenital <- unlist(map(.x = sites_to_include_vector ,.f = ~ spine_icd_list_by_region[[.x]]$Congenital))
  }
  
  
  if(any(category_input == "Complication")){
    spine_options_list$Complication <- unlist(map(.x = sites_to_include_vector ,.f = ~ spine_icd_list_by_region[[.x]]$Complication))
  }
  
  if(any(category_input == "Other")){
    spine_options_list$Other <- unlist(map(.x = sites_to_include_vector ,.f = ~ spine_icd_list_by_region[[.x]]$Other))
  }
  
  if(any(category_input == "Tumor")){
    spine_options_list$Tumor <- unlist(map(.x = sites_to_include_vector ,.f = ~ spine_icd_list_by_region[[.x]]$Tumor))
  }
  
  return(spine_options_list)
}

# all_spine_diagnosis_choices_list <- list()
# 
# all_spine_diagnosis_choices_list <- map(.x = diagnosis_categories_vector, .f = ~ (spine_icd10_codes_df %>% 
#                                                                                 filter(spine_category == .x) %>%
#                                                                                 select(spine_category, diagnosis) %>% 
#                                                                                 pivot_wider(names_from = spine_category, values_from = diagnosis) %>% 
#                                                                                 unnest())[[1]])
# 
# names(all_spine_diagnosis_choices_list) <- diagnosis_categories_vector

spine_png <- image_read(path = "spine_posterior.png")

anterior_spine_jpg <- image_read(path = "spine_anterior.jpg")

implant_starts_df <- read_csv(file = "full_coordinates_df.csv") %>%
  filter(!is.na(x))

#############-----------------------   Build Key Dataframes  ----------------------###############

left_label_x <- 0.32
right_label_x <- 1 - left_label_x

labels_df <- levels_numbered_df %>%
  filter(str_detect(level, pattern = "-") == FALSE) %>%
  mutate(y = c(0.925, 0.88, 0.865, 0.847, 0.828, 0.811, 0.793, 0.779, 0.757, 0.733, 0.711, 0.682, 0.653, 0.623, 0.593, 0.561, 0.53, 0.491, 0.457, 0.421, 0.385, 0.353, 0.311, 0.271, 0.238, 0.21, 0.19, 0.17))

interbody_levels_df <- levels_numbered_df %>%
  filter(str_detect(level, pattern = "-"))


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

#############-----------------------   Build Implants  ----------------------###############

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


#############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############
#############-------#############-----------------------   ANTERIOR  ----------------------###############-------###############

anterior_df <- implant_starts_df %>%
  filter(approach == "anterior") %>%
  remove_empty() 

labels_anterior_df <- anterior_df %>%
  filter(object == "corpectomy") %>%
  select(level, x, y) %>%
  distinct() 

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
                                                                    inferior_endplate_superior_body_y = ..6, y_click = ..7, direction = ..8)))



#############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
#############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
#############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
#############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
#############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############
#############-------#############-----------------------   ASSEMBLE ALL  ----------------------###############-------###############

all_points_all_implants_constructed_df <- implants_constructed_df %>%
  union_all(osteotomy_df) %>%
  union_all(decompression_df) %>%
  union_all(anterior_objects_df) %>% 
  union_all(all_interbody_df) %>%
  select(level, vertebral_number, everything())

revision_implants_df <- all_points_all_implants_constructed_df %>%
  filter(object == "pedicle_screw" | object == "pelvic_screw" | object == "occipital_screw") %>%
  filter(approach == "posterior") %>%
  arrange(vertebral_number) %>%
  distinct() %>%
  group_by(level, object, side) %>%
  filter(y == max(y)) %>%
  ungroup() %>%
  select(-ends_with("_x"), -ends_with("_y"))
  # select(level, vertebral_number, approach, category, implant, object, side, x, y, fusion, interbody_fusion,fixation_uiv_liv, object_constructed)

all_implants_constructed_df <- all_points_all_implants_constructed_df %>%
  select(-ends_with("_x"), -ends_with("_y"))
  # select(level, body_interspace, vertebral_number, approach, category, implant, object, side, x, y, fusion, interbody_fusion, fixation_uiv_liv, direction, object_constructed)

all_objects_y_range_df <- all_implants_constructed_df %>%
  select(object, y) %>%
  group_by(object) %>%
  filter(y == min(y) | y == max(y)) %>%
  ungroup() %>%
  distinct()

rm(osteotomy_df, decompression_df, all_interbody_df)

