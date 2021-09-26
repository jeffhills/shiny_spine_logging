# # screws_v3
# library(shiny)
# library(sf)
# library(tidyverse)
# library(ggplot2)
# library(colourpicker)
# library(kableExtra)
# library(cowplot)
# library(ggpubr)
# library(magick)
# library(ggpattern)
# library(glue)
# library(rlist)
# library(janitor)
# library(redcapAPI)
# library(nngeo)
# 
# source("hook_function.R", local = TRUE)
# source("screw_function.R", local = TRUE)
# source("osteotomies_decompressions_functions.R", local = TRUE)
# source("operative_note_posterior_generator.R", local = TRUE)
# source("operative_note_updated_anterior_generator.R", local = TRUE)


spine_png <- image_read(path = "posterior_spine_figure.png")

anterior_spine_jpg <- image_read(path = "spine_anterior.jpg")

implant_starts_df <- read_csv(file = "full_coordinates_df.csv") %>%
  filter(!is.na(x))

left_label_x <- 0.32
right_label_x <- 1 - left_label_x

labels_df <- implant_starts_df %>%
  select(level, vertebral_number, body_center_y) %>%
  filter(!is.na(vertebral_number)) %>%
  filter(str_detect(string = level, pattern = "-", negate = TRUE)) %>%
  distinct()%>%
  mutate(y = case_when(
    str_starts(string = level, pattern = "O") == TRUE ~ body_center_y + 0.01,
    str_starts(string = level, pattern = "C") == TRUE ~ body_center_y,
    str_starts(string = level, pattern = "T") == TRUE ~ body_center_y + 0.01,
    str_starts(string = level, pattern = "L") == TRUE ~ body_center_y + 0.01,
    str_starts(string = level, pattern = "I") == TRUE ~ body_center_y + 0.01,
    str_starts(string = level, pattern = "S") == TRUE ~ body_center_y + 0.01
  )) %>%
  mutate(y = if_else(level == "C7", body_center_y + 0.01, y)) %>%
  arrange(vertebral_number)

interbody_levels_df <- implant_starts_df %>%
  filter(str_detect(level, "-")) %>%
  select(level, vertebral_number) %>%
  distinct() %>% 
  add_row(level = "O-C1", vertebral_number = 0.5) %>%
  add_row(level = "Sacro-iliac", vertebral_number = 25.5) %>%
  arrange(vertebral_number)

# open_canal_df <- implant_starts_df %>%
#   filter(object == "laminectomy") %>%
#   mutate(category = "revision") %>%
#   filter(!is.na(width)) %>%
#   mutate(object_constructed = pmap(list(..1 = left_x,
#                                         ..2 = superior_y,
#                                         ..3 = right_x,
#                                         ..4 = inferior_y,
#                                         ..5 = width), .f = ~ build_decompression_function(left_x = ..1, right_x = ..3, superior_y = ..2, inferior_y = ..4, top_width = ..5)))

implants_constructed_df <- implant_starts_df %>%
  filter(category == "implant") %>%
  mutate(rod = "main_rod") %>%
  mutate(sublaminar_band_length = length) %>%
  mutate(length_for_tether = length) %>%
  mutate(object_constructed = pmap(.l =  list(..1 = object, 
                                              ..2 = x, 
                                              ..3 = y, 
                                              ..4 = angle,
                                              ..5 = length, 
                                              ..6 = width,
                                              ..7 = rod, 
                                              ..8 = sublaminar_band_length,
                                              ..9 = length_for_tether), .f = ~ screw_hook_implant_function(implant_type = ..1, 
                                                                                                           main_or_sattelite_rod = ..7, 
                                                                                                           start_x = ..2,
                                                                                                           y = ..3,
                                                                                                           angle = ..4,
                                                                                                           screw_length_mod = ..5,
                                                                                                           screw_width_mod = ..6, 
                                                                                                           sublaminar_band_length = ..8, 
                                                                                                           length_for_tether = ..9)))

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

# decompression_df <- implant_starts_df %>%
#   filter(approach == "posterior") %>%
#   filter(category == "decompression") %>%
#   # filter(!is.na(width)) %>%
#   mutate(object_constructed = pmap(list(..1 = left_x,
#                                         ..2 = superior_y,
#                                         ..3 = right_x,
#                                         ..4 = inferior_y,
#                                         ..5 = width, 
#                                         ..6 = object,
#                                         ..7 = lateral_pars_x,
#                                         ..8 = superior_tp_y,
#                                         ..9 = side, 
#                                         ..10 = inferior_pedicle_y), 
#                                    .f = ~ build_decompression_function(left_x = ..1, right_x = ..3, superior_y = ..2, inferior_y = ..4, top_width = ..5, object = ..6, x_lateral_pars = ..7, y_inferior_tp = ..8, side = ..9, inferior_pedicle_y = ..10)))

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
                                        ..10 = inferior_pedicle_y), 
                                   .f = ~ build_decompression_function(left_x = ..1, right_x = ..3, superior_y = ..2, inferior_y = ..4, top_width = ..5, object = ..6, x_lateral_pars = ..7, y_inferior_tp = ..8, side = ..9, inferior_pedicle_y = ..10)))

open_canal_df <- decompression_df %>%
  filter(object == "laminectomy") %>%
  mutate(category = "revision")

interbody_df <- implant_starts_df %>%
  filter(category == "interbody") %>%
  filter(approach == "posterior") %>%
  filter(!is.na(width)) %>%
  mutate(object_constructed = pmap(list(..1 = left_x,
                                        ..2 = superior_y,
                                        ..3 = right_x,
                                        ..4 = inferior_y,
                                        ..5 = width,
                                        ..6 = object), 
                                   .f = ~ build_interbody_function(left_x = ..1, right_x = ..3, superior_y = ..2, inferior_y = ..4, top_width = ..5, object = ..6)))

llif_interbody_df <- interbody_df %>%
  filter(object == "tlif") %>%
  mutate(object = "llif")

plif_interbody_df <- interbody_df %>%
  filter(object == "tlif") %>%
  mutate(object = "plif")

all_interbody_df <- interbody_df %>%
  union_all(llif_interbody_df) %>%
  union_all(plif_interbody_df) %>%
  distinct()

rm(llif_interbody_df, plif_interbody_df)
# all_interbody_df <- interbody_df %>%
#   filter(object == "tlif") %>%
#   mutate(object = "llif") %>%
#   union_all(interbody_df) %>%
#   union_all(interbody_df %>%
#               filter(object == "tlif") %>%
#               mutate(object = "plif")) %>%
#   distinct()

anterior_df <- implant_starts_df %>%
  filter(str_detect(string = category, pattern = "anterior")) %>%
  remove_empty() 

labels_anterior_df <- anterior_df %>%
  filter(object == "corpectomy") %>%
  select(level, x, y) %>%
  # filter(str_detect(string = level, pattern = "-") == FALSE) %>%
  distinct() 

anterior_objects_df <- anterior_df %>%
  mutate(object_constructed = pmap(list(..1 = object,
                                        ..2 = width,
                                        ..3 = inferior_endplate_y,
                                        ..4 = superior_endplate_y, 
                                        ..5 = superior_enplate_inferior_body_y, 
                                        ..6 = inferior_endplate_superior_body_y,
                                        ..7 = y),
                                   .f = ~ anterior_implant_function(object_type = ..1, 
                                                                    body_width = ..2,
                                                                    inferior_endplate_y = ..3,
                                                                    superior_endplate_y = ..4,
                                                                    superior_endplate_inferior_body_y = ..5, 
                                                                    inferior_endplate_superior_body_y = ..6, y_click = ..7)))

all_implants_constructed_df <- implants_constructed_df %>%
  union_all(osteotomy_df) %>%
  union_all(decompression_df) %>%
  union_all(anterior_objects_df) %>% 
  union_all(all_interbody_df) %>%
  select(level, vertebral_number, everything())

revision_implants_df <- all_implants_constructed_df %>%
  filter(object == "pedicle_screw" | object == "pelvic_screw" | object == "occipital_screw") %>%
  # filter(str_detect(string = object, pattern = "pedicle_screw")) %>%
  filter(approach == "posterior") %>%
  arrange(vertebral_number) %>%
  distinct() %>%
  group_by(level, object, side) %>%
  filter(y == max(y)) %>%
  ungroup()

rm(decompression_df, osteotomy_df, interbody_df, all_interbody_df)
# 


levels_numbered_df <- all_implants_constructed_df %>%
  select(level, vertebral_number) %>%
  distinct() %>%
  arrange(vertebral_number)

# ################# Anterior Functions ##################
# 
anterior_fusion_levels_function <- function(all_objects_added_df){
  anterior_fusion_implants_df <- all_objects_added_df %>%
    filter(approach == "anterior") %>%
    select(level, vertebral_number, object) %>%
    filter(object != "screw_washer") %>%
    filter(object != "anterior_disc_arthroplasty") %>%
    distinct()
  
  corpectomy_df <- anterior_fusion_implants_df %>%
    filter(str_detect(string = object, pattern = "corpectomy")) %>%
    select(level, object) %>%
    distinct() %>%
    mutate(level = map(.x = level, .f = ~ jh_get_cranial_caudal_interspace_body_list_function(level = .x))) %>%
    unnest() %>%
    unnest() %>%
    filter(str_detect(string = level, pattern = "-")) %>%
    mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
    mutate(object = "fusion") %>%
    distinct()
  
  fusion_levels_df <- anterior_fusion_implants_df %>%
    union_all(corpectomy_df) %>%
    filter(str_detect(level, pattern = "-")) %>%
    mutate(object = "fusion") %>%
    distinct() %>%
    arrange(vertebral_number)
  
  return(fusion_levels_df)
  
}

