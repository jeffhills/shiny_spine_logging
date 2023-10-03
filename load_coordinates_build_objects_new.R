# define any necessary vectors
levels_vector <- c('Occiput', 'O-C1', 'C1', 'C1-C2', 'C2', 'C2-C3', 'C3', 'C3-C4', 'C4', 'C4-C5', 'C5', 'C5-C6', 'C6', 'C6-C7', 'C7', 'C7-T1', 'T1', 'T1-T2', 'T2', 'T2-T3', 'T3', 'T3-T4', 'T4', 'T4-T5', 'T5', 'T5-T6', 'T6', 'T6-T7', 'T7', 'T7-T8', 'T8', 'T8-T9', 'T9', 'T9-T10', 'T10', 'T10-T11', 'T11', 'T11-T12', 'T12', 'T12-L1', 'L1', 'L1-L2', 'L2', 'L2-L3', 'L3', 'L3-L4', 'L4', 'L4-L5', 'L5', 'L5-S1', 'S1', 'Sacro-iliac', 'Iliac', 'S2AI')

vertebral_numbers_vector <- c(0,0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5, 11, 11.5, 12, 12.5, 13, 13.5, 14, 14.5, 15, 15.5, 16, 16.5, 17, 17.5, 18, 18.5, 19, 19.5, 20, 20.5, 21, 21.5, 22, 22.5, 23, 23.5, 24, 24.5, 25, 25.5, 26, 27)

levels_numbered_df <- tibble(level = levels_vector, vertebral_number = vertebral_numbers_vector)

vertebral_bodies_vector <- c('C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'T1', 'T2', 'T3', 'T4', 'T5', 'T6', 'T7', 'T8', 'T9', 'T10', 'T11', 'T12', 'L1', 'L2', 'L3', 'L4', 'L5', 'L6', 'S1', "S2AI", "S2AI_2")

interspaces_vector <- c('O-C1', 'C1-C2', 'C2-C3', 'C3-C4', 'C4-C5', 'C5-C6', 'C6-C7', 'C7-T1', 'T1-T2', 'T2-T3', 'T3-T4', 'T4-T5', 'T5-T6', 'T6-T7', 'T7-T8', 'T8-T9', 'T9-T10', 'T10-T11', 'T11-T12', 'T12-L1', 'L1-L2', 'L2-L3', 'L3-L4', 'L4-L5', 'L5-S1', 'L4-S1', 'L5-L6', 'L6-S1', 'Sacro-iliac')

anterior_plate_vector <- c('C1-C2', 'C2-C3', 'C3-C4', 'C4-C5', 'C5-C6', 'C6-C7', 'C7-T1', 'T1-T2', 'T2-T3', 'T3-T4', 'T4-T5', 'T5-T6', 'T6-T7', 'T7-T8', 'T8-T9', 'T9-T10', 'T10-T11', 'T11-T12', 'T12-L1', 'L1-L2', 'L2-L3', 'L3-L4', 'L4-L5', 'L5-S1')

screw_label_vector <- c('occiput_left_occipital_screw', 'occiput_right_occipital_screw', 'c1_left_lateral_mass_screw', 'c1_right_lateral_mass_screw', 'c1_left_translaminar_screw', 'c1_right_translaminar_screw', 'c2_left_pars_screw', 'c2_right_pars_screw', 'c2_left_pedicle_screw', 'c2_right_pedicle_screw', 'c2_left_transarticular_screw', 'c2_right_transarticular_screw', 'c2_left_translaminar_screw', 'c2_right_translaminar_screw', 'c2_central_screw_washer', 'c3_left_lateral_mass_screw', 'c3_right_lateral_mass_screw', 'c3_left_pedicle_screw', 'c3_right_pedicle_screw', 'c3_left_translaminar_screw', 'c3_right_translaminar_screw', 'c3_central_screw_washer', 'c4_left_lateral_mass_screw', 'c4_right_lateral_mass_screw', 'c4_left_pedicle_screw', 'c4_right_pedicle_screw', 'c4_left_translaminar_screw', 'c4_right_translaminar_screw', 'c4_central_screw_washer', 'c5_left_lateral_mass_screw', 'c5_right_lateral_mass_screw', 'c5_left_pedicle_screw', 'c5_right_pedicle_screw', 'c5_left_translaminar_screw', 'c5_right_translaminar_screw', 'c5_central_screw_washer', 'c6_left_lateral_mass_screw', 'c6_right_lateral_mass_screw', 'c6_left_pedicle_screw', 'c6_right_pedicle_screw', 'c6_left_translaminar_screw', 'c6_right_translaminar_screw', 'c6_central_screw_washer', 'c7_left_lateral_mass_screw', 'c7_right_lateral_mass_screw', 'c7_left_pedicle_screw', 'c7_right_pedicle_screw', 'c7_left_translaminar_screw', 'c7_right_translaminar_screw', 'c7_central_screw_washer', 't1_left_pedicle_screw', 't1_right_pedicle_screw', 't1_left_translaminar_screw', 't1_right_translaminar_screw', 't1_central_screw_washer', 't2_left_pedicle_screw', 't2_right_pedicle_screw', 't2_left_translaminar_screw', 't2_right_translaminar_screw', 't2_central_screw_washer', 't3_left_pedicle_screw', 't3_right_pedicle_screw', 't3_left_translaminar_screw', 't3_right_translaminar_screw', 't3_central_screw_washer', 't4_left_pedicle_screw', 't4_right_pedicle_screw', 't4_left_translaminar_screw', 't4_right_translaminar_screw', 't4_central_screw_washer', 't5_left_pedicle_screw', 't5_right_pedicle_screw', 't5_left_translaminar_screw', 't5_right_translaminar_screw', 't5_central_screw_washer', 't6_left_pedicle_screw', 't6_right_pedicle_screw', 't6_left_translaminar_screw', 't6_right_translaminar_screw', 't6_central_screw_washer', 't7_left_pedicle_screw', 't7_right_pedicle_screw', 't7_left_translaminar_screw', 't7_right_translaminar_screw', 't7_central_screw_washer', 't8_left_pedicle_screw', 't8_right_pedicle_screw', 't8_left_translaminar_screw', 't8_right_translaminar_screw', 't8_central_screw_washer', 't9_left_pedicle_screw', 't9_right_pedicle_screw', 't9_left_translaminar_screw', 't9_right_translaminar_screw', 't9_central_screw_washer', 't10_left_pedicle_screw', 't10_right_pedicle_screw', 't10_left_translaminar_screw', 't10_right_translaminar_screw', 't10_central_screw_washer', 't11_left_pedicle_screw', 't11_right_pedicle_screw', 't11_left_translaminar_screw', 't11_right_translaminar_screw', 't11_central_screw_washer', 't12_left_pedicle_screw', 't12_right_pedicle_screw', 't12_left_translaminar_screw', 't12_right_translaminar_screw', 't12_central_screw_washer', 'l1_left_pedicle_screw', 'l1_right_pedicle_screw', 'l1_left_translaminar_screw', 'l1_right_translaminar_screw', 'l1_central_screw_washer', 'l2_left_pedicle_screw', 'l2_right_pedicle_screw', 'l2_left_translaminar_screw', 'l2_right_translaminar_screw', 'l2_central_screw_washer', 'l3_left_pedicle_screw', 'l3_right_pedicle_screw', 'l3_left_translaminar_screw', 'l3_right_translaminar_screw', 'l3_central_screw_washer', 'l4_left_pedicle_screw', 'l4_right_pedicle_screw', 'l4_left_translaminar_screw', 'l4_right_translaminar_screw', 'l4_central_screw_washer', 'l5_left_pedicle_screw', 'l5_right_pedicle_screw', 'l5_left_translaminar_screw', 'l5_right_translaminar_screw', 'l5_central_screw_washer', 's1_left_pedicle_screw', 's1_right_pedicle_screw', 's1_central_screw_washer', 'iliac_left_pelvic_screw', 'iliac_right_pelvic_screw', 's2ai_left_pelvic_screw', 's2ai_right_pelvic_screw')

screw_label_level_vector <- c('Occiput', 'Occiput', 'C1', 'C1', 'C1', 'C1', 'C2', 'C2', 'C2', 'C2', 'C2', 'C2', 'C2', 'C2', 'C2', 'C3', 'C3', 'C3', 'C3', 'C3', 'C3', 'C3', 'C4', 'C4', 'C4', 'C4', 'C4', 'C4', 'C4', 'C5', 'C5', 'C5', 'C5', 'C5', 'C5', 'C5', 'C6', 'C6', 'C6', 'C6', 'C6', 'C6', 'C6', 'C7', 'C7', 'C7', 'C7', 'C7', 'C7', 'C7', 'T1', 'T1', 'T1', 'T1', 'T1', 'T2', 'T2', 'T2', 'T2', 'T2', 'T3', 'T3', 'T3', 'T3', 'T3', 'T4', 'T4', 'T4', 'T4', 'T4', 'T5', 'T5', 'T5', 'T5', 'T5', 'T6', 'T6', 'T6', 'T6', 'T6', 'T7', 'T7', 'T7', 'T7', 'T7', 'T8', 'T8', 'T8', 'T8', 'T8', 'T9', 'T9', 'T9', 'T9', 'T9', 'T10', 'T10', 'T10', 'T10', 'T10', 'T11', 'T11', 'T11', 'T11', 'T11', 'T12', 'T12', 'T12', 'T12', 'T12', 'L1', 'L1', 'L1', 'L1', 'L1', 'L2', 'L2', 'L2', 'L2', 'L2', 'L3', 'L3', 'L3', 'L3', 'L3', 'L4', 'L4', 'L4', 'L4', 'L4', 'L5', 'L5', 'L5', 'L5', 'L5', 'S1', 'S1', 'S1', 'Iliac', 'Iliac', 'S2AI', 'S2AI')

screw_label_object <- c('occipital_screw', 'occipital_screw', 'lateral_mass_screw', 'lateral_mass_screw', 'translaminar_screw', 'translaminar_screw', 'pars_screw', 'pars_screw', 'pedicle_screw', 'pedicle_screw', 'transarticular_screw', 'transarticular_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'lateral_mass_screw', 'lateral_mass_screw', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'lateral_mass_screw', 'lateral_mass_screw', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'lateral_mass_screw', 'lateral_mass_screw', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'lateral_mass_screw', 'lateral_mass_screw', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'lateral_mass_screw', 'lateral_mass_screw', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'translaminar_screw', 'translaminar_screw', 'screw_washer', 'pedicle_screw', 'pedicle_screw', 'screw_washer', 'pelvic_screw', 'pelvic_screw', 'pelvic_screw', 'pelvic_screw')

screw_size_labels_df <- tibble(level = screw_label_level_vector, screw_label = screw_label_vector) %>%
  mutate(screw_diameter_label = paste(screw_label, "diameter", sep = "_")) %>%
  mutate(screw_length_label = paste(screw_label, "length", sep = "_"))

#############-----------------------   LOAD DATA  ----------------------###############


spine_png <- image_read(path = "spine_posterior.png", density = 5)

posterior_spine_ggdraw <- ggdraw() +
  draw_image(
    spine_png,
    scale = 1,
    y = 0,
    valign = 0,
    x = 0,
    height = 1, clip = "on"
  ) 

anterior_spine_jpg <- image_read(path = "spine_anterior.jpg")

anterior_spine_ggdraw <- ggdraw() +
  draw_image(
    anterior_spine_jpg,
    scale = 1,
    y = 0,
    valign = 0,
    x = 0,
    height = 1, clip = "on"
    # width = 1
  ) 

# implant_starts_df <- fread(file = "full_coordinates_no_empty.csv", header = TRUE, check.names = TRUE) 

all_object_ids_df <- fread(file = "all_object_ids_df.csv") 

# imported_coordinates <- fread("imported_coordinates_rounded.csv")
# 
# all_implants_constructed_df <<- all_object_ids_df %>%
#   filter(category == "implant") %>%
#   left_join(fread("imported_coordinates_rounded.csv") %>%
#               group_by(object_id) %>%
#               nest() %>%
#               mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
#               select(object_id, object_constructed))

all_implants_constructed_df <<- all_object_ids_df %>%
  filter(category == "implant") %>%
  left_join(fread("coordinates/implant.csv") %>%
              group_by(object_id) %>%
              nest() %>%
              mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
              select(object_id, object_constructed))

implant_coordinates_df <- fread("coordinates/implant.csv") %>%
  filter(str_detect(object_id, "screw")) %>%
  separate(col = object_id, into = c("level", "side", "object"), sep = "_", extra = "merge") %>%
  mutate(object = str_remove_all(object, "_1"))

# coordinate_file_names_list <- list(anterior_body = "coordinates/anterior_body.csv",
#                                    anterior_disc = "coordinates/anterior_disc.csv",
#                                    anterior_interbody_fusion = "coordinates/anterior_interbody_fusion.csv",
#                                    decompression = "coordinates/decompression.csv",
#                                    implant = "coordinates/implant.csv",
#                                    incision_drainage = "coordinates/incision_drainage.csv",
#                                    interbody = "coordinates/interbody.csv",
#                                    osteotomy = "coordinates/osteotomy.csv",
#                                    tumor = "coordinates/tumor.csv")
# 
# load_and_bind_coordinates_function <- function(all_implants_df, coordinate_file_name = "x"){
#   coordinate_objects_constructed_df <- all_object_ids_df %>%
#     filter(category == str_remove_all(coordinate_file_name, "coordinates/|.csv")) %>%
#     left_join(fread(paste0(coordinate_file_name)) %>%
#                 group_by(object_id) %>%
#                 nest() %>%
#                 mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
#                 select(object_id, object_constructed))
# 
#   all_implants_df %>%
#     bind_rows(coordinate_objects_constructed_df) %>%
#     distinct()
# 
# }
# 
# for (i in coordinate_file_names_list) {
# 
#   all_implants_constructed_df <- load_and_bind_coordinates_function(all_implants_df = all_implants_constructed_df,
#                                                                     coordinate_file_name = i)
# 
# }
#############-----------------------   Build Key Dataframes  ----------------------###############

left_label_x <- 0.32
right_label_x <- 1 - left_label_x

labels_df <- levels_numbered_df %>%
  filter(str_detect(level, pattern = "-") == FALSE) %>%
  mutate(y = c(0.925, 0.88, 0.865, 0.847, 0.828, 0.811, 0.793, 0.779, 0.757, 0.733, 0.711, 0.682, 0.653, 0.623, 0.593, 0.561, 0.53, 0.491, 0.457, 0.421, 0.385, 0.353, 0.311, 0.271, 0.238, 0.21, 0.19, 0.17))

interbody_levels_df <- levels_numbered_df %>%
  filter(str_detect(level, pattern = "-"))

labels_anterior_df <- all_object_ids_df %>%
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



implant_levels_numbered_df <- all_object_ids_df %>%
  filter(implant == "yes") %>%
  select(level, vertebral_number) %>%
  distinct() %>%
  arrange(vertebral_number)

all_screw_coordinates_df <- fread("all_screw_coordinates_df.csv")

all_cages_df <- all_object_ids_df %>%
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

### FOR RODS CROSSING
lines_df <- labels_df %>%
  mutate(vertebral_number = vertebral_number - 0.5) %>%
  select(-level) %>%
  left_join(levels_numbered_df) %>%
  filter(!is.na(level)) %>%
  select(level, vertebral_number, y) %>%
  mutate(x_left = 0.4, x_right = 0.6) %>%
  mutate(y = if_else(level == "Sacro-iliac", y - 0.01, y)) %>%
  rename(y_lower = y)  %>% 
  mutate(y_upper = y_lower + 0.018) %>%
  mutate(y_upper = case_when(
    vertebral_number < 10.25 ~ y_upper,
    between(vertebral_number, 10.25, 15.75) ~ y_upper + 0.0075,
    between(vertebral_number, 15.75, 20.75) ~ y_upper + 0.01,
    between(vertebral_number, 20.75, 23.75) ~ y_upper + 0.012,
    vertebral_number > 23.75 ~ y_upper
  ))


lower_lines_list <- pmap(.l = list(..1 = lines_df$x_left, ..2 = lines_df$x_right, ..3 = lines_df$y_lower), 
                         .f = ~ st_linestring(as.matrix(tibble(x = c(..1, ..2), y = ..3))))

upper_lines_list <- pmap(.l = list(..1 = lines_df$x_left, ..2 = lines_df$x_right, ..3 = lines_df$y_upper), 
                         .f = ~ st_linestring(as.matrix(tibble(x = c(..1, ..2), y = ..3))))


names(lower_lines_list) <- lines_df$level

names(upper_lines_list) <- lines_df$level
rods_crossing_by_level_df <- tibble(level = names(lower_lines_list))

#############-----------------------   Build Revision Implants  ----------------------###############
# revision_implants_df <- all_implants_constructed_df %>%
#   filter(category == "implant") %>%
#   filter(object == "pedicle_screw" | str_detect(object, "pelvic_screw") | object == "occipital_screw" | object == "lateral_mass_screw" | object == "pars_screw" | str_detect(object, "hook")) %>%
#   filter(approach == "posterior")
# 
# revision_screws_df <- revision_implants_df %>%
#   filter(str_detect(object, "hook") == FALSE) %>%
#   filter((str_detect(string = level, pattern = "C1|C3|C4|C5|C6") & object == "pedicle_screw") == FALSE) %>%
#   group_by(level, side) %>%
#   filter(y == max(y)) %>%
#   ungroup() %>%
#   union_all(revision_implants_df %>% filter(level == "Iliac"))%>%
#   filter(object != "pelvic_screw_2")

# revision_implants_df <- all_object_ids_df %>%
#   filter(category == "implant") %>%
#   filter(object == "pedicle_screw" | str_detect(object, "pelvic_screw") | object == "occipital_screw" | object == "lateral_mass_screw" | object == "pars_screw" | str_detect(object, "hook")) %>%
#   filter(approach == "posterior") %>%
#   arrange(vertebral_number)%>%
#   left_join(all_implants_constructed_df)

# revision_screws_df <- all_object_ids_df %>%
#   filter(category == "implant") %>%
#   filter(object == "pedicle_screw" | str_detect(object, "pelvic_screw") | object == "occipital_screw" | object == "lateral_mass_screw") %>%
#   filter((str_detect(string = level, pattern = "C1|C3|C4|C5|C6") & object == "pedicle_screw") == FALSE) %>%
#   filter(approach == "posterior") %>%
#   arrange(vertebral_number) %>%
#   left_join(all_implants_constructed_df)

# revision_screws_df <- all_object_ids_df %>%
#   filter(str_detect(object, "hook") == FALSE) %>%
#   filter((str_detect(string = level, pattern = "C1|C3|C4|C5|C6") & object == "pedicle_screw") == FALSE) %>%
#   left_join(all_implants_constructed_df) %>%
#   group_by(level, side) %>%
#   filter(y == max(y)) %>%
#   ungroup() %>%
#   union_all(revision_implants_df %>% filter(level == "Iliac"))%>%
#   filter(object != "pelvic_screw_2") %>%
#   # filter(object == "pedicle_screw" | str_detect(object, "pelvic_screw") | object == "occipital_screw" | object == "lateral_mass_screw" | object == "pars_screw" | str_detect(object, "hook")) %>%
#   arrange(vertebral_number) 


#############-----------------------   Build: arthroplasty  ----------------------###############
# 
# arthroplasty_coordinates_df <- all_object_ids_df %>%
#   filter(str_detect(object, "arthropl")) %>%
#   mutate(inferior_endplate_y = c(0.965, 0.925, 0.897, 0.87, 0.846, 0.821, 0.795, 0.77, 0.74, 0.712, 0.682, 0.65, 0.62, 0.591, 0.56, 0.53, 0.495, 0.457, 0.415, 0.367, 0.317, 0.263, 0.215, 0.168), 
#          superior_endplate_y = c(0.955, 0.918, 0.888, 0.865, 0.838, 0.815, 0.79, 0.76, 0.732, 0.705, 0.675, 0.645, 0.613, 0.584, 0.555, 0.525, 0.49, 0.45, 0.406, 0.357, 0.305, 0.255, 0.205, 0.16), 
#          width = c(0.0175, 0.021, 0.02275, 0.02275, 0.0245, 0.0245, 0.0245, 0.02625, 0.02625, 0.028, 0.028, 0.02975, 0.02975, 0.0315, 0.0315, 0.0315, 0.03325, 0.035, 0.035, 0.035, 0.03675, 0.0385, 0.04025, 0.042)
#          )

arthroplasty_function <- function(y_for_inferior_endplate, y_for_superior_endplate, endplate_width){
  endplate_height <- y_for_inferior_endplate - y_for_superior_endplate
  
  bottom_oval <- st_ellipse(st_point(c(0.5, y_for_superior_endplate)), ex = endplate_width/2, ey = endplate_height*0.75)
  
  left_bottom_point <- c(0.5 - endplate_width/2, y_for_superior_endplate)
  right_bottom_point <- c(0.5 + endplate_width/2, y_for_superior_endplate)
  # bottom_top_point <- c(0.5, y_for_superior_endplate + endplate_height/2)
  
  top_left_point <- c(0.5 - endplate_width/2, y_for_inferior_endplate)
  top_right_point <- c(0.5 + endplate_width/2, y_for_inferior_endplate)
  
  full_disc_sf <- st_buffer(st_polygon(list(rbind(left_bottom_point, right_bottom_point, top_right_point, top_left_point, left_bottom_point))), dist = endplate_height*0.1)
  
  top_disc_buff <- st_buffer(st_difference(x = full_disc_sf, y = bottom_oval), dist = -0.0004)
  
  bottom_disc_buff <- st_buffer(st_intersection(x = bottom_oval, y = full_disc_sf), dist = -0.0004)
  
  disc_df <- tibble(object_constructed = c(st_geometry(top_disc_buff), st_geometry(bottom_disc_buff[[1]]))) %>%
    mutate(color = c("blue", "lightblue"))
  
  return(disc_df)
}

