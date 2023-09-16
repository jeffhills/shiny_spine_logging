#############-----------------------   LOAD DATA  ----------------------###############


spine_png <- image_read(path = "spine_posterior.png", density = 5)

anterior_spine_jpg <- image_read(path = "spine_anterior.jpg")

# implant_starts_df <- fread(file = "full_coordinates_no_empty.csv", header = TRUE, check.names = TRUE) 

all_object_ids_df <- fread(file = "all_object_ids_df.csv") 

imported_coordinates <- fread("imported_coordinates_rounded.csv")

# imported_coordinates <- fread("all_object_coordinates_minus_arthroplasty.csv") %>%
#   mutate(x = round(x, 3), y = round(y, 3)) %>%
#   distinct() %>%
#   group_by(object_id) %>%
#   mutate(object_count = row_number()) %>%
#   ungroup() 
# 
# first_row_id <- imported_coordinates%>%
#   group_by(object_id) %>%
#   filter(object_count == 1) %>%
#   select(-object_count)
# 
# new_last_row <- imported_coordinates %>%
#   group_by(object_id) %>%
#   filter(object_count == max(object_count)) %>%
#   mutate(object_count = object_count + 1) %>%
#   select(object_id, object_count) %>%
#   left_join(
#     first_row_id
#   ) %>%
#   select(object_id, x, y, object_count) %>%
#   ungroup()
# 
# 
# imported_coordinates <- imported_coordinates %>%
#   union_all(new_last_row) %>%
#   arrange(object_id, object_count) %>%
#   select(-object_count) 

#############-----------------------   Build Key Dataframes  ----------------------###############

left_label_x <- 0.32
right_label_x <- 1 - left_label_x

labels_df <- levels_numbered_df %>%
  filter(str_detect(level, pattern = "-") == FALSE) %>%
  mutate(y = c(0.925, 0.88, 0.865, 0.847, 0.828, 0.811, 0.793, 0.779, 0.757, 0.733, 0.711, 0.682, 0.653, 0.623, 0.593, 0.561, 0.53, 0.491, 0.457, 0.421, 0.385, 0.353, 0.311, 0.271, 0.238, 0.21, 0.19, 0.17))

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

interbody_levels_df <- levels_numbered_df %>%
  filter(str_detect(level, pattern = "-"))

open_canal_df <- all_object_ids_df %>%
  filter(object == "laminectomy") %>%
  mutate(category = "revision")

polygon_objects_constructed_df <- imported_coordinates %>%
  filter(str_detect(object_id, "grade_1|tether|C1_central_corpectomy_cage_1") == FALSE) %>%
  group_by(object_id) %>%
  nest() %>%
  mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
  select(object_id, object_constructed)

line_objects_constructed_df <- imported_coordinates %>%
  filter(str_detect(object_id, "grade_1|tether|C1_central_corpectomy_cage_1")) %>%
  group_by(object_id) %>%
  nest() %>%
  mutate(object_constructed = map(.x = data, .f = ~ st_linestring(as.matrix(.x)))) %>%
  select(object_id, object_constructed)

all_implants_constructed_df <- all_object_ids_df %>%
  left_join(polygon_objects_constructed_df %>%
              bind_rows(line_objects_constructed_df))


all_objects_y_range_df <- all_implants_constructed_df %>%
  select(object, y) %>%
  distinct()

implant_levels_numbered_df <- all_implants_constructed_df %>%
  filter(implant == "yes") %>%
  select(level, vertebral_number) %>%
  distinct() %>%
  arrange(vertebral_number)

all_screw_coordinates_df <- fread("all_screw_coordinates_df.csv")

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

#############-----------------------   Build Revision Implants  ----------------------###############
revision_implants_df <- all_implants_constructed_df %>%
  filter(category == "implant") %>%
  filter(object == "pedicle_screw" | str_detect(object, "pelvic_screw") | object == "occipital_screw" | object == "lateral_mass_screw" | object == "pars_screw" | str_detect(object, "hook")) %>%
  filter(approach == "posterior") 

revision_screws_df <- revision_implants_df %>%
  filter(str_detect(object, "hook") == FALSE) %>%
  filter((str_detect(string = level, pattern = "C1|C3|C4|C5|C6") & object == "pedicle_screw") == FALSE) %>%
  group_by(level, side) %>%
  filter(y == max(y)) %>%
  ungroup() %>%
  union_all(revision_implants_df %>% filter(level == "Iliac"))%>%
  filter(object != "pelvic_screw_2") 

