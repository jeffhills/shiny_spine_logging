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


# open_canal_df <- all_object_ids_df %>%
#   filter(object == "laminectomy") %>%
#   mutate(category = "revision")

# all_implants_constructed_df <- all_object_ids_df %>%
#   left_join(imported_coordinates %>%
#               group_by(object_id) %>%
#               nest() %>%
#               mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
#               select(object_id, object_constructed))


# all_implants_constructed_df <- imported_coordinates %>%
#   group_by(object_id) %>%
#   nest() %>%
#   mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
#   select(object_id, object_constructed)

# polygon_objects_constructed_df <- imported_coordinates %>%
#   filter(str_detect(object_id, "grade_1|tether|C1_central_corpectomy_cage_1") == FALSE) %>%
#   group_by(object_id) %>%
#   nest() %>%
#   mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
#   select(object_id, object_constructed)
# 
# line_objects_constructed_df <- imported_coordinates %>%
#   filter(str_detect(object_id, "grade_1|tether|C1_central_corpectomy_cage_1")) %>%
#   group_by(object_id) %>%
#   nest() %>%
#   mutate(object_constructed = map(.x = data, .f = ~ st_linestring(as.matrix(.x)))) %>%
#   select(object_id, object_constructed)
# 
# all_implants_constructed_df <- all_object_ids_df %>%
#   left_join(polygon_objects_constructed_df %>%
#               bind_rows(line_objects_constructed_df))


# all_objects_y_range_df <- imported_coordinates %>%
#   select(object, y) %>%
#   distinct()

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

revision_screws_df <- all_object_ids_df %>%
  filter(category == "implant") %>%
  filter(object == "pedicle_screw" | str_detect(object, "pelvic_screw") | object == "occipital_screw" | object == "lateral_mass_screw") %>%
  filter((str_detect(string = level, pattern = "C1|C3|C4|C5|C6") & object == "pedicle_screw") == FALSE) %>%
  filter(approach == "posterior") %>%
  arrange(vertebral_number)%>%
  left_join(all_implants_constructed_df)

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

arthroplasty_coordinates_df <- all_object_ids_df %>%
  filter(str_detect(object, "arthropl")) %>%
  mutate(inferior_endplate_y = c(0.965, 0.925, 0.897, 0.87, 0.846, 0.821, 0.795, 0.77, 0.74, 0.712, 0.682, 0.65, 0.62, 0.591, 0.56, 0.53, 0.495, 0.457, 0.415, 0.367, 0.317, 0.263, 0.215, 0.168), 
         superior_endplate_y = c(0.955, 0.918, 0.888, 0.865, 0.838, 0.815, 0.79, 0.76, 0.732, 0.705, 0.675, 0.645, 0.613, 0.584, 0.555, 0.525, 0.49, 0.45, 0.406, 0.357, 0.305, 0.255, 0.205, 0.16), 
         width = c(0.0175, 0.021, 0.02275, 0.02275, 0.0245, 0.0245, 0.0245, 0.02625, 0.02625, 0.028, 0.028, 0.02975, 0.02975, 0.0315, 0.0315, 0.0315, 0.03325, 0.035, 0.035, 0.035, 0.03675, 0.0385, 0.04025, 0.042)
         )


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

# arthroplasty_constructed_df <- arthroplasty_coordinates_df %>%
#   # filter(vertebral_number %in% c(4.5,5.5)) %>%
#   mutate(object_constructed = pmap(.l = list(..1 = inferior_endplate_y,
#                                              ..2 = superior_endplate_y,
#                                              ..3 = width),
#                                    .f = ~ arthroplasty_function(y_for_inferior_endplate = ..1,
#                                                                 y_for_superior_endplate = ..2,
#                                                                 endplate_width = ..3))) %>%
#   select(names(all_implants_constructed_df))
# 
# all_implants_constructed_df <- all_implants_constructed_df %>%
#   bind_rows(arthroplasty_constructed_df) %>%
#   distinct()
