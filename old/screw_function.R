############ SCREW FUNCTION ###########33

screw_function <- function(screw_start_x, screw_start_y, angle, screw_length_modifier = 1, screw_width_modifier = 1){
  
  rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  
  screw_length <- 0.02*screw_length_modifier
  
  screw_width <- 0.007*screw_width_modifier
  
  if(screw_start_x < 0.5){
    mid_screw_head <- st_point(c(screw_start_x, screw_start_y))
    top_screw_head <- st_point(c(mid_screw_head[1], mid_screw_head[2] + screw_width*.5))
    bottom_screw_head <- st_point(c(mid_screw_head[1], mid_screw_head[2] - screw_width*.5))
    screw_tip <- st_point(c(mid_screw_head[1] + screw_length, mid_screw_head[2]))
    
    screw <- st_polygon(list(rbind(top_screw_head, mid_screw_head, bottom_screw_head, screw_tip, top_screw_head)))
    
    screw_rotated <- (screw - mid_screw_head)*rot(angle*-1*pi/180) + mid_screw_head
  }
  
  if(screw_start_x > 0.5){
    mid_screw_head <- st_point(c(screw_start_x, screw_start_y))
    top_screw_head <- st_point(c(mid_screw_head[1], mid_screw_head[2] + screw_width*.5))
    bottom_screw_head <- st_point(c(mid_screw_head[1], mid_screw_head[2] - screw_width*.5))
    screw_tip <- st_point(c(mid_screw_head[1] - screw_length, mid_screw_head[2]))
    
    screw <- st_polygon(list(rbind(top_screw_head, mid_screw_head, bottom_screw_head, screw_tip, top_screw_head)))
    
    screw_rotated <- (screw - mid_screw_head)*rot(angle*pi/180) + mid_screw_head
  }
  
  screw_sf <- st_buffer(x = screw_rotated, dist = 0.001, endCapStyle = "SQUARE")
  
  screw_sf <- st_buffer(x = screw_sf, dist = 0.00075, endCapStyle = "ROUND")
  
  screw_head <- mid_screw_head
  
  return(screw_sf)
}


screw_hook_implant_function <- function(implant_type,
                                        main_or_sattelite_rod = "main_rod",
                                        start_x, 
                                        y, 
                                        angle = 0, 
                                        screw_length_mod = 1, 
                                        screw_width_mod = 1, 
                                        hook_length_mod = 1,
                                        hook_direction,
                                        hook_width_mod = 1,
                                        hook_thickness_percent_mod = 1, 
                                        sublaminar_band_length = 0.01, 
                                        length_for_tether = 0.02){
  # if(implant_type == "pedicle_screw" | implant_type == "lateral_mass_screw" |implant_type == "pars_screw" |implant_type == "pelvic_screw" | implant_type == "translaminar_screw"){
    
  if(main_or_sattelite_rod == "satellite_rod"){
    if(start_x < 0.5){
      start_x <- start_x - 0.02
      screw_length_mod <- screw_length_mod*2
    }else{
      start_x <-  start_x + 0.02
      screw_length_mod <- screw_length_mod*2
    }
  }
  
  if(str_detect(string = implant_type, pattern = "screw")){
    object <- screw_function(screw_start_x = start_x, screw_start_y = y, angle = angle, screw_length_modifier = screw_length_mod, screw_width_modifier = screw_width_mod)
  }
  
  if(implant_type == "tp_hook"){
    object <-hook_tp_function(hook_start_x = start_x, hook_edge_y = y)
  }
  
  if(implant_type == "laminar_downgoing_hook"){
    object <-hook_laminar_function(hook_direction = "downgoing", hook_start_x = start_x, hook_edge_y = y)
  }
  
  if(implant_type == "laminar_upgoing_hook"){
    object <- hook_laminar_function(hook_direction = "upgoing", hook_start_x = start_x, hook_edge_y = y)
  }
  
  if(implant_type == "pedicle_hook"){
    object <- build_pedicle_hook_function(x_start = start_x, y_start = y)
  }
  
  if(implant_type == "sublaminar_wire"){
    object <- build_sublaminar_band_function(x_start = start_x, y_top = y, y_length = sublaminar_band_length)
  }
  
  if(implant_type == "tether"){
    object <- tether_function(tether_start_y = y, tether_length = length_for_tether)
  }
  
  if(implant_type == "cement_augmentation"){
    object <- st_buffer(x = st_point(x = c(0.5, y)), dist = 0.01, endCapStyle = "ROUND")
  }
  
  return(object)
}


####### ROD FUNCTION

# build_additional_rod_function <- function(rod_type = NULL, new_rod_vector, full_implant_df, offset_medial = 0.006){
#   offset_value <- if_else(full_implant_df$x[1] < 0.5, offset_medial, offset_medial*-1)
#   
#   if(rod_type == "accessory"){
#     accessory_rod_df <- tibble(level = new_rod_vector) %>%
#       left_join(full_implant_df) %>%
#       select(x, y) %>%
#       arrange(rev(y)) %>%
#       distinct()
#     
#     top_connector <- tibble(x = c(accessory_rod_df$x[1],
#                                   accessory_rod_df$x[1] - offset_value*1.5), 
#                             y = max(accessory_rod_df$y) - 0.01) %>%
#       as.matrix()
#     
#     bottom_connector <- tibble(x = c(tail(accessory_rod_df$x, n = 1),
#                                      tail(accessory_rod_df$x, n = 1) - offset_value*1.5),
#                                y = min(accessory_rod_df$y) + 0.01) %>%
#       as.matrix()
#     
#     accessory_rod_matrix <- accessory_rod_df %>%
#       mutate(x = x - offset_value) %>%
#       as.matrix()
#     
#     accessory_rod_top_connector <- st_buffer(st_linestring(top_connector), dist = 0.003, endCapStyle = "ROUND")
#     accessory_rod <- st_buffer(st_linestring(accessory_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
#     accessory_rod_bottom_connector <- st_buffer(st_linestring(bottom_connector), dist = 0.003, endCapStyle = "ROUND")
#     
#     additional_rod_sf <- st_multipolygon(list(accessory_rod_top_connector, accessory_rod, accessory_rod_bottom_connector))
#     
#   }
#   
#   if(rod_type == "satellite"){
#     satellite_rod_matrix <- tibble(level = new_rod_vector) %>%
#       left_join(full_implant_df) %>%
#       select(x, y) %>%
#       arrange(rev(y)) %>%
#       distinct() %>%
#       mutate(x = x + offset_value*0.8) %>%
#       as.matrix()
#     
#     main_rod_matrix <- full_implant_df %>%
#       anti_join(tibble(level = new_rod_vector)) %>%
#       select(x, y) %>% 
#       mutate(x = x - offset_value*0.5) %>%
#       arrange(rev(y)) %>%
#       distinct() %>%
#       as.matrix()
#     
#     sat_rod_sf <- st_buffer(st_linestring(satellite_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
#     main_rod_sf <- st_buffer(st_linestring(main_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
#     
#     additional_rod_sf <- st_multipolygon(list(main_rod_sf, sat_rod_sf))
#   }
#   
#   if(rod_type == "intercalary"){
#     overlap_rod_df <- tibble(level = new_rod_vector) %>%
#       left_join(full_implant_df) %>%
#       select(x, y) %>%
#       arrange(rev(y)) %>%
#       distinct()
#     
#     bottom_of_proximal_rod_y <- median(overlap_rod_df$y)
#     
#     upper_rod_df <- full_implant_df %>%
#       filter(y >= bottom_of_proximal_rod_y) %>%
#       select(x, y)
#     
#     lower_rod_df <- full_implant_df %>%
#       filter(y < bottom_of_proximal_rod_y) %>%
#       select(x, y) 
#     
#     upper_lower_span <- abs(min(upper_rod_df$y[1]) - max(lower_rod_df$y[1]))
#     
#     upper_rod_matrix <- upper_rod_df %>%
#       mutate(y = if_else(y == min(y), y - 0.01, y)) %>%
#       as.matrix() 
#     
#     lower_rod_matrix <- lower_rod_df %>%
#       mutate(y = if_else(y == max(y), y + 0.01, y)) %>%
#       as.matrix() 
#     
#     
#     bottom_connector <- tibble(x = c(lower_rod_df$x[1], 
#                                   lower_rod_df$x[1] - offset_value*1.1), 
#                             y = (max(lower_rod_df$y)*1.02)) %>%
#       select(x, y) %>%
#       as.matrix()
#     
#     top_connector <- tibble(x = c(upper_rod_df$x[1], 
#                                      upper_rod_df$x[1] - offset_value*1.1), 
#                                y = (min(upper_rod_df$y)*0.98)) %>%
#       select(x, y) %>%
#       as.matrix()
#   
#     
#     additional_rod_matrix <- overlap_rod_df %>%
#       mutate(x = x - offset_value) %>%
#       as.matrix()
#     
#     upper_rod <- st_buffer(st_linestring(upper_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
#     lower_rod <- st_buffer(st_linestring(lower_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
#     rod_top_connector <- st_buffer(st_linestring(top_connector), dist = 0.003, endCapStyle = "ROUND")
#     connecting_rod <- st_buffer(st_linestring(additional_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
#     rod_bottom_connector <- st_buffer(st_linestring(bottom_connector), dist = 0.003, endCapStyle = "ROUND")
#     
#     additional_rod_sf <- st_multipolygon(list(upper_rod,lower_rod, connecting_rod, rod_top_connector, rod_bottom_connector))
#     
#   }
#   
#   if(rod_type == "linked"){
#     overlap_df <- tibble(level = new_rod_vector) %>%
#       left_join(full_implant_df) %>%
#       select(level, vertebral_number, x, y) %>%
#       arrange(vertebral_number) %>%
#       # arrange(rev(y)) %>%
#       distinct()
#     
#     top_of_distal_rod_y <- max(overlap_df$y)
#     
#     bottom_of_proximal_rod_y <- min(overlap_df$y)
# 
#     
#     overlap_midpoint_y <- mean(overlap_df$y)
#     
#     if(full_implant_df$x[1] > 0.5){
#       upper_rod_df <- full_implant_df %>%
#         group_by(as.character(vertebral_number)) %>%
#         filter(x == min(x)) %>%
#         ungroup() %>%
#         filter(y >= bottom_of_proximal_rod_y) %>%
#         arrange(rev(y)) %>%
#         mutate(x = x + offset_value/2) %>%
#         select(x, y) %>%
#         ungroup()
#       
#       lower_rod_df <- full_implant_df %>%
#         group_by(as.character(vertebral_number)) %>%
#         filter(x == max(x)) %>%
#         ungroup() %>%
#         filter(y <= top_of_distal_rod_y) %>%
#         arrange(rev(y)) %>%
#         mutate(x = x - offset_value/2) %>%
#         select(x, y) %>%
#         ungroup()
#       
#     }else{
#       upper_rod_df <- full_implant_df %>%
#         group_by(as.character(vertebral_number)) %>%
#         filter(x == max(x)) %>%
#         ungroup() %>%
#         filter(y >= bottom_of_proximal_rod_y) %>%
#         arrange(rev(y)) %>%
#         mutate(x = x + offset_value/2) %>%
#         select(x, y) %>%
#         ungroup()
#       
#       lower_rod_df <- full_implant_df %>%
#         group_by(as.character(vertebral_number)) %>%
#         filter(x == min(x)) %>%
#         ungroup() %>%
#         filter(y <= top_of_distal_rod_y) %>%
#         arrange(rev(y)) %>%
#         mutate(x = x - offset_value/2) %>%
#         select(x, y) %>%
#         ungroup()
#       
# 
#     }
#     
#     upper_rod_matrix <- upper_rod_df%>%
#       as.matrix() 
#     
#     lower_rod_matrix <- lower_rod_df%>%
#       as.matrix() 
#     
# 
#     top_connector <- tibble(x = c(lower_rod_df$x[1], 
#                                   lower_rod_df$x[1] + offset_value), 
#                             y = (max(lower_rod_df$y)-0.01)) %>%
#       select(x, y) %>%
#       as.matrix()
#     
#     bottom_connector <- tibble(x = c(upper_rod_df$x[length(upper_rod_df$x)], 
#                                      upper_rod_df$x[length(upper_rod_df$x)] - offset_value), 
#                             y = (min(upper_rod_df$y)+0.01)) %>%
#       select(x, y) %>%
#       as.matrix()
#     
#     upper_rod <- st_buffer(st_linestring(upper_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
#     lower_rod <- st_buffer(st_linestring(lower_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
#     top_connector <- st_buffer(st_linestring(top_connector), dist = 0.003, endCapStyle = "ROUND")
#     bottom_connector <- st_buffer(st_linestring(bottom_connector), dist = 0.003, endCapStyle = "ROUND")
#     
#     additional_rod_sf <- st_multipolygon(list(upper_rod, lower_rod, top_connector, bottom_connector))
#   }
#   return(additional_rod_sf)
# }









######### ANTERIOR IMPLANTS ###############
# Anterior Arthroplasty_function
# arthroplasty_function <- function(y_for_inferior_endplate, y_for_superior_endplate, endplate_width){
#   endplate_height <- y_for_inferior_endplate - y_for_superior_endplate
#   
#   left_bottom_point <- c(0.5 - endplate_width/2, y_for_superior_endplate)
#   right_bottom_point <- c(0.5 + endplate_width/2, y_for_superior_endplate)
#   bottom_top_point <- c(0.5, y_for_superior_endplate + endplate_height/2)
#   
#   top_left_point <- c(0.5 - endplate_width/2, y_for_inferior_endplate)
#   top_right_point <- c(0.5 + endplate_width/2, y_for_inferior_endplate)
#   
#   bottom_half_sf <- st_buffer(st_polygon(list(rbind(left_bottom_point, right_bottom_point, bottom_top_point, left_bottom_point))), dist = endplate_height*0.2)
#   
#   full_disc_sf <- st_buffer(st_polygon(list(rbind(left_bottom_point, right_bottom_point, top_right_point, top_left_point, left_bottom_point))), dist = endplate_height*0.1)
#   
#   top_disc_buff <- st_buffer(st_difference(x = full_disc_sf, y = bottom_half_sf), dist = -0.0004)
#   
#   # disc_total <- st_union(x = top_disc_buff, y = bottom_half_sf)
#   
#   disc_list <- list(top = top_disc_buff, bottom = st_buffer(bottom_half_sf, dist = -0.0004))
#   
#   return(disc_list)
# }

arthroplasty_function <- function(y_for_inferior_endplate, y_for_superior_endplate, endplate_width){
  endplate_height <- y_for_inferior_endplate - y_for_superior_endplate
  
  # bottom_circle_point <- st_point(c(0.5, y_for_inferior_endplate))
  
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



anterior_implant_function <- function(object_type, 
                                      body_width, 
                                      y_click,
                                      superior_endplate_y, 
                                      inferior_endplate_y, 
                                      superior_endplate_inferior_body_y = NULL,
                                      inferior_endplate_superior_body_y = NULL){
  
  left_body_x <- 0.5 - (body_width*0.9)
  right_body_x <- 0.5 + (body_width*0.9)
  left_cage_x <- 0.5 - (body_width*0.35)
  right_cage_x <- 0.5 + (body_width*0.35)
  
  if(object_type == "anterior_disc_arthroplasty"){
    object_sf <- arthroplasty_function(y_for_inferior_endplate = inferior_endplate_y, y_for_superior_endplate = superior_endplate_y, endplate_width = body_width)
  }
  
  if(object_type == "corpectomy_cage"){
    left_x <- left_cage_x
    right_x <- right_cage_x
    
    bottom_left <- c(left_x, inferior_endplate_y)
    bottom_far_left <- c(left_body_x, superior_endplate_inferior_body_y)
    bottom_far_right <- c(right_body_x, superior_endplate_inferior_body_y)
    bottom_right <- c(right_x, inferior_endplate_y)
    top_right <- c(right_x, superior_endplate_y)
    top_far_right <- c(right_body_x, inferior_endplate_superior_body_y)
    top_far_left <- c(left_body_x, inferior_endplate_superior_body_y)
    top_left <- c(left_x, superior_endplate_y)
    

    
    
    object_sf <- st_buffer(st_polygon(list(rbind(bottom_left,
                                                 bottom_far_left,
                                                 bottom_far_right,
                                                 bottom_right,
                                                 top_right, 
                                                 top_far_right,
                                                 top_far_left,
                                                 top_left, 
                                                 bottom_left))), dist = 0.002, endCapStyle = "FLAT")
  }
  
  if(object_type == "anterior_plate" | object_type == "anterior_buttress_plate"){
    left_x <- left_cage_x
    right_x <- right_cage_x
    
    bottom_left <- c(left_x, inferior_endplate_y)
    bottom_right <- c(right_x, inferior_endplate_y)
    top_right <- c(right_x, superior_endplate_y)
    top_left <- c(left_x, superior_endplate_y)
    
    object_sf <- st_buffer(st_polygon(list(rbind(bottom_left,bottom_right,top_right, top_left, bottom_left))), dist = 0.0015, endCapStyle = "ROUND")
  }
  
  if(object_type == "corpectomy"){
    left_x <- left_body_x
    right_x <- right_body_x
    
    bottom_left <- c(left_x, inferior_endplate_y)
    bottom_right <- c(right_x, inferior_endplate_y)
    top_right <- c(right_x, superior_endplate_y)
    top_left <- c(left_x, superior_endplate_y)
    
    object_sf <- st_buffer(st_polygon(list(rbind(bottom_left,bottom_right,top_right, top_left, bottom_left))), dist = 0.0015, endCapStyle = "ROUND")
  }
  
  if(object_type == "diskectomy_fusion"){
    left_x <- 0.5 - (body_width*0.8)
    right_x <- 0.5 + (body_width*0.8)
    
    bottom_left <- c(left_x, inferior_endplate_y)
    bottom_right <- c(right_x, inferior_endplate_y)
    top_right <- c(right_x, superior_endplate_y)
    top_left <- c(left_x, superior_endplate_y)
    
    bottom_left_interbody <- c(left_cage_x, inferior_endplate_y)
    bottom_right_interbody  <- c(right_cage_x, inferior_endplate_y)
    top_right_interbody  <- c(right_cage_x, superior_endplate_y)
    top_left_interbody  <- c(left_cage_x, superior_endplate_y)
    
    object_sf <- st_buffer(st_polygon(list(rbind(bottom_left,bottom_right,top_right, top_left, bottom_left))), dist = 0.002, endCapStyle = "FLAT")
    
  }
  
  if(object_type == "decompression_diskectomy_fusion"){
    left_x <- 0.5 - (body_width*0.8)
    right_x <- 0.5 + (body_width*0.8)
    
    bottom_left <- c(left_x, inferior_endplate_y)
    bottom_right <- c(right_x, inferior_endplate_y)
    top_right <- c(right_x, superior_endplate_y)
    top_left <- c(left_x, superior_endplate_y)
    
    bottom_left_interbody <- c(left_cage_x, inferior_endplate_y)
    bottom_right_interbody  <- c(right_cage_x, inferior_endplate_y)
    top_right_interbody  <- c(right_cage_x, superior_endplate_y)
    top_left_interbody  <- c(left_cage_x, superior_endplate_y)
    
    object_sf <- st_buffer(st_polygon(list(rbind(bottom_left,bottom_right,top_right, top_left, bottom_left))), dist = 0.002, endCapStyle = "FLAT")
    
  }
  
  if(object_type == "diskectomy_fusion_no_interbody_device"){
    left_x <- 0.5 - (body_width*1.1)
    right_x <- 0.5 + (body_width*1.1)
    
    bottom_left <- c(left_x, inferior_endplate_y)
    bottom_right <- c(right_x, inferior_endplate_y)
    top_right <- c(right_x, superior_endplate_y)
    top_left <- c(left_x, superior_endplate_y)
    
    object_sf <- st_buffer(st_polygon(list(rbind(bottom_left,bottom_right,top_right, top_left, bottom_left))), dist = 0.003, endCapStyle = "FLAT")
  }
  
  # if(object_type == "screw_washer"){
  #   object_sf <- st_buffer(st_point(c(0.5, inferior_endplate_y)),dist = 0.01, endCapStyle = "ROUND")
  # }
  if(object_type == "screw_washer"){
    object_sf <- st_buffer(st_point(c(0.5, y_click)),dist = 0.01, endCapStyle = "ROUND")
  }
  
  return(object_sf)
}




make_screw_sizes_ui_function <-  function(level = NULL, left_screw_level = "no_screw", right_screw_level = "no_screw", left_selected = "Unknown", right_selected = "Unknown"){
  
  tags$tr(width = "100%", 
          tags$td(width = "10%", div(style = "font-size:14px; font-weight:bold; text-align:center; padding-bottom:10px",   paste(level))),
          tags$td(width = "10%", div(id = "my_small_text_input", 
                                     textInput(inputId = glue("left_{level}_screw_diameter"), 
                                               label = NULL, 
                                               placeholder = "D",
                                               width = "90%"))
          ),
          tags$td(width = "10%", div(id = "my_small_text_input", 
                                     textInput(inputId = glue("left_{level}_screw_length"), 
                                               label = NULL, 
                                               placeholder = "L",
                                               width = "90%"))
          ),
          tags$td(width = "10%", div(id = "my_small_text_input", 
                                     textInput(inputId = glue("right_{level}_screw_diameter"), 
                                               label = NULL, 
                                               placeholder = "D",
                                               width = "90%"))
          ),
          tags$td(width = "10%", div(id = "my_small_text_input", 
                                     textInput(inputId = glue("right_{level}_screw_length"), 
                                               label = NULL, 
                                               placeholder = "L",
                                               width = "90%"))
          )
  )
  
}

make_screw_types_function <-  function(level = NULL, left_screw_level = "no_screw", right_screw_level = "no_screw", left_selected = "Unknown", right_selected = "Unknown"){
  
  
  if(left_screw_level != "no_screw"){
    left_ui <- radioGroupButtons("option2",
                                 inputId = left_screw_level,
                                 label = NULL,
                                 choices = c("M", "U", "P", "Red", "Offset"),
                                 selected = left_selected,
                                 size = "xs",
                                 justified = TRUE,
                                 width = "95%"
    )
  }else{
    left_ui <- NULL
  }
  
  if(right_screw_level != "no_screw"){
    right_ui <- radioGroupButtons("option2",
                                  inputId = right_screw_level,
                                  label = NULL,
                                  choices = c("M", "U", "P", "Red", "Offset"),
                                  selected = right_selected,
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