#### BUILDING OBJECTS FUNCTIONS

#############-----------------------   Build: xxxxxxxxxxxx  ----------------------###############


#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############
#############-----------------------   POSTERIOR  ----------------------###############

#############-----------------------   Build: screws  ----------------------###############

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


#############-----------------------   Build: TP HOOKS  ----------------------###############

hook_tp_function <- function(hook_start_x, 
                             hook_edge_y,
                             downgoing_y_start_modifier= 0,
                             hook_length_modifier = 1, 
                             hook_width_modifier = 1, 
                             hook_thickness_percent_modifier = 1){
  
  hook_length <- 0.005*hook_length_modifier
  # 
  hook_width <- 0.006*hook_width_modifier
  
  hook_thickness <- 0.002*hook_thickness_percent_modifier
  
  ### #### #### #### #### ##### LEFT hook #### #### #### #### #### #### #### ####
  ### TP HOOK ####
  
  if(hook_start_x < 0.5){
    
    left_downgoing_hook_connector_top <- c(hook_start_x, hook_edge_y + downgoing_y_start_modifier)
    
    left_downgoing_hook_connector_bottom <- c(left_downgoing_hook_connector_top[1], left_downgoing_hook_connector_top[2] - hook_length*1.8)
    
    left_downgoing_hook_top <- c(left_downgoing_hook_connector_top[1] - hook_width, left_downgoing_hook_connector_top[2])
    
    left_downgoing_hook_bottom <- c(left_downgoing_hook_top[1], left_downgoing_hook_connector_top[2] - hook_length)
    
    left_downgoing_hook <- st_linestring(rbind(left_downgoing_hook_bottom, left_downgoing_hook_top, left_downgoing_hook_connector_top, left_downgoing_hook_connector_bottom))
    tp_hook_sf <- st_buffer(x = left_downgoing_hook, dist = hook_thickness, endCapStyle = "ROUND")
    
  }
  
  if(hook_start_x > 0.5){
    
    right_downgoing_hook_connector_top <- c(hook_start_x, hook_edge_y + downgoing_y_start_modifier)
    
    right_downgoing_hook_connector_bottom <- c(right_downgoing_hook_connector_top[1], right_downgoing_hook_connector_top[2] - hook_length*1.8)
    
    right_downgoing_hook_top <- c(right_downgoing_hook_connector_top[1] + hook_width, right_downgoing_hook_connector_top[2])
    
    right_downgoing_hook_bottom <- c(right_downgoing_hook_top[1], right_downgoing_hook_connector_top[2] - hook_length)
    
    right_downgoing_hook <- st_linestring(rbind(right_downgoing_hook_bottom, right_downgoing_hook_top, right_downgoing_hook_connector_top, right_downgoing_hook_connector_bottom))
    
    tp_hook_sf <- st_buffer(x = right_downgoing_hook, dist = hook_thickness, endCapStyle = "ROUND")
  }
  return(tp_hook_sf)
  
}



#############-----------------------   Build: Laminar Hooks  ----------------------###############
hook_laminar_function <- function(hook_direction, 
                                  hook_start_x, 
                                  hook_edge_y,
                                  downgoing_y_start_modifier= 0,
                                  hook_length_modifier = 1, 
                                  hook_width_modifier = 1, 
                                  hook_thickness_percent_modifier = 1){
  
  hook_length <- 0.005*hook_length_modifier
  # 
  hook_width <- 0.006*hook_width_modifier
  
  hook_thickness <- 0.002*hook_thickness_percent_modifier
  
  ### #### #### #### #### ##### LEFT hook #### #### #### #### #### #### #### ####
  
  if(hook_start_x < 0.5){
    if(hook_direction == "downgoing"){
      
      left_downgoing_hook_connector_top <- c(hook_start_x, hook_edge_y)
      
      left_downgoing_hook_connector_bottom <- c(left_downgoing_hook_connector_top[1], left_downgoing_hook_connector_top[2] - hook_length*1.8)
      
      left_downgoing_hook_top <- c(left_downgoing_hook_connector_top[1] + hook_width, left_downgoing_hook_connector_top[2])
      
      left_downgoing_hook_bottom <- c(left_downgoing_hook_top[1], left_downgoing_hook_top[2] - hook_length)
      
      left_downgoing_hook <- st_linestring(rbind(left_downgoing_hook_bottom, left_downgoing_hook_top, left_downgoing_hook_connector_top, left_downgoing_hook_connector_bottom))
      laminar_hook_sf <- st_buffer(x = left_downgoing_hook, dist = hook_thickness, endCapStyle = "ROUND")
    }
    
    if(hook_direction == "upgoing"){
      
      left_upgoing_hook_connector_bottom <- c(hook_start_x, hook_edge_y)
      
      left_upgoing_hook_connector_top <- c(left_upgoing_hook_connector_bottom[1], left_upgoing_hook_connector_bottom[2] + hook_length*1.8)
      
      left_upgoing_hook_bottom <- c(left_upgoing_hook_connector_bottom[1] + hook_width, left_upgoing_hook_connector_bottom[2])
      
      left_upgoing_hook_top <- c(left_upgoing_hook_bottom[1], left_upgoing_hook_bottom[2] + hook_length)
      
      left_upgoing_hook <- st_linestring(rbind(left_upgoing_hook_connector_top, left_upgoing_hook_connector_bottom, left_upgoing_hook_bottom, left_upgoing_hook_top))
      laminar_hook_sf <- st_buffer(x = left_upgoing_hook, dist = hook_thickness, endCapStyle = "ROUND")
    }
  }
  
  if(hook_start_x > 0.5){
    if(hook_direction == "downgoing"){
      
      right_downgoing_hook_connector_top <- c(hook_start_x, hook_edge_y)
      
      right_downgoing_hook_connector_bottom <- c(right_downgoing_hook_connector_top[1], right_downgoing_hook_connector_top[2] - hook_length*1.8)
      
      right_downgoing_hook_top <- c(right_downgoing_hook_connector_top[1] - hook_width, right_downgoing_hook_connector_top[2])
      
      right_downgoing_hook_bottom <- c(right_downgoing_hook_top[1], right_downgoing_hook_top[2] - hook_length)
      
      right_downgoing_hook <- st_linestring(rbind(right_downgoing_hook_bottom, right_downgoing_hook_top, right_downgoing_hook_connector_top, right_downgoing_hook_connector_bottom))
      laminar_hook_sf <- st_buffer(x = right_downgoing_hook, dist = hook_thickness, endCapStyle = "ROUND")
    }
    
    if(hook_direction == "upgoing"){
      
      right_upgoing_hook_connector_bottom <- c(hook_start_x, hook_edge_y)
      
      right_upgoing_hook_connector_top <- c(right_upgoing_hook_connector_bottom[1], right_upgoing_hook_connector_bottom[2] + hook_length*1.8)
      
      right_upgoing_hook_bottom <- c(right_upgoing_hook_connector_bottom[1] - hook_width, right_upgoing_hook_connector_bottom[2])
      
      right_upgoing_hook_top <- c(right_upgoing_hook_bottom[1], right_upgoing_hook_bottom[2] + hook_length)
      
      
      right_upgoing_hook <- st_linestring(rbind(right_upgoing_hook_connector_top, right_upgoing_hook_connector_bottom, right_upgoing_hook_bottom, right_upgoing_hook_top))
      laminar_hook_sf <- st_buffer(x = right_upgoing_hook, dist = hook_thickness, endCapStyle = "ROUND")
    }
  }
  return(laminar_hook_sf)
  
}

#############-----------------------   Build: Pedicle Hook  ----------------------###############

build_pedicle_hook_function <- function(x_start, y_start){
  
  y_lenth <- 0.01
  
  x_width <- 0.012
  
  if(x_start < 0.5){
    start <- c(x_start, y_start)
    
    point_2 <- c(start[1], start[2] - y_lenth)
    
    point_3 <- c(start[1] + x_width*0.5, start[2] - y_lenth)
    
    point_4 <- c(point_3[1], point_3[2] + y_lenth*0.75)
    
    point_5 <- point_3
    
    point_6 <- c(point_2[1] + x_width, point_2[2])
    
    point_7 <- c(point_6[1], point_6[2] + y_lenth*0.75)
    
    hook <- st_linestring(rbind(start, point_3, point_4, point_5, point_6, point_7))
    
    hook_sf <- st_buffer(hook, dist = 0.002, endCapStyle = "ROUND")
  }
  
  if(x_start > 0.5){
    start <- c(x_start, y_start)
    
    point_2 <- c(start[1], start[2] - y_lenth)
    
    point_3 <- c(start[1] - x_width*0.5, start[2] - y_lenth)
    
    point_4 <- c(point_3[1], point_3[2] + y_lenth*0.75)
    
    point_5 <- point_3
    
    point_6 <- c(point_2[1] - x_width, point_2[2])
    
    point_7 <- c(point_6[1], point_6[2] + y_lenth*0.75)
    
    hook <- st_linestring(rbind(start, point_3, point_4, point_5, point_6, point_7))
    
    hook_sf <- st_buffer(hook, dist = 0.002, endCapStyle = "ROUND")
  }
  
  return(hook_sf)
  
}

#############-----------------------   Build: Tether  ----------------------###############
tether_function <- function(tether_start_y, tether_length){
  
  superior_start <- c(0.5, tether_start_y)
  
  superior_left <- superior_start - c(0.005, 0)
  
  superior_right <- superior_start + c(0.005, 0)
  
  inferior_center <- c(0.5, tether_start_y - tether_length)
  
  inferior_left <- inferior_center - c(0.005, 0)
  
  inferior_right <- inferior_center + c(0.005, 0)
  
  tether <- st_linestring(rbind(superior_left, inferior_right, inferior_left, superior_right, superior_left))
  
  tether_sf <- st_buffer(x = tether, dist = 0.001, endCapStyle = "ROUND")
  
  return(tether_sf)
  
}

#############-----------------------   Build: Sublaminar Band  ----------------------###############


build_sublaminar_band_function <- function(x_start, y_top, y_length){
  
  y_start <- y_top - y_length*0.5
  
  x_width <- 0.008
  
  if(x_start < 0.5){
    start <- c(x_start, y_start)
    point_2 <- c(start[1] + x_width, start[2] + y_length*0.5)
    point_3 <- start
    point_4 <- c(start[1] + x_width, start[2] - y_length*0.5)
    
    sublaminar_superior_point <- point_2
    
    sublaminar_inferior_point <- point_4
    
    connector <- st_buffer(st_linestring(rbind(start, point_2, point_3, point_4)), dist = .0015)
    
    # sublaminar_portion <- st_buffer(st_linestring(rbind(sublaminar_superior_point, sublaminar_inferior_point)), dist = .0005)
    
    sublaminar_portion <- st_buffer(st_linestring(rbind(start, point_2, point_4, start)), dist = .0005)
    
    sublaminar_wire_sf <- st_polygon(sublaminar_portion)  
  }
  
  if(x_start > 0.5){
    start <- c(x_start, y_start)
    point_2 <- c(start[1] - x_width, start[2] + y_length*0.5)
    point_3 <- start
    point_4 <- c(start[1] - x_width, start[2] - y_length*0.5)
    
    sublaminar_superior_point <- point_2
    
    sublaminar_inferior_point <- point_4
    
    connector <- st_buffer(st_linestring(rbind(start, point_2, point_3, point_4)), dist = .0015)
    
    # sublaminar_portion <- st_buffer(st_linestring(rbind(sublaminar_superior_point, sublaminar_inferior_point)), dist = .0005)
    
    sublaminar_portion <- st_buffer(st_linestring(rbind(start, point_2, point_4, start)), dist = .0001)
    
    # sublaminar_wire_sf <- st_polygon(st_union(connector, sublaminar_portion))  
    
    sublaminar_wire_sf <- st_polygon(sublaminar_portion)  
  }
  
  return(connector)
  
}

#############-----------------------   Build: screws/hooks/other combined  ----------------------###############

screw_hook_implant_function <- function(implant_type,
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


#############-----------------------   Build: OSTEOTOMY  ----------------------###############


build_osteotomy_function <- function(level, 
                                     x_click,
                                     osteotomy_grade,
                                     left_x, 
                                     superior_y,
                                     right_x,
                                     inferior_y,          
                                     superior_vert_lateral_pars_x, 
                                     superior_vert_inferior_pedicle_y, 
                                     superior_lamina_y, 
                                     superior_tp_y,
                                     lateral_tp_x,
                                     inferior_lamina_y,
                                     lateral_pars_x, 
                                     inferior_pedicle_y, 
                                     inferior_facet_lateral_border_x,
                                     inferior_facet_medial_border_x,
                                     inferior_facet_superior_border_y,
                                     inferior_facet_inferior_border_y
){
  if(osteotomy_grade == "grade_1"){
    if(str_detect(string = level, pattern = "L")){
      
      start_point <- c(left_x, superior_y)
      end_point <- c(right_x, inferior_y)
      
      osteotomy_sf <- st_linestring(rbind(start_point, end_point))
    }else{
      
      start_point <- c(left_x, superior_y)
      mid_point <- c(right_x, superior_y)
      end_point <- c(right_x, inferior_y)
      
      osteotomy_sf <- st_linestring(rbind(start_point,mid_point, end_point))
    } 
  }
  
  if(osteotomy_grade == "grade_2"){
    
    start_point <- c(left_x, superior_y)
    bottom_left <- c(left_x, inferior_y)
    mid_point_left <- c(right_x, inferior_y)
    mid_point_right <- c(1-right_x, inferior_y)
    bottom_right <- c(1 - left_x, inferior_y)
    end_point <- c(1 - left_x, superior_y)
    
    osteotomy_sf <- st_polygon(list(rbind(start_point,
                                          bottom_left, 
                                          mid_point_left,
                                          mid_point_right,
                                          bottom_right,
                                          end_point, 
                                          mid_point_right,
                                          mid_point_left, 
                                          start_point)))
    
    # osteotomy_sf <- st_linestring(rbind(start_point,mid_point_left,mid_point_right, end_point))
    
    osteotomy_sf <- st_buffer(osteotomy_sf, dist = 0.002, endCapStyle = "ROUND")
    
  }
  
  if(osteotomy_grade == "complete_facetectomy"){
    if(x_click < 0.5){
      #start top left and then work in counterclockwise
      point_1 <- c(inferior_facet_lateral_border_x, inferior_facet_superior_border_y)
      point_2 <- c(inferior_facet_lateral_border_x, inferior_facet_inferior_border_y)
      point_3 <- c(inferior_facet_medial_border_x, inferior_facet_inferior_border_y)
      point_4 <- c(inferior_facet_medial_border_x, inferior_facet_superior_border_y)
    }else{
      #start top left and then work in counterclockwise
      point_1 <- c(1- inferior_facet_lateral_border_x, inferior_facet_superior_border_y)
      point_2 <- c(1 - inferior_facet_lateral_border_x, inferior_facet_inferior_border_y)
      point_3 <- c(1 - inferior_facet_medial_border_x, inferior_facet_inferior_border_y)
      point_4 <- c(1 - inferior_facet_medial_border_x, inferior_facet_superior_border_y)
    }
    
    
    
    osteotomy_sf <- st_linestring(rbind(point_1,
                                        point_2,
                                        point_3,
                                        point_4,
                                        point_1))
    
    osteotomy_sf <- st_buffer(st_polygon(list(osteotomy_sf)), dist = 0.0025, endCapStyle = "ROUND")
  }
  
  if(osteotomy_grade == "grade_3" | osteotomy_grade == "grade_4" | osteotomy_grade == "grade_5"){
    
    #start top left and then work in counterclockwise
    point_1 <- c(superior_vert_lateral_pars_x, superior_vert_inferior_pedicle_y)
    point_2 <- c(superior_vert_lateral_pars_x, superior_lamina_y)
    point_3 <- c(lateral_tp_x, superior_lamina_y)
    point_4 <- c(lateral_tp_x, inferior_pedicle_y)
    point_5 <- c(lateral_pars_x, inferior_pedicle_y)
    point_6 <- c(lateral_pars_x, inferior_lamina_y)
    point_7 <- c(1-lateral_pars_x, inferior_lamina_y)
    point_8 <- c(1-lateral_pars_x, inferior_pedicle_y)
    point_9 <- c(1-lateral_tp_x, inferior_pedicle_y)
    point_10 <- c(1-lateral_tp_x, superior_lamina_y)
    point_11 <- c(1-superior_vert_lateral_pars_x, superior_lamina_y)
    point_12 <- c(1-superior_vert_lateral_pars_x, superior_vert_inferior_pedicle_y)
    
    osteotomy_sf <- st_linestring(rbind(point_1,
                                        point_2,
                                        point_3,
                                        point_4,
                                        point_5,
                                        point_6,
                                        point_7,
                                        point_8,
                                        point_9,
                                        point_10,
                                        point_11,
                                        point_12,
                                        point_1))
    
    osteotomy_sf <- st_buffer(st_polygon(list(osteotomy_sf)), dist = 0.005, endCapStyle = "ROUND")
  }
  return(osteotomy_sf)
}


#############-----------------------   Build: DECOMPRESSIONS  ----------------------###############


build_decompression_function <- function(left_x, right_x, superior_y, inferior_y, top_width, object="x", x_lateral_pars, y_inferior_tp, side, inferior_pedicle_y){
  
  if(object == "laminoplasty"){
    object_start <- c(x_lateral_pars+0.01, inferior_pedicle_y + 0.005)
    point_2 <- c(x_lateral_pars + 0.014, inferior_pedicle_y)
    point_3 <- c(x_lateral_pars + 0.01, inferior_pedicle_y - 0.005)
    point_4 <- c(x_lateral_pars + 0.014, inferior_pedicle_y)
    point_5 <- c(x_lateral_pars + 0.017, inferior_pedicle_y)
    point_6 <- c(x_lateral_pars + 0.02, inferior_pedicle_y-0.003)
    
    decompression_sf <- st_buffer(x = st_linestring(rbind(object_start, point_2, point_3, point_4, point_5, point_6)), dist = 0.003, endCapStyle = "ROUND")
    
  }
  
  if(object == "transpedicular_approach"){
    if(side == "right"){
      object_center <- c(1-x_lateral_pars, y_inferior_tp)
    }else{
      object_center <- c(x_lateral_pars, y_inferior_tp)
    }
    decompression_sf <- st_buffer(x = st_point(object_center), dist = 0.013, endCapStyle = "ROUND")
  }
  
  
  if(object == "costovertebral_approach" | object == "costotransversectomy"){
    if(side == "right"){
      object_center <- c(1-x_lateral_pars, y_inferior_tp)
      decompression <- st_linestring(rbind(object_center, c(object_center[[1]] + 0.02, object_center[[2]])))
    }else{
      object_center <- c(x_lateral_pars, y_inferior_tp)
      decompression <- st_linestring(rbind(object_center, c(object_center[[1]] - 0.02, object_center[[2]])))
    }
    decompression_sf <- st_buffer(decompression, dist = 0.013, endCapStyle = "ROUND")
  }
  
  if(object == "laminectomy" | object == "sublaminar_decompression" | object == "laminotomy" | object == "diskectomy"){
    bottom_width <- right_x - left_x
    
    top_width_difference = bottom_width - top_width
    
    bottom_left <- c(left_x, inferior_y)
    bottom_right <- c(right_x, inferior_y)
    top_left <- c(left_x + 0.5*top_width_difference, superior_y)
    top_right <- c(right_x - 0.5*top_width_difference, superior_y)
    
    decompression <- st_linestring(rbind(bottom_left,bottom_right,top_right, top_left, bottom_left))
    
    decompression_sf <- st_buffer(x = st_polygon(list(decompression)), dist = 0.0025, endCapStyle = "ROUND")  
  }
  
  
  return(decompression_sf)
}


#############-----------------------   Build: INTERBODY  ----------------------###############

# build_interbody_function <- function(left_x, right_x, superior_y, inferior_y, top_width, object = "xx"){

build_interbody_function <- function(object = "xx",
                                     x_click = 0.5,
                                     left_x, 
                                     right_x,
                                     superior_y, 
                                     inferior_y,
                                     top_width,
                                     inferior_facet_lateral_border_x = 0.47,
                                     inferior_facet_medial_border_x = 0.49,
                                     inferior_facet_superior_border_y = 0.5,
                                     inferior_facet_inferior_border_y = 0.5){

  if(object == "intervertebral_cage"){
    left_body_x <- left_x
    right_body_x <- right_x 
    left_cage_x <- left_x + 0.01
    right_cage_x <- right_x - 0.01
    
    inferior_endplate_y <- inferior_y
    lowest_y <- inferior_y - 0.005
    
    superior_endplate_y <- superior_y - 0.005
    highest_y <- superior_y 
    
    bottom_left <- c(left_cage_x, inferior_endplate_y)
    bottom_far_left <- c(left_body_x, lowest_y)
    bottom_far_right <- c(right_body_x, lowest_y)
    bottom_right <- c(right_cage_x, inferior_endplate_y)
    top_right <- c(right_cage_x, superior_endplate_y)
    top_far_right <- c(right_body_x, highest_y)
    top_far_left <- c(left_body_x, highest_y)
    top_left <- c(left_cage_x, superior_endplate_y)
    
    interbody_sf <- st_buffer(st_polygon(list(rbind(bottom_left,
                                                    bottom_far_left,
                                                    bottom_far_right,
                                                    bottom_right,
                                                    top_right, 
                                                    top_far_right,
                                                    top_far_left,
                                                    top_left, 
                                                    bottom_left))), dist = 0.001, endCapStyle = "FLAT")
    
  }
  
  if(object == "tlif" | object == "no_implant_interbody_fusion"){
    if(x_click < 0.5){
      #start top left and then work in counterclockwise
      point_1 <- c(inferior_facet_lateral_border_x, inferior_facet_superior_border_y)
      point_2 <- c(inferior_facet_lateral_border_x, inferior_facet_inferior_border_y)
      point_3 <- c(inferior_facet_medial_border_x, inferior_facet_inferior_border_y)
      point_4 <- c(inferior_facet_medial_border_x, inferior_facet_superior_border_y)
    }else{
      #start top left and then work in counterclockwise
      point_1 <- c(1- inferior_facet_lateral_border_x, inferior_facet_superior_border_y)
      point_2 <- c(1 - inferior_facet_lateral_border_x, inferior_facet_inferior_border_y)
      point_3 <- c(1 - inferior_facet_medial_border_x, inferior_facet_inferior_border_y)
      point_4 <- c(1 - inferior_facet_medial_border_x, inferior_facet_superior_border_y)
    }
    
    interbody <- st_linestring(rbind(point_1,
                                        point_2,
                                        point_3,
                                        point_4,
                                        point_1))
    
    interbody_sf <- st_buffer(st_polygon(list(interbody)), dist = 0.0025, endCapStyle = "ROUND")
  }
  
  if(object == "tlif" | object == "no_implant_interbody_fusion"){
    if(x_click < 0.5){
      #start top left and then work in counterclockwise
      point_1 <- c(inferior_facet_lateral_border_x, inferior_facet_superior_border_y)
      point_2 <- c(inferior_facet_lateral_border_x, inferior_facet_inferior_border_y)
      point_3 <- c(inferior_facet_medial_border_x, inferior_facet_inferior_border_y)
      point_4 <- c(inferior_facet_medial_border_x, inferior_facet_superior_border_y)
    }else{
      #start top left and then work in counterclockwise
      point_1 <- c(1- inferior_facet_lateral_border_x, inferior_facet_superior_border_y)
      point_2 <- c(1 - inferior_facet_lateral_border_x, inferior_facet_inferior_border_y)
      point_3 <- c(1 - inferior_facet_medial_border_x, inferior_facet_inferior_border_y)
      point_4 <- c(1 - inferior_facet_medial_border_x, inferior_facet_superior_border_y)
    }
    
    interbody <- st_linestring(rbind(point_1,
                                        point_2,
                                        point_3,
                                        point_4,
                                        point_1))
    
    interbody_sf <- st_buffer(st_polygon(list(interbody)), dist = 0.0025, endCapStyle = "ROUND")
  }
  
  if(object == "plif"){
    if(x_click < 0.5){
      #start top left and then work in counterclockwise
      point_1 <- c(0.485, inferior_facet_superior_border_y)
      point_2 <- c(0.485, inferior_facet_inferior_border_y)
      point_3 <- c(0.495, inferior_facet_inferior_border_y)
      point_4 <- c(0.495, inferior_facet_superior_border_y)
    }else{
      #start top left and then work in counterclockwise
      point_1 <- c(1- 0.485, inferior_facet_superior_border_y)
      point_2 <- c(1 - 0.485, inferior_facet_inferior_border_y)
      point_3 <- c(1 - 0.495, inferior_facet_inferior_border_y)
      point_4 <- c(1 - 0.495, inferior_facet_superior_border_y)
    }
    
    interbody <- st_linestring(rbind(point_1,
                                     point_2,
                                     point_3,
                                     point_4,
                                     point_1))
    
    interbody_sf <- st_buffer(st_polygon(list(interbody)), dist = 0.0025, endCapStyle = "ROUND")
  }

  
  return(interbody_sf)
}



#############-----------------------   ANTERIOR  ----------------------###############
#############-----------------------   ANTERIOR  ----------------------###############
#############-----------------------   ANTERIOR  ----------------------###############
#############-----------------------   ANTERIOR  ----------------------###############
#############-----------------------   ANTERIOR  ----------------------###############


#############-----------------------   Build: arthroplasty  ----------------------###############

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

  if(object_type == "screw_washer"){
    object_sf <- st_buffer(st_point(c(0.5, y_click)),dist = 0.01, endCapStyle = "ROUND")
  }
  
  return(object_sf)
}
