
#################################################################

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
  
  
  #### #### #### #### RIGHT hook #### #### #### #### ####
  ### TP HOOK ####
  
  
  return(laminar_hook_sf)
  
}

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
