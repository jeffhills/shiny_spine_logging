# schwab_points_df <- read_csv(file = "schwab_osteotomy.csv")


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


# 
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

# build_decompression_function <- function(left_x, right_x, superior_y, inferior_y, top_width, object="x", x_lateral_pars, y_inferior_tp, side){
#   
#   if(object == "laminoplasty"){
#     
#   }
# 
#   if(object == "transpedicular_approach"){
#     if(side == "right"){
#       object_center <- c(1-x_lateral_pars, y_inferior_tp)
#     }else{
#       object_center <- c(x_lateral_pars, y_inferior_tp)
#     }
#     decompression_sf <- st_buffer(x = st_point(object_center), dist = 0.013, endCapStyle = "ROUND")
#   }else{
#     if(object == "costovertebral_approach"){
#       if(side == "right"){
#           object_center <- c(1-x_lateral_pars, y_inferior_tp)
#         decompression <- st_linestring(rbind(object_center, c(object_center[[1]] + 0.02, object_center[[2]])))
#       }else{
#         object_center <- c(x_lateral_pars, y_inferior_tp)
#         decompression <- st_linestring(rbind(object_center, c(object_center[[1]] - 0.02, object_center[[2]])))
#       }
#       decompression_sf <- st_buffer(decompression, dist = 0.013, endCapStyle = "ROUND")
# 
#     }else{
#       bottom_width <- right_x - left_x
#       
#       top_width_difference = bottom_width - top_width
#       
#       bottom_left <- c(left_x, inferior_y)
#       bottom_right <- c(right_x, inferior_y)
#       top_left <- c(left_x + 0.5*top_width_difference, superior_y)
#       top_right <- c(right_x - 0.5*top_width_difference, superior_y)
#       
#       decompression <- st_linestring(rbind(bottom_left,bottom_right,top_right, top_left, bottom_left))
#       
#       decompression_sf <- st_buffer(x = st_polygon(list(decompression)), dist = 0.0025, endCapStyle = "ROUND")
#     }
#   }
#   return(decompression_sf)
# }

# build_interbody_function <- function(left_x, right_x, superior_y, inferior_y, top_width, object = "xx"){
#   
#   if(object == "intervertebral_cage"){
#     
#   }else{
#     bottom_width <- right_x - left_x
#     
#     top_width_difference = bottom_width - top_width
#     
#     bottom_left <- c(left_x, inferior_y)
#     bottom_right <- c(right_x, inferior_y)
#     top_left <- c(left_x + 0.5*top_width_difference, superior_y)
#     top_right <- c(right_x - 0.5*top_width_difference, superior_y)
#     
#     interbody <- st_linestring(rbind(bottom_left,bottom_right,top_right, top_left, bottom_left))
#     
#     interbody_sf <- st_buffer(x = st_polygon(list(interbody)), dist = 0.0025, endCapStyle = "ROUND")  
#   }
#   
#   
#   return(interbody_sf)
# }

build_interbody_function <- function(left_x, right_x, superior_y, inferior_y, top_width, object = "xx"){
  
  if(str_detect(string = object, pattern = "intervert")){
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
    
  }else{
    bottom_width <- right_x - left_x
    
    top_width_difference = bottom_width - top_width
    
    bottom_left <- c(left_x, inferior_y)
    bottom_right <- c(right_x, inferior_y)
    top_left <- c(left_x + 0.5*top_width_difference, superior_y)
    top_right <- c(right_x - 0.5*top_width_difference, superior_y)
    
    interbody <- st_linestring(rbind(bottom_left,bottom_right,top_right, top_left, bottom_left))
    
    interbody_sf <- st_buffer(x = st_polygon(list(interbody)), dist = 0.0025, endCapStyle = "ROUND")  
  }
  
  
  return(interbody_sf)
}
