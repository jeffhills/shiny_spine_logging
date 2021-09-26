# screws_v3
library(shiny)
library(sf)
library(tidyverse)
library(ggplot2)
library(colourpicker)
library(kableExtra)
library(cowplot)
library(ggpubr)
library(magick)
library(ggpattern)
library(glue)
library(rlist)
library(janitor)


spine_png <- image_read(path = "posterior_spine_figure.png")

# left_label_x <- 0.23



occiput_y <- .917
c1_y <- .89
c2_y <- .865
c3_y <- .848
c4_y <- .831
c5_y <- .814
c6_y <- .797
c7_y <- .78
t1_y <- .755
t2_y <- .73
t3_y <- .705
t4_y <- .68
t5_y <- .65
t6_y <- .62
t7_y <- .59
t8_y <- .56
t9_y <- .525
t10_y <- .49
t11_y <- .455
t12_y <- .42
l1_y <- .38
l2_y <- .35
l3_y <- .31
l4_y <- .27
l5_y <- .237
s1_y <- .205
iliac_y <- .18
s2ai_y <- .15

occiput_x_offset <- .025
c1_x_offset <- .025
c2_x_offset <- .025
c3_x_offset <- .025
c4_x_offset <- .025
c5_x_offset <- .025
c6_x_offset <- .025
c7_x_offset <- .023
t1_x_offset <- .02
t2_x_offset <- .02
t3_x_offset <- .02
t4_x_offset <- .02
t5_x_offset <- .02
t6_x_offset <- .02
t7_x_offset <- .02
t8_x_offset <- .02
t9_x_offset <- .02
t10_x_offset <- .025
t11_x_offset <- .025
t12_x_offset <- .025
l1_x_offset <- .025
l2_x_offset <- .025
l3_x_offset <- .03
l4_x_offset <- .03
l5_x_offset <- .032
s1_x_offset <- .034
iliac_x_offset <- .05
s2ai_x_offset <- .03


levels_vector <- c("Occiput",
                   "C1",
                   "C2",
                   "C3",
                   "C4",
                   "C5",
                   "C6",
                   "C7",
                   "T1",
                   "T2",
                   "T3",
                   "T4",
                   "T5",
                   "T6",
                   "T7",
                   "T8",
                   "T9",
                   "T10",
                   "T11",
                   "T12",
                   "L1",
                   "L2",
                   "L3",
                   "L4",
                   "L5",
                   "S1",
                   "Iliac",
                   "S2AI")

vertebral_number_df <- tibble(level = c("Occiput",
                  "C1",
                  "C2",
                  "C3",
                  "C4",
                  "C5",
                  "C6",
                  "C7",
                  "T1",
                  "T2",
                  "T3",
                  "T4",
                  "T5",
                  "T6",
                  "T7",
                  "T8",
                  "T9",
                  "T10",
                  "T11",
                  "T12",
                  "L1",
                  "L2",
                  "L3",
                  "L4",
                  "L5",
                  "S1",
                  "Iliac",
                  "S2AI")) %>%
  mutate(vertebral_number = row_number()) %>%
  mutate(level = str_to_lower(level))


label_y_vector <- c(occiput_y, 
                    c1_y,
                    c2_y,
                    c3_y,
                    c4_y,
                    c5_y,
                    c6_y,
                    c7_y,
                    t1_y,
                    t2_y,
                    t3_y,
                    t4_y,
                    t5_y,
                    t6_y,
                    t7_y,
                    t8_y,
                    t9_y,
                    t10_y,
                    t11_y,
                    t12_y,
                    l1_y,
                    l2_y,
                    l3_y,
                    l4_y,
                    l5_y,
                    s1_y,
                    iliac_y,
                    s2ai_y)

label_x_offset <- c(occiput_x_offset, 
                    c1_x_offset,
                    c2_x_offset,
                    c3_x_offset,
                    c4_x_offset,
                    c5_x_offset,
                    c6_x_offset,
                    c7_x_offset,
                    t1_x_offset,
                    t2_x_offset,
                    t3_x_offset,
                    t4_x_offset,
                    t5_x_offset,
                    t6_x_offset,
                    t7_x_offset,
                    t8_x_offset,
                    t9_x_offset,
                    t10_x_offset,
                    t11_x_offset,
                    t12_x_offset,
                    l1_x_offset,
                    l2_x_offset,
                    l3_x_offset,
                    l4_x_offset,
                    l5_x_offset,
                    s1_x_offset,
                    iliac_x_offset,
                    s2ai_x_offset)


labels_df <- tibble("labels" = levels_vector) %>%
  mutate(x_left = left_label_x, 
         y = label_y_vector,
         x_right = right_label_x)


implant_start_x_offset <- 0.02

implant_start_df <- tibble("labels" = levels_vector) %>%
  mutate(y = label_y_vector, 
         left = 0.5 - implant_start_x_offset,
         right = 0.5 + implant_start_x_offset) %>%
  pivot_longer(cols = c(left, right), names_to = "side", values_to = "x") %>%
  select(labels, side, x, y)



############ SCREW FUNCTION ###########33

screw_function <- function(screw_start_x_offset, screw_start_y, screw_type, angle, screw_length_modifier = 1, screw_width_modifier = 1, y_scale = 1){
  
  rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  
  screw_length <- 0.02*screw_length_modifier
  
  screw_width <- 0.01*screw_width_modifier
  
  #### LEFT SCREW ####
  
  left_screw_start <- c(0.5 - screw_start_x_offset, screw_start_y)
  
  right_screw_start <- c(0.5 + screw_start_x_offset, screw_start_y)
  
  if(screw_type == "translaminar"){
    left_screw_start <- c(0.5 - screw_start_x_offset + screw_length, screw_start_y)
    
    right_screw_start <- c(0.5 + screw_start_x_offset - screw_length, screw_start_y)
  }
  
  screw_width <- case_when(
    screw_type == "pedicle" ~ screw_width/2,
    screw_type == "lateral mass" ~ screw_width/4,
    screw_type == "pars" ~ screw_width/4,
    screw_type == "translaminar" ~ screw_width/2,
    screw_type == "iliac" | screw_type == "s2ai" ~ screw_width/2 
  )
  
  screw_length <- case_when(
    screw_type == "pedicle" ~ screw_length,
    screw_type == "lateral mass" ~ screw_length/2,
    screw_type == "pars" ~ screw_length/2,
    screw_type == "translaminar" ~ screw_length,
    screw_type == "iliac" | screw_type == "s2ai" ~ screw_length 
  )
  
  left_point_1 <- st_point(c(left_screw_start[1], left_screw_start[2]))
  left_point_2 <- st_point(c(left_point_1[1] - screw_length, left_point_1[2] + screw_width))
  left_point_3 <- st_point(c(left_point_1[1] - screw_length, left_point_1[2] - screw_width))
  
  left_screw <- st_polygon(list(rbind(left_point_1, left_point_2, left_point_3, left_point_1)))
  
  left_screw_rotated <- (left_screw - left_point_1)*rot(angle*-1*pi/180) + left_point_1
  
  if(screw_type == "iliac" | screw_type == "s2ai"){
    left_screw_rotated <- left_screw_rotated - c(screw_length, +0.01)
  }
  
  left_screw_sf <- st_buffer(x = left_screw_rotated, dist = 0.001, endCapStyle = "SQUARE")
  
  left_screw_sf <- st_buffer(x = left_screw_sf, dist = 0.001, endCapStyle = "ROUND")
  
  
  left_screwhead_point_1 <- left_screw_rotated[[1]][2,]
  left_screwhead_point_2 <- left_screw_rotated[[1]][3,]
  
  left_screw_head <- c(mean(left_screwhead_point_1[1], left_screwhead_point_2[1]),
                       mean(left_screwhead_point_1[2], left_screwhead_point_2[2]))
  
  
  #### RIGHT SCREW ####
  
  right_point_1 <- st_point(c(right_screw_start[1], right_screw_start[2]))
  right_point_2 <- st_point(c(right_point_1[1] + screw_length, right_point_1[2] - screw_width))
  right_point_3 <- st_point(c(right_point_1[1] + screw_length, right_point_1[2] + screw_width))
  
  
  right_screw <- st_polygon(list(rbind(right_point_1, right_point_2, right_point_3, right_point_1)))
  
  
  right_screw_rotated <- (right_screw - right_point_1)*rot(angle*pi/180) + right_point_1
  
  if(screw_type == "iliac" | screw_type == "s2ai"){
    right_screw_rotated <- right_screw_rotated + c(screw_length, -0.01)
  }
  
  right_screw_sf <- st_buffer(x = right_screw_rotated, dist = 0.001, endCapStyle = "SQUARE")
  
  right_screw_sf <- st_buffer(x = right_screw_sf, dist = 0.001, endCapStyle = "ROUND")
  
  right_screwhead_point_1 <- right_screw_rotated[[1]][2,]
  right_screwhead_point_2 <- right_screw_rotated[[1]][3,]
  
  right_screw_head <- c(mean(right_screwhead_point_1[1], right_screwhead_point_2[1]), 
                        mean(right_screwhead_point_1[2], right_screwhead_point_2[2]))
  
  
  
  return(list(left_screw_sf = left_screw_sf, 
              left_screw_head = left_screw_head,
              right_screw_sf = right_screw_sf,
              right_screw_head = right_screw_head))
  
}


occiput_screw_screwhead <- screw_function(screw_start_x_offset = occiput_x_offset, screw_start_y = occiput_y, screw_type = "lateral mass", angle = 90)
c1_lm_screw_screwhead <- screw_function(screw_start_x_offset = c1_x_offset, screw_start_y = c1_y, screw_type = "lateral mass", angle = 75)
c2_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = c2_x_offset, screw_start_y = c2_y, screw_type = "pedicle", screw_length_modifier = 0.5, screw_width_modifier = 0.6, angle = 20)
c2_pars_screw_screwhead <- screw_function(screw_start_x_offset = c2_x_offset, screw_start_y = c2_y, screw_type = "pars", angle = 70)
c3_lm_screw_screwhead <- screw_function(screw_start_x_offset = c3_x_offset, screw_start_y = c3_y, screw_type = "lateral mass", angle = 120)
c3_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = c3_x_offset, screw_start_y = c3_y, screw_type = "pedicle", screw_length_modifier = 0.5, screw_width_modifier = 0.6, angle = 15)
c4_lm_screw_screwhead <- screw_function(screw_start_x_offset = c4_x_offset, screw_start_y = c4_y, screw_type = "lateral mass", angle = 120)
c4_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = c4_x_offset, screw_start_y = c4_y, screw_type = "pedicle", screw_length_modifier = 0.5, screw_width_modifier = 0.6, angle = 10)
c5_lm_screw_screwhead <- screw_function(screw_start_x_offset = c5_x_offset, screw_start_y = c5_y, screw_type = "lateral mass", angle = 120)
c5_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = c5_x_offset, screw_start_y = c5_y, screw_type = "pedicle", screw_length_modifier = 0.5, screw_width_modifier = 0.6, angle = 5)
c6_lm_screw_screwhead <- screw_function(screw_start_x_offset = c6_x_offset, screw_start_y = c6_y, screw_type = "lateral mass", angle = 120)
c6_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = c6_x_offset, screw_start_y = c6_y, screw_type = "pedicle", screw_length_modifier = 0.5, screw_width_modifier = 0.6, angle = 0)
c7_lm_screw_screwhead <- screw_function(screw_start_x_offset = c7_x_offset, screw_start_y = c7_y, screw_type = "lateral mass", angle = 120)
c7_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = c7_x_offset, screw_start_y = c7_y, screw_type = "pedicle", angle = -10)
t1_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = t1_x_offset, screw_start_y = t1_y, screw_type = "pedicle", angle = -10)
t2_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = t2_x_offset, screw_start_y = t2_y, screw_type = "pedicle", angle = -10)
t3_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = t3_x_offset, screw_start_y = t3_y, screw_type = "pedicle", angle = -10)
t4_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = t4_x_offset, screw_start_y = t4_y, screw_type = "pedicle", angle = -10)
t5_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = t5_x_offset, screw_start_y = t5_y, screw_type = "pedicle", angle = -10)
t6_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = t6_x_offset, screw_start_y = t6_y, screw_type = "pedicle", angle = -5)
t7_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = t7_x_offset, screw_start_y = t7_y, screw_type = "pedicle", angle = 0)
t8_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = t8_x_offset, screw_start_y = t8_y, screw_type = "pedicle", angle = 0)
t9_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = t9_x_offset, screw_start_y = t9_y, screw_type = "pedicle", angle = 0)
t10_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = t10_x_offset, screw_start_y = t10_y, screw_type = "pedicle", angle = 5)
t11_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = t11_x_offset, screw_start_y = t11_y, screw_type = "pedicle", angle = 10)
t12_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = t12_x_offset, screw_start_y = t12_y, screw_type = "pedicle", angle = 10)
l1_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = l1_x_offset, screw_start_y = l1_y, screw_type = "pedicle", angle = 5)
l2_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = l2_x_offset, screw_start_y = l2_y, screw_type = "pedicle", angle = 0)
l3_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = l3_x_offset, screw_start_y = l3_y, screw_type = "pedicle", angle = 0)
l4_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = l4_x_offset, screw_start_y = l4_y, screw_type = "pedicle", angle = -10)
l5_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = l5_x_offset, screw_start_y = l5_y, screw_type = "pedicle", angle = -20)
s1_pedicle_screw_screwhead <- screw_function(screw_start_x_offset = s1_x_offset, screw_start_y = s1_y, screw_type = "pedicle", angle = -30)
iliac_screw_screwhead <- screw_function(screw_start_x_offset = iliac_x_offset, screw_start_y = iliac_y, screw_type = "iliac",screw_length_modifier = 3, angle = -150)
s2ai_screw_screwhead <- screw_function(screw_start_x_offset = s2ai_x_offset, screw_start_y = s2ai_y, screw_type = "s2ai", screw_length_modifier = 3, angle = -155)


screw_list <- list(left_occiput_screw = occiput_screw_screwhead$left_screw_sf,
                   left_c1_lm_screw = c1_lm_screw_screwhead$left_screw_sf, 
                   left_c2_pedicle_screw = c2_pedicle_screw_screwhead$left_screw_sf,
                   left_c2_pars_screw = c2_pars_screw_screwhead$left_screw_sf,
                   left_c3_lm_screw = c3_lm_screw_screwhead$left_screw_sf,
                   left_c3_pedicle_screw = c3_pedicle_screw_screwhead$left_screw_sf,
                   left_c4_lm_screw = c4_lm_screw_screwhead$left_screw_sf,
                   left_c4_pedicle_screw = c4_pedicle_screw_screwhead$left_screw_sf,
                   left_c5_lm_screw = c5_lm_screw_screwhead$left_screw_sf,
                   left_c5_pedicle_screw = c5_pedicle_screw_screwhead$left_screw_sf,
                   left_c6_lm_screw = c6_lm_screw_screwhead$left_screw_sf,
                   left_c6_pedicle_screw = c6_pedicle_screw_screwhead$left_screw_sf,
                   left_c7_pedicle_screw = c7_pedicle_screw_screwhead$left_screw_sf,
                   left_t1_pedicle_screw = t1_pedicle_screw_screwhead$left_screw_sf, 
                   left_t2_pedicle_screw = t2_pedicle_screw_screwhead$left_screw_sf,
                   left_t3_pedicle_screw = t3_pedicle_screw_screwhead$left_screw_sf,
                   left_t4_pedicle_screw = t4_pedicle_screw_screwhead$left_screw_sf,
                   left_t5_pedicle_screw = t5_pedicle_screw_screwhead$left_screw_sf,
                   left_t6_pedicle_screw = t6_pedicle_screw_screwhead$left_screw_sf,
                   left_t7_pedicle_screw = t7_pedicle_screw_screwhead$left_screw_sf,
                   left_t8_pedicle_screw = t8_pedicle_screw_screwhead$left_screw_sf, 
                   left_t9_pedicle_screw = t9_pedicle_screw_screwhead$left_screw_sf,
                   left_t10_pedicle_screw = t10_pedicle_screw_screwhead$left_screw_sf,
                   left_t11_pedicle_screw = t11_pedicle_screw_screwhead$left_screw_sf,
                   left_t12_pedicle_screw = t12_pedicle_screw_screwhead$left_screw_sf,
                   left_l1_pedicle_screw = l1_pedicle_screw_screwhead$left_screw_sf, 
                   left_l2_pedicle_screw = l2_pedicle_screw_screwhead$left_screw_sf, 
                   left_l3_pedicle_screw = l3_pedicle_screw_screwhead$left_screw_sf, 
                   left_l4_pedicle_screw = l4_pedicle_screw_screwhead$left_screw_sf,
                   left_l5_pedicle_screw = l5_pedicle_screw_screwhead$left_screw_sf,
                   left_s1_pedicle_screw = s1_pedicle_screw_screwhead$left_screw_sf,
                   left_iliac_screw = iliac_screw_screwhead$left_screw_sf,
                   left_s2ai_screw = s2ai_screw_screwhead$left_screw_sf, 
                   right_occiput_screw = occiput_screw_screwhead$right_screw_sf,
                   right_c1_lm_screw = c1_lm_screw_screwhead$right_screw_sf, 
                   right_c2_pedicle_screw = c2_pedicle_screw_screwhead$right_screw_sf,
                   right_c2_pars_screw = c2_pars_screw_screwhead$right_screw_sf,
                   right_c3_lm_screw = c3_lm_screw_screwhead$right_screw_sf,
                   right_c3_pedicle_screw = c3_pedicle_screw_screwhead$right_screw_sf,
                   right_c4_lm_screw = c4_lm_screw_screwhead$right_screw_sf,
                   right_c4_pedicle_screw = c4_pedicle_screw_screwhead$right_screw_sf,
                   right_c5_lm_screw = c5_lm_screw_screwhead$right_screw_sf,
                   right_c5_pedicle_screw = c5_pedicle_screw_screwhead$right_screw_sf,
                   right_c6_lm_screw = c6_lm_screw_screwhead$right_screw_sf,
                   right_c6_pedicle_screw = c6_pedicle_screw_screwhead$right_screw_sf,
                   right_c7_pedicle_screw = c7_pedicle_screw_screwhead$right_screw_sf,
                   right_t1_pedicle_screw = t1_pedicle_screw_screwhead$right_screw_sf, 
                   right_t2_pedicle_screw = t2_pedicle_screw_screwhead$right_screw_sf,
                   right_t3_pedicle_screw = t3_pedicle_screw_screwhead$right_screw_sf,
                   right_t4_pedicle_screw = t4_pedicle_screw_screwhead$right_screw_sf,
                   right_t5_pedicle_screw = t5_pedicle_screw_screwhead$right_screw_sf,
                   right_t6_pedicle_screw = t6_pedicle_screw_screwhead$right_screw_sf,
                   right_t7_pedicle_screw = t7_pedicle_screw_screwhead$right_screw_sf,
                   right_t8_pedicle_screw = t8_pedicle_screw_screwhead$right_screw_sf, 
                   right_t9_pedicle_screw = t9_pedicle_screw_screwhead$right_screw_sf,
                   right_t10_pedicle_screw = t10_pedicle_screw_screwhead$right_screw_sf,
                   right_t11_pedicle_screw = t11_pedicle_screw_screwhead$right_screw_sf,
                   right_t12_pedicle_screw = t12_pedicle_screw_screwhead$right_screw_sf,
                   right_l1_pedicle_screw = l1_pedicle_screw_screwhead$right_screw_sf, 
                   right_l2_pedicle_screw = l2_pedicle_screw_screwhead$right_screw_sf, 
                   right_l3_pedicle_screw = l3_pedicle_screw_screwhead$right_screw_sf, 
                   right_l4_pedicle_screw = l4_pedicle_screw_screwhead$right_screw_sf,
                   right_l5_pedicle_screw = l5_pedicle_screw_screwhead$right_screw_sf,
                   right_s1_pedicle_screw = s1_pedicle_screw_screwhead$right_screw_sf,
                   right_iliac_screw = iliac_screw_screwhead$right_screw_sf,
                   right_s2ai_screw = s2ai_screw_screwhead$right_screw_sf)

screwhead_list <- list(left_occiput_screw = occiput_screw_screwhead$left_screw_head,
                   left_c1_lm_screw = c1_lm_screw_screwhead$left_screw_head, 
                   left_c2_pedicle_screw = c2_pedicle_screw_screwhead$left_screw_head,
                   left_c2_pars_screw = c2_pars_screw_screwhead$left_screw_head,
                   left_c3_lm_screw = c3_lm_screw_screwhead$left_screw_head,
                   left_c3_pedicle_screw = c3_pedicle_screw_screwhead$left_screw_head,
                   left_c4_lm_screw = c4_lm_screw_screwhead$left_screw_head,
                   left_c4_pedicle_screw = c4_pedicle_screw_screwhead$left_screw_head,
                   left_c5_lm_screw = c5_lm_screw_screwhead$left_screw_head,
                   left_c5_pedicle_screw = c5_pedicle_screw_screwhead$left_screw_head,
                   left_c6_lm_screw = c6_lm_screw_screwhead$left_screw_head,
                   left_c6_pedicle_screw = c6_pedicle_screw_screwhead$left_screw_head,
                   left_c7_pedicle_screw = c7_pedicle_screw_screwhead$left_screw_head,
                   left_t1_pedicle_screw = t1_pedicle_screw_screwhead$left_screw_head, 
                   left_t2_pedicle_screw = t2_pedicle_screw_screwhead$left_screw_head,
                   left_t3_pedicle_screw = t3_pedicle_screw_screwhead$left_screw_head,
                   left_t4_pedicle_screw = t4_pedicle_screw_screwhead$left_screw_head,
                   left_t5_pedicle_screw = t5_pedicle_screw_screwhead$left_screw_head,
                   left_t6_pedicle_screw = t6_pedicle_screw_screwhead$left_screw_head,
                   left_t7_pedicle_screw = t7_pedicle_screw_screwhead$left_screw_head,
                   left_t8_pedicle_screw = t8_pedicle_screw_screwhead$left_screw_head, 
                   left_t9_pedicle_screw = t9_pedicle_screw_screwhead$left_screw_head,
                   left_t10_pedicle_screw = t10_pedicle_screw_screwhead$left_screw_head,
                   left_t11_pedicle_screw = t11_pedicle_screw_screwhead$left_screw_head,
                   left_t12_pedicle_screw = t12_pedicle_screw_screwhead$left_screw_head,
                   left_l1_pedicle_screw = l1_pedicle_screw_screwhead$left_screw_head, 
                   left_l2_pedicle_screw = l2_pedicle_screw_screwhead$left_screw_head, 
                   left_l3_pedicle_screw = l3_pedicle_screw_screwhead$left_screw_head, 
                   left_l4_pedicle_screw = l4_pedicle_screw_screwhead$left_screw_head,
                   left_l5_pedicle_screw = l5_pedicle_screw_screwhead$left_screw_head,
                   left_s1_pedicle_screw = s1_pedicle_screw_screwhead$left_screw_head,
                   left_iliac_screw = iliac_screw_screwhead$left_screw_head,
                   left_s2ai_screw = s2ai_screw_screwhead$left_screw_head, 
                   right_occiput_screw = occiput_screw_screwhead$right_screw_head,
                   right_c1_lm_screw = c1_lm_screw_screwhead$right_screw_head, 
                   right_c2_pedicle_screw = c2_pedicle_screw_screwhead$right_screw_head,
                   right_c2_pars_screw = c2_pars_screw_screwhead$right_screw_head,
                   right_c3_lm_screw = c3_lm_screw_screwhead$right_screw_head,
                   right_c3_pedicle_screw = c3_pedicle_screw_screwhead$right_screw_head,
                   right_c4_lm_screw = c4_lm_screw_screwhead$right_screw_head,
                   right_c4_pedicle_screw = c4_pedicle_screw_screwhead$right_screw_head,
                   right_c5_lm_screw = c5_lm_screw_screwhead$right_screw_head,
                   right_c5_pedicle_screw = c5_pedicle_screw_screwhead$right_screw_head,
                   right_c6_lm_screw = c6_lm_screw_screwhead$right_screw_head,
                   right_c6_pedicle_screw = c6_pedicle_screw_screwhead$right_screw_head,
                   right_c7_pedicle_screw = c7_pedicle_screw_screwhead$right_screw_head,
                   right_t1_pedicle_screw = t1_pedicle_screw_screwhead$right_screw_head, 
                   right_t2_pedicle_screw = t2_pedicle_screw_screwhead$right_screw_head,
                   right_t3_pedicle_screw = t3_pedicle_screw_screwhead$right_screw_head,
                   right_t4_pedicle_screw = t4_pedicle_screw_screwhead$right_screw_head,
                   right_t5_pedicle_screw = t5_pedicle_screw_screwhead$right_screw_head,
                   right_t6_pedicle_screw = t6_pedicle_screw_screwhead$right_screw_head,
                   right_t7_pedicle_screw = t7_pedicle_screw_screwhead$right_screw_head,
                   right_t8_pedicle_screw = t8_pedicle_screw_screwhead$right_screw_head, 
                   right_t9_pedicle_screw = t9_pedicle_screw_screwhead$right_screw_head,
                   right_t10_pedicle_screw = t10_pedicle_screw_screwhead$right_screw_head,
                   right_t11_pedicle_screw = t11_pedicle_screw_screwhead$right_screw_head,
                   right_t12_pedicle_screw = t12_pedicle_screw_screwhead$right_screw_head,
                   right_l1_pedicle_screw = l1_pedicle_screw_screwhead$right_screw_head, 
                   right_l2_pedicle_screw = l2_pedicle_screw_screwhead$right_screw_head, 
                   right_l3_pedicle_screw = l3_pedicle_screw_screwhead$right_screw_head, 
                   right_l4_pedicle_screw = l4_pedicle_screw_screwhead$right_screw_head,
                   right_l5_pedicle_screw = l5_pedicle_screw_screwhead$right_screw_head,
                   right_s1_pedicle_screw = s1_pedicle_screw_screwhead$right_screw_head,
                   right_iliac_screw = iliac_screw_screwhead$right_screw_head,
                   right_s2ai_screw = s2ai_screw_screwhead$right_screw_head)


pedicle_lm_screw_screwhead_long_df <- tibble(implant = names(screw_list), 
                                  "screws" = screw_list,
                                  "screw_heads" = screwhead_list) %>%
  separate(col = implant, into = c("side", "level"), sep = "_", remove = FALSE) %>%
  left_join(vertebral_number_df)




c2_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = c2_y, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
c3_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = c3_y, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
c4_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = c4_y, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
c5_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = c5_y, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
c6_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = c6_y, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
c7_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = c7_y-0.01, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
t1_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = t1_y -0.011, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
t2_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = t2_y-0.013, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
t3_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = t3_y-0.013, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
t4_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = t4_y-0.013, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
t5_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = t5_y-0.013, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
t6_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = t6_y-0.013, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
t7_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = t7_y-0.013, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
t8_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = t8_y-0.013, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
t9_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = t9_y-0.013, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
t10_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = t10_y-0.013, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
t11_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = t11_y-0.013, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
t12_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = t12_y -0.013, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
l1_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = l1_y -0.013, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
l2_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = l2_y-0.013, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
l3_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = l3_y-0.013, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
l4_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = l4_y-0.013, screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)
l5_translaminar_screw_screwhead <- screw_function(screw_start_x_offset = 0.01, screw_start_y = l5_y -0.013 , screw_type = "translaminar", screw_width_modifier = 0.6, angle = -10)

translaminar_screw_list <- list(left_c2_translaminar_screw = c2_translaminar_screw_screwhead$left_screw_sf,
                                left_c3_translaminar_screw = c3_translaminar_screw_screwhead$left_screw_sf,
                                left_c4_translaminar_screw = c4_translaminar_screw_screwhead$left_screw_sf,
                                left_c5_translaminar_screw = c5_translaminar_screw_screwhead$left_screw_sf,
                                left_c6_translaminar_screw = c6_translaminar_screw_screwhead$left_screw_sf,
                                left_c7_translaminar_screw = c7_translaminar_screw_screwhead$left_screw_sf,
                                left_t1_translaminar_screw = t1_translaminar_screw_screwhead$left_screw_sf, 
                                left_t2_translaminar_screw = t2_translaminar_screw_screwhead$left_screw_sf,
                                left_t3_translaminar_screw = t3_translaminar_screw_screwhead$left_screw_sf,
                                left_t4_translaminar_screw = t4_translaminar_screw_screwhead$left_screw_sf,
                                left_t5_translaminar_screw = t5_translaminar_screw_screwhead$left_screw_sf,
                                left_t6_translaminar_screw = t6_translaminar_screw_screwhead$left_screw_sf,
                                left_t7_translaminar_screw = t7_translaminar_screw_screwhead$left_screw_sf,
                                left_t8_translaminar_screw = t8_translaminar_screw_screwhead$left_screw_sf, 
                                left_t9_translaminar_screw = t9_translaminar_screw_screwhead$left_screw_sf,
                                left_t10_translaminar_screw = t10_translaminar_screw_screwhead$left_screw_sf,
                                left_t11_translaminar_screw = t11_translaminar_screw_screwhead$left_screw_sf,
                                left_t12_translaminar_screw = t12_translaminar_screw_screwhead$left_screw_sf,
                                left_l1_translaminar_screw = l1_translaminar_screw_screwhead$left_screw_sf, 
                                left_l2_translaminar_screw = l2_translaminar_screw_screwhead$left_screw_sf, 
                                left_l3_translaminar_screw = l3_translaminar_screw_screwhead$left_screw_sf, 
                                left_l4_translaminar_screw = l4_translaminar_screw_screwhead$left_screw_sf,
                                left_l5_translaminar_screw = l5_translaminar_screw_screwhead$left_screw_sf,
                                right_c2_translaminar_screw = c2_translaminar_screw_screwhead$right_screw_sf,
                                right_c3_translaminar_screw = c3_translaminar_screw_screwhead$right_screw_sf,
                                right_c4_translaminar_screw = c4_translaminar_screw_screwhead$right_screw_sf,
                                right_c5_translaminar_screw = c5_translaminar_screw_screwhead$right_screw_sf,
                                right_c6_translaminar_screw = c6_translaminar_screw_screwhead$right_screw_sf,
                                right_c7_translaminar_screw = c7_translaminar_screw_screwhead$right_screw_sf,
                                right_t1_translaminar_screw = t1_translaminar_screw_screwhead$right_screw_sf, 
                                right_t2_translaminar_screw = t2_translaminar_screw_screwhead$right_screw_sf,
                                right_t3_translaminar_screw = t3_translaminar_screw_screwhead$right_screw_sf,
                                right_t4_translaminar_screw = t4_translaminar_screw_screwhead$right_screw_sf,
                                right_t5_translaminar_screw = t5_translaminar_screw_screwhead$right_screw_sf,
                                right_t6_translaminar_screw = t6_translaminar_screw_screwhead$right_screw_sf,
                                right_t7_translaminar_screw = t7_translaminar_screw_screwhead$right_screw_sf,
                                right_t8_translaminar_screw = t8_translaminar_screw_screwhead$right_screw_sf, 
                                right_t9_translaminar_screw = t9_translaminar_screw_screwhead$right_screw_sf,
                                right_t10_translaminar_screw = t10_translaminar_screw_screwhead$right_screw_sf,
                                right_t11_translaminar_screw = t11_translaminar_screw_screwhead$right_screw_sf,
                                right_t12_translaminar_screw = t12_translaminar_screw_screwhead$right_screw_sf,
                                right_l1_translaminar_screw = l1_translaminar_screw_screwhead$right_screw_sf, 
                                right_l2_translaminar_screw = l2_translaminar_screw_screwhead$right_screw_sf, 
                                right_l3_translaminar_screw = l3_translaminar_screw_screwhead$right_screw_sf, 
                                right_l4_translaminar_screw = l4_translaminar_screw_screwhead$right_screw_sf,
                                right_l5_translaminar_screw = l5_translaminar_screw_screwhead$right_screw_sf)

translaminar_screwhead_list <- list(left_c2_translaminar_screw = c2_translaminar_screw_screwhead$left_screw_head,
                                left_c3_translaminar_screw = c3_translaminar_screw_screwhead$left_screw_head,
                                left_c4_translaminar_screw = c4_translaminar_screw_screwhead$left_screw_head,
                                left_c5_translaminar_screw = c5_translaminar_screw_screwhead$left_screw_head,
                                left_c6_translaminar_screw = c6_translaminar_screw_screwhead$left_screw_head,
                                left_c7_translaminar_screw = c7_translaminar_screw_screwhead$left_screw_head,
                                left_t1_translaminar_screw = t1_translaminar_screw_screwhead$left_screw_head, 
                                left_t2_translaminar_screw = t2_translaminar_screw_screwhead$left_screw_head,
                                left_t3_translaminar_screw = t3_translaminar_screw_screwhead$left_screw_head,
                                left_t4_translaminar_screw = t4_translaminar_screw_screwhead$left_screw_head,
                                left_t5_translaminar_screw = t5_translaminar_screw_screwhead$left_screw_head,
                                left_t6_translaminar_screw = t6_translaminar_screw_screwhead$left_screw_head,
                                left_t7_translaminar_screw = t7_translaminar_screw_screwhead$left_screw_head,
                                left_t8_translaminar_screw = t8_translaminar_screw_screwhead$left_screw_head, 
                                left_t9_translaminar_screw = t9_translaminar_screw_screwhead$left_screw_head,
                                left_t10_translaminar_screw = t10_translaminar_screw_screwhead$left_screw_head,
                                left_t11_translaminar_screw = t11_translaminar_screw_screwhead$left_screw_head,
                                left_t12_translaminar_screw = t12_translaminar_screw_screwhead$left_screw_head,
                                left_l1_translaminar_screw = l1_translaminar_screw_screwhead$left_screw_head, 
                                left_l2_translaminar_screw = l2_translaminar_screw_screwhead$left_screw_head, 
                                left_l3_translaminar_screw = l3_translaminar_screw_screwhead$left_screw_head, 
                                left_l4_translaminar_screw = l4_translaminar_screw_screwhead$left_screw_head,
                                left_l5_translaminar_screw = l5_translaminar_screw_screwhead$left_screw_head,
                                right_c2_translaminar_screw = c2_translaminar_screw_screwhead$right_screw_head,
                                right_c3_translaminar_screw = c3_translaminar_screw_screwhead$right_screw_head,
                                right_c4_translaminar_screw = c4_translaminar_screw_screwhead$right_screw_head,
                                right_c5_translaminar_screw = c5_translaminar_screw_screwhead$right_screw_head,
                                right_c6_translaminar_screw = c6_translaminar_screw_screwhead$right_screw_head,
                                right_c7_translaminar_screw = c7_translaminar_screw_screwhead$right_screw_head,
                                right_t1_translaminar_screw = t1_translaminar_screw_screwhead$right_screw_head, 
                                right_t2_translaminar_screw = t2_translaminar_screw_screwhead$right_screw_head,
                                right_t3_translaminar_screw = t3_translaminar_screw_screwhead$right_screw_head,
                                right_t4_translaminar_screw = t4_translaminar_screw_screwhead$right_screw_head,
                                right_t5_translaminar_screw = t5_translaminar_screw_screwhead$right_screw_head,
                                right_t6_translaminar_screw = t6_translaminar_screw_screwhead$right_screw_head,
                                right_t7_translaminar_screw = t7_translaminar_screw_screwhead$right_screw_head,
                                right_t8_translaminar_screw = t8_translaminar_screw_screwhead$right_screw_head, 
                                right_t9_translaminar_screw = t9_translaminar_screw_screwhead$right_screw_head,
                                right_t10_translaminar_screw = t10_translaminar_screw_screwhead$right_screw_head,
                                right_t11_translaminar_screw = t11_translaminar_screw_screwhead$right_screw_head,
                                right_t12_translaminar_screw = t12_translaminar_screw_screwhead$right_screw_head,
                                right_l1_translaminar_screw = l1_translaminar_screw_screwhead$right_screw_head, 
                                right_l2_translaminar_screw = l2_translaminar_screw_screwhead$right_screw_head, 
                                right_l3_translaminar_screw = l3_translaminar_screw_screwhead$right_screw_head, 
                                right_l4_translaminar_screw = l4_translaminar_screw_screwhead$right_screw_head,
                                right_l5_translaminar_screw = l5_translaminar_screw_screwhead$right_screw_head)

translaminar_screw_screwhead_long_df <- tibble(implant = names(translaminar_screw_list), 
                                  "screws" = translaminar_screw_list,
                                  "screw_heads" = translaminar_screwhead_list) %>%
  separate(col = implant, into = c("side", "level"), sep = "_", remove = FALSE) %>%
  left_join(vertebral_number_df)


screw_screwhead_long_df <- pedicle_lm_screw_screwhead_long_df %>%
  union_all(translaminar_screw_screwhead_long_df) 