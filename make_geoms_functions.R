jh_make_posterior_geoms_function <- function(all_posterior_objects_df, plot_with_patterns = FALSE){
  # all_posterior_objects_df <- construct_objects_live_function(all_posterior_objects_df)
  
  geoms_list_posterior <- list()
  if(any(str_detect(all_posterior_objects_df$object, pattern = "vertebroplasty"))){
    geoms_list_posterior$vertebroplasty_sf_geom <- geom_sf_pattern(data = st_multipolygon((all_posterior_objects_df %>% filter(str_detect(string = object, pattern = "vertebroplasty")))$object_constructed),
                                                                   pattern = "plasma",
                                                                   pattern_alpha = 0.5,
                                                                   alpha = 0.3,
                                                                   color = "grey96")
  }else{
    geoms_list_posterior$vertebroplasty_sf_geom <- NULL
  }
  
  if(any(str_detect(all_posterior_objects_df$object, pattern = "vertebral_cement_augmentation"))){
    geoms_list_posterior$vertebroplasty_sf_geom <- geom_sf_pattern(data = st_multipolygon((all_posterior_objects_df %>% filter(str_detect(string = object, pattern = "vertebral_cement_augmentation")))$object_constructed),
                                                                   pattern = "plasma",
                                                                   pattern_alpha = 0.5,
                                                                   alpha = 0.6,
                                                                   color = "grey75")
  }else{
    geoms_list_posterior$vertebral_cement_augmentation_sf_geom <- NULL
  }
  
  ## OSTEOTOMIES
  if(any(str_detect(all_posterior_objects_df$object, pattern = "grade_1"))){
    geoms_list_posterior$osteotomy_1_sf <- geom_sf(data = st_geometrycollection((all_posterior_objects_df %>% filter(object == "grade_1"))$object_constructed), color = "red", size = 1)
  }else{
    geoms_list_posterior$osteotomy_1_sf <- NULL
  }
  
  if(any(str_detect(all_posterior_objects_df$object, pattern = "complete_facetectomy"))){
    geoms_list_posterior$osteotomy_facetectomy_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_multipolygon((all_posterior_objects_df %>% filter(object == "complete_facetectomy"))$object_constructed),
      pattern = "stripe",
      pattern_colour = "red",
      alpha = 0.5,
      pattern_angle = 10,
      pattern_spacing = 0.01,
      pattern_density = 0.15,
    )
  }else{
    geoms_list_posterior$osteotomy_facetectomy_sf_geom <- NULL
  }
  
  if(any(str_detect(all_posterior_objects_df$object, pattern = "grade_2"))){
    geoms_list_posterior$osteotomy_2_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_union(st_combine(st_multipolygon((all_posterior_objects_df %>% filter(object == "grade_2"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE),
      pattern = "stripe",
      pattern_colour = "red",
      alpha = 0.5,
      pattern_angle = 10,
      pattern_spacing = 0.01,
      pattern_density = 0.15,
    )
  }else{
    geoms_list_posterior$osteotomy_2_sf_geom <- NULL
  }
  
  if(any(str_detect(all_posterior_objects_df$object, pattern = "grade_3"))){
    geoms_list_posterior$osteotomy_3_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_union(st_combine(st_multipolygon((all_posterior_objects_df %>% filter(object == "grade_3"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE),
      pattern = "stripe",
      pattern_colour = "red",
      alpha = 0.6,
      pattern_angle = 10,
      pattern_spacing = 0.03,
      pattern_density = 0.1
    )
  }else{
    geoms_list_posterior$osteotomy_3_sf_geom <- NULL
  }
  
  if(any(str_detect(all_posterior_objects_df$object, pattern = "grade_4"))){
    geoms_list_posterior$osteotomy_4_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_union(st_combine(st_multipolygon((all_posterior_objects_df %>% filter(object == "grade_4"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE),
      pattern = "stripe",
      pattern_colour = "red",
      alpha = 0.6,
      pattern_angle = 10,
      pattern_spacing = 0.02,
      pattern_density = 0.02
    )
  }else{
    geoms_list_posterior$osteotomy_4_sf_geom <- NULL
  }
  
  if(any(str_detect(all_posterior_objects_df$object, pattern = "grade_5"))){
    geoms_list_posterior$osteotomy_5_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_union(st_combine(st_multipolygon((all_posterior_objects_df %>% filter(object == "grade_5"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE),
      pattern = "crosshatch",
      pattern_colour = "red",
      alpha = 0.6,
      pattern_angle = 10,
      pattern_spacing = 0.02,
      pattern_density = 0.02
    )
  }else{
    geoms_list_posterior$osteotomy_5_sf_geom <- NULL
  }
  
  ## Decompresions ##
  if(any(str_detect(all_posterior_objects_df$object, pattern = "laminectomy_for_facet_cyst"))){
    geoms_list_posterior$laminectomy_for_facet_cyst_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_union(st_combine(st_multipolygon((all_posterior_objects_df %>% filter(object == "laminectomy_for_facet_cyst"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE),
      pattern = "stripe",
      pattern_colour = "orange",
      alpha = 0.7,
      pattern_spacing = 0.01
    )
  }else{
    geoms_list_posterior$laminectomy_for_facet_cyst_sf_geom <- NULL
  }
  
  if(any(str_detect(all_posterior_objects_df$object, pattern = "laminectomy"))){
    geoms_list_posterior$laminectomy_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_union(st_combine(st_multipolygon((all_posterior_objects_df %>% filter(object == "laminectomy"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE),
      pattern = "stripe",
      pattern_colour = "red",
      alpha = 0.7,
      pattern_spacing = 0.01
    )
  }else{
    geoms_list_posterior$laminectomy_sf_geom <- NULL
  }
  
  if(any(str_detect(all_posterior_objects_df$object, pattern = "sublaminar_decompression"))){
    geoms_list_posterior$sublaminar_decompression_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_multipolygon((all_posterior_objects_df %>% filter(object == "sublaminar_decompression"))$object_constructed),
      pattern = "stripe",
      pattern_colour = "red",
      alpha = 0.7,
      pattern_spacing = 0.01
    )
  }else{
    geoms_list_posterior$sublaminar_decompression_sf_geom <- NULL
  }
  
  if(any(str_detect(all_posterior_objects_df$object, pattern = "laminotomy"))){
    geoms_list_posterior$laminotomy_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_multipolygon((all_posterior_objects_df %>% filter(object == "laminotomy"))$object_constructed),
      pattern = "stripe",
      pattern_colour = "red",
      alpha = 0.7,
      pattern_spacing = 0.01
    )
  }else{
    geoms_list_posterior$laminotomy_sf_geom <- NULL
  }
  if(any(str_detect(all_posterior_objects_df$object, pattern = "cervical_foraminotomy"))){
    geoms_list_posterior$cervical_foraminotomy_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_multipolygon((all_posterior_objects_df %>% filter(object == "cervical_foraminotomy"))$object_constructed),
      pattern = "stripe",
      pattern_colour = "red",
      alpha = 0.7,
      pattern_spacing = 0.01
    )
  }else{
    geoms_list_posterior$cervical_foraminotomy_sf_geom <- NULL
  }
  
  if(any(str_detect(all_posterior_objects_df$object, pattern = "transpedicular_approach"))){
    geoms_list_posterior$transpedicular_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_multipolygon((all_posterior_objects_df %>% filter(object == "transpedicular_approach"))$object_constructed),
      pattern = "stripe",
      pattern_colour = "red",
      alpha = 0.7,
      pattern_spacing = 0.01
    )
  }else{
    geoms_list_posterior$transpedicular_sf_geom <- NULL
  }
  if(any(str_detect(all_posterior_objects_df$object, pattern = "costo"))){
    geoms_list_posterior$costovertebral_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_multipolygon((all_posterior_objects_df %>% filter(object == "costovertebral_approach" | object == "costotransversectomy"))$object_constructed),
      pattern = "stripe",
      pattern_colour = "red",
      alpha = 0.7,
      pattern_spacing = 0.01
    )
  }else{
    geoms_list_posterior$costovertebral_sf_geom <- NULL
  }
  if(any(str_detect(all_posterior_objects_df$object, pattern = "lateral_extracavitary"))){
    geoms_list_posterior$lateral_extracavitary_approach_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_multipolygon((all_posterior_objects_df %>% filter(object == "lateral_extracavitary_approach"))$object_constructed),
      pattern = "stripe",
      pattern_colour = "red",
      alpha = 0.7,
      pattern_spacing = 0.01
    )
  }else{
    geoms_list_posterior$lateral_extracavitary_approach_sf_geom <- NULL
  }
  
  if(any(str_detect(all_posterior_objects_df$object, pattern = "lateral_extraforaminal_approach"))){
    geoms_list_posterior$lateral_extraforaminal_approach_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_multipolygon((all_posterior_objects_df %>% filter(object == "lateral_extraforaminal_approach"))$object_constructed),
      pattern = "stripe",
      pattern_colour = "red",
      alpha = 0.7,
      pattern_spacing = 0.01
    )
  }else{
    geoms_list_posterior$lateral_extraforaminal_approach_sf_geom <- NULL
  }
  
  if(any(all_posterior_objects_df$object == "diskectomy")){
    geoms_list_posterior$diskectomy_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_multipolygon((all_posterior_objects_df %>% filter(object == "diskectomy"))$object_constructed),
      pattern = "stripe",
      pattern_colour = "red",
      alpha = 0.7,
      pattern_spacing = 0.01
    )
  }else{
    geoms_list_posterior$diskectomy_sf_geom <- NULL
  }
  
  if(any(str_detect(all_posterior_objects_df$object, pattern = "laminoplasty"))){
    geoms_list_posterior$laminoplasty_sf_geom <- geom_sf(data = st_multipolygon((all_posterior_objects_df %>% filter(object == "laminoplasty"))$object_constructed), fill  = "blue") 
    geoms_list_posterior$laminoplasty_cut_df_sf_geom <- geom_line(data = tibble(x = 0.515,
                                                                                # y = c(max(as_tibble(st_coordinates(head(all_posterior_objects_df %>% filter(object == "laminoplasty"), 1)$object_constructed))$Y), 
                                                                                # min(as_tibble(st_coordinates(tail(all_posterior_objects_df %>% filter(object == "laminoplasty"), 1)$object_constructed))$Y))),
                                                                                y =  c(max((all_posterior_objects_df %>% filter(object == "laminoplasty"))$y + 0.01),
                                                                                       min((all_posterior_objects_df %>% filter(object == "laminoplasty"))$y) - 0.01)),
                                                                  aes(x = x, y = y), linetype = "dotted", size = 2, color = "red") 
  }else{
    geoms_list_posterior$laminoplasty_sf_geom <- NULL
    geoms_list_posterior$laminoplasty_cut_df_sf_geom <- NULL
  }
  
  
  #### TUMOR
  
  if(any(str_detect(all_posterior_objects_df$object, pattern = "corpectomy_extracavitary_tumor"))){
    geoms_list_posterior$corpectomy_extracavitary_tumor_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_multipolygon((all_posterior_objects_df %>% filter(object == "corpectomy_extracavitary_tumor"))$object_constructed),
      pattern = "stripe",
      pattern_colour = "darkred",
      alpha = 0.7,
      pattern_spacing = 0.01
    )
  }else{
    geoms_list_posterior$corpectomy_extracavitary_tumor_sf_geom <- NULL
  }
  
  if(any(str_detect(all_posterior_objects_df$object, pattern = "laminectomy_for_tumor"))){
    geoms_list_posterior$laminectomy_for_tumor_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_union(st_combine(st_multipolygon((all_posterior_objects_df %>% filter(object == "laminectomy_for_tumor"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE),
      pattern = "stripe",
      pattern_colour = "darkred",
      alpha = 0.7,
      pattern_spacing = 0.01
    )
  }else{
    geoms_list_posterior$laminectomy_for_tumor_sf_geom <- NULL
  }
  
  ## INTERBODY
  if(any(all_posterior_objects_df$object == "tlif") || any(all_posterior_objects_df$object == "llif") ||any(all_posterior_objects_df$object == "plif")){
    geoms_list_posterior$interbody_device_sf_geom <- geom_sf(data = st_multipolygon((all_posterior_objects_df %>% filter(object == "tlif" | object == "plif" | object == "llif"))$object_constructed), fill = "red")
  }else{
    geoms_list_posterior$interbody_device_sf_geom <- NULL
  }
  
  if(any(all_posterior_objects_df$object == "no_implant_interbody_fusion")){
    geoms_list_posterior$no_implant_interbody_fusion_sf_geom <- geom_sf(data = st_multipolygon((all_posterior_objects_df %>% filter(object == "no_implant_interbody_fusion"))$object_constructed), fill = "#E66565")
  }else{
    geoms_list_posterior$no_implant_interbody_fusion_sf_geom <- NULL
  }
  
  if(any(str_detect(all_posterior_objects_df$object, pattern = "intervertebral_cage"))){
    geoms_list_posterior$intervertebral_cage_sf_geom <-  ggpattern::geom_sf_pattern(
      data =  st_union(st_combine(st_multipolygon((all_posterior_objects_df %>% filter(object == "intervertebral_cage"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE),
      pattern = "crosshatch",
      pattern_fill = "grey90",
      fill = "#7899F5",
      alpha = 0.3,
      pattern_spacing = 0.01,
      pattern_density = 0.7
    )
  }else{
    geoms_list_posterior$intervertebral_cage_sf_geom <- NULL
  }
  
  ## SCREWS
  if(any(str_detect((all_posterior_objects_df$object), pattern = "screw"))){
    if(plot_with_patterns == TRUE){
      geoms_list_posterior$screws_geom <- ggpattern::geom_sf_pattern(
        data = st_multipolygon((all_posterior_objects_df %>% filter(str_detect(string = object, pattern = "screw")))$object_constructed),
        pattern = "stripe",
        pattern_fill = "blue",
        pattern_fill2 = "#445566",
        alpha = 0.8,
        pattern_colour = "blue",
        pattern_density = 0.02,
        pattern_spacing = 0.01,
        pattern_angle = 80
      )
    }else{
      geoms_list_posterior$screws_geom <- geom_sf(data = st_multipolygon((all_posterior_objects_df %>% filter(str_detect(string = object, pattern = "screw")))$object_constructed), fill = "blue")
    }
    
  }else{
    geoms_list_posterior$screws_geom <- NULL
  }
  
  ## HOOKS
  if(any(str_detect(all_posterior_objects_df$object, pattern = "hook"))){
    hook_df <- all_posterior_objects_df %>%
      filter(str_detect(string = object, pattern = "hook"))
    geoms_list_posterior$hooks_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_multipolygon(hook_df$object_constructed),
      pattern = "gradient",
      pattern_fill = "blue",
      pattern_fill2 = "#445566",
      alpha = 0.8
    )
  }else{
    geoms_list_posterior$hooks_sf_geom <- NULL
  }
  
  ## SUBLAMINAR BANDS
  if(any(str_detect(all_posterior_objects_df$object, pattern = "wire"))){
    geoms_list_posterior$sublaminar_wires_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_multipolygon((all_posterior_objects_df %>% filter(str_detect(string = object, pattern = "wire")))$object_constructed),
      pattern = "gradient",
      pattern_fill = "red",
      pattern_fill2 = "#445566",
      alpha = 0.8
    )
  }else{
    geoms_list_posterior$sublaminar_wires_sf_geom <- NULL
  }
  
  ##structural_allograft
  if(any(str_detect(all_posterior_objects_df$object, pattern = "structural_allograft"))){
    geoms_list_posterior$structural_allograft_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_union(st_combine(st_multipolygon((all_posterior_objects_df %>% filter(object == "structural_allograft"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE),
      pattern = "plasma",
      pattern_alpha = 0.6,
      alpha = 0.6,
      fill = "brown"
    )
    
  }else{
    geoms_list_posterior$structural_allograft_sf_geom <- NULL
  }
  
  ## Tethers
  if(any(str_detect(all_posterior_objects_df$object, pattern = "tether"))){
    geoms_list_posterior$tethers_sf_geom <- geom_sf(data = st_multipolygon((all_posterior_objects_df %>% filter(str_detect(string = object, pattern = "tether")))$object_constructed))
  }else{
    geoms_list_posterior$tethers_sf_geom <- NULL
  }
  
  
  if(any(str_detect(all_posterior_objects_df$object, pattern = "incision_drainage"))){
    geoms_list_posterior$incision_drainage_sf_geom <- ggpattern::geom_sf_pattern(
      data = st_union(st_combine(st_multipolygon((all_posterior_objects_df %>% filter(object == "incision_drainage"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE),
      pattern = "stripe",
      pattern_colour = "red",
      alpha = 0.1,
      pattern_spacing = 0.03
    )
  }else{
    geoms_list_posterior$incision_drainage_sf_geom <- NULL
  }
  
  
  return(geoms_list_posterior = geoms_list_posterior)
  
}




###########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#################### MAKE ANTERIOR GEOMS ###########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#################### 
###########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#################### MAKE ANTERIOR GEOMS ###########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#################### 
###########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#################### MAKE ANTERIOR GEOMS ###########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#################### 
###########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#################### MAKE ANTERIOR GEOMS ###########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#################### 
###########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#################### MAKE ANTERIOR GEOMS ###########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#################### 

jh_make_anterior_geoms_function <- function(all_anterior_objects_df){
  
  geoms_list_anterior_diskectomy <- list()
  geoms_list_anterior_interbody <- list()
  geoms_list_anterior_instrumentation <- list()
  if(any(all_anterior_objects_df$object %in% c("decompression_diskectomy_fusion", "diskectomy_fusion", "diskectomy_fusion_no_interbody_device"))){
    diskectomy_fusion_df <- all_anterior_objects_df %>%
      filter(object %in% c("decompression_diskectomy_fusion", "diskectomy_fusion", "diskectomy_fusion_no_interbody_device")) %>%
      group_by(vertebral_number) %>%
      mutate(count = row_number()) %>%
      filter(count == max(count)) %>%
      ungroup() %>%
      distinct()
    
    geoms_list_anterior_diskectomy$diskectomy_fusion_sf_geom <- ggpattern::geom_sf_pattern(
      data =  st_multipolygon((diskectomy_fusion_df)$object_constructed),
      pattern = "circle",
      pattern_fill = "#F5A105",
      color = "#F7B28F",
      fill = "#A89E9E",
      alpha = 0.6,
      pattern_spacing = 0.005,
      pattern_density = 0.7
    )
  }else{
    geoms_list_anterior_diskectomy$diskectomy_fusion_sf_geom <- NULL
  }
  
  ########  Corpectomy  ########
  if(any(all_anterior_objects_df$object == "corpectomy")){
    geoms_list_anterior_diskectomy$corpectomy_sf_geom <- ggpattern::geom_sf_pattern(
      data =  st_union(st_combine(st_multipolygon((all_anterior_objects_df %>% filter(object == "corpectomy"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE),
      pattern = "stripe",
      pattern_colour = "red",
      alpha = 0.6,
      pattern_angle = 90,
      pattern_spacing = 0.01,
      pattern_density = 0.1
    )
  }else{
    geoms_list_anterior_diskectomy$corpectomy_sf_geom <- NULL
  }
  
  ########  Arthroplasty  ########
  if(any(all_anterior_objects_df$object == "anterior_disc_arthroplasty")){
    anterior_disc_arthroplasty_df <- all_anterior_objects_df %>%
      filter(object == "anterior_disc_arthroplasty") %>%
      remove_empty() %>%
      unnest(object_constructed)
    
    geoms_list_anterior_interbody$anterior_disc_arthroplasty_sf_geom <-  geom_sf(data = anterior_disc_arthroplasty_df,
                                                                                 aes(geometry = object_constructed, fill = color))
    geoms_list_anterior_interbody$arthroplasty_fill <- scale_fill_identity()
  }else{
    geoms_list_anterior_interbody$anterior_disc_arthroplasty_sf_geom <- NULL
    geoms_list_anterior_interbody$arthroplasty_fill <- NULL
  }
  
  ########  Corpectomy Cage  ########
  if(any(all_anterior_objects_df$object == "corpectomy_cage")){
    geoms_list_anterior_interbody$corpectomy_cage_sf_geom <-  ggpattern::geom_sf_pattern(
      data =  st_union(st_combine(st_multipolygon((all_anterior_objects_df %>% filter(object == "corpectomy_cage"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE),
      pattern = "crosshatch",
      pattern_fill = "grey90",
      fill = "#7899F5",
      alpha = 0.3,
      pattern_spacing = 0.01,
      pattern_density = 0.7
    )
  }else{
    geoms_list_anterior_interbody$corpectomy_cage_sf_geom <- NULL
  }
  
  
  ########  Interbody Implant  ########
  if(any(all_anterior_objects_df$object == "anterior_interbody_implant")){
    geoms_list_anterior_interbody$anterior_interbody_sf_geom <- ggpattern::geom_sf_pattern(
      data =  st_multipolygon((all_anterior_objects_df %>% filter(object == "anterior_interbody_implant"))$object_constructed),
      pattern = "crosshatch",
      pattern_fill = "grey90",
      fill = "#7899F5",
      alpha = 0.3,
      pattern_spacing = 0.02,
      pattern_density = 0.7
    ) 
  }else{
    geoms_list_anterior_interbody$anterior_interbody_sf_geom <- NULL
  }
  
  ########  Screw/Washer  ########
  if(any(all_anterior_objects_df$object == "screw_washer")){
    washer_df <- all_anterior_objects_df %>%
      filter(object == "screw_washer") %>%
      mutate(object_constructed = pmap(list(..1 = y), .f = ~ st_buffer(x = st_point(c(0.5, ..1)), dist = 0.005)))
    
    geoms_list_anterior_instrumentation$screw_washer_sf_geom <- geom_sf(data = st_multipolygon((all_anterior_objects_df %>% filter(object == "screw_washer"))$object_constructed), fill = "#3B86CC")
    geoms_list_anterior_instrumentation$screw_washer_screw_sf_geom <- geom_sf(data = st_multipolygon(washer_df$object_constructed), fill = "black")
    
  }else{
    geoms_list_anterior_instrumentation$screw_washer_sf_geom <- NULL
    geoms_list_anterior_instrumentation$screw_washer_screw_sf_geom <- NULL
  }
  ########  Anterior Buttress Plate  ########
  if(any(all_anterior_objects_df$object == "anterior_buttress_plate")){
    geoms_list_anterior_instrumentation$anterior_buttress_plate_sf_geom <-ggpattern::geom_sf_pattern(
      data =  st_multipolygon((all_anterior_objects_df %>% filter(object == "anterior_buttress_plate"))$object_constructed),
      pattern = "circle",
      pattern_fill = "grey90",
      fill = "#7899F5",
      pattern_spacing = 0.01,
      pattern_density = 0.7
    )
  }else{
    geoms_list_anterior_instrumentation$anterior_buttress_plate_sf_geom <- NULL
  }
  ########  Anterior Plate  ########
  if(any(all_anterior_objects_df$object == "anterior_plate")){
    geoms_list_anterior_instrumentation$anterior_plate_sf_geom <- ggpattern::geom_sf_pattern(
      data =  st_union(st_combine(st_multipolygon((all_anterior_objects_df %>% filter(object == "anterior_plate"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE),
      pattern = "circle",
      pattern_fill = "grey60",
      fill = "#3B86CC",
      pattern_spacing = 0.01,
      pattern_density = 0.7
    )
  }else{
    geoms_list_anterior_instrumentation$anterior_plate_sf_geom <- NULL
  }
  
  return(list(geoms_list_anterior_diskectomy = geoms_list_anterior_diskectomy,
              geoms_list_anterior_interbody = geoms_list_anterior_interbody,
              geoms_list_anterior_instrumentation = geoms_list_anterior_instrumentation))
  
}
