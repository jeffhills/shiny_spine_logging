procedure_classifier_type_df <- tribble(~object_name, ~procedure_label, ~paragraph_type, 
                                        'incision_drainage', 'Incision and drainage', 'combine',
                                        'vertebroplasty', 'Vertebroplasty', 'combine',
                                        'vertebral_cement_augmentation', 'Vertebral body augmentation', 'combine',
                                        'laminar_downgoing_hook', 'Posterior spinal instrumentation', 'combine',
                                        'laminar_upgoing_hook', 'Posterior spinal instrumentation', 'combine',
                                        'lateral_mass_screw', 'Posterior spinal instrumentation', 'combine',
                                        'occipital_screw', 'Occiput instrumentation', 'combine',
                                        'pars_screw', 'Posterior spinal instrumentation', 'combine',
                                        'pedicle_hook', 'Posterior spinal instrumentation', 'combine',
                                        'pedicle_screw', 'Posterior spinal instrumentation', 'combine',
                                        'crosslink', 'Posterior spinal instrumentation', 'combine',
                                        'laminar_downgoing_hook', 'Posterior spinal instrumentation', 'combine',
                                        'sublaminar_wire', 'Posterior spinal instrumentation', 'combine',
                                        'tp_hook', 'Posterior spinal instrumentation', 'combine',
                                        'translaminar_screw', 'Posterior spinal instrumentation', 'combine',
                                        'pelvic_screw', 'Pelvic instrumentation', 'combine',
                                        'pelvic_screw_1', 'Pelvic instrumentation', 'combine',
                                        'pelvic_screw_2', 'Pelvic instrumentation', 'combine',
                                        'tether', 'Spinous process posterior tethering/wiring', 'combine',
                                        'costovertebral_approach', 'Costovertebral approach with decompression of the spinal cord', 'distinct',
                                        'revision_costovertebral_approach', 'Reexploration and revision decompression using a costovertebral approach', 'distinct',
                                        'transpedicular_approach', 'Transpedicular approach with decompression', 'distinct',
                                        'lateral_extraforaminal_approach', 'Decompression using a lateral extraforaminal approach', 'distinct',
                                        'lateral_extracavitary_approach', 'Arthrodesis using a modified lateral extracavitary approach', 'distinct',
                                        'corpectomy_extracavitary_tumor', 'Partial Corpectomy with decompression using a modified lateral extracavitary approach for tumor', 'distinct',
                                        'laminectomy_for_tumor', 'Laminectomy for biopsy and excision of extradural spinal tumor', 'combine',
                                        'laminectomy_for_facet_cyst', 'Laminectomy for excision of facet cyst (instraspinal lesion, not neoplasm)', 'combine',
                                        'revision_transpedicular_approach', 'Reexploration and revision decompression using a transpedicular approach', 'distinct',
                                        'sublaminar_decompression', 'Bilateral laminectomy, foraminotomy, and medial facetectomy for decompression of the cauda equina and nerve roots', 'combine',
                                        'laminectomy', 'Central Laminectomy for decompression', 'combine',
                                        'diskectomy', 'Laminotomy and discectomy for decompression of the nerve root', 'combine',
                                        'laminotomy', 'Partial laminectomy with facetectomy & foraminotomy for decompression of the cauda equina and nerve roots', 'combine',
                                        'cervical_foraminotomy', 'Posterior cervical foraminotomy for decompression of the nerve root', 'combine',
                                        'revision_diskectomy', 'Reexploration and revision laminotomy and discectomy for decompression of the nerve root', 'combine',
                                        'revision_sublaminar_decompression', 'Reexploration and revision decompression with bilateral partial laminectomies, foraminotomies, and medial facetectomies', 'combine',
                                        'revision_laminectomy', 'Reexploration and revision decompression with central laminectomy', 'combine',
                                        'revision_laminotomy', 'Reexploration and revision decompression with laminotomy and medial facetectomy', 'combine',
                                        'laminoplasty', 'Laminoplasty', 'combine',
                                        'grade_1', 'Inferior facetectomies', 'combine',
                                        'complete_facetectomy', 'Partial laminectomy with complete facetectomy & foraminotomy for decompression of the nerve root', 'combine',
                                        'revision_complete_facetectomy', 'Reexploration and revision decompression with a laminotomy and complete facetectomy', 'combine',
                                        'grade_2', 'Posterior column osteotomy', 'combine',
                                        'grade_3', 'Pedicle subtraction osteotomy', 'distinct',
                                        'grade_4', 'Extended three column osteotomy (vertebral body partial corpectomy)', 'distinct',
                                        'grade_5', 'Vertebral column resection', 'distinct',
                                        'costotransversectomy', 'Costovertebral approach with costotransversectomy', 'combine',
                                        'no_implant_interbody_fusion', 'Interbody fusion (without interbody implant)', 'distinct',
                                        'llif', 'Lateral lumbar interbody fusion and insertion of interbody device', 'distinct',
                                        'plif', 'Posterior lumbar interbody fusion and insertion of interbody device', 'distinct',
                                        'tlif', 'Transforaminal lumbar interbody fusion and insertion of interbody device', 'distinct',
                                        'intervertebral_cage', 'Insertion of intervertebral biomechanical implant', 'distinct',
                                        'structural_allograft', 'Application of structural allograft', 'combine',
                                        'anterior_disc_arthroplasty', 'Total disk arthroplasty', 'distinct',
                                        'decompression_diskectomy_fusion', 'Anterior discectomy and fusion with decompression of the central canal and nerve roots', 'distinct',
                                        'diskectomy_fusion', 'Anterior discectomy and fusion', 'distinct',
                                        'diskectomy_fusion_no_interbody_device', 'Anterior discectomy and fusion', 'distinct',
                                        'anterior_interbody_implant', 'Insertion of interbody biomechanical implant', 'distinct',
                                        'corpectomy', 'Anterior vertebral corpectomy', 'combine',
                                        'partial_corpectomy', 'Anterior vertebral partial corpectomy', 'combine',
                                        'corpectomy_cage', 'Anterior insertion of intervertebral biomechanical implant', 'combine',
                                        'anterior_plate', 'Anterior spinal instrumentation (distinct from an interbody implant)', 'combine',
                                        'anterior_plate_screw', 'Anterior spinal instrumentation (distinct from an interbody implant)', 'combine',
                                        'anterior_buttress_plate', 'Anterior spinal instrumentation (distinct from an interbody implant)', 'combine',
                                        'screw_washer', 'Anterior spinal instrumentation (distinct from an interbody implant)', 'combine')

#### ADDING AN OBJECT: you must do the following:
# - add the object to the "op_note_procedure_performed_summary_classifier_function"
# - add the object to the "op_note_number_of_paragraphs_for_procedure_category"
# - add the revision objects if needed
# - add the geom


################-------------------------#################### -------- ANTERIOR & POSTERIOR FUNCTIONS  -----------################-------------------------#################### 
################-------------------------#################### -------- ANTERIOR & POSTERIOR FUNCTIONS  -----------################-------------------------#################### 
################-------------------------#################### -------- ANTERIOR & POSTERIOR FUNCTIONS  -----------################-------------------------#################### 



op_note_procedure_performed_summary_classifier_function <- function(object){
  
  label_df <- procedure_classifier_type_df %>%
    filter(object_name == object)
  
  procedure_category <- label_df$procedure_label[[1]]
  
  return(procedure_category)
}

op_note_number_of_paragraphs_for_procedure_category <- function(procedure_cat){
  procedure_cat <- str_to_lower(procedure_cat)
  
  # "object_name"     "procedure_label" "paragraph_type" 
  
  paragraph_type_df <- procedure_classifier_type_df %>%
    mutate(procedure_label = str_to_lower(procedure_label)) %>%
    filter(procedure_label == procedure_cat) %>%
    select(paragraph_type)
  
  paragraph_type <- paragraph_type_df$paragraph_type[[1]]
  
  return(paragraph_type)
}


extract_levels_function <- function(input_df){
  levels_df <- input_df %>%
    arrange(vertebral_number) %>%
    select(level) %>%
    distinct()
  
  glue_collapse(x = levels_df$level, sep = ", ", last = " and ")
  
}

################-------------------------#################### -------- ANTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- ANTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- ANTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- ANTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- ANTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- ANTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- ANTERIOR -----------################-------------------------#################### 

#############-----------------------   Building Paragraphs: Combine Multiple Procedures into One ----------------------###############

op_note_object_combine_paragraph_function <- function(object, levels_nested_df){
  
  if(object == "anterior_plate"){
    levels_df <- levels_nested_df %>%
      separate(col = level, into = c("cranial", "caudal"), sep = "-") %>%
      pivot_longer(cols = c(cranial, caudal)) %>%
      select(level = value) %>%
      mutate(level = if_else(level == "Iliac" | level =="S2AI", "S1", level)) %>%
      mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
      arrange(vertebral_number) %>%
      distinct()
    
    statement <- glue("I placed an anterior plate spanning {glue_collapse(levels_df$level, sep = ', ', last = ' and ')}. After selecting an appropriately sized plate, I held the plate into position and drilled and tapped the path for the screws. The screws were then inserted sequentially into the vertebral body of {glue_collapse(levels_df$level, sep = ', ', last = ' and ')} to hold the plate into position.")
    }
  
  if(object == "anterior_plate_screw"){
      levels_df <- levels_nested_df %>%
        mutate(level = if_else(level == "Iliac" | level =="S2AI", "S1", level)) %>%
        arrange(vertebral_number) %>%
        distinct() %>%
        mutate(screw_size_type = str_remove_all(string = screw_size_type, pattern = " Anterior")) %>%
        pivot_wider(names_from = side, values_from = screw_size_type) %>%
        mutate(left = if_else(is.na(left), "", left)) %>%
        mutate(right = if_else(is.na(right), "", right)) %>%
        mutate(left_statement = if_else(str_count(string = left) > 1, 
                                        paste("a", left, "screw on the left"), 
                                        "")) %>%
        mutate(right_statement = if_else(str_count(string = right) > 1, 
                                         if_else(str_count(string = left) > 1, 
                                                 paste("and a", right, "screw on the right"), 
                                                 paste("a", right, "screw on the right")), 
                                         "")
        ) %>%
        mutate(screw_statement = glue("At {level}, I placed {paste(left_statement, right_statement)}."))
      
      statement <- as.character(glue_collapse(levels_df$screw_statement, sep = " "))
    
  }
  
  if(object == "anterior_buttress_plate"){
    statement_df <- levels_nested_df %>%
      mutate(statements = glue("At the {level} level, I placed an anterior buttress plate. After selecting an appropriately sized plate and length for the screws, I held the plate into position and drilled and tapped the path for the screw. I then inserted the screw to secure the plate to the anterior vertebral body. The plate had adequate coverage of the intervertebral disc space."))
    
    statement <- glue_collapse(statement_df$statements, sep = " ")
  }
  
  if(object == "screw_washer"){
    statement_df <- levels_nested_df %>%
      mutate(statements = glue("At the {level} level, I placed a screw to work as a buttress. The midline of the anterior vertebral endplate was identified and a drill was used to create a trajectory for the screw. I then inserted the screw the into the {level} vertebral body."))
    
    statement <- glue_collapse(statement_df$statements, sep = " ")
  }
  
  if(object == "corpectomy"){
    
    levels_df <- levels_nested_df %>%
      select(level) %>%
      distinct()
    
    corpectomy_levels <- glue_collapse(levels_df$level, sep = ", ", last = " and ")
    
    cranial_level_character <- jh_get_vertebral_level_function(number = unique(min(levels_nested_df$vertebral_number)))
    cranial_interspace_character <- jh_get_cranial_caudal_interspace_body_list_function(level = cranial_level_character)$cranial_interspace
    
    caudal_level_character <- jh_get_vertebral_level_function(number = unique(max(levels_nested_df$vertebral_number)))
    caudal_interspace_character <- jh_get_cranial_caudal_interspace_body_list_function(level = caudal_level_character)$caudal_interspace
    
    statement_df <- levels_nested_df %>%
      mutate(paragraph = glue("I confirmed that I had adequately exposed up to the {cranial_interspace_character} disk cranially, and down to {caudal_interspace_character} disk caudally. I then started with the diskectomies. Using a combination of a knife, currette, pituitary ronguer, and Kerrison rongeurs, the anterior longitudinal ligament was incised and the discs from {cranial_interspace_character} down to {caudal_interspace_character} were completely excised. Once I was satisfied with the diskectomies, I used a combination of a burr and rongeur's to excise roughly 80% of the {corpectomy_levels} vertebral {if_else(length(corpectomy_levels)>1, 'bodies', 'body')}. I carried the corpectomy laterally to the edge of the uncus and dorsally to the posterior longitudinal ligament, effectively decompressing the central canal.")) %>%
      select(paragraph) %>%
      distinct()
    
    statement <- paste(statement_df$paragraph[[1]])
    
  }
  
  if(object == "partial_corpectomy"){
    
    ## for 1 level partial corpectomy:
    if(nrow(levels_nested_df) == 1){
      
      corpectomy_direction <- unique(levels_nested_df$direction)
      corpectomy_level <- unique(levels_nested_df$level)
      
      if(corpectomy_direction == "superior"){
        interspace_character <- jh_get_cranial_caudal_interspace_body_list_function(level = corpectomy_level)$cranial_interspace
        
        cranial_to_corpectomy_level <- jh_get_cranial_caudal_interspace_body_list_function(level = corpectomy_level)$cranial_level
        
        statement_df <- levels_nested_df %>%
          mutate(paragraph = glue("I confirmed that I had adequately exposed the {corpectomy_level} body, the {interspace_character} disk, and the {cranial_to_corpectomy_level} body. I then started with the discectomy. Using a combination of a knife, currette, pituitary ronguer, and Kerrison rongeurs, the anterior longitudinal ligament was incised and the {interspace_character} disc was completely excised. Once I was satisfied with the discectomy, I used a combination of a burr and rongeur to excise roughly 60% of the {corpectomy_level} vertebral body. I carried the corpectomy laterally to the edge of the uncus and dorsally to the posterior longitudinal ligament, effectively decompressing the central canal.")) %>%
          select(paragraph) %>%
          distinct()
      }else{
        interspace_character <- jh_get_cranial_caudal_interspace_body_list_function(level = corpectomy_level)$caudal_interspace
        
        caudal_to_corpectomy_level <- jh_get_cranial_caudal_interspace_body_list_function(level = corpectomy_level)$caudal_level
        
        statement_df <- levels_nested_df %>%
          mutate(paragraph = glue("I confirmed that I had adequately exposed the {corpectomy_level} body, the {interspace_character} disk, and the {caudal_to_corpectomy_level} body. I then started with the discectomy. Using a combination of a knife, currette, pituitary ronguer, and Kerrison rongeurs, the anterior longitudinal ligament was incised and the {interspace_character} disc was completely excised. Once I was satisfied with the discectomy, I used a combination of a burr and rongeur to excise roughly 60% of the {corpectomy_level} vertebral body. I carried the corpectomy laterally to the edge of the uncus and dorsally to the posterior longitudinal ligament, effectively decompressing the central canal.")) %>%
          select(paragraph) %>%
          distinct()
      }
      
    }
    ####### 2 level partial corpectomy #####
    if(nrow(levels_nested_df) > 1){
      corpectomy_levels <- glue_collapse(unique(levels_nested_df$level), sep = ", ", last = " and ")
      cranial_level_character <- jh_get_vertebral_level_function(number = unique(min(levels_nested_df$vertebral_number)))
      caudal_level_character <- jh_get_vertebral_level_function(number = unique(max(levels_nested_df$vertebral_number)))
      interspace_character <- jh_get_cranial_caudal_interspace_body_list_function(level = cranial_level_character)$caudal_interspace
      
      statement_df <- levels_nested_df %>%
        mutate(paragraph = glue("I confirmed that I had adequately exposed {cranial_level_character} body, the {interspace_character} disk, and the {caudal_level_character} body. I then started with the discectomy. Using a combination of a knife, currette, pituitary ronguer, and Kerrison rongeurs, the anterior longitudinal ligament was incised and the {interspace_character} disc was completely excised. Once I was satisfied with the discectomy, I used a combination of a burr and rongeur to excise roughly 60% of the {cranial_level_character} and {caudal_level_character} vertebral bodies. I carried the corpectomies laterally to the edge of the uncus and dorsally to the posterior longitudinal ligament, effectively decompressing the central canal.")) %>%
        select(paragraph) %>%
        distinct()
    }
    

    statement <- paste(statement_df$paragraph[[1]])
    
  }
  
  if(object == "corpectomy_cage"){

    cranial_level_character <- jh_get_vertebral_level_function(number = unique(min(levels_nested_df$vertebral_number)))
    
    cranial_to_corpectomy_level <- jh_get_cranial_caudal_interspace_body_list_function(level = cranial_level_character)$cranial_level
 
    caudal_level_character <- jh_get_vertebral_level_function(number = unique(max(levels_nested_df$vertebral_number)))
    
    caudal_to_corpectomy_level <- jh_get_cranial_caudal_interspace_body_list_function(level = caudal_level_character)$caudal_level
    
    
    statement_df <- levels_nested_df %>%
      mutate(paragraph = glue("I confirmed satisfactory decompression and bone preparation for the corpectomy endplates. I then measured the superior to inferior distance of the corpectomy defect and selected an appropriately sized implant. The implant was packed with graft and then inserted into the corpectomy defect and the implant had a good press fit. I obtained an xray to confirm final positioning in the AP and lateral view and I was satisfied with the positioning.")) %>%
      select(paragraph) %>%
      distinct()
    
    statement <- paste(statement_df$paragraph[[1]])
    
  }
  
  return(statement)
  
}
#############-----------------------               End              ----------------------###############


#############-----------------------   Building Paragraphs: Distinct Operations ----------------------###############
distinct_anterior_procedure_paragraph_function <- function(level_input, object_input, side_input, implant_statement_input){
  
  if(object_input == "anterior_disc_arthroplasty"){
    paragraph <- glue("I then proceeded with total disk arthroplasty at the {level_input} interspace.  Using a combination of knife, currette, pituitary ronguer, and Kerrison rongeurs, the disc was excised and the superior and inferior endplates were lightly decorticated to prepare the endplates for fusion, taking care not to disrupt the cortical endplate. The endplates were distracted and the posterior longitudinal ligament was adequately excised, fully decompressing the central canal and thecal sac. To complete the bilateral foraminotomies, I worked laterally on the left and right with a Kerrison rongeur, resecting any residual disk or osteophyte and fully decompressing the left and the right exiting nerve roots. Once I was satisfied with the decompression and end plate preparation, I trialed the implants and selected an appropriately sized disk replacement. The final implant was then inserted into the interspace of {level_input}. This completed the total disc arthroplasty of the {level_input} interspace.")
  }
  
  if(object_input == "decompression_diskectomy_fusion"){
    paragraph <- glue("I then proceeded with the discectomy, decompression, and interbody fusion of the {level_input} interspace. First, I smoothed the anterior disk space and resected any osteophyte using a combination of a ronguer and burr. Using a combination of knife, currette, pituitary ronguer, and Kerrison rongeurs, the anterior longitudinal ligament was incised, the disc was excised and the superior and inferior endplates were lightly decorticated to prepare the endplates for fusion, taking care not to disrupt the cortical endplate. The endplates were distracted and the posterior longitudinal ligament was adequately excised, fully decompressing the central canal. I worked laterally on the left and right with a Kerrison rongeur, resecting any residual disk or osteophyte and fully decompressing the left and the right exiting nerve roots to complete the bilateral foraminotomies. This completed the anterior discectomy with decompression of the {level_input} interspace and partially completed the fusion of {level_input}.")
  }
  
  if(object_input == "diskectomy_fusion"){
    paragraph <- glue("I then proceeded with the discectomy and interbody fusion of the {level_input} interspace. First, I smoothed the anterior disk space and resected any osteophyte using a combination of a ronguer and burr. Using a combination of knife, currette, pituitary ronguer, and Kerrison rongeurs, the anterior longitudinal ligament was incised, the disc was excised and the superior and inferior endplates were lightly decorticated to prepare the endplates for fusion, taking care not to disrupt the cortical endplate. This completed the anterior discectomy of {level_input} interspace and partially completed the fusion of {level_input}.")
  }
  
  if(object_input == "diskectomy_fusion_no_interbody_device"){
    paragraph <- glue("I then proceeded with the discectomy and interbody fusion of the {level_input} interspace. First, I smoothed the anterior disk space and resected any osteophyte using a combination of a ronguer and burr. Using a combination of knife, currette, pituitary ronguer, and Kerrison rongeurs, the anterior longitudinal ligament was incised and the disc was excised. The endplates were distracted and the and the superior and inferior endplates were lightly decorticated to prepare the endplates for fusion, taking care not to disrupt the cortical endplate. Once I was satisfied with the endplate preparation, bone graft was placed into the disk space. This completed the anterior discectomy and interbody fusion of the {level_input} interspace.")
  }
  
  if(object_input == "anterior_interbody_implant"){
    paragraph <- glue("I then proceeded with the insertion of the interbody implant into the {level_input} interspace. I again confirmed that the endplates were adequately decorticated and appropriately level. Once I was fully satisfied with the preparation of the endplates, I used trials and measured the disk space to determine the appropriate size of the interbody implant. {implant_statement_input} I then inserted the interbody implant into the disk space of {level_input}. The final position was confirmed using intraoperative xray. This completed the anterior interbody implant at {level_input}.")
  }
  
  
  if(object_input == "corpectomy"){
    paragraph <- glue("I then proceeded with decompression and anterior vertebral body corpectomy at the {level_input} vertebral level. I confirmed that the exposure had been carried cranially to visualze the entire {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_interspace} disk, the anterior body of {level_input} and caudally to the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_interspace} disk space. First I started with the diskectomies. Using a combination of a knife, currette, pituitary ronguer, and Kerrison rongeurs, the anterior longitudinal ligament was incised and the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_interspace} and {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_interspace} disc's were completely excised. Once I was satisfied with the diskectomies, I used a combination of a burr and rongeur's to excise roughly 80% of the {level_input} vertebral body. I carried the corpectomy dorsally to the posterior longitudinal ligament, effectively decompressing the central canal. This completed the vertebral body corpectomy at {level_input}.")
  
    }
  
  if(object_input == "corpectomy_cage"){
    paragraph <- glue("I then proceeded with the insertion of the intervertebral implant into the {level_input} interspace. I again confirmed that the endplates were adequately decorticated and appropriately level. Once I was fully satisfied with the preparation of the endplates, I used trials and measured the interspace to determine the appropriate size of the interbody implant. {implant_statement_input} I then inserted the  implant into the {level_input} space. The final position was confirmed using intraoperative xray. This completed the insertion of the intervertebral implant at {level_input}.")
  } 
  
  return(paragraph)
}



anterior_op_note_distinct_paragraph_function <- function(levels_nested_df){
  
  object_statement_paragraphs_df <- levels_nested_df %>%
    mutate(paragraph = pmap(.l = list(..1 = object, 
                                      ..2 = level, 
                                      ..3 = side, 
                                      ..4 = implant_statement), 
                            .f = ~ distinct_anterior_procedure_paragraph_function(level_input = ..2,
                                                                                  object_input = ..1,
                                                                                  side_input = ..3, 
                                                                                  implant_statement_input = ..4))
    ) %>%
    mutate(paragraph = as.character(paragraph))

  
  paragraphs <- object_statement_paragraphs_df %>%
    select(level, paragraph)
  
  # paragraphs <- glue_collapse(object_statement_paragraphs_df$paragraph, sep = "\n\n")
  
  return(paragraphs)
}
#############-----------------------               End              ----------------------###############


#############-----------------------   Paragraphs: Generate FULL Paragraphs  ----------------------###############

anterior_create_full_paragraph_statement_function <- function(procedure_paragraph_intro, df_with_levels_object_nested, paragraphs_combined_or_distinct){
  
  if(paragraphs_combined_or_distinct == "combine"){
    
    if(procedure_paragraph_intro == "anterior spinal instrumentation (distinct from an interbody implant)"){
      
      levels_df <- df_with_levels_object_nested %>%
        separate(col = level, into = c("cranial", "caudal"), sep = "-") %>%
        pivot_longer(cols = c(cranial, caudal)) %>%
        select(level = value) %>%
        filter(!is.na(level)) %>%
        mutate(level = if_else(level == "Iliac" | level == "S2AI", "S1", level)) %>%
        mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
        arrange(vertebral_number) %>%
        distinct()
    }else{
      levels_df <- df_with_levels_object_nested %>%
        select(level, vertebral_number) %>%
        distinct() %>%
        arrange(vertebral_number)
    }
    
    start_statement <- glue("I then proceeded with the {procedure_paragraph_intro} at {glue_collapse(levels_df$level, sep = (', '), last = ' and ')}.")
    
    df_with_statement <- df_with_levels_object_nested %>%
      group_by(object) %>%
      nest() %>%
      mutate(tech_statement = map(.x = data, 
                                  .f = ~ op_note_object_combine_paragraph_function(object = object, 
                                                                                   levels_nested_df = .x))) %>%
      select(object, tech_statement) %>%
      unnest(tech_statement) %>%
      distinct()
    
    end_statement <- glue("This completed the {procedure_paragraph_intro} at {glue_collapse(levels_df$level, sep = (', '), last = ' and ')}.")
    
    paragraph <- paste(start_statement, 
                       glue_collapse(df_with_statement$tech_statement, sep = " "), 
                       end_statement)
  }
  
  
  if(paragraphs_combined_or_distinct == "distinct"){
    paragraph <- anterior_op_note_distinct_paragraph_function(levels_nested_df = df_with_levels_object_nested)
    
  }
  
  return(paragraph)
  
}

#############-----------------------               End              ----------------------###############

#############-----------------------   Paragraphs: Generate All Distinct Paragraphs  ----------------------###############

all_anterior_procedures_paragraphs_function <- function(all_objects_to_add_df, bone_graft_df = tibble(name = character(), value = double())){
  
  if(nrow(bone_graft_df)>0){

    bone_graft_statement_df <- bone_graft_df %>%
      mutate(statement = as.character(glue("{value}cc of {name}")))
    
    all_statements <- glue_collapse(x = bone_graft_statement_df$statement, sep = ", ", last = " and ")
    
    fusion_graft_statement <- as.character(glue("I placed a total of {all_statements} into the fusion bed"))
    
  }else{
    fusion_graft_statement <- NULL
  }
  
  if(any(names(all_objects_to_add_df) == "screw_size_type") == FALSE){
    all_objects_to_add_df <- all_objects_to_add_df %>%
      mutate(screw_size_type = " ")
  }
  if(any(names(all_objects_to_add_df) == "implant_statement") == FALSE){
    all_objects_to_add_df <- all_objects_to_add_df %>%
      mutate(implant_statement = " ")
  }
  
  anterior_df <- all_objects_to_add_df %>%
    select(level, vertebral_number, object, side, contains("screw_size_type"), contains("implant_statement"), direction) %>%
    mutate(order_number = row_number())
  
  anterior_df <- anterior_df %>%
    mutate(object = as_factor(object)) %>%
    mutate(object = fct_relevel(object, c("decompression_diskectomy_fusion",
                                          "diskectomy_fusion",
                                          "diskectomy_fusion_no_interbody_device",
                                          "anterior_disc_arthroplasty",
                                          "corpectomy",
                                          "partial_corpectomy",
                                          "anterior_interbody_implant", 
                                          "corpectomy_cage",
                                          "screw_washer",
                                          "anterior_buttress_plate",
                                          "anterior_plate", 
                                          "anterior_plate_screw"))) %>%
    mutate(object = fct_drop(object)) %>%
    arrange(object)
  
  
  anterior_procedure_category_nested_df <- anterior_df %>%
    mutate(screw_size_type = if_else(is.na(screw_size_type), " ", screw_size_type)) %>%
    # mutate(procedure_category = str_to_lower(op_note_procedure_performed_summary_classifier_function(object = object))) %>%
    mutate(procedure_category = map(.x = object, .f = ~op_note_procedure_performed_summary_classifier_function(.x))) %>%
    unnest(procedure_category) %>%
    mutate(procedure_category = str_to_lower(procedure_category)) %>%
    mutate(paragraphs_combine_or_distinct = map(.x = procedure_category, .f = ~op_note_number_of_paragraphs_for_procedure_category(.x))) %>%
    unnest(paragraphs_combine_or_distinct) %>%
    select(level, vertebral_number , procedure_category, object, side, paragraphs_combine_or_distinct, implant_statement, screw_size_type, direction) %>%
    group_by(procedure_category) %>%
    nest() %>%
    mutate(paragraphs_combine_or_distinct = map(.x = procedure_category, .f = ~op_note_number_of_paragraphs_for_procedure_category(.x))) %>%
    unnest(paragraphs_combine_or_distinct)
  # mutate(paragraphs_combine_or_distinct = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_category)) 
  

  ########### NEW ############
  paragraphs_df <- anterior_procedure_category_nested_df %>%
    mutate(paragraphs = pmap(.l = list(..1 = procedure_category, 
                                       ..2 = data,
                                       ..3 = paragraphs_combine_or_distinct), 
                             .f = ~ anterior_create_full_paragraph_statement_function(procedure_paragraph_intro = ..1, 
                                                                                      df_with_levels_object_nested = ..2, 
                                                                                      paragraphs_combined_or_distinct = ..3))) %>%
    select(-data)
  
  procedure_order_df <- anterior_df %>%
    select(level, object, order_number) %>%
    mutate(procedure_category = map(.x = object, .f = ~op_note_procedure_performed_summary_classifier_function(.x))) %>%
    unnest(procedure_category) %>%
    mutate(procedure_category = str_to_lower(procedure_category)) %>%
    select(order_number, level, procedure_category)
  
  
  distinct_unnested_df <- paragraphs_df %>%
    filter(paragraphs_combine_or_distinct == "distinct") %>%
    unnest(paragraphs) %>%
    rename(paragraphs = paragraph) %>%
    left_join(procedure_order_df) %>%
    select(order_number, everything())%>%
    select(order_number, procedure_category, paragraphs) 
  
  combine_unnested_df <- paragraphs_df %>%
    filter(paragraphs_combine_or_distinct == "combine") %>%
    unnest(paragraphs)%>%
    left_join(procedure_order_df) %>%
    select(order_number, procedure_category, paragraphs) 
  
  full_paragraphs_unnested_df <- distinct_unnested_df %>%
    union_all(combine_unnested_df) %>%
    ungroup() %>%
    arrange(order_number) %>%
    select(paragraphs) %>%
    distinct()
  
  procedure_paragraphs <- glue_collapse(x = full_paragraphs_unnested_df$paragraphs, sep = "\n\n")

  ##################
  # procedure_paragraphs <- glue_collapse(x = paragraphs_df$paragraphs, sep = "\n\n")
   
  
  if(nrow(bone_graft_df)>0){
    procedure_paragraphs <- str_replace_all(string = procedure_paragraphs, pattern = ". The final position was ", replacement = as.character(glue(". {fusion_graft_statement}. The final position was ")))
  }
  
  
  return(procedure_paragraphs)
  
}
#############-----------------------               End              ----------------------###############

#############-----------------------  FULL ANTERIOR OPERATIVE NOTE BODY GENERATOR ----------------------###############

op_note_anterior_function <- function(all_objects_to_add_df,
                                      anterior_approach_laterality = "left-sided",
                                      approach_statement = "none",
                                      antibiotics = vector(),
                                      additional_procedures_vector = NULL,
                                      neuromonitoring_list = list(),
                                      local_anesthesia = "",
                                      bmp = NULL,
                                      anterior_biologics_df = tibble(name = character(), value = double()),
                                      bone_graft_vector = NULL,
                                      morselized_allograft = 0,
                                      morselized_autograft_separate = 0,
                                      additional_procedural_details = NULL, 
                                      deep_drains = 1, 
                                      superficial_drains = 0,
                                      end_procedure_details = NULL,
                                      closure = NULL,
                                      dressing = NULL,
                                      multiple_position_procedure = FALSE, 
                                      sex = "The patient"){
  
  he_or_she <- case_when(str_to_lower(sex) == "male" ~ "he", 
                         str_to_lower(sex) == "female" ~ "she", 
                         TRUE ~ "the patient")
  
  his_or_her <- case_when(str_to_lower(sex) == "male" ~ "his", 
                         str_to_lower(sex) == "female" ~ "her", 
                         TRUE ~ "the patient's")
  
  if(any(names(all_objects_to_add_df) == "implant_statement")== FALSE){
    all_objects_to_add_df <- all_objects_to_add_df %>%
      mutate(implant_statement = "")
  }
  
  if(any(names(all_objects_to_add_df) == "screw_size_type")== FALSE){
    all_objects_to_add_df <- all_objects_to_add_df %>%
      mutate(screw_size_type = "")
  }
  
  
  procedure_details_list <- list()
  
  procedures_numbered_list <- list()
  
  additional_procedures_for_numbered_list <- c(as.character(glue("Application of {glue_collapse(bone_graft_vector, sep = ', ', last = ', and ')}")))
  
  additional_procedures_for_numbered_list <- append(additional_procedures_for_numbered_list, additional_procedures_vector)
  
  procedures_numbered_list$primary_procedures <- anterior_op_note_procedures_performed_numbered_function(objects_added_df = all_objects_to_add_df, 
                                                                                                         additional_procedures_performed_vector = additional_procedures_for_numbered_list)
  first_paragraph_list <- list()
  
  first_paragraph_list$transport_anesthesia <- paste(glue("The patient was brought to the operating room and after obtaining appropriate IV access, {he_or_she} underwent general anesthesia."))
  
  if(length(antibiotics) == 0){
    first_paragraph_list$antibiotic_statement <- "Preoperative antibiotics were administered."
  }else if(any(antibiotics == "None (Antibiotics were held)")){
    first_paragraph_list$antibiotic_statement <- "Preoperative antibiotics were held until tissue cultures could be obtained."
  }else{
    first_paragraph_list$antibiotic_statement <- paste(glue("Preoperative antibiotics were administered including {glue_collapse(antibiotics, sep = ',', last = ' and ')}."))
  }
  
  
  if(any(str_detect(additional_procedures_vector, "Halo"))){
    first_paragraph_list$head_statement <- paste(glue("A Halo was applied to {his_or_her} skull for positioning and the pins were sequentially tightened to the appropriate torque."))
  }
  if(any(str_detect(additional_procedures_vector, "Tongs"))){
    first_paragraph_list$head_statement <- paste(glue("Cranial tongs were applied to {his_or_her} skull for positioning and an appropriate weight was selected for cranial traction."))
  }
  
  if(length(neuromonitoring_list$modalities) > 0){
    first_paragraph_list$spinal_cord_monitoring <- glue("Spinal cord monitoring needles were inserted by the neurophysiology technologist for monitoring using {glue_collapse(x = neuromonitoring_list$modalities, sep = ', ', last = ' and ')}. ")
  }
  
  if(length(neuromonitoring_list$pre_positioning_motors) > 0){
    first_paragraph_list$pre_positioning_motors <- neuromonitoring_list$pre_positioning_motors 
  }
  
  # if(any(str_detect(additional_procedures_vector, "Spinal Cord Monitoring"))){
  #   first_paragraph_list$spinal_cord_monitoring <- "Spinal Cord Monitoring needles were inserted by the neurophysiology technologist."
  # }
  
  
  if(str_detect(anterior_approach_laterality, "Lateral Retroperitoneal")){
    first_paragraph_list$positioning <- paste(glue("{str_to_title(he_or_she)} was then positioned in the lateral position on the OR table and all bony prominences were appropriately padded. After prepping and draping in the standard fashion, a surgical timeout was performed."))
    
  }else{
    first_paragraph_list$positioning <- paste(glue("{str_to_title(he_or_she)} was then positioned Supine on the OR table and all bony prominences were appropriately padded. After prepping and draping in the standard fashion, a surgical timeout was performed."))
  }
  
  
  proximal_exposure_level <- all_objects_to_add_df %>%
    filter(vertebral_number == min(vertebral_number)) %>%
    mutate(vertebral_number = round(vertebral_number - 0.25, 0)) %>%
    mutate(level = if_else(vertebral_number >=25, "S1", level)) %>%
    select(vertebral_number) %>%
    distinct() %>%
    mutate(level = jh_get_vertebral_level_function(number = vertebral_number)) %>%
    select(level) %>%
    distinct()
  
  distal_exposure_level <- all_objects_to_add_df %>%
    filter(vertebral_number == max(vertebral_number)) %>%
    mutate(vertebral_number = round(vertebral_number + 0.25, 0)) %>%
    mutate(level = if_else(vertebral_number >=25, "S1", level)) %>%
    select(vertebral_number) %>%
    distinct() %>%
    mutate(level = jh_get_vertebral_level_function(number = vertebral_number)) %>%
    select(level) %>%
    distinct() %>%
    mutate(level = if_else(level == "S2AI", "S1", 
                           if_else(level == "Iliac", "S1", 
                                   level)))
  
  ## Approach for cervical vs thoracic/lumbar
  if(max(all_objects_to_add_df$vertebral_number)<11){
    approach_statement <- if_else(approach_statement == "none", "", approach_statement)
    
    first_paragraph_list$surgical_approach <- paste(glue("A standard {anterior_approach_laterality} Smith Robinson approach was utilized to get to the anterior cervical spine. The skin, subcutaneous tissue were incised, the platysma was transected, and then blunt dissection was carried out between the sternocleidomastoid and carotid sheath laterally, and trachea and esophagus  medially, down to the prevertebral fascia. Once the anterior spine was palpated, fluoroscopy was used to localize and confirm levels. "), 
                                                    glue("The longus coli was elevated bilaterally from {proximal_exposure_level$level[[1]]} proximally and to {distal_exposure_level$level[[1]]} distally. Once exposure was adequate, the deep retractors were placed into the anterior spine. "),
                                                    approach_statement)
  }else{
    
    if(anterior_approach_laterality == "Lateral Retroperitoneal Antepsoas"){
      first_paragraph_list$surgical_approach <- glue("Fluoroscopy was used to obtain a perfect AP and lateral xray and mark the levels. The skin and subcutaneous fat was incised in line with the disk space. The aponeurosis of the external obliques were incised and the muscle fibers divided. The internal obliques were divided, and the transversus abdominis was then divided. I then bluntly dissected the plane between the retroperitoneal fat and the psoas fascia, and the peritoneal cavity was retracted medially. The surface of the psoas was identified and followed down to the vertebral body and I remained anterior to the psoas. Once I was down to the spine, a pin was placed to confirm levels using xray. The dilators were then inserted sequentially and EMG was used to test each dilator. Once the largest dilator had been placed, the retractors were inserted. ")
      
    }else if(anterior_approach_laterality == "Lateral Retroperitoneal Transpsoas"){
      first_paragraph_list$surgical_approach <- glue("Fluoroscopy was used to obtain a perfect AP and lateral xrays and mark the levels. The skin and subcutaneous fat was incised in line with the disk space. The aponeurosis of the external obliques were incised and the muscle fibers divided. The internal obliques were divided, and the transversus abdominis was then divided. I then bluntly dissected the plane between the retroperitoneal fat and the psoas fascia, and the peritoneal cavity was retracted medially. The surface of the psoas was identified and followed down to the vertebral body. Once I was down to the spine, a pin was placed to confirm levels using xray. The dilators were then inserted sequentially and EMG was used to test each dilator. Once the largest dilator had been placed, the retractors were inserted. ")
      
    }else{
      first_paragraph_list$surgical_approach <- glue("The anterior approach to the spine was carried out with assistance from the vascular surgeon. A {anterior_approach_laterality} approach was carried out down toward the spine. Once the approach was complete, retractors were placed and the surgical levels were confirmed using fluoroscopy. ")
      
    }
  }
  
  ###### LOCAL ANESTHESIA
  if(str_detect(string = str_to_lower(local_anesthesia), pattern = "exposure")){ 
    first_paragraph_list$local_anesthesia <- glue("{str_to_sentence(local_anesthesia)}")
  }
  
  procedure_details_list$approach_statement <- glue_collapse(x = first_paragraph_list, sep = " ")
  
  ################### PROCEDURE PARAGRAPHS ##################
  
  procedure_details_list$procedures <- all_anterior_procedures_paragraphs_function(all_objects_to_add_df = all_objects_to_add_df, 
                                                                                   bone_graft_df = anterior_biologics_df)
  
  ############################# CLOSURE #########################
  
  closure_statements_list <- list()
  
  if(max(all_objects_to_add_df$vertebral_number)<11){
    closure_statements_list$start <- "After confirming appropriate hemostasis, I then proceeded with closure of the wound in a layered fashion."
  }else{
    closure_statements_list$start <- "After confirming appropriate hemostasis, the wound was then closed in a layered fashion."
  }
  
  
  if(deep_drains >0){
    closure_statements_list$deep_drains_statement <- case_when(deep_drains == 1 ~ 'One drain was placed deep to the fascial layer.',
                                                               deep_drains >1 ~ paste(glue("A total of {deep_drains} drains were placed deep to the fascial layer.")))
  }
  
  if(superficial_drains > 0){
    closure_statements_list$superficial_drains_statement <- case_when(superficial_drains == 1 ~ 'One drain was placed superficial to the fascial layer.',
                                                                      superficial_drains >1 ~ paste(glue("A total of {superficial_drains} drains were placed superficial to the fascial layer.")))
  }
  
  if(length(closure > 0)){
    closure_statements_list$superficial_closure <- paste("The subdermal layer was closed and",
                                                         str_to_lower(glue_collapse(closure, sep = ', ', last = ' and ')),
                                                         if_else(length(closure) == 1, "was", "were"),
                                                         "used to close the skin layer."
    )
  }else{
    closure_statements_list$superficial_closure <- "The subdermal layers and skin layers were then closed."
  }
  
  ###### LOCAL ANESTHESIA
  if(str_detect(string = str_to_lower(local_anesthesia), pattern = "closure")){ 
    closure_statements_list$local_anesthesia <- glue("{str_to_sentence(local_anesthesia)}")
  }
  
  # closure_statements_list$dressing <- glue("Lastly, {glue_collapse(dressing, sep = ', ', last = ', and ')} was applied to the surgical site.")
  closure_statements_list$dressing <- str_replace_all(str_to_sentence(glue("Lastly, {glue_collapse(dressing, sep = ', ', last = ', and ')} was applied to the surgical site.")), pattern = "steristrips was", replacement = "steristrips were")
  
  procedure_details_list$closure <- glue_collapse(closure_statements_list, sep = " ")
  
  procedure_details_list <- map(.x = procedure_details_list, .f = ~str_replace_all(string = .x, pattern = "a 8", replacement = "an 8"))
  
  ### FINAL PARAGRAPH ####
  
  procedures_listed <- op_note_procedures_present_listed_function(objects_added_df = all_objects_to_add_df,
                                                                  # revision_decompression_vector = revision_decompression_vector,
                                                                  additional_procedures_performed_vector = additional_procedures_for_numbered_list)
  
  
  if(multiple_position_procedure == TRUE){
    procedure_details_list$final_paragraph <- glue("At the conclusion of the case, all counts were correct. {neuromonitoring_list$neuromonitoring_signal_stability} The drapes were removed and we prepared for the next portion of the procedure. I was personally present for the entirety of the {procedures_listed}.")
  }else{
    procedure_details_list$final_paragraph <- glue("At the conclusion of the case, all counts were correct. {neuromonitoring_list$neuromonitoring_signal_stability} The drapes were removed and {he_or_she} was transferred to the hospital bed, and awoke uneventfully. I was personally present for the entirety of the {procedures_listed}.")
  }
  
  procedure_paragraphs <- glue_collapse(x = procedure_details_list, sep = "\n\n")
  
  return(list(procedure_details_paragraph = procedure_paragraphs, 
              procedures_numbered_paragraph = procedures_numbered_list[[1]]))
  
}

#############-----------------------               End              ----------------------###############


#############-----------------------   PROCEDURES PERFORMED NUMBERED SECTION  ----------------------###############

anterior_op_note_procedures_performed_numbered_function <- function(objects_added_df,
                                                                    revision_decompression_vector = NULL,
                                                                    fusion_levels_vector = NULL,
                                                                    additional_procedures_performed_vector = NULL){
  
  plate_levels_df <- objects_added_df %>%
    filter(object == "anterior_plate") %>%
    mutate(level = map(.x = level, .f = ~ jh_get_cranial_caudal_interspace_body_list_function(level = .x)$cranial_level)) %>%
    union_all(objects_added_df %>%
                filter(object == "anterior_plate")%>%
                mutate(level = map(.x = level, .f = ~ jh_get_cranial_caudal_interspace_body_list_function(level = .x)$caudal_level))) %>%
    unnest(level) %>%
    select(-vertebral_number) %>%
    mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
    select(level,vertebral_number, approach, category, object, side) %>%
    distinct() %>%
    arrange(vertebral_number)
  
  objects_added_df <- objects_added_df %>%
    filter(str_detect(string = object, pattern = "anterior_plate") == FALSE) %>%
    union_all(plate_levels_df)
  
  if(length(revision_decompression_vector) > 0){
    
    if(nrow(objects_added_df)>0){
      summary_nested_df <- objects_added_df %>%
        mutate(revision_levels_vector = list(revision_decompression_vector)) %>%
        select(level, vertebral_number, approach, category, object, side, revision_levels_vector) %>%
        mutate(revision_level = map2(.x = level, .y = revision_levels_vector, .f = ~ str_detect(string = .x, pattern = .y))) %>%
        select(-revision_levels_vector) %>%
        mutate(revision_level = map(.x = revision_level, .f = ~ any(.x))) %>%
        unnest(revision_level) %>%
        mutate(object = if_else(category == "decompression" & revision_level == TRUE, paste0("revision_", object), object)) %>%
        select(level, object, vertebral_number) %>%
        # mutate(procedure_class = op_note_procedure_performed_summary_classifier_function(object = object)) %>%
        mutate(procedure_class = map(.x = object, .f = ~op_note_procedure_performed_summary_classifier_function(.x))) %>%
        unnest(procedure_class) %>%
        # mutate(procedures_per_line = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_class)) 
        mutate(procedures_per_line = map(.x = procedure_class, .f = ~op_note_number_of_paragraphs_for_procedure_category(.x))) %>%
        unnest(procedures_per_line) 
    }else{
      summary_nested_df <- objects_added_df %>%
        select(level, object, vertebral_number) %>%
        mutate(procedure_class = map(.x = object, .f = ~op_note_procedure_performed_summary_classifier_function(.x))) %>%
        unnest(procedure_class) %>%
        mutate(procedures_per_line = map(.x = procedure_class, .f = ~op_note_number_of_paragraphs_for_procedure_category(.x))) %>%
        unnest(procedures_per_line) 
    }
    
  }else{
    summary_nested_df <- objects_added_df %>%
      select(level, object, vertebral_number) %>%
      mutate(procedure_class = map(.x = object, .f = ~op_note_procedure_performed_summary_classifier_function(.x))) %>%
      unnest(procedure_class) %>%
      mutate(procedures_per_line = map(.x = procedure_class, .f = ~op_note_number_of_paragraphs_for_procedure_category(.x))) %>%
      unnest(procedures_per_line) 
  }
  
  
  summary_single_statements <- summary_nested_df %>%
    filter(procedures_per_line == "distinct") %>%
    mutate(procedure_performed_statement = glue("{procedure_class} at {level}"))%>%
    select(procedure_class, procedure_performed_statement)
  
  summary_multiple_nested <- summary_nested_df %>%
    filter(procedures_per_line == "combine") %>%
    group_by(procedure_class) %>%
    nest() %>%
    ungroup() %>%
    mutate(count = row_number()) %>%
    mutate(levels = map(.x = data, .f = ~ extract_levels_function(input_df = .x))) %>%
    select(-data) %>%
    unnest(levels) %>%
    mutate(procedure_performed_statement = glue("{procedure_class} at {levels}")) %>%
    mutate(procedure_performed_statement = if_else(procedure_class == "Pelvic instrumentation", paste("Instrumentation of the Pelvis with", levels, "fixation"), as.character(procedure_performed_statement))) %>%
    select(procedure_class, procedure_performed_statement)
  
  added_procedures_df <- tibble(procedure_performed_statement = additional_procedures_performed_vector) 
  
  procedures_numbered_df <- summary_nested_df %>%
    select(procedure_class) %>%
    distinct() %>%
    left_join(summary_single_statements %>%
                union_all(summary_multiple_nested)) %>%
    select(procedure_performed_statement) %>%
    add_row(procedure_performed_statement = if_else(is.null(fusion_levels_vector), "", paste("Posterior Spinal Fusion at", glue_collapse(fusion_levels_vector, sep = ", ", last = " and ")))) %>%
    union_all(added_procedures_df) %>%
    filter(procedure_performed_statement !="") %>%
    mutate(count = row_number()) %>%
    mutate(procedures_performed_numbered = glue("{count}. {procedure_performed_statement}")) %>%
    select(procedures_performed_numbered)
  
  glue_collapse(procedures_numbered_df$procedures_performed_numbered, sep = "\n")
}

################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 


################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 


################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 



################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- POSTERIOR -----------################-------------------------#################### 

jh_make_lateral_mass_screws_after_decompression_op_note_function <- function(procedure_paragraph_list){
  op_note_df <- enframe(procedure_paragraph_list) %>%
    unnest(value) %>%
    mutate(order = row_number()) %>%
    select(order, everything())
  
  
  lateral_mass_screw_intro <- "For lateral mass screw placement, the entry point was identified using the lateral and medial borders of the lateral mass and superior and inferior borders of the facet. The superficial cortex was opened at each entry point using a burr and the screw path drilled incrementally to the far cortex and the dorsal cortex was then tapped. "

  
  op_paragraphs_df <- op_note_df %>%
    filter(name == "procedures") %>%
    select(order, everything())
  
  
  paragraph_string_split_df <- tibble(procedure_text = str_split(op_paragraphs_df$value, pattern = "\n\n")) %>%
    unnest(procedure_text) %>%
    mutate(sequence_of_procedures = row_number()) %>%
    select(sequence_of_procedures, everything())
  
  instrumentation_not_lateral_mass_df <- paragraph_string_split_df %>%
    filter(str_detect(string = procedure_text, pattern = "I then proceeded with the posterior spinal instrumentation")) %>%
    separate(col = procedure_text, into = c("procedure_text", "lateral_mass_screws"), sep = lateral_mass_screw_intro) %>%
    mutate(procedure_text = paste(procedure_text, lateral_mass_screw_intro, "The length of the screws were recorded and bone wax was placed in the screw path. I waited to place the screws until after the decompression.")) %>%
    select(-lateral_mass_screws)
  
  lateral_mass_screws_text_df <- paragraph_string_split_df %>%
    filter(str_detect(string = procedure_text, pattern = "I then proceeded with the posterior spinal instrumentation")) %>%
    separate(col = procedure_text, into = c("procedure_text", "lateral_mass_screws"), sep = lateral_mass_screw_intro) %>%
    mutate(procedure_text = paste(procedure_text, lateral_mass_screw_intro, "The length of the screws were recorded and bone wax was placed in the screw path. I waited to place the screws until after the decompression.")) %>%
    select(sequence_of_procedures, procedure_text = lateral_mass_screws) %>%
    mutate(procedure_text = paste("I then proceeded to place the lateral mass screws.", procedure_text)) %>%
    mutate(sequence_of_procedures = as.integer(100))
  
  new_procedures_separated_df <- paragraph_string_split_df %>%
    filter(str_detect(string = procedure_text, pattern = "I then proceeded with the posterior spinal instrumentation") == FALSE) %>%
    union_all(instrumentation_not_lateral_mass_df) %>%
    union_all(lateral_mass_screws_text_df) %>%
    arrange(sequence_of_procedures)
  
  
  final_revised_all_paragraphs_df <- op_note_df %>%
    filter(name == "procedures") %>%
    mutate(value = glue_collapse(x = new_procedures_separated_df$procedure_text, sep = "\n\n")) %>%
    union_all(op_note_df %>% filter(name != "procedures")) %>%
    arrange(order)
  
  glue_collapse(x = final_revised_all_paragraphs_df$value, sep = "\n\n")
}

#############-----------------------   GENERATE FULL POSTERIOR OPERATIVE NOTE  ----------------------###############
#############-----------------------   GENERATE FULL POSTERIOR OPERATIVE NOTE  ----------------------###############
#############-----------------------   GENERATE FULL POSTERIOR OPERATIVE NOTE  ----------------------###############

op_note_posterior_function <- function(all_objects_to_add_df = tibble(level = character(), 
                                                                      approach = character(),
                                                                      category = character(),
                                                                      vertebral_number = character(),
                                                                      implant = character(),
                                                                      object = character(),
                                                                      side = character(), 
                                                                      implant_statement = character(),
                                                                      screw_size_type = character()),
                                       fusion_levels_df = tibble(level = character(), vertebral_number = double(), object = character()),
                                       head_position = "Cranial Tongs", 
                                       surgical_approach = "Midline",
                                       approach_mis_open = "Open",
                                       approach_robot_nav_xray = "Open",
                                       implant_start_point_method_input = "",
                                       implant_confirmation_method = "Intraoperative Xray was used to confirm the final position of all implants.",
                                       neuromonitoring_list = list(modalities = c(), emg = "", pre_positioning_motors = "", neuromonitoring_signal_stability = ""),
                                       local_anesthesia = "",
                                       revision_decompression_vector = c(),
                                       revision_implants_df = tibble(level = character(), vertebral_number = double(), object = character(), x = double(), y = double(), prior_rod_connected = character(), remove_retain = character()),
                                       left_main_rod_size = "5.5",
                                       left_main_rod_material = "Titanium",
                                       right_main_rod_size = "5.5",
                                       right_main_rod_material = "Titanium",
                                       additional_rods_statement = NULL,
                                       antibiotics = vector(),
                                       prior_fusion_levels_vector = vector(),
                                       instrumentation_removal_vector = vector(),
                                       additional_procedures_vector = NULL,
                                       bmp = 0,
                                       biologics_list = list(),
                                       # bone_graft_vector = NULL,
                                       # morselized_allograft = 0,
                                       morselized_autograft_separate = FALSE,
                                       # structural_allograft_location = NULL,
                                       # structural_autograft_harvest = NULL,
                                       # structural_autograft_location = NULL,
                                       deep_drains = 0,
                                       superficial_drains = 0,
                                       end_procedure_details = NULL,
                                       closure = NULL,
                                       dressing = NULL, 
                                       multiple_position_procedure = FALSE,
                                       alignment_correction_technique = " ",
                                       sex = "The patient", 
                                       lateral_mass_screws_after_decompression = "No", 
                                       instruments_used_for_bony_work = "High-speed burr only"){
  
  he_or_she <- case_when(str_to_lower(sex) == "male" ~ "he", 
                         str_to_lower(sex) == "female" ~ "she", 
                         TRUE ~ "the patient")
  his_or_her <- case_when(str_to_lower(sex) == "male" ~ "his", 
                         str_to_lower(sex) == "female" ~ "her", 
                         TRUE ~ "the patient's")
  
  procedure_details_list <- list()
  
  procedures_numbered_list <- list()
  
  if(is.null(all_objects_to_add_df)){
    all_objects_to_add_df <- tibble(level = character(),
                                    approach = character(),
                                    category = character(),
                                    vertebral_number = double(),
                                    implant = character(),
                                    object = character(),
                                    side = character(),
                                    x = double(),
                                    y = double(),
                                    fusion = character(),
                                    interbody_fusion = character(),
                                    body_interspace = character(),
                                    fixation_uiv_liv = character())
  }
  
  if(is.null(revision_implants_df)){
    revision_implants_df <- tibble(level = character(),
                                   approach = character(),
                                   category = character(),
                                   vertebral_number = double(),
                                   implant = character(),
                                   object = character(),
                                   side = character(),
                                   x = double(),
                                   y = double(),
                                   prior_rod_connected = character(),
                                   remove_retain = character())
  }
  
  if("implant_statement" %in% names(all_objects_to_add_df) == FALSE){
    all_objects_to_add_df <- all_objects_to_add_df %>%
      mutate(implant_statement = " ")
  }
  if("screw_size_type" %in% names(all_objects_to_add_df) == FALSE){
    all_objects_to_add_df <- all_objects_to_add_df %>%
      mutate(screw_size_type = " ")
  }
  
  ### MUST BE AT TOP BECAUSE THEY ARE REMOVED IN THE NEXT SECTION
  crosslink_df <- all_objects_to_add_df %>%
    filter(object == "crosslink") %>%
    select(level, object)
  
  structural_allograft_df <- all_objects_to_add_df %>%
    filter(object == "structural_allograft") %>%
    select(level, object)
  
  all_objects_to_add_df <- all_objects_to_add_df %>%
    filter(object != "crosslink") %>%
    filter(object != "structural_allograft")
  
  additional_procedures_for_numbered_list <- as.list(additional_procedures_vector)
  
  if(length(biologics_list)>0){
    additional_procedures_for_numbered_list$bone_grafting <- "Application of bone graft/osteopromotive material"
  }
  
  # additional_procedures_for_numbered_list <- append(additional_procedures_for_numbered_list, additional_procedures_vector)
  
  if(nrow(structural_allograft_df) > 0 ){
    
    additional_procedures_for_numbered_list$structural_allograft <- paste(glue("Application of strucural allograft strut at {glue_collapse(structural_allograft_df$level, sep = ', ', last = ', and ')}"))
    
    # additional_procedures_for_numbered_list <- append(additional_procedures_for_numbered_list, c(as.character(glue("Application of strucural allograft strut at {glue_collapse(structural_allograft_df$level, sep = ', ', last = ', and ')}"))))
  }
  if(morselized_autograft_separate == TRUE){
    additional_procedures_for_numbered_list$morselized_autograft_separate <- "Application of morselized autograft obtained through a separate fascial incision"
  }
  
  procedures_numbered_list$primary_procedures <- op_note_procedures_performed_numbered_function(objects_added_df = all_objects_to_add_df, 
                                                                                                revision_decompression_vector = revision_decompression_vector, 
                                                                                                fusion_levels_vector = fusion_levels_df$level, 
                                                                                                additional_procedures_performed_vector = additional_procedures_for_numbered_list)
  
  first_paragraph_list <- list()

  
  first_paragraph_list$transport_anesthesia <- paste(glue("The patient was brought to the operating room and after obtaining appropriate IV access, {he_or_she} underwent general anesthesia."))
  
  if(length(antibiotics) == 0){
    first_paragraph_list$antibiotic_statement <- "Preoperative Antibiotics were administered."
  }else if(any(antibiotics == "None (Antibiotics were held)")){
    first_paragraph_list$antibiotic_statement <- "Preoperative antibiotics were held until tissue cultures could be obtained."
  }else{
    first_paragraph_list$antibiotic_statement <- paste0("Preoperative Antibiotics were administered including ", glue_collapse(antibiotics, sep = ", ", last = " and "), ".")
  }
  
  if(length(neuromonitoring_list$modalities) > 0){
    first_paragraph_list$spinal_cord_monitoring <- glue("Spinal cord monitoring needles were inserted by the neurophysiology technologist for monitoring using {glue_collapse(x = neuromonitoring_list$modalities, sep = ', ', last = ' and ')}. ")
  }
  
  if(length(neuromonitoring_list$pre_positioning_motors) > 0){
    first_paragraph_list$pre_positioning_motors <- neuromonitoring_list$pre_positioning_motors 
  }
  

  first_paragraph_list$head_statement <- as.character(case_when(
    head_position == "Supine/Lateral" ~ glue("{str_to_title(his_or_her)} head rested in a position of comfort, securely on the bed."),
    head_position == "Proneview Faceplate" ~ glue("The proneview faceplate was used to pad and secure {his_or_her} head and face during surgery."),
    head_position == "C-flex head positioner" ~ glue("The C-flex head positioner was used to pad and secure {his_or_her} head during surgery."), 
    head_position == "Cranial Tongs" ~ glue("Cranial tongs were applied to {his_or_her} skull for positioning and an appropriate weight was selected for cranial traction."),
    head_position == "Halo" ~ glue("A Halo was applied to {his_or_her} skull for positioning and the pins were sequentially tightened to the appropriate torque."),
    head_position == "Mayfield" ~ glue("A Mayfield head holder was applied to {his_or_her} skull for positioning and secured to the bed.")
  ))
  
  first_paragraph_list$positioning <- paste(glue("{str_to_title(he_or_she)} was then positioned prone on the OR table and all bony prominences were appropriately padded. After prepping and draping in the standard fashion, a surgical timeout was performed."))
  
  
  ############### JUST START OVER AT THIS POINT IF IT IS A MULTIPLE POSITION CASE
  if(multiple_position_procedure == TRUE){
    first_paragraph_list <- list()
    first_paragraph_list$positioning <- paste(glue("{str_to_title(he_or_she)} was then positioned prone on the OR table and all bony prominences were appropriately padded."))
    
    first_paragraph_list$head_statement <- as.character(case_when(
      head_position == "Supine/Lateral" ~ glue("{str_to_title(his_or_her)} head rested in a position of comfort, securely on the bed."),
      head_position == "Proneview Faceplate" ~ glue("The proneview faceplate was used to pad and secure {his_or_her} head and face during surgery."),
      head_position == "C-flex head positioner" ~ glue("The C-flex head positioner was used to pad and secure {his_or_her} head during surgery."), 
      head_position == "Cranial Tongs" ~ glue("Cranial tongs were applied to {his_or_her} skull for positioning and an appropriate weight was selected for cranial traction."),
      head_position == "Halo" ~ glue("A Halo was applied to {his_or_her} skull for positioning and the pins were sequentially tightened to the appropriate torque."),
      head_position == "Mayfield" ~ glue("A Mayfield head holder was applied to {his_or_her} skull for positioning and secured to the bed.")
    ))
    
    first_paragraph_list$draping <- paste("The posterior spine was then prepped and draped in the standard fashion.")
    
  }
  
  if(nrow(all_objects_to_add_df)>0){
    proximal_exposure_level <- all_objects_to_add_df %>%
      filter(vertebral_number == min(vertebral_number)) %>%
      mutate(vertebral_number = round(vertebral_number - 0.25, 0)) %>%
      mutate(level = if_else(vertebral_number >=25, "S1", level)) %>%
      select(vertebral_number) %>%
      distinct() %>%
      mutate(level = jh_get_vertebral_level_function(number = vertebral_number)) %>%
      select(level) %>%
      distinct()
    
    distal_exposure_level <- all_objects_to_add_df %>%
      filter(vertebral_number == max(vertebral_number)) %>%
      mutate(vertebral_number = round(vertebral_number + 0.25, 0)) %>%
      mutate(level = if_else(vertebral_number >=25, "S1", level)) %>%
      select(vertebral_number) %>%
      distinct() %>%
      mutate(level = jh_get_vertebral_level_function(number = vertebral_number)) %>%
      select(level) %>%
      distinct() %>%
      mutate(level = if_else(level == "S2AI", "S1", 
                             if_else(level == "Iliac", "S1", 
                                     level)))  
    
    if(surgical_approach == "Midline"){
      first_paragraph_list$surgical_approach <- glue("A standard posterior approach to the spine was performed, exposing proximally to the {proximal_exposure_level$level[1]} level and distally to the {distal_exposure_level$level[1]} level.")
      
    }
    if(surgical_approach == "Paraspinal (Wiltse)"){
      first_paragraph_list$surgical_approach <- glue("A paraspinal (Wiltse) posterior approach to the spine was performed. Skin was incised with a knife and cautery was used to control any skin bleeding. I dissected down until I identifed the muscle layer and bluntly dissected between the multifidus and longissimus intermuscular plane. I exposed proximally to the {proximal_exposure_level$level[1]} level and distally to the {distal_exposure_level$level[1]} level. I manually palpated the transverse process and confirmed the level with xray. ")
      
    }
    if(surgical_approach == "Stab"){
      first_paragraph_list$surgical_approach <- glue("Intraoperative xray was used to confirm levels and stab incisions were made to access each pedicle from the {proximal_exposure_level$level[1]} level and distally to the {distal_exposure_level$level[1]} level.")
      
    }
    
  }else{
    if(nrow(revision_implants_df)>0){
      proximal_exposure_level <- revision_implants_df %>%
        filter(vertebral_number == min(vertebral_number)) %>%
        mutate(vertebral_number = round(vertebral_number - 0.25, 0)) %>%
        mutate(level = if_else(vertebral_number >=25, "S1", level)) %>%
        select(vertebral_number) %>%
        distinct() %>%
        mutate(level = jh_get_vertebral_level_function(number = vertebral_number)) %>%
        select(level) %>%
        distinct()
      
      distal_exposure_level <- revision_implants_df %>%
        filter(vertebral_number == max(vertebral_number)) %>%
        mutate(vertebral_number = round(vertebral_number + 0.25, 0)) %>%
        mutate(level = if_else(vertebral_number >=25, "S1", level)) %>%
        select(vertebral_number) %>%
        distinct() %>%
        mutate(level = jh_get_vertebral_level_function(number = vertebral_number)) %>%
        select(level) %>%
        distinct() %>%
        mutate(level = if_else(level == "S2AI", "S1", 
                               if_else(level == "Iliac", "S1", 
                                       level)))  
      
      first_paragraph_list$surgical_approach <- glue("A standard posterior approach to the spine was performed, exposing proximally to the {proximal_exposure_level$level[1]} level and distally to the {distal_exposure_level$level[1]} level.")
    }else{
      first_paragraph_list$surgical_approach <- glue("A standard posterior approach to the spine was performed, appropriately exposing all necessary levels.")
    }
  } 
  
  if(str_detect(string = str_to_lower(local_anesthesia), pattern = "exposure")){ 
    first_paragraph_list$local_anesthesia <- glue("{str_to_sentence(local_anesthesia)}")
  }
  
  procedure_details_list$approach_statement <- glue_collapse(x = first_paragraph_list, sep = " ")
  
  ################### Revision Procedures PARAGRAPHS ##################
  revision_statements_list <- list()
  
  
  if(any(additional_procedures_vector == "Exploration of prior spinal fusion")){
    if(length(prior_fusion_levels_vector) > 0){
      revision_statements_list$exploration_fusion_statement <- glue("I then proceeded with exploration of the prior fusion. Overlying scar was excised from the posterior elements and the prior fusion at {glue_collapse(prior_fusion_levels_vector, sep = ', ', last = ' and ')} was inspected and examined for any motion.")
    }
  } 
  
  if(any(str_detect(additional_procedures_vector, pattern = "Irrigation"))){
    revision_statements_list$exploration_fusion_statement <- glue("I proceeded with irrigation and debridement of the wound. Starting deep and working superficially, any necrotic appearing tissue was debrided in a layered fashion. The wound was then irrigated with a copious amount of irrigation and the wound was again debrided of any necrotic or ischemic appearing tissue. The wound was again irrigated copiously until I was satisfied with the debridement and appearance of the wound.")
  } 
  
  if(length(revision_statements_list)>0){
    procedure_details_list$exploration <- paste("Once the exposure was completed, ",
                                                glue_collapse(revision_statements_list, sep = " "))
  }
  
  
  if(nrow(revision_implants_df) > 0){
    procedure_details_list$revision_implants <- revision_implants_paragraph_function(revision_implants_details_df = revision_implants_df)
  }
  
  
  ################### PROCEDURE PARAGRAPHS ##################
  
  if(nrow(all_objects_to_add_df)>0){
    if(length(approach_robot_nav_xray) > 0){
      approach_robot_nav_xray <- approach_robot_nav_xray
    }else{
      approach_robot_nav_xray <- "Na"
    }
    
    procedure_details_list$procedures <- op_note_procedure_paragraphs_function(objects_added_df = all_objects_to_add_df,
                                                                               revision_decompression_vector = revision_decompression_vector, 
                                                                               approach_technique = approach_mis_open, 
                                                                               image_guidance = approach_robot_nav_xray, 
                                                                               neuromonitoring_emg_statement = neuromonitoring_list$emg, 
                                                                               implant_start_point_method = implant_start_point_method_input, 
                                                                               implant_confirmation_method = implant_confirmation_method)
  }
  
  
  ################### COMPLETING INSTRUMENTATION ##################
  if(nrow(all_objects_to_add_df)>0){
  posterior_implants_all_df <- all_objects_to_add_df %>%
    mutate(procedure_category = map(.x = object, .f = ~ op_note_procedure_performed_summary_classifier_function(.x))) %>%
    unnest(procedure_category) %>%
    mutate(procedure_category = str_to_lower(procedure_category)) %>%
    filter(str_detect(string = procedure_category, pattern = "instrumentation")) %>%
    left_join(levels_numbered_df) %>%
    select(level, vertebral_number, procedure_category, object, side) %>%
    arrange(vertebral_number) %>%
    remove_missing() %>%
    group_by(level, object) %>%
    mutate(object = if_else(str_detect(object, "pelvic_screw"), "pelvic_screw", object)) %>%
    add_tally(name = "total_per_level") %>%
    mutate(side = if_else(total_per_level == 2, "bilateral", side)) %>%
    ungroup() %>%
    distinct() 
  
  posterior_implant_df <- posterior_implants_all_df %>%
    filter(procedure_category != "pelvic instrumentation") %>%
    mutate(level = if_else(level == "Iliac", "Pelvis", level)) %>%
    mutate(level = if_else(level == "S2AI", "Pelvis", level))
  
  instrumented_levels_vector <- unique((posterior_implants_all_df %>% 
                                          mutate(level = if_else(level == "Iliac", "the pelvis", level)) %>%
                                          mutate(level = if_else(level == "S2AI", "the pelvis", level)) %>%
                                          distinct())$level)
  }else{
    posterior_implant_df <- tibble(level = character(), side = character(), object = character())
  }


  if(nrow(posterior_implant_df) > 0){ 
    rod_statements_list <- list()

    if(any(str_detect(posterior_implant_df$side, "bilateral"))){

      rod_statements_list$rod_contouring <- glue("To complete the spinal instrumentation, a {left_main_rod_size} {left_main_rod_material} rod was contoured for the left and a {right_main_rod_size} {right_main_rod_material} rod was contoured for the right. ") 
      rod_statements_list$alignment_and_rod_placement <-  case_when(
        str_detect(string = alignment_correction_technique, pattern = "rod benders") ~ glue("The rods were set into place, secured with set screws and {alignment_correction_technique}."), 
        str_detect(string = str_to_lower(alignment_correction_technique), pattern = "pro-axis") ~ glue("{alignment_correction_technique} and the rods were set into place and secured with set screws."), 
        TRUE ~ glue("The rods placed into position and secured with set screws. "))
      
      if(nrow(revision_implants_df) > 0){
        if(any(revision_implants_df$prior_rod_connected == "yes")){
          rod_statements_list$revision_rod_connector <- paste("Rod connectors were used to connect the new rod to the previously placed construct that was left in place.")
        }
      }
      
      rod_statements_list$additional_rods_statement <- additional_rods_statement
      rod_statements_list$set_screws <- glue("The set screws were then tightened with a final tightener to the appropriate torque.")

      if(nrow(crosslink_df) > 0){
        rod_statements_list$crosslink <- glue("To increase the rigidity of the construct, a crosslink connector was applied at the level of {glue_collapse(crosslink_df$level, sep = ', ', last = ' and ')}.")
      }
    
      rod_statements_list$xrays <- glue("Intraoperative Xray was used to confirm the final position of all implants.")
      rod_statements_list$final <- glue("This completed the instrumentation of {glue_collapse(x = instrumented_levels_vector, sep = ', ', last = ', and ')}.")
      
      procedure_details_list$posterior_instrumentation_rods <- glue_collapse(rod_statements_list, sep = " ")
      
    }else{
      if(any(str_detect(posterior_implant_df$side, "left"))){
        rod_statements_list$rod_contouring <- glue("To complete the spinal instrumentation, a {left_main_rod_size} {left_main_rod_material} rod was contoured for the left and the rod was placed into position and secured with set screws.")
        rod_statements_list$additional_rods_statement <- additional_rods_statement
        rod_statements_list$set_screws <- glue("The set screws were then tightened with a final tightener to the appropriate torque.")
        rod_statements_list$xrays <- glue("Intraoperative Xray was used to confirm the final position of all implants.")
        rod_statements_list$final <- glue("This completed the instrumentation of {glue_collapse(x = instrumented_levels_vector, sep = ', ', last = ', and ')}.")
        
        procedure_details_list$posterior_instrumentation_rods <- glue_collapse(rod_statements_list, sep = " ")
        
      }else{
        rod_statements_list$rod_contouring <- glue("To complete the spinal instrumentation, a {right_main_rod_size} {right_main_rod_material} rod was contoured for the right and the rod was placed into position and secured with set screws.")
        rod_statements_list$additional_rods_statement <- additional_rods_statement
        rod_statements_list$set_screws <- glue("The set screws were then tightened with a final tightener to the appropriate torque.")
        rod_statements_list$xrays <- glue("Intraoperative Xray was used to confirm the final position of all implants.")
        rod_statements_list$final <- glue("This completed the instrumentation of {glue_collapse(x = instrumented_levels_vector, sep = ', ', last = ', and ')}.")
        
        procedure_details_list$posterior_instrumentation_rods <- glue_collapse(rod_statements_list, sep = " ")
      }
    }
    
  }
  
  if(nrow(structural_allograft_df) > 0){
    procedure_details_list$structural_allograft <- glue("To aide in posterior arthrodesis and to cover the dorsal bony defect, I then proceeded with placement of a structural allograft strut. The dorsal bony defect was measured and a structural allograft was selected and trimmed to the appropriate size. The allograft was secured into place, spanning the level of {glue_collapse(structural_allograft_df$level, sep = ', ', last = ' and ')}. This completed the application of structural allograft.")
  }
  
  
  #############   #############   #############  FUSIONS FUSIONS ###########  #############   ############# 
  #############   #############   #############  FUSIONS FUSIONS ###########  #############   ############# 
  
  fusion_statement_list <- list()
  
  if(nrow(fusion_levels_df) > 0){
    fusion_segments_df <- fusion_levels_df %>%
      separate(col = level, into = c("proximal", "distal")) %>%
      pivot_longer(cols = c(proximal, distal), names_to = "name", values_to = "level") %>%
      select(level) %>%
      mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
      arrange(vertebral_number) %>%
      distinct() 
    
    fusion_statement_list$fusion_begin_statement <- glue("I then proceeded with the spinal fusion portion of the procedure. The posterior elements of {glue_collapse(fusion_segments_df$level, sep = ', ', last = ', and ')} were decorticated.")
    
    if(length(biologics_list)>0){
      fusion_statement_list$biologics <- paste("After the fusion bed was prepared,", glue_collapse(x = biologics_list, sep = ", ", last = " and "), "was impacted into the fusion bed.")
    }
    
    if(bmp > 0){
      fusion_statement_list$bmp_statement <- glue("To improve the odds of a successful fusion, {bmp}mg of BMP was placed into the posterior fusion bed. ")
    }
    
    if(morselized_autograft_separate == TRUE){
      fusion_statement_list$morselized_autograft_separate <- "A separate fascial incision was made to obtain additional morselized autograft and this was placed into the fusion bed. "
    }
    
    
    fusion_statement_list$complete <- glue("This completed the posterior spinal fusion of {glue_collapse(fusion_levels_df$level, sep = ', ', last = ', and ')}.")
    
    procedure_details_list$posterior_fusion_details <- glue_collapse(fusion_statement_list, sep = " ")
  }
  
  ############################# CLOSURE #########################
  closure_statements_list <- list()
  
  closure_statements_list$start <- "I then proceeded with closure of the wound."
  
  if(length(end_procedure_details) > 0){
    closure_statements_list$added_to_wound <- glue("{glue_collapse(end_procedure_details, sep = ', ', last = ' and ')} was then placed into the surgical bed.")
  }
  
  if(deep_drains >0){
    closure_statements_list$deep_drains_statement <- case_when(deep_drains == 1 ~ 'One drain was placed deep to the fascial layer.',
                                                               deep_drains >1 ~ paste(glue("A total of {deep_drains} drains were placed deep to the fascial layer.")))
  }
  
  closure_statements_list$layered_closure <- "The wound was then closed in a layered fashion. The fascial layer was closed with a watertight closure."
  
  if(superficial_drains > 0){
    closure_statements_list$superficial_drains_statement <- case_when(superficial_drains == 1 ~ 'One drain was placed superficial to the fascial layer.',
                                                                      superficial_drains >1 ~ paste(glue("A total of {superficial_drains} drains were placed superficial to the fascial layer.")))
  }
  
  if(length(closure > 0)){
    if(any(closure == "left open")){
      closure_statements_list$superficial_closure <- paste("After the fascial layer was closed, the subdermal and skin layers were left open.")
      
    }else{
      closure_statements_list$superficial_closure <- paste("The subdermal layer was closed and",
                                                           str_to_lower(glue_collapse(closure, sep = ', ', last = ' and ')),
                                                           if_else(length(closure) == 1, "was", "were"),
                                                           "used to close the skin layer."
      )
      
    }
    
  }else{
    closure_statements_list$superficial_closure <- "The subdermal layers and skin layers were then closed."
    
  }
  ###### LOCAL ANESTHESIA
  if(str_detect(string = str_to_lower(local_anesthesia), pattern = "closure")){ 
    closure_statements_list$local_anesthesia <- glue("{str_to_sentence(local_anesthesia)}")
  }
  
  if(any(dressing == "Wound Vac")){
    closure_statements_list$dressing <- "A wound vac was then applied to the open wound, measuring roughly ***cm length by ***cm width by ***cm in depth."
    
  }else{
    closure_statements_list$dressing <- str_replace_all(str_to_sentence(glue("Lastly, {glue_collapse(dressing, sep = ', ', last = ', and ')} was applied to the surgical site.")), pattern = "steristrips was", replacement = "steristrips were")
    
  }
  
  procedure_details_list$closure <- glue_collapse(closure_statements_list, sep = " ")
  
  procedure_details_list <- map(.x = procedure_details_list, .f = ~str_replace_all(string = .x, pattern = "a 8", replacement = "an 8"))
  
  ### FINAL PARAGRAPH ####
  
  procedures_listed <- op_note_procedures_present_listed_function(objects_added_df = all_objects_to_add_df, 
                                                                  revision_decompression_vector = revision_decompression_vector, 
                                                                  fusion_levels_vector = fusion_levels_df$level, 
                                                                  additional_procedures_performed_vector = additional_procedures_for_numbered_list)
  
  
  if(multiple_position_procedure == TRUE){
    procedure_details_list$final_paragraph <- glue("At the conclusion of the case, all counts were correct. {neuromonitoring_list$neuromonitoring_signal_stability} The drapes were removed and {he_or_she} was turned uneventfully. I was personally present for the entirety of this portion of the case, including {procedures_listed}.")
  }else{
    procedure_details_list$final_paragraph <- glue("At the conclusion of the case, all counts were correct. {neuromonitoring_list$neuromonitoring_signal_stability} The drapes were removed and {he_or_she} was turned onto the hospital bed, and awoke uneventfully. I was personally present for the entirety of the case, including {procedures_listed}.")
  }
  
  
  if(lateral_mass_screws_after_decompression == "Yes"){
    procedure_paragraphs <- jh_make_lateral_mass_screws_after_decompression_op_note_function(procedure_details_list)
    
  }else{
    procedure_paragraphs <- glue_collapse(x = procedure_details_list, sep = "\n\n")
  }

  procedure_paragraphs <- str_replace_all(procedure_paragraphs, "a  ", "a ")
  
  if(instruments_used_for_bony_work == "Bone scalpel only"){
    procedure_paragraphs <- str_replace_all(procedure_paragraphs, "high-speed burr", "bone scalpel")
  }
  
  if(instruments_used_for_bony_work == "High-speed burr and bone scalpel"){
    procedure_paragraphs <- str_replace_all(procedure_paragraphs, "high-speed burr", "bone scalpel and high-speed burr")
  }
  
  
  return(list(procedure_details_paragraph = procedure_paragraphs, 
              procedures_numbered_paragraph = procedures_numbered_list[[1]] 
              # procedure_details_list = procedure_details_list
              ))
  
}



#############-----------------------               End              ----------------------###############

#############-----------------------   Generate ALL THE PROCEDURE PARAGRAPHS: input 1: the objects added df ----------------------###############

op_note_procedure_paragraphs_function <- function(objects_added_df, 
                                                  revision_decompression_vector = c(), 
                                                  approach_technique = "Na", 
                                                  image_guidance = "Na", 
                                                  neuromonitoring_emg_statement = "No", 
                                                  implant_start_point_method = "NA",
                                                  implant_confirmation_method = "NA"){
  
  if("implant_statement" %in% names(objects_added_df) == FALSE){ 
    objects_added_df <- objects_added_df %>%
      mutate(implant_statement = "")
  }
  
  if("screw_size_type" %in% names(objects_added_df) == FALSE){
    objects_added_df <- objects_added_df %>%
      mutate(screw_size_type = " ")
  }
  
  if(length(revision_decompression_vector) > 0){
    if(nrow(objects_added_df)>0){
      df_for_paragraphs <- objects_added_df %>%
        mutate(revision_levels_vector = list(revision_decompression_vector)) %>%
        select(level, vertebral_number, approach, category, object, side, revision_levels_vector, implant_statement, screw_size_type) %>%
        mutate(revision_level = map2(.x = level, .y = revision_levels_vector, .f = ~ str_detect(string = .x, pattern = .y))) %>%
        select(-revision_levels_vector) %>%
        mutate(revision_level = map(.x = revision_level, .f = ~ any(.x))) %>%
        unnest(revision_level) %>%
        replace_na(list(implant_statement = " ", screw_size_type = " ", revision_level = FALSE)) %>%
        mutate(revision_label = paste0("revision_", object)) %>%
        mutate(object = if_else(revision_level == FALSE, object, if_else(category == "decompression", revision_label, object))) %>%
        select(-revision_label) %>%
        mutate(procedure_category = map(.x = object, .f = ~ op_note_procedure_performed_summary_classifier_function(.x))) %>%
        unnest(procedure_category) %>%
        mutate(procedure_category = str_to_lower(procedure_category)) %>%
        select(level, vertebral_number, procedure_category, object, side, implant_statement, screw_size_type) %>%
        mutate(procedures_combine = map(.x = procedure_category, .f = ~ op_note_number_of_paragraphs_for_procedure_category(.x))) %>%
        unnest(procedures_combine) %>%
        group_by(procedure_category, procedures_combine, object) %>%
        nest() %>%
        ungroup() %>%
        group_by(procedure_category, procedures_combine) %>%
        nest() %>%
        mutate(nested_data = map(.x = data, .f =  ~ unnest(data = .x, cols = c()))) %>%
        select(-data)  
    }else{
      df_for_paragraphs <- objects_added_df %>%
        mutate(procedure_category = map(.x = object, .f = ~ op_note_procedure_performed_summary_classifier_function(.x))) %>%
        unnest(procedure_category) %>%
        mutate(procedure_category = str_to_lower(procedure_category)) %>%
        select(level, vertebral_number, procedure_category, object, side, implant_statement, screw_size_type) %>%
        mutate(procedures_combine = map(.x = procedure_category, .f = ~ op_note_number_of_paragraphs_for_procedure_category(.x))) %>%
        unnest(procedures_combine) %>%
        group_by(procedure_category, procedures_combine, object) %>%
        nest() %>%
        ungroup() %>%
        group_by(procedure_category, procedures_combine) %>%
        nest() %>%
        mutate(nested_data = map(.x = data, .f = ~ unnest(data = .x, cols = c()))) %>%
        select(-data)  
    }
  }else{
    df_for_paragraphs <- objects_added_df %>%
      mutate(procedure_category = map(.x = object, .f = ~ op_note_procedure_performed_summary_classifier_function(.x))) %>%
      unnest(procedure_category) %>%
      mutate(procedure_category = str_to_lower(procedure_category)) %>%
      select(level, vertebral_number, procedure_category, object, side, implant_statement, screw_size_type) %>%
      mutate(procedures_combine = map(.x = procedure_category, .f = ~ op_note_number_of_paragraphs_for_procedure_category(.x))) %>%
      unnest(procedures_combine) %>%
      group_by(procedure_category, procedures_combine, object) %>%
      nest() %>%
      ungroup() %>%
      group_by(procedure_category, procedures_combine) %>%
      nest() %>%
      mutate(nested_data = map(.x = data, .f = ~ unnest(data = .x, cols = c()))) %>%
      select(-data)  
  }
  # THIS CREATES A DATAFRAME WITH NESTED DATAFRAMES.... full dataframe names = 'procedure_category, procedures_combine, nested_data'
  # nested df names ('nested_data') = 'object, level, vertebral_number, side, implant_statement, screw_size_type'
  
  if(is.na(implant_start_point_method) | is.null(implant_start_point_method)){
    implant_start_point_method <- "NA"
  }
  if(is.na(implant_confirmation_method) | is.null(implant_confirmation_method)){
    implant_confirmation_method <- "NA"
  }
  
  procedures_op_full_df <- df_for_paragraphs %>%
    mutate(approach_technique_input = approach_technique,
           image_guidance_technique_input = image_guidance[1], 
           neuromonitoring_emg_statement_input = neuromonitoring_emg_statement, 
           method_implant_start_point = implant_start_point_method, 
           method_implant_confirmation = implant_confirmation_method) %>%
    mutate(procedure_statement = pmap(list(..1 = procedure_category,
                                           ..2 = nested_data, 
                                           ..3 = procedures_combine, 
                                           ..4 = approach_technique_input, 
                                           ..5 = image_guidance_technique_input, 
                                           ..6 = neuromonitoring_emg_statement_input,
                                           ..7 = method_implant_start_point, 
                                           ..8 = method_implant_confirmation),
                                      .f = ~create_full_paragraph_statement_function(procedure_paragraph_intro = ..1,
                                                                                     df_with_levels_object_nested = ..2, 
                                                                                     paragraphs_combined_or_distinct = ..3, 
                                                                                     approach_technique = ..4, 
                                                                                     image_guidance = ..5, 
                                                                                     neuromonitoring_emg = ..6, 
                                                                                     implant_start_point_method = ..7, 
                                                                                     implant_confirmation_method = ..8))) %>%
    select(procedure_category, procedures_combine, procedure_statement) %>%
    unnest(procedure_statement)
  
  glue_collapse(x = procedures_op_full_df$procedure_statement, sep = "\n\n")
  
}

#############-----------------------               End              ----------------------###############

#############-----------------------   Generate a FULL PARAGRAPH ----------------------###############
create_full_paragraph_statement_function <- function(procedure_paragraph_intro, 
                                                     df_with_levels_object_nested, 
                                                     paragraphs_combined_or_distinct, 
                                                     approach_technique = "NA", 
                                                     image_guidance = "NA", 
                                                     implant_start_point_method = "NA",
                                                     implant_confirmation_method = "NA",
                                                     neuromonitoring_emg = "No"){
  
  if("data" %in% names(df_with_levels_object_nested)){
    df_with_levels_object_nested <- df_with_levels_object_nested %>%
      unnest(data)
  }
  
  if(paragraphs_combined_or_distinct == "combine"){
    
    #IMAGING MODALITY USED FOR STARTPOINTS?:
    if(procedure_paragraph_intro == "posterior spinal instrumentation" && str_length(implant_start_point_method) > 5){
      implant_start_point_method_text <- implant_start_point_method
    }else{
      implant_start_point_method_text <- ""
    }
    
    df_with_statement <- df_with_levels_object_nested %>%
      mutate(object = if_else(str_detect(object, "pelvic_screw"), "pelvic_screw", object)) %>%
      group_by(object) %>%
      nest() %>% 
      mutate(object_levels_side_df = data) %>%  ### this creates a dataframe with only two columns: 'object' and 'object_levels_side_df'. #The nested dataframe has columns: level, vertebral_number, side, implant_statement, screw_size_type
      select(-data) %>%
      mutate(approach = approach_technique, 
             image_guidance_used = image_guidance, 
             implant_start_point_method = implant_start_point_method_text) %>%
      mutate(tech_statement = pmap(list(..1 = object,
                                        ..2 = object_levels_side_df, 
                                        ..3 = approach, 
                                        ..4 = image_guidance_used,
                                        ..5 = implant_start_point_method),
                                   .f = ~op_note_technique_combine_statement(object = ..1,
                                                                             levels_side_df = ..2, 
                                                                             approach_technique = ..3, 
                                                                             image_guidance = ..4, 
                                                                             implant_start_point_identification = ..5)
                                   )
             ) %>%
      select(object, tech_statement) %>%
      unnest(tech_statement) %>%
      distinct()
    
    df_levels <- df_with_levels_object_nested %>%
      select(level, vertebral_number) %>%
      distinct() %>%
      arrange(vertebral_number)
    
    
    ## EMG USED:
    if(neuromonitoring_emg != "No"){
      emg_statement <- neuromonitoring_emg
    }else{
      emg_statement <- ""
    }
    
    if(procedure_paragraph_intro == "posterior spinal instrumentation"){
      if(str_length(implant_confirmation_method) > 6){
        implant_confirmation_method_text <- implant_confirmation_method
      }else{
        implant_confirmation_method_text <- ""
      }
    }else{
      implant_confirmation_method_text <- ""
    }
    
    ## Completion statement:
    if(procedure_paragraph_intro == "posterior spinal instrumentation"){
      procedure_completion <- glue("{implant_confirmation_method_text} {emg_statement} This partially completed the {procedure_paragraph_intro} of {glue_collapse(unique(x = df_levels$level), sep = ', ', last = ', and ')}.")
    }else if(procedure_paragraph_intro == "incision and drainage"){
      procedure_completion <- glue("This completed the {procedure_paragraph_intro}.")
    }else if(procedure_paragraph_intro == "inferior facetectomies"){
      procedure_completion <- glue(" ")
    }else if(procedure_paragraph_intro == "posterior column osteotomy" & length(unique(x = df_levels$level))>1){
      procedure_completion <- glue("This completed the posterior column osteotomies at {glue_collapse(unique(x = df_levels$level), sep = ', ', last = ', and ')}.")
    }else{
      procedure_completion <- glue("This completed the {procedure_paragraph_intro} at {glue_collapse(unique(x = df_levels$level), sep = ', ', last = ', and ')}.")
      
      }

    ##CREATE THE STATEMENT
    if(procedure_paragraph_intro == "pelvic instrumentation"){
      statement <- glue("I then proceeded with instrumentation of the pelvis. {glue_collapse(df_with_statement$tech_statement, sep = ' ')} This completed the instrumentation of the pelvis.")
    }else if(procedure_paragraph_intro == "inferior facetectomies"){
      statement <- glue("I first performed {procedure_paragraph_intro} at {glue_collapse(unique(x = df_levels$level), sep = ', ', last = ', and ')}. {glue_collapse(df_with_statement$tech_statement, sep = ' ')}")
    }else if(procedure_paragraph_intro == "posterior column osteotomy" & length(unique(x = df_levels$level))>1){
      statement <- glue("I then proceeded with posterior column osteotomies at {glue_collapse(unique(x = df_levels$level), sep = ', ', last = ', and ')}. {glue_collapse(df_with_statement$tech_statement, sep = ' ')} {procedure_completion}")
    }else{
      statement <- glue("I then proceeded with the {procedure_paragraph_intro} at {glue_collapse(unique(x = df_levels$level), sep = ', ', last = ', and ')}. {glue_collapse(df_with_statement$tech_statement, sep = ' ')} {procedure_completion}")
    }
    
  }
  
  if(paragraphs_combined_or_distinct == "distinct"){
    df_with_statement <- df_with_levels_object_nested %>%
      replace_na(list(implant_statement = " ", screw_size_type = " ")) %>%
      mutate(tech_statement_detail = pmap(.l = list(..1 = object, 
                                                    ..2 = level, 
                                                    ..3 = side, 
                                                    ..4 = implant_statement),
                                          .f = ~op_note_distinct_technique_statement(object = ..1, level = ..2, side = ..3, interbody_statement = ..4))) %>% 
      select(object, level, object, tech_statement_detail) %>%
      unnest(tech_statement_detail) %>%
      mutate(procedure_category = map(.x = object, .f = ~ op_note_procedure_performed_summary_classifier_function(.x))) %>%
      unnest(procedure_category) %>%
      mutate(procedure_category = str_to_lower(procedure_category)) %>%
      
      mutate(tech_statement = paste(tech_statement_detail, glue("This completed the {procedure_category} at the {level} {if_else(str_detect(level, '-'), 'interspace', 'level')}.")))
    
    statement <- glue_collapse(df_with_statement$tech_statement, sep = "\n\n")
  }
  
  return(statement)
  
}

#############-----------------------               End              ----------------------###############


#############-----------------------   Building Paragraphs that are combined (e.g. pedicle screws, decompressions) ----------------------###############

op_note_technique_combine_statement <- function(object, 
                                                levels_side_df,
                                                approach_technique = "Open",
                                                image_guidance = "NA", 
                                                implant_start_point_identification = "NA"){
  
  if(ncol(levels_side_df)==1){
    levels_side_df <- levels_side_df%>%
      unnest(data)
  }
  
  ### PELVIC SCREWS:
  if(str_detect(object, "pelvic_screw")){
    
    pelvic_screws_wide <- levels_side_df %>%
      select(level, side) %>%
      mutate(count = row_number()) %>%
      pivot_wider(names_from = side, values_from = count) %>%
      unnest(everything())
    
    pelvic_screws_statement_list <- list()
    
    if(sum(str_count(string = pelvic_screws_wide$level, "Iliac")) == 1){
      pelvic_screws_statement_list$iliac_technique <- glue("For iliac screw insertion, the posterior iliac crest was identified and the entry point was countersunk using a rongeur. A probe was used to bore the path of the screw between the two cortices, aimed toward the anterior inferior iliac spine. Once the path was created, the walls of the path were palpated for breaches and the length of the path measured.")
    }
    if(sum(str_count(string = pelvic_screws_wide$level, "Iliac")) == 2){
      pelvic_screws_statement_list$iliac_technique <- glue("For iliac screw insertion, the posterior iliac crest was identified and the entry point was countersunk using a rongeur. A probe was used to bore the path of the screw between the two cortices, aimed toward the anterior inferior iliac spine. Once the path was created, the walls of the path were palpated for breaches and the length of the path measured. To fit two iliac screws, the start point was moved slightly proximal and distal to a traditional start point.")
    }
    
    if(sum(str_count(string = levels_side_df$level, "Iliac")) > 0){
      iliac_screws_df <- levels_side_df %>%
        filter(level == "Iliac") %>%
        mutate(statement = glue("a {str_to_lower(screw_size_type)} iliac screw was placed on the {side}")) 
      
      pelvic_screws_statement_list$iliac_sizes_statement <- glue("Using this technique, {glue_collapse(x = iliac_screws_df$statement, sep = ', ', last = ' and ')}.")
    }
    
    if(sum(str_count(string = pelvic_screws_wide$level, "S2AI")) == 1){
      pelvic_screws_statement_list$s2ai_technique <- glue("For S2AI screw insertion, the starting point was identified at the inferior and lateral border of the S1 foramen. A probe was used to bore the path of the screw between the two cortices, across the sacroiliac joint, and aimed toward the anterior inferior iliac spine. Once the path was created, the walls of the path were palpated for breaches and the length of the path measured.")
    }
    if(sum(str_count(string = pelvic_screws_wide$level, "S2AI")) == 2){
      pelvic_screws_statement_list$s2ai_technique <- glue("For S2AI screw insertion, the starting point was identified at the inferior and lateral border of the S1 foramen. A probe was used to bore the path of the screw between the two cortices, across the sacroiliac joint, and aimed toward the anterior inferior iliac spine. Once the path was created, the walls of the path were palpated for breaches and the length of the path measured. To fit two S2AI screws, the start point was moved slightly proximal and distal to a traditional start point.")
    }
    
    if(sum(str_count(string = levels_side_df$level, "S2AI")) > 0){
      s2ai_screws_df <- levels_side_df %>%
        filter(level == "S2AI") %>%
        mutate(statement = glue("a {str_to_lower(screw_size_type)} S2AI screw was placed on the {side}")) 
      
      pelvic_screws_statement_list$s2ai_sizes_statement <- glue("Using this technique, {glue_collapse(x = s2ai_screws_df$statement, sep = ', ', last = ' and ')}.")
    }
    
    pelvic_screw_statement <- glue_collapse(pelvic_screws_statement_list, sep = " ")
    
  }else{
    pelvic_screw_statement <- glue(" ")
    
  }
  

  
  if(str_detect(string = object, pattern = "screw")){
    screw_statements_wide_df <- levels_side_df %>%
      mutate(unilateral_screw_statement = str_to_lower(glue("a {screw_size_type} screw was inserted on the {side}"))) %>%
      select(level, side, unilateral_screw_statement) %>%
      pivot_wider(names_from = side, values_from = unilateral_screw_statement) %>%
      unnest(everything()) 
    
    screw_statements_df <- tibble(level = character(), left = character(), right = character(), unilateral_screw_statement = character()) %>%
      union_all(screw_statements_wide_df) %>%
      mutate(left = if_else(is.na(left), "xx", left)) %>%
      mutate(right = if_else(is.na(right), "xx", right)) %>%
      # replace_na(replace = list(left = "xx", right = "xx")) %>%
      mutate(screw_statement = case_when(
        left != "xx" & right != "xx" ~ glue("At {level}, {left} and {right}."), 
        left == "xx" & right != "xx" ~ glue("At {level}, {right}."), 
        left != "xx" & right == "xx" ~ glue("At {level}, {left}."), 
        left == "xx" & right == "xx" ~ glue(""))) %>%
      mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
      arrange(vertebral_number) 
  }else{
    screw_statements_df <- tibble(screw_statement = c(" "))
  }
  
  
  if(object == 'diskectomy'){
    nerve_roots_df <- levels_side_df %>%
      separate(col = level, into = c("exiting_root", "traversing_root")) %>%
      mutate(exiting_root = paste(side, exiting_root), 
             traversing_root = paste(side, traversing_root))
    
    nerve_roots_list <- list() 
    if(nrow(nerve_roots_df) >1){
      nerve_roots_list$traversing_roots <- paste(glue_collapse(nerve_roots_df$traversing_root, sep = ", ", last = " and "), "nerve roots")
    }else{
      nerve_roots_list$traversing_roots <- paste(glue_collapse(nerve_roots_df$traversing_root, sep = ", ", last = " and "), "nerve root")
    }
    if(nrow(nerve_roots_df) >1){
      nerve_roots_list$exiting_roots <- paste(glue_collapse(nerve_roots_df$exiting_root, sep = ", ", last = " and "), "nerve roots")
    }else{
      nerve_roots_list$exiting_roots <- paste(glue_collapse(nerve_roots_df$exiting_root, sep = ", ", last = " and "), "nerve root")
    }
    
  }else{
    nerve_roots_list <- list() 
    nerve_roots_list$traversing_roots <- " " 
    nerve_roots_list$exiting_roots <- " "
  }
  
  exiting_roots_df <- levels_side_df %>%
    mutate(exiting_root = map(.x = level, .f = ~jh_get_exiting_nerve_root_function(.x))) %>%
    unnest(exiting_root) %>%
    filter(!is.na(exiting_root)) %>%
    select(level, exiting_root, side) %>%
    group_by(exiting_root) %>%
    add_tally(name = "number_per_level") %>%
    mutate(exiting_roots_statement = as.character(if_else(number_per_level == 1, glue("the {side} {exiting_root} exiting nerve root"), glue("the left and right {exiting_root} exiting nerve roots")))) %>%
    ungroup() %>%
    select(-side) %>%
    distinct() %>%
    mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
    arrange(vertebral_number)
  
  levels_side_df <- levels_side_df %>%
    group_by(level) %>%
    add_tally(name = "number_per_level") %>%
    mutate(level_side = as.character(if_else(number_per_level == 1, glue("on the {side} at {level}"), glue("on the left and on the right at {level}")))) %>%
    ungroup() %>%
    select(-side) %>%
    distinct() %>%
    mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
    arrange(vertebral_number)   
  

  
  start_point_identification_text <- case_when(
    str_detect(implant_start_point_identification, "NA") ~ "",
    str_detect(implant_start_point_identification, "anatomic landmarks") ~ "",
    str_detect(implant_start_point_identification, "fluoroscopy and pedicle markers") ~ "Pedicle markers were then placed and intraoperative fluoroscopy was used to confirm screw start points and make any necessary adjustments. ",
    str_detect(implant_start_point_identification, "fluoroscopy was used to identify") ~ "Intraoperative fluoroscopy was used to confirm screw start points and trajectory. ",
    str_detect(implant_start_point_identification, "navigation") ~ "Intraoperative Navigation was used for placing screws. ",
  )
  
  # screw technique:
  screw_technique_statement <- case_when(
    image_guidance == "NA" ~ paste(glue("For pedicle screws, the transverse process, pars, and superior facet were used as landmarks to identify the appropriate starting point. After identifying the start point, the superficial cortex was opened at each entry point using a burr. {start_point_identification_text} A pedicle probe was then used to navigate down the pedicle, followed by palpating a medial, lateral, superior and inferior pedicle wall, measuring, and tapping if appropriate.")),
    image_guidance == "Fluoroscopy-guided" ~ "For pedicle screws, the TP was palpated and a Jamshidi needle was placed at the junction of the TP and superior facet. An AP and lateral xray was used to identify and confirm an appropriate start point and trajectory. The Jamshidi needle was then advanced down the pedicle and a wire placed through the pedicle and into the vertebral body. With the wire in place, the pedicles were then sequentially tapped to allow for screw placement. Xray was used frequently throughout the procedure to confirm the position.",
    image_guidance == "Navigated" ~ "An intraoperative CT scan was obtained with a reference point attached to the patient. Navigated instruments were then used in conjuction with the navigation system to identify the appropraite starting point and trajectory for each screw. The pedicle was cannulated with navigation assistance and then palpated for a medial, inferior, superior and lateral wall to confirm a safe trajectory for placement of the screw.",
    image_guidance == "Robotic" ~ "Intraoperative 3 dimensional imaging was used to place the pedicle screws with robotic assistance. Once the images were merged with the preoperative plan, the appropriate start point was identified for each pedicle and then pedicle was then cannulated and tapped in preparation for screw placement. "
  )
  
  
  technique_statement <- case_when(
    object == "incision_drainage" ~ glue("The wound was inspected thoroughly and any necrotic appearing tissue was excised. Tissue samples from deep and superficial wound bed were sent for cultures. The wound bed was thoroughly irrigated and then working layer by layer from deep to superficial, the bone, deep muscle, fascia and subcutaneous tissue was meticulously debrided with a currette. Devitalized muscle, fascia and subcutaneous tissue was excised. Once I felt the wound was clean and an adequate debridement had been completed, I again copiously irrigated the wound."), 
    object == "vertebroplasty" ~ glue("For vertebral body cement augmentation, a cannula was inserted into the vertebral body through the pedicle. The cement was then injected under x-ray guidance until an appropriate amount of cement had been injected into the vertebral body. Vertebroplasty was performed at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}."), 
    object == "vertebral_cement_augmentation" ~ glue("For vertebral body cement augmentation, I first created a cavity within the vertebral body. The cement was then injected under x-ray guidance until an appropriate amount of cement had been injected into the vertebral body. Vertebral augmentation was performed at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}."), 
    object == "laminar_downgoing_hook" ~ glue("For downgoing laminar hooks, the ligamentum flavum was removed at the site of the hook insertion. A hook was then inserted securely under the lamina {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "laminar_upgoing_hook" ~ glue("For upgoing laminar hooks, the inferior edge of the cranial lamina was identified and the plane between the lamina and ligamentum was developed and the upgoing hook is then placed within this plane, secured to the lamina aimed cranially {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "tp_hook" ~ glue("For transverse process hooks, the cranial edge of the transverse process was identified. A transverse process finder was used to develop the plane and a transverse process hook was placed securely into position along the superior edge of the transverse process. A transverse process hook was placed {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."),  
    object == "lateral_mass_screw" ~ glue("For lateral mass screw placement, the entry point was identified using the lateral and medial borders of the lateral mass and superior and inferior borders of the facet. The superficial cortex was opened at each entry point using a burr and the screw path drilled incrementally to the far cortex and the dorsal cortex was then tapped. {glue_collapse(x = screw_statements_df$screw_statement, sep = ' ')}"), 
    object == "occipital_screw" ~ glue("An appropriately sized occipital plate was selected and was placed centered in the midline and just caudal to the external occipital protuberance, but cranial to the foramen magnum. The plate was held in place to identify the appropriate start points for the occipital screws. The occipital screws were drilled incrementally until the anterior cortex was penetrated. The length of the path was measured to acheive bicortical fixation. The screw paths were then tapped and the screws placed, securing the plate to the occiput."), 
    object == "pars_screw" ~ glue("For pars screws, the start point was identified just 3-4mm cranial to the inferior facet joint and in the midpoint of the pars from medial to lateral. {start_point_identification_text} Once the start point was identified, the superficial cortex was opened at the entry point using the burr. The screw path was then cannulated, aiming as dorsal as possible while not perforating the dorsal cortex of the pars. The path was then tapped and the length measured and pars screw placed. {glue_collapse(x = screw_statements_df$screw_statement, sep = ' ')}"), # {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "pedicle_screw" ~ glue("{screw_technique_statement} {glue_collapse(x = screw_statements_df$screw_statement, sep = ' ')}"), 
    object == "translaminar_screw" ~ glue("For translaminar screw fixation, the starting point was identified using the spinolaminar junction and in the central plane of the contralateral lamina. A burr hole was made to open the superficial cortex, the path was then cannulated and intralaminar screw placed. {glue_collapse(x = screw_statements_df$screw_statement, sep = ' ')}"), # {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."),
    object == "pedicle_hook" ~ glue("For pedicle hooks, a pedicle finder was carefully placed just under the inferior facet and above the superior facet, until the pedicle was found. Once the pedicle was identified, a pedicle hook was inserted directly toward the inferior pedicle, secured within the residual facet joint {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "pelvic_screw" ~ pelvic_screw_statement, 
    object == "pelvic_screw_1" ~ pelvic_screw_statement,
    object == "pelvic_screw_2" ~ pelvic_screw_statement,
    object == "sublaminar_wire" ~ glue("For sublaminar wiring, the doubled-up sublaminar was was bent into a hook shape and and passed under the inferior lamina from a caudal to cranial direction. Once the wire was safely passed, the tip of the wire was cut and the two strands were directed in opposite directions of the midline to be attached to the rods on the left and right side. Wires were passed under {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}."), 
    object == "tether" ~ glue("For spinous process tethering/wiring, a hole was drilled in the spinous processes over the lamina. A band was then threaded through the hole and appropriately tensioned and secured in place, tethering {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}."), 
    object == 'complete_facetectomy' ~ glue("For a complete facetectomy, the inferior, superior, medial and lateral borders of the inferior and superior facet were identified. Both the superior and inferior facet were completely excised and the underlying exiting nerve root decompressed. I performed a complete facetectomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}. This effectively decompressed {glue_collapse(x = exiting_roots_df$exiting_roots_statement, sep = ', ', last = ' and ')}."),
    object == 'grade_1' ~ glue("The inferior, superior, medial and lateral borders of the inferior facet were identified. The inferior facets {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} were excised and the bone was morselized to be used as morselized autograft."),
    object == 'grade_2' ~ glue("For the posterior column osteotomy, a small rent was made in the interlaminar space to develop the plane between the ligamentum and the dura. A Kerrison rongeur was then used to excise the ligamentum. This was carried out laterally in both directions until the facets were encountered. The superior and inferior facet were both adequately resected along with any necessary lamina to fully release the posterior column. I performed posterior column (Smith-Peterson) osteotomies at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}."),
    object == 'diskectomy' ~ glue("For a discectomy, I used the cranial and caudal laminar edges, pars, and facet joints as landmarks. Using a high-speed burr and Kerrison rongeurs, I first performed a laminotomy, exposing the ligamentum flavum. I then carefully excised the ligamentum flavum, exposing the dura and exiting nerve root. The dural sac was identified and traversing root was protected. The annulus was then incised and the diseased disk material was removed.  I performed a discectomy with partial medial facetectomy and foraminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the {nerve_roots_list$traversing_roots}. Following the decompression, the canal and foramen and lateral recess were palpated to confirm an appropriate decompression had been completed."),
    object == 'laminotomy' ~ glue("For the laminotomy, the cranial and caudal laminar edges, pars, and facet joints were used as landmarks. Once I had determined the laminotomy site, I used a combination of a high-speed burr and Kerrison rongeurs to resect the bone dorsal to the nerve root. I performed a laminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. Following the decompression, the foramen was palpated to confirm an adequate decompression had been completed."),
    object == 'cervical_foraminotomy' ~ glue("For the posterior cervical foraminotomy at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}, the superior and inferior facet and joint line was clearly identified. I used a combination of a high-speed burr to resect roughly 50% of the overlying medial inferior facet, which exposed the superior facet. I then proceeded with the high-speed burr, resecting the superior articular facet out to the lateral border of the pedicles. Copious irrigation was used during the resection. After removing the superior facet, the exiting nerve root was identified. A Kerrison 1 was used to remove any remaining overlying tissue. I performed a posterior cervical foraminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. Following the decompression, the hemostasis was obtained and I was able to palpate the lateral walls of the cranial and caudal pedicles, confirming adequate decompression."),
    object == 'laminectomy' ~ glue("Using the cranial and caudal edges of the lamina and pars as landmarks, I performed a laminectomy at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}. First, using a combination of a high-speed burr and Kerrison rongeurs, I resected the dorsal lamina. I then excised the underlying ligamentum flavum. Following the decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'laminectomy' ~ glue("Using the cranial and caudal edges of the lamina and pars as landmarks, I performed a laminectomy at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} in order to excise the underlying facet cyst. First, using a combination of a high-speed burr and Kerrison rongeurs, I resected the dorsal lamina. I then excised the underlying ligamentum flavum. This allowed exposure to the underlying facet cyst, which was fully excised. Following the decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'laminectomy_for_tumor' ~ glue("Using the cranial and caudal edges of the lamina and pars as landmarks, I performed a laminectomy at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} in order to perform an open biopsy and excise the intraspinal extradural tumor. First, using a combination of a high-speed burr and Kerrison rongeurs, I resected the dorsal lamina. I then excised the underlying ligamentum flavum. At this point, I encountered the extradural lesion which measured roughly {length(levels_side_df$level)*3.5}cm long by ~2-3cm wide. I obtained a biopsy of the tissue and then proceeded to excise any visible tumor in order to adequately create separation between visible tumor and the dura. Following the laminectomy and excision of the lesion, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'sublaminar_decompression' ~ glue("Using a combination of a high-speed burr and Kerrison rongeurs, I performed a partial laminectomy bilaterally at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace. First, I resected the dorsal lamina and then excised the ligamentum flavum, exposing the dura. To fully decompress the exiting and traversing nerve roots within the neural foramen and lateral recess, the medial facet was partially excised and a foraminotomy performed on the left and right at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace. Following the decompression, the canal and foramen were palpated to confirm an adequate decompression had been completed."),
    object == 'revision_diskectomy' ~ glue("For the revision discectomy, I used the cranial and caudal laminar edges, pars, and facet joints as landmarks. I performed a revision and reexploration with discectomy and partial medial facetectomy and foraminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. I carefully dissected off the scar until I was able to safely identify the bony edges and develop a plane between the dura and scar. Using a high-speed burr and Kerrison rongeurs, I performed a foraminotomy and excised the scar and ligamentum. The dural sac was identified and traversing root was protected. The annulus and disk space were identified and the diseased disk material was excised. Following the revision decompression, the canal and foramen and lateral recess were palpated to confirm an adequate decompression had been completed."),
    object == 'revision_laminotomy' ~ glue("For the revision laminotomy, the cranial and caudal laminar edges, pars, and facet joints were used as landmarks. I carefully exposed the dorsal elements until I had identified the edges of the laminar defect. Once I determined the laminotomy site, I used a combination of a high-speed burr and Kerrison rongeurs to resect the bone dorsal to the nerve root. I performed a revision laminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. Following the decompression, the foramen was palpated to confirm an adequate decompression had been completed."),
    object == 'revision_laminectomy' ~ glue("Using the cranial and caudal edges of the lamina and pars as landmarks, I performed a reexploration and revision laminectomy at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}. Using a combination of a high-speed burr, currettes and Kerrison rongeurs, I carefully dissected off the scar until I was able to safely identify the bony edges and develop a plane between the dura and scar. I removed any dorsal lamina, ligamentum flavum and scar thought to still be causing compression. The central canal was palpated to confirm full decompression of the thecal sac. Following the revision decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'revision_sublaminar_decompression' ~ glue("Using a combination of a high-speed burr and Kerrison rongeurs, I performed a reexploration and revision decompression bilaterally at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace. First, I carefully dissected off the scar until I was able to safely identify the bony edges and develop a plane between the dura and scar. I proceeded with partial laminectomies and excised the scar and underlying ligamentum flavum. The medial facet was partially excised and a foraminotomy performed on the left and right at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace, to fully decompress the exiting and traversing nerve roots within the neural foramen and lateral recess. Following the revision decompression, the canal and foramen were palpated to confirm an adequate decompression had been completed."),
    object == 'revision_complete_facetectomy' ~ glue("The cranial and caudal laminar edges, pars, and facet joints were used as landmarks to perform the reexploration and revision laminotomy and complete facetectomy. The bony edges were carefully identified and overlying scar excised. Once I was satisfied with the exposure and landmarks, a combination of a high-speed burr and rongeurs were used and the superior and inferior facet were completely excised and the underlying exiting nerve root decompressed. I performed a complete facetectomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}. This effectively decompressed {glue_collapse(x = exiting_roots_df$exiting_roots_statement, sep = ', ', last = ' and ')}."),
    object == 'costotransversectomy' ~ glue("For the costotransversectomy, the rib was identified and a subperiosteal dissection was carried out laterally 6-7cm. Once the medial rib was skeletonized and a safe, subperiorsteal plane was confirmed, 5-6cm of the medial rib was transected. This process was completed {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}, allowing a safe costovertebral approach to the anterior vertebral body."),
    object == 'lateral_extracavitary_approach' ~ glue("For the modified lateral extracavitary approach at {glue_collapse(levels_side_df$level, sep = ', ', last = ' and ')}, the pedicle of {glue_collapse(levels_side_df$level, sep = ', ', last = ' and ')} was identified and using a combination of a high-speed burr, Kerrison rongeurs and osteotome, the inferior and superior facets were fully resected, allowing access to the interspace. The lateral border of the disk space was identified and the dissection was carried out anteriorly around the entire anterior portion of the vertebral body. This was carried out {glue_collapse(levels_side_df$level_side, sep = ', ', last = ' and ')} and allowed safe access to the anterior longitudinal ligament and the most anterior portion of the vertbral body."),
    object == 'laminoplasty' ~ glue("For laminoplasty, an opening and hinge trough was created with a high-speed burr angled perpendicular to the lamina at the lateral mass-lamina junction. This was performed at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}. After all troughs were completed, greenstick fractures were created to open the lamina at the laminoplasty levels, the ligamentum flavum at the proximal and distal levels was excised, and laminoplasty plates were sequentially secured into place. The spinous processes were trimmed and the canal was palpated to confirm an adequate decompression had been completed.")
  )
  
  technique_statement <- str_replace_all(string = technique_statement, pattern = "a na ", replacement = "a ")
  
  technique_statement
}

#############-----------------------               End              ----------------------###############


#############-----------------------   Building Paragraphs: Distinct Procedures as Paragraphs ----------------------###############

op_note_distinct_technique_statement <- function(object, level, side, interbody_statement = NULL){
  
  cranial_level <- jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level
  caudal_level <- jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level
  
  cranial_interspace <- jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_interspace
  caudal_interspace <- jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_interspace
  
  thoracic_nerve_ligation_statement <- {if_else(str_detect(level, 'T'), glue(" Clamps were then placed on the exiting {cranial_level} and {level} nerve roots medial to the dorsal root ganglion and the spinal cord was monitored for any neuromonitoring changes. Once we were satisfied with the neuromonitoring data, the nerve roots were ligated with sutures. "), glue(" "))} 
  
  interbody_statement_expandable <- if_else(str_detect(str_to_lower(interbody_statement), "expandable"), "The implant was then expanded until a solid bony fit was sensed.", " ")
  
  technique_statement <- case_when(
    object == 'transpedicular_approach' ~ glue("I then proceeded with a transpedicular approach for decompression from the {side} at {level}. Using a combination of a high-speed burr and Kerrison rongeurs, the posterior elements were resected and the {side} {level} pedicle was skeletonized and resected, to allow full decompression of the {if_else(str_starts(level, 'L'), 'cauda equina and nerve roots', 'spinal cord')} at the {level} level. Following the decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'costovertebral_approach' ~ glue("I then proceeded with a costovertebral approach for decompression from the {side} at {level}. The {side} {level} rib was identified, skeletonized, and 5-6cm of the rib was resected to allow access to the dorsal vertebral body. Using a combination of a high-speed burr and Kerrison rongeurs, the posterior elements were resected and the {side} {level} pedicle was skeletonized and resected, to allow full decompression of the spinal cord at the {level} level. Following the decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'lateral_extraforaminal_approach' ~ glue("I then proceeded with a lateral extraforaminal approach from the {side} at {level}. The pedicle of {level} was identified. The {side} {level} inferior and {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} superior facets were identified and the {level} pars was used as reference points. Using a combination of a high-speed burr and rongeurs, the lateral {level} pars was partially resected, and the {level} nerve root was identified and protected. This allowed access to the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_interspace} interspace. The lateral border of the disk space was identified and the dissection was carried out laterally and medially until adequate exposure was obtained. The retropulsed disc fragment was identified causing the neural compression. I then proceed with excision of the disc until an adeqate decompression was completed.."),
    object == 'lateral_extracavitary_approach' ~ glue("I then proceeded with a modified lateral extracavitary approach from the {side} at {level}. The pedicle of {level} was identified. Using a combination of a high-speed burr and Kerrison rongeurs and osteotome, the {side} {level} inferior and {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} superior facets were fully resected, allowing access to the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_interspace} interspace. The lateral border of the disk space was identified and the dissection was carried out laterally anteriorly around the entire anterior portion of the vertebral body. This allowed safe access to the ALL and most anterior portion of the vertbral body. The disc was excised and superior and inferior endplates of the interspace were prepared in order to acheive arthrodesis of the {level} interspace."),
    object == 'revision_transpedicular_approach' ~ glue("I then proceeded with a transpedicular approach for a reexploration and revision decompression from the {side} at {level}. Using a combination of a high-speed burr, currettes, and Kerrison rongeurs, the scar and posterior elements were carefully resected until I found the plane between scar and dura. The {side} {level} pedicle was skeletonized and resected, to allow full decompression of the {if_else(str_starts(level, 'L'), 'cauda equina and nerve roots', 'spinal cord')} at the {level} level. Following the revision decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'revision_costovertebral_approach' ~ glue("I then proceeded with a costovertebral approach for a reexploration and revision decompression from the {side} at {level}. The {side} {level} rib was identified, skeletonized, and 5-6cm of the rib was resected to allow access to the dorsal vertebral body. Using a combination of a high-speed burr, currettes, and Kerrison rongeurs, the scar and posterior elements were carefully resected until I found the plane between scar and dura. The {side} {level} pedicle was skeletonized and resected, to allow full decompression of the spinal cord at the {level} level. Following the revision decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'corpectomy_extracavitary_tumor' ~ glue("I then proceeded with a partial vertebral corpectomy using a modified lateral extracavitary approach from the {side} at {level}. The pedicle of {level} was identified. Using a combination of a high-speed burr and Kerrison rongeurs and osteotome, the {side} {level} inferior and {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} superior facets were resected and the {level} superior and {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} inferior facets were resected as needed to skeletonize the {level} pedicle, allowing access to the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_interspace} interspace and {level} vertebral body. The lateral border of the disk space was identified and the dissection was carried out laterally and slightly anteriorly over the vertebral body. Once the posterior elements were resected and I had gained adequate exposure to the vertebral body via the extracavitary approach and the {level} nerve root could be visualized and protected, I used a combination of a burr, ronguers, osteotomes and currettes to resect any visible tumor and vertebral bone, creating a cavity in the vertebral body. Once an adequate cavity had been created, the remaining shell of the posterior vertebral body along with the PLL was impacted into the cavity, completing the decompression of the {if_else(str_detect(level, 'L'),'cauda equina and nerve roots', 'spinal cord')} and separation of visible tumor by at least 10mm. "),
    object == 'grade_3' ~ glue("I then proceeded with the pedicle subtraction osteotomy at the level of {level}. A central laminectomy was performed and confirmed at the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} level in order to prevent compression after osteotomy closure. First, the necessary posterior elements were resected. Starting on the contralateral side, the inferior facet of {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} was resected up to the pedicle and then the {level} superior facet was completely resected, allowing the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} nerve root to be visualized and tracked laterally. Then a cut was made through the pars just inferior to the {level} pedicle and the {level} inferior facet was completely resected along with the superior facet of {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level}. The {level} nerve root was visualized and carefully tracked laterally. Once the {level} pedicle was completely skeletonized, and the cranial and caudal nerve roots identified and a full decompression was confirmed from the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} pedicle to the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} pedicle, the process was repeated on the other side. Once the necessary posterior elements were resected, a subperiosteal plane was then developed around the anterior portion of the {level} body and retractors were held in place to protect the retroperitoneal  structures. Next, the pedicle was resected down to the dorsal vertebral body, leaving a lip medially to protect the neural structures. The dura, cranial and caudal nerve roots were protected and a burr was used to develop a defect in the vertebral body at the level of the pedicle. The lateral portion of the pedicle wall was taken down, allowing easier access to the vertebral body and the vertebral body was hollowed out, taking care to leave adequate anterior body and cortex. The remaining pedicle walls were then resected with a box osteotome. We then placed a temporary rod and repeated the same process on the contralateral pedicle. Once an adequate defect had been created in the vertebral body, the dorsal wall of the vertebral body was impacted. The nerve roots were checked again and the underside of the thecal sac was palpated to confirm no bony impingement. Once we were satisfied that the thecal sac was fully decompressed circumferentially and that the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} and {level} nerve roots were completely free, the rods were placed into position and the osteotomy was closed. The set screws were final tightened to lock the osteotomy in position. Intraoperative xray was performed to confirm appropriate correction through the osteotomy site"),    object == 'grade_4' ~ glue("I then proceeded with an extended three column osteotomy (partial corpectomy) at the level of {level}. The posterior elements of {level} were completely removed. A complete central laminectomy was performed and confirmed at the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} level in order to prevent compression after osteotomy closure. A temporary rod was placed and starting on the contralateral side, a complete superior facetectomy was performed and any dorsal bone and scar was removed, and the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} nerve root was visualized and tracked laterally. Then a cut was made through the pars just inferior to the pedicle, and the superior facet of the inferior vertebra was resected, allowing the {level} nerve root to be visualized and tracked laterally. Once the {level} pedicle was completely skeletonized, and the cranial and caudal nerve roots identified and a full decompression was confirmed from the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} pedicle to the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} pedicle, a subperiosteal plane was developed around the anterior portion of the {level} body and retractors were held in place to protect the retroperitoneal  structures. Next, the pedicle was resected down to the dorsal vertebral body, leaving a lip medially to protect the neural structures. The dura, cranial and caudal nerve roots were protected and a burr was used to develop a defect in the vertebral body. The lateral portion of the pedicle wall was taken down, allowing easier access to the vertebral body and the vertebral body was hollowed out, extending up into the cranial disk space, taking care not to perforate the anterior cortex. The remaining pedicle walls were then resected with a box osteotome. We then placed a temporary rod and repeated the same process on the contralateral pedicle. Once roughly 50% of the vertebral body had been resected, the dorsal wall of the vertebral body was impacted. The nerve roots were checked again and the underside of the thecal sac was palpated to confirm no bony impingement. Once we were satisfied that the thecal sac was fully decompressed circumferentially and that the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} and {level} nerve roots were completely free, the osteotomy was closed, and the other rod was placed into position and set screws final tightened to lock the osteotomy in position. Intraoperative xray was performed to confirm appropriate alignment."),
    object == 'grade_4' ~ glue("I then proceeded with an extended three column osteotomy (partial corpectomy) at the level of {level}. A central laminectomy was performed and confirmed at the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} level in order to prevent compression after osteotomy closure. First, the necessary posterior elements were resected. Starting on the contralateral side, the inferior facet of {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} was resected up to the pedicle and then the {level} superior facet was completely resected, allowing the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} nerve root to be visualized and tracked laterally. Then a cut was made through the pars just inferior to the {level} pedicle and the {level} inferior facet was completely resected along with the superior facet of {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level}. The {level} nerve root was visualized and carefulyl tracked laterally. Once the {level} pedicle was completely skeletonized, and the cranial and caudal nerve roots identified and a full decompression was confirmed from the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} pedicle to the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} pedicle, the process was repeated on the other side. Once the necessary posterior elements were resected, a subperiosteal plane was then developed around the anterior portion of the {level} body and retractors were held in place to protect the retroperitoneal  structures. Next, the pedicle was resected down to the dorsal vertebral body, leaving a lip medially to protect the neural structures. The dura, cranial and caudal nerve roots were protected and a burr was used to develop a defect in the vertebral body at the level of the pedicle. The lateral portion of the pedicle wall was taken down, allowing easier access to the vertebral body and the vertebral body was hollowed out, extending up into the cranial disk space, taking care not to perforate the anterior cortex. The remaining pedicle walls were then resected with a box osteotome. We then placed a temporary rod and repeated the same process on the contralateral pedicle. Once roughly 50% of the vertebral body had been resected, the dorsal wall of the vertebral body was impacted. The nerve roots were checked again and the underside of the thecal sac was palpated to confirm no bony impingement. Once we were satisfied that the thecal sac was fully decompressed circumferentially and that the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} and {level} nerve roots were completely free, the osteotomy was closed, and the other rod was placed into position and set screws final tightened to lock the osteotomy in position. Intraoperative xray was performed to confirm appropriate alignment."),
    object == 'grade_5' ~ glue("I then proceeded with a vertebral column resection at the level of {level}. A central decompression was performed and confirmed extending from the {cranial_level} pedicle to the {caudal_level} pedicle in order to prevent neural compression after osteotomy closure. A complete facetectomy was performed superiorly and any dorsal bone was removed, allowing the {cranial_level} nerve root to be visualized and tracked laterally. Similarly, a complete facetectomy was performed inferiorly, allowing the {level} nerve root to be visualized and tracked laterally. Next, the transverse process was resected and the {level} pedicles were completely skeletonized. The cranial and caudal nerve roots were then free and a full decompression was confirmed from the {cranial_level} pedicle to the {caudal_level} pedicle.{thoracic_nerve_ligation_statement} In preparation for the vertebral column resection, a unilateral stabilizing rod was placed on the convex side and secured with set screws multiple levels above and below the {level} level. Starting on the concavity, a combination of blunt and electrocautery dissection was used to develop a subperiosteal plane around the entire anterior portion of the {level} body and retractors were held in place to protect the anterior and neural structures. A lateral pedicle body window was developed for accessing the cancellous bone of the vertebral body and using currettes and rongeurs, the accessible cancellous vertebral body was removed, extending up toward the cranial and caudal disk space. The remaining pedicle of {level} was resected down to the dorsal vertebral body, allowing the spinal cord to drift medially. The same process was then carried out on the convexity to access the cancellous vertebral body and remove the convex pedicle. The entire vertebral body was removed except for a thin anterior section, preserving the anterior longitudinal ligament. Discectomies were performed at the {cranial_interspace} and {caudal_interspace}, taking care to preserve the endplates. Once the entire body had been removed, a plane was developed between the ventral dural sac and the posterior body wall to be sure the dural sac was completely free from the body wall. An impactor was then used to impact the posterior body wall, completing the vertebral column resection. The dural sac was again inspected to confirm no potential sites of impingement. We then turned toward closure of the resection site. Starting with compression on the convexity, the site was closed through alternating compression on the convexity and slight distraction on the concave side. Once I was satisfied with the closure, rods were secured into place bilaterally with set screws."),
    
    object == 'no_implant_interbody_fusion' ~ glue("I then proceeded with the interbody fusion from the {side} side at the level of {level}. The inferior and superior facets of {str_replace_all(level, '-', ' and ')}, respectively, were resected and the exiting and traversing nerve roots were appropriately protected. The annulus was incised, allowing access to the disk space. Using a combination of currettes, shavers, and pituitary rongeurs, the disk was excised and the endplates were prepped for an interbody fusion."),
    object == 'plif' ~ glue("I then proceeded with the posterior lumbar interbody fusion (PLIF) procedure and placement of interbody implant at the level of {level}. The inferior and superior facets of {str_replace_all(level, '-', ' and ')}, respectively, were partially resected and the exiting and traversing nerve roots were appropriately protected. The dura was retracted in order to access the disc space. The annulus was incised, allowing access to the disk space. The disk was excised and endplates were prepped using a combination of currettes, shavers, and pituitary rongeurs. Once the endplates were adequately prepped, trials were used to determine the appropriate size of the implant. {interbody_statement} Once I was satisfied with the size of the trial, the interbody implant was placed into the interbody space along with allograft bone. {interbody_statement_expandable} The final position of the implant was confirmed using intraoperative xray."),
    object == "tlif" ~ glue("I then proceeded with the transforaminal interbody fusion (TLIF) procedure and placement of interbody implant from the {side} side at the level of {level}. The inferior and superior facets of {str_replace_all(level, '-', ' and ')}, respectively, were resected and the exiting and traversing nerve roots were appropriately protected. The annulus was incised, allowing access to the disk space. The disk was excised and endplates were prepped using a combination of currettes, shavers, and pituitary rongeurs. Once the endplates were adequately prepped, trials were used to determine the appropriate size of the implant. {interbody_statement} Once I was satisfied with the size of the trial, the interbody implant was placed into the interbody space along with allograft bone. The final position of the implant was confirmed using intraoperative xray."),
    object == "intervertebral_cage" ~ glue("I then proceed with placement of the intervertebral cage. After ensuring the neural structures were completely free and the bony surfaces cranially and caudally were appropriately prepared, the intervertebral distance was measured. {interbody_statement} The implant was then inserted into the defect, abutting bone superiorly and inferiorly. {interbody_statement_expandable} Intraoperative xray was used to confirm appropriate size, fit, and alignment of the intervertebral implant spanning {level}.")
    
  )
  
  technique_statement
}

#############-----------------------               End              ----------------------###############





#############-----------------------   Generate STATEMENT FOR ----------------------###############

op_note_procedures_present_listed_function <- function(objects_added_df,
                                                       revision_decompression_vector = c(),
                                                       fusion_levels_vector = c(),
                                                       additional_procedures_performed_vector = NULL){
  
  additional_procedures_performed_vector <- keep(.x = additional_procedures_performed_vector, 
                                                 .p = ~ str_detect(.x, "Biopsy") | 
                                                   str_detect(.x, "Repair of dura")  | 
                                                   str_detect(.x, "Dural")  | 
                                                   str_detect(.x, "Removal of spinal") | 
                                                   str_detect(.x, "Exploration of") | 
                                                   str_detect(.x, "Open treatment"))
  
  if(length(revision_decompression_vector) > 0){
    if(nrow(objects_added_df)>0){
      summary_nested_df <- objects_added_df %>%
        mutate(revision_levels_vector = list(revision_decompression_vector)) %>%
        select(level, vertebral_number, approach, category, object, side, revision_levels_vector) %>%
        mutate(revision_level = map2(.x = level, .y = revision_levels_vector, .f = ~ str_detect(string = .x, pattern = .y))) %>%
        select(-revision_levels_vector) %>%
        mutate(revision_level = map(.x = revision_level, .f = ~ any(.x))) %>%
        unnest(revision_level) %>%
        mutate(object = if_else(category == "decompression" & revision_level == TRUE, paste0("revision_", object), object)) %>%
        select(level, object, vertebral_number) %>%
        mutate(procedure_class = map(.x = object, .f = ~ op_note_procedure_performed_summary_classifier_function(.x))) %>%
        unnest(procedure_class) %>%
        mutate(procedures_per_line = map(.x = procedure_class, .f = ~op_note_number_of_paragraphs_for_procedure_category(.x))) %>%
        unnest(procedures_per_line) 
    }else{
      summary_nested_df <- objects_added_df %>%
        select(level, object, vertebral_number) %>%
        mutate(procedure_class = map(.x = object, .f = ~ op_note_procedure_performed_summary_classifier_function(.x))) %>%
        unnest(procedure_class) %>%
        mutate(procedures_per_line = map(.x = procedure_class, .f = ~op_note_number_of_paragraphs_for_procedure_category(.x))) %>%
        unnest(procedures_per_line)
    }
  }else{
    summary_nested_df <- objects_added_df %>%
      select(level, object, vertebral_number) %>%
      mutate(procedure_class = map(.x = object, .f = ~ op_note_procedure_performed_summary_classifier_function(.x))) %>%
      unnest(procedure_class) %>%
      mutate(procedures_per_line = map(.x = procedure_class, .f = ~op_note_number_of_paragraphs_for_procedure_category(.x))) %>%
      unnest(procedures_per_line)
  }
  
  
  summary_single_statements <- summary_nested_df %>%
    filter(procedures_per_line == "distinct") %>%
    mutate(procedure_performed_statement = glue("{str_to_lower(procedure_class)} at {level}"))%>%
    select(procedure_class, procedure_performed_statement)
  
  summary_multiple_nested <- summary_nested_df %>%
    filter(procedures_per_line == "combine") %>%
    group_by(procedure_class) %>%
    nest() %>%
    ungroup() %>%
    mutate(count = row_number()) %>%
    mutate(levels = map(.x = data, .f = ~ extract_levels_function(input_df = .x))) %>%
    select(-data) %>%
    unnest(levels) %>%
    mutate(procedure_performed_statement = glue("{str_to_lower(procedure_class)} at {levels}")) %>%
    mutate(procedure_performed_statement = if_else(str_detect(procedure_class, "pelvic instr"), 
                                                   paste("instrumentation of the pelvis with", levels, "fixation"), 
                                                   as.character(procedure_performed_statement))) %>%
    select(procedure_class, procedure_performed_statement)
  
  added_procedures_df <- tibble(procedure_performed_statement = str_to_lower(additional_procedures_performed_vector)) 
  
  procedures_listed_df <- summary_nested_df %>%
    select(procedure_class) %>%
    distinct() %>%
    left_join(summary_single_statements %>%
                union_all(summary_multiple_nested)) %>%
    select(procedure_performed_statement) %>%
    add_row(procedure_performed_statement = if_else(length(fusion_levels_vector) == 0, "xx", paste("posterior spinal fusion at", glue_collapse(fusion_levels_vector, sep = ", ", last = " and ")))) %>%
    union_all(added_procedures_df) %>%
    filter(procedure_performed_statement !="xx")
  
  as.character(glue_collapse(procedures_listed_df$procedure_performed_statement, sep = ", ", last = ", and the "))
}
#############-----------------------               End              ----------------------###############




revision_implants_paragraph_function <- function(revision_implants_details_df){
  revision_implants_statements_list <- list()
  
  left_revision_implants_statements_df <- revision_implants_details_df %>%  
    filter(side == "left")
  
  if(nrow(left_revision_implants_statements_df) > 0){
    
    left_implants_prior_rod_connected_yes_vector <- (left_revision_implants_statements_df %>% filter(prior_rod_connected == "yes"))$level
    left_implants_prior_rod_connected_no_vector <- (left_revision_implants_statements_df %>% filter(prior_rod_connected == "no"))$level
    
    left_implants_prior_implants_retained_vector <- (left_revision_implants_statements_df %>% filter(remove_retain == "retain"))$level
    left_implants_prior_implants_removed_vector <- (left_revision_implants_statements_df %>% filter(remove_retain == "remove"))$level
    
    
    if(length(left_implants_prior_implants_removed_vector) > 0){
      revision_implants_statements_list$left_remove_implants_start <- paste("I then proceeded with removal of prior spinal instrumentation on the left.")
      
      
      if(length(left_implants_prior_rod_connected_no_vector) > 0 & length(left_implants_prior_rod_connected_yes_vector) > 0){
        if(jh_get_vertebral_number_function(level_to_get_number = tail(left_implants_prior_rod_connected_no_vector, 1)) < jh_get_vertebral_number_function(level_to_get_number = head(left_implants_prior_rod_connected_yes_vector, 1))){  ## this means that new rod is connected proximal to the old construct
          
          revision_implants_statements_list$left_cut_rod <- glue("Once the instrumentation on the left was adequately exposed, the rod was cut between {tail(left_implants_prior_rod_connected_no_vector, 1)} and {head(left_implants_prior_rod_connected_yes_vector, 1)}.")
          
          revision_implants_statements_list$left_remove_implants <- glue("The {glue_collapse(left_implants_prior_implants_removed_vector, sep = ', ', last = ' and ')} {if_else(length(left_implants_prior_implants_removed_vector)>1, 'implants were', 'implant was')} then removed without major difficulty.") 
          
          revision_implants_statements_list$left_retain_implants <- glue("The {glue_collapse(left_implants_prior_implants_retained_vector, sep = ', ', last = ' and ')} {if_else(length(left_implants_prior_implants_retained_vector)>1, 'implants were', 'implant was')} left in place on the left.")
          
        }else{ ### the new rod is connected distal to the old construct
          revision_implants_statements_list$left_cut_rod <- glue("Once the instrumentation on the left was adequately exposed, the rod was cut between {tail(left_implants_prior_rod_connected_yes_vector, 1)} and {head(left_implants_prior_rod_connected_no_vector, 1)}.")
          
          revision_implants_statements_list$left_remove_implants <- glue("The {glue_collapse(left_implants_prior_implants_removed_vector, sep = ', ', last = ' and ')} {if_else(length(left_implants_prior_implants_removed_vector)>1, 'implants were', 'implant was')} then removed without major difficulty.") 
          revision_implants_statements_list$left_retain_implants <- glue("The {glue_collapse(left_implants_prior_implants_retained_vector, sep = ', ', last = ' and ')} {if_else(length(left_implants_prior_implants_retained_vector)>1, 'implants were', 'implant was')} left in place on the left.")
        }
      }
      
      if(length(left_implants_prior_rod_connected_no_vector) > 0 & length(left_implants_prior_rod_connected_yes_vector) == 0){
        revision_implants_statements_list$left_remove_rod <- glue("Once the instrumentation on the left was adequately exposed, the set screws were removed and the prior rod was completely removed.")
        
        if(length(left_implants_prior_implants_removed_vector) > 0){
          revision_implants_statements_list$left_remove_implants <- glue("The {glue_collapse(left_implants_prior_implants_removed_vector, sep = ', ', last = ' and ')} {if_else(length(left_implants_prior_implants_removed_vector)>1, 'implants were', 'implant was')} then removed without major difficulty.") 
          
        }
        
        if(length(left_implants_prior_implants_retained_vector) > 0){
          revision_implants_statements_list$left_retain_implants <- glue("The {glue_collapse(left_implants_prior_implants_retained_vector, sep = ', ', last = ' and ')} {if_else(length(left_implants_prior_implants_retained_vector)>1, 'implants were', 'implant was')} left in place on the left.")
        }
      }
      
      if(length(left_implants_prior_rod_connected_no_vector) == 0 & length(left_implants_prior_rod_connected_yes_vector) > 0){
        if(length(left_implants_prior_implants_retained_vector) > 0){
          revision_implants_statements_list$left_retain_all_implants <- glue("The implants on the left at {glue_collapse(left_implants_prior_implants_retained_vector, sep = ', ', last = ' and ')} were left in place, connected to the prior rod.")
        }
      }
    }else{
      revision_implants_statements_list$left_retain_all_implants <- glue("The implants on the left at {glue_collapse(left_implants_prior_implants_retained_vector, sep = ', ', last = ' and ')} were left in place, connected to the prior rod.")
    }
    
  }
  
  right_revision_implants_statements_df <- revision_implants_details_df %>%  
    filter(side == "right")
  
  if(nrow(right_revision_implants_statements_df) > 0){
    
    right_implants_prior_rod_connected_yes_vector <- (right_revision_implants_statements_df %>% filter(prior_rod_connected == "yes"))$level
    right_implants_prior_rod_connected_no_vector <- (right_revision_implants_statements_df %>% filter(prior_rod_connected == "no"))$level
    
    right_implants_prior_implants_retained_vector <- (right_revision_implants_statements_df %>% filter(remove_retain == "retain"))$level
    right_implants_prior_implants_removed_vector <- (right_revision_implants_statements_df %>% filter(remove_retain == "remove"))$level
    
    
    if(length(right_implants_prior_implants_removed_vector) > 0){
      revision_implants_statements_list$right_remove_implants_start <- paste("I then proceeded with removal of prior spinal instrumentation on the right.")
      
      
      if(length(right_implants_prior_rod_connected_no_vector) > 0 & length(right_implants_prior_rod_connected_yes_vector) > 0){
        if(jh_get_vertebral_number_function(level_to_get_number = tail(right_implants_prior_rod_connected_no_vector, 1)) < jh_get_vertebral_number_function(level_to_get_number = head(right_implants_prior_rod_connected_yes_vector, 1))){  ## this means that new rod is connected proximal to the old construct
          
          revision_implants_statements_list$right_cut_rod <- glue("Once the instrumentation on the right was adequately exposed, the rod was cut between {tail(right_implants_prior_rod_connected_no_vector, 1)} and {head(right_implants_prior_rod_connected_yes_vector, 1)}.")
          
          revision_implants_statements_list$right_remove_implants <- glue("The {glue_collapse(right_implants_prior_implants_removed_vector, sep = ', ', last = ' and ')} {if_else(length(right_implants_prior_implants_removed_vector)>1, 'implants were', 'implant was')} then removed without major difficulty.") 
          
          revision_implants_statements_list$right_retain_implants <- glue("The {glue_collapse(right_implants_prior_implants_retained_vector, sep = ', ', last = ' and ')} {if_else(length(right_implants_prior_implants_retained_vector)>1, 'implants were', 'implant was')} left in place on the right.")
          
        }else{ ### the new rod is connected distal to the old construct
          revision_implants_statements_list$right_cut_rod <- glue("Once the instrumentation on the right was adequately exposed, the rod was cut between {tail(right_implants_prior_rod_connected_yes_vector, 1)} and {head(right_implants_prior_rod_connected_no_vector, 1)}.")
          
          revision_implants_statements_list$right_remove_implants <- glue("The {glue_collapse(right_implants_prior_implants_removed_vector, sep = ', ', last = ' and ')} {if_else(length(right_implants_prior_implants_removed_vector)>1, 'implants were', 'implant was')} then removed without major difficulty.") 
          revision_implants_statements_list$right_retain_implants <- glue("The {glue_collapse(right_implants_prior_implants_retained_vector, sep = ', ', last = ' and ')} {if_else(length(right_implants_prior_implants_retained_vector)>1, 'implants were', 'implant was')} left in place on the right.")
        }
      }
      
      if(length(right_implants_prior_rod_connected_no_vector) > 0 & length(right_implants_prior_rod_connected_yes_vector) == 0){
        revision_implants_statements_list$right_remove_rod <- glue("Once the instrumentation on the right was adequately exposed, the set screws were removed and the prior rod was completely removed.")
        
        if(length(right_implants_prior_implants_removed_vector) > 0){
          revision_implants_statements_list$right_remove_implants <- glue("The {glue_collapse(right_implants_prior_implants_removed_vector, sep = ', ', last = ' and ')} {if_else(length(right_implants_prior_implants_removed_vector)>1, 'implants were', 'implant was')} then removed without major difficulty.") 
          
        }
        
        if(length(right_implants_prior_implants_retained_vector) > 0){
          revision_implants_statements_list$right_retain_implants <- glue("The {glue_collapse(right_implants_prior_implants_retained_vector, sep = ', ', last = ' and ')} {if_else(length(right_implants_prior_implants_retained_vector)>1, 'implants were', 'implant was')} left in place on the right.")
        }
      }
      
      if(length(right_implants_prior_rod_connected_no_vector) == 0 & length(right_implants_prior_rod_connected_yes_vector) > 0){
        if(length(right_implants_prior_implants_retained_vector) > 0){
          revision_implants_statements_list$right_retain_all_implants <- glue("The implants on the right at {glue_collapse(right_implants_prior_implants_retained_vector, sep = ', ', last = ' and ')} were left in place, connected to the prior rod.")
        }
      }
    }else{
      revision_implants_statements_list$right_retain_all_implants <- glue("The implants on the right at {glue_collapse(right_implants_prior_implants_retained_vector, sep = ', ', last = ' and ')} were left in place, connected to the prior rod.")
    }
  }
  
  if(length(revision_implants_statements_list)>0){
    revision_paragraph <- paste(glue_collapse(revision_implants_statements_list, sep = " "))
  }else{
    revision_paragraph <- ""
  }
  
  return(revision_paragraph)
}





#############-----------------------   PROCEDURES PERFORMED NUMBERED SECTION  ----------------------###############

op_note_procedures_performed_numbered_function <- function(objects_added_df,
                                                           revision_decompression_vector = NULL,
                                                           fusion_levels_vector = NULL,
                                                           additional_procedures_performed_vector = NULL){
  if(length(revision_decompression_vector) > 0){
    if(nrow(objects_added_df)>0){
      summary_nested_df <- objects_added_df %>%
        mutate(revision_levels_vector = list(revision_decompression_vector)) %>%
        select(level, vertebral_number, approach, category, object, side, revision_levels_vector, implant_statement, screw_size_type) %>%
        mutate(revision_level = map2(.x = level, .y = revision_levels_vector, .f = ~ str_detect(string = .x, pattern = .y))) %>%
        select(-revision_levels_vector) %>%
        mutate(revision_level = map(.x = revision_level, .f = ~ any(.x))) %>%
        unnest(revision_level) %>%
        replace_na(list(implant_statement = " ", screw_size_type = " ", revision_level = FALSE)) %>%
        mutate(revision_label = paste0("revision_", object)) %>%
        mutate(object = if_else(revision_level == FALSE, object, if_else(category == "decompression", revision_label, object))) %>%
        select(-revision_label) %>%
        # mutate(procedure_class = op_note_procedure_performed_summary_classifier_function(object = object)) %>%
        mutate(procedure_class = map(.x = object, .f = ~ op_note_procedure_performed_summary_classifier_function(.x))) %>%
        unnest(procedure_class) %>%
        # mutate(procedures_per_line = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_class)) %>%
        mutate(procedures_per_line = map(.x = procedure_class, .f = ~op_note_number_of_paragraphs_for_procedure_category(.x))) %>%
        unnest(procedures_per_line) %>%
        mutate(across(everything(), .fns = ~ as.character(.)))
    }else{
      summary_nested_df <- objects_added_df %>%
        select(level, object, vertebral_number) %>%
        # mutate(procedure_class = op_note_procedure_performed_summary_classifier_function(object = object)) %>%
        mutate(procedure_class = map(.x = object, .f = ~ op_note_procedure_performed_summary_classifier_function(.x))) %>%
        unnest(procedure_class) %>%
        # mutate(procedures_per_line = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_class)) 
        mutate(procedures_per_line = map(.x = procedure_class, .f = ~op_note_number_of_paragraphs_for_procedure_category(.x))) %>%
        unnest(procedures_per_line)%>%
        mutate(across(everything(), .fns = ~ as.character(.)))
    }
  }else{
    summary_nested_df <- objects_added_df %>%
      select(level, object, vertebral_number) %>%
      # mutate(procedure_class = op_note_procedure_performed_summary_classifier_function(object = object)) %>%
      mutate(procedure_class = map(.x = object, .f = ~ op_note_procedure_performed_summary_classifier_function(.x))) %>%
      unnest(procedure_class) %>%
      # mutate(procedures_per_line = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_class)) 
      mutate(procedures_per_line = map(.x = procedure_class, .f = ~op_note_number_of_paragraphs_for_procedure_category(.x))) %>%
      unnest(procedures_per_line)%>%
      mutate(across(everything(), .fns = ~ as.character(.)))
  }
  
  
  summary_single_statements <- summary_nested_df %>%
    filter(procedures_per_line == "distinct") %>%
    mutate(procedure_performed_statement = glue("{procedure_class} at {level}"))%>%
    select(procedure_class, procedure_performed_statement) %>%
    mutate(across(everything(), .fns = ~ as.character(.)))
  
  summary_multiple_nested <- summary_nested_df %>%
    filter(procedures_per_line == "combine") %>%
    group_by(procedure_class) %>%
    nest() %>%
    ungroup() %>%
    mutate(count = row_number()) %>%
    mutate(levels = map(.x = data, .f = ~ extract_levels_function(input_df = .x))) %>%
    select(-data) %>%
    unnest(levels) %>%
    mutate(procedure_performed_statement = glue("{procedure_class} at {levels}")) %>%
    mutate(procedure_performed_statement = if_else(procedure_class == "Pelvic instrumentation", paste("Instrumentation of the Pelvis with", levels, "fixation"), as.character(procedure_performed_statement))) %>%
    select(procedure_class, procedure_performed_statement)%>%
    mutate(across(everything(), .fns = ~ as.character(.)))
  
  added_procedures_df <- tibble(procedure_performed_statement = additional_procedures_performed_vector)%>%
    mutate(across(everything(), .fns = ~ as.character(.))) 
  
  procedures_numbered_df <- summary_nested_df %>%
    select(procedure_class) %>%
    filter(str_detect(string = procedure_class, pattern = "Inferior facetectom") == FALSE) %>%
    distinct() %>%
    left_join(summary_single_statements %>%
                union_all(summary_multiple_nested)) %>%
    select(procedure_performed_statement) %>%
    add_row(procedure_performed_statement = if_else(length(fusion_levels_vector) == 0, "xx", paste("Posterior Spinal Fusion at", glue_collapse(fusion_levels_vector, sep = ", ", last = " and ")))) %>%
    union_all(added_procedures_df) %>%
    filter(procedure_performed_statement !="xx") %>%
    mutate(count = row_number()) %>%
    mutate(procedures_performed_numbered = glue("{count}. {procedure_performed_statement}")) %>%
    select(procedures_performed_numbered)%>%
    mutate(across(everything(), .fns = ~ as.character(.)))
  
  glue_collapse(procedures_numbered_df$procedures_performed_numbered, sep = "\n")
}
