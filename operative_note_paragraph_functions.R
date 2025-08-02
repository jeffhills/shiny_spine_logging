#### ADDING AN OBJECT: you must do the following:
# - add object to 'all_object_ids_df.csv' and in load_coordinates_build_objects_new.R
# - add the object to a csv file in the 'coordinates' folder or create a new csv file
# - add the object to the "op_note_procedure_performed_summary_classifier_function"
# - add the object to the "op_note_number_of_paragraphs_for_procedure_category"
# - add the revision objects if needed
# - add the geom
# - in the app, if you made a new csv file in the coordinates folder, you need to load the new file. 
# - add to app list for the button


procedure_classifier_type_df <- tribble(~object_name, ~procedure_label, ~paragraph_type, 
                                        'incision_drainage', 'Incision and drainage', 'combine',
                                        'vertebroplasty', 'Vertebroplasty', 'combine',
                                        'vertebral_cement_augmentation', 'Vertebral body augmentation', 'combine',
                                        'laminar_downgoing_hook', 'Posterior spinal instrumentation', 'combine',
                                        'laminar_upgoing_hook', 'Posterior spinal instrumentation', 'combine',
                                        'c1_lateral_mass_screw', 'Posterior spinal instrumentation', 'combine',
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
                                        'transarticular_screw', 'Posterior spinal instrumentation', 'combine',
                                        'pelvic_screw', 'Pelvic instrumentation', 'combine',
                                        'pelvic_screw_1', 'Pelvic instrumentation', 'combine',
                                        'pelvic_screw_2', 'Pelvic instrumentation', 'combine',
                                        'reinsertion_screw', 'Reinsertion of spinal fixation', 'combine',
                                        'tether', 'Spinous process posterior tethering/wiring', 'combine',
                                        'costovertebral_approach', 'Costovertebral approach with decompression of the spinal cord', 'distinct',
                                        'revision_costovertebral_approach', 'Reexploration and revision decompression using a costovertebral approach', 'distinct',
                                        'transpedicular_approach', 'Transpedicular approach with decompression', 'distinct',
                                        'lateral_extraforaminal_approach', 'Decompression using a lateral extraforaminal approach', 'distinct',
                                        'lateral_extracavitary_approach', 'Arthrodesis using a modified lateral extracavitary approach', 'distinct',
                                        'corpectomy_extracavitary_tumor', 'Partial Corpectomy with decompression using a modified lateral extracavitary approach for tumor', 'distinct',
                                        'laminectomy_for_tumor', 'Laminectomy for biopsy and excision of extradural spinal tumor', 'combine',
                                        'laminectomy_for_facet_cyst', 'Laminectomy for excision of facet cyst (intraspinal lesion, not neoplasm)', 'combine',
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
                                        'partial_vertebral_excision', 'Anterior partial vertebral excision', 'combine',
                                        'corpectomy_cage', 'Anterior insertion of intervertebral biomechanical implant', 'combine',
                                        'anterior_plate', 'Anterior spinal instrumentation (distinct from an interbody implant)', 'combine',
                                        'anterior_plate_screw', 'Anterior spinal instrumentation (distinct from an interbody implant)', 'combine',
                                        'anterior_buttress_plate', 'Anterior spinal instrumentation (distinct from an interbody implant)', 'combine',
                                        'screw_washer', 'Anterior spinal instrumentation (distinct from an interbody implant)', 'combine',
                                        'removal_scs_paddle', 'Removal of spinal cord stimulator paddle', 'distinct',
                                        'removal_scs_perc_array', 'Removal of spinal cord stimulator perutaneous array', 'distinct',
                                        'removal_scs_receiver', 'Removal of spinal cord stimulator pulse generator/receiver', 'distinct'
                                        )



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



################-------------------------#################### -------- ANTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- ANTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- ANTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- ANTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- ANTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- ANTERIOR -----------################-------------------------#################### 
################-------------------------#################### -------- ANTERIOR -----------################-------------------------#################### 

#############-----------------------   Building Paragraphs: Combine Multiple Procedures into One ----------------------###############

op_note_object_combine_paragraph_function <- function(object, levels_nested_df, approach){
  
  if(object == "anterior_plate"){
    levels_df <- levels_nested_df %>%
      separate(col = level, into = c("cranial", "caudal"), sep = "-") %>%
      pivot_longer(cols = c(cranial, caudal)) %>%
      select(level = value) %>%
      mutate(level = if_else(level == "Iliac" | level =="S2AI", "S1", level)) %>%
      mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
      arrange(vertebral_number) %>%
      distinct()
    
    statement <- glue("I placed an anterior plate spanning {glue_collapse(levels_df$level, sep = ', ', last = ' and ')}. After selecting an appropriately sized plate, I held the plate into position and drilled the path for the screws. The screws were then inserted sequentially to hold the plate into position.")
  }
  
  if(object == "anterior_plate_screw"){
    
    screw_statement_df <- levels_nested_df %>%
      arrange(vertebral_number) %>%
      mutate(screw_size_type = str_remove_all(screw_size_type, " Anterior")) %>%
      mutate(screw_statement = glue("a {screw_size_type} screw on the {side}")) %>%
      group_by(level) %>%
      mutate(full_statement = paste0("At ", level, ", I placed ", glue_collapse(x = screw_statement, sep = ", ", last = " and "), ".")) %>%
      ungroup() %>%
      select(full_statement) %>%
      distinct()
    #
    
    statement <- paste(as.character(glue_collapse(screw_statement_df$full_statement, sep = " ")), "Intraoperative Xrays were taken to confirm position of all implants. ")
    
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
    
    if(str_detect(str_to_lower(approach), "transpsoas")){
      statement_df <- levels_nested_df %>%
        mutate(paragraph = glue("I confirmed that I had adequately exposed the {cranial_interspace_character} disk.  I then started with the diskectomies. Using a combination of a knife, curette, and pituitary rongeur, the annulus was incised and the disc was excised and the endplate was prepared for fusion. Under xray, I used a cobb to release the contralateral annulus. I then repositioned the retractor over the {caudal_interspace_character} disk. I used the EMG probe and direct visualization to confirm I had not entrapped any of the lumbar plexus. I repeated the same process at the {caudal_interspace_character} disk. I then repositioned my retractor over the {corpectomy_levels} body and confirmed my position on xray and used EMG again to confirm the lumbar plexus was clear. I ligated the segmental artery and vein. I then used an osteotome to perform the corpectomy and the the bone fragments were removed until I had adequately completed the corpectomy so that the implant could be placed.")) %>%
        select(paragraph) %>%
        distinct()
      
      statement <- paste(statement_df$paragraph[[1]])
    }else{
      statement_df <- levels_nested_df %>%
        mutate(paragraph = glue("I confirmed that I had adequately exposed up to the {cranial_interspace_character} disk cranially, and down to {caudal_interspace_character} disk caudally. I then started with the diskectomies. Using a combination of a knife, curette, pituitary rongeur, and Kerrison rongeur, the anterior longitudinal ligament was incised and the discs at {cranial_interspace_character} to {caudal_interspace_character} were completely excised. Once I was satisfied with the diskectomies, I used a combination of a burr and rongeur's to excise roughly 80% of the {corpectomy_levels} vertebral {if_else(length(corpectomy_levels)>1, 'bodies', 'body')}. I carried the corpectomy laterally to the edge of the uncus and dorsally to the posterior longitudinal ligament, effectively decompressing the central canal.")) %>%
        select(paragraph) %>%
        distinct()
      
      statement <- paste(statement_df$paragraph[[1]])
    }
  }
  
  if(object == "partial_corpectomy"){
    
    ## for 1 level partial corpectomy:
    if(nrow(levels_nested_df) == 1){
      
      corpectomy_direction <- unique(levels_nested_df$direction)
      corpectomy_level <- unique(levels_nested_df$level)
      
      if(corpectomy_direction == "superior"){
        interspace_character <- jh_get_cranial_caudal_interspace_body_list_function(level = corpectomy_level)$cranial_interspace
        
        cranial_to_corpectomy_level <- jh_get_cranial_caudal_interspace_body_list_function(level = corpectomy_level)$cranial_level
        
        if(str_starts(corpectomy_level, "T")){
          statement_df <- levels_nested_df %>%
            mutate(paragraph = glue("I confirmed that I had adequately exposed {corpectomy_level} body, and the {interspace_character} disk. Using a combination of a high-speed burr, curette, pituitary, and Kerrison rongeur, the pedicle of {corpectomy_level} was excised, and the posterior border of the vertebral body and dura was identified. Then, the posterior vertebral body of {corpectomy_level} was excised, creating a cavity for the ventral decompression. The decompression was carried dorsally to the posterior longitudinal ligament. ")) %>%
            select(paragraph) %>%
            distinct()
        }else{
          statement_df <- levels_nested_df %>%
            mutate(paragraph = glue("I confirmed that I had adequately exposed the {corpectomy_level} body, the {interspace_character} disk, and the {cranial_to_corpectomy_level} body. I then started with the discectomy. Using a combination of a knife, curette, pituitary rongeur, and Kerrison rongeur, the anterior longitudinal ligament was incised and the {interspace_character} disc was completely excised. Once I was satisfied with the discectomy, I used a combination of a burr and rongeur to excise roughly 60% of the {corpectomy_level} vertebral body. I carried the corpectomy laterally to the edge of the uncus and dorsally to the posterior longitudinal ligament, effectively decompressing the central canal.")) %>%
            select(paragraph) %>%
            distinct() 
        }
      }else{
        interspace_character <- jh_get_cranial_caudal_interspace_body_list_function(level = corpectomy_level)$caudal_interspace
        
        caudal_to_corpectomy_level <- jh_get_cranial_caudal_interspace_body_list_function(level = corpectomy_level)$caudal_level
        
        statement_df <- levels_nested_df %>%
          mutate(paragraph = glue("I confirmed that I had adequately exposed the {corpectomy_level} body, the {interspace_character} disk, and the {caudal_to_corpectomy_level} body. I then started with the discectomy. Using a combination of a knife, curette, pituitary rongeur, and Kerrison rongeur, the anterior longitudinal ligament was incised and the {interspace_character} disc was completely excised. Once I was satisfied with the discectomy, I used a combination of a burr and rongeur to excise roughly 60% of the {corpectomy_level} vertebral body. I carried the corpectomy laterally to the edge of the uncus and dorsally to the posterior longitudinal ligament, effectively decompressing the central canal.")) %>%
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
      
      if(str_starts(cranial_level_character, "T") & str_starts(caudal_level_character, "T")){
        statement_df <- levels_nested_df %>%
          mutate(paragraph = glue("I confirmed that I had adequately exposed the {cranial_level_character} body, the {interspace_character} disk, and the {caudal_level_character} body. Using a combination of a high-speed burr, curette, pituitary, and Kerrison rongeur, the pedicle of {caudal_level_character} was excised, and the posterior border of the vertebral body and dura was identified. Then, the posterior vertebral body of {cranial_level_character} and {caudal_level_character} were excised, creating a cavity for the ventral decompression. The decompression was carried dorsally to the posterior longitudinal ligament. ")) %>%
          select(paragraph) %>%
          distinct()
      }else{
        statement_df <- levels_nested_df %>%
          mutate(paragraph = glue("I confirmed that I had adequately exposed the {cranial_level_character} body, the {interspace_character} disk, and the {caudal_level_character} body. I then started with the discectomy. Using a combination of a knife, curette, pituitary rongeur, and Kerrison rongeur, the anterior longitudinal ligament was incised and the {interspace_character} disc was completely excised. Once I was satisfied with the discectomy, I used a combination of a burr and rongeur to excise roughly 60% of the {cranial_level_character} and {caudal_level_character} vertebral bodies. I carried the corpectomies laterally to the edge of the uncus and dorsally to the posterior longitudinal ligament, effectively decompressing the central canal.")) %>%
          select(paragraph) %>%
          distinct()  
      }
      
    }
    
    
    statement <- paste(statement_df$paragraph[[1]])
    
  }
  
  if(object == "corpectomy_cage"){
    
    cranial_level_character <- jh_get_vertebral_level_function(number = unique(min(levels_nested_df$vertebral_number)))
    
    cranial_to_corpectomy_level <- jh_get_cranial_caudal_interspace_body_list_function(level = cranial_level_character)$cranial_level
    
    caudal_level_character <- jh_get_vertebral_level_function(number = unique(max(levels_nested_df$vertebral_number)))
    
    caudal_to_corpectomy_level <- jh_get_cranial_caudal_interspace_body_list_function(level = caudal_level_character)$caudal_level
    
    
    statement_df <- levels_nested_df %>%
      mutate(paragraph = glue("I confirmed satisfactory bone preparation for the corpectomy endplates. I then measured the superior to inferior distance of the corpectomy defect and used a trial to confirm the appropriate size of the implant and the endplates. {implant_statement} ")) %>%
      mutate(paragraph = if_else(str_detect(implant_statement, "expand"),
                                 paste(paragraph, "Intraoperative xray was taken to confirm position of the implant. "),
                                 paste(paragraph, "The implant had a good press fit. Intraoperative xray was taken to confirm position of the implant. ")
      )
      ) %>%
      select(paragraph) %>%
      distinct()
    
    statement <- paste(statement_df$paragraph[[1]])
  }
  
  if(object == "partial_vertebral_excision"){
    
    cranial_level_character <- jh_get_vertebral_level_function(number = unique(min(levels_nested_df$vertebral_number)))
    
    cranial_to_corpectomy_level <- jh_get_cranial_caudal_interspace_body_list_function(level = cranial_level_character)$cranial_level
    
    caudal_level_character <- jh_get_vertebral_level_function(number = unique(max(levels_nested_df$vertebral_number)))
    
    caudal_to_corpectomy_level <- jh_get_cranial_caudal_interspace_body_list_function(level = caudal_level_character)$caudal_level
    
    
    statement_df <- levels_nested_df %>%
      mutate(paragraph = glue("I confirmed the boundaries of the osteophyte/lesion to be excised were clearly exposed and other nearby structures were protected. I then used a combination of a osteotome, rongeur and burr to excise the fragment until I felt it was adequately resected. ")) %>%
      select(paragraph) %>%
      distinct()
    
    statement <- paste(statement_df$paragraph[[1]])
  }

  
  return(statement)
  
}



#############-----------------------               End              ----------------------###############


#############-----------------------   Building Paragraphs: Distinct Operations ----------------------###############
distinct_anterior_procedure_paragraph_function <- function(level_input, object_input, side_input, implant_statement_input){
  
  burr_statement <- if_else(jh_get_vertebral_number_function(level_to_get_number = level_input) < 20, " First, I smoothed the anterior disk space and resected any osteophyte using a combination of a rongeur and burr. ", " ")
  
  if(object_input == "anterior_disc_arthroplasty"){
    paragraph <- glue("I then proceeded with total disk arthroplasty at the {level_input} interspace.{burr_statement}Using a combination of knife, curette, pituitary rongeur, and Kerrison rongeur, the disc was excised and the superior and inferior endplates were lightly decorticated to prepare the endplates for fusion, taking care not to disrupt the cortical endplate. The endplates were distracted and the posterior longitudinal ligament was adequately excised, fully decompressing the central canal and thecal sac. To complete the bilateral foraminotomies, I worked laterally on the left and right with a Kerrison rongeur, resecting any residual disk or osteophyte and fully decompressing the left and the right exiting nerve roots. Once I was satisfied with the decompression and end plate preparation, I trialed the implants and selected an appropriately sized disk replacement. The final implant was then inserted into the interspace of {level_input}. This completed the total disc arthroplasty of the {level_input} interspace.")
  }
  
  if(object_input == "decompression_diskectomy_fusion"){
    paragraph <- glue("I then proceeded with the discectomy, decompression, and interbody fusion of the {level_input} interspace.{burr_statement}Using a combination of knife, curette, pituitary rongeur, and Kerrison rongeur, the anterior longitudinal ligament was incised, the disc was excised and the superior and inferior endplates were lightly decorticated to prepare the endplates for fusion, taking care not to disrupt the cortical endplate. The endplates were distracted and the posterior longitudinal ligament was adequately excised, fully decompressing the central canal. I worked laterally on the left and right with a Kerrison rongeur, resecting any residual disk or osteophyte and fully decompressing the left and the right exiting nerve roots to complete the bilateral foraminotomies. This completed the anterior discectomy with decompression of the {level_input} interspace and partially completed the fusion of {level_input}.")
  }
  
  if(object_input == "diskectomy_fusion"){
    paragraph <- glue("I then proceeded with the discectomy and interbody fusion of the {level_input} interspace.{burr_statement}Using a combination of knife, curette, pituitary rongeur, and Kerrison rongeur, the anterior longitudinal ligament was incised, the disc was excised and the superior and inferior endplates were decorticated to prepare the endplates for fusion, taking care not to disrupt the cortical endplate. This completed the anterior discectomy of {level_input} interspace and partially completed the fusion of {level_input}.")
  }
  
  if(object_input == "diskectomy_fusion_no_interbody_device"){
    paragraph <- glue("I then proceeded with the discectomy and interbody fusion of the {level_input} interspace.{burr_statement}Using a combination of knife, curette, pituitary rongeur, and Kerrison rongeur, the anterior longitudinal ligament was incised and the disc was excised. The endplates were distracted and the and the superior and inferior endplates were lightly decorticated to prepare the endplates for fusion, taking care not to disrupt the cortical endplate. Once I was satisfied with the endplate preparation, bone graft was placed into the disk space. ANTERIOR_INTERBODY_BIOLOGIC_STATEMENT This completed the anterior discectomy and interbody fusion of the {level_input} interspace.")
  }
  
  if(object_input == "anterior_interbody_implant"){
    paragraph <- glue("I then proceeded with the insertion of the interbody implant into the {level_input} interspace. I again confirmed that the endplates were adequately decorticated and appropriately level. Once I was fully satisfied with the preparation of the endplates, I used trials and measured the disk space to determine the appropriate size of the interbody implant. {implant_statement_input} ANTERIOR_INTERBODY_BIOLOGIC_STATEMENT The final position was confirmed using intraoperative X-ray. This completed the anterior interbody implant at {level_input}.")
  }
  
  
  if(object_input == "corpectomy"){
    paragraph <- glue("I then proceeded with decompression and anterior vertebral body corpectomy at the {level_input} vertebral level. I confirmed that the exposure had been carried cranially to visualze the entire {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_interspace} disk, the anterior body of {level_input} and caudally to the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_interspace} disk space. First I started with the diskectomies. Using a combination of a knife, curette, pituitary rongeur, and Kerrison rongeur, the anterior longitudinal ligament was incised and the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_interspace} and {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_interspace} disc's were completely excised. Once I was satisfied with the diskectomies, I used a combination of a burr and rongeur's to excise roughly 80% of the {level_input} vertebral body. I carried the corpectomy dorsally to the posterior longitudinal ligament, effectively decompressing the central canal. This completed the vertebral body corpectomy at {level_input}.")
    
  }
  
  if(object_input == "corpectomy_cage"){
    paragraph <- glue("I then proceeded with the insertion of the intervertebral implant into the {level_input} level. I again confirmed that the endplates were adequately decorticated and appropriately level. Once I was fully satisfied with the preparation of the endplates, I used trials and measured the interspace to determine the appropriate size of the interbody implant. {implant_statement_input} I then inserted the  implant into the {level_input} space. ANTERIOR_INTERBODY_BIOLOGIC_STATEMENT The final position was confirmed using intraoperative X-ray. This completed the insertion of the intervertebral implant at {level_input}.")
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
  
  paragraphs <- glue_collapse(object_statement_paragraphs_df$paragraph, sep = "\n\n")
  
  return(paragraphs)
}
#############-----------------------               End              ----------------------###############


#############-----------------------   Paragraphs: Generate FULL Paragraphs  ----------------------###############

anterior_create_full_paragraph_statement_function <- function(procedure_paragraph_intro, 
                                                              df_with_levels_object_nested,
                                                              paragraphs_combined_or_distinct, 
                                                              approach = "anterior"){
  
  if(paragraphs_combined_or_distinct == "combine"){
    
    ### need to do this because screw implants and plate have disc or vertebral levels listed
    if(procedure_paragraph_intro == "anterior spinal instrumentation (distinct from an interbody implant)"){
      levels_df <- df_with_levels_object_nested %>%
        select(level, vertebral_number) %>%
        separate(level, into = c("cranial", "caudal"), sep = "-") %>%
        pivot_longer(cols = -vertebral_number, names_to = "region", values_to = "level") %>%
        select(level) %>%
        filter(!is.na(level)) %>%
        distinct() %>%
        mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
        arrange(vertebral_number) 
      
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
      mutate(approach = approach, 
             object = as.character(object)) %>%
      mutate(tech_statement = pmap(.l = list(
        ..1 = data, 
        ..2 = object, 
        ..3 = approach), 
        .f = ~ op_note_object_combine_paragraph_function(levels_nested_df = ..1, 
                                                         object = ..2,
                                                         approach = ..3)
      )) %>%
      select(object, tech_statement) %>%
      unnest(tech_statement) %>%
      distinct()
    
    # mutate(tech_statement = map(.x = data, 
    #                             .f = ~ op_note_object_combine_paragraph_function(object = object, 
    #                                                                              levels_nested_df = .x))) %>%
    
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

all_anterior_procedures_paragraphs_function <- function(all_objects_to_add_df,
                                                        approach = "anterior",
                                                        bone_graft_df = tibble(name = character(),
                                                                               value = double()),
                                                        anterior_bmp = 0){
  
  if(nrow(bone_graft_df)>0){
    if(any(bone_graft_df$name == "Other")){
      other_bone_graft <- (bone_graft_df %>%
        filter(value == 999))$name
      if(length(unique(all_objects_to_add_df$level)) > 1){
        other_bone_graft_statement <- glue("Additionally, {other_bone_graft} was evenly divided and added to increase the odds of fusion.") 
      }else{
        other_bone_graft_statement <- glue("Additionally, {other_bone_graft} was added to increase the odds of fusion.")
      }
      }else{
        other_bone_graft_statement <- ""
    }
    bone_graft_statement_df <- bone_graft_df %>%
      filter(value != 999) %>%
      mutate(statement = as.character(glue("{value}cc of {name}")))
    
    all_statements <- glue_collapse(x = bone_graft_statement_df$statement, sep = ", ", last = " and ")
    
    fusion_graft_statement <- as.character(glue("I placed a total of {all_statements} into the fusion bed. {other_bone_graft_statement}"))
    
  }else{
    fusion_graft_statement <- " "
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
    select(level, vertebral_number, object, side, contains("screw_size_type"), contains("implant_statement"), direction)
  
  
  anterior_order_of_operations_df <- tibble(object =  c("decompression_diskectomy_fusion",
                                                        "diskectomy_fusion",
                                                        "diskectomy_fusion_no_interbody_device",
                                                        "anterior_disc_arthroplasty",
                                                        "diskectomy_only",
                                                        "corpectomy",
                                                        "partial_corpectomy",
                                                        "partial_vertebral_excision",
                                                        "anterior_interbody_implant", 
                                                        "corpectomy_cage",
                                                        "screw_washer",
                                                        "anterior_buttress_plate",
                                                        "anterior_plate", 
                                                        "anterior_plate_screw")) %>%
    mutate(order_of_operation = row_number())
  
  
  anterior_df <- anterior_df %>%
    left_join(anterior_order_of_operations_df) %>%
    arrange(order_of_operation) %>%
    mutate(object = fct_inorder(object)) %>%
    select(-order_of_operation)
  
  
  anterior_procedure_category_nested_df <- anterior_df %>%
    mutate(screw_size_type = if_else(is.na(screw_size_type), " ", screw_size_type)) %>%
    mutate(procedure_category = map(.x = object, .f = ~op_note_procedure_performed_summary_classifier_function(.x))) %>%
    unnest(procedure_category) %>%
    mutate(procedure_category = str_to_lower(procedure_category)) %>%
    mutate(paragraphs_combine_or_distinct = map(.x = procedure_category, .f = ~op_note_number_of_paragraphs_for_procedure_category(.x))) %>%
    unnest(paragraphs_combine_or_distinct) %>%
    select(level, vertebral_number , procedure_category, object, side, paragraphs_combine_or_distinct, implant_statement, screw_size_type, direction) %>%
    group_by(procedure_category) %>%
    nest() %>%
    mutate(paragraphs_combine_or_distinct = map(.x = procedure_category, 
                                                .f = ~op_note_number_of_paragraphs_for_procedure_category(.x))) %>%
    unnest(paragraphs_combine_or_distinct)
  
  # mutate(paragraphs_combine_or_distinct = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_category)) 
  
  
  ########### NEW ############
  paragraphs_df <- anterior_procedure_category_nested_df %>%
    mutate(paragraphs = pmap(.l = list(..1 = procedure_category, 
                                       ..2 = data,
                                       ..3 = paragraphs_combine_or_distinct, 
                                       ..4 = approach), 
                             .f = ~ anterior_create_full_paragraph_statement_function(procedure_paragraph_intro = ..1, 
                                                                                      df_with_levels_object_nested = ..2, 
                                                                                      paragraphs_combined_or_distinct = ..3, 
                                                                                      approach = ..4))) %>%
    select(-data)
  
  ##################
  
  procedure_paragraphs <- glue_collapse(x = paragraphs_df$paragraphs, sep = "\n\n")
  
  

  if(!is.null(anterior_bmp) && anterior_bmp > 0){
    anterior_bmp_dose_per_level <- round(anterior_bmp/str_count(string = procedure_paragraphs, pattern = "ANTERIOR_INTERBODY_BIOLOGIC_STATEMENT"), 1)
    
    fusion_graft_statement <- glue("{fusion_graft_statement} Additionally, I placed {anterior_bmp_dose_per_level}mg of BMP into the fusion bed to improve the odds of successful fusion. ")
 
  }
  
  procedure_paragraphs <- str_replace_all(string = procedure_paragraphs, pattern = "ANTERIOR_INTERBODY_BIOLOGIC_STATEMENT", replacement = as.character(glue("{fusion_graft_statement}")))
  
  
  
  return(procedure_paragraphs)
  
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
    bind_rows(objects_added_df %>%
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
    bind_rows(plate_levels_df)
  
  if(length(revision_decompression_vector) > 0){
    
    if(nrow(objects_added_df)>0){
      summary_nested_df <- objects_added_df %>%
        mutate(revision_levels_vector = list(revision_decompression_vector)) %>%
        select(level, vertebral_number, approach, category, object, side, revision_levels_vector) %>%
        mutate(revision_level = map2(.x = level, .y = revision_levels_vector, .f = ~ str_detect(string = .x, pattern = .y))) %>%
        select(-revision_levels_vector) %>%
        mutate(revision_level = map(.x = revision_level, .f = ~ any(.x))) %>%
        unnest(revision_level) %>%
        mutate(object = if_else(category == "decompression" & revision_level == TRUE, paste0("revision_", object), paste0(object))) %>%
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
                bind_rows(summary_multiple_nested)) %>%
    select(procedure_performed_statement) %>%
    add_row(procedure_performed_statement = if_else(is.null(fusion_levels_vector), "", paste("Posterior Spinal Fusion at", glue_collapse(fusion_levels_vector, sep = ", ", last = " and ")))) %>%
    bind_rows(added_procedures_df) %>%
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
    bind_rows(instrumentation_not_lateral_mass_df) %>%
    bind_rows(lateral_mass_screws_text_df) %>%
    arrange(sequence_of_procedures)
  
  
  final_revised_all_paragraphs_df <- op_note_df %>%
    filter(name == "procedures") %>%
    mutate(value = glue_collapse(x = new_procedures_separated_df$procedure_text, sep = "\n\n")) %>%
    bind_rows(op_note_df %>% filter(name != "procedures")) %>%
    arrange(order)
  
  glue_collapse(x = final_revised_all_paragraphs_df$value, sep = "\n\n")
}

##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################


##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################


##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################
##########################-----------------------   POSTERIOR OPERATIVE NOTE PARAGRAPHS  ----------------------############################


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
    bind_rows(instrumentation_not_lateral_mass_df) %>%
    bind_rows(lateral_mass_screws_text_df) %>%
    arrange(sequence_of_procedures)
  
  
  final_revised_all_paragraphs_df <- op_note_df %>%
    filter(name == "procedures") %>%
    mutate(value = glue_collapse(x = new_procedures_separated_df$procedure_text, sep = "\n\n")) %>%
    bind_rows(op_note_df %>% filter(name != "procedures")) %>%
    arrange(order)
  
  glue_collapse(x = final_revised_all_paragraphs_df$value, sep = "\n\n")
}



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
        # mutate(object = if_else(level == "C1", paste0("c1_", object), object)) %>%
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
        # mutate(object = if_else(level == "C1", paste0("c1_", object), object)) %>%
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
  
  paragraph_df_prepped_df <- df_for_paragraphs %>%
    mutate(approach_technique_input = approach_technique,
           image_guidance_technique_input = image_guidance[1], 
           neuromonitoring_emg_statement_input = neuromonitoring_emg_statement, 
           method_implant_start_point = implant_start_point_method, 
           method_implant_confirmation = implant_confirmation_method)
    
    if(any(df_for_paragraphs$procedure_category == "pelvic instrumentation") & any(df_for_paragraphs$procedure_category == "posterior spinal instrumentation")){
      paragraph_df_prepped_df <- paragraph_df_prepped_df %>% 
          select(-method_implant_confirmation) %>%
          mutate(method_implant_confirmation = if_else(procedure_category == "pelvic instrumentation", implant_confirmation_method, " "))
    }
  

  
procedures_op_full_df <-  paragraph_df_prepped_df %>%
  ungroup() %>%
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


  # paragraph_df_prepped_df
  
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
    # print(paste(names(df_with_levels_object_nested), "are names for", procedure_paragraph_intro))
    
    df_with_levels_object_nested <- df_with_levels_object_nested %>%
      unnest(data)
  }
  
  # if("data" %in% names(df_with_levels_object_nested[[1]])){
  #   df_with_levels_object_nested <- df_with_levels_object_nested[[1]] %>%
  #     unnest(data)
  # }
  
  if(paragraphs_combined_or_distinct == "combine"){
    
    #IMAGING MODALITY USED FOR STARTPOINTS?:
    if(procedure_paragraph_intro == "posterior spinal instrumentation" && str_length(implant_start_point_method) > 5){
      implant_start_point_method_text <- implant_start_point_method
    }else{
      implant_start_point_method_text <- ""
    }
    
    if(any(str_detect(df_with_levels_object_nested$object, "pelvic_screw"))){
      df_with_levels_object_nested <- df_with_levels_object_nested %>%
        arrange(side) %>%
        mutate(object = "pelvic_screw")
    }
    
    df_with_statement <- df_with_levels_object_nested %>%
      # mutate(object = if_else(str_detect(object, "pelvic_screw"), "pelvic_screw", object)) %>%
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
    
    if(procedure_paragraph_intro == "posterior spinal instrumentation" | procedure_paragraph_intro == "pelvic instrumentation"){
    # if(procedure_paragraph_intro %in% c("posterior spinal instrumentation", "pelvic instrumentation")){
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
    }else if(procedure_paragraph_intro == "reinsertion of spinal fixation"){
      procedure_completion <- glue("This completed the reinsertion of spinal fixation at {glue_collapse(unique(x = df_levels$level), sep = ', ', last = ', and ')}.")
    }else if(procedure_paragraph_intro == "pelvic instrumentation"){
      procedure_completion <- glue("{implant_confirmation_method_text} This completed the instrumentation of the pelvis.")
    }else{
      procedure_completion <- glue("This completed the {procedure_paragraph_intro} at {glue_collapse(unique(x = df_levels$level), sep = ', ', last = ', and ')}.")
    }
    
    # print(paste("Made it to 1064 for ", procedure_paragraph_intro))
    
    ##CREATE THE STATEMENT
    if(procedure_paragraph_intro == "pelvic instrumentation"){
      statement <- glue("I then proceeded with instrumentation of the pelvis. {glue_collapse(df_with_statement$tech_statement, sep = ' ')} {procedure_completion}")
    }else if(procedure_paragraph_intro == "inferior facetectomies"){
      statement <- glue("I first performed {procedure_paragraph_intro} at {glue_collapse(unique(x = df_levels$level), sep = ', ', last = ', and ')}. {glue_collapse(df_with_statement$tech_statement, sep = ' ')}")
    }else if(procedure_paragraph_intro == "posterior column osteotomy" & length(unique(x = df_levels$level))>1){
      statement <- glue("I then proceeded with posterior column osteotomies at {glue_collapse(unique(x = df_levels$level), sep = ', ', last = ', and ')}. {glue_collapse(df_with_statement$tech_statement, sep = ' ')} {procedure_completion}")
    }else if(procedure_paragraph_intro == "reinsertion of spinal fixation"){
      statement <- glue("I reinserted screws at {glue_collapse(unique(x = df_levels$level), sep = ', ', last = ', and ')}. {glue_collapse(df_with_statement$tech_statement, sep = ' ')} {procedure_completion}")
    }else{
      statement <- glue("I then proceeded with the {procedure_paragraph_intro} at {glue_collapse(unique(x = df_levels$level), sep = ', ', last = ', and ')}. {glue_collapse(df_with_statement$tech_statement, sep = ' ')} {procedure_completion}")
    }
   
    # print(paste("Made it to 1079 for ", procedure_paragraph_intro))
    
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
    levels_side_df <- levels_side_df %>%
      mutate(level = str_remove_all(level, "_2"))
    
    pelvic_screws_wide <- levels_side_df %>%
      mutate(level = str_remove_all(level, "_2")) %>%
      select(level, side) %>%
      mutate(count = row_number()) %>%
      pivot_wider(names_from = side, values_from = count) %>%
      unnest(everything())
    
    
    
    pelvic_screws_statement_list <- list()
    
    # if(sum(str_count(string = pelvic_screws_wide$level, "Iliac")) == 1){
    #   pelvic_screws_statement_list$iliac_technique <- glue("For iliac screw insertion, the posterior iliac crest was identified and the entry point was countersunk. A probe was used to bore the path of the screw between the two cortices, aimed toward the anterior inferior iliac spine. Once the path was created, the walls of the path were palpated for breaches and the length of the path measured.")
    # }
    # if(sum(str_count(string = pelvic_screws_wide$level, "Iliac")) == 2){
    #   pelvic_screws_statement_list$iliac_technique <- glue("For iliac screw insertion, the posterior iliac crest was identified and the entry point was countersunk. A probe was used to bore the path of the screw between the two cortices, aimed toward the anterior inferior iliac spine. Once the path was created, the walls of the path were palpated for breaches and the length of the path measured. To fit two iliac screws, the start point was moved slightly proximal and distal to a traditional start point.")
    # }
    # 
    # if(sum(str_count(string = levels_side_df$level, "Iliac")) > 0){
    #   iliac_screws_df <- levels_side_df %>%
    #     filter(level == "Iliac") %>%
    #     mutate(statement = glue("a {str_to_lower(screw_size_type)} iliac screw was placed on the {side}")) 
    #   
    #   pelvic_screws_statement_list$iliac_sizes_statement <- glue("Using this technique, {glue_collapse(x = iliac_screws_df$statement, sep = ', ', last = ' and ')}.")
    # }
    
    if(any(levels_side_df$level == "Iliac")){
      pelvic_screws_statement_list$iliac_technique <- glue("For iliac screw insertion, the posterior iliac crest was identified and the entry point was countersunk. A probe was used to bore the path of the screw between the two cortices, aimed toward the anterior inferior iliac spine. Once the path was created, the walls of the path were palpated for breaches and the length of the path measured.")
      
      left_iliac_screws_df <- levels_side_df %>%
        filter(level == "Iliac", side == "left")
      
      if(nrow(left_iliac_screws_df)>0){
        left_iliac_screw_sizes <- glue_collapse(left_iliac_screws_df$screw_size_type, sep = " and ")
        
        if(length(left_iliac_screws_df$screw_size_type)>1){
          pelvic_screws_statement_list$left_iliac_screws <- str_to_sentence(glue("On the left, {left_iliac_screw_sizes} iliac screws were placed."))
        }else{
          pelvic_screws_statement_list$left_iliac_screws <- str_to_sentence(glue("An {left_iliac_screw_sizes} iliac screw was placed on the left."))
        }
      }
      
      right_iliac_screws_df <- levels_side_df %>%
        filter(level == "Iliac", side == "right")
      
      if(nrow(right_iliac_screws_df)>0){
        right_iliac_screw_sizes <- glue_collapse(right_iliac_screws_df$screw_size_type, sep = " and ")
        
        if(length(right_iliac_screws_df$screw_size_type)>1){
          pelvic_screws_statement_list$right_iliac_screws <- str_to_sentence(glue("On the right, {right_iliac_screw_sizes} iliac screws were placed."))
        }else{
          pelvic_screws_statement_list$right_iliac_screws <- str_to_sentence(glue("An {right_iliac_screw_sizes} iliac screw was placed on the right."))
        }
      }
      
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
  pelvic_screw_statement <- as.character(pelvic_screw_statement)
  
  
  
  if(str_detect(string = object, pattern = "screw") & object != "pelvic_screw"){
    screw_statements_wide_df <- levels_side_df %>%
      mutate(unilateral_screw_statement = str_to_lower(glue("a {screw_size_type} screw was inserted on the {side}"))) %>%
      select(level, side, unilateral_screw_statement) %>%
      pivot_wider(names_from = side, values_from = unilateral_screw_statement) %>%
      unnest(everything()) 
    
    screw_statements_df <- tibble(level = character(), left = character(), right = character(), unilateral_screw_statement = character()) %>%
      bind_rows(screw_statements_wide_df) %>%
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
    image_guidance == "Fluoroscopy-guided" ~ "For pedicle screws, the TP was palpated and a Jamshidi needle was placed at the junction of the TP and superior facet. AP and lateral X-rays were used to identify and confirm an appropriate start point and trajectory. The Jamshidi needle was then advanced down the pedicle and a wire placed through the pedicle and into the vertebral body. With the wire in place, the pedicles were then sequentially tapped to allow for screw placement. X-ray was used frequently throughout the procedure to confirm the position.",
    image_guidance == "Navigated" ~ "An intraoperative CT scan was obtained with a reference point attached to the patient. Navigated instruments were then used in conjunction with the navigation system to identify the appropriate starting point and trajectory for each screw. The pedicle was cannulated with navigation assistance and then palpated for a medial, inferior, superior and lateral wall to confirm a safe trajectory for placement of the screw.",
    image_guidance == "Robotic (using intraop CT)" ~ "Pedicle screws were placed with robotic assistance. A reference frame was secured to the patient's ilium and an intraoperative 3D spin was obtained. The pedicle screw trajectory and position were then planned. Once the plan for all screws was confirmed, I proceeded with robotic-assisted pedicle screw placement. An incision was made in the skin followed by a burr, drill and tap of the pedicle screw tract. ",
    image_guidance == "Robotic (using preop CT)" ~ "Pedicle screws were placed with robotic assistance. A reference frame was secured to the patient's ilium and AP and lateral intraop xrays were obtained and merged with the preoperative CT scan. Once an appropriate merge was confirmed and the plan for the screws was finalized, I proceeded with robotic-assisted pedicle screw placement. An incision was made in the skin followed by a burr, drill and tap of the pedicle screw tract. "
    
  )
  
  
  
  technique_statement <- case_when(
    object == "incision_drainage" ~ glue("The wound was inspected thoroughly and any necrotic appearing tissue was excised. Tissue samples were sent for cultures. The wound bed was thoroughly irrigated and the bone, deep muscle, fascia and subcutaneous tissue was debrided. Devitalized muscle, fascia and subcutaneous tissue was excised. The wound was irrigated thoroughly until I felt the wound was clean and an adequate debridement had been completed."), 
    object == "vertebroplasty" ~ glue("For vertebral body cement augmentation, a cannula was inserted into the vertebral body through the pedicle. The cement was then injected under x-ray guidance until an appropriate amount of cement had been injected into the vertebral body. Vertebroplasty was performed at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}."), 
    object == "vertebral_cement_augmentation" ~ glue("For vertebral body cement augmentation, I first created a cavity within the vertebral body. The cement was then injected under x-ray guidance until an appropriate amount of cement had been injected into the vertebral body. Vertebral augmentation was performed at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}."), 
    object == "laminar_downgoing_hook" ~ glue("For downgoing laminar hooks, the ligamentum flavum was removed at the site of the hook insertion. A hook was then inserted securely under the lamina {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "laminar_upgoing_hook" ~ glue("For upgoing laminar hooks, the inferior edge of the cranial lamina was identified and the plane between the lamina and ligamentum was developed and the upgoing hook is then placed within this plane, secured to the lamina aimed cranially {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "tp_hook" ~ glue("For transverse process hooks, the cranial edge of the transverse process was identified. A transverse process finder was used to develop the plane and a transverse process hook was placed securely into position along the superior edge of the transverse process. A transverse process hook was placed {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."),  
    object == "c1_lateral_mass_screw" ~ glue("For C1 lateral mass screw placement, after the exposure of the C1-C2 joint space was complete, the medial and lateral edges of the C1 lateral masses were identified and the articular surfaces of the C1-C2 joint were decorticated to allow for fusion. A burr was then used to create a start point in the center of the lateral mass. The path was then drilled incrementally to the anterior cortex. X-ray was used to confirm the drill was just up to the posterior cortex of the C1 tubercle on the lateral X-ray, and the path was then tapped. {glue_collapse(x = screw_statements_df$screw_statement, sep = ' ')}"), 
    object == "lateral_mass_screw" ~ glue("For lateral mass screw placement, the entry point was identified using the lateral and medial borders of the lateral mass and superior and inferior borders of the facet. The superficial cortex was opened at each entry point using a burr and the screw path drilled incrementally to the far cortex and the dorsal cortex was then tapped. {glue_collapse(x = screw_statements_df$screw_statement, sep = ' ')}"), 
    object == "occipital_screw" ~ glue("An appropriately sized occipital plate was selected and was placed centered in the midline and just caudal to the external occipital protuberance, but cranial to the foramen magnum. The plate was held in place to identify the appropriate start points for the occipital screws. The occipital screws were drilled incrementally until the anterior cortex was penetrated. The length of the path was measured to acheive bicortical fixation. The screw paths were then tapped and the screws placed, securing the plate to the occiput."), 
    object == "pars_screw" ~ glue("For pars screws, the start point was identified just 3-4mm cranial to the inferior facet joint and in the midpoint of the pars from medial to lateral. {start_point_identification_text} Once the start point was identified, the superficial cortex was opened at the entry point using the burr. The screw path was then cannulated, aiming as dorsal as possible while not perforating the dorsal cortex of the pars. The path was then tapped and the length measured and pars screw placed. {glue_collapse(x = screw_statements_df$screw_statement, sep = ' ')}"), # {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "pedicle_screw" ~ glue("{screw_technique_statement} {glue_collapse(x = screw_statements_df$screw_statement, sep = ' ')}"), 
    object == "reinsertion_screw" ~ glue("The prior screw tracks were palpated for any breaches or defects and the trajectory of the screw was modified as needed. {glue_collapse(x = screw_statements_df$screw_statement, sep = ' ')}"),
    object == "translaminar_screw" ~ glue("For translaminar screw fixation, the starting point was identified using the spinolaminar junction and in the central plane of the contralateral lamina. A burr hole was made to open the superficial cortex, the path was then cannulated and intralaminar screw placed. {glue_collapse(x = screw_statements_df$screw_statement, sep = ' ')}"), # {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."),
    object == "transarticular_screw" ~ glue("For transarticular screws, the start point was identified just 3-4mm cranial to the inferior facet joint and in the midpoint of the pars from medial to lateral. {start_point_identification_text} Once the start point was identified, the superficial cortex was opened at the entry point using the burr. The screw path was then cannulated, aiming toward the C1 lateral mass. The path was then tapped and the length measured and pars screw placed. {glue_collapse(x = screw_statements_df$screw_statement, sep = ' ')}"), # {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."),     object == "pedicle_hook" ~ glue("For pedicle hooks, a pedicle finder was carefully placed just under the inferior facet and above the superior facet, until the pedicle was found. Once the pedicle was identified, a pedicle hook was inserted directly toward the inferior pedicle, secured within the residual facet joint {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "pelvic_screw" ~ glue("{pelvic_screw_statement}"), 
    object == "pelvic_screw_1" ~ glue("{pelvic_screw_statement}"),
    object == "pelvic_screw_2" ~ glue("{pelvic_screw_statement}"),
    object == "sublaminar_wire" ~ glue("For sublaminar wiring, the doubled-up sublaminar was was bent into a hook shape and and passed under the inferior lamina from a caudal to cranial direction. Once the wire was safely passed, the tip of the wire was cut and the two strands were directed in opposite directions of the midline to be attached to the rods on the left and right side. Wires were passed under {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}."), 
    object == "tether" ~ glue("For spinous process tethering/wiring, a hole was drilled in the spinous processes over the lamina. A band was then threaded through the hole and appropriately tensioned and secured in place, tethering {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}."), 
    object == 'complete_facetectomy' ~ glue("For a complete facetectomy, the inferior, superior, medial and lateral borders of the inferior and superior facet were identified. Both the superior and inferior facet were completely excised and the underlying exiting nerve root decompressed. I performed a complete facetectomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}. This effectively decompressed {glue_collapse(x = exiting_roots_df$exiting_roots_statement, sep = ', ', last = ' and ')}."),
    object == 'grade_1' ~ glue("The inferior, superior, medial and lateral borders of the inferior facet were identified. The inferior facets {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} were excised and the bone was morselized to be used as morselized autograft."),
    object == 'grade_2' ~ glue("The borders of the inferior facet were identified and the inferior facets were adequately resected. A plane was then developed between the ligamentum and the dura centrally. This was carried out laterally in both directions until the facets were encountered. The superior facets were both adequately resected along with any necessary lamina to fully release the posterior column. {if_else(length(levels_side_df$level)>1, 'I performed posterior column (Smith-Peterson) osteotomies at', '')} {if_else(length(levels_side_df$level)>1, glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and '), '')}{if_else(length(levels_side_df$level)>1, '.', '')}"),   
    object == 'diskectomy' ~ glue("For the discectomy, I used the cranial and caudal laminar edges, pars, and facet joints as landmarks. Using a high-speed burr and Kerrison rongeur, I first performed a laminotomy, exposing the ligamentum flavum. I then carefully excised the ligamentum flavum, exposing the dura and traversing {nerve_roots_list$traversing_roots} nerve root. The dural sac was identified and traversing root was protected. The annulus was then incised and the diseased disk material was removed.  I performed a discectomy with partial medial facetectomy and foraminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the {nerve_roots_list$traversing_roots}. Following the decompression, the canal and foramen and lateral recess were palpated to confirm an appropriate decompression had been completed."),
    object == 'laminotomy' ~ glue("For the laminotomy, the cranial and caudal laminar edges, pars, and facet joints were used as landmarks. Once I had determined the laminotomy site, I used a combination of a high-speed burr and Kerrison rongeur to resect the bone dorsal to the nerve root. I performed a laminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. Following the decompression, the foramen was palpated to confirm an adequate decompression had been completed."),
    object == 'cervical_foraminotomy' ~ glue("For the posterior cervical foraminotomy at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}, the superior and inferior facet and joint line was clearly identified. I used a combination of a high-speed burr to resect roughly 50% of the overlying medial inferior facet, which exposed the superior facet. I then proceeded with the high-speed burr, resecting the superior articular facet out to the lateral border of the pedicles. Copious irrigation was used during the resection. After removing the superior facet, the exiting nerve root was identified. A Kerrison 1 was used to remove any remaining overlying tissue. I performed a posterior cervical foraminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. Following the decompression, the hemostasis was obtained and I was able to palpate the lateral walls of the cranial and caudal pedicles, confirming adequate decompression."),
    object == 'laminectomy' ~ glue("Using the cranial and caudal edges of the lamina and pars as landmarks, I performed a laminectomy at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}. First, using a combination of a high-speed burr and Kerrison rongeur, I resected the dorsal lamina. I then excised the underlying ligamentum flavum. Following the decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'laminectomy_for_facet_cyst' ~ glue("Using the cranial and caudal edges of the lamina and pars as landmarks, I performed a laminectomy at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} in order to excise the underlying facet cyst. First, using a combination of a high-speed burr and Kerrison rongeur, I resected the dorsal lamina. I then excised the underlying ligamentum flavum. This allowed exposure to the underlying facet cyst, which was fully excised. I then adequately resected amount of medial facet to fully decompress the lateral recess. Following the decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'laminectomy_for_tumor' ~ glue("Using the cranial and caudal edges of the lamina and pars as landmarks, I performed a laminectomy at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} in order to perform an open biopsy and excise the intraspinal extradural tumor. First, using a combination of a high-speed burr and Kerrison rongeur, I resected the dorsal lamina. I then excised the underlying ligamentum flavum. At this point, I encountered the extradural lesion which measured roughly {length(levels_side_df$level)*3.5}cm long by ~2-3cm wide. I obtained a biopsy of the tissue and then proceeded to excise any visible tumor in order to adequately create separation between visible tumor and the dura. Following the laminectomy and excision of the lesion, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'sublaminar_decompression' ~ glue("Using a combination of a high-speed burr and Kerrison rongeur, I performed a partial laminectomy bilaterally at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace. First, I resected the dorsal lamina and then excised the ligamentum flavum, exposing the dura. To fully decompress the exiting and traversing nerve roots within the neural foramen and lateral recess, the medial facet was partially excised and a foraminotomy performed on the left and right at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace. Following the decompression, the canal and foramen were palpated to confirm an adequate decompression had been completed."),
    object == 'revision_diskectomy' ~ glue("For the revision discectomy, I used the cranial and caudal laminar edges, pars, and facet joints as landmarks. I performed a revision and reexploration with discectomy and partial medial facetectomy and foraminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. I carefully dissected off the scar until I was able to safely identify the bony edges and develop a plane between the dura and scar. Using a high-speed burr and Kerrison rongeur, I performed a foraminotomy and excised the scar and ligamentum. The dural sac was identified and traversing root was protected. The annulus and disk space were identified and the diseased disk material was excised. Following the revision decompression, the canal and foramen and lateral recess were palpated to confirm an adequate decompression had been completed."),
    object == 'revision_laminotomy' ~ glue("For the revision laminotomy, the cranial and caudal laminar edges, pars, and facet joints were used as landmarks. I carefully exposed the dorsal elements until I had identified the edges of the laminar defect. Once I determined the laminotomy site, I used a combination of a high-speed burr and Kerrison rongeur to resect the bone dorsal to the nerve root. I performed a revision laminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. Following the decompression, the foramen was palpated to confirm an adequate decompression had been completed."),
    object == 'revision_laminectomy' ~ glue("I first identified the edge of the pars as a landmark at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}. Using a combination of a high-speed burr, curettes and Kerrison rongeur, I carefully dissected off the scar until I was able to safely identify the bony edges and develop a plane between the dura and scar. I removed any dorsal lamina, ligamentum flavum and scar. Following the revision decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'revision_sublaminar_decompression' ~ glue("Using a combination of a high-speed burr and Kerrison rongeur, I performed a reexploration and revision decompression bilaterally at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace. First, I carefully dissected off the scar until I was able to safely identify the bony edges and develop a plane between the dura and scar. I proceeded with partial laminectomies and excised the scar and underlying ligamentum flavum. The medial facet was partially excised and a foraminotomy performed on the left and right at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace, to fully decompress the exiting and traversing nerve roots within the neural foramen and lateral recess. Following the revision decompression, the canal and foramen were palpated to confirm an adequate decompression had been completed."),
    object == 'revision_complete_facetectomy' ~ glue("The cranial and caudal laminar edges, pars, and facet joints were used as landmarks to perform the reexploration and revision laminotomy and complete facetectomy. The bony edges were carefully identified and overlying scar excised. Once I was satisfied with the exposure and landmarks, a combination of a high-speed burr and rongeur were used and the superior and inferior facet were completely excised and the underlying exiting nerve root decompressed. I performed a complete facetectomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}. This effectively decompressed {glue_collapse(x = exiting_roots_df$exiting_roots_statement, sep = ', ', last = ' and ')}."),
    object == 'costotransversectomy' ~ glue("For the costotransversectomy, the rib was identified and a subperiosteal dissection was carried out laterally 6-7cm. Once the medial rib was skeletonized and a safe, subperiorsteal plane was confirmed, 5-6cm of the medial rib was transected. This process was completed {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}, allowing a safe costovertebral approach to the anterior vertebral body."),
    object == 'lateral_extracavitary_approach' ~ glue("For the modified lateral extracavitary approach at {glue_collapse(levels_side_df$level, sep = ', ', last = ' and ')}, the pedicle of {glue_collapse(levels_side_df$level, sep = ', ', last = ' and ')} was identified and using a combination of a high-speed burr, Kerrison rongeur and osteotome, the inferior and superior facets were fully resected, allowing access to the interspace. The lateral border of the disk space was identified and the dissection was carried out anteriorly around the entire anterior portion of the vertebral body. This was carried out {glue_collapse(levels_side_df$level_side, sep = ', ', last = ' and ')} and allowed safe access to the anterior longitudinal ligament and the most anterior portion of the vertbral body."),
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
  
  # interbody_statement_expandable <- if_else(str_detect(str_to_lower(interbody_statement), "expandable"), "The implant was then expanded until a solid bony fit was sensed.", " ")
  
  technique_statement <- case_when(
    object == 'transpedicular_approach' ~ glue("I then proceeded with a transpedicular approach for decompression from the {side} at {level}. Using a combination of a high-speed burr and Kerrison rongeur, the posterior elements were resected and the {side} {level} pedicle was skeletonized and resected, to allow full decompression of the {if_else(str_starts(level, 'L'), 'cauda equina and nerve roots', 'spinal cord')} at the {level} level. Following the decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'costovertebral_approach' ~ glue("I then proceeded with a costovertebral approach for decompression from the {side} at {level}. The {side} {level} rib was identified, skeletonized, and 5-6cm of the rib was resected to allow access to the dorsal vertebral body. Using a combination of a high-speed burr and Kerrison rongeur, the posterior elements were resected and the {side} {level} pedicle was skeletonized and resected, to allow full decompression of the spinal cord at the {level} level. Following the decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'lateral_extraforaminal_approach' ~ glue("I then proceeded with a lateral extraforaminal approach from the {side} at {level}. The pedicle of {level} was identified. The {side} {level} inferior and {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} superior facets were identified and the {level} pars was used as reference points. Using a combination of a high-speed burr and rongeur, the lateral {level} pars was partially resected, and the {level} nerve root was identified and protected. This allowed access to the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_interspace} interspace. The lateral border of the disk space was identified and the dissection was carried out laterally and medially until adequate exposure was obtained. The retropulsed disc fragment was identified causing the neural compression. I then proceed with excision of the disc until an adeqate decompression was completed.."),
    object == 'lateral_extracavitary_approach' ~ glue("I then proceeded with a modified lateral extracavitary approach from the {side} at {level}. The pedicle of {level} was identified. Using a combination of a high-speed burr and Kerrison rongeur and osteotome, the {side} {level} inferior and {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} superior facets were fully resected, allowing access to the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_interspace} interspace. The dissection was carried out to the lateral border of the disk space. The disk space was then entered and the disc excised and endplates prepared for arthrodesis. This allowed safe access to the most anterior portion of the vertebral body to distract and mobilize this interspace."),
    object == 'revision_transpedicular_approach' ~ glue("I then proceeded with a transpedicular approach for a reexploration and revision decompression from the {side} at {level}. Using a combination of a high-speed burr, curettes, and Kerrison rongeur, the scar and posterior elements were carefully resected until I found the plane between scar and dura. The {side} {level} pedicle was skeletonized and resected, to allow full decompression of the {if_else(str_starts(level, 'L'), 'cauda equina and nerve roots', 'spinal cord')} at the {level} level. Following the revision decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'revision_costovertebral_approach' ~ glue("I then proceeded with a costovertebral approach for a reexploration and revision decompression from the {side} at {level}. The {side} {level} rib was identified, skeletonized, and 5-6cm of the rib was resected to allow access to the dorsal vertebral body. Using a combination of a high-speed burr, curettes, and Kerrison rongeur, the scar and posterior elements were carefully resected until I found the plane between scar and dura. The {side} {level} pedicle was skeletonized and resected, to allow full decompression of the spinal cord at the {level} level. Following the revision decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'corpectomy_extracavitary_tumor' ~ glue("I then proceeded with a partial vertebral corpectomy using a modified lateral extracavitary approach from the {side} at {level}. The pedicle of {level} was identified. Using a combination of a high-speed burr and Kerrison rongeur and osteotome, the {side} {level} inferior and {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} superior facets were resected and the {level} superior and {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} inferior facets were resected as needed to skeletonize the {level} pedicle, allowing access to the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_interspace} interspace and {level} vertebral body. The lateral border of the disk space was identified and the dissection was carried out laterally and slightly anteriorly over the vertebral body. Once the posterior elements were resected and I had gained adequate exposure to the vertebral body via the extracavitary approach and the {level} nerve root could be visualized and protected, I used a combination of a burr, rongeur, osteotomes and curettes to resect any visible tumor and vertebral bone, creating a cavity in the vertebral body. Once an adequate cavity had been created, the remaining shell of the posterior vertebral body along with the PLL was impacted into the cavity, completing the decompression of the {if_else(str_detect(level, 'L'),'cauda equina and nerve roots', 'spinal cord')} and separation of visible tumor by at least 10mm. "),
    object == 'grade_3' ~ glue("I then proceeded with the pedicle subtraction osteotomy at the level of {level}. A central laminectomy was performed and confirmed at the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} level in order to prevent compression after osteotomy closure. First, the necessary posterior elements were resected. The inferior facets of {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} were resected up to the pedicle and then the {level} superior facets were completely resected, allowing the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} nerve roots to be visualized and tracked laterally. Then a cut was made through the pars just inferior to the {level} pedicle and the {level} inferior facet was completely resected along with the superior facet of {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level}. The {level} nerve root was visualized and carefully tracked laterally. Once the {level} pedicle was completely skeletonized, and the cranial and caudal nerve roots identified and a full decompression was confirmed from the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} pedicle to the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} pedicle, a subperiosteal plane was developed around the anterior portion of the {level} body and the retroperitoneal  structures were protected. The pedicle was resected down to the dorsal vertebral body bilaterally. The lateral portion of the pedicle wall was taken down, and adequate vertebral body was resected to allow for closure of the osteotomy, taking care to leave adequate anterior body and cortex. Once an adequate defect had been created in the vertebral body, the dorsal wall of the vertebral body was impacted. The nerve roots were checked again and the underside of the thecal sac was palpated to confirm no bony impingement. Once I was satisfied that the thecal sac was fully decompressed circumferentially and that the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} and {level} nerve roots were completely free, rods were connected bilaterally and the osteotomy was closed in a controlled fashion. The set screws were final tightened to lock the osteotomy in position. Intraoperative X-ray was performed to confirm appropriate correction through the osteotomy site."),    # object == 'grade_4' ~ glue("I then proceeded with an extended three column osteotomy (partial corpectomy) at the level of {level}. The posterior elements of {level} were completely removed. A complete central laminectomy was performed and confirmed at the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} level in order to prevent compression after osteotomy closure. A temporary rod was placed and starting on the contralateral side, a complete superior facetectomy was performed and any dorsal bone and scar was removed, and the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} nerve root was visualized and tracked laterally. Then a cut was made through the pars just inferior to the pedicle, and the superior facet of the inferior vertebra was resected, allowing the {level} nerve root to be visualized and tracked laterally. Once the {level} pedicle was completely skeletonized, and the cranial and caudal nerve roots identified and a full decompression was confirmed from the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} pedicle to the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} pedicle, a subperiosteal plane was developed around the anterior portion of the {level} body and retractors were held in place to protect the retroperitoneal  structures. Next, the pedicle was resected down to the dorsal vertebral body, leaving a lip medially to protect the neural structures. The dura, cranial and caudal nerve roots were protected and a burr was used to develop a defect in the vertebral body. The lateral portion of the pedicle wall was taken down, allowing easier access to the vertebral body and the vertebral body was hollowed out, extending up into the cranial disk space, taking care not to perforate the anterior cortex. The remaining pedicle walls were then resected with a box osteotome. We then placed a temporary rod and repeated the same process on the contralateral pedicle. Once roughly 50% of the vertebral body had been resected, the dorsal wall of the vertebral body was impacted. The nerve roots were checked again and the underside of the thecal sac was palpated to confirm no bony impingement. Once we were satisfied that the thecal sac was fully decompressed circumferentially and that the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} and {level} nerve roots were completely free, the osteotomy was closed, and the other rod was placed into position and set screws final tightened to lock the osteotomy in position. Intraoperative X-ray was performed to confirm appropriate alignment."),
    object == 'grade_4' ~ glue("I then proceeded with an extended three column osteotomy (partial corpectomy) at the level of {level}. The posterior elements of {level} were completely removed. A complete central laminectomy was confirmed at the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} level in order to prevent compression after osteotomy closure. Once I was satisfied that an adequate decompression had been performed proximally and distally, I worked to skeletonize the {level} pedicles. The {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} inferior facet and pars were resected bilaterally up to the level of the pedicle, exposing the exiting {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} nerve roots. I tracked these laterally to be sure there was no compression or dorsal bone that could cause compression after osteotomy closure. The {level} pars and inferior facet, and {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} superior facet were resected, exposing the {level} exiting nerve roots bilaterally. Again, I tracked the nerve roots laterally to confirm no sites of compression. At this point, the pedicles were skeletonized bilaterally. I developed a plane along laterally around the anterior portion of the {level} body to stay dorsal to the retroperitoneal structures. Then, each pedicle was fully resected along with the dorsal and superior vertebral body. I extended the resection into the disc space, partially resecting the superior {level} endplate along with the disc. Once roughly 50% of the vertebral body had been resected, the nerve roots were checked again and the underside of the thecal sac was palpated to confirm no bony impingement, and the dorsal wall of the vertebral body was impacted.  Once I was satisfied that the thecal sac was fully decompressed circumferentially and that the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} and {level} nerve roots were completely free, rods were connected bilaterally and the osteotomy was closed in a controlled fashion. Intraoperative X-ray was performed to confirm appropriate alignment and once I was satisfied with the alignment, the set screws across the osteotomy site were final tightened. "),
    object == 'grade_5' ~ glue("I then proceeded with a vertebral column resection at the level of {level}. A central decompression was performed and confirmed extending from the {cranial_level} pedicle to the {caudal_level} pedicle in order to prevent neural compression after osteotomy closure. A complete facetectomy was performed superiorly and any dorsal bone was removed, allowing the {cranial_level} nerve root to be visualized and tracked laterally. Similarly, a complete facetectomy was performed inferiorly, allowing the {level} nerve root to be visualized and tracked laterally. Next, the transverse process was resected and the {level} pedicles were completely skeletonized. The cranial and caudal nerve roots were then free and a full decompression was confirmed from the {cranial_level} pedicle to the {caudal_level} pedicle.{thoracic_nerve_ligation_statement} In preparation for the vertebral column resection, a unilateral stabilizing rod was placed on the convex side and secured with set screws multiple levels above and below the {level} level. Starting on the concavity, a combination of blunt and electrocautery dissection was used to develop a subperiosteal plane around the entire anterior portion of the {level} body and retractors were held in place to protect the anterior and neural structures. A lateral pedicle body window was developed for accessing the cancellous bone of the vertebral body and using curettes and rongeur, the accessible cancellous vertebral body was removed, extending up toward the cranial and caudal disk space. The remaining pedicle of {level} was resected down to the dorsal vertebral body, allowing the spinal cord to drift medially. The same process was then carried out on the convexity to access the cancellous vertebral body and remove the convex pedicle. The entire vertebral body was removed except for a thin anterior section, preserving the anterior longitudinal ligament. Discectomies were performed at the {cranial_interspace} and {caudal_interspace}, taking care to preserve the endplates. Once the entire body had been removed, a plane was developed between the ventral dural sac and the posterior body wall to be sure the dural sac was completely free from the body wall. An impactor was then used to impact the posterior body wall, completing the vertebral column resection. The dural sac was again inspected to confirm no potential sites of impingement. We then turned toward closure of the resection site. Starting with compression on the convexity, the site was closed through alternating compression on the convexity and slight distraction on the concave side. Once I was satisfied with the closure, rods were secured into place bilaterally with set screws."),
    
    object == 'no_implant_interbody_fusion' ~ glue("I then proceeded with the interbody fusion from the {side} side at the level of {level}. The inferior and superior facets of {str_replace_all(level, '-', ' and ')}, respectively, were resected and the exiting and traversing nerve roots were appropriately protected. The annulus was incised, allowing access to the disk space. Using a combination of curettes, shavers, and pituitary rongeur, the disk was excised and the endplates were prepped for an interbody fusion."),
    object == 'plif' ~ glue("I then proceeded with the posterior lumbar interbody fusion (PLIF) procedure and placement of interbody implant at the level of {level}. The inferior and superior facets of {str_replace_all(level, '-', ' and ')}, respectively, were partially resected and the exiting and traversing nerve roots were appropriately protected. The dura was retracted in order to access the disc space. The annulus was incised, allowing access to the disk space. The disk was excised and endplates were prepped using a combination of curettes, shavers, and pituitary rongeur. Once the endplates were adequately prepped, trials were used to determine the appropriate size of the implant. Once I was satisfied with the size of the trial, the interbody implant was placed into the interbody space along with allograft bone. {interbody_statement} The final position of the implant was confirmed using intraoperative X-ray."),
    object == "tlif" ~ glue("I then proceeded with the transforaminal interbody fusion (TLIF) procedure and placement of interbody implant from the {side} side at the level of {level}. The inferior and superior facets of {str_replace_all(level, '-', ' and ')}, respectively, were resected and the exiting and traversing nerve roots were appropriately protected. The annulus was incised, allowing access to the disk space. The disk was excised and endplates were prepped using a combination of curettes, shavers, and pituitary rongeur. Once the endplates were adequately prepped, trials were used to determine the appropriate size of the implant. Once I was satisfied with the size of the trial, the interbody implant was placed into the interbody space along with allograft bone. {interbody_statement} The final position of the implant was confirmed using intraoperative X-ray."),
    object == "intervertebral_cage" ~ glue("I then proceed with placement of the intervertebral cage. After ensuring the neural structures were completely free and the bony surfaces cranially and caudally were appropriately prepared, the intervertebral distance was measured. {interbody_statement} The implant was then inserted into the defect, abutting bone superiorly and inferiorly. Intraoperative X-ray was used to confirm appropriate size, fit, and alignment of the intervertebral implant spanning {level}."),
    object == "removal_scs_paddle" ~ glue("I then proceeded with removing the spinal cord stimulator paddle. Fluoroscopy was used to confirm the levels. The dissection was carried out until I encountered the leads. The leads were released from the soft tissue and the interspace was identified and. The leads were tracked down to the paddle device, which was then removed. This completed the removal of the spinal cord stimulator paddle."),
    object == "removal_scs_perc_array" ~ glue("I then proceeded with removing the spinal cord stimulator percutaneous array. Fluoroscopy was used to confirm the levels. The dissection was carried out until I encountered the leads. The leads were released from the soft tissue and the interspace was identified. The array was then removed without difficulty. This completed the removal of the spinal cord stimulator percutaneous array."),
    object == "removal_scs_receiver" ~ glue("I then proceeded with removing the spinal cord stimulator pulse generator/receiver. The prior incision was used and dissection was carried down to the receiver. The soft tissue was released around the receiver and sutures excised until it was completely free and the device was removed. The pseudocapsule was partially excised. This completed the removal of the spinal cord stimulator pulse generator/receiver.")
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
  
  objects_added_df <- objects_added_df %>%
    filter(object != "grade_1")
  
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
  
  if(any(summary_nested_df$procedure_class == "Anterior spinal instrumentation (distinct from an interbody implant)")){
    ant_instrumentation_df <- summary_nested_df %>%
      filter(procedure_class == "Anterior spinal instrumentation (distinct from an interbody implant)") %>%
      separate(level, into = c("cranial", "caudal"), sep = "-") %>%
      pivot_longer(cols = c("cranial", "caudal"), names_to = "region", values_to = "level") %>%
      # select(level) %>%
      filter(!is.na(level)) %>%
      distinct() %>%
      mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
      arrange(vertebral_number) %>%
      select(level, object, vertebral_number, procedure_class, procedures_per_line) 
    
    summary_nested_df <- summary_nested_df %>%
      filter(procedure_class != "Anterior spinal instrumentation (distinct from an interbody implant)") %>%
      bind_rows(ant_instrumentation_df)
    
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
                bind_rows(summary_multiple_nested)) %>%
    select(procedure_performed_statement) %>%
    add_row(procedure_performed_statement = if_else(length(fusion_levels_vector) == 0, "xx", paste("posterior spinal fusion at", glue_collapse(fusion_levels_vector, sep = ", ", last = " and ")))) %>%
    bind_rows(added_procedures_df) %>%
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
      revision_implants_statements_list$left_remove_implants_start <- paste("I then proceeded with removal of prior spinal instrumentation.")
      
      
      if(length(left_implants_prior_rod_connected_no_vector) > 0 & length(left_implants_prior_rod_connected_yes_vector) > 0){
        if(jh_get_vertebral_number_function(level_to_get_number = tail(left_implants_prior_rod_connected_no_vector, 1)) < jh_get_vertebral_number_function(level_to_get_number = head(left_implants_prior_rod_connected_yes_vector, 1))){  ## this means that new rod is connected proximal to the old construct
          
          revision_implants_statements_list$left_cut_rod <- glue("Once the instrumentation on the left was adequately exposed, the rod was cut between {tail(left_implants_prior_rod_connected_no_vector, 1)} and {head(left_implants_prior_rod_connected_yes_vector, 1)}.")
          
          revision_implants_statements_list$left_remove_implants <- glue("The left {glue_collapse(left_implants_prior_implants_removed_vector, sep = ', ', last = ' and ')} {if_else(length(left_implants_prior_implants_removed_vector)>1, 'implants were', 'implant was')} then removed without major difficulty.") 
          
          # revision_implants_statements_list$left_retain_implants <- glue("The {glue_collapse(left_implants_prior_implants_retained_vector, sep = ', ', last = ' and ')} {if_else(length(left_implants_prior_implants_retained_vector)>1, 'implants were', 'implant was')} left in place on the left.")
          
        }else{ ### the new rod is connected distal to the old construct
          revision_implants_statements_list$left_cut_rod <- glue("Once the instrumentation on the left was adequately exposed, the rod was cut between {tail(left_implants_prior_rod_connected_yes_vector, 1)} and {head(left_implants_prior_rod_connected_no_vector, 1)}.")
          
          revision_implants_statements_list$left_remove_implants <- glue("The left {glue_collapse(left_implants_prior_implants_removed_vector, sep = ', ', last = ' and ')} {if_else(length(left_implants_prior_implants_removed_vector)>1, 'implants were', 'implant was')} then removed without major difficulty.") 
          # revision_implants_statements_list$left_retain_implants <- glue("The {glue_collapse(left_implants_prior_implants_retained_vector, sep = ', ', last = ' and ')} {if_else(length(left_implants_prior_implants_retained_vector)>1, 'implants were', 'implant was')} left in place on the left.")
        }
      }
      
      if(length(left_implants_prior_rod_connected_no_vector) > 0 & length(left_implants_prior_rod_connected_yes_vector) == 0){
        revision_implants_statements_list$left_remove_rod <- glue("Once the instrumentation on the left was adequately exposed, the set screws were removed and the prior rod was completely removed.")
        
        if(length(left_implants_prior_implants_removed_vector) > 0){
          revision_implants_statements_list$left_remove_implants <- glue("The {glue_collapse(left_implants_prior_implants_removed_vector, sep = ', ', last = ' and ')} {if_else(length(left_implants_prior_implants_removed_vector)>1, 'implants were', 'implant was')} then removed without major difficulty.") 
          
        }
        
        # if(length(left_implants_prior_implants_retained_vector) > 0){
        #   revision_implants_statements_list$left_retain_implants <- glue("The {glue_collapse(left_implants_prior_implants_retained_vector, sep = ', ', last = ' and ')} {if_else(length(left_implants_prior_implants_retained_vector)>1, 'implants were', 'implant was')} left in place on the left.")
        # }
      }
      
      # if(length(left_implants_prior_rod_connected_no_vector) == 0 & length(left_implants_prior_rod_connected_yes_vector) > 0){
      #   if(length(left_implants_prior_implants_retained_vector) > 0){
      #     revision_implants_statements_list$left_retain_all_implants <- glue("The implants on the left at {glue_collapse(left_implants_prior_implants_retained_vector, sep = ', ', last = ' and ')} were left in place, connected to the prior rod.")
      #   }
      # }
    }else{
      revision_implants_statements_list$left_retain_all_implants <- glue("The implants on the left were left in place.")
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
      # revision_implants_statements_list$right_remove_implants_start <- paste("I then proceeded with removal of prior spinal instrumentation on the right.")
      
      
      if(length(right_implants_prior_rod_connected_no_vector) > 0 & length(right_implants_prior_rod_connected_yes_vector) > 0){
        if(jh_get_vertebral_number_function(level_to_get_number = tail(right_implants_prior_rod_connected_no_vector, 1)) < jh_get_vertebral_number_function(level_to_get_number = head(right_implants_prior_rod_connected_yes_vector, 1))){  ## this means that new rod is connected proximal to the old construct
          
          revision_implants_statements_list$right_cut_rod <- glue("Once the instrumentation on the right was adequately exposed, the rod was cut between {tail(right_implants_prior_rod_connected_no_vector, 1)} and {head(right_implants_prior_rod_connected_yes_vector, 1)}.")
          
          revision_implants_statements_list$right_remove_implants <- glue("The right {glue_collapse(right_implants_prior_implants_removed_vector, sep = ', ', last = ' and ')} {if_else(length(right_implants_prior_implants_removed_vector)>1, 'implants were', 'implant was')} then removed without major difficulty.") 
          
          # revision_implants_statements_list$right_retain_implants <- glue("The {glue_collapse(right_implants_prior_implants_retained_vector, sep = ', ', last = ' and ')} {if_else(length(right_implants_prior_implants_retained_vector)>1, 'implants were', 'implant was')} left in place on the right.")
          
        }else{ ### the new rod is connected distal to the old construct
          revision_implants_statements_list$right_cut_rod <- glue("Once the instrumentation on the right was adequately exposed, the rod was cut between {tail(right_implants_prior_rod_connected_yes_vector, 1)} and {head(right_implants_prior_rod_connected_no_vector, 1)}.")
          
          revision_implants_statements_list$right_remove_implants <- glue("The right {glue_collapse(right_implants_prior_implants_removed_vector, sep = ', ', last = ' and ')} {if_else(length(right_implants_prior_implants_removed_vector)>1, 'implants were', 'implant was')} then removed without major difficulty.") 
          # revision_implants_statements_list$right_retain_implants <- glue("The {glue_collapse(right_implants_prior_implants_retained_vector, sep = ', ', last = ' and ')} {if_else(length(right_implants_prior_implants_retained_vector)>1, 'implants were', 'implant was')} left in place on the right.")
        }
      }
      
      if(length(right_implants_prior_rod_connected_no_vector) > 0 & length(right_implants_prior_rod_connected_yes_vector) == 0){
        revision_implants_statements_list$right_remove_rod <- glue("Once the instrumentation on the right was adequately exposed, the set screws were removed and the prior rod was completely removed.")
        
        if(length(right_implants_prior_implants_removed_vector) > 0){
          revision_implants_statements_list$right_remove_implants <- glue("The right {glue_collapse(right_implants_prior_implants_removed_vector, sep = ', ', last = ' and ')} {if_else(length(right_implants_prior_implants_removed_vector)>1, 'implants were', 'implant was')} then removed without major difficulty.") 
          
        }
        
        # if(length(right_implants_prior_implants_retained_vector) > 0){
        #   revision_implants_statements_list$right_retain_implants <- glue("The {glue_collapse(right_implants_prior_implants_retained_vector, sep = ', ', last = ' and ')} {if_else(length(right_implants_prior_implants_retained_vector)>1, 'implants were', 'implant was')} left in place on the right.")
        # }
      }
      
      # if(length(right_implants_prior_rod_connected_no_vector) == 0 & length(right_implants_prior_rod_connected_yes_vector) > 0){
        # if(length(right_implants_prior_implants_retained_vector) > 0){
          # revision_implants_statements_list$right_retain_all_implants <- glue("The implants on the right at {glue_collapse(right_implants_prior_implants_retained_vector, sep = ', ', last = ' and ')} were left in place, connected to the prior rod.")
        # }
      # }
    }else{
      revision_implants_statements_list$right_retain_all_implants <- glue("The implants on the right were left in place.")
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
jh_compress_spinal_levels_in_sentence <- function(sentence) {
  vertebrae <- c(paste0("C", 1:7), paste0("T", 1:12), paste0("L", 1:5), "S1", "S2", "S2AI", "Ilium")
  parts <- str_match(sentence, "^(\\d+\\.\\s*)?(.*? at )(.+)$")
  if (is.na(parts[1,1])) return(sentence)
  
  prefix <- parts[2] %||% ""
  intro <- parts[3]
  levels <- str_replace_all(parts[4], " and ", ",") %>%
    str_split(",\\s*") %>% unlist() %>%
    str_remove("\\.+$") %>% str_trim() %>% unique()
  
  ord <- match(levels, vertebrae)
  valid <- !is.na(ord)
  levels <- levels[valid]
  ord <- ord[valid]
  if (length(ord) == 0) return(sentence)
  
  sorted_levels <- levels[order(ord)]
  sorted_ord <- sort(ord)
  
  if (all(diff(sorted_ord) == 1)) {
    return(glue::glue("{prefix}{intro}{sorted_levels[1]}-{sorted_levels[length(sorted_levels)]}"))
  }
  
  runs <- rle(c(NA, diff(sorted_ord)) == 1)
  ends <- cumsum(runs$lengths)
  starts <- c(1, head(ends + 1, -1))
  
  compressed <- sapply(seq_along(starts), function(i) {
    rng <- sorted_levels[starts[i]:ends[i]]
    if (length(rng) <= 2) paste(rng, collapse = ", ") else paste0(rng[1], "-", rng[length(rng)])
  })
  
  paste0(prefix, intro, glue::glue_collapse(compressed, sep = ", "))
}

jh_compress_spinal_fusion_levels_in_sentence <- function(sentence) {
  vertebrae <- c(paste0("C", 1:7), paste0("T", 1:12), paste0("L", 1:5), "S1", "S2", "S2AI", "Ilium")
  parts <- str_match(sentence, "^(\\d+\\.\\s*)?(.*? at )(.+)$")
  if (is.na(parts[1,1])) return(sentence)
  
  prefix <- parts[2] %||% ""
  intro <- parts[3]
  segment_text <- str_replace_all(parts[4], " and ", ",")
  segments <- str_split(segment_text, ",\\s*")[[1]] %>%
    str_remove("\\.+$") %>% str_trim() %>% unique()
  
  # Extract individual vertebrae from each motion segment
  split_levels <- str_split(segments, "-", simplify = TRUE)
  upper_levels <- split_levels[,1]
  lower_levels <- split_levels[,2]
  
  # Remove invalid or malformed entries
  valid <- upper_levels %in% vertebrae & lower_levels %in% vertebrae
  upper_levels <- upper_levels[valid]
  lower_levels <- lower_levels[valid]
  
  # Full list of involved vertebrae: start + end
  involved_levels <- unique(c(upper_levels[1], lower_levels))
  involved_ord <- match(involved_levels, vertebrae)
  
  if (length(involved_ord) > 3 && all(diff(sort(involved_ord)) == 1)) {
    compressed_range <- paste0(involved_levels[1], "-", involved_levels[length(involved_levels)])
    return(glue::glue("{prefix}{intro}{compressed_range}"))
  } else {
    return(sentence)
  }
}

op_note_procedures_performed_numbered_function <- function(objects_added_df,
                                                           revision_implants_df = tibble(level = character(), vertebral_number = double(), object = character(), x = double(), y = double(), prior_rod_connected = character(), remove_retain = character()),
                                                           revision_decompression_vector = NULL,
                                                           fusion_levels_vector = NULL,
                                                           additional_procedures_performed_vector = NULL){
  
  
  ##### THIS IS WHAT CREATES the "REINSERTION OF SPINAL FIXATION" procedure
  # if(nrow(revision_implants_df)>0 & nrow(objects_added_df)>0){
  #   implant_removal_df <- revision_implants_df %>%
  #     filter(remove_retain == "remove") %>%
  #     select(level, remove_retain) %>%
  #     distinct()
  #   
  #   re_inserted_df <- objects_added_df %>%
  #     filter(str_detect(object, "screw")) %>%
  #     left_join(implant_removal_df) %>%
  #     filter(remove_retain == "remove")
  #   
  #   objects_added_df <- objects_added_df %>%
  #     anti_join(re_inserted_df %>% select(-remove_retain)) %>%
  #     bind_rows((re_inserted_df %>%
  #                  mutate(object = "reinsertion_screw")))
  #   
  # } 
  
  if(any(objects_added_df$object == "incision_drainage")){
    id_df <- objects_added_df %>%
      filter(object == "incision_drainage")
    
    top_level <- (id_df %>%
      filter(vertebral_number == min(vertebral_number)))$level
    bottom_level <- (id_df %>%
                    filter(vertebral_number == max(vertebral_number)))$level
    
    if(str_detect(str_to_lower(top_level), "o|c|t")){
      i_d_cpt_code <- "22010"
    }else{
        i_d_cpt_code <- "22015"
    }
    
    id_statement <- glue("Incision & Drainage of deep, subfascial spine abscess from {top_level} to {bottom_level} ({i_d_cpt_code})")
    
    objects_added_df <- objects_added_df %>%
      filter(object != "incision_drainage")
  }else{
    id_statement <- "xx"
  }
  
  if(nrow(objects_added_df)>0){
    objects_added_df <- objects_added_df %>%
    mutate(level = if_else(level == "Iliac_2", "Iliac", level))%>%
    mutate(level = if_else(level == "S2AI_2", "S2AI", level))
  }
  
  
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
    # mutate(vertebral_number = as.double(vertebral_number)) %>%
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
                bind_rows(summary_multiple_nested)) %>%
    select(procedure_performed_statement) %>%
    add_row(procedure_performed_statement = if_else(length(fusion_levels_vector) == 0, "xx", paste("Posterior Spinal Fusion at", glue_collapse(fusion_levels_vector, sep = ", ", last = " and ")))) %>%
    add_row(procedure_performed_statement = id_statement) %>%
    bind_rows(added_procedures_df) %>%
    filter(procedure_performed_statement !="xx") %>%
    mutate(count = row_number()) %>%
    mutate(procedures_performed_numbered = glue("{count}. {procedure_performed_statement}")) %>%
    select(procedures_performed_numbered)%>%
    mutate(across(everything(), .fns = ~ as.character(.))) 
  
  glue_collapse(procedures_numbered_df$procedures_performed_numbered, sep = "\n")
}
