anterior_create_full_paragraph_statement_function <- function(procedure_paragraph_intro, df_with_levels_object_nested, paragraphs_combined_or_distinct){
  if(paragraphs_combined_or_distinct == "combine"){
    
    if(procedure_paragraph_intro == "anterior spinal instrumentation"){
      
      
      levels_df <- df_with_levels_object_nested %>%
        mutate(level = if_else(object == "anterior_buttress_plate", identify_cranial_caudal_interspace_body_list_function(level = level)$caudal_level, level)) %>%
        separate(col = level, into = c("cranial", "caudal"), sep = "-") %>%
        pivot_longer(cols = c(cranial, caudal)) %>%
        select(level = value) %>%
        filter(!is.na(level)) %>%
        mutate(level = if_else(level == "Iliac" | level == "S2AI", "S1", level)) %>%
        left_join(levels_numbered_df) %>%
        arrange(vertebral_number) %>%
        distinct()
    }else{
      levels_df <- df_with_levels_object_nested %>%
        select(level, vertebral_number) %>%
        distinct() %>%
        arrange(vertebral_number)
    }
    
    start_statement <- glue("I then proceeded with {procedure_paragraph_intro} at {glue_collapse(levels_df$level, sep = (', '), last = ' and ')}.")
    
    df_with_statement <- df_with_levels_object_nested %>%
      group_by(object) %>%
      nest() %>%
      mutate(tech_statement = map(.x = data, .f = ~ op_note_object_combine_paragraph_function(object = object, levels_nested_df = .x))) %>%
      select(object, tech_statement) %>%
      unnest() %>%
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


###########################################
###########################################

op_note_object_combine_paragraph_function <- function(object, levels_nested_df){
  
  if(object == "anterior_plate"){
    levels_df <- levels_nested_df %>%
      separate(col = level, into = c("cranial", "caudal"), sep = "-") %>%
      pivot_longer(cols = c(cranial, caudal)) %>%
      select(level = value) %>%
      mutate(level = if_else(level == "Iliac" | level =="S2AI", "S1", level)) %>%
      left_join(levels_numbered_df) %>%
      arrange(vertebral_number) %>%
      distinct()
    
    statement <- glue("I placed an anterior plate spanning {glue_collapse(levels_df$level, sep = ', ', last = ' and ')}. After selecting an appropriately sized plate and length for the screws, I held the plate into position and drilled and tapped the path for the screws. The screws were then inserted sequentially into the vertebral body of {glue_collapse(levels_df$level, sep = ', ', last = ' and ')} to hold the plate into position.")
  }
  
  if(object == "anterior_buttress_plate"){
    statement_df <- levels_nested_df %>%
      mutate(statements = glue("At the {level} level, I placed an anterior buttress plate. After selecting an appropriately sized plate and length for the screws, I held the plate into position and drilled and tapped the path for the screw. I then inserted the screw to secure the plate to the anterior vertebral body."))
    
    statement <- glue_collapse(statement_df$statements, sep = " ")
  }
  
  if(object == "screw_washer"){
    statement_df <- levels_nested_df %>%
      mutate(statements = glue("At the {level} level, I placed a screw to work as a buttress. The midline of the anterior vertebral endplate was identified and a drill was used to create a trajectory for the screw. I then inserted the screw the into the {level} vertebral body."))
    
    statement <- glue_collapse(statement_df$statements, sep = " ")
  }
  
  if(object == "corpectomy_cage"){
    cranial_level_df <- levels_nested_df %>%
      filter(vertebral_number == min(vertebral_number)) %>%
      mutate(level = identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_level) %>%
      select(level) %>%
      distinct()
    
    caudal_level_df <- levels_nested_df %>%
      filter(vertebral_number == max(vertebral_number)) %>%
      mutate(level = identify_cranial_caudal_interspace_body_list_function(level = level)$caudal_level) %>%
      select(level) %>%
      distinct()
    
    statement_df <- levels_nested_df %>%
      mutate(paragraph = glue("I confirmed satisfactory decompression and end plate preparation. I then measured the distance from the inferior endplate of {cranial_level_df$level[[1]]} to the superior endplate of {caudal_level_df$level[[1]]} and selected an appropriately sized implant and inserted the implant into the corpectomy defect. The implant had a good press fit between the endplates. This completed the insertion of the biomechanical implant at the {glue_collapse(levels_nested_df$level, sep = ', ', last = ' and ')}.")) %>%
      select(paragraph) %>%
      distinct()
    
    statement <- paste(statement_df$paragraph[[1]])
    
  }
  
  return(statement)
  
}

###########################################
###########################################

anterior_op_note_distinct_paragraph_function <- function(levels_nested_df){
  
  object_statement_paragraphs_df <- levels_nested_df %>%
    mutate(paragraph = case_when(
      object == "anterior_disc_arthroplasty" ~ glue("I then proceeded with total disk arthroplasty at the {level} interspace.  Using a combination of knife, currette, pituitary ronguer, and Kerrison rongeurs, the disc was excised and the superior and inferior endplates were lightly decorticated to prepare the endplates for fusion, taking care not to disrupt the cortical endplate. The endplates were distracted and the posterior longitudinal ligament was adequately excised, fully decompressing the central canal and thecal sac. To complete the bilateral foraminotomies, I worked laterally on the left and right with a Kerrison rongeur, resecting any residual disk or osteophyte and fully decompressing the left and the right exiting nerve roots. Once I was satisfied with the decompression and end plate preparation, I trialed the implants and selected an appropriately sized disk replacement. The final implant was then inserted into the interspace of {level}. This completed the total disc arthroplasty of the {level} interspace."),
      
      object == "decompression_diskectomy_fusion" ~ glue("I then proceeded with the diskectomy, decompression, and interbody fusion of the {level} interspace. First, I smoothed the anterior disk space and resected any osteophyte using a combination of a ronguer and burr. Using a combination of knife, currette, pituitary ronguer, and Kerrison rongeurs, the anterior longitudinal ligament was incised, the disc was excised and the superior and inferior endplates were lightly decorticated to prepare the endplates for fusion, taking care not to disrupt the cortical endplate. The endplates were distracted and the posterior longitudinal ligament was adequately excised, fully decompressing the central canal. I worked laterally on the left and right with a Kerrison rongeur, resecting any residual disk or osteophyte and fully decompressing the left and the right exiting nerve roots to complete the bilateral foraminotomies. This completed the anterior diskectomy with decompression of the {level} interspace and partially completed the fusion of {level}."),
      
      object == "diskectomy_fusion" ~ glue("I then proceeded with the diskectomy and interbody fusion of the {level} interspace. First, I smoothed the anterior disk space and resected any osteophyte using a combination of a ronguer and burr. Using a combination of knife, currette, pituitary ronguer, and Kerrison rongeurs, the anterior longitudinal ligament was incised, the disc was excised and the superior and inferior endplates were lightly decorticated to prepare the endplates for fusion, taking care not to disrupt the cortical endplate. This completed the anterior diskectomy of {level} interspace and partially completed the fusion of {level}."),
      
      object == "diskectomy_fusion_no_interbody_device" ~ glue("I then proceeded with the diskectomy, decompression, and interbody fusion of the {level} interspace. First, I smoothed the anterior disk space and resected any osteophyte using a combination of a ronguer and burr. Using a combination of knife, currette, pituitary ronguer, and Kerrison rongeurs, the anterior longitudinal ligament was incised and the disc was excised. The endplates were distracted and the and the superior and inferior endplates were lightly decorticated to prepare the endplates for fusion, taking care not to disrupt the cortical endplate. Once I was satisfied with the endplate preparation, bone graft was placed into the disk space. This completed the anterior diskectomy and fusion of the {level} interspace."),
      
      object == "anterior_interbody_implant" ~ glue("I then proceeded with the insertion of the interbody implant into the {level} interspace. I again confirmed that the endplates were adequately decorticated and appropriately level. Once I was fully satisfied with the preparation of the endplates, I used trials and measured the disk space to determine the appropriate size of the interbody implant. {implant_statement} I then inserted the interbody implant into the disk space of {level}. The final position was confirmed using intraoperative xray. This completed the anterior interbody implant at {level}."),
      
      object == "corpectomy" ~ glue("I then proceeded with decompression and anterior vertebral body corpectomy at the {level} vertebral level. I confirmed that the exposure had been carried cranially to visualze the entire {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_interspace} disk, the anterior body of {level} and caudally to the {identify_cranial_caudal_interspace_body_list_function(level = level)$caudal_interspace} disk space. First I started with the diskectomies. Using a combination of a knife, currette, pituitary ronguer, and Kerrison rongeurs, the anterior longitudinal ligament was incised and the {identify_cranial_caudal_interspace_body_list_function(level = level)$cranial_interspace} and {identify_cranial_caudal_interspace_body_list_function(level = level)$caudal_interspace} disc's were completely excised. Once I was satisfied with the diskectomies, I used a combination of a burr and rongeur's to excise roughly 80% of the {level} vertebral body. I carried the corpectomy dorsally to the posterior longitudinal ligament, effectively decompressing the central canal. This completed the vertebral body corpectomy at {level}.")
    )
    )
  
  paragraphs <- glue_collapse(object_statement_paragraphs_df$paragraph, sep = "\n\n")
  
  
  return(paragraphs)
  
}

all_anterior_procedures_paragraphs_function <- function(all_objects_to_add_df){
  
  # object == "disk_arthroplasty" ~ "Total disk arthroplasty",
  # object == "decompression_diskectomy_fusion" ~ "Anterior diskectomy and fusion with decompression of the central canal and nerve roots",
  # object == "diskectomy_fusion" ~ "Anterior diskectomy and fusion",
  # object == "diskectomy_fusion_no_interbody_device" ~ "Anterior diskectomy and fusion with decompression of the central canal",
  # object == "anterior_interbody_implant" ~ "Insertion of interbody biomechanical implant",
  # object == "corpectomy" ~ "Anterior vertebral corpectomy",
  # object == "corpectomy_cage" ~ "Insertion of intervertebral biomechanical implant",
  # object == "anterior_plate" ~ "Anterior spinal instrumentation (distinct from interbody implant)",
  # object == "anterior_buttress_plate" ~ "Anterior spinal instrumentation (distinct from interbody implant)",
  # object == "screw_washer" ~ "Anterior spinal instrumentation (distinct from interbody implant)"
  
  anterior_df <- all_objects_to_add_df %>%
    select(level, vertebral_number, object, side, implant_statement) %>%
    separate(level, into = c("cranial", "caudal"), remove = FALSE) %>%
    mutate(level = if_else(object == "anterior_buttress_plate", caudal, level)) %>%
    mutate(order_number = row_number())
  
  # if(any(anterior_df$object == "decompression_diskectomy_fusion") | any(anterior_df$object == "diskectomy_fusion")){
  #   anterior_df <- anterior_df %>%
  #     filter(object == "decompression_diskectomy_fusion" | object == "diskectomy_fusion") %>%
  #     mutate(object = "anterior_interbody_implant") %>%
  #     mutate(order_number = order_number + 0.5) %>%
  #     union_all(anterior_df) %>%
  #     arrange(order_number) %>%
  #     distinct()
  # }
  
  anterior_df <- anterior_df %>%
    mutate(object = as_factor(object)) %>%
    mutate(object = fct_relevel(object, c("decompression_diskectomy_fusion",
                                          "diskectomy_fusion",
                                          "diskectomy_fusion_no_interbody_device",
                                          "anterior_disc_arthroplasty",
                                          "corpectomy",
                                          "anterior_interbody_implant", 
                                          "corpectomy_cage",
                                          "screw_washer",
                                          "anterior_buttress_plate",
                                          "anterior_plate"))) %>%
    arrange(object)
  
  anterior_procedure_category_nested_df <- anterior_df %>%
    mutate(procedure_category = op_note_procedure_category_function(object = object)) %>%
    mutate(paragraphs_combine_or_distinct = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_category)) %>%
    select(level, vertebral_number , procedure_category, object, side, paragraphs_combine_or_distinct, implant_statement) %>%
    group_by(procedure_category) %>%
    nest() %>%
    mutate(paragraphs_combine_or_distinct = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_category)) 
  
  
  paragraphs_df <- anterior_procedure_category_nested_df %>%
    mutate(paragraphs = pmap(.l = list(..1 = procedure_category, 
                                       ..2 = data,
                                       ..3 = paragraphs_combine_or_distinct), 
                             .f = ~ anterior_create_full_paragraph_statement_function(procedure_paragraph_intro = ..1, df_with_levels_object_nested = ..2, paragraphs_combined_or_distinct = ..3)))
  
  procedure_paragraphs <- glue_collapse(x = paragraphs_df$paragraphs, sep = "\n\n")
  
  return(procedure_paragraphs)
  
}











################### FULL OPERATIVE NOTE BODY GENERATOR #######################

op_note_anterior_function <- function(all_objects_to_add_df,
                                       anterior_approach_laterality,
                                       acdf_interbody_height = 8, 
                                       interbody_device = "cage",
                                       microscope_statement = "none",
                                       antibiotics = vector(),
                                       additional_procedures_vector = NULL,
                                       bmp = NULL,
                                       bone_graft_vector = NULL,
                                       morselized_allograft = 0,
                                       morselized_autograft_separate = 0,
                                       additional_procedural_details = NULL, 
                                       deep_drains = 1, 
                                       superficial_drains = 0,
                                       end_procedure_details = NULL,
                                       closure = NULL,
                                       dressing = NULL){
  
  procedure_details_list <- list()
  
  procedures_numbered_list <- list()
  
  additional_procedures_for_numbered_list <- c(as.character(glue("Application of {glue_collapse(bone_graft_vector, sep = ', ', last = ', and ')}")))
  
  additional_procedures_for_numbered_list <- append(additional_procedures_for_numbered_list, additional_procedures_vector)
  
  procedures_numbered_list$primary_procedures <- anterior_op_note_procedures_performed_numbered_function(objects_added_df = all_objects_to_add_df, 
                                                          additional_procedures_performed_vector = additional_procedures_for_numbered_list)
  
  
  first_paragraph_list <- list()
  
  first_paragraph_list$transport_anesthesia <- paste("The patient was brought to the operating room and after obtaining appropriate IV access, the patient underwent general anesthesia.")
  
  if(length(antibiotics) == 0){
    first_paragraph_list$antibiotic_statement <- "Preoperative Antibiotics were administered."
  }else{
    if(antibiotics == "None (Antibiotics were held)"){
      first_paragraph_list$antibiotic_statement <- "Preoperative antibiotics were held until tissue cultures could be obtained."
    }else{
      first_paragraph_list$antibiotic_statement <- paste("Preoperative Antibiotics were administered including ", glue_collapse(antibiotics, sep = ", ", last = " and "), ".")
    }
  }
  
  if(any(str_detect(additional_procedures_vector, "Halo"))){
    first_paragraph_list$head_statement <- "A Halo was applied to the patient's skull for positioning and the pins were sequentially tightened to the appropriate torque."
  }
  if(any(str_detect(additional_procedures_vector, "Tongs"))){
    first_paragraph_list$head_statement <- "Cranial tongs were applied to the patient's skull for positioning and an appropriate weight was selected for cranial traction."
  }

  
  if(any(str_detect(additional_procedures_vector, "Spinal Cord Monitoring"))){
    first_paragraph_list$spinal_cord_monitoring <- "Spinal Cord Monitoring needles were inserted by the neurophysiology technologist."
  }
  
  first_paragraph_list$positioning <- paste("The patient was then positioned Supine on the OR table and all bony prominences were appropriately padded.",
                                            "After prepping and draping in the standard fashion, a surgical timeout was performed.")
  
  proximal_exposure_level <- all_objects_to_add_df %>%
    filter(vertebral_number == min(vertebral_number)) %>%
    mutate(vertebral_number = round(vertebral_number - 0.25, 0)) %>%
    mutate(level = if_else(vertebral_number >=25, "S1", level)) %>%
    select(vertebral_number) %>%
    distinct() %>%
    left_join(levels_numbered_df) %>%
    select(level) %>%
    distinct()
  
  distal_exposure_level <- all_objects_to_add_df %>%
    filter(vertebral_number == max(vertebral_number)) %>%
    mutate(vertebral_number = round(vertebral_number + 0.25, 0)) %>%
    mutate(level = if_else(vertebral_number >=25, "S1", level)) %>%
    select(vertebral_number) %>%
    distinct() %>%
    left_join(levels_numbered_df) %>%
    select(level) %>%
    distinct() %>%
    mutate(level = if_else(level == "S2AI", "S1", 
                           if_else(level == "Iliac", "S1", 
                                   level)))
  
  ## Approach for cervical vs thoracic/lumbar
  if(max(all_objects_to_add_df$vertebral_number)<11){
    microscope_statement <- if_else(microscope_statement == "none", "", microscope_statement)
    
    first_paragraph_list$surgical_approach <- paste(glue("A standard {anterior_approach_laterality}-sided Smith Robinson approach was utilized to get to the anterior cervical spine. The skin, subcutaneous tissue were incised, the platysma was transected, and then blunt dissection was carried out between the sternocleidomastoid and carotid sheath laterally, and trachea and esophogus medially, down to the prevertebral fascia. Once the anterior spine was palpated, fluoroscopy was used to localize and confirm levels. "), 
                                                   glue("The longus coli was elevated bilaterally from {proximal_exposure_level$level[[1]]} proximally and to {distal_exposure_level$level[[1]]} distally."),
                                                   microscope_statement)
  }else{
    first_paragraph_list$surgical_approach <- glue("The anterior approach to the spine was carried out with assistance from our vascular surgeon. A {anterior_approach_laterality} incision was made on and the approach was carried out down toward the spine. Once the approach was complete, levels were confirmed using fluoroscopy. ")
  }
  
  procedure_details_list$approach_statement <- glue_collapse(x = first_paragraph_list, sep = " ")
  
  ################### PROCEDURE PARAGRAPHS ##################
  
  procedure_details_list$procedures <- all_anterior_procedures_paragraphs_function(all_objects_to_add_df = all_objects_to_add_df)
  
  
  
 
  
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
  
  closure_statements_list$layered_closure <- "The wound was then closed in a layered fashion."
  
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
  
  
  closure_statements_list$dressing <- glue("Lastly, {glue_collapse(dressing, sep = ', ', last = ', and ')} was applied to the surgical site.")
  
  
  procedure_details_list$closure <- glue_collapse(closure_statements_list, sep = " ")
  
  procedure_details_list <- map(.x = procedure_details_list, .f = ~str_replace_all(string = .x, pattern = "a 8", replacement = "an 8"))
  
  ### FINAL PARAGRAPH ####
  
  
  
  procedures_listed <- op_note_procedures_present_listed_function(objects_added_df = all_objects_to_add_df,
                                                                  # revision_decompression_vector = revision_decompression_vector,
                                                                  additional_procedures_performed_vector = additional_procedures_for_numbered_list)

  procedure_details_list$final_paragraph <- glue("At the conclusion of the case, all counts were correct. The drapes were removed and the patient was transferred to the hospital bed, and awoke uneventfully. I was personally present for the entirety of the case, including {procedures_listed}.")

  
  procedure_paragraphs <- glue_collapse(x = procedure_details_list, sep = "\n\n")
  
  return(list(procedure_details_paragraph = procedure_paragraphs, 
              procedures_numbered_paragraph = procedures_numbered_list[[1]]))
  
}


##################################### ##################################### ##################################### 
##################################### PROCEDURES PERFORMED #####################################  ##################################### 
##################################### ##################################### ##################################### 

op_note_procedure_performed_summary_classifier_function <- function(object){
  procedure_category <- case_when(
    object == "cement_augmentation" ~ "Vertebral cement augmentation",
    object == "laminar_downgoing_hook" ~ "Posterior spinal instrumentation",
    object == "laminar_upgoing_hook" ~ "Posterior spinal instrumentation",
    object == "lateral_mass_screw" ~ "Posterior spinal instrumentation",
    object == "occipital_screw" ~ "Occiput instrumentation",
    object == "pars_screw" ~ "Posterior spinal instrumentation",
    object == "pedicle_hook" ~ "Posterior spinal instrumentation",
    object == "pedicle_screw" ~ "Posterior spinal instrumentation",
    object == "laminar_downgoing_hook" ~ "Posterior spinal instrumentation",
    object == "sublaminar_wire" ~ "Posterior spinal instrumentation",
    object == "tp_hook" ~ "Posterior spinal instrumentation",
    object == "translaminar_screw" ~ "Posterior spinal instrumentation",
    object == "pelvic_screw" ~ "Pelvic instrumentation",
    object == "tether" ~ "Spinous process posterior tethering/wiring",
    object == "costovertebral_approach" ~ "Decompression using a costovertebral approach",
    object == "revision_costovertebral_approach" ~ "Reexploration and revision decompression using a costovertebral approach",
    object == "transpedicular_approach" ~ "Decompression using a transpedicular approach",
    object == "revision_transpedicular_approach" ~ "Reexploration and revision decompression using a transpedicular approach",
    object == "diskectomy" ~ "Decompression with diskectomy and laminotomy",
    object == "sublaminar_decompression" ~ "Decompression with bilateral partial laminectomies, foraminotomies, and medial facetectomies",
    object == "laminectomy" ~ "Decompression with central laminectomy",
    object == "laminotomy" ~  "Decompression with laminotomy and medial facetectomy",
    object == "revision_diskectomy" ~ "Reexploration and revision decompression with diskectomy and laminotomy",
    object == "revision_sublaminar_decompression" ~ "Reexploration and revision decompression with bilateral partial laminectomies, foraminotomies, and medial facetectomies",
    object == "revision_laminectomy" ~ "Reexploration and revision decompression with central laminectomy",
    object == "revision_laminotomy" ~ "Reexploration and revision decompression with laminotomy and medial facetectomy",
    object == "laminoplasty" ~ "Laminoplasty",
    object == "grade_1" ~ "Inferior facetectomies",
    object == "complete_facetectomy" ~ "Complete facetectomy",
    object == "grade_2" ~ "Posterior column osteotomy",
    object == "grade_3" ~ "Pedicle subtraction osteotomy",
    object == "grade_4" ~ "Extended three column osteotomy (vertebral body partial corpectomy)",
    object == "grade_5" | object == "grade_6" ~ "Vertebral column resection",
    object == "costotransversectomy" ~ "Costovertebral approach with costotransversectomy",
    object == "no_implant_interbody_fusion" ~ "Interbody fusion (without interbody implant)",
    object == "llif" ~ "Lateral lumbar interbody fusion and insertion of interbody device",
    object == "plif" ~ "Posterior lumbar interbody fusion and insertion of interbody device",
    object == "tlif" ~ "Transforaminal lumbar interbody fusion and insertion of interbody device",
    object == "intervertebral_cage" ~ "Insertion of intervertebral biomechanical device",
    ## ANTERIOR
    object == "anterior_disc_arthroplasty" ~ "Total disk arthroplasty",
    object == "decompression_diskectomy_fusion" ~ "Anterior diskectomy and fusion with decompression of the central canal and nerve roots",
    object == "diskectomy_fusion" ~ "Anterior diskectomy and fusion",
    object == "diskectomy_fusion_no_interbody_device" ~ "Anterior diskectomy and fusion with decompression of the central canal",
    object == "anterior_interbody_implant" ~ "Insertion of interbody biomechanical implant",
    object == "corpectomy" ~ "Anterior vertebral corpectomy",
    object == "corpectomy_cage" ~ "Insertion of intervertebral biomechanical implant",
    object == "anterior_plate" ~ "Anterior spinal instrumentation (distinct from interbody implant)",
    object == "anterior_buttress_plate" ~ "Anterior spinal instrumentation (distinct from interbody implant)",
    object == "screw_washer" ~ "Anterior spinal instrumentation (distinct from interbody implant)"
  )
  procedure_category
}

op_note_procedure_performed_summary_per_line_function <- function(procedure_type){
  procedures_per_line <- case_when(
    procedure_type == 'Vertebral cement augmentation' ~ 'multiple',
    procedure_type == 'Posterior spinal instrumentation' ~ 'multiple',
    procedure_type == 'Occiput instrumentation' ~ 'multiple',
    procedure_type == 'Pelvic instrumentation' ~ 'multiple',
    procedure_type == 'Spinous process posterior tethering/wiring' ~ 'multiple',
    procedure_type == 'Complete facetectomy' ~ 'multiple',
    procedure_type == 'Inferior facetectomies' ~ 'multiple',
    procedure_type == 'Posterior column osteotomy' ~ 'multiple',
    procedure_type == 'Pedicle subtraction osteotomy' ~ 'one',
    procedure_type == 'Extended three column osteotomy (vertebral body partial corpectomy)' ~ 'one',
    procedure_type == 'Vertebral column resection' ~ 'one',
    procedure_type == 'Costovertebral approach with costotransversectomy' ~ 'multiple',
    procedure_type == 'Decompression with diskectomy and laminotomy' ~ 'multiple',
    procedure_type == 'Decompression with laminotomy and medial facetectomy' ~ 'multiple',
    procedure_type == 'Decompression with central laminectomy' ~ 'multiple',
    procedure_type == 'Decompression with bilateral partial laminectomies, foraminotomies, and medial facetectomies' ~ 'multiple',
    procedure_type == "Reexploration and revision decompression with diskectomy and laminotomy" ~ 'multiple',
    procedure_type == "Reexploration and revision decompression with bilateral partial laminectomies, foraminotomies, and medial facetectomies" ~ 'multiple',
    procedure_type == "Reexploration and revision decompression with central laminectomy" ~ 'multiple',
    procedure_type == "Reexploration and revision decompression with laminotomy and medial facetectomy"~ 'multiple',
    procedure_type == 'Laminoplasty' ~ 'multiple',
    procedure_type == 'Decompression using a transpedicular approach' ~ 'one',
    procedure_type == 'Decompression using a costovertebral approach' ~ 'one',
    procedure_type == 'Reexploration and revision decompression using a transpedicular approach' ~ 'one',
    procedure_type == 'Reexploration and revision decompression using a costovertebral approach' ~ 'one',
    procedure_type == 'Interbody fusion (without interbody implant)' ~ 'one',
    procedure_type == 'Lateral lumbar interbody fusion and insertion of interbody device' ~ 'one',
    procedure_type == 'Transforaminal lumbar interbody fusion and insertion of interbody device' ~ 'one',
    procedure_type == 'Interbody fusion (without interbody implant)' ~ 'one',
    procedure_type == 'Posterior lumbar interbody fusion and insertion of interbody device' ~ 'one',
    ## Anterior ##
    procedure_type ==  "Total disk arthroplasty"~ 'one',
    procedure_type == "Anterior diskectomy and fusion with decompression of the central canal and nerve roots"~ 'one',
    procedure_type == "Anterior diskectomy and fusion"~ 'one',
    procedure_type == "Anterior diskectomy and fusion with decompression of the central canal"~ 'one',
    procedure_type == "Insertion of interbody biomechanical implant"~ 'one',
    procedure_type == "Anterior vertebral corpectomy"~ 'one',
    procedure_type == "Insertion of intervertebral biomechanical implant"~ 'multiple',
    procedure_type == "Anterior spinal instrumentation (distinct from interbody implant)"~ 'multiple',
    procedure_type == "Anterior spinal instrumentation (distinct from interbody implant)"~ 'multiple',
    procedure_type == "Anterior spinal instrumentation (distinct from interbody implant)" ~ 'multiple',
    procedure_type == "Insertion of intervertebral biomechanical device" ~ 'one'
    
  )
  procedures_per_line
}

extract_levels_function <- function(input_df){
  levels_df <- input_df %>%
    arrange(vertebral_number) %>%
    select(level) %>%
    distinct()
  
  glue_collapse(x = levels_df$level, sep = ", ", last = " and ")
  
}


anterior_op_note_procedures_performed_numbered_function <- function(objects_added_df,
                                                           revision_decompression_vector = NULL,
                                                           fusion_levels_vector = NULL,
                                                           additional_procedures_performed_vector = NULL){
  
  plate_levels_df <- objects_added_df %>%
    filter(str_detect(string = object, pattern = "anterior_plate")) %>%
    mutate(level = map(.x = level, .f = ~ identify_cranial_caudal_interspace_body_list_function(level = .x)$cranial_level)) %>%
    union_all(objects_added_df %>%
                filter(str_detect(string = object, pattern = "anterior_plate")) %>%
                mutate(level = map(.x = level, .f = ~ identify_cranial_caudal_interspace_body_list_function(level = .x)$caudal_level))) %>%
    unnest(level) %>%
    select(-vertebral_number) %>%
    left_join(levels_numbered_df) %>%
    select(level,vertebral_number, approach, category, object, side) %>%
    distinct() %>%
    arrange(vertebral_number)
  
  objects_added_df <- objects_added_df %>%
    filter(str_detect(string = object, pattern = "anterior_plate") == FALSE) %>%
    union_all(plate_levels_df)
  
  if(length(revision_decompression_vector) > 0){
    
    summary_nested_df <- objects_added_df %>%
      mutate(revision_levels_vector = list(revision_decompression_vector)) %>%
      select(level, vertebral_number, approach, category, object, side, revision_levels_vector) %>%
      mutate(revision_level = map2(.x = level, .y = revision_levels_vector, .f = ~ str_detect(string = .x, pattern = .y))) %>%
      select(-revision_levels_vector) %>%
      mutate(revision_level = map(.x = revision_level, .f = ~ any(.x))) %>%
      unnest() %>%
      mutate(object = if_else(category == "decompression" & revision_level == TRUE, paste0("revision_", object), object)) %>%
      select(level, object, vertebral_number) %>%
      mutate(procedure_class = op_note_procedure_performed_summary_classifier_function(object = object)) %>%
      mutate(procedures_per_line = op_note_procedure_performed_summary_per_line_function(procedure_type = procedure_class)) 
  }else{
    summary_nested_df <- objects_added_df %>%
      select(level, object, vertebral_number) %>%
      mutate(procedure_class = op_note_procedure_performed_summary_classifier_function(object = object)) %>%
      mutate(procedures_per_line = op_note_procedure_performed_summary_per_line_function(procedure_type = procedure_class)) 
  }
  
  
  summary_single_statements <- summary_nested_df %>%
    filter(procedures_per_line == "one") %>%
    mutate(procedure_performed_statement = glue("{procedure_class} at {level}"))%>%
    select(procedure_class, procedure_performed_statement)
  
  summary_multiple_nested <- summary_nested_df %>%
    filter(procedures_per_line == "multiple") %>%
    group_by(procedure_class) %>%
    nest() %>%
    ungroup() %>%
    mutate(count = row_number()) %>%
    mutate(levels = map(.x = data, .f = ~ extract_levels_function(input_df = .x))) %>%
    select(-data) %>%
    unnest() %>%
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
