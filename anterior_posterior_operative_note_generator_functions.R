################-------------------------#################### -------- ANTERIOR & POSTERIOR FUNCTIONS  -----------################-------------------------#################### 
################-------------------------#################### -------- ANTERIOR & POSTERIOR FUNCTIONS  -----------################-------------------------#################### 
################-------------------------#################### -------- ANTERIOR & POSTERIOR FUNCTIONS  -----------################-------------------------#################### 

op_note_procedure_performed_summary_classifier_function <- function(object){
  procedure_category <- case_when(
    object == "incision_drainage" ~ "Incision and drainage",
    object == "vertebroplasty" ~ "Vertebroplasty",
    object == "vertebral_cement_augmentation" ~ "Vertebral body augmentation",
    object == "laminar_downgoing_hook" ~ "Posterior spinal instrumentation",
    object == "laminar_upgoing_hook" ~ "Posterior spinal instrumentation",
    object == "lateral_mass_screw" ~ "Posterior spinal instrumentation",
    object == "occipital_screw" ~ "Occiput instrumentation",
    object == "pars_screw" ~ "Posterior spinal instrumentation",
    object == "pedicle_hook" ~ "Posterior spinal instrumentation",
    object == "pedicle_screw" ~ "Posterior spinal instrumentation",
    object == "crosslink" ~ "Posterior spinal instrumentation",
    object == "laminar_downgoing_hook" ~ "Posterior spinal instrumentation",
    object == "sublaminar_wire" ~ "Posterior spinal instrumentation",
    object == "tp_hook" ~ "Posterior spinal instrumentation",
    object == "translaminar_screw" ~ "Posterior spinal instrumentation",
    object == "pelvic_screw" ~ "Pelvic instrumentation",
    object == "pelvic_screw_1" ~ "Pelvic instrumentation",
    object == "pelvic_screw_2" ~ "Pelvic instrumentation",
    object == "tether" ~ "Spinous process posterior tethering/wiring",
    object == "costovertebral_approach" ~ "Decompression using a costovertebral approach",
    object == "revision_costovertebral_approach" ~ "Reexploration and revision decompression using a costovertebral approach",
    object == "transpedicular_approach" ~ "Decompression using a transpedicular approach",
    object == "lateral_extraforaminal_approach" ~ "Decompression using a lateral extraforaminal approach",
    object == "lateral_extracavitary_approach" ~ "Arthrodesis using a modified lateral extracavitary approach",
    object == "corpectomy_extracavitary_tumor" ~ "Partial Corpectomy with decompression using a modified lateral extracavitary approach for tumor",
    object == "laminectomy_for_tumor" ~ "Laminectomy for biopsy and excision of extradural spinal tumor",
    object == "laminectomy_for_facet_cyst" ~ "Laminectomy for excision of facet cyst (instraspinal lesion, not neoplasm)",
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
    object == "complete_facetectomy" ~ "Decompression with a complete facetectomy",
    object == "grade_2" ~ "Posterior column osteotomy",
    object == "grade_3" ~ "Pedicle subtraction osteotomy",
    object == "grade_4" ~ "Extended three column osteotomy (vertebral body partial corpectomy)",
    object == "grade_5" ~ "Vertebral column resection",
    object == "costotransversectomy" ~ "Costovertebral approach with costotransversectomy",
    object == "no_implant_interbody_fusion" ~ "Interbody fusion (without interbody implant)",
    object == "llif" ~ "Lateral lumbar interbody fusion and insertion of interbody device",
    object == "plif" ~ "Posterior lumbar interbody fusion and insertion of interbody device",
    object == "tlif" ~ "Transforaminal lumbar interbody fusion and insertion of interbody device",
    object == "intervertebral_cage" ~ "Insertion of intervertebral biomechanical implant",
    object == "structural_allograft" ~ "Application of structural allograft",
    ## ANTERIOR
    object == "anterior_disc_arthroplasty" ~ "Total disk arthroplasty",
    object == "decompression_diskectomy_fusion" ~ "Anterior diskectomy and fusion with decompression of the central canal and nerve roots",
    object == "diskectomy_fusion" ~ "Anterior diskectomy and fusion",
    object == "diskectomy_fusion_no_interbody_device" ~ "Anterior diskectomy and fusion",
    object == "anterior_interbody_implant" ~ "Insertion of interbody biomechanical implant",
    object == "corpectomy" ~ "Anterior vertebral corpectomy",
    object == "corpectomy_cage" ~ "Anterior insertion of intervertebral biomechanical implant",
    object == "anterior_plate" ~ "Anterior spinal instrumentation (distinct from an interbody implant)",
    object == "anterior_buttress_plate" ~ "Anterior spinal instrumentation (distinct from an interbody implant)",
    object == "screw_washer" ~ "Anterior spinal instrumentation (distinct from an interbody implant)"
  )
  procedure_category
}

op_note_number_of_paragraphs_for_procedure_category <- function(procedure_cat){
  
  procedure_cat <- str_to_lower(procedure_cat)
  
  paragraph_type <- case_when(
    procedure_cat == "incision and drainage" ~ "combine",
    procedure_cat == "vertebroplasty" ~ "combine",
    procedure_cat == "vertebral body augmentation" ~ "combine",
    procedure_cat == "posterior spinal instrumentation" ~ "combine",
    procedure_cat == "occiput instrumentation" ~ "combine",
    procedure_cat == "pelvic instrumentation" ~ "combine",
    procedure_cat == "spinous process posterior tethering/wiring" ~ "combine",
    procedure_cat == "decompression using a costovertebral approach" ~ "distinct",
    procedure_cat == "reexploration with revision decompression using a costovertebral approach" ~ "distinct",
    procedure_cat == "decompression using a transpedicular approach" ~ "distinct",
    procedure_cat == "decompression using a lateral extraforaminal approach" ~ "distinct",
    procedure_cat == "reexploration with revision decompression using a transpedicular approach" ~ "distinct",
    procedure_cat == "arthrodesis using a modified lateral extracavitary approach" ~ "distinct",
    procedure_cat == "partial corpectomy with decompression using a modified lateral extracavitary approach for tumor" ~ "distinct",
    procedure_cat == "laminectomy for biopsy and excision of extradural spinal tumor" ~ "combine",
    procedure_cat == "laminectomy for excision of facet cyst (instraspinal lesion, not neoplasm)" ~ "combine",
    procedure_cat == "decompression" ~ "combine",
    procedure_cat == "reexploration with revision decompression" ~ "combine",
    procedure_cat == 'decompression with diskectomy and laminotomy' ~ 'combine',
    procedure_cat == 'decompression with laminotomy and medial facetectomy' ~ 'combine',
    procedure_cat == 'decompression with central laminectomy' ~ 'combine',
    procedure_cat == 'decompression with bilateral partial laminectomies, foraminotomies, and medial facetectomies' ~ 'combine',
    procedure_cat == "reexploration and revision decompression with diskectomy and laminotomy" ~ 'combine',
    procedure_cat == "reexploration and revision decompression with bilateral partial laminectomies, foraminotomies, and medial facetectomies" ~ 'combine',
    procedure_cat == "reexploration and revision decompression with central laminectomy" ~ 'combine',
    procedure_cat == "reexploration and revision decompression with laminotomy and medial facetectomy"~ 'combine',
    procedure_cat == "laminoplasty" ~ "combine",
    procedure_cat == "inferior facetectomies" ~ "combine",
    procedure_cat == "decompression with a complete facetectomy" ~ "combine",
    procedure_cat == "posterior column osteotomy" ~ "combine",
    procedure_cat == "pedicle subtraction osteotomy" ~ "distinct",
    procedure_cat == "extended three column osteotomy (vertebral body partial corpectomy)" ~ "distinct",
    procedure_cat == "vertebral column resection" ~ "distinct",
    procedure_cat == "costovertebral approach with costotransversectomy" ~ "combine",
    procedure_cat == "interbody fusion (without interbody implant)" ~ "distinct",
    procedure_cat == "lateral lumbar interbody fusion and insertion of interbody device" ~ "distinct",
    procedure_cat == "posterior lumbar interbody fusion and insertion of interbody device" ~ "distinct",
    procedure_cat == "transforaminal lumbar interbody fusion and insertion of interbody device" ~ "distinct",
    procedure_cat == "insertion of intervertebral biomechanical implant" ~ "distinct",
    procedure_cat == "application of structural allograft" ~ "combine",
    #anterior#
    procedure_cat == "total disk arthroplasty" ~ "distinct",
    procedure_cat == "anterior diskectomy and fusion with decompression of the central canal and nerve roots" ~ "distinct",
    procedure_cat == "anterior diskectomy and fusion" ~ "distinct",
    procedure_cat == "insertion of interbody biomechanical implant" ~ "distinct",
    procedure_cat == "anterior vertebral corpectomy" ~ "combine",
    procedure_cat == "anterior insertion of intervertebral biomechanical implant" ~ "combine",
    procedure_cat == "anterior spinal instrumentation (distinct from an interbody implant)" ~ "combine"
  )
  
  paragraph_type
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
    
    statement <- glue("I placed an anterior plate spanning {glue_collapse(levels_df$level, sep = ', ', last = ' and ')}. After selecting an appropriately sized plate and length for the screws, I held the plate into position and drilled and tapped the path for the screws. The screws were then inserted sequentially into the vertebral body of {glue_collapse(levels_df$level, sep = ', ', last = ' and ')} to hold the plate into position.")
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
    
    cranial_level_df <- levels_nested_df %>%
      filter(vertebral_number == min(vertebral_number)) %>%
      mutate(level = jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level) %>%
      select(level) %>%
      distinct()
    
    caudal_level_df <- levels_nested_df %>%
      filter(vertebral_number == max(vertebral_number)) %>%
      mutate(level = jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level) %>%
      select(level) %>%
      distinct()

    
    statement_df <- levels_nested_df %>%
      mutate(paragraph = glue("I confirmed that the exposure had been carried cranially to visualze the entire {jh_get_cranial_caudal_interspace_body_list_function(level = cranial_level_df$level[[1]])$caudal_interspace} disk, the anterior body of {glue_collapse(levels_df$level, sep = ', ', last = ' and ')}, and caudally to the {jh_get_cranial_caudal_interspace_body_list_function(level = caudal_level_df$level[[1]])$cranial_interspace} disk space. I then started with the diskectomies. Using a combination of a knife, currette, pituitary ronguer, and Kerrison rongeurs, the anterior longitudinal ligament was incised and the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_interspace} to {jh_get_cranial_caudal_interspace_body_list_function(level = caudal_level_df$level[[1]])$caudal_interspace} disc's were completely excised. Once I was satisfied with the diskectomies, I used a combination of a burr and rongeur's to excise roughly 80% of the {glue_collapse(levels_df$level, sep = ', ', last = ' and ')} vertebral body. I carried the corpectomy laterally to the edge of the uncus and dorsally to the posterior longitudinal ligament, effectively decompressing the central canal.")) %>%
      select(paragraph) %>%
      distinct()
    
    statement <- paste(statement_df$paragraph[[1]])
    
  }
  
  if(object == "corpectomy_cage"){
    cranial_level_df <- levels_nested_df %>%
      filter(vertebral_number == min(vertebral_number)) %>%
      mutate(level = jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level) %>%
      select(level) %>%
      distinct()
    
    caudal_level_df <- levels_nested_df %>%
      filter(vertebral_number == max(vertebral_number)) %>%
      mutate(level = jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level) %>%
      select(level) %>%
      distinct()
    
    statement_df <- levels_nested_df %>%
      mutate(paragraph = glue("I confirmed satisfactory decompression and end plate preparation. I then measured the distance from the inferior endplate of {cranial_level_df$level[[1]]} to the superior endplate of {caudal_level_df$level[[1]]} and selected an appropriately sized implant and inserted the implant into the corpectomy defect. The implant had a good press fit between the endplates.")) %>%
      select(paragraph) %>%
      distinct()
    
    statement <- paste(statement_df$paragraph[[1]])
    
  }
  
  return(statement)
  
}
#############-----------------------               End              ----------------------###############


#############-----------------------   Building Paragraphs: Distinct Operations ----------------------###############

anterior_op_note_distinct_paragraph_function <- function(levels_nested_df){
  
  object_statement_paragraphs_df <- levels_nested_df %>%
    mutate(paragraph = case_when(
      object == "anterior_disc_arthroplasty" ~ glue("I then proceeded with total disk arthroplasty at the {level} interspace.  Using a combination of knife, currette, pituitary ronguer, and Kerrison rongeurs, the disc was excised and the superior and inferior endplates were lightly decorticated to prepare the endplates for fusion, taking care not to disrupt the cortical endplate. The endplates were distracted and the posterior longitudinal ligament was adequately excised, fully decompressing the central canal and thecal sac. To complete the bilateral foraminotomies, I worked laterally on the left and right with a Kerrison rongeur, resecting any residual disk or osteophyte and fully decompressing the left and the right exiting nerve roots. Once I was satisfied with the decompression and end plate preparation, I trialed the implants and selected an appropriately sized disk replacement. The final implant was then inserted into the interspace of {level}. This completed the total disc arthroplasty of the {level} interspace."),
      
      object == "decompression_diskectomy_fusion" ~ glue("I then proceeded with the diskectomy, decompression, and interbody fusion of the {level} interspace. First, I smoothed the anterior disk space and resected any osteophyte using a combination of a ronguer and burr. Using a combination of knife, currette, pituitary ronguer, and Kerrison rongeurs, the anterior longitudinal ligament was incised, the disc was excised and the superior and inferior endplates were lightly decorticated to prepare the endplates for fusion, taking care not to disrupt the cortical endplate. The endplates were distracted and the posterior longitudinal ligament was adequately excised, fully decompressing the central canal. I worked laterally on the left and right with a Kerrison rongeur, resecting any residual disk or osteophyte and fully decompressing the left and the right exiting nerve roots to complete the bilateral foraminotomies. This completed the anterior diskectomy with decompression of the {level} interspace and partially completed the fusion of {level}."),
      
      object == "diskectomy_fusion" ~ glue("I then proceeded with the diskectomy and interbody fusion of the {level} interspace. First, I smoothed the anterior disk space and resected any osteophyte using a combination of a ronguer and burr. Using a combination of knife, currette, pituitary ronguer, and Kerrison rongeurs, the anterior longitudinal ligament was incised, the disc was excised and the superior and inferior endplates were lightly decorticated to prepare the endplates for fusion, taking care not to disrupt the cortical endplate. This completed the anterior diskectomy of {level} interspace and partially completed the fusion of {level}."),
      
      object == "diskectomy_fusion_no_interbody_device" ~ glue("I then proceeded with the diskectomy and interbody fusion of the {level} interspace. First, I smoothed the anterior disk space and resected any osteophyte using a combination of a ronguer and burr. Using a combination of knife, currette, pituitary ronguer, and Kerrison rongeurs, the anterior longitudinal ligament was incised and the disc was excised. The endplates were distracted and the and the superior and inferior endplates were lightly decorticated to prepare the endplates for fusion, taking care not to disrupt the cortical endplate. Once I was satisfied with the endplate preparation, bone graft was placed into the disk space. This completed the anterior diskectomy and interbody fusion of the {level} interspace."),
      
      object == "anterior_interbody_implant" ~ glue("I then proceeded with the insertion of the interbody implant into the {level} interspace. I again confirmed that the endplates were adequately decorticated and appropriately level. Once I was fully satisfied with the preparation of the endplates, I used trials and measured the disk space to determine the appropriate size of the interbody implant. {implant_statement} I then inserted the interbody implant into the disk space of {level}. The final position was confirmed using intraoperative xray. This completed the anterior interbody implant at {level}."),
      
      object == "corpectomy" ~ glue("I then proceeded with decompression and anterior vertebral body corpectomy at the {level} vertebral level. I confirmed that the exposure had been carried cranially to visualze the entire {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_interspace} disk, the anterior body of {level} and caudally to the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_interspace} disk space. First I started with the diskectomies. Using a combination of a knife, currette, pituitary ronguer, and Kerrison rongeurs, the anterior longitudinal ligament was incised and the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_interspace} and {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_interspace} disc's were completely excised. Once I was satisfied with the diskectomies, I used a combination of a burr and rongeur's to excise roughly 80% of the {level} vertebral body. I carried the corpectomy dorsally to the posterior longitudinal ligament, effectively decompressing the central canal. This completed the vertebral body corpectomy at {level}."),
      
      object == "corpectomy_cage" ~ glue("I then proceeded with the insertion of the intervertebral implant into the {level} interspace. I again confirmed that the endplates were adequately decorticated and appropriately level. Once I was fully satisfied with the preparation of the endplates, I used trials and measured the interspace to determine the appropriate size of the interbody implant. {implant_statement} I then inserted the  implant into the {level} space. The final position was confirmed using intraoperative xray. This completed the insertion of the intervertebral implant at {level}."),
      
    )
    )
  
  paragraphs <- glue_collapse(object_statement_paragraphs_df$paragraph, sep = "\n\n")
  
  return(paragraphs)
}
#############-----------------------               End              ----------------------###############


#############-----------------------   Paragraphs: Generate FULL Paragraphs  ----------------------###############

anterior_create_full_paragraph_statement_function <- function(procedure_paragraph_intro, df_with_levels_object_nested, paragraphs_combined_or_distinct){
  if(paragraphs_combined_or_distinct == "combine"){
    
    if(procedure_paragraph_intro == "anterior spinal instrumentation (distinct from an interbody implant)"){
      
      levels_df <- df_with_levels_object_nested %>%
        # mutate(level = if_else(object == "anterior_buttress_plate", jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level, level)) %>%
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
    
    start_statement <- glue("I then proceeded with {procedure_paragraph_intro} at {glue_collapse(levels_df$level, sep = (', '), last = ' and ')}.")
    
    df_with_statement <- df_with_levels_object_nested %>%
      group_by(object) %>%
      nest() %>%
      mutate(tech_statement = map(.x = data, .f = ~ op_note_object_combine_paragraph_function(object = object, levels_nested_df = .x))) %>%
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

all_anterior_procedures_paragraphs_function <- function(all_objects_to_add_df){
  
  anterior_df <- all_objects_to_add_df %>%
    select(level, vertebral_number, object, side, implant_statement) %>%
    # separate(level, into = c("cranial", "caudal"), remove = FALSE) %>%
    # mutate(level = if_else(object == "anterior_buttress_plate", caudal, level)) %>%
    mutate(order_number = row_number())
  
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
    mutate(procedure_category = str_to_lower(op_note_procedure_performed_summary_classifier_function(object = object))) %>%
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
#############-----------------------               End              ----------------------###############

#############-----------------------  FULL ANTERIOR OPERATIVE NOTE BODY GENERATOR ----------------------###############

op_note_anterior_function <- function(all_objects_to_add_df,
                                      anterior_approach_laterality,
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
                                      dressing = NULL,
                                      multiple_position_procedure = FALSE){
  
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
      first_paragraph_list$antibiotic_statement <- paste0("Preoperative Antibiotics were administered including ", glue_collapse(antibiotics, sep = ", ", last = " and "), ".")
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
    microscope_statement <- if_else(microscope_statement == "none", "", microscope_statement)
    
    first_paragraph_list$surgical_approach <- paste(glue("A standard {anterior_approach_laterality} Smith Robinson approach was utilized to get to the anterior cervical spine. The skin, subcutaneous tissue were incised, the platysma was transected, and then blunt dissection was carried out between the sternocleidomastoid and carotid sheath laterally, and trachea and esophogus medially, down to the prevertebral fascia. Once the anterior spine was palpated, fluoroscopy was used to localize and confirm levels. "), 
                                                    glue("The longus coli was elevated bilaterally from {proximal_exposure_level$level[[1]]} proximally and to {distal_exposure_level$level[[1]]} distally. Once exposure was adequate, the deep retractors were placed into the anterior spine. "),
                                                    microscope_statement)
  }else{
    first_paragraph_list$surgical_approach <- glue("The anterior approach to the spine was carried out with assistance from the vascular surgeon. A {anterior_approach_laterality} incision was made and the approach was carried out down toward the spine. Once the approach was complete, retractors were placed and the surgical levels were confirmed using fluoroscopy. ")
  }
  
  procedure_details_list$approach_statement <- glue_collapse(x = first_paragraph_list, sep = " ")
  
  ################### PROCEDURE PARAGRAPHS ##################
  
  procedure_details_list$procedures <- all_anterior_procedures_paragraphs_function(all_objects_to_add_df = all_objects_to_add_df)
  
  ############################# CLOSURE #########################
  
  closure_statements_list <- list()
  
  if(max(all_objects_to_add_df$vertebral_number)<11){
    closure_statements_list$start <- "After confirming appropriate hemostasis, I then proceeded with closure of the wound in a layered fashion."
  }else{
    closure_statements_list$start <- "After confirming appropriate hemostasis, the wound was then closed in a layered fashion."
  }
  
  
  # if(length(end_procedure_details) > 0){
  #   closure_statements_list$added_to_wound <- glue("{glue_collapse(end_procedure_details, sep = ', ', last = ' and ')} was then placed into the surgical bed.")
  # }
  
  if(deep_drains >0){
    closure_statements_list$deep_drains_statement <- case_when(deep_drains == 1 ~ 'One drain was placed deep to the fascial layer.',
                                                               deep_drains >1 ~ paste(glue("A total of {deep_drains} drains were placed deep to the fascial layer.")))
  }
  
  # closure_statements_list$layered_closure <- "The wound was closed in a layered fashion."
  
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
  
  # closure_statements_list$dressing <- glue("Lastly, {glue_collapse(dressing, sep = ', ', last = ', and ')} was applied to the surgical site.")
  closure_statements_list$dressing <- str_replace_all(str_to_sentence(glue("Lastly, {glue_collapse(dressing, sep = ', ', last = ', and ')} was applied to the surgical site.")), pattern = "steristrips was", replacement = "steristrips were")
  
  procedure_details_list$closure <- glue_collapse(closure_statements_list, sep = " ")
  
  procedure_details_list <- map(.x = procedure_details_list, .f = ~str_replace_all(string = .x, pattern = "a 8", replacement = "an 8"))
  
  ### FINAL PARAGRAPH ####
  
  procedures_listed <- op_note_procedures_present_listed_function(objects_added_df = all_objects_to_add_df,
                                                                  # revision_decompression_vector = revision_decompression_vector,
                                                                  additional_procedures_performed_vector = additional_procedures_for_numbered_list)
  
  
  if(multiple_position_procedure == TRUE){
    procedure_details_list$final_paragraph <- glue("At the conclusion of the case, all counts were correct. The drapes were removed and we prepared for the next portion of the procedure. I was personally present for the entirety of the {procedures_listed}.")
  }else{
    procedure_details_list$final_paragraph <- glue("At the conclusion of the case, all counts were correct. The drapes were removed and the patient was transferred to the hospital bed, and awoke uneventfully. I was personally present for the entirety of the {procedures_listed}.")
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
    filter(str_detect(string = object, pattern = "anterior_plate")) %>%
    mutate(level = map(.x = level, .f = ~ jh_get_cranial_caudal_interspace_body_list_function(level = .x)$cranial_level)) %>%
    union_all(objects_added_df %>%
                filter(str_detect(string = object, pattern = "anterior_plate")) %>%
                mutate(level = map(.x = level, .f = ~ jh_get_cranial_caudal_interspace_body_list_function(level = .x)$caudal_level))) %>%
    unnest(level) %>%
    select(-vertebral_number) %>%
    mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
    # left_join(levels_numbered_df) %>%
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
      unnest(revision_level) %>%
      mutate(object = if_else(category == "decompression" & revision_level == TRUE, paste0("revision_", object), object)) %>%
      select(level, object, vertebral_number) %>%
      mutate(procedure_class = op_note_procedure_performed_summary_classifier_function(object = object)) %>%
      mutate(procedures_per_line = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_class)) 
  }else{
    summary_nested_df <- objects_added_df %>%
      select(level, object, vertebral_number) %>%
      mutate(procedure_class = op_note_procedure_performed_summary_classifier_function(object = object)) %>%
      mutate(procedures_per_line = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_class)) 
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

#############-----------------------   Generate ALL THE PROCEDURE PARAGRAPHS: input 1: the objects added df ----------------------###############

op_note_procedure_paragraphs_function <- function(objects_added_df, revision_decompression_vector = c()){
  
  if("implant_statement" %in% names(objects_added_df) == FALSE){ 
    objects_added_df <- objects_added_df %>%
      mutate(implant_statement = "")
  }
  
  if("screw_size_type" %in% names(objects_added_df) == FALSE){
    objects_added_df <- objects_added_df %>%
      mutate(screw_size_type = " ")
  }
  
  if(length(revision_decompression_vector) > 0){
    df_for_paragraphs <- objects_added_df %>%
      mutate(revision_levels_vector = list(revision_decompression_vector)) %>%
      select(level, vertebral_number, approach, category, object, side, revision_levels_vector, implant_statement, screw_size_type) %>%
      mutate(revision_level = map2(.x = level, .y = revision_levels_vector, .f = ~ str_detect(string = .x, pattern = .y))) %>%
      select(-revision_levels_vector) %>%
      mutate(revision_level = map(.x = revision_level, .f = ~ any(.x))) %>%
      unnest() %>%
      replace_na(list(implant_statement = " ", screw_size_type = " ", revision_level = FALSE)) %>%
      mutate(revision_label = paste0("revision_", object)) %>%
      mutate(object = if_else(revision_level == FALSE, object, if_else(category == "decompression", revision_label, object))) %>%
      select(-revision_label) %>%
      mutate(procedure_category = str_to_lower(op_note_procedure_performed_summary_classifier_function(object = object))) %>%
      select(level, vertebral_number, procedure_category, object, side, implant_statement, screw_size_type) %>%
      mutate(procedures_combine = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_category)) %>%
      group_by(procedure_category, procedures_combine, object) %>%
      nest() %>%
      ungroup() %>%
      group_by(procedure_category, procedures_combine) %>%
      nest() %>%
      mutate(nested_data = map(.x = data, .f = ~ unnest(.x))) %>%
      select(-data) 
  }else{
    df_for_paragraphs <- objects_added_df %>%
      mutate(procedure_category = str_to_lower(op_note_procedure_performed_summary_classifier_function(object = object))) %>%
      select(level, vertebral_number, procedure_category, object, side, implant_statement, screw_size_type) %>%
      mutate(procedures_combine = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_category)) %>%
      group_by(procedure_category, procedures_combine, object) %>%
      nest() %>%
      ungroup() %>%
      group_by(procedure_category, procedures_combine) %>%
      nest() %>%
      mutate(nested_data = map(.x = data, .f = ~ unnest(.x))) %>%
      select(-data)  
  }
  # THIS CREATES A DATAFRAME WITH NESTED DATAFRAMES.... full dataframe names = 'procedure_category, procedures_combine, nested_data'
  # nested df names ('nested_data') = 'object, level, vertebral_number, side, implant_statement, screw_size_type'
  
  procedures_op_full_df <- df_for_paragraphs %>%
    # replace_na(list(procedures_combine = "distinct", procedure_category = "insertion of intervertebral biomechanical implant")) %>%
    mutate(procedure_statement = pmap(list(..1 = procedure_category,
                                           ..2 = nested_data, 
                                           ..3 = procedures_combine),
                                      .f = ~create_full_paragraph_statement_function(procedure_paragraph_intro = ..1,
                                                                                     df_with_levels_object_nested = ..2, 
                                                                                     paragraphs_combined_or_distinct = ..3))) %>%
    select(procedure_category, procedures_combine, procedure_statement) %>%
    unnest(procedure_statement)
  
  glue_collapse(x = procedures_op_full_df$procedure_statement, sep = "\n\n")
  
}

#############-----------------------               End              ----------------------###############

#############-----------------------   Generate a FULL PARAGRAPH ----------------------###############
create_full_paragraph_statement_function <- function(procedure_paragraph_intro, df_with_levels_object_nested, paragraphs_combined_or_distinct){
  if(paragraphs_combined_or_distinct == "combine"){
    
    df_with_statement <- df_with_levels_object_nested %>%
      mutate(object = if_else(str_detect(object, "pelvic_screw"), "pelvic_screw", object)) %>%
      group_by(object) %>%
      nest() %>% 
      mutate(object_levels_side_df = data) %>%  ### this creates a dataframe with only two columns: 'object' and 'object_levels_side_df'. #The nested dataframe has columns: level, vertebral_number, side, implant_statement, screw_size_type
      select(-data) %>%
      mutate(tech_statement = map2(.x = object, .y = object_levels_side_df, .f = ~ op_note_technique_combine_statement(object = .x, levels_side_df = .y))) %>%
      select(object, tech_statement) %>%
      unnest(tech_statement) %>%
      distinct()
    
    df_levels <- df_with_levels_object_nested %>%
      select(level, vertebral_number) %>%
      distinct() %>%
      arrange(vertebral_number)
    
    if(procedure_paragraph_intro == "pelvic instrumentation" | procedure_paragraph_intro == "posterior spinal instrumentation"){
      if(procedure_paragraph_intro == "pelvic instrumentation"){
        statement <- glue("I then proceeded with instrumentation of the pelvis. {glue_collapse(df_with_statement$tech_statement, sep = ' ')} This completed the instrumentation of the pelvis.")
      }
      if(procedure_paragraph_intro == "posterior spinal instrumentation"){
        statement <- glue("I then proceeded with the posterior spinal instrumentation. {glue_collapse(df_with_statement$tech_statement, sep = ' ')} This partially completed the {procedure_paragraph_intro} of {glue_collapse(unique(x = df_levels$level), sep = ', ', last = ', and ')}.")
      }
      
    }else{
      statement <- glue("I then proceeded with the {procedure_paragraph_intro}. {glue_collapse(df_with_statement$tech_statement, sep = ' ')} This completed the {procedure_paragraph_intro} of {glue_collapse(unique(x = df_levels$level), sep = ', ', last = ', and ')}.")
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
      mutate(procedure_category = str_to_lower(op_note_procedure_performed_summary_classifier_function(object = object))) %>%
      mutate(tech_statement = paste(tech_statement_detail, glue("This completed the {procedure_category} at the {level} {if_else(str_detect(level, '-'), 'interspace', 'level')}.")))
    
    statement <- glue_collapse(df_with_statement$tech_statement, sep = "\n\n")
  }
  
  return(statement)
  
}

#############-----------------------               End              ----------------------###############


#############-----------------------   Building Paragraphs that are combined (e.g. pedicle screws, decompressions) ----------------------###############

op_note_technique_combine_statement <- function(object, levels_side_df){
  
  if(str_detect(object, "pelvic_screw")){
    
    pelvic_screws_wide <- levels_side_df %>%
      select(level, side) %>%
      mutate(count = row_number()) %>%
      pivot_wider(names_from = side, values_from = count) %>%
      unnest()
    
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
  
  screw_statements_wide_df <- levels_side_df %>%
    mutate(unilateral_screw_statement = str_to_lower(glue("a {screw_size_type} screw was inserted on the {side}"))) %>%
    select(level, side, unilateral_screw_statement) %>%
    pivot_wider(names_from = side, values_from = unilateral_screw_statement) %>%
    unnest() 
  
  screw_statements_df <- tibble(level = character(), left = character(), right = character(), unilateral_screw_statement = character()) %>%
    union_all(screw_statements_wide_df) %>%
    replace_na(replace = list(left = "xx", right = "xx")) %>%
    mutate(screw_statement = case_when(
      left != "xx" & right != "xx" ~ glue("At {level}, {left} and {right}."), 
      left == "xx" & right != "xx" ~ glue("At {level}, {right}."), 
      left != "xx" & right == "xx" ~ glue("At {level}, {left}."), 
      left == "xx" & right == "xx" ~ glue(""))) %>%
    mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
    arrange(vertebral_number)
  
  
  
  exiting_roots_df <- levels_side_df %>%
    mutate(exiting_root = jh_get_exiting_nerve_root_function(interspace = level)) %>%
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
  
  
  
  
  technique_statement <- case_when(
    object == "incision_drainage" ~ glue("The wound was inspected thoroughly and any necrotic appearing tissue was excised. Tissue samples from deep and superficial wound bed were sent for cultures. The wound bed was thoroughly irrigated and then Working layer by layer from deep to superficial, the wound was meticulously debrided. Once I felt the wound was clean and an adequate debridement had been completed, I again copiously irrigated the wound."), 
    object == "vertebroplasty" ~ glue("For vertebral body cement augmentation, a cannula was inserted into the vertebral body through the pedicle. The cement was then injected under x-ray guidance until an appropriate amount of cement had been injected into the vertebral body. Vertebroplasty was performed at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}."), 
    object == "vertebral_cement_augmentation" ~ glue("For vertebral body cement augmentation, I first created a cavity within the vertebral body. The cement was then injected under x-ray guidance until an appropriate amount of cement had been injected into the vertebral body. Vertebral augmentation was performed at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}."), 
    object == "laminar_downgoing_hook" ~ glue("For downgoing laminar hooks, the ligamentum flavum was removed at the site of the hook insertion. A hook was then inserted securely under the lamina {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "laminar_upgoing_hook" ~ glue("For upgoing laminar hooks, the inferior edge of the cranial lamina was identified and the plane between the lamina and ligamentum was developed and the upgoing hook is then placed within this plane, secured to the lamina aimed cranially {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "tp_hook" ~ glue("For transverse process hooks, the cranial edge of the transverse process was identified. A transverse process finder was used to develop the plane and a transverse process hook was placed securely into position along the superior edge of the transverse process. A transverse process hook was placed {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."),  
    object == "lateral_mass_screw" ~ glue("For lateral mass screw placement, the entry point was identified using the lateral and medial borders of the lateral mass and superior and inferior borders of the facet. The superficial cortex was opened at each entry point using a high speed burr and the screw path drilled incrementally to the far cortex. {glue_collapse(x = screw_statements_df$screw_statement, sep = ' ')}"), 
    object == "occipital_screw" ~ glue("An appropriately sized occipital plate was selected and was placed centered in the midline and just caudal to the external occipital protuberance, but cranial to the foramen magnum. The plate was held in place to identify the appropriate start points for the occipital screws. The occipital screws were drilled incrementally until the anterior cortex was penetrated. The length of the path was measured to acheive bicortical fixation. The screw paths were then tapped and the screws placed, securing the plate to the occiput."), 
    object == "pars_screw" ~ glue("For pars screws, the start point was identified just 3-4mm cranial to the inferior facet joint and in the midpoint of the pars from medial to lateral. Once the start point was identified, the superficial cortex was opened at the entry point using the high speed burr. A drill was used to cannulate a screw path, aiming as dorsal as possible while not perforating the dorsal cortex of the pars. The path was then tapped and the length measured and pars screw placed. {glue_collapse(x = screw_statements_df$screw_statement, sep = ' ')}"), # {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "pedicle_screw" ~ glue("For pedicle screws, the transverse process, pars, and superior facet were used as landmarks to identify the appropriate starting point. After identifying the start point, the superficial cortex was opened at each entry point using a high speed burr. A pedicle probe was then used to navigate down the pedicle, followed by palpating a medial, lateral, superior and inferior pedicle wall, measuring, and tapping if appropriate. {glue_collapse(x = screw_statements_df$screw_statement, sep = ' ')}"), 
    object == "translaminar_screw" ~ glue("For translaminar screw fixation, the starting point was identified using the spinolaminar junction and in the central plane of the contralateral lamina. A burr hole was made to open the superficial cortex, the path was then cannulated and intralaminar screw placed. {glue_collapse(x = screw_statements_df$screw_statement, sep = ' ')}"), # {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."),
    object == "pedicle_hook" ~ glue("For pedicle hooks, a pedicle finder was carefully placed just under the inferior facet and above the superior facet, until the pedicle was found. Once the pedicle was identified, a pedicle hook was inserted directly toward the inferior pedicle, secured within the residual facet joint {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}."), 
    object == "pelvic_screw" ~ pelvic_screw_statement,
    object == "pelvic_screw_1" ~ pelvic_screw_statement,
    object == "pelvic_screw_2" ~ pelvic_screw_statement,
    object == "sublaminar_wire" ~ glue("For sublaminar wiring, the doubled-up sublaminar was was bent into a hook shape and and passed under the inferior lamina from a caudal to cranial direction. Once the wire was safely passed, the tip of the wire was cut and the two strands were directed in opposite directions of the midline to be attached to the rods on the left and right side. Wires were passed under {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}."), 
    object == "tether" ~ glue("For posterior vertebral tethering, a hole was drilled in the transverse process over the lamina. A band was then thread through the hole and tied in a figure-8 fashion, tethering {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}."), 
    object == 'complete_facetectomy' ~ glue("For a complete facetectomy, the inferior, superior, medial and lateral borders of the inferior and superior facet were identified. Both the superior and inferior facet were completely excised and the underlying exiting nerve root decompressed. I performed a complete facetectomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}. This effectively decompressed {glue_collapse(x = exiting_roots_df$exiting_roots_statement, sep = ', ', last = ' and ')}."),
    object == 'grade_1' ~ glue("The inferior, superior, medial and lateral borders of the inferior facet were identified. The inferior facets {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} were excised and the bone was morselized to be used as morselized autograft."),
    object == 'grade_2' ~ glue("For the posterior column osteotomies, a small rent was made in the interlaminar space to develop the plane between the ligamentum and the dura. A Kerrison rongeur was then used to excise the ligamentum. this was carried out laterally in both directions until the facets were encountered. The superior and inferior facet were both adequately resected to fully release the posterior column. I performed posterior column (Smith-Peterson) osteotomies at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}"),
    object == 'diskectomy' ~ glue("For a diskectomy, I used the cranial and caudal laminar edges, pars, and facet joints as landmarks. I performed a diskectomy with partial medial facetectomy and foraminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. Using a high-speed burr and Kerrison rongeurs, I first performed a foraminotomy and excised the ligamentum flavum. The dural sac was identified and traversing root was protected. The annulus was then incised and the diseased disk materal was removed. Following the decompression, the canal and foramen and lateral recess were palpated to confirm an appropriate decompression had been completed."),
    object == 'laminotomy' ~ glue("For the laminotomy, the cranial and caudal laminar edges, pars, and facet joints were used as landmarks. Once I had determined the laminotomy site, I used a combination of a high-speed burr and Kerrison rongeurs to resect the bone dorsal to the nerve root. I performed a laminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. Following the decompression, the foramen was palpated to confirm an adequate decompression had been completed."),
    object == 'laminectomy' ~ glue("Using the cranial and caudal edges of the lamina and pars as landmarks, I performed a laminectomy at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}. First, using a combination of a high-speed burr and Kerrison rongeurs, I resected the dorsal lamina. I then excised the underlying ligamentum flavum. Following the decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'laminectomy' ~ glue("Using the cranial and caudal edges of the lamina and pars as landmarks, I performed a laminectomy at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} in order to excise the underlying facet cyst. First, using a combination of a high-speed burr and Kerrison rongeurs, I resected the dorsal lamina. I then excised the underlying ligamentum flavum. This allowed exposure to the underlying facet cyst, which was fully excised. Following the decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'laminectomy_for_tumor' ~ glue("Using the cranial and caudal edges of the lamina and pars as landmarks, I performed a laminectomy at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} in order to perform an open biopsy and excise the intraspinal extradural tumor. First, using a combination of a high-speed burr and Kerrison rongeurs, I resected the dorsal lamina. I then excised the underlying ligamentum flavum. At this point, I encountered the extradural lesion which measured roughly {length(levels_side_df$level)*3.5}cm long by ~2-3cm wide. I obtained a biopsy of the tissue and then proceeded to excise any visible tumor in order to adequately create separation between visible tumor and the dura. Following the laminectomy and excision of the lesion, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'sublaminar_decompression' ~ glue("Using a combination of a high-speed burr and Kerrison rongeurs, I performed a partial laminectomy bilaterally at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace. First, I resected the dorsal lamina and then excised the ligamentum flavum, exposing the dura. To fully decompress the exiting and traversing nerve roots within the neural foramen and lateral recess, the medial facet was partially excised and a foraminotomy performed on the left and right at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace. Following the decompression, the canal and foramen were palpated to confirm an adequate decompression had been completed."),
    object == 'revision_diskectomy' ~ glue("For the revision diskectomy, I used the cranial and caudal laminar edges, pars, and facet joints as landmarks. I performed a revision and reexploration with diskectomy and partial medial facetectomy and foraminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. I carefully dissected off the scar until I was able to safely identify the bony edges and develop a plane between the dura and scar. Using a high-speed burr and Kerrison rongeurs, I performed a foraminotomy and excised the scar and ligamentum. The dural sac was identified and traversing root was protected. The annulus and disk space were identified and the diseased disk material was excised. Following the revision decompression, the canal and foramen and lateral recess were palpated to confirm an adequate decompression had been completed."),
    object == 'revision_laminotomy' ~ glue("For the revision laminotomy, the cranial and caudal laminar edges, pars, and facet joints were used as landmarks. I carefully exposed the dorsal elements until I had identified the edges of the laminar defect. Once I determined the laminotomy site, I used a combination of a high-speed burr and Kerrison rongeurs to resect the bone dorsal to the nerve root. I performed a revision laminotomy {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')} interspace to fully decompress the nerve root. Following the decompression, the foramen was palpated to confirm an adequate decompression had been completed."),
    object == 'revision_laminectomy' ~ glue("Using the cranial and caudal edges of the lamina and pars as landmarks, I performed a reexploration and revision laminectomy at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}. Using a combination of a high-speed burr, currettes and Kerrison rongeurs, I carefully dissected off the scar until I was able to safely identify the bony edges and develop a plane between the dura and scar. I removed any dorsal lamina, ligamentum flavum and scar thought to still be causing compression. The central canal was palpated to confirm full decompression of the thecal sac. Following the revision decompression, the canal was palpated to confirm an adequate decompression had been completed."),
    object == 'revision_sublaminar_decompression' ~ glue("Using a combination of a high-speed burr and Kerrison rongeurs, I performed a reexploration and revision decompression bilaterally at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace. First, I carefully dissected off the scar until I was able to safely identify the bony edges and develop a plane between the dura and scar. I proceeded with partial laminectomies and excised the scar and underlying ligamentum flavum. The medial facet was partially excised and a foraminotomy performed on the left and right at the {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')} interspace, to fully decompress the exiting and traversing nerve roots within the neural foramen and lateral recess. Following the revision decompression, the canal and foramen were palpated to confirm an adequate decompression had been completed."),
    object == 'costotransversectomy' ~ glue("For the costotransversectomy, the rib was identified and a subperiosteal dissection was carried out laterally 6-7cm. Once the medial rib was skeletonized and a safe, subperiorsteal plane was confirmed, 5-6cm of the medial rib was transected. This process was completed {glue_collapse(x = levels_side_df$level_side, sep = ', ', last = ' and ')}, allowing a safe costovertebral approach to the anterior vertebral body."),
    object == 'lateral_extracavitary_approach' ~ glue("For the modified lateral extracavitary approach at {glue_collapse(levels_side_df$level, sep = ', ', last = ' and ')}, the pedicle of {glue_collapse(levels_side_df$level, sep = ', ', last = ' and ')} was identified and using a combination of a high-speed burr, Kerrison rongeurs and osteotome, the inferior and superior facets were fully resected, allowing access to the interspace. The lateral border of the disk space was identified and the dissection was carried out anteriorly around the entire anterior portion of the vertebral body. This was carried out {glue_collapse(levels_side_df$level_side, sep = ', ', last = ' and ')} and allowed safe access to the anterior longitudinal ligament and the most anterior portion of the vertbral body."),
    object == 'laminoplasty' ~ glue("For laminoplasty, an opening and hinge trough was created with a high-speed burr angled perpendicular to the lamina at the lateral mass-lamina junction. This was performed at {glue_collapse(x = levels_side_df$level, sep = ', ', last = ' and ')}. After all troughs were completed, greenstick fractures were created to open the lamina at the laminoplasty levels, the ligamentum flavum at the proximal and distal levels was excised, and laminoplasty plates were sequentially secured into place. The spinous processes were trimmed and the canal was palpated to confirm an adequate decompression had been completed.")
  )
  
  technique_statement <- str_replace_all(string = technique_statement, pattern = "a na ", replacement = "a ")
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
    object == 'grade_3' ~ glue("I then proceeded with the pedicle subtraction osteotomy at the level of {level}. The posterior elements of {level} were completely removed. A complete central laminectomy was performed and confirmed at the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} level in order to prevent compression after osteotomy closure. A temporary rod was placed and starting on the contralateral side, a complete superior facetectomy was performed and any dorsal bone and scar was removed, and the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} nerve root was visualized and tracked laterally. Then a cut was made through the pars just inferior to the pedicle, and the superior facet of the inferior vertebra was resected, allowing the {level} nerve root to be visualized and tracked laterally. Next, the transverse process was resected. Once the {level} pedicle was completely skeletonized, and the cranial and caudal nerve roots identified and a full decompression was confirmed from the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} pedicle to the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} pedicle, a subperiosteal plane was developed around the anterior portion of the {level} body and retractors were held in place to protect the retroperitoneal  structures. Next, the pedicle was resected down to the dorsal vertebral body, leaving a lip medially to protect the neural structures. The dura, cranial and caudal nerve roots were protected and a high speed burr was used to develop a defect in the vertebral body at the level of the pedicle. The lateral portion of the pedicle wall was taken down, allowing easier access to the vertebral body and the vertebral body was hollowed out, taking care to leave adequate anterior body and cortex. The remaining pedicle walls were then resected with a box osteotome. We then placed a temporary rod and repeated the same process on the contralateral pedicle. Once an adequate defect had been generated in the vertebral body, the dorsal wall of the vertebral body was impacted. The nerve roots were checked again and the underside of the thecal sac was palpated to confirm no bony impingement. Once we were satisfied that the thecal sac was fully decompressed circumferentially and that the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} and {level} nerve roots were completely free, the osteotomy was closed, and the other rod was placed into position and set screws final tightened to lock the osteotomy in position. Intraoperative xray was performed to confirm appropriate alignment."),
    object == 'grade_4' ~ glue("I then proceeded with an extended three column osteotomy (partial corpectomy) at the level of {level}. The posterior elements of {level} were completely removed. A complete central laminectomy was performed and confirmed at the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} level in order to prevent compression after osteotomy closure. A temporary rod was placed and starting on the contralateral side, a complete superior facetectomy was performed and any dorsal bone and scar was removed, and the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} nerve root was visualized and tracked laterally. Then a cut was made through the pars just inferior to the pedicle, and the superior facet of the inferior vertebra was resected, allowing the {level} nerve root to be visualized and tracked laterally. Next, the transverse process was resected. Once the {level} pedicle was completely skeletonized, and the cranial and caudal nerve roots identified and a full decompression was confirmed from the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} pedicle to the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$caudal_level} pedicle, a subperiosteal plane was developed around the anterior portion of the {level} body and retractors were held in place to protect the retroperitoneal  structures. Next, the pedicle was resected down to the dorsal vertebral body, leaving a lip medially to protect the neural structures. The dura, cranial and caudal nerve roots were protected and a high speed burr was used to develop a defect in the vertebral body. The lateral portion of the pedicle wall was taken down, allowing easier access to the vertebral body and the vertebral body was hollowed out, extending up into the cranial disk space, taking care not to perforate the anterior cortex. The remaining pedicle walls were then resected with a box osteotome. We then placed a temporary rod and repeated the same process on the contralateral pedicle. Once roughly 50% of the vertebral body had been resected, the dorsal wall of the vertebral body was impacted. The nerve roots were checked again and the underside of the thecal sac was palpated to confirm no bony impingement. Once we were satisfied that the thecal sac was fully decompressed circumferentially and that the {jh_get_cranial_caudal_interspace_body_list_function(level = level)$cranial_level} and {level} nerve roots were completely free, the osteotomy was closed, and the other rod was placed into position and set screws final tightened to lock the osteotomy in position. Intraoperative xray was performed to confirm appropriate alignment."),
    object == 'grade_5' ~ glue("I then proceeded with a vertebral column resection at the level of {level}. A complete central decompression was performed and confirmed extending from the {cranial_level} pedicle to the {caudal_level} pedicle in order to prevent neural compression after osteotomy closure. A complete facetectomy was performed superiorly and any dorsal bone was removed, allowing the {cranial_level} nerve root to be visualized and tracked laterally. Similarly, a complete facetectomy was performed inferiorly, allowing the {level} nerve root to be visualized and tracked laterally. Next, the transverse process was resected and the {level} pedicles were completely skeletonized. The cranial and caudal nerve roots were then free and a full decompression was confirmed from the {cranial_level} pedicle to the {caudal_level} pedicle.{thoracic_nerve_ligation_statement} In preparation for the vertebral column resection, a unilateral stabilizing rod was placed on the convex side and secured with set screws multiple levels above and below the {level} level. Starting on the concavity, a combination of blunt and electrocautery dissection was used to develop a subperiosteal plane around the entire anterior portion of the {level} body and retractors were held in place to protect the anterior and neural structures. A lateral pedicle body window was developed for accessing the cancellous bone of the vertebral body and using currettes and rongeurs, the accessible cancellous vertebral body was removed, extending up toward the cranial and caudal disk space. The remaining pedicle of {level} was resected down to the dorsal vertebral body, allowing the spinal cord to drift medially. The same process was then carried out on the convexity to access the cancellous vertebral body and remove the convex pedicle. The entire vertebral body was removed except for a thin anterior section, preserving the anterior longitudinal ligament. Discectomies were performed at the {cranial_interspace} and {caudal_interspace}, taking care to preserve the endplates. Once the entire body had been removed, a plane was developed between the ventral dural sac and the posterior body wall to be sure the dural sac was completely free from the body wall. An impactor was then used to impact the posterior body wall, completing the vertebral column resection. The dural sac was again inspected to confirm no potential sites of impingement. We then turned toward closure of the resection site. Starting with compression on the convexity, the site was closed through alternating compression on the convexity and slight distraction on the concave side. Once I was satisfied with the closure, rods were secured into place bilaterally with set screws."),
    
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
                                                       revision_decompression_vector = NULL,
                                                       fusion_levels_vector = NULL,
                                                       additional_procedures_performed_vector = NULL){
  
  additional_procedures_performed_vector <- keep(.x = additional_procedures_performed_vector, .p = ~ str_detect(.x, "Biopsy") | 
                                                   str_detect(.x, "Repair of dura")  | 
                                                   str_detect(.x, "Dural")  | 
                                                   str_detect(.x, "Removal of spinal") | 
                                                   str_detect(.x, "Exploration of") | 
                                                   str_detect(.x, "Open treatment"))
  
  if(length(revision_decompression_vector) > 0){
    summary_nested_df <- objects_added_df %>%
      mutate(revision_levels_vector = list(revision_decompression_vector)) %>%
      select(level, vertebral_number, approach, category, object, side, revision_levels_vector) %>%
      mutate(revision_level = map2(.x = level, .y = revision_levels_vector, .f = ~ str_detect(string = .x, pattern = .y))) %>%
      select(-revision_levels_vector) %>%
      mutate(revision_level = map(.x = revision_level, .f = ~ any(.x))) %>%
      unnest(revision_level) %>%
      mutate(object = if_else(category == "decompression" & revision_level == TRUE, paste0("revision_", object), object)) %>%
      select(level, object, vertebral_number) %>%
      mutate(procedure_class = str_to_lower(op_note_procedure_performed_summary_classifier_function(object = object))) %>%
      mutate(procedures_per_line = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_class)) 
  }else{
    summary_nested_df <- objects_added_df %>%
      select(level, object, vertebral_number) %>%
      mutate(procedure_class = op_note_procedure_performed_summary_classifier_function(object = object)) %>%
      mutate(procedures_per_line = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_class)) 
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


#############-----------------------   GENERATE FULL POSTERIOR OPERATIVE NOTE  ----------------------###############
#############-----------------------   GENERATE FULL POSTERIOR OPERATIVE NOTE  ----------------------###############
#############-----------------------   GENERATE FULL POSTERIOR OPERATIVE NOTE  ----------------------###############

op_note_posterior_function <- function(all_objects_to_add_df,
                                       fusion_levels_df,
                                       head_position = "Cranial Tongs",
                                       revision_decompression_vector,
                                       revision_implants_df,
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
                                       bone_graft_vector = NULL,
                                       morselized_allograft = 0,
                                       morselized_autograft_separate = 0,
                                       structural_allograft_location = NULL,
                                       structural_autograft_harvest = NULL,
                                       structural_autograft_location = NULL,
                                       deep_drains = 0,
                                       superficial_drains = 0,
                                       end_procedure_details = NULL,
                                       closure = NULL,
                                       dressing = NULL, 
                                       multiple_position_procedure = FALSE){
  
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
  
  crosslink_df <- all_objects_to_add_df %>%
    filter(object == "crosslink") %>%
    select(level, object)
  
  structural_allograft_df <- all_objects_to_add_df %>%
    filter(object == "structural_allograft") %>%
    select(level, object)
  
  all_objects_to_add_df <- all_objects_to_add_df %>%
    filter(object != "crosslink") %>%
    filter(object != "structural_allograft")
  
  additional_procedures_for_numbered_list <- c(as.character(glue("Application of {glue_collapse(bone_graft_vector, sep = ', ', last = ', and ')}")))
  
  additional_procedures_for_numbered_list <- append(additional_procedures_for_numbered_list, additional_procedures_vector)
  
  if(nrow(structural_allograft_df) > 0 ){
    additional_procedures_for_numbered_list <- append(additional_procedures_for_numbered_list, c(as.character(glue("Application of strucural allograft strut at {glue_collapse(structural_allograft_df$level, sep = ', ', last = ', and ')}"))))
  }
  
  procedures_numbered_list$primary_procedures <- op_note_procedures_performed_numbered_function(objects_added_df = all_objects_to_add_df, 
                                                                                                revision_decompression_vector = revision_decompression_vector, 
                                                                                                fusion_levels_vector = fusion_levels_df$level, 
                                                                                                additional_procedures_performed_vector = additional_procedures_for_numbered_list)
  
  first_paragraph_list <- list()
  
  first_paragraph_list$transport_anesthesia <- paste("The patient was brought to the operating room and after obtaining appropriate IV access, the patient underwent general anesthesia.")
  
  if(length(antibiotics) == 0){
    first_paragraph_list$antibiotic_statement <- "Preoperative Antibiotics were administered."
  }else{
    if(antibiotics == "None (Antibiotics were held)"){
      first_paragraph_list$antibiotic_statement <- "Preoperative antibiotics were held until tissue cultures could be obtained."
    }else{
      first_paragraph_list$antibiotic_statement <- paste0("Preoperative Antibiotics were administered including ", glue_collapse(antibiotics, sep = ", ", last = " and "), ".")
    }
  }
  
  if(any(str_detect(additional_procedures_vector, "Spinal Cord Monitoring"))){
    first_paragraph_list$spinal_cord_monitoring <- "Spinal Cord Monitoring needles were inserted by the neurophysiology technologist."
  }
  
  first_paragraph_list$head_statement <- case_when(
    head_position == "Supine/Lateral" ~ "The patient's head rested in a position of comfort, securely on the bed.",
    head_position == "Proneview Faceplate" ~ "The proneview faceplate was used to pad and secure the patient's head and face during surgery.",
    head_position == "Cranial Tongs" ~ "Cranial tongs were applied to the patient's skull for positioning and an appropriate weight was selected for cranial traction.",
    head_position == "Halo" ~ "A Halo was applied to the patient's skull for positioning and the pins were sequentially tightened to the appropriate torque.",
    head_position == "Mayfield" ~ "A Mayfield head holder was applied to the patient's skull for positioning and secured to the bed.",
  )
  
  
  first_paragraph_list$positioning <- paste("The patient was then positioned prone on the OR table and all bony prominences were appropriately padded.",
                                            "After prepping and draping in the standard fashion, a surgical timeout was performed.")
  ############### JUST START OVER AT THIS POINT IF IT IS A MULTIPLE POSITION CASE
  if(multiple_position_procedure == TRUE){
    first_paragraph_list <- list()
    first_paragraph_list$positioning <- paste("The patient was then positioned prone on the OR table and all bony prominences were appropriately padded.")
    
    first_paragraph_list$head_statement <- case_when(
      head_position == "Proneview Faceplate" ~ "The proneview faceplate was used to pad and secure the patient's head and face during surgery.",
      head_position == "Cranial Tongs" ~ "Cranial tongs applied to the patient's skull with an appropriate weight for cranial traction was used to position the head.",
      head_position == "Halo" ~ "A Halo applied to the patient's skull was used for positioning and the pins were sequentially tightened to the appropriate torque.",
      head_position == "Mayfield" ~ "A Mayfield head holder was applied to the patient's skull for positioning and secured to the bed.",
    )
    
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
    
    first_paragraph_list$surgical_approach <- glue("A standard posterior approach to the spine was performed, exposing proximally to the {proximal_exposure_level$level[1]} level and distally to the {distal_exposure_level$level[1]} level.")
    
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
    procedure_details_list$procedures <- op_note_procedure_paragraphs_function(objects_added_df = all_objects_to_add_df,
                                                                               revision_decompression_vector = revision_decompression_vector)
  }
  
  
  ################### COMPLETING INSTRUMENTATION ##################
  posterior_implants_all_df <- all_objects_to_add_df %>%
    mutate(procedure_category = str_to_lower(op_note_procedure_performed_summary_classifier_function(object = object))) %>%
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
    distinct() %>%
    distinct()
  
  posterior_implant_df <- posterior_implants_all_df %>%
    filter(procedure_category != "pelvic instrumentation") %>%
    mutate(level = if_else(level == "Iliac", "Pelvis", level)) %>%
    mutate(level = if_else(level == "S2AI", "Pelvis", level))
  
  instrumented_levels_vector <- unique((posterior_implants_all_df %>% 
                                          mutate(level = if_else(level == "Iliac", "the pelvis", level)) %>%
                                          mutate(level = if_else(level == "S2AI", "the pelvis", level)) %>%
                                          distinct())$level)
  
  (posterior_implants_all_df %>% 
    mutate(level = if_else(level == "Iliac", "the pelvis", level)) %>%
    mutate(level = if_else(level == "S2AI", "the pelvis", level)) %>%
    distinct())$level
  
  # instrumented_levels_vector <- unique(posterior_implants_all_df$level)
  
  if(nrow(posterior_implant_df) > 0){
    rod_statements_list <- list()
    
    if(any(str_detect(posterior_implant_df$side, "bilateral"))){
      rod_statements_list$rod_contouring <- glue("To complete the spinal instrumentation, a {left_main_rod_size} {left_main_rod_material} rod was contoured for the left and a {right_main_rod_size} {right_main_rod_material} rod was contoured for the right and the rods placed into position and secured with set screws.")
      
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
    
    if(bmp > 0){
      fusion_statement_list$bmp_statement <- glue("To improve the odds of a successful fusion, {bmp}mg of BMP was placed into the posterior fusion bed. ")
    }
    
    if(any(str_detect(string = bone_graft_vector, pattern = "Morselized"))){
      
      morselized_graft_df <- tibble(graft_type = discard(.x = bone_graft_vector, .p = ~ str_detect(string = .x, pattern =  "Structural"))) %>%
        mutate(morselized_allo_amount = morselized_allograft) %>%
        mutate(graft_type = if_else(morselized_allo_amount > 0 & graft_type == "Morselized Allograft", 
                                    paste0("A total of ", morselized_allo_amount, "cc of morselized allograft"), 
                                    graft_type))
      
      fusion_statement_list$morselized_allograft_statement <- glue("{glue_collapse(morselized_graft_df$graft_type, sep = ', ', last = ' and ')} was then impacted into the lateral gutters and into the facet joints.")
    }else{
      fusion_statement_list$morselized_allograft_statement <- glue("Allograft chips and and local autograft was then impacted into the lateral gutters and into the facet joints of {glue_collapse(fusion_levels_df$level, sep = ', ', last = ', and ')}.")
    }
    
    fusion_statement_list$complete <- glue("This completed the posterior spinal fusion procedure of {glue_collapse(fusion_levels_df$level, sep = ', ', last = ', and ')}.")
    
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
  
  if(any(dressing == "Wound Vac")){
    closure_statements_list$dressing <- "A wound vac was then applied to the open wound, measuring roughly *** in length by *** in width."
    
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
    procedure_details_list$final_paragraph <- glue("At the conclusion of the case, all counts were correct. The drapes were removed and the patient was turned uneventfully. I was personally present for the entirety of this portion of the case, including {procedures_listed}.")
  }else{
    procedure_details_list$final_paragraph <- glue("At the conclusion of the case, all counts were correct. The drapes were removed and the patient was turned onto the hospital bed, and awoke uneventfully. I was personally present for the entirety of the case, including {procedures_listed}.")
  }
  
  
  
  procedure_paragraphs <- glue_collapse(x = procedure_details_list, sep = "\n\n")
  
  procedure_paragraphs <- str_replace_all(procedure_paragraphs, "a  ", "a ")
  
  
  return(list(procedure_details_paragraph = procedure_paragraphs, 
              procedures_numbered_paragraph = procedures_numbered_list[[1]]))
  
}



#############-----------------------               End              ----------------------###############


#############-----------------------   PROCEDURES PERFORMED NUMBERED SECTION  ----------------------###############

op_note_procedures_performed_numbered_function <- function(objects_added_df,
                                                           revision_decompression_vector = NULL,
                                                           fusion_levels_vector = NULL,
                                                           additional_procedures_performed_vector = NULL){
  if(length(revision_decompression_vector) > 0){
    summary_nested_df <- objects_added_df %>%
      mutate(revision_levels_vector = list(revision_decompression_vector)) %>%
      select(level, vertebral_number, approach, category, object, side, revision_levels_vector, implant_statement, screw_size_type) %>%
      mutate(revision_level = map2(.x = level, .y = revision_levels_vector, .f = ~ str_detect(string = .x, pattern = .y))) %>%
      select(-revision_levels_vector) %>%
      mutate(revision_level = map(.x = revision_level, .f = ~ any(.x))) %>%
      unnest() %>%
      replace_na(list(implant_statement = " ", screw_size_type = " ", revision_level = FALSE)) %>%
      mutate(revision_label = paste0("revision_", object)) %>%
      mutate(object = if_else(revision_level == FALSE, object, if_else(category == "decompression", revision_label, object))) %>%
      select(-revision_label) %>%
      mutate(procedure_class = op_note_procedure_performed_summary_classifier_function(object = object)) %>%
      mutate(procedures_per_line = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_class)) 
  }else{
    summary_nested_df <- objects_added_df %>%
      select(level, object, vertebral_number) %>%
      mutate(procedure_class = op_note_procedure_performed_summary_classifier_function(object = object)) %>%
      mutate(procedures_per_line = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_class)) 
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
    select(procedures_performed_numbered)
  
  glue_collapse(procedures_numbered_df$procedures_performed_numbered, sep = "\n")
}

