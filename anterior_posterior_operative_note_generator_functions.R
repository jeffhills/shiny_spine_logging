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


#############-----------------------  FULL ANTERIOR OPERATIVE NOTE BODY GENERATOR ----------------------###############

op_note_anterior_function <- function(all_objects_to_add_df,
                                      anterior_plate_revision_df = tibble(level = character(), 
                                                                          prior_plate_status = character()),
                                      anterior_approach_laterality = "left-sided",
                                      approach_statement = "none",
                                      antibiotics = vector(),
                                      antifibrinolytic = "",
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
                                      multiple_position_procedure = "NA", 
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
    first_paragraph_list$antibiotic_statement <- paste0(glue_collapse(antibiotics, sep = ", ", last = " and "), " was administered for preoperative antibiotics.")
    }
  
  if(antifibrinolytic != ""){
    first_paragraph_list$antifibrinolytic <- antifibrinolytic
  }
  
  if(any(str_detect(additional_procedures_vector, "Halo"))){
    first_paragraph_list$head_statement <- paste(glue("A Halo was applied to {his_or_her} skull for positioning and the pins were sequentially tightened to the appropriate torque."))
  }
  
  if(any(str_detect(additional_procedures_vector, "Tongs"))){
    first_paragraph_list$head_statement <- paste(glue("Cranial tongs were applied to {his_or_her} skull for positioning and an appropriate weight was selected for cranial traction."))
  }
  
  if(length(neuromonitoring_list$modalities) > 0 & (any(neuromonitoring_list$modalities == "None") == FALSE)){
    first_paragraph_list$spinal_cord_monitoring <- glue("Neuromonitoring needles were inserted by the neurophysiology technologist for monitoring using {glue_collapse(x = neuromonitoring_list$modalities, sep = ', ', last = ' and ')}. ")
  }
  
  if(length(neuromonitoring_list$pre_positioning_motors) > 0){
    first_paragraph_list$pre_positioning_motors <- neuromonitoring_list$pre_positioning_motors 
  }
  
  if(str_detect(anterior_approach_laterality, "Lateral")){
    first_paragraph_list$positioning <- paste(glue("{str_to_title(he_or_she)} was then positioned in the lateral position on the OR table and all bony prominences were appropriately padded."))
  }else{
    first_paragraph_list$positioning <- paste(glue("{str_to_title(he_or_she)} was then positioned supine on the OR table and all bony prominences were appropriately padded."))
  }
  
  first_paragraph_list$timeout <- "After prepping and draping in the standard fashion, a surgical timeout was performed."
  
  if(multiple_position_procedure == "posterior_first"){
    first_paragraph_list <- list()
    if(str_detect(anterior_approach_laterality, "Lateral")){
      first_paragraph_list$positioning <- paste(glue("{str_to_title(he_or_she)} was then positioned in the lateral position on the OR table and all bony prominences were appropriately padded."))
    }else{
      first_paragraph_list$positioning <- paste(glue("{str_to_title(he_or_she)} was then positioned supine on the OR table and all bony prominences were appropriately padded."))
    }
  }

  if(any(anterior_plate_revision_df$prior_plate_status == "removed")){
    anterior_plate_revision_levels_df <- anterior_plate_revision_df %>%
      filter(prior_plate_status == "removed") %>%
      select(level) %>%
      mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level))
    
    proximal_exposure_level <- all_objects_to_add_df %>%
      union_all(anterior_plate_revision_levels_df) %>%
      filter(vertebral_number == min(vertebral_number)) %>%
      mutate(vertebral_number = round(vertebral_number - 0.25, 0)) %>%
      mutate(level = if_else(vertebral_number >=25, "S1", level)) %>%
      select(vertebral_number) %>%
      distinct() %>%
      mutate(level = jh_get_vertebral_level_function(number = vertebral_number)) %>%
      select(level) %>%
      distinct()
    
    distal_exposure_level <- all_objects_to_add_df %>%
      union_all(anterior_plate_revision_levels_df) %>%
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
    
  }else{
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
  }
  
  ## Approach for cervical vs thoracic/lumbar
  if(max(all_objects_to_add_df$vertebral_number)<11){
    approach_statement <- if_else(approach_statement == "none", "", approach_statement)
    
    first_paragraph_list$surgical_approach <- paste(glue("A standard {anterior_approach_laterality} Smith Robinson approach was utilized to get to the anterior cervical spine. The skin, subcutaneous tissue were incised, the platysma was transected, and then blunt dissection was carried out between the sternocleidomastoid and carotid sheath laterally, and trachea and esophagus  medially, down to the prevertebral fascia. Once the anterior spine was palpated, fluoroscopy was used to localize and confirm levels. "), 
                                                    glue("The longus coli was elevated bilaterally from {proximal_exposure_level$level[[1]]} proximally and to {distal_exposure_level$level[[1]]} distally. Once exposure was adequate, the deep retractors were placed into the anterior spine. "),
                                                    approach_statement)
  }else{
    
    if(anterior_approach_laterality == "Lateral Antepsoas"){
      first_paragraph_list$surgical_approach <- glue("Fluoroscopy was used to obtain a perfect AP and lateral xray and mark the levels. The skin and subcutaneous fat was incised in line with the disk space. The aponeurosis of the external obliques were incised and the muscle fibers divided. The internal obliques were divided, and the transversus abdominis was then divided. I then bluntly dissected the plane between the retroperitoneal fat and the psoas fascia, and the peritoneal cavity was retracted anteriorly. The surface of the psoas was identified and followed down to the vertebral body and I remained anterior to the psoas. Once I was down to the spine, a pin was placed to confirm levels using xray. The dilators were then inserted sequentially and EMG was used to test each dilator. Once the largest dilator had been placed, the retractors were inserted. ")
      
    }else if(anterior_approach_laterality == "Lateral Transpsoas"){
      first_paragraph_list$surgical_approach <- glue("Fluoroscopy was used to obtain a perfect AP and lateral xrays and mark the levels. The skin and subcutaneous fat was incised in line with the disk space. The aponeurosis of the external obliques were incised and the muscle fibers divided. The internal obliques were divided, and the transversus abdominis was then divided. I then bluntly dissected the plane between the retroperitoneal fat and the psoas fascia, and the peritoneal cavity was retracted anteriorly. The surface of the psoas was identified and followed down to the vertebral body. Once I was down to the spine, a pin was placed to confirm levels using xray. The dilators were then inserted sequentially and EMG was used to test each dilator to confirm I was not impinging the lumbar plexus. Once the largest dilator had been placed, the retractor was inserted and I used triggered EMG and direct visualization to confirm I had not trapped any of the lumbar plexus. ")
      
    }else{
      first_paragraph_list$surgical_approach <- glue("The anterior approach to the spine was carried out with assistance from the vascular surgeon. A {anterior_approach_laterality} approach was carried out down toward the spine. Once the approach was complete, retractors were placed and the surgical levels were confirmed using fluoroscopy. ")
      
    }
  }
  
  # ###### LOCAL ANESTHESIA
  # if(str_detect(string = str_to_lower(local_anesthesia), pattern = "exposure")){ 
  #   first_paragraph_list$local_anesthesia <- glue("{str_to_sentence(local_anesthesia)}")
  # }
   
  procedure_details_list$approach_statement <- glue_collapse(x = first_paragraph_list, sep = " ")
  
  ################## PROCEDURE PARAGRAPHS ##################
  
  if(nrow(anterior_plate_revision_df)>0){
    
    if(any(anterior_plate_revision_df$prior_plate_status == "removed")){
      plate_removed_vertebral_bodies_vector <- anterior_plate_revision_df %>%
        filter(prior_plate_status == "removed") %>%
        select(level) %>%
        separate(level, into = c("proximal", "distal")) %>%
        pivot_longer(cols = c(proximal, distal), names_to = "region", values_to = "level") %>%
        select(level) %>%
        distinct()%>%
        as_vector()
      
      plate_removed_levels <- (anterior_plate_revision_df %>%
                                 filter(prior_plate_status == "removed"))$level
      
      plate_removal_statement <- glue("I then proceeded with the removal of the prior instrumentation. Once the plate was fully exposed, the screws were removed from the {glue_collapse(x = plate_removed_vertebral_bodies_vector, sep = ', ', last = ' and ')} bodies without difficulty. The plate spanning {glue_collapse(x = plate_removed_levels, sep = ', ', last = ' and ')} was then removed. ")
    
    }else{
      plate_removal_statement <- " "
    }
    
    if(any(anterior_plate_revision_df$prior_plate_status == "retained")){
      plate_retained_levels <- (anterior_plate_revision_df %>%
                                 filter(prior_plate_status == "retained"))$level
      
      plate_retained_statement <- glue("The plate spanning {glue_collapse(x = plate_retained_levels, sep = ', ', last = ' and ')} was left in place. ")
    }else{
      plate_retained_statement <- " "
      }

    
    procedure_details_list$revision_paragraph <- glue("{plate_removal_statement} {plate_retained_statement}")
    
  }
  
  ################### PROCEDURE PARAGRAPHS ##################
  
  procedure_details_list$procedures <- all_anterior_procedures_paragraphs_function(all_objects_to_add_df = all_objects_to_add_df, 
                                                                                   bone_graft_df = anterior_biologics_df, 
                                                                                   approach = anterior_approach_laterality)
  
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
  
  
  if(multiple_position_procedure == "anterior_first"){
    procedure_details_list$final_paragraph <- glue("At the conclusion of the case, all counts were correct. {neuromonitoring_list$neuromonitoring_signal_stability} I was personally present for the entirety of the anterior portion, including the {procedures_listed}.")
  }else{
    procedure_details_list$final_paragraph <- glue("At the conclusion of the case, all counts were correct. {neuromonitoring_list$neuromonitoring_signal_stability} The drapes were removed and {he_or_she} was transferred to the hospital bed. I was personally present for the entirety of the {procedures_listed}.")
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
                                       c2_nerve_transection = "na",
                                       surgical_approach = "Midline",
                                       approach_mis_open = "Open",
                                       approach_robot_nav_xray = "Open",
                                       implant_start_point_method_input = "",
                                       implant_confirmation_method = "Intraoperative Xray was used to confirm the final position of all implants.",
                                       neuromonitoring_list = list(modalities = c(), emg = "", pre_positioning_motors = "", neuromonitoring_signal_stability = ""),
                                       local_anesthesia = "",
                                       complications_list = list(),
                                       revision_decompression_vector = c(),
                                       revision_implants_df = tibble(level = character(), vertebral_number = double(), object = character(), x = double(), y = double(), prior_rod_connected = character(), remove_retain = character()),
                                       left_main_rod_size = "5.5",
                                       left_main_rod_material = "Titanium",
                                       right_main_rod_size = "5.5",
                                       right_main_rod_material = "Titanium",
                                       additional_rods_statement = NULL,
                                       antibiotics = vector(),
                                       antifibrinolytic = "",
                                       prior_fusion_levels_vector = vector(),
                                       instrumentation_removal_vector = vector(),
                                       additional_procedures_vector = NULL,
                                       bmp = 0,
                                       biologics_list = list(),
                                       morselized_autograft_separate = FALSE,
                                       deep_drains = 0,
                                       superficial_drains = 0,
                                       end_procedure_details = NULL,
                                       closure = NULL,
                                       dressing = NULL, 
                                       multiple_position_procedure = "NA",
                                       alignment_correction_technique = " ",
                                       sex = "The patient", 
                                       lateral_mass_screws_after_decompression = "No", 
                                       instruments_used_for_bony_work = "High-speed burr only", 
                                       attending_assistant = ""){
  
  he_or_she <- case_when(str_to_lower(sex) == "male" ~ "he", 
                         str_to_lower(sex) == "female" ~ "she", 
                         TRUE ~ "the patient")
  his_or_her <- case_when(str_to_lower(sex) == "male" ~ "his", 
                         str_to_lower(sex) == "female" ~ "her", 
                         TRUE ~ "the patient's")
  
  if(nrow(revision_implants_df)>0 & nrow(all_objects_to_add_df)>0){
    implant_removal_df <- revision_implants_df %>%
      filter(remove_retain == "remove") %>%
      select(level, remove_retain) %>%
      distinct()
    
    re_inserted_df <- all_objects_to_add_df %>%
      filter(str_detect(object, "screw")) %>%
      left_join(implant_removal_df) %>%
      filter(remove_retain == "remove")
    
    all_objects_to_add_df <- all_objects_to_add_df %>%
      anti_join(re_inserted_df %>% select(-remove_retain)) %>%
      union_all((re_inserted_df %>%
                   mutate(object = "reinsertion_screw")))
    
  } 
  
  
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
                                                                                                revision_implants_df = revision_implants_df, 
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
    first_paragraph_list$antibiotic_statement <- paste0(glue_collapse(antibiotics, sep = ", ", last = " and "), " was administered for preoperative antibiotics.")
  }
  
  if(antifibrinolytic != ""){
    first_paragraph_list$antifibrinolytic <- antifibrinolytic
  }
  
  if(length(neuromonitoring_list$modalities) > 0 & (any(neuromonitoring_list$modalities == "None") == FALSE)){
    first_paragraph_list$spinal_cord_monitoring <- glue("Neuromonitoring needles were inserted by the neurophysiology technologist for monitoring using {glue_collapse(x = neuromonitoring_list$modalities, sep = ', ', last = ' and ')}. ")
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
  if(multiple_position_procedure == "anterior_first"){
    
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
      select(level, vertebral_number) %>%
      union_all(revision_implants_df %>%
                  filter(remove_retain == "remove") %>% 
                  select(level, vertebral_number)) %>%
      filter(vertebral_number == min(vertebral_number)) %>%
      mutate(vertebral_number = round(vertebral_number - 0.25, 0)) %>%
      mutate(level = if_else(vertebral_number >=25, "S1", level)) %>%
      select(vertebral_number) %>%
      distinct() %>%
      mutate(level = jh_get_vertebral_level_function(number = vertebral_number)) %>%
      select(level) %>%
      distinct()
    
    distal_exposure_level <- all_objects_to_add_df %>%
      select(level, vertebral_number) %>%
      union_all(revision_implants_df %>%
                  filter(remove_retain == "remove") %>% 
                  select(level, vertebral_number)) %>%
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
      first_paragraph_list$surgical_approach <- glue("A paraspinal (Wiltse) posterior approach to the spine was performed. Skin was incised with a knife and cautery was used to control any skin bleeding. I dissected down until I identified the muscle layer and bluntly dissected between the multifidus and longissimus intermuscular plane. I exposed proximally to the {proximal_exposure_level$level[1]} level and distally to the {distal_exposure_level$level[1]} level. I manually palpated the transverse process and confirmed the level with xray. ")
      
    }
    if(surgical_approach == "Stab"){
      first_paragraph_list$surgical_approach <- glue("Intraoperative xray was used to confirm levels and stab incisions were made to access each pedicle from the {proximal_exposure_level$level[1]} level and distally to the {distal_exposure_level$level[1]} level.")
      
    }
    
  }else{
    if(nrow(revision_implants_df)>0){
      if(any(revision_implants_df$remove_retain == "remove")){
        proximal_exposure_level <- revision_implants_df %>%
          filter(remove_retain == "remove") %>%
          filter(vertebral_number == min(vertebral_number)) %>%
          mutate(vertebral_number = round(vertebral_number - 0.25, 0)) %>%
          mutate(level = if_else(vertebral_number >=25, "S1", level)) %>%
          select(vertebral_number) %>%
          distinct() %>%
          mutate(level = jh_get_vertebral_level_function(number = vertebral_number)) %>%
          select(level) %>%
          distinct()
        
        distal_exposure_level <- revision_implants_df %>%
          filter(remove_retain == "remove") %>%
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
      }else{
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
      }
      
      first_paragraph_list$surgical_approach <- glue("A standard posterior approach to the spine was performed, exposing proximally to the {proximal_exposure_level$level[1]} level and distally to the {distal_exposure_level$level[1]} level.")
    }else{
      first_paragraph_list$surgical_approach <- glue("A standard posterior approach to the spine was performed, appropriately exposing all necessary levels.")
    }
  } 
  
  if(str_detect(string = str_to_lower(local_anesthesia), pattern = "exposure")){ 
    first_paragraph_list$local_anesthesia <- glue("{str_to_sentence(local_anesthesia)}")
  }
  
  if(length(complications_list) > 0){
    if(any(names(complications_list) == "durotomy")){
      if(str_detect(string = paste(complications_list$durotomy), pattern = "exposure")){
        first_paragraph_list$durotomy <- paste(complications_list$durotomy)
      }
    }
  }
  
  
  procedure_details_list$approach_statement <- glue_collapse(x = first_paragraph_list, sep = " ")
  
  ################### Revision Procedures PARAGRAPHS ##################
  revision_statements_list <- list()
  
  if(nrow(revision_implants_df) > 0){
    procedure_details_list$revision_implants <- revision_implants_paragraph_function(revision_implants_details_df = revision_implants_df)
  }
  
  
  if(any(additional_procedures_vector == "Exploration of prior spinal fusion")){
    if(length(prior_fusion_levels_vector) > 0){
      revision_statements_list$exploration_fusion_statement <- glue("I then proceeded with exploration of the prior fusion. Overlying scar was excised from the posterior elements and the prior fusion at {glue_collapse(prior_fusion_levels_vector, sep = ', ', last = ' and ')} was inspected and examined for any motion.")
    }
  } 
  
  if(any(str_detect(additional_procedures_vector, pattern = "Irrigation"))){
    revision_statements_list$exploration_fusion_statement <- glue("I proceeded with irrigation and debridement of the wound. Starting deep and working superficially, any necrotic appearing tissue was debrided in a layered fashion. The wound was then irrigated with a copious amount of irrigation and the wound was again debrided of any necrotic or ischemic appearing tissue. The wound was again irrigated copiously until I was satisfied with the debridement and appearance of the wound.")
  } 
  
  if(length(revision_statements_list)>0){
    if(nrow(revision_implants_df) > 0){
      procedure_details_list$exploration <- paste("Once the implants were removed and exposure was completed, ",
                                                  glue_collapse(revision_statements_list, sep = " "))
    }else{
      procedure_details_list$exploration <- paste(glue_collapse(revision_statements_list, sep = " "))
      }
  }
  
  
  ################# C2 nerve transection
  if(c2_nerve_transection == "bilateral_transection"){
    procedure_details_list$c2_transection <- glue("To aide in accurate placement of the C1 lateral mass screws, I proceeded with the transection of the C2 nerve roots (greater occipital nerve) bilaterally. The posterior C1 arch was identified and care was taken to stay on the underside of the C1 arch and avoid the vertebral artery. The exposure was carried laterally until the C2 nerve root was encountered on the left and the right. At this point, to allow accurate placement of the C1 screws and minimize risk of postoperative entrapment neuropathy or occipital neuralgia, the C2 nerve root was transected medial to the dorsal root ganglia on the left and the right. This completed the transection of the C2 nerve root/Greater Occipital Nerve. ")
  }
  if(c2_nerve_transection == "left"){
    procedure_details_list$c2_transection <- glue("To aide in accurate placement of the C1 lateral mass screws, I proceeded with the transection of the C2 nerve root (greater occipital nerve) on the left. The posterior C1 arch was identified and care was taken to stay on the underside of the C1 arch and avoid the vertebral artery. The exposure was carried laterally until the C2 nerve root was encountered on the left. At this point, to allow accurate placement of the C1 screws and minimize risk of postoperative entrapment neuropathy or occipital neuralgia, the C2 nerve root was transected medial to the dorsal root ganglia on the left. This completed the transection of the C2 nerve root/Greater Occipital Nerve. ")
  }
  if(c2_nerve_transection == "right"){
    procedure_details_list$c2_transection <- glue("To aide in accurate placement of the C1 lateral mass screws, I proceeded with the transection of the C2 nerve root (greater occipital nerve) on the right. The posterior C1 arch was identified and care was taken to stay on the underside of the C1 arch and avoid the vertebral artery. The exposure was carried laterally until the C2 nerve root was encountered on the right. At this point, to allow accurate placement of the C1 screws and minimize risk of postoperative entrapment neuropathy or occipital neuralgia, the C2 nerve root was transected medial to the dorsal root ganglia on the right. This completed the transection of the C2 nerve root/Greater Occipital Nerve. ")
  }
  if(c2_nerve_transection == "bilateral_preserved"){
    procedure_details_list$c2_transection <- glue("I proceeded with the exposure of the C1-C2 joint and the C1 lateral masses. The posterior C1 arch was identified, and care was taken to stay on the underside of the C1 arch and avoid the vertebral artery. The exposure was carried laterally until the C2 nerve root was encountered. Bipolar cautery and hemostatic agents were used to aide in hemostasis of the venous plexus. A penfield was used to dissect the cranial and caudal borders of the C2 nerve root. The caudal aspect of the lateral posterior C1 arch was burred to allow a path to the C1 lateral mass just cranial to the C2 nerve root. The C1-C2 joint was exposed caudal to the C2 nerve root. ")
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
  
  if(length(complications_list) > 0){
    if(any(names(complications_list) == "durotomy")){
      if(str_detect(string = paste(complications_list$durotomy), pattern = "exposure") == FALSE){
        procedure_details_list$durotomy <- paste(complications_list$durotomy)
      }
    }
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
        TRUE ~ glue("The rods were placed into position and secured with set screws. "))
      
      if(nrow(revision_implants_df) > 0){
        if(any(revision_implants_df$prior_rod_connected == "yes")){
          rod_statements_list$revision_rod_connector <- paste("Rod connectors were used to connect the new rod to the previously placed construct that was left in place.")
        }
      }
      
      rod_statements_list$additional_rods_statement <- additional_rods_statement
      if(any(additional_procedures_vector == "Open treatment of vertebral fracture")){
        rod_statements_list$set_screws <- glue("The rod was used to correct the alignment due to the fracture. Set screws were then tightened with a final tightener to the appropriate torque.")
      }else{
        rod_statements_list$set_screws <- glue("The set screws were then tightened with a final tightener to the appropriate torque.")
      }
      
      if(nrow(crosslink_df) > 0){
        rod_statements_list$crosslink <- glue("To increase the rigidity of the construct, a crosslink connector was applied at the level of {glue_collapse(crosslink_df$level, sep = ', ', last = ' and ')}.")
      }
      
      rod_statements_list$xrays <- glue("Intraoperative Xray was used to confirm the final position of all implants and to confirm appropriate alignment had been achieved.")
    
      if(any(additional_procedures_vector == "Open treatment of vertebral fracture")){
        rod_statements_list$final <- glue("This completed the instrumentation of {glue_collapse(x = instrumented_levels_vector, sep = ', ', last = ', and ')}, and the open treatment of the vertebral fracture.")
      }else{
        rod_statements_list$final <- glue("This completed the instrumentation of {glue_collapse(x = instrumented_levels_vector, sep = ', ', last = ', and ')}.") 
      }
      
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
  
  procedure_details_list <- map(.x = procedure_details_list, .f = ~str_replace_all(string = .x, pattern = "  |   |    ", replacement = " "))
  
  
  ### FINAL PARAGRAPH ####
  
  procedures_listed <- op_note_procedures_present_listed_function(objects_added_df = all_objects_to_add_df, 
                                                                  revision_decompression_vector = revision_decompression_vector, 
                                                                  fusion_levels_vector = fusion_levels_df$level, 
                                                                  additional_procedures_performed_vector = additional_procedures_for_numbered_list)
  
  if(str_length(attending_assistant)>2){
    attending_assistant_statement <- glue("Due to the complexity of the case and no immediately available qualified resident, Dr. {attending_assistant} assisted in all portions of the procedure, including {procedures_listed}. Dr. {attending_assistant}'s assistance was critical to move the procedure along quicker and with less blood loss, and therefore safer.")
  }else{
    attending_assistant_statement <- ""
  }
  
  procedure_details_list$final_paragraph <- glue("At the conclusion of the case, all counts were correct. {neuromonitoring_list$neuromonitoring_signal_stability} The drapes were removed and {he_or_she} was turned uneventfully. I was personally present for the entirety of this portion of the case, including {procedures_listed}. {attending_assistant_statement}")
  
  
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


