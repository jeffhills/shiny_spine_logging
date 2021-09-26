
############################################# ANTERIOR OPERATIVE NOTE ####################################
############################################# ANTERIOR OPERATIVE NOTE ####################################
############################################# ANTERIOR OPERATIVE NOTE ####################################
############################################# ANTERIOR OPERATIVE NOTE ####################################

acdf_statement_function <- function(level, interbody_height = 8, interbody_device = "cage"){
  initial_level_statement <- glue("I then began the anterior cervical diskectomy and fusion procedure at the {level} level.")
  
  acdf_procedure_statement <- paste("The anterior disk space was smoothed and any osteophyte resected using a combination of a ronguer and burr.",
                                    "Using a combination of knife, currette, pituitary ronguer, and Kerrison rongeurs, the disc was excised and the superior and inferior endplates were lightly decorticated to prepare the endplates for fusion, taking care not to disrupt the cortical endplate.",
                                    "The posterior longitudinal ligament was excised and adequate central decompression was confirmed.",
                                    "Foraminotomies were performed bilaterally to fully decompress the left and the right exiting nerve roots.",
                                    sep = " "
  )
  
  paste(initial_level_statement, acdf_procedure_statement, glue("This completed the anterior cervical diskectomy at {level}."), sep = " ")
}

acdf_interbody_statement_function <- function(level, interbody_height = 8, interbody_device = "cage"){
  initial_level_statement <- glue("I then proceeded with insertion of the interbody implant at the {level} level.")
  
  acdf_procedure_statement <- paste("A trial used to size the disk space and",
                                    glue("the {interbody_height}mm trial fit appropriately. I then inserted a local allograft that was collected from the osteophytes and the {interbody_height}mm height {interbody_device} into the disk space."),sep = " "
  )    
  
  paste(initial_level_statement, acdf_procedure_statement, glue("This completed the insertion of the interbody device and the fusion at {level}."), sep = " ")
}



full_anterior_op_note_generator_function <- function(all_objects_to_add_df,
                                                     anterior_approach_laterality,
                                                     acdf_interbody_height = 8, 
                                                     interbody_device = "cage",
                                                     microscope_statement = "none",
                                                     fusion_levels_df = NULL,
                                                     open_canal_vector = NULL, 
                                                     left_main_rod_size = NULL,
                                                     left_main_rod_material = NULL, 
                                                     right_main_rod_size = NULL, 
                                                     right_main_rod_material = NULL, 
                                                     additional_rods_statement = NULL, 
                                                     antibiotics = NULL,
                                                     bmp = NULL,
                                                     allograft = NULL,
                                                     additional_procedural_details = NULL, 
                                                     deep_drains = 1, 
                                                     superficial_drains = 0,
                                                     end_procedure_details = NULL,
                                                     closure = NULL){
  op_note_df <- all_objects_to_add_df %>%
    select(-object_constructed) %>%
    union_all(fusion_levels_df) %>%
    mutate(object = str_to_lower(object)) %>%
    # mutate(object = str_replace_all(object, "_", " ")) %>%
    filter(approach == "anterior") 
  
  anterior_fusion_df <- op_note_df %>%
    filter(category == "fusion")%>%
    filter(!is.na(level)) %>%
    select(level, vertebral_number, approach, category, object, side) %>%
    arrange(vertebral_number)
  
  levels_df <- op_note_df %>%
    filter(vertebral_number == min(vertebral_number) | vertebral_number == max(vertebral_number)) %>%
    mutate(level = if_else(vertebral_number >=25, "S1", level)) %>%
    select(level, vertebral_number) %>%
    distinct() %>%
    arrange(vertebral_number)  
  
  
  levels_df <- levels_df %>%
    mutate(vertebral_number = if_else(str_detect(level, "-"), vertebral_number - 0.5, vertebral_number)) %>%
    union_all(levels_df) %>% 
    mutate(vertebral_number = round(vertebral_number, 0)) %>%
    select(vertebral_number) %>%
    left_join(levels_numbered_df) %>%
    select(level, vertebral_number) %>%
    distinct()
  
  levels_range_vector <- levels_df %>%
    filter(vertebral_number == min(vertebral_number) | vertebral_number == max(vertebral_number)) %>%
    select(level) %>%
    as_vector()
  
  procedure_details_list <- list()
  
  procedures_performed_list <- list()
  
  
  if(length(antibiotics) == 0){
    antibiotic_statement <- "Preoperative Antibiotics were administered."
  }else{
    if(antibiotics == "None (Antibiotics were held)"){
      antibiotic_statement <- "Preoperative antibiotics were held until tissue cultures could be obtained. "
    }else{
      antibiotic_statement <- paste("Preoperative Antibiotics were administered including ", glue_collapse(antibiotics, sep = ", ", last = " and "), ".")
    }
  }
  
  
  if(any(str_detect(additional_procedural_details, "Halo"))){
    head_statement <- "A Halo was applied to the patient's skull for positioning and the pins were sequentially tightened to the appropriate torque."
  }else{
    if(any(str_detect(additional_procedural_details, "Tongs"))){
      head_statement <- "Cranial tongs were applied to the patient's skull for positioning and an appropriate weight was selected for cranial traction. The head, shoulders and arms were padded and once we were satisfied with the positioning, the patient was secured to the operating table. "
    }else{
      head_statement <- "The head, shoulders and arms were padded and once we were satisfied with the positioning, the patient was secured to the operating table. "
    }
    
  } 
  
  if(any(str_detect(additional_procedural_details, "Spinal Cord Monitoring"))){
    spinal_cord_monitoring <- "Spinal Cord Monitoring needles were inserted by the neurophysiology technologist."
  }else{
    spinal_cord_monitoring <- NULL
  }
  
  ## Approach for cervical vs thoracic/lumbar
  if(max(levels_df$vertebral_number)<11){
    microscope_statement <- if_else(microscope_statement == "none", "", microscope_statement)
    
    spinal_approach <- paste("A standard ",
                             anterior_approach_laterality,
                             "-sided Smith Robinson approach was utilized to get to the anterior cervical spine. ", 
                             "The skin, subcutaneous tissue were incised, the platysma was transected, and then blunt dissection was carried out between the sternocleidomastoid and carotid sheath laterally, and trachea and esophogus medially, down to the prevertebral fascia. ", 
                             "Once the anterior spine was palpated, fluoroscopy was used to localize and confirm levels.", 
                             glue("The longus coli was elevated bilaterally from {levels_range_vector[[1]]} proximally and to {levels_range_vector[[2]]} distally."), 
                             microscope_statement, sep = ""
    )
  }else{
    spinal_approach <- glue("The anterior approach to the spine was carried out with assistance from our vascular surgeon on the {anterior_approach_laterality} side. Once the approach was complete, levels were confirmed using fluoroscopy. ")
  }
  
  
  procedure_details_list$approach_statement <- paste("The operative site was identified and marked in the preoperative holding area. " ,
                                                     "The patient was brought to the operating room and transferred to the OR table in the supine position, administered a general anesthetic and endotracheal intubation was performed by the anesthesia team. ", 
                                                     "Additional intravenous access was obtained as indicated. ",
                                                     antibiotic_statement,
                                                     head_statement, 
                                                     spinal_cord_monitoring,
                                                     "After prepping and draping in the standard fashion, a surgical timeout was performed. ", 
                                                     spinal_approach,
                                                     sep = ""
  )
  
  ########### ACDF ########
  if(any(str_detect(string = op_note_df$object, pattern = "diskectomy_fusion"))){
    acdf_df <- op_note_df %>%
      filter(object == "diskectomy_fusion")
    
    procedures_performed_list <- append(procedures_performed_list, values = map(.x =  acdf_df$level, ~ paste("Anterior Cervical Diskectomy and Fusion at ", .x)))
    
    procedure_details_list$acdf_procedures_statements <- glue_collapse(x = pmap(.l = list(..1 = acdf_df$level), 
                                                                                .f = ~ acdf_statement_function(level = ..1)),
                                                                       sep = "\n\n")
    
    
  }
  
  ########### ACDF ########
  if(any(str_detect(string = op_note_df$object, pattern = "diskectomy_fusion"))){
    acdf__interbody_df <- op_note_df %>%
      filter(object == "diskectomy_fusion")
    
    procedures_performed_list <- append(procedures_performed_list, values = map(.x =  acdf__interbody_df$level, ~ paste("Insertion of Interbody Biomechanical Implant at ", .x)))
    
    procedure_details_list$acdf_interbody_statements <- glue_collapse(x = pmap(.l = list(..1 = acdf_df$level), 
                                                                               .f = ~ acdf_interbody_statement_function(level = ..1)),
                                                                      sep = "\n\n")
    
    
  }
  
  
  #### ANTERIOR PLATE
  if(any(str_detect(string = op_note_df$object, pattern = "anterior_plate"))){
    
    anterior_plate_lower_numbers_df <- op_note_df %>%
      filter(object == "anterior_plate") %>%
      arrange(vertebral_number) %>%
      mutate(vertebral_number = vertebral_number - 0.5) %>%
      select(vertebral_number) 
    
    
    anterior_plate_df <- anterior_plate_lower_numbers_df %>%
      filter(vertebral_number == max(vertebral_number)) %>%
      mutate(vertebral_number = vertebral_number + 1) %>%
      union_all(anterior_plate_lower_numbers_df) %>%
      arrange(vertebral_number) %>%
      left_join(levels_numbered_df)
    
    procedures_performed_list <- append(procedures_performed_list, values = paste0("Anterior spinal Instrumentation (plating, distinct from interbody device) of ", 
                                                                                   glue_collapse(anterior_plate_df$level, sep = ", ", last = " and ")))
    
    procedure_details_list$anterior_plating_procedure <- paste0("I then proceeded with anterior spinal instrumentation. A plate was used to provide additional stabilization across the fusion levels. ", 
                                                                "A plate was selected to span ",
                                                                glue_collapse(anterior_plate_df$level, sep = ", ", last = " and "),
                                                                ".",
                                                                "Once we were satisfied with the position of the plate, screws were placed and tightened sequentially into the vertebral bodies, securing the plate in place. ",
                                                                "This completed the anterior spinal instrumentation of ",
                                                                glue_collapse(anterior_plate_df$level, sep = ", ", last = " and "),
                                                                ".")
  }
  ############################# CLOSURE #########################
  
  
  closure_statements_list <- list()
  
  if(any(str_detect(end_procedure_details, pattern = "Vanco"))){
    closure_statements_list$vanc_powder <- "Vancomycin powder was spread throughout the surgical bed."
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
    if(any(str_detect(string = closure, pattern = "Incisional"))){
      closure_vector <- closure[closure!="Incisional Wound Vac"]
      
      closure_statements_list$skin_dressing_statement <- paste("The wound was then closed in a layered fashion.",
                                                               if_else(length(closure_vector) > 0,
                                                                       (paste(glue_collapse(closure_vector, sep = ", ", last = " and "), " were used to close the skin.")),
                                                                       "A subcutaneous layer was used to seal the skin layer."),
                                                               "An incisional wound vac was then placed over the surgical incision.", sep = " ")
    }else{
      closure_vector <- closure
      closure_statements_list$skin_dressing_statement <- paste("The wound was then closed in a layered fashion.",
                                                               if_else(length(closure_vector) > 0,
                                                                       (paste0("A subcutaneous layer and ", 
                                                                               glue_collapse(closure_vector, sep = ", ", last = " and "), 
                                                                               " were used on the skin.")),
                                                                       "A subcutaneous layer was used to seal the skin layer."),
                                                               "A watertight dressing was then placed over the surgical incision.", sep = " ")
      
    }
  }else{
    closure_statements_list$skin_dressing_statement <- paste("The wound was then closed in a layered fashion.",
                                                             "A watertight dressing was then placed over the surgical incision.", sep = " ")
  }
  
  
  
  
  procedure_details_list$closure <- paste("I then proceeded with closure of the wound. ",
                                          glue_collapse(x = closure_statements_list, sep = " "),
                                          " All counts were correct at the conclusion of the case. ",
                                          "I was present and scrubbed for the entire case including ",
                                          glue_collapse(procedures_performed_list, sep = ', ', last = ', and '),
                                          ".",
                                          sep = "")
  
  
  
  
  return(list(procedures_performed_summary_list = procedures_performed_list,
              procedure_details_list = procedure_details_list))
  
}
