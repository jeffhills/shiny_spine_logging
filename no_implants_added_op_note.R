#############-----------------------   No Implants Added OPERATIVE NOTE  ----------------------###############

no_implants_op_note_posterior_function <- function(head_position = NULL,
                                       revision_decompression_vector = NULL,
                                       revision_implants_df = NULL,
                                       antibiotics = NULL,
                                       prior_fusion_levels_vector = NULL,
                                       additional_procedures_vector = NULL,
                                       bone_graft_vector = NULL,
                                       deep_drains = 0,
                                       superficial_drains = 0,
                                       end_procedure_details = NULL,
                                       closure = NULL,
                                       dressing = NULL){
  
  procedure_details_list <- list()
  
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
  
  
  first_paragraph_list$surgical_approach <- glue("A standard posterior approach to the spine was performed, appropriately exposing all necessary levels.")
  
  procedure_details_list$approach_statement <- glue_collapse(x = first_paragraph_list, sep = " ")
  
  
  ################### Revision Procedures PARAGRAPHS ##################
  revision_statements_list <- list()
  
  if(any(additional_procedures_vector == "Exploration of prior spinal fusion")){
    if(length(prior_fusion_levels_vector) > 0){
      revision_statements_list$exploration_fusion_statement <- glue("I then proceeded with exploration of the prior fusion. Overlying scar was excised from the posterior elements and the prior fusion at {glue_collapse(prior_fusion_levels_vector, sep = ', ', last = ' and ')} was inspected and examined for any motion.")
    }
  } 
  
  if(any(str_detect(additional_procedures_vector, pattern = "Irrigation"))){
    revision_statements_list$irrigation_debridement <- glue("I proceeded with irrigation and debridement of the wound. Starting deep and working superficially, any necrotic appearing tissue was debrided in a layered fashion. The wound was then irrigated with a copious amount of irrigation and the wound was again debrided of any necrotic or ischemic appearing tissue. The wound was again copiously irrigated until I was satisfied with the debridement and appearance of the wound.")
  } 
  
  if(length(revision_statements_list)>0){
    procedure_details_list$exploration <- paste("Once the exposure was completed, ",
                                                glue_collapse(revision_statements_list, sep = " "))
  }
  
  if(nrow(revision_implants_df) > 0){
    procedure_details_list$revision_implants <- revision_implants_paragraph_function(revision_implants_details_df = revision_implants_df)
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
    closure_statements_list$superficial_closure <- paste("The subdermal layer was closed and",
                                                         str_to_lower(glue_collapse(closure, sep = ', ', last = ' and ')),
                                                         if_else(length(closure) == 1, "was", "were"),
                                                         "used to close the skin layer."
    )
  }else{
    closure_statements_list$superficial_closure <- "The subdermal layers and skin layers were then closed."
    
  }
  
  closure_statements_list$dressing <- str_replace_all(str_to_sentence(glue("Lastly, {glue_collapse(dressing, sep = ', ', last = ', and ')} was applied to the surgical site.")), pattern = "steristrips was", replacement = "steristrips were")
  
  procedure_details_list$closure <- glue_collapse(closure_statements_list, sep = " ")
  
  procedure_details_list <- map(.x = procedure_details_list, .f = ~str_replace_all(string = .x, pattern = "a 8", replacement = "an 8"))
  
  ### FINAL PARAGRAPH ####

  procedure_details_list$final_paragraph <- str_to_sentence(glue("At the conclusion of the case, all counts were correct. The drapes were removed and the patient was turned onto the hospital bed uneventfully. I was personally present for the entirety of the {glue_collapse(x = additional_procedures_vector, sep = ', ', last = ' and ')}."))
  
  procedure_paragraphs <- glue_collapse(x = procedure_details_list, sep = "\n\n")
  
  ### Procedures numbered
  procedures_numbered_df <- tibble(procedure = additional_procedures_vector) %>%
    mutate(numbered = row_number()) %>%
    mutate(procedures_numbered = paste0(numbered, ". ", procedure))
  
  procedures_numbered_list <- glue_collapse(x = procedures_numbered_df$procedures_numbered, sep = "\n")
  
  
  return(list(procedure_details_paragraph = procedure_paragraphs, 
              procedures_numbered_paragraph = procedures_numbered_list))
  
}



#############-----------------------               End              ----------------------###############


#############-----------------------   PROCEDURES PERFORMED NUMBERED SECTION  ----------------------###############



############################################################################################
no_implants_op_note_procedures_performed_numbered_function <- function(additional_procedures_performed_vector = NULL){
  
  if(is.null(additional_procedures_performed_vector)){
    additional_procedures_performed_vector <- c(" ")
  }
  procedures_numbered_df <- tibble(procedure_performed_statement = additional_procedures_performed_vector) %>%
    mutate(count = row_number()) %>%
    mutate(procedures_performed_numbered = glue("{count}. {procedure_performed_statement}")) %>%
    select(procedures_performed_numbered)
  # if(length(revision_decompression_vector) > 0){
  #   summary_nested_df <- objects_added_df %>%
  #     mutate(revision_levels_vector = list(revision_decompression_vector)) %>%
  #     select(level, vertebral_number, approach, category, object, side, revision_levels_vector, implant_statement, screw_size_type) %>%
  #     mutate(revision_level = map2(.x = level, .y = revision_levels_vector, .f = ~ str_detect(string = .x, pattern = .y))) %>%
  #     select(-revision_levels_vector) %>%
  #     mutate(revision_level = map(.x = revision_level, .f = ~ any(.x))) %>%
  #     unnest() %>%
  #     replace_na(list(implant_statement = " ", screw_size_type = " ", revision_level = FALSE)) %>%
  #     mutate(revision_label = paste0("revision_", object)) %>%
  #     mutate(object = if_else(revision_level == FALSE, object, if_else(category == "decompression", revision_label, object))) %>%
  #     select(-revision_label) %>%
  #     mutate(procedure_class = op_note_procedure_performed_summary_classifier_function(object = object)) %>%
  #     mutate(procedures_per_line = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_class)) 
  # }else{
  #   summary_nested_df <- objects_added_df %>%
  #     select(level, object, vertebral_number) %>%
  #     mutate(procedure_class = op_note_procedure_performed_summary_classifier_function(object = object)) %>%
  #     mutate(procedures_per_line = op_note_number_of_paragraphs_for_procedure_category(procedure_cat = procedure_class)) 
  # }
  # 
  # 
  # summary_single_statements <- summary_nested_df %>%
  #   filter(procedures_per_line == "distinct") %>%
  #   mutate(procedure_performed_statement = glue("{procedure_class} at {level}"))%>%
  #   select(procedure_class, procedure_performed_statement)
  # 
  # summary_multiple_nested <- summary_nested_df %>%
  #   filter(procedures_per_line == "combine") %>%
  #   group_by(procedure_class) %>%
  #   nest() %>%
  #   ungroup() %>%
  #   mutate(count = row_number()) %>%
  #   mutate(levels = map(.x = data, .f = ~ extract_levels_function(input_df = .x))) %>%
  #   select(-data) %>%
  #   unnest(levels) %>%
  #   mutate(procedure_performed_statement = glue("{procedure_class} at {levels}")) %>%
  #   mutate(procedure_performed_statement = if_else(procedure_class == "Pelvic instrumentation", paste("Instrumentation of the Pelvis with", levels, "fixation"), as.character(procedure_performed_statement))) %>%
  #   select(procedure_class, procedure_performed_statement)
  # 
  # added_procedures_df <- tibble(procedure_performed_statement = additional_procedures_performed_vector) 
  # 
  # procedures_numbered_df <- summary_nested_df %>%
  #   select(procedure_class) %>%
  #   filter(str_detect(string = procedure_class, pattern = "Inferior facetectom") == FALSE) %>%
  #   distinct() %>%
  #   left_join(summary_single_statements %>%
  #               union_all(summary_multiple_nested)) %>%
  #   select(procedure_performed_statement) %>%
  #   add_row(procedure_performed_statement = if_else(length(fusion_levels_vector) == 0, "xx", paste("Posterior Spinal Fusion at", glue_collapse(fusion_levels_vector, sep = ", ", last = " and ")))) %>%
  #   union_all(added_procedures_df) %>%
  #   filter(procedure_performed_statement !="xx") %>%
    # mutate(count = row_number()) %>%
    # mutate(procedures_performed_numbered = glue("{count}. {procedure_performed_statement}")) %>%
    # select(procedures_performed_numbered)
  
  return(glue_collapse(procedures_numbered_df$procedures_performed_numbered, sep = "\n"))
  
}