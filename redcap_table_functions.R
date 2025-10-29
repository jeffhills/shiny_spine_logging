############ GENERATE PATIENT DETAILS DATAFRAME ###########

redcap_table_patient_details_df_function <- function(last_name_input = "xx", 
                                                     first_name_input = "xx",
                                                     date_of_birth_input = "m/d/y",
                                                     sex_input = "test"){
  tibble(last_name = last_name_input,
                               first_name = first_name_input,
                               date_of_birth = date_of_birth_input,
                               sex = sex_input) 
  
}

### first function for inputting supplemental rods ##
jh_supplement_rods_summarize_for_table_function <- function(side,
                                                            add_accessory_rod_true_false = FALSE, 
                                                            accessory_rod_material = "na", 
                                                            accessory_rod_size = "na",
                                                            add_satellite_rod_true_false = FALSE, 
                                                            satellite_rod_material = "na", 
                                                            satellite_rod_size = "na",
                                                            add_intercalary_rod_true_false = FALSE, 
                                                            intercalary_rod_material = "na", 
                                                            intercalary_rod_size = "na",
                                                            add_linked_rod_true_false = FALSE, 
                                                            linked_rod_material = "na", 
                                                            linked_rod_size = "na",
                                                            add_kickstand_rod_true_false = FALSE, 
                                                            kickstand_rod_material = "na", 
                                                            kickstand_rod_size = "na"){
  
  rod_list <- list()
  
  if(add_accessory_rod_true_false){
    accessory_size <- if_else(accessory_rod_size == "na", "", accessory_rod_size)
    accessory_material <- if_else(accessory_rod_material == "na", "", accessory_rod_material)
    
    rod_list$accessory_rod <- as.character(glue("{str_to_title(side)} Accessory rod, {accessory_rod_size}-{accessory_rod_material}"))
  }
  if(add_satellite_rod_true_false){
    rod_list$satellite_rod <- as.character(glue("{str_to_title(side)} Satellite rod, {satellite_rod_size}-{satellite_rod_material}"))
  }
  if(add_intercalary_rod_true_false){
    rod_list$intercalary_rod <- as.character(glue("{str_to_title(side)} Intercalary rod, {intercalary_rod_size}-{intercalary_rod_material}"))
  }
  if(add_linked_rod_true_false){
    rod_list$linked_rod <- as.character(glue("{str_to_title(side)} Linked rod, {linked_rod_size}-{linked_rod_material}"))
  }
  if(add_kickstand_rod_true_false){
    rod_list$kickstand_rod <- as.character(glue("{str_to_title(side)} Kickstand rod, {kickstand_rod_size}-{kickstand_rod_material}"))
  }
  
  if(length(rod_list)>0){
    as.character(glue_collapse(x = rod_list, sep = " + "))
  }else{
    "none"
  }
  
}

jh_rods_table_add_supplemental_rod_size_material_function <- function(rods_crossing_df_input, 
                                                                      side = "left",
                                                                      main_rod_size = "xx",
                                                                      main_rod_material = "xx",
                                                                      accessory_rod_size = "xx",
                                                                      accessory_rod_material = "xx", 
                                                                      satellite_rod_size = "xx",
                                                                      satellite_rod_material = "xx", 
                                                                      intercalary_rod_size = "xx",
                                                                      intercalary_rod_material = "xx", 
                                                                      linked_rod_size = "xx",
                                                                      linked_rod_material = "xx", 
                                                                      kickstand_rod_size = "xx",
                                                                      kickstand_rod_material = "xx" 
){
  
  accessory_rod_size_input <- if(length(accessory_rod_size) == 0){
    "xx"
  }else{
    accessory_rod_size
  }
  accessory_rod_material_input <- if(length(accessory_rod_material) == 0){
    "xx"
  }else{
    accessory_rod_material
  }
  satellite_rod_size_input <- if(length(satellite_rod_size) == 0){
    "xx"
  }else{
    satellite_rod_size
  }
  satellite_rod_material_input <- if(length(satellite_rod_material) == 0){
    "xx"
  }else{
    satellite_rod_material
  }
  intercalary_rod_size_input <- if(length(intercalary_rod_size) == 0){
    "xx"
  }else{
    intercalary_rod_size
  }
  intercalary_rod_material_input <- if(length(intercalary_rod_material) == 0){
    "xx"
  }else{
    intercalary_rod_material
  }
  linked_rod_size_input <- if(length(linked_rod_size) == 0){
    "xx"
  }else{
    linked_rod_size
  }
  linked_rod_material_input <- if(length(linked_rod_material) == 0){
    "xx"
  }else{
    linked_rod_material
  }
  kickstand_rod_size_input <- if(length(kickstand_rod_size) == 0){
    "xx"
  }else{
    kickstand_rod_size
  }
  kickstand_rod_material_input <- if(length(kickstand_rod_material) == 0){
    "xx"
  }else{
    kickstand_rod_material
  }
  
  if(side == "left") {
    rods_with_size_material_df <- rods_crossing_df_input %>%
      mutate(left_main_rod_size = main_rod_size, 
             left_main_rod_material = main_rod_material) %>% 
      mutate(left_accessory_rod_size = if_else(str_detect(str_to_lower(left_rods_crossing), "accessory"), accessory_rod_size_input, "xx"), 
             left_accessory_rod_material = if_else(str_detect(str_to_lower(left_rods_crossing), "accessory"), accessory_rod_material_input, "xx"),
             
             left_satellite_rod_size = if_else(str_detect(str_to_lower(left_rods_crossing), "satellite"), satellite_rod_size_input, "xx"), 
             left_satellite_rod_material = if_else(str_detect(str_to_lower(left_rods_crossing), "satellite"), satellite_rod_material_input, "xx"),
             
             left_intercalary_rod_size = if_else(str_detect(str_to_lower(left_rods_crossing), "intercalary"), intercalary_rod_size_input, "xx"), 
             left_intercalary_rod_material = if_else(str_detect(str_to_lower(left_rods_crossing), "intercalary"), intercalary_rod_material_input, "xx"),
             
             left_linked_rod_size = if_else(str_detect(str_to_lower(left_rods_crossing), "linked"), linked_rod_size_input, "xx"), 
             left_linked_rod_material = if_else(str_detect(str_to_lower(left_rods_crossing), "linked"), linked_rod_material_input, "xx"),
             
             left_kickstand_rod_size = if_else(str_detect(str_to_lower(left_rods_crossing), "kickstand"), kickstand_rod_size_input, "xx"), 
             left_kickstand_rod_material = if_else(str_detect(str_to_lower(left_rods_crossing), "kickstand"), kickstand_rod_material_input, "xx")) 
  }else if(side == "right"){
    rods_with_size_material_df <-  rods_crossing_df_input %>%
      mutate(right_main_rod_size = main_rod_size, 
             right_main_rod_material = main_rod_material) %>%
      mutate(right_accessory_rod_size = if_else(str_detect(str_to_lower(right_rods_crossing), "accessory"), accessory_rod_size_input, "xx"), 
             right_accessory_rod_material = if_else(str_detect(str_to_lower(right_rods_crossing), "accessory"), accessory_rod_material_input, "xx"),
             
             right_satellite_rod_size = if_else(str_detect(str_to_lower(right_rods_crossing), "satellite"), satellite_rod_size_input, "xx"), 
             right_satellite_rod_material = if_else(str_detect(str_to_lower(right_rods_crossing), "satellite"), satellite_rod_material_input, "xx"),
             
             right_intercalary_rod_size = if_else(str_detect(str_to_lower(right_rods_crossing), "intercalary"), intercalary_rod_size_input, "xx"), 
             right_intercalary_rod_material = if_else(str_detect(str_to_lower(right_rods_crossing), "intercalary"), intercalary_rod_material_input, "xx"),
             
             right_linked_rod_size = if_else(str_detect(str_to_lower(right_rods_crossing), "linked"), linked_rod_size_input, "xx"), 
             right_linked_rod_material = if_else(str_detect(str_to_lower(right_rods_crossing), "linked"), linked_rod_material_input, "xx"),
             
             right_kickstand_rod_size = if_else(str_detect(str_to_lower(right_rods_crossing), "kickstand"), kickstand_rod_size_input, "xx"), 
             right_kickstand_rod_material = if_else(str_detect(str_to_lower(right_rods_crossing), "kickstand"), kickstand_rod_material_input, "xx")) 
    
  }else{
    rods_with_size_material_df <-  rods_crossing_df_input
  }
  rods_with_size_material_df %>%
    select_if(~ !all(. == "xx"))
} 

################# GENERATE SURGICAL DETAILS DATAFRAME #################

redcap_table_surgical_details_df_function <- function(all_objects_df_input = tibble(level = character(), vertebral_number = double(), side = character(), object = character()),
                                                      hospital_input = "na",
                                                      hospital_mrn_input = "na", 
                                                      date_of_birth_input = "1900-01-01",
                                                      date_of_surgery_input = "1900-01-01",
                                                      primary_surgeon_first_name_input = "na",
                                                      primary_surgeon_last_name_input = "na",
                                                      surgical_assistants_input = "na",
                                                      symptoms_input = "na",
                                                      primary_diagnosis_input = "na",
                                                      indications_input = "na",
                                                      asa_class_input = "na",
                                                      anesthesia_input = "na",
                                                      primary_revision_input = "na",
                                                      revision_indication_input = "na",
                                                      prior_fusion_levels_input = NULL,
                                                      left_revision_implants_removed_input = c(),
                                                      right_revision_implants_removed_input = c(),
                                                      staged_procedure_input = FALSE,
                                                      stage_number = 1,
                                                      approach_sequence_input = "na",
                                                      spine_approach_input = "na",
                                                      approach_specified_anterior_input = "na",
                                                      approach_specified_posterior_input = "na",
                                                      posterior_fusion_levels_confirmed_input = c(),
                                                      anterior_fusion_levels_confirmed_input = c(),
                                                      left_main_rod_size_input = "na",
                                                      right_main_rod_size_input = "na",
                                                      left_main_rod_material_input = "na",
                                                      right_main_rod_material_input = "na",
                                                      add_left_accessory_rod_input = FALSE,
                                                      add_left_satellite_rod_input = FALSE,
                                                      add_left_intercalary_rod_input = FALSE,
                                                      add_left_linked_rod_input = FALSE,
                                                      add_left_kickstand_rod_input = FALSE,
                                                      left_accessory_rod_material_input = "na",
                                                      left_accessory_rod_size_input = "na",
                                                      left_satellite_rod_material_input = "na",
                                                      left_satellite_rod_size_input = "na",
                                                      left_intercalary_rod_material_input = "na",
                                                      left_intercalary_rod_size_input = "na",
                                                      left_linked_rod_material_input = "na",
                                                      left_linked_rod_size_input = "na",
                                                      left_kickstand_rod_material_input = "na",
                                                      left_kickstand_rod_size_input = "na",
                                                      
                                                      add_right_accessory_rod_input = FALSE,
                                                      add_right_satellite_rod_input = FALSE,
                                                      add_right_intercalary_rod_input = FALSE,
                                                      add_right_linked_rod_input = FALSE,
                                                      add_right_kickstand_rod_input = FALSE,
                                                      right_accessory_rod_material_input = "na",
                                                      right_accessory_rod_size_input = "na",
                                                      right_satellite_rod_material_input = "na",
                                                      right_satellite_rod_size_input = "na",
                                                      right_intercalary_rod_material_input = "na",
                                                      right_intercalary_rod_size_input = "na",
                                                      right_linked_rod_material_input = "na",
                                                      right_linked_rod_size_input = "na",
                                                      right_kickstand_rod_material_input = "na",
                                                      right_kickstand_rod_size_input = "na",
                                                      
                                                      crosslink_connectors_input = c(),
                                                      anterior_bmp_dose_reactive_input = "na",
                                                      anterior_bone_graft_input = "",
                                                      anterior_allograft_amount_input = "",
                                                      anterior_biologics_input = "",
                                                      anterior_bone_marrow_aspirate_volume_input = "",
                                                      anterior_cell_based_allograft_volume_input = "",
                                                      anterior_dbm_volume_input = "",
                                                      anterior_ifactor_volume_input = "",
                                                      anterior_biologics_other_input = "",
                                                      posterior_bmp_dose_reactive_input = "",
                                                      posterior_biologics_input = c(),
                                                      posterior_bone_graft_input = "",
                                                      posterior_allograft_amount_input = "",
                                                      posterior_pearlmatrix_volume_input = "",
                                                      posterior_bone_marrow_aspirate_volume_input = "",
                                                      posterior_cell_based_allograft_volume_input = "",
                                                      posterior_dbm_volume_input = "",
                                                      posterior_ifactor_volume_input = "",
                                                      posterior_biologics_other_input = "",
                                                      intraoperative_complications_vector_input = "",
                                                      other_intraoperative_complications_input = "",
                                                      implant_manufacturer_input = "",
                                                      operative_note_text_input = "na"
                                                      ){
    surgery_details_list <- list()
    
      ##########   date_of_surgery #############
      surgery_details_list$date_of_surgery <- as.character(date_of_surgery_input)
      
      ##########   Hospital #############
      surgery_details_list$hospital <- as.character(hospital_input)
      
      surgery_details_list$hospital_mrn <- as.character(hospital_mrn_input)
      
      ##########   age #############
      surgery_details_list$age <- if_else(paste(date_of_birth_input) == "1900-01-01", "--", as.character(trunc((date_of_birth_input %--% date_of_surgery_input) / years(1))))
      
      ##########   attending #############
      surgery_details_list$attending <- paste(primary_surgeon_first_name_input, primary_surgeon_last_name_input)
      
      ##########   assisting #############
      surgery_details_list$assisting <- surgical_assistants_input
      
      ##########   symptoms #############
      if(length(symptoms_input)>0){
        surgery_details_list$symptoms <- glue_collapse(str_to_lower(symptoms_input), sep = "; ") 
      }
      
      ##########   DIAGNOSIS #############
      
      if(length(primary_diagnosis_input) >0){
        
        surgery_details_list$diagnosis_category <- glue_collapse((tibble(diagnosis = primary_diagnosis_input) %>%
                                                                    left_join(spine_codes_df) %>%
                                                                    select(section) %>%
                                                                    mutate(str_to_title(section)))$section, sep = "; ")
        
        surgery_details_list$diagnosis <- glue_collapse(str_to_lower(primary_diagnosis_input), sep = "; ")
        
        surgery_details_list$diagnosis_icd10_code <- glue_collapse((tibble(diagnosis = primary_diagnosis_input) %>%
                                                                      left_join(spine_codes_df) %>%
                                                                      select(icd_10_code))$icd_10_code, sep = "; ")
        
      }
      
      ##########   indications #############
      if(indications_input != " "){
        surgery_details_list$indications <- indications_input
      }
      
      ##########   asa_class #############
      if(length(asa_class_input) >0){
        surgery_details_list$asa_class <- asa_class_input
      }
      
      ##########   anesthesia  #############
      if(length(anesthesia_input) >0){
        surgery_details_list$anesthesia <- anesthesia_input
      }
      
      ##########   primary_revision  #############
      surgery_details_list$primary_revision <- primary_revision_input
      
      if(length(revision_indication_input) >0){
        surgery_details_list$revision_indication <- glue_collapse(revision_indication_input, sep = "; ") 
      }
      
      ##########   prior_fusion_levels #############
      if(length(prior_fusion_levels_input)>0){
        surgery_details_list$prior_fusion_levels <- glue_collapse(prior_fusion_levels_input, sep = "; ")   
      }
      
      # ##########   levels_instrumentation_removed #############
      if(length(left_revision_implants_removed_input) > 0 | length(right_revision_implants_removed_input) > 0){
        removal_df <- tibble(levels_removed = left_revision_implants_removed_input) %>%
          bind_rows(tibble(levels_removed = right_revision_implants_removed_input)) %>%
          distinct() %>%
          mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = levels_removed)) %>%
          arrange(vertebral_number)
        
        surgery_details_list$levels_instrumentation_removed <- glue_collapse(removal_df$levels_removed, sep = ",")
        
      }
      
      ##########   staged_procedure #############
      surgery_details_list$staged_procedure <- if_else(staged_procedure_input == TRUE, "Yes", "No")
      
      ##########   stage_number #############
      if(staged_procedure_input == TRUE){
        surgery_details_list$stage_number <- stage_number
      }
      
      
      ##########   approach #############
      
      
      if(nrow(all_objects_df_input)>0){
        surgery_details_list$main_approach <- approach_sequence_input
      }else{
        surgery_details_list$main_approach <- str_to_lower(spine_approach_input)
      }
      
      if(str_detect(surgery_details_list$main_approach, "anterior")){
        surgery_details_list$anterior_approach <- approach_specified_anterior_input
      }
      if(str_detect(surgery_details_list$main_approach, "posterior")){
        surgery_details_list$posterior_approach <- approach_specified_posterior_input
      }
      
      ##########   fusion performed #############
      surgery_details_list$fusion <- if_else(length(posterior_fusion_levels_confirmed_input) > 0 | length(anterior_fusion_levels_confirmed_input) > 0, "yes", "no")
      
      # surgery_details_list$fusion <- "yes"
      
      ##########   number of fused vertebrae  #############
      surgery_details_list$number_of_fusion_levels <- if_else(surgery_details_list$fusion == "yes",
                                                              paste(length(union(posterior_fusion_levels_confirmed_input, anterior_fusion_levels_confirmed_input))+1),
                                                              "0")
      
      ##########   interspaces_fused #############
      if(surgery_details_list$fusion == "yes"){
        surgery_details_list$interspaces_fused <- glue_collapse(x = keep(.x = levels_vector, .p = ~ .x %in% union(posterior_fusion_levels_confirmed_input, anterior_fusion_levels_confirmed_input)), sep = "; ")
      }
      
      
      ##########   interbody_fusion #############
      if(any(all_objects_df_input$interbody_fusion == "yes")){
        surgery_details_list$interbody_fusion <- "yes"
      }else{
        surgery_details_list$interbody_fusion <- "no"
      }
      
      ##########   interbody_fusion_levels #############
      if(surgery_details_list$interbody_fusion == "yes"){
        interbody_fusion_df <- all_objects_df_input %>%
          filter(interbody_fusion == "yes") %>%
          select(level, body_interspace) %>%
          distinct()   
        
        interbody_fusion_levels <- unique((interbody_fusion_df %>% filter(body_interspace == "interspace"))$level)
        
        if(any(interbody_fusion_df$body_interspace == "body")){
          interbody_fusion_levels <- unique(jh_reorder_levels_function(level_vector = append(interbody_fusion_levels, 
                                                                                             jh_convert_body_levels_to_interspace_vector_function(vertebral_bodies_vector = (interbody_fusion_df %>% filter(body_interspace == "body"))$level))))
        }
        surgery_details_list$number_of_interbody_fusions <- length(interbody_fusion_levels)
        surgery_details_list$interbody_fusion_levels <- glue_collapse(interbody_fusion_levels, sep = "; ")
      }else{
        surgery_details_list$number_of_interbody_fusions <- "0"
      }
      
      
      ##########   UIV  #############
      all_vertebrae_fixation_df <- all_objects_df_input %>%
        # filter(level != "S2AI") %>%
        # filter(level != "Iliac") %>%
        filter(fixation_uiv_liv == "yes") %>%
        select(level, vertebral_number, body_interspace) %>%
        distinct() %>%
        arrange(vertebral_number)
      
      if(any(all_objects_df_input$object == "occipital_screw")){
        surgery_details_list$uiv <- "Occiput"
      }else{
        if(nrow(all_vertebrae_fixation_df) > 0){
          upper_level <- (all_vertebrae_fixation_df %>% filter(vertebral_number == min(vertebral_number)) %>% select(level) %>% distinct())$level
          
          if(jh_check_body_or_interspace_function(upper_level) == "interspace"){
            surgery_details_list$uiv <- jh_get_cranial_caudal_interspace_body_list_function(level = upper_level)$cranial_level
          }else{
            surgery_details_list$uiv <- upper_level
          }
        }else{
          surgery_details_list$uiv <- "not instrumented"
        }    
      }
      
      ##########   LIV  #############
      if(nrow(all_vertebrae_fixation_df) > 0){
        lowest_level <- (all_vertebrae_fixation_df %>% filter(vertebral_number == max(vertebral_number)) %>% select(level) %>% distinct())$level
        
        if(jh_check_body_or_interspace_function(lowest_level) == "interspace"){
          surgery_details_list$liv <- jh_get_cranial_caudal_interspace_body_list_function(level = lowest_level)$caudal_level
        }else if(jh_check_body_or_interspace_function(lowest_level) == "pelvis"){
          surgery_details_list$liv <- "pelvis"
        }else{
          surgery_details_list$liv <- lowest_level
        }
      }else{
        surgery_details_list$liv <- "not instrumented"
      }
      
      ##########   UPPER & LOWER TREATED  #############
      
      # print(all_objects_df_input$level)
      spine_treated_df <- all_objects_df_input %>%
        filter(str_detect(level, "S2AI|Iliac|SI") == FALSE) %>%
        filter(level != "Occiput")
        # filter(level %in% c('C1', 'C1-C2', 'C2', 'C2-C3', 'C3', 'C3-C4', 'C4', 'C4-C5', 'C5', 'C5-C6', 'C6', 'C6-C7', 'C7', 'C7-T1', 'T1', 'T1-T2', 'T2', 'T2-T3', 'T3', 'T3-T4', 'T4', 'T4-T5', 'T5', 'T5-T6', 'T6', 'T6-T7', 'T7', 'T7-T8', 'T8', 'T8-T9', 'T9', 'T9-T10', 'T10', 'T10-T11', 'T11', 'T11-T12', 'T12', 'T12-L1', 'L1', 'L1-L2', 'L2', 'L2-L3', 'L3', 'L3-L4', 'L4', 'L4-L5', 'L5', 'L5-S1', 'S1')
        #        )
      
      spine_treated <- if_else(nrow(spine_treated_df) > 0, TRUE, FALSE)
      
      # print(spine_treated)
      
      if(nrow(spine_treated_df) > 0){
        ##### UPPER TREATED #####
        surgery_details_list$upper_treated_vertebrae <- (spine_treated_df %>% filter(vertebral_number == min(vertebral_number)) %>% select(level) %>% distinct())$level[[1]]
        
        if(jh_check_body_or_interspace_function(surgery_details_list$upper_treated_vertebrae) == "interspace"){
          surgery_details_list$upper_treated_vertebrae <- jh_get_cranial_caudal_interspace_body_list_function(level = surgery_details_list$upper_treated_vertebrae)$cranial_level
        }
        
        ######### LOWER TREATED ######
        surgery_details_list$lower_treated_vertebrae <- (spine_treated_df %>% filter(vertebral_number == max(vertebral_number)) %>% select(level) %>% distinct())$level[[1]]
        
        if(jh_check_body_or_interspace_function(surgery_details_list$lower_treated_vertebrae) == "interspace"){
          surgery_details_list$lower_treated_vertebrae <- jh_get_cranial_caudal_interspace_body_list_function(level = surgery_details_list$lower_treated_vertebrae)$caudal_level
        }
        
      }else{
        surgery_details_list$upper_treated_vertebrae <- "none"
        surgery_details_list$lower_treated_vertebrae <- "none"
      }
      
      
      ##########   PELVIC FIXATION  #############
      surgery_details_list$pelvic_fixation <- if_else(any(str_detect(string = all_objects_df_input$object, pattern = "pelvic_screw")), "yes", "no")
      
      if(surgery_details_list$pelvic_fixation == "yes"){
        surgery_details_list$pelvic_fixation_screws <- glue_collapse((all_objects_df_input %>% filter(str_detect(object, "pelvic_screw")))$level, sep = "; ")
      }
      
      ##########   RODS  #############
      
       ##########   RODS  #############
  
  if(str_detect(str_to_lower(surgery_details_list$main_approach), "posterior")){
    surgery_details_list$left_rod <- if_else(left_main_rod_size_input == "None", "None", paste(left_main_rod_size_input, left_main_rod_material_input))
    surgery_details_list$right_rod <- if_else(right_main_rod_size_input == "None", "None", paste(right_main_rod_size_input, right_main_rod_material_input))
    
    surgery_details_list$left_supplemental_rods <- jh_supplement_rods_summarize_for_table_function(side = "left",
                                                                                                   add_accessory_rod_true_false = add_left_accessory_rod_input, 
                                                                                                   accessory_rod_material = left_accessory_rod_material_input, 
                                                                                                   accessory_rod_size = left_accessory_rod_size_input,
                                                                                                   add_satellite_rod_true_false = add_left_satellite_rod_input, 
                                                                                                   satellite_rod_material = left_satellite_rod_material_input, 
                                                                                                   satellite_rod_size = left_satellite_rod_size_input,
                                                                                                   add_intercalary_rod_true_false = add_left_intercalary_rod_input, 
                                                                                                   intercalary_rod_material = left_intercalary_rod_material_input, 
                                                                                                   intercalary_rod_size = left_intercalary_rod_size_input,
                                                                                                   add_linked_rod_true_false = add_left_linked_rod_input, 
                                                                                                   linked_rod_material = left_linked_rod_material_input, 
                                                                                                   linked_rod_size = left_linked_rod_size_input,
                                                                                                   add_kickstand_rod_true_false = add_left_kickstand_rod_input, 
                                                                                                   kickstand_rod_material = left_kickstand_rod_material_input, 
                                                                                                   kickstand_rod_size = left_kickstand_rod_size_input)
    
    surgery_details_list$right_supplemental_rods <- jh_supplement_rods_summarize_for_table_function(side = "right",
                                                                                                    add_accessory_rod_true_false = add_right_accessory_rod_input, 
                                                                                                    accessory_rod_material = right_accessory_rod_material_input, 
                                                                                                    accessory_rod_size = right_accessory_rod_size_input,
                                                                                                    add_satellite_rod_true_false = add_right_satellite_rod_input, 
                                                                                                    satellite_rod_material = right_satellite_rod_material_input, 
                                                                                                    satellite_rod_size = right_satellite_rod_size_input,
                                                                                                    add_intercalary_rod_true_false = add_right_intercalary_rod_input, 
                                                                                                    intercalary_rod_material = right_intercalary_rod_material_input, 
                                                                                                    intercalary_rod_size = right_intercalary_rod_size_input,
                                                                                                    add_linked_rod_true_false = add_right_linked_rod_input, 
                                                                                                    linked_rod_material = right_linked_rod_material_input, 
                                                                                                    linked_rod_size = right_linked_rod_size_input,
                                                                                                    add_kickstand_rod_true_false = add_right_kickstand_rod_input, 
                                                                                                    kickstand_rod_material = right_kickstand_rod_material_input, 
                                                                                                    kickstand_rod_size = right_kickstand_rod_size_input)
  }
  
  # left_accessory_rod_material_input = "na",
  # left_accessory_rod_size_input = "na",
  # left_satellite_rod_material_input = "na",
  # left_satellite_rod_size_input = "na",
  # left_intercalary_rod_material_input = "na",
  # left_intercalary_rod_size_input = "na",
  # left_linked_rod_material_input = "na",
  # left_linked_rod_size_input = "na",
  
      # 
      # if(str_detect(surgery_details_list$main_approach, "posterior")){
      #   surgery_details_list$left_rod <- if_else(left_main_rod_size_input == "None", "None", paste(left_main_rod_size_input, left_main_rod_material_input))
      #   surgery_details_list$right_rod <- if_else(right_main_rod_size_input == "None", "None", paste(right_main_rod_size_input, right_main_rod_material_input))
      # }
      # if(any(add_left_accessory_rod_input,
      #        add_left_satellite_rod_input,
      #        add_left_intercalary_rod_input,
      #        add_left_linked_rod_input,
      #        add_right_accessory_rod_input,
      #        add_right_satellite_rod_input,
      #        add_right_intercalary_rod_input,
      #        add_right_linked_rod_input)){
      #   
      #   supplemental_rods_df <- tibble(supplemental_rod = c("accessory_rod",
      #                                                       "satellite_rod",
      #                                                       "intercalary_rod",
      #                                                       "linked_rod",
      #                                                       "accessory_rod",
      #                                                       "satellite_rod",
      #                                                       "intercalary_rod",
      #                                                       "linked_rod"),
      #                                  side = c("left", "left", "left", "left", "right", "right", "right", "right"),
      #                                  yes_no = c(add_left_accessory_rod_input,
      #                                             add_left_satellite_rod_input,
      #                                             add_left_intercalary_rod_input,
      #                                             add_left_linked_rod_input,
      #                                             add_right_accessory_rod_input,
      #                                             add_right_satellite_rod_input,
      #                                             add_right_intercalary_rod_input,
      #                                             add_right_linked_rod_input)) %>%
      #     filter(yes_no == TRUE)
      #   if(nrow(supplemental_rods_df %>% filter(side == "left")) >0){
      #     surgery_details_list$left_supplemental_rods <- glue_collapse((supplemental_rods_df %>% filter(side == "left"))$supplemental_rod, sep = "; ")
      #   }else{
      #     surgery_details_list$left_supplemental_rods <- "none"
      #   }
      #   
      #   if(nrow(supplemental_rods_df %>% filter(side == "right")) >0){
      #     surgery_details_list$right_supplemental_rods <- glue_collapse((supplemental_rods_df %>% filter(side == "right"))$supplemental_rod, sep = "; ")
      #   }else{
      #     surgery_details_list$right_supplemental_rods <- "none"
      #   }
      # }else{
      #   surgery_details_list$left_supplemental_rods <- "none"
      #   surgery_details_list$right_supplemental_rods <- "none"
      # }
      
      ############# CROSSLINKS #############
      
      if(length(crosslink_connectors_input) > 0){
        surgery_details_list$crosslink_connector_levels <- glue_collapse(crosslink_connectors_input, sep = "; ")
      }else{
        surgery_details_list$crosslink_connector_levels <- "none"
      }
      
      #################### SPINE UIV PPX  #########################
      if(surgery_details_list$fusion == "yes"){
        uiv_ppx_df <- all_objects_df_input %>%
          filter(between(vertebral_number, jh_get_vertebral_number_function(surgery_details_list$uiv) -3, jh_get_vertebral_number_function(surgery_details_list$uiv) + 1.5)) %>%
          filter(str_detect(string = object, pattern = "hook") |
                   str_detect(string = object, pattern = "tether") |
                   str_detect(string = object, pattern = "wire") |
                   str_detect(string = object, pattern = "vertebroplasty") |
                   str_detect(string = object, pattern = "vertebral_cement_augmentation")) %>%
          select(level, object) %>%
          distinct()
        if(nrow(uiv_ppx_df) > 0){
          surgery_details_list$uiv_ppx_used <- "yes"
          surgery_details_list$uiv_ppx <- str_replace(string = glue_collapse(x = unique(uiv_ppx_df$object), sep = "; "), pattern = "_", replacement = " ")
        }else{
          surgery_details_list$uiv_ppx_used <- "no"
        }
      }
      
      
      ##########   interspaces decompressed  #############
      decompressions_df <- all_objects_df_input %>%
        filter(category == "decompression" |
                 object == "decompression_diskectomy_fusion" |
                 str_detect(string = object, pattern = "decompression") |
                 object == "diskectomy_only" |
                 object == "anterior_disc_arthroplasty") %>%
        select(level, body_interspace) %>%
        distinct()
      
      if(nrow(decompressions_df)>0){
        interspaces_decompressed <- unique((decompressions_df %>% filter(body_interspace == "interspace"))$level)
        
        if(any(decompressions_df$body_interspace == "body")){
          interspaces_decompressed <- unique(jh_reorder_levels_function(level_vector = append(interspaces_decompressed,
                                                                                              jh_convert_body_levels_to_interspace_vector_function(vertebral_bodies_vector = (decompressions_df %>% filter(body_interspace == "body"))$level))))
        }
        ##########   number_of_levels_decompressed #############
        surgery_details_list$number_of_levels_decompressed <- length(interspaces_decompressed)
        
        surgery_details_list$interspaces_decompressed <- glue_collapse(interspaces_decompressed, sep = "; ")
      }else{
        surgery_details_list$number_of_levels_decompressed <- "0"
      }
      
      ##########   THREE COLUMN OSTEOTOMY #############
      surgery_details_list$three_column_osteotomy <- if_else(any(all_objects_df_input$object == "grade_3") |
                                                               any(all_objects_df_input$object == "grade_4") |
                                                               any(all_objects_df_input$object == "grade_5") |
                                                               any(all_objects_df_input$object == "grade_6"), "yes", "no")
      if(surgery_details_list$three_column_osteotomy == "yes"){
        surgery_details_list$three_column_osteotomy_level <- glue_collapse(x = (all_objects_df_input %>%
                                                                                  filter(object == "grade_3" | object == "grade_4" | object == "grade_5") %>%
                                                                                  select(level) %>%
                                                                                  distinct() %>%
                                                                                  as_vector()), sep = "; ")
        
      }
      
      ###### SPINE CERVICAL VS LUMBAR FOR PRO CAPTURE #####
      if(surgery_details_list$lower_treated_vertebrae %in% c("Occiput", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "T1", "T2", "T3", "T4", "T5", "T6")){
        surgery_details_list$spine_region <- "cervical"
      }else{
        surgery_details_list$spine_region <- "lumbar"
      }
      
      #################### BMP & ALLOGRAFT  #######################

      if(str_detect(surgery_details_list$main_approach, "anterior") & surgery_details_list$fusion == "yes"){
        surgery_details_list$anterior_bmp_mg_dose <-  anterior_bmp_dose_reactive_input
        if(length(anterior_bone_graft_input) > 0){
          surgery_details_list$anterior_bone_graft <- glue_collapse(anterior_bone_graft_input, sep = "; ")
          
          if(str_detect(string = surgery_details_list$anterior_bone_graft, pattern = "Morselized Allograft")){
            surgery_details_list$anterior_allograft_amount <- paste(anterior_allograft_amount_input)
          }
        }
        
        if(length(anterior_biologics_input)>0){
          anterior_biologics_list <- list()
          anterior_biologics_list$anterior_bma <- if_else("Bone Marrow Aspirate" %in% anterior_biologics_input, glue("Bone Marrow Aspirate ({anterior_bone_marrow_aspirate_volume_input})cc"), glue("xx"))
          anterior_biologics_list$anterior_cell_based <- if_else("Cell Based Allograft" %in% anterior_biologics_input, glue("Cell Based Allograft ({anterior_cell_based_allograft_volume_input})cc"), glue("xx"))
          anterior_biologics_list$anterior_dbm <- if_else("DBM" %in% anterior_biologics_input, glue("DBM ({anterior_dbm_volume_input})cc"), glue("xx"))
          anterior_biologics_list$anterior_ifactor <- if_else("iFactor" %in% anterior_biologics_input, glue("iFactor ({anterior_ifactor_volume_input})cc"), glue("xx"))
          anterior_biologics_list$anterior_other_biologic <- if_else("Other" %in% anterior_biologics_input, glue("{anterior_biologics_other_input}"), glue("xx"))
          
          anterior_biologics_list <- discard(anterior_biologics_list, .p = ~ .x == "xx")
          
          if(length(anterior_biologics_list) > 0){
            surgery_details_list$anterior_biologics <- glue_collapse(anterior_biologics_list, sep = "; ") 
          }else{
            surgery_details_list$anterior_biologics <- "xx"
          }
        }
      }
      
      if(str_detect(surgery_details_list$main_approach, "posterior") & surgery_details_list$fusion == "yes"){
        surgery_details_list$posterior_bmp_mg_dose <-  posterior_bmp_dose_reactive_input
        
        if(length(posterior_bone_graft_input) > 0){
          surgery_details_list$posterior_bone_graft <- glue_collapse(posterior_bone_graft_input, sep = "; ")
          
          if(str_detect(string = surgery_details_list$posterior_bone_graft, pattern = "Morselized Allograft")){
            surgery_details_list$posterior_allograft_amount <- paste(posterior_allograft_amount_input)
          }
        }
        
        if(length(posterior_biologics_input)>0){
          posterior_biologics_list <- list()
          posterior_biologics_list$posterior_bma <- if_else("Bone Marrow Aspirate" %in% posterior_biologics_input, glue("Bone Marrow Aspirate ({posterior_bone_marrow_aspirate_volume_input})cc"), glue("xx"))
          posterior_biologics_list$posterior_cell_based <- if_else("Cell Based Allograft" %in% posterior_biologics_input, glue("Cell Based Allograft ({posterior_cell_based_allograft_volume_input})cc"), glue("xx"))
          posterior_biologics_list$posterior_dbm <- if_else("DBM" %in% posterior_biologics_input, glue("DBM ({posterior_dbm_volume_input})cc"), glue("xx"))
          posterior_biologics_list$posterior_ifactor <- if_else("iFactor" %in% posterior_biologics_input, glue("iFactor ({posterior_ifactor_volume_input})cc"), glue("xx"))
          posterior_biologics_list$posterior_pearlmatrix <- if_else("pearlmatrix" %in% str_to_lower(posterior_biologics_input), glue("PearlMatrix ({posterior_pearlmatrix_volume_input})cc"), glue("xx"))
          
          # posterior_pearlmatrix_volume_input
          
          posterior_biologics_list$posterior_other_biologic <- if_else("Other" %in% posterior_biologics_input, glue("{posterior_biologics_other_input}"), glue("xx"))
          
          posterior_biologics_list <- discard(posterior_biologics_list, .p = ~ .x == "xx")
          
          if(length(posterior_biologics_list) > 0){
            surgery_details_list$posterior_biologics <- glue_collapse(posterior_biologics_list, sep = "; ") 
          }else{
            surgery_details_list$posterior_biologics <- "xx"
          }
        }
      }
      
      ####### complications  #####
      complication_df <- tibble(complication = append(intraoperative_complications_vector_input, other_intraoperative_complications_input)) %>%
        filter(complication != "") %>%
        filter(complication != " ") %>%
        remove_empty()
      
      if(nrow(complication_df) > 0){
        surgery_details_list$complications <- glue_collapse(complication_df$complication, sep = '; ')
      }else{
        surgery_details_list$complications <- "none"
      }
      
      if(length(implant_manufacturer_input)>0){
        surgery_details_list$implant_manufacturer <- glue_collapse(implant_manufacturer_input, sep = ", ")
      }
      
      surgery_details_list$operative_note <- paste(operative_note_text_input)
      
      ####### FULL TABLE  #####
      
  
    surgery_details_df <- enframe(surgery_details_list) %>%
      mutate(across(everything(), ~ as.character(.x)))  %>%
        filter(value != "na", value != "na na", value != "", value != "--")
    
    return(list(surgery_details_df = surgery_details_df, 
                surgery_details_list = surgery_details_list))

}

################################ ################################ INTRAOP DETAILS DF ############################## ################################ 
################################ ################################ INTRAOP DETAILS DF ############################## ################################ 
################################ ################################ INTRAOP DETAILS DF ############################## ################################ 
################################ ################################ INTRAOP DETAILS DF ############################## ################################ 

redcap_table_intraop_details_df_function <- function(all_objects_df_input = tibble(level = character(), vertebral_number = double(), side = character(), object = character()),
                                                     date_of_surgery_input = "1900-01-01", 
                                                     preop_antibiotics_input = c(), 
                                                     neuromonitoring_input = c(),
                                                     anti_fibrinolytic_input = c(),
                                                     txa_loading_input = "",
                                                     txa_maintenance_input = "",
                                                     surgical_findings_input = "",
                                                     specimens_removed_input = "",
                                                     ebl_input = "xx",
                                                     urine_output_input = "xx",
                                                     crystalloids_administered_input = NA,
                                                     colloids_administered_input = NA,
                                                     transfusion_input = NA,
                                                     cell_saver_transfused_input = NA,
                                                     prbc_transfused_input = NA,
                                                     ffp_transfused_input = NA,
                                                     cryoprecipitate_transfused_input = NA,
                                                     platelets_transfused_input = NA,
                                                     intraoperative_complications_vector_input = c(),
                                                     other_intraoperative_complications_input = "",
                                                     deep_drains_anterior_input = NA,
                                                     superficial_drains_anterior_input = NA,
                                                     deep_drains_posterior_input = NA,
                                                     superficial_drains_posterior_input = NA,
                                                     additional_end_procedure_details_anterior_input = NA,
                                                     additional_end_procedure_details_posterior_input = NA,
                                                     closure_details_anterior_input = NA,
                                                     dressing_details_anterior_input = NA,
                                                     closure_details_posterior_input = NA,
                                                     dressing_details_posterior_input = NA
                                                     ){
  intraop_details_list <- list()
  
  intraop_details_list$dos_intraop_repeating <- as.character(date_of_surgery_input)
  
  ################### Abx  #########################
  if(length(preop_antibiotics_input)>0){
    intraop_details_list$antibiotics <- glue_collapse(preop_antibiotics_input, sep = '; ')
  }else{
    intraop_details_list$antibiotics <- "unknown"
  }
  
  
  ################### NEUROMONINTORING  #########################
  if(length(neuromonitoring_input)>0){
    intraop_details_list$neuromonitoring <-  glue_collapse(neuromonitoring_input, sep = '; ')
  }else{
    intraop_details_list$neuromonitoring <- "none"
  }
  
  ################### antifibrinolytic  #########################
  if(length(anti_fibrinolytic_input) > 0){
    antifibrinolytics_vector <- str_to_lower(as.character(glue_collapse(anti_fibrinolytic_input, sep = "; ")))
    intraop_details_list$anti_fibrinolytic <- str_replace_all(string = antifibrinolytics_vector,
                                                              pattern = "tranexamic acid \\(txa\\)",
                                                              replacement = glue("tranexamic acid (txa) Loading: {txa_loading_input}mg/kg, Maint: {txa_maintenance_input}mg/kg/hr"))
  }else{
    intraop_details_list$anti_fibrinolytic <- "none"
  }
  
  ####### surgical findings #####
  intraop_details_list$surgical_findings <- if_else(surgical_findings_input == "", "none", surgical_findings_input)
  
  ####### Specimens  #####
  intraop_details_list$specimens <- if_else(specimens_removed_input == "", "none", specimens_removed_input)
  
  ####### EBL  #####
  intraop_details_list$ebl_ml <- if_else(is.na(ebl_input), "xx", paste(ebl_input))
  
  ####### Urine Output  #####
  intraop_details_list$urine_output <- if_else(is.na(urine_output_input), "xx", paste(urine_output_input)) 
  
  ####### Crystalloids  #####
  intraop_details_list$crystalloids_ml <- if_else(is.na(crystalloids_administered_input), "xx", paste(crystalloids_administered_input))
  
  ####### Colloids  #####
  intraop_details_list$colloids_ml <- if_else(is.na(colloids_administered_input), "xx", paste(colloids_administered_input)) 
  
  ####### Transfusion  #####
  intraop_details_list$transfusion <- if_else(transfusion_input == TRUE, "yes", "no")
  
  ####### cell_saver  #####
  intraop_details_list$cell_saver_cc <- if_else(is.na(cell_saver_transfused_input), "xx", paste(cell_saver_transfused_input)) 
  
  ####### prbc  #####
  intraop_details_list$prbc_units <- if_else(is.na(prbc_transfused_input), "xx", paste(prbc_transfused_input)) 
  
  ####### ffp  #####
  intraop_details_list$ffp_units <- if_else(is.na(ffp_transfused_input), "xx", paste(ffp_transfused_input)) 
  
  ####### cryoprecipitate  #####
  intraop_details_list$cryoprecipitate_units <- if_else(is.na(cryoprecipitate_transfused_input), "xx", paste(cryoprecipitate_transfused_input)) 
  
  ####### platelets  #####
  intraop_details_list$platelets_units <- if_else(is.na(platelets_transfused_input), "xx", paste(platelets_transfused_input))  
  
  ####### complications  #####
  complication_df <- tibble(complication = append(intraoperative_complications_vector_input, other_intraoperative_complications_input)) %>%
    filter(complication != "") %>%
    filter(complication != " ") %>%
    remove_empty()
  
  if(nrow(complication_df) > 0){
    intraop_details_list$intraoperative_complications <- glue_collapse(complication_df$complication, sep = '; ')
  }else{
    intraop_details_list$intraoperative_complications <- "none"
  }
  
  ####### other procedures  #####
  if(any(all_objects_df_input$approach == "anterior")){
    intraop_details_list$deep_drains_anterior <- paste(deep_drains_anterior_input)
    intraop_details_list$superficial_drains_anterior <- paste(superficial_drains_anterior_input) 
  }
  if(any(all_objects_df_input$approach == "posterior")){
    intraop_details_list$deep_drains_posterior <- paste(deep_drains_posterior_input)
    intraop_details_list$superficial_drains_posterior <- paste(superficial_drains_posterior_input) 
  }
  
  if(length(additional_end_procedure_details_anterior_input)>0){
    intraop_details_list$end_procedure_details_anterior <- glue_collapse(additional_end_procedure_details_anterior_input, sep = "; ")
  }else{
    intraop_details_list$end_procedure_details_anterior <- " "
  }
  if(length(additional_end_procedure_details_posterior_input)>0){
    intraop_details_list$end_procedure_details_posterior <- glue_collapse(additional_end_procedure_details_posterior_input, sep = "; ")
  }else{
    intraop_details_list$end_procedure_details_posterior <- " "
  }
  
  if(length(closure_details_anterior_input)>0){
    intraop_details_list$closure_details_anterior <- glue_collapse(closure_details_anterior_input, sep = "; ")
  }
  if(length(dressing_details_anterior_input)>0){
    intraop_details_list$closure_details_anterior <- glue_collapse(dressing_details_anterior_input, sep = "; ")
  }
  
  if(length(closure_details_posterior_input)>0){
    intraop_details_list$closure_details_posterior <- glue_collapse(closure_details_posterior_input, sep = "; ")
  }
  if(length(dressing_details_posterior_input)>0){
    intraop_details_list$dressing_details_posterior <- glue_collapse(dressing_details_posterior_input, sep = "; ")
  }
  
  ####### GENERATE DATAFRAME #####
  
  intraop_details_df <- enframe(intraop_details_list) %>%
    mutate(across(everything(), ~ as.character(.x))) %>%
    filter(value != "xx")%>%
    filter(value != "")%>%
    filter(value != " ")
  

  return(list(intraop_details_df = intraop_details_df, 
              intraop_details_list = intraop_details_list))

}
