################# OP NOTE FUNCTIONS ##################

decompression_statement_function <- function(level, object, side, revision_modifier = "xx"){
  revision_statement <- if_else(revision_modifier == "xx", "", "reexploration and revision decompression with a")
  if(object == "laminectomy"){
    decompression_statement <-  glue("a {revision_statement} central laminectomy at the level of {level} to fully decompress the central canal")
  }
  if(object == "sublaminar decompression"){
    decompression_statement <- glue("a {revision_statement} sublaminar decompression bilaterally at the {level} interspace, which included partial laminectomies with partial medial facetectomies and foraminotomies on the left and right at the {level} interspace to fully decompress the exiting and traversing nerve roots within the neural foramen and lateral recess")
  }
  if(object == "diskectomy"){
    decompression_statement <- glue("a {revision_statement} laminotomy and diskectomy on the {side} at the {level} interspace to fully decompress the nerve root")
  }
  
  if(object == "laminotomy"){
    decompression_statement <- glue("a {revision_statement} laminotomy with partial medial facetectomy on the {side} side at the {level} interspace to fully decompress the nerve root, ")
  }
  
  if(object == "transpedicular approach"){
    decompression_statement <-  glue("a {revision_statement} transpedicular approach on the {side} at {level}, which included skeletonizing and resecting the {side} {level} pedicle, to allow full decompression of the {if_else(str_starts(level, 'L'), 'cauda equina and nerve roots', 'spinal cord')} at the {level} level")
  }
  
  if(object == "costovertebral approach"){
    decompression_statement <-  glue("a {revision_statement} costovertebral approach on the {side} at {level}, which included resection of the {side} {level} rib head, skeletonizing and resecting the {side} {level} pedicle, to allow full decompression of the spinal cord at the {level} level")
  }
  if(object == "laminoplasty"){

    decompression_statement <-  glue("{revision_statement} laminoplasty with decompression of the spinal cord at the {level} level")
  }
  
  decompression_statement
}
# 


collapsing_decompression_levels_function <- function(input_df, object_input, side_input, revision_input){
  vector <- input_df %>%
    filter(object == object_input, 
           side == side_input, 
           revision == revision_input) %>%
    select(level) %>%
    as_vector()
  
  glue_collapse(x = vector, sep = ", ", last = ", and ")
}


interbody_statement_function <- function(level, approach, object, side){
  if(object == "no implant interbody fusion"){
    glue("performed an interbody fusion procedure from the {side} side at the level of {level}. The inferior and superior facets of {str_replace_all(level, '-', ' and ')}, respectively, were resected and the exiting and traversing nerve roots were appropriately protected. The annulus was incised, allowing access to the disk space. The disk was excised and endplates were prepped using a combination of currettes, shavers, and pituitary rongeurs. Once the endplates were adequately prepped, allograft was placed into the disk space.") 
  }else{
    interbody_type <- case_when(
      object == "tlif" ~ "transforaminal lumbar interbody fusion",
      object == "plif" ~ "posterior Lumbar Interbody fusion",
      object == "llif" ~ "lateral lumbar interbody fusion"
    )
    glue("performed a {interbody_type} procedure from the {side} side at the level of {level}. The inferior and superior facets of {str_replace_all(level, '-', ' and ')}, respectively, were resected and the exiting and traversing nerve roots were appropriately protected. The annulus was incised, allowing access to the disk space. The disk was excised and endplates were prepped using a combination of currettes, shavers, and pituitary rongeurs. Once the endplates were adequately prepped, the size was confirmed and the interbody implant was placed into position along with allograft. The final position was confirmed using intraoperative fluoroscopy.") 
    
  }
}

instrumentation_function <- function(level, object, side){
  
  side <- if_else(side == "bilateral", "left and on the right", side)
  
  if(str_detect(object, "screw") | str_detect(object, "hook") | str_detect(object, "wire")){
    
    if(str_detect(object, "occip")){
      glue("an {object} into the occiput after identifying the appropriate starting point, drilling, and measuring the depth")
    }else{
      glue("a {object} on the {side} at the {level} level")
    }
  }else{
    if(str_detect(object, "kypho")){
      glue("I performed a {object} at the {level} level")
    }else{
      glue("I placed a {object} at the {level} level") 
    }
  }
  
}



############################################## FULL OP NOTE LIST GENERATOR ####################### ####################### #######################
############################################## FULL OP NOTE LIST GENERATOR ####################### ####################### #######################
############################################## FULL OP NOTE LIST GENERATOR ####################### ####################### #######################
############################################## FULL OP NOTE LIST GENERATOR ####################### ####################### #######################
############################################## FULL OP NOTE LIST GENERATOR ####################### ####################### #######################
############################################## FULL OP NOTE LIST GENERATOR ####################### ####################### #######################
############################################## FULL OP NOTE LIST GENERATOR ####################### ####################### #######################
############################################## FULL OP NOTE LIST GENERATOR ####################### ####################### #######################
############################################## FULL OP NOTE LIST GENERATOR ####################### ####################### #######################
############################################## FULL OP NOTE LIST GENERATOR ####################### ####################### #######################
############################################## FULL OP NOTE LIST GENERATOR ####################### ####################### #######################
############################################## FULL OP NOTE LIST GENERATOR ####################### ####################### #######################

full_posterior_op_note_generator_function <- function(all_objects_to_add_df,
                                                      fusion_levels_df,
                                                      open_canal_vector, 
                                                      left_main_rod_size,
                                                      left_main_rod_material, 
                                                      right_main_rod_size, 
                                                      right_main_rod_material, 
                                                      additional_rods_statement, 
                                                      antibiotics,
                                                      bmp = NULL,
                                                      allograft = NULL,
                                                      additional_procedural_details, 
                                                      deep_drains, 
                                                      superficial_drains,
                                                      end_procedure_details,
                                                      closure){
  op_note_df <- all_objects_to_add_df %>%
    select(-object_constructed) %>%
    union_all(fusion_levels_df) %>%
    mutate(object = str_to_lower(object)) %>%
    mutate(object = str_replace_all(object, "_", " ")) %>%
    filter(approach == "posterior") 
  
  revision_decompression_levels_df <- open_canal_df %>%
    filter(level %in% open_canal_vector) %>%
    select(vertebral_number) %>%
    mutate(interspace_proximal = vertebral_number - 0.5) %>%
    mutate(interspace_distal = vertebral_number + 0.5) %>%
    pivot_longer(cols = everything()) %>%
    arrange(value) %>%
    select(vertebral_number = value) %>%
    mutate(revision = "revision")
  
  posterior_decompression_df <- op_note_df %>%
    filter(category == "decompression") %>%
    select(level, vertebral_number, approach, category, object, side) %>%
    arrange(vertebral_number) %>%
    left_join(revision_decompression_levels_df)%>%
    replace_na(list(revision = "xx")) %>%
    distinct()
  
  posterior_interbody_df <- op_note_df %>%
    filter(category == "interbody")%>%
    select(level, vertebral_number, approach, category, object, side) %>%
    arrange(vertebral_number)
  
  
  posterior_osteotomy_df <- op_note_df %>%
    filter(category == "osteotomy") %>%
    select(level, vertebral_number, category, object, side) %>%
    mutate(interspace_number = vertebral_number - 0.5) %>%
    left_join(interbody_levels_df %>% rename(interspace = level, interspace_number = vertebral_number)) %>%
    group_by(object) %>%
    add_tally(name = "object_tally") %>%
    ungroup() %>%
    mutate(object = str_replace_all(object, "_", " "))
  
  posterior_implant_df <- op_note_df %>%
    filter(category == "implant") %>%
    select(level, vertebral_number, approach, category, object, side) %>%
    arrange(vertebral_number) %>%
    remove_missing() %>%
    group_by(level, object) %>%
    add_tally(name = "total_per_level") %>%
    mutate(side = if_else(total_per_level == 2, "bilateral", side)) %>%
    ungroup() %>%
    distinct()
  
  posterior_fusion_df <- op_note_df %>%
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
    mutate(vertebral_number = if_else(str_detect(level, "-"), vertebral_number + 0.5, vertebral_number)) %>%
    mutate(vertebral_number = round(vertebral_number, 0)) %>%
    select(vertebral_number) %>%
    left_join(levels_numbered_df) %>%
    select(level, vertebral_number) %>%
    distinct()
  
  procedure_details_list <- list()
  
  procedures_summary_list <- list()
  
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
      head_statement <- "Cranial tongs were applied to the patient's skull for positioning and an appropriate weight was selected for cranial traction."
    }else{
      head_statement <- "The proneview faceplate was used to pad and secure the patient's face during surgery. "
    }
    
  } 
  
  if(any(str_detect(additional_procedural_details, "Spinal Cord Monitoring"))){
    spinal_cord_monitoring <- "Spinal Cord Monitoring needles were inserted by the neurophysiology technologist."
  }else{
    spinal_cord_monitoring <- NULL
  }
  
  # proximal_exposure_level <- levels_df %>%
  proximal_exposure_level <- op_note_df %>%
    filter(vertebral_number == min(vertebral_number)) %>%
    mutate(vertebral_number = round(vertebral_number - 0.25, 0)) %>%
    mutate(level = if_else(vertebral_number >=25, "S1", level)) %>%
    select(vertebral_number) %>%
    distinct() %>%
    left_join(levels_numbered_df) %>%
    select(level) %>%
    distinct()
  
  distal_exposure_level <- op_note_df %>%
    filter(vertebral_number == max(vertebral_number)) %>%
    mutate(vertebral_number = round(vertebral_number + 0.25, 0)) %>%
    mutate(level = if_else(vertebral_number >=25, "S1", level)) %>%
    select(vertebral_number) %>%
    distinct() %>%
    left_join(levels_numbered_df) %>%
    select(level) %>%
    distinct()
    
  procedure_details_list$approach_statement <- paste("The patient was brought to the operating room and after obtaining appropriate IV access, the patient underwent general anesthesia.",
                                                     antibiotic_statement,
                                                     head_statement, 
                                                     spinal_cord_monitoring,
                                                     "The patient was then positioned prone on the OR table and all bony prominences were appropriately padded.",
                                                     "After prepping and draping in the standard fashion, a surgical timeout was performed.", 
                                                     glue("A standard posterior approach to the spine was performed, exposing proximally to the {proximal_exposure_level$level[1]} level and distally to the {distal_exposure_level$level[1]} level."), 
                                                     sep = " "
  )
  
  # procedure_details_list$approach_statement <- paste("The patient was brought to the operating room and after obtaining appropriate IV access, the patient underwent general anesthesia.",
  #                                                    antibiotic_statement,
  #                                                    head_statement, 
  #                                                    spinal_cord_monitoring,
  #                                                    "The patient was then positioned prone on the OR table and all bony prominences were appropriately padded.",
  #                                                    "After prepping and draping in the standard fashion, a surgical timeout was performed.", 
  #                                                    glue("A standard posterior approach to the spine was performed, exposing proximally to the {levels_df$level[1]} level and distally to the {levels_df$level[2]} level."), 
  #                                                   sep = " "
  # )
                                                     

  

  
  ############## OSTEOTOMY AND FACETECTOMY #######################.00.0
  
  if(nrow(posterior_osteotomy_df) > 0){
    complete_facetectomy_df <- posterior_osteotomy_df %>%
      filter(object == "complete facetectomy")
    
    grade_1_df <- posterior_osteotomy_df %>%
      filter(object == "grade 1")
    
    grade_2_df <- posterior_osteotomy_df %>%
      filter(object == "grade 2")
    
    grade_3_df <- posterior_osteotomy_df %>%
      filter(object == "grade 3")
    
    grade_4_df <- posterior_osteotomy_df %>%
      filter(object == "grade 4")
    
    grade_5_df <- posterior_osteotomy_df %>%
      filter(object == "grade 5")
    
    
    if(nrow(complete_facetectomy_df) > 0){
      complete_facetectomy_details <- if_else(max(complete_facetectomy_df$object_tally) == 1,
                                              glue("A complete facetectomy was performed by resecting the entire superior and inferior facets at the {glue_collapse(x = complete_facetectomy_df$interspace, sep = ', ', last = ', and ')} level. "),
                                              glue("Complete facetectomies were performed by resecting the entire superior and inferior facets at the {glue_collapse(x = complete_facetectomy_df$interspace, sep = ', ', last = ', and ')} levels. ")
      )
      
      procedures_summary_list$complete_facetectomies <- glue("Complete facetectomies at {glue_collapse(x = complete_facetectomy_df$interspace, sep = ', ', last = ', and ')}")
      
    }else{
      complete_facetectomy_details <- NULL
    }
    
    if(nrow(grade_1_df) > 0){
      grade_1_osteotomy_details <- if_else(max(grade_1_df$object_tally) == 1,
                                           glue("An inferior facetectomy was performed by resecting the inferior facets at the {glue_collapse(x = (grade_1_df %>% select(level) %>% distinct())$level, sep = ', ', last = ', and ')} level. "),
                                           glue("Inferior facetectomies were performed by resecting the inferior facets at the {glue_collapse(x = (grade_1_df %>% select(level) %>% distinct())$level, sep = ', ', last = ', and ')} levels. ")
      )
      
      procedures_summary_list$grade_1_osteotomy <- glue("Inferior facetectomy at {glue_collapse(x = (grade_1_df %>% select(level) %>% distinct())$level, sep = ', ', last = ', and ')}")
    }else{
      grade_1_osteotomy_details <- NULL
    }
    
    if(nrow(grade_2_df) > 0){
      grade_2_osteotomy_details <- if_else(max(grade_2_df$object_tally) == 1,
                                           glue("A posterior column osteotomy (Smith-Peterson Osteotomy) was performed by resecting the inferior and superior facets bilaterally and ligamentous attachments, including the ligamentum flavum, at the {glue_collapse(x = grade_2_df$interspace, sep = ', ', last = ', and ')} level. "),
                                           glue("Posterior column osteotomies (Smith-Peterson Osteotomy) were performed by resecting the inferior and superior facets bilaterally and ligamentous attachments, including the ligamentum flavum, at the {glue_collapse(x = grade_2_df$interspace, sep = ', ', last = ', and ')} levels. ")
      )
      procedures_summary_list$grade_2_osteotomy <- glue("Posterior column osteotomies at {glue_collapse(x = grade_2_df$interspace, sep = ', ', last = ', and ')}")
    }else{
      grade_2_osteotomy_details <- NULL
    }
    
    procedure_details_list$posterior_osteotomy_details <- paste("I then proceeded with facetectomies/osteotomies.", grade_1_osteotomy_details, complete_facetectomy_details, grade_2_osteotomy_details, sep = "")
    
  }
  
  ########### INSTRUMENTATION ########
  
  if(nrow(posterior_implant_df) > 0){
    
    segments_instrumented <- posterior_implant_df %>%
      select(level) %>%
      distinct()
    
    
    posterior_instrumentation_levels_details <- glue_collapse(x = pmap(.l = list(..1 = posterior_implant_df$level, 
                                                                                 ..2 = posterior_implant_df$object, 
                                                                                 ..3 = posterior_implant_df$side),
                                                                       .f = ~ instrumentation_function(level = ..1, 
                                                                                                       object =  ..2, 
                                                                                                       side = ..3)),
                                                              sep = ", ",
                                                              last = ", and ")
    
    if(any(str_detect(string = posterior_implant_df$object, pattern = "pedicle"))){
      pedicle_screw_technique_statement <- paste(" For pedicle screws, the transverse process, pars, and superior facet were used as landmarks to identify the appropriate starting point. After identifying the start point, the superficial cortex was opened at each entry point using a high speed burr. This was followed by cannulating the pedicle using a pedicle probe, palpating for a medial, lateral, superior and inferior pedicle walls, measuring, and tapping if appropriate.")
    }else{
      pedicle_screw_technique_statement <- ""
    }
    
    if(any(str_detect(string = posterior_implant_df$object, pattern = "mass"))){
      lateral_mass_screw_technique_statement <- paste(" For lateral mass screw placement, the entry point was identified using the lateral and medial borders of the lateral mass and superior and inferior borders of the facet. The superficial cortex was opened at each entry point using a high speed burr and the screw path drilled sequentially to the far cortex, with the goal of acheiving bicortical screw purchase.")
    }else{
      lateral_mass_screw_technique_statement <- ""
    }
    
    procedure_details_list$posterior_instrumentation_implants <- glue("I then proceeded with instrumenting the spine.{pedicle_screw_technique_statement}{lateral_mass_screw_technique_statement} I placed {posterior_instrumentation_levels_details}. " )
    
    procedures_summary_list$posterior_instrumentation <- glue("Posterior Spinal Instrumentation of {glue_collapse(x = posterior_implant_df$level, sep = ', ', last = ', and ')}")
    
  }
  
  ########### DECOMPRESSIONS ########
  if(nrow(posterior_decompression_df) > 0){
    
    decompression_levels <- glue_collapse(x = pmap(.l = list(..1 = posterior_decompression_df$level, 
                                                             ..2 = posterior_decompression_df$object, 
                                                             ..3 = posterior_decompression_df$side,
                                                             ..4 = posterior_decompression_df$revision), 
                                                   .f = ~ decompression_statement_function(level = ..1,
                                                                                           object = ..2, 
                                                                                           side = ..3, 
                                                                                           revision_modifier = ..4)),
                                          sep = ". Then I performed ", last = ". Then I performed ")
    
    if(any(posterior_decompression_df$object == "laminoplasty")){
      procedure_details_list$decompression_details <- paste("I then proceeded with the decompressions using a combination of a high-speed burr and Kerrison rongeurs. For laminoplasty, an opening and hinge trough were created with a high-speed burr angled perpendicular to the lamina at the lateral mass-lamina junction. I performed ",
                                                            glue_collapse(x = pmap(.l = list(..1 = posterior_decompression_df$level, 
                                                                                             ..2 = posterior_decompression_df$object, 
                                                                                             ..3 = posterior_decompression_df$side,
                                                                                             ..4 = posterior_decompression_df$revision), 
                                                                                   .f = ~ decompression_statement_function(level = ..1,
                                                                                                                           object = ..2, 
                                                                                                                           side = ..3, 
                                                                                                                           revision_modifier = ..4)),
                                                                          sep = ", ", last = ", and "), 
                                                            ". After all troughs were completed, greenstick fractures were created to open the lamina at the laminoplasty levels, the ligamentum flavum at the proximal and distal levels was excised, and laminoplasty plates were secured in place. The spinous processes were trimmed and the canal was palpated to confirm an adequate decompression had been completed. This completed the decompression portion of the procedure.", 
                                                            sep = "")
    }else{
      procedure_details_list$decompression_details <- paste("I then proceeded with the decompressions. Using a combination of a high-speed burr and Kerrison rongeurs, I performed ",
                                                            decompression_levels, 
                                                            ". Following the decompression, the canal was palpated to confirm an adequate decompression had been completed. This completed the decompression portion of the procedure.", 
                                                            sep = "")  
    }
    
    
    
    decompression_prepared_df <- posterior_decompression_df %>%
      select(level, vertebral_number, approach, category, object, side, revision) %>%
      arrange(vertebral_number) %>%
      group_by(object, revision) %>%
      select(-vertebral_number) %>%
      mutate(revision_text = if_else(revision == "xx", "", "Reexploration and revision decompression with ")) %>%
      mutate(decompression_text = case_when(object == "sublaminar decompression" ~ "bilateral partial laminectomies, medial facetectomies and foraminotomies of ", 
                                            object == "laminectomy" ~ "central laminectomy of ", 
                                            object == "diskectomy" ~ paste(side, "sided laminotomy and diskectomy at the "), 
                                            object == "laminotomy" ~ paste(side, "sided laminotomy with partial medial facetectomy at the "),
                                            object == "transpedicular approach" ~ paste(side, "sided transpedicular decompression at "),
                                            object == "costovertebral approach" ~ paste(side, "sided costovertebral approach and decompression at "),
                                            object == "laminoplasty" ~ "laminoplasty with full decompression of the spinal cord and application of laminoplasty plates at "
                                            )) %>%
      mutate(decompression_text = str_to_sentence(paste(revision_text, decompression_text, sep = ""))) %>%
      ungroup() %>%
      select(level, object, side, decompression_text, revision)
    
    
    decompressions_performed_df <- decompression_prepared_df %>%
      select(-level) %>%
      distinct() %>%
      mutate(decompression_levels = pmap(.l = list(..1 = object, 
                                                   ..2 = side, 
                                                   ..3 = revision), .f = ~ collapsing_decompression_levels_function(input_df = decompression_prepared_df, object_input = ..1, side_input = ..2, revision_input = ..3))) %>%
      unnest() %>%
      mutate(final_decompression_text = if_else(str_detect(decompression_levels, "-"), 
                                                paste(decompression_text, decompression_levels, "interspace"), 
                                                paste(decompression_text, decompression_levels, "segment")))
    
    decompression_names <- posterior_decompression_df %>%
      select(object, side, revision) %>%
      distinct() %>%
      mutate(list_name = str_remove_all(string = paste(object, side, revision, sep = "_"), pattern = "_xx")) %>%
      select(list_name)
    # 
    list_decompressions <- decompressions_performed_df$final_decompression_text
    
    list_decompressions <- set_names(decompressions_performed_df$final_decompression_text, decompression_names$list_name)
    
    # names(list_decompressions) <- decompression_names
    
    
    procedures_summary_list <- append(procedures_summary_list, list_decompressions)
    
    

  }    
  
  ########### INTERBODY ########
  if(nrow(posterior_interbody_df) > 0){
    
    posterior_interbody_procedure_statements <- glue_collapse(x = pmap(.l = list(..1 = posterior_interbody_df$level, 
                                                                                 ..2 = posterior_interbody_df$object, 
                                                                                 ..3 = posterior_interbody_df$side), .f = ~ interbody_statement_function(level = ..1, object = ..2, side = ..3)),
                                                              sep = " Then I ", last = " Then I ")
    
    
    procedure_details_list$posterior_interbody_details <- paste(if_else(nrow(posterior_interbody_df) == 1, "I then proceeded with the interbody fusion. I ", 
                                                                        "I then proceeded with the interbody fusions. I first "), posterior_interbody_procedure_statements, " This completed the interbody fusion portion of the procedure.", sep = "")
    
    procedures_summary_list$posterior_interbody_fusion <- glue("Posterior Interbody fusion of {glue_collapse(x = posterior_interbody_df$level, sep = ', ', last = ', and ')}")
    
    if(nrow(posterior_interbody_df %>% filter(object == "tlif" | object == "plif")) > 0){
      procedures_summary_list$posterior_interbody_implant <- glue("Insertion of interbody biomechanical device to {glue_collapse(x = posterior_interbody_df$level, sep = ', ', last = ', and ')}")
    }
  }
  
############# COMPLETING INSTRUMENTATION ###########
  
  
  if(nrow(posterior_implant_df) > 0){
    
    if(any(str_detect(posterior_implant_df$side, "bilateral"))){
      procedure_details_list$posterior_instrumentation_rods <- paste("To complete the spinal instrumentation, a ", 
                                                                     glue("{left_main_rod_size} {left_main_rod_material} rod was contoured for the left and a {right_main_rod_size} {right_main_rod_material}"),
                                                                     " rod was contoured for the right and the rods placed into position and secured with set screws. ",
                                                                     additional_rods_statement,
                                                                     "The set screws were then tightened with a final tightener to the appropriate torque. Fluoroscopy was used to confirm the final position of all implants. This completed the instrumentation of ",
                                                                     glue_collapse(x = segments_instrumented$level, sep = ', ', last = ', and '), 
                                                                     ".") 
    }else{
      if(any(str_detect(posterior_implant_df$side, "left"))){
        procedure_details_list$posterior_instrumentation_rods <- paste("To complete the spinal instrumentation, a ", 
                                                                       glue("{left_main_rod_size} {left_main_rod_material}"),
                                                                       " rod was contoured for the left and placed into position and secured with set screws. ",
                                                                       additional_rods_statement,
                                                                       "The set screws were then tightened with a final tightener to the appropriate torque. Fluoroscopy was used to confirm the final position of all implants. This completed the instrumentation of ",
                                                                       glue_collapse(x = segments_instrumented$level, sep = ', ', last = ', and '), 
                                                                       ".") 
      }else{
        procedure_details_list$posterior_instrumentation_rods <- paste("To complete the spinal instrumentation, a ", 
                                                                       glue("{right_main_rod_size} {right_main_rod_material}"),
                                                                       " rod was contoured for the right and placed into position and secured with set screws. ",
                                                                       additional_rods_statement,
                                                                       "The set screws were then tightened with a final tightener to the appropriate torque. Fluoroscopy was used to confirm the final position of all implants. This completed the instrumentation of ",
                                                                       glue_collapse(x = segments_instrumented$level, sep = ', ', last = ', and '), 
                                                                       ".") 
      }
    }
    
  }
  
############# FUSIONS FUSIONS ###########
############# FUSIONS FUSIONS ###########

  if(nrow(posterior_fusion_df) > 0){
    fusion_segments_df <- posterior_fusion_df %>%
      separate(col = level, into = c("proximal", "distal")) %>%
      pivot_longer(cols = c(proximal, distal), names_to = "name", values_to = "level") %>%
      select(level) %>%
      distinct()
    
    if(bmp == 0){
      bmp_statement <-" "
    }else{
      bmp_statement <- glue("To improve the odds of a successful fusion, {bmp}mg of BMP was placed into the fusion bed. ")
    }
    if(is.na(allograft)){
      allograft_statement <-"bone graft "
    }else{
      allograft_statement <- glue("{allograft}cc of allograft ")
    }
    
    procedure_details_list$posterior_fusion_details <- paste("I then proceeded with the spinal fusion. The posterior elements of ", 
                                                             glue_collapse(fusion_segments_df$level, sep = ', ', last = ', and '), 
                                                             " were decorticated and ", 
                                                             allograft_statement, 
                                                             "was impacted into the lateral gutters. ", 
                                                             bmp_statement, 
                                                             "This completed the spinal fusion procedure of ", 
                                                             glue_collapse(x = posterior_fusion_df$level, sep = ', ', last = ', and '), 
                                                             ".", sep = "")

    
    procedures_summary_list$posterior_fusion <- glue("Posterior Spinal Fusion of {glue_collapse(x = posterior_fusion_df$level, sep = ', ', last = ', and ')} levels")
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
      
      closure_statements_list$skin_dressing_statement <- paste("The wound was then closed in a layered fashion. The fascial layer was closed with a watertight closure followed by a subdermal layer.",
                                                               if_else(length(closure_vector) > 0,
                                                                       (paste(glue_collapse(closure_vector, sep = ", ", last = " and "), " were used to close the skin.")),
                                                                       "A subcutaneous layer was used to seal the skin layer."),
                                                               "An incisional wound vac was then placed over the surgical incision.", sep = " ")
    }else{
      closure_vector <- closure
      closure_statements_list$skin_dressing_statement <- paste("The wound was then closed in a layered fashion. The fascial layer was closed with a watertight closure followed by a subdermal layer.",
                                                               if_else(length(closure_vector) > 0,
                                                                       (paste(glue_collapse(closure_vector, sep = ", ", last = " and "), " were used to close the skin.")),
                                                                       "A subcutaneous layer was used to seal the skin layer."),
                                                               "A watertight dressing was then placed over the surgical incision.", sep = " ")
      
    }
  }else{
    closure_statements_list$skin_dressing_statement <- paste("The wound was then closed in a layered fashion.",
                                                             "The fascial layer was closed with a watertight closure followed by a subdermal & subcutaneous layer.",
                                                             "A watertight dressing was then placed over the surgical incision.", sep = " ")
  }




  procedure_details_list$closure <- paste("I then proceeded with closure of the wound. ",
                                          glue_collapse(x = closure_statements_list, sep = " "),
                                          " All counts were correct at the conclusion of the case. ",
                                          "I was present and scrubbed for the entire case including ",
                                          glue_collapse(procedures_summary_list, sep = ', ', last = ', and '),
                                          ".",
                                          sep = "")
                                          
  
  return(list(procedures_performed_summary_list = procedures_summary_list,
              procedure_details_list = procedure_details_list))
  
}

