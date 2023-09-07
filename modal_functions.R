################################################    INITIAL STARTUP MODAL ######################################
################################################    INITIAL STARTUP MODAL ######################################
################################################    INITIAL STARTUP MODAL ######################################
################################################    INITIAL STARTUP MODAL ######################################

startup_modal_box <-
  function(header_text = "Enter Details to Proceed",
           header_text_color = "black",
           starting_first_name = "",
           starting_last_name = "",
           starting_dob = "",
           starting_dos = "",
           starting_sex = "", 
           hospital_input = "",
           starting_mrn = "",
           redcap_token_input = "",
           button_proceed = "proceed_to_details"
  ) {
    if (button_proceed == "proceed_to_details") {
      footer_button <- actionBttn(
        inputId = "close_startup_modal",
        label = "Proceed",
        style = "simple",
        color = "primary",
        icon = icon("arrow-right")
      )
    } else{
      footer_button <- modalButton("Proceed")
    }
    
    modalDialog(
      size = "l",
      easyClose = FALSE,
      footer = footer_button,
      column(
        12,
        fluidRow(column(8,
                        tags$div(
                          style = glue(
                            "font-size:22px; font-weight:bold; color:{header_text_color}"
                          ),
                          header_text
                        ),),
                 column(
                   4,
                   actionBttn(
                     inputId = "test_patient_button",
                     label = "Use Test Patient",
                     size = "sm"
                   )
                 ),),
        fluidRow(
          column(
            4,
            textInput(
              inputId = "patient_last_name",
              label = "Patient Last Name",
              value = starting_last_name
            ),
          ),
          column(
            4,
            textInput(
              inputId = "patient_first_name",
              label = "Patient First Name",
              value = starting_first_name
            ),
          ),
          column(
            4,
            textInput(inputId = "hospital", 
                      label = "Hospital/Institution:", 
                      value = hospital_input),
          )
        ),
        fluidRow(
          column(
            2,
            awesomeRadio(
              inputId = "sex",
              label = "Sex:",
              choices = c("Male", "Female"),
              selected = starting_sex,
              inline = TRUE
            )
          ),
          column(
            3,
            dateInput(
              inputId = "date_of_birth",
              label = "Date of Birth (mm-dd-yyyy):",
              value = starting_dob,
              format = "mm-dd-yyyy",
              max = Sys.Date() - 250
            )
          ),
          column(
            4,
            dateInput(
              inputId = "date_of_surgery",
              label = "Date of Surgery (mm-dd-yyyy):",
              value = starting_dos,
              format = "mm-dd-yyyy",
              max = Sys.Date() 
            )
          ),
          column(
            3,
            textInput(inputId = "hospital_mrn", 
                      label = "Hospital MRN:")
          )
        ),
        fluidRow(
          radioGroupButtons(
            inputId = "redcap_institution",
            label = "Institution:",
            choices = c("UTHSCSA", 
                        "UCSD"),
            individual = TRUE,
            selected = "UTHSCSA",
            checkIcon = list(
              yes = tags$i(class = "fa fa-circle", 
                           style = "color: steelblue"),
              no = tags$i(class = "fa fa-circle-o", 
                          style = "color: steelblue"))
          )
        ),
        fluidRow(
          column(
            6,
            textInput(inputId = "redcap_token", 
                      label = "Redcap Token ID:", 
                      placeholder = "## Enter Unique Redcap Token ##", 
                      value = redcap_token_input),
          ),
          column(
            6,
            br(),
            actionBttn(
              inputId = "search_for_prior_patient",
              label = "Retrieve this Patient",
              style = "simple",
              icon = icon("search"),
              color = "royal",
              size = "sm"
            ),
          )
        ),
        uiOutput(outputId = "prior_patient_match")
        # column(12, 
        #        textOutput(outputId = "match_found_result"),
        #        tags$div(style = "font-size:24px; font-weight:bold; color:darkblue; font-family:sans-serif; font-style:italic", "xxxx"),
        #        tableOutput(outputId = "patient_prior_data")
        #        )
      )
    )
  }
#############~~~~~~~~~~~~~~~~~~~~###################################    COMPLICATION MODAL 2 ##############~~~~~~~~~~~~~########################
#############~~~~~~~~~~~~~~~~~~~~###################################    COMPLICATION MODAL 2 ##############~~~~~~~~~~~~~########################
#############~~~~~~~~~~~~~~~~~~~~###################################    COMPLICATION MODAL 2 ##############~~~~~~~~~~~~~########################
#############~~~~~~~~~~~~~~~~~~~~###################################    COMPLICATION MODAL 2 ##############~~~~~~~~~~~~~####################

complication_modal_function <-   function(date_of_surgery_vector = NULL
) {
  
  modalDialog(
    size = "l", 
    easyClose = FALSE,
    # footer = modalButton("Proceed"),
    footer = actionBttn(
      inputId = "complication_done_button",
      label = "Done with Complications",
      style = "simple",
      color = "primary",
      icon = icon("arrow-right")
    ),
    box(
      width = 12,
      title = "Record Complication::",
      solidHeader = TRUE,
      status = "info",
      column(
        12,
        jh_make_shiny_table_row_function(left_column_label = "Date of Surgery", 
                                         input_type = "prettyRadioButtons", 
                                         input_id = "complication_date_of_surgery", 
                                         choices_vector = date_of_surgery_vector, 
                                         initial_value_selected = tail(date_of_surgery_vector, 1)),
        hr(),
        dateInput(
          inputId = "complication_date",
          label = "Date Complication First Observed: (mm-dd-yyyy):",
          value = "",
          format = "mm-dd-yyyy",
          min = head(date_of_surgery_vector, 1),
          max = Sys.Date() 
        ),
        hr(),
        jh_make_shiny_table_row_function(left_column_label = "Complication:",
                                         input_type = "radioGroupButtons", 
                                         input_id = "complication_description", 
                                         choices_vector = c("Ileus",
                                                            "Neurologic Deficit",
                                                            "New Leg Pain",
                                                            "Wound Infection", 
                                                            "Wound Complication",
                                                            "Postoperative Epidural Hematoma",
                                                            "Cauda Equina Syndrome",
                                                            "Deep Vein Thrombosis", 
                                                            "Urinary tract Infection",
                                                            "Delirium", 
                                                            "CSF Leak", 
                                                            "Arrhythmia or tachycardia",
                                                            "Pleural effusion",
                                                            "Pneumothorax",
                                                            "Pulmonary Congestion",
                                                            "Hemothorax",
                                                            "Hypotension",
                                                            "Medication Reaction",
                                                            "---",
                                                            "Pseudarthrosis", 
                                                            "Implant Failure",
                                                            "Malpositioned implant",
                                                            "Proximal Junctional Kyphosis", 
                                                            "Proximal Junctional Failure", 
                                                            "Distal Junctional Kyphosis",
                                                            "Adjacent segment Degeneration",
                                                            "Symptomatic Prominent Hardware",
                                                            "Painful SI joint following S2AI instrumentation", 
                                                            "Symptomatic Prominent Iliac Screw",
                                                            "Vertebral Compression Fracture",
                                                            "Permanent Neurologic Deficit",
                                                            "Death", 
                                                            "Other")),
        conditionalPanel(condition = "input.complication_description.indexOf('Other') > -1", 
                         fluidRow(
                           jh_make_shiny_table_row_function(left_column_label = "Explain Other:", 
                                                          input_id = "complication_other", 
                                                          input_type = "text",
                                                          initial_value_selected = "")
                         )
                         ), 
        conditionalPanel(condition = "input.complication_description.indexOf('Neurologic Deficit') > -1",
                         fluidRow(
                           jh_make_shiny_table_row_function(left_column_label = "Side, Distribution, and Severity of Neurologic Deficit:", 
                                                          input_id = "complication_neuro_deficit", 
                                                          input_type = "text")
                         )
                         ), 
        hr(),
        jh_make_shiny_table_row_function(left_column_label = "Additional Comments:", 
                                         input_id = "complication_comment", 
                                         input_type = "text", 
                                         initial_value_selected = ""),
        # hr(),
        # tableOutput(outputId = "complication_for_redcap_upload_table"),
        hr(),
        actionBttn(inputId = "complication_submit_to_redcap", label = "Submit Complication to Redcap & Reset", color = "success", size = "lg")
      )
    )
  )
}



#############~~~~~~~~~~~~~~~~~~~~###################################    STARTUP MODAL 2 ##############~~~~~~~~~~~~~########################
#############~~~~~~~~~~~~~~~~~~~~###################################    STARTUP MODAL 2 ##############~~~~~~~~~~~~~########################
#############~~~~~~~~~~~~~~~~~~~~###################################    STARTUP MODAL 2 ##############~~~~~~~~~~~~~########################
#############~~~~~~~~~~~~~~~~~~~~###################################    STARTUP MODAL 2 ##############~~~~~~~~~~~~~########################
#############~~~~~~~~~~~~~~~~~~~~###################################    STARTUP MODAL 2 ##############~~~~~~~~~~~~~########################
#############~~~~~~~~~~~~~~~~~~~~###################################    STARTUP MODAL 2 ##############~~~~~~~~~~~~~########################
#############~~~~~~~~~~~~~~~~~~~~###################################    STARTUP MODAL 2 ##############~~~~~~~~~~~~~########################
#############~~~~~~~~~~~~~~~~~~~~###################################    STARTUP MODAL 2 ##############~~~~~~~~~~~~~########################
#############~~~~~~~~~~~~~~~~~~~~###################################    STARTUP MODAL 2 ##############~~~~~~~~~~~~~########################

startup_modal_box_diagnosis_symptoms <-
  function(diagnosis_category_value = NULL,
           primary_diagnosis_value = NULL,
           other_diagnosis = NULL,
           symptoms_initial_value = "",
           symptoms_other = "",
           stage_number_value = 1,
           staged_procedure_initial_value = FALSE,
           multiple_approach_initial_value = FALSE,
           multi_approach_starting_position = "Posterior",
           spinal_regions_selected = c("Lumbar"),
           primary_or_revision = "Primary",
           levels_with_prior_decompression = "",
           prior_fusion_levels = "",
           prior_instrumentation = FALSE,
           revision_approach = "none",
           prior_anterior_plate_levels = c(),
           prior_anterior_plate_removed_levels = c(),
           left_prior_implants = "",
           left_prior_implants_removed = "",
           right_prior_implants = "",
           right_prior_implants_removed = "",
           left_rod_status = "retained_connected",
           left_implants_still_connected = "",
           right_rod_status = "retained_connected",
           right_implants_still_connected = "", 
           revision_indication = ""
  ) {
    
    diagnosis_section_category <- case_when(
      diagnosis_category_value == "Degen/Inflammatory" ~ "msk",
      diagnosis_category_value == "Deformity" ~ "deformity",
      diagnosis_category_value == "Trauma" ~ "trauma",
      diagnosis_category_value == "Tumor" ~ "tumor",
      diagnosis_category_value == "Infection" ~ "infection",
      diagnosis_category_value == "Congenital" ~ "congenital",
      diagnosis_category_value == "Other Neurological Diseases" ~ "other_neuro_conditions" 
      
    )
    
    
    modalDialog(
      size = "l", 
      easyClose = FALSE,
      # footer = modalButton("Proceed"),
      footer = actionBttn(
        inputId = "close_startup_modal_2",
        label = "Proceed",
        style = "simple",
        color = "primary",
        icon = icon("arrow-right")
      ),
      box(
        width = 12,
        title = "Diagnosis & Symptoms:",
        solidHeader = TRUE,
        status = "info",
        column(
          12,
          tags$div(style = "font-size:20px; font-weight:bold", "Select All Relevant Spinal Regions:"),
          fluidRow(
            checkboxGroupButtons(
              inputId = "spinal_regions",
              label = NULL,
              choices = spine_region_labels,
              individual = TRUE,
              size = "normal",
              justified = FALSE,
              selected = spinal_regions_selected,
              checkIcon = list(yes = icon("ok",
                                          lib = "glyphicon"))
            )
          ),
          hr(),
          conditionalPanel(
            condition = "input.spinal_regions.length > 0",
            tags$div(style = "font-size:20px; font-weight:bold", "Select Diagnostic Categories:"),
            tags$div(style = "font-size:14px; font-weight:bold", "(Select all that apply)"),
            fluidRow(
              checkboxGroupButtons(
                inputId = "diagnosis_category",
                label = NULL,
                choices = spine_category_labels,
                selected = diagnosis_category_value,
                checkIcon = list(yes = icon("ok",
                                            lib = "glyphicon"))
              )
            )
          ),
          br(),
          conditionalPanel(
            condition = "input.diagnosis_category.length > 0",
            fluidRow(
              column(
                width = 5,
                tags$div(style = "font-size:18px; font-weight:bold", "Diagnosis:"),
                tags$div(style = "font-size:14px; font-weight:bold", "(Select all that apply)")
              ),
              column(
                width = 7,
                pickerInput(
                  inputId = "primary_diagnosis",
                  label = "Diagnosis Search:",
                  choices = jh_filter_icd_codes_generate_vector_function(section_input = diagnosis_section_category, spine_region_input = spinal_regions_selected), 
                  options = pickerOptions(
                    liveSearch = TRUE,
                    virtualScroll = 50,
                    liveSearchNormalize = TRUE
                  ),
                  multiple = TRUE,
                  selected = primary_diagnosis_value
                )
              ),
            ),
            hr(),
            fluidRow(
              column(
                width = 5,
                tags$div(style = "font-size:18px; font-weight:bold", "Symptoms:")
              ),
              column(
                width = 7,
                pickerInput(
                  inputId = "symptoms",
                  label = NULL,
                  choices = list(
                    "Low Back & Legs:" = c("Low Back Pain", "Left Leg Pain", "Right Leg Pain")
                  ),
                  multiple = TRUE,
                  selected = symptoms_initial_value,
                  width = "100%"
                ),
                conditionalPanel(
                  condition = "input.symptoms.indexOf('Other') > -1",
                  textInput(inputId = "symptoms_other", label = "Other:", value = symptoms_other)
                )
              )
            ),
            fluidRow(
              textInput(inputId = "relevant_history", label = "Other Comments/History:")
            )
          )
        )
      ),
      box(
        width = 12,
        title = "General Procedure Details:",
        solidHeader = TRUE,
        status = "info",
        column(
          12,
          tags$div(style = "font-size:20px; font-weight:bold", "Procedure: Stage & Approach:"),
          jh_make_shiny_table_row_function(
            left_column_label = "Staged Procedure?",
            input_type = "switch",
            input_id = "staged_procedure",
            left_column_percent_width = 50,
            font_size = 16,
            switch_input_on_label = "Yes",
            switch_input_off_label = "No",
            initial_value_selected = staged_procedure_initial_value,
          ),
          conditionalPanel(
            condition = "input.staged_procedure == true",
            jh_make_shiny_table_row_function(
              left_column_label = "Stage Number:",
              input_type = "awesomeRadio",
              input_id = "stage_number",
              left_column_percent_width = 50,
              font_size = 14,
              choices_vector = c(1, 2, 3, 4, 5),
              checkboxes_inline = TRUE,
              initial_value_selected = stage_number_value
            )
          ),
          jh_make_shiny_table_row_function(
            left_column_label = "Multiple Approach, single stage?",
            input_type = "switch",
            input_id = "multiple_approach",
            left_column_percent_width = 50,
            font_size = 16,
            switch_input_on_label = "Yes",
            switch_input_off_label = "No",
            initial_value_selected = multiple_approach_initial_value
          ),
          conditionalPanel(condition = "input.multiple_approach == true",
                           jh_make_shiny_table_row_function(
                             left_column_label = "Starting Position:",
                             input_type = "awesomeRadio",
                             input_id = "multi_approach_starting_position",
                             left_column_percent_width = 50,
                             font_size = 14,
                             choices_vector = c("Posterior", "Anterior", "Lateral"),
                             checkboxes_inline = TRUE,
                             initial_value_selected = multi_approach_starting_position
                           )
          ),
          fluidRow(
            column(
              12,
              jh_make_shiny_table_row_function(
                left_column_label = "Primary or Revision:",
                input_type = "radioGroupButtons",
                input_id = "primary_revision",
                left_column_percent_width = 50,
                font_size = 16,
                choices_vector = c("Primary", "Revision"),
                initial_value_selected = primary_or_revision,
                checkboxes_inline = TRUE,
                individual_buttons = TRUE
              ), 
              conditionalPanel(condition = "input.primary_revision.indexOf('Revision') > -1",
                               fluidRow(column(
                                 width = 12,
                                 jh_make_shiny_table_row_function(
                                   left_column_label = "Anterior or Posterior Revision?",
                                   input_type = "awesomeRadio",
                                   input_id = "revision_approach",
                                   left_column_percent_width = 50,
                                   font_size = 16,
                                   choices =  c("Anterior" = "anterior", "Posterior" = "posterior", "NA" = "none"), 
                                   initial_value_selected = revision_approach,
                                   checkboxes_inline = TRUE,
                                   individual_buttons = TRUE
                                 )
                               )
                               )
              ),
              conditionalPanel(
                "input.revision_approach.indexOf('anterior') > -1 || input.revision_approach.indexOf('posterior') > -1 ",
                # "input.primary_revision.indexOf('Revision') > -1",
                jh_make_shiny_table_row_function(
                  left_column_label = "Revision for (select all that apply):",
                  input_type = "picker",
                  input_id = "revision_indication",
                  initial_value_selected = revision_indication,
                  left_column_percent_width = 60,
                  font_size = 14,
                  choices_vector = c("acute infection", 
                                     "wound complication", 
                                     "chronic infection", 
                                     "early radiculopathy",
                                     "malpositioned implant",
                                     "painful implant",
                                     "pseudarthrosis", 
                                     "rod fracture", 
                                     "proximal junction failure", 
                                     "distal junctional failure",
                                     "adjacent segment disease",
                                     "recurrent symptoms", 
                                     "sagittal imbalance", 
                                     "coronal imbalance",
                                     "other implant-related complication",
                                     "other")
                ),
                jh_make_shiny_table_row_function(
                  left_column_label = "Select Levels with Prior Decompression:",
                  input_type = "picker",
                  input_id = "open_canal",
                  initial_value_selected = levels_with_prior_decompression,
                  left_column_percent_width = 60,
                  font_size = 14,
                  choices_vector = (levels_numbered_df %>%
                                      filter(between(vertebral_number, 0.1, 25.1)) %>%
                                      filter(str_detect(level, "-", negate = TRUE)))$level
                  # open_canal_df$level
                ),
                jh_make_shiny_table_row_function(
                  left_column_label = "Select Prior Fusion Levels:",
                  input_type = "picker",
                  input_id = "prior_fusion_levels",
                  initial_value_selected = prior_fusion_levels,
                  left_column_percent_width = 60,
                  font_size = 14,
                  choices_vector = unique(interbody_levels_df$level)
                ),
                fluidRow(
                  jh_make_shiny_table_row_function(
                    left_column_label = "Prior Instrumentation?",
                    left_column_percent_width = 60,
                    font_size = 14,
                    input_type = "switch",
                    input_id = "prior_instrumentation",
                    initial_value_selected = prior_instrumentation
                  )
                ),
                conditionalPanel(condition = "input.prior_instrumentation == true", 
                                 conditionalPanel(
                                   condition = "input.revision_approach.indexOf('anterior') > -1",
                                   box(
                                     title = tags$div(style = "font-size:22px; font-weight:bold; text-align:center", "Prior Anterior Instrumentation"),
                                     width = 12,
                                     collapsible = TRUE,
                                     fluidRow(
                                       column(
                                         6,
                                         tags$div(style = "font-size:18px; font-weight:bold; text-align:center", "Plate Present:"),
                                         awesomeCheckboxGroup(
                                           inputId = "prior_anterior_plate_levels",
                                           label = "Prior Anterior Plate Levels:",
                                           selected = prior_anterior_plate_levels,
                                           choices = revision_anterior_plate_df$level
                                         )
                                       ),
                                       column(
                                         6,
                                         tags$div(style = "font-size:18px; font-weight:bold; text-align:center", "Plate Removed"),
                                         awesomeCheckboxGroup(
                                           inputId = "prior_anterior_plate_removed_levels",
                                           label = "Prior Anterior Plate Removed:",
                                           selected = prior_anterior_plate_removed_levels,
                                           choices = revision_anterior_plate_df$level
                                         )
                                       )
                                     )
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.revision_approach.indexOf('posterior') > -1",
                                   box(
                                     title = tags$div(style = "font-size:22px; font-weight:bold; text-align:center", "Prior Posterior Instrumentation"),
                                     width = 12,
                                     collapsible = TRUE,
                                     fluidRow(column(
                                       6,
                                       tags$div(style = "font-size:18px; font-weight:bold; text-align:center", "LEFT Implants")
                                     ),
                                     column(
                                       6,
                                       tags$div(style = "font-size:18px; font-weight:bold; text-align:center", "RIGHT Implants")
                                     )),
                                     fluidRow(
                                       column(
                                         5,
                                         column(
                                           6,
                                           awesomeCheckboxGroup(
                                             inputId = "left_revision_implants",
                                             label = "Present:",
                                             selected = left_prior_implants,
                                             choices = unique((revision_implants_df %>% filter(x < 0.5))$level)
                                           )
                                         ),
                                         column(
                                           6,
                                           awesomeCheckboxGroup(
                                             inputId = "left_revision_implants_removed",
                                             label = "Removed:",
                                             status = "danger",
                                             selected = left_prior_implants_removed,
                                             choices = unique((revision_implants_df %>% filter(x < 0.5))$level)
                                           )
                                         )
                                       ),
                                       column(1,),
                                       column(
                                         5,
                                         column(
                                           6,
                                           awesomeCheckboxGroup(
                                             inputId = "right_revision_implants",
                                             label = "Present:",
                                             selected = right_prior_implants,
                                             choices = unique((revision_implants_df %>% filter(x < 0.5))$level)
                                           )
                                         ),
                                         column(
                                           6,
                                           awesomeCheckboxGroup(
                                             inputId = "right_revision_implants_removed",
                                             label = "Removed:",
                                             status = "danger",
                                             selected = right_prior_implants_removed,
                                             choices = unique((revision_implants_df %>% filter(x < 0.5))$level)
                                           )
                                         )
                                       )
                                     )
                                   ),
                                   fluidRow(
                                     column(
                                       4,
                                       conditionalPanel(
                                         condition = "input.left_revision_implants.length > 1",
                                         awesomeRadio(
                                           inputId = "left_revision_rod_status",
                                           label = "Prior Left Rod was:",
                                           choices = c(
                                             "Removed" = "removed",
                                             "Retained" = "retained",
                                             "Cut and Partially Retained" = "retained_cut"
                                             ),
                                           selected = left_rod_status,
                                           inline = FALSE,
                                           status = "success"
                                         )
                                       )
                                     ),
                                     column(
                                       2,
                                       conditionalPanel(
                                         condition = "input.left_revision_rod_status.indexOf('retained_cut') > -1",
                                         pickerInput(
                                           inputId = "left_revision_implants_connected_to_prior_rod",
                                           label = "Select screws connected to the old rod:",
                                           choices = c(""),
                                           selected = left_implants_still_connected,
                                           multiple = TRUE
                                         )
                                       ),
                                       conditionalPanel(
                                         condition = "input.left_revision_rod_status.indexOf('retained_cut') > -1 || input.left_revision_rod_status.indexOf('retained') > -1",
                                         pickerInput(
                                           inputId = "left_revision_implants_rod_connectors",
                                           label = "Select the levels where rod connectors were placed below (if any)",
                                           choices = c(""),
                                           selected = c(""),
                                           multiple = TRUE
                                         )
                                       )
                                     ),
                                     column(
                                       4,
                                       conditionalPanel(
                                         condition = "input.right_revision_implants.length > 1",
                                         awesomeRadio(
                                           inputId = "right_revision_rod_status",
                                           label = "Prior Right Rod was:",
                                           choices = c(
                                             "Removed" = "removed",
                                             "Retained" = "retained",
                                             "Cut and Partially Retained" = "retained_cut"
                                           ),
                                           selected = right_rod_status,
                                           inline = FALSE,
                                           status = "success"
                                         )
                                       )
                                     ),
                                     column(
                                       2,
                                       conditionalPanel(
                                         condition = "input.right_revision_rod_status.indexOf('retained_cut') > -1",
                                         pickerInput(
                                           inputId = "right_revision_implants_connected_to_prior_rod",
                                           label = "Select screws connected to the old rod:",
                                           choices = c(""),
                                           selected = right_implants_still_connected,
                                           multiple = TRUE
                                         )
                                       ),
                                       conditionalPanel(
                                         condition = "input.right_revision_rod_status.indexOf('retained_cut') > -1 || input.right_revision_rod_status.indexOf('retained') > -1",
                                         pickerInput(
                                           inputId = "right_revision_implants_rod_connectors",
                                           label = "Select the levels where rod connectors were placed below (if any)",
                                           choices = c(""),
                                           selected = c(""),
                                           multiple = TRUE
                                         )
                                       )
                                     )
                                   )
                                 )
                )
              )
            )
          )
        )
      )
    )
  }

################################################    LATERAL MASS SCREW AND DECOMPRESSION SEQUENCE MODAL  ######################################
################################################    LATERAL MASS SCREW AND DECOMPRESSION SEQUENCE MODAL  ######################################
################################################    LATERAL MASS SCREW AND DECOMPRESSION SEQUENCE MODAL  ######################################

lateral_mass_screws_after_decompression_modal_function <- function(implant_objects_df = tibble(object = character(),
                                                                                               category = character()), 
                                                                   lateral_mass_screws_after_decompression = "No"){
  
  lateral_mass_screw_present <- any(implant_objects_df$object == "lateral_mass_screw")
  
  decompression_present <- any(implant_objects_df$category == "decompression")
  
  if(lateral_mass_screw_present == TRUE & decompression_present == TRUE){
    show_modal <- "Yes"
  }else{
    show_modal <- "No"
  }
  
  if(show_modal == "Yes"){
    showModal(  modalDialog(title = "Lateral Mass Screws and Decompression", 
                            easyClose = TRUE, 
                            footer = modalButton(label = "Confirmed"),
                            box(width = 12,
                                fluidRow(
                                  prettyRadioButtons(
                                    inputId = "lateral_mass_screws_after_decompression",
                                    label = "Were lateral mass screws placed AFTER the decompression?", 
                                    inline = TRUE,
                                    choices = c("No", 
                                                "Yes"),
                                    icon = icon("check"), 
                                    selected =  lateral_mass_screws_after_decompression,
                                    bigger = TRUE,
                                    status = "info"
                                  )
                                )
                            )
    )
    )
  }
}

################################################    FUSION AND TECHNIQUE DETAILS MODAL  ######################################
################################################    FUSION AND TECHNIQUE DETAILS MODAL  ######################################
################################################    FUSION AND TECHNIQUE DETAILS MODAL  ######################################
###############################################    FUSION AND TECHNIQUE DETAILS MODAL  ######################################

confirm_fusion_levels_and_technique_details_modal_box_function <- function(implants_placed = "no",
                                                                           procedure_approach = "",
                                                                           fusion_levels_confirmed = c(),
                                                                           approach_specified_posterior = "Midline",
                                                                           approach_open_mis = "Open",
                                                                           approach_robot_navigation = "NA",
                                                                           approach_specified_anterior = "Left-sided",
                                                                           implant_start_point_method = "Implant start points were identified using anatomic landmarks.",
                                                                           implant_position_confirmation_method = "Intraoperative fluoroscopy was used to confirm position of all implants.",
                                                                           deformity_correction_choices = c("The rods were secured into place with set screws. "), 
                                                                           alignment_correction_method = c("The rods were secured into place with set screws. "), 
                                                                           instruments_used_for_bony_work = "High-speed burr only", 
                                                                           row_label_font_size = 16, 
                                                                           question_label_column_width = 25, 
                                                                           question_text_align = "right", 
                                                                           anterior_approach, 
                                                                           posterior_approach){
  
  modalDialog(title = "Confirm Surgical Details:", 
              size = "l",
              easyClose = FALSE,
              footer = actionButton(inputId = "fusion_levels_technique_details_modal_complete_button", label = "Confirmed"),
              box(width = 12, title = div(style = "font-size:20px; font-weight:bold; text-align:left", "Approach & Technique Specifics:"),
                  if(posterior_approach == "yes"){
                    jh_make_shiny_table_row_function(
                      left_column_label = "POSTERIOR approach was:",
                      input_type = "prettyRadioButtons",
                      text_align = question_text_align,
                      input_id = "approach_specified_posterior",
                      left_column_percent_width = question_label_column_width,
                      font_size = row_label_font_size,
                      checkboxes_inline = TRUE,
                      choices_vector = c("Midline",
                                         "Paraspinal (Wiltse)"
                                         ),
                      initial_value_selected = approach_specified_posterior
                    )
                    },
                  hr(),
                  if(posterior_approach == "yes"){
                    jh_make_shiny_table_row_function(
                      left_column_label = "The procedure was performed:",
                      input_type = "prettyRadioButtons",
                      text_align = question_text_align,
                      input_id = "approach_open_mis",
                      left_column_percent_width = question_label_column_width,
                      font_size = row_label_font_size,
                      checkboxes_inline = TRUE,
                      choices_vector = c("Open",
                                         "Tubular", 
                                         "Endoscopic", 
                                         "Mini Open",
                                         "Percutaneous Screw"
                      ),
                      initial_value_selected = approach_open_mis
                    )
                    },
                    if(posterior_approach == "yes"){
                      jh_make_shiny_table_row_function(
                        left_column_label = "Select any modality used:",
                        input_type = "prettyRadioButtons",
                        text_align = question_text_align,
                        input_id = "approach_robot_navigation",
                        left_column_percent_width = question_label_column_width,
                        font_size = row_label_font_size,
                        checkboxes_inline = TRUE,
                        choices_vector = c("Fluoroscopy-guided",
                                           "Navigated", 
                                           "Robotic", 
                                           "NA"),
                        initial_value_selected = approach_robot_navigation
                      )
                      },
                  hr(),
                  if(anterior_approach == "yes"){
                    jh_make_shiny_table_row_function(
                      left_column_label = "ANTERIOR Approach was:",
                      input_type = "prettyRadioButtons",
                      text_align = question_text_align,
                      input_id = "approach_specified_anterior",
                      left_column_percent_width = question_label_column_width,
                      font_size = row_label_font_size,
                      checkboxes_inline = TRUE,
                      choices_vector = c("Left-sided", 
                                         "Right-sided",
                                         "Paramedian",
                                         "Lateral Transpsoas",
                                         "Lateral Antepsoas",
                                         "Thoracoabdominal",
                                         "Thoracotomy",
                                         "Transperitoneal",
                                         "Retroperitoneal"),
                      initial_value_selected = approach_specified_anterior
                    )
                  },
                  hr(),
                  if(implants_placed == "yes"){
                    if(procedure_approach == "posterior" | procedure_approach == "combined"){
                    jh_make_shiny_table_row_function(
                      input_type = "awesomeRadio",
                      left_column_label = "Method for identifying screw start point:",
                      text_align = question_text_align,
                      input_id = "implant_start_point_method",
                      left_column_percent_width = question_label_column_width,
                      font_size = row_label_font_size,
                      checkboxes_inline = FALSE,
                      choices_vector = c(
                        "Implant start points were identified using anatomic landmarks.",
                        "Intraoperative fluoroscopy and pedicle markers were used to confirm screw start points..", 
                        "Intraoperative fluoroscopy was used to identify and confirm implant start points.",
                        "Intraoperative navigation was used for identifying start points.",
                        "NA"),
                      initial_value_selected = implant_start_point_method
                    )
                    }
                  },
                  hr(),
                  if(implants_placed == "yes"){
                    jh_make_shiny_table_row_function(
                      input_type = "awesomeRadio",
                      left_column_label = "Method for confirming implant position:",
                      text_align = question_text_align,
                      input_id = "implant_position_confirmation_method",
                      left_column_percent_width = question_label_column_width,
                      font_size = row_label_font_size,
                      checkboxes_inline = FALSE,
                      choices_vector = c(
                        "Intraoperative fluoroscopy was used to confirm position of all implants.", 
                        "Intraoperative CT scan was used to confirm position of all implants.",
                        "NA"),
                      initial_value_selected = implant_position_confirmation_method
                    )
                  },
                  hr(),
                  if(implants_placed == "yes"){
                    if(procedure_approach == "posterior" | procedure_approach == "combined"){
                      prettyCheckboxGroup(inputId = "alignment_correction_method", 
                                          label = "Select any techniques for alignment correction:", 
                                          choices = deformity_correction_choices,
                                          selected = alignment_correction_method, 
                                          status = "primary",
                                          inline = FALSE)
                    }
                  },
                  if(implants_placed == "yes" && (procedure_approach == "posterior" | procedure_approach == "combined")){
                    conditionalPanel(condition = "input.alignment_correction_method.indexOf('Other') > -1",
                                     textInput(inputId = "alignment_correction_method_other", 
                                               label = "Enter 'Other' Method as a full sentence:"))
                  },
                  hr(),
                  jh_make_shiny_table_row_function(
                    input_type = "prettyRadioButtons",
                    left_column_label = "Select any instruments used:",
                    text_align = question_text_align,
                    input_id = "instruments_used_for_bony_work",
                    left_column_percent_width = question_label_column_width,
                    font_size = row_label_font_size,
                    checkboxes_inline = TRUE,  
                    status_color = "info",
                    choices_vector = c("High-speed burr only",
                                       "Bone scalpel only",
                                       "High-speed burr and bone scalpel",
                                       "NA"),
                    initial_value_selected = instruments_used_for_bony_work
                  ),
                  hr(),
                  jh_make_shiny_table_row_function(
                    input_type = "prettyCheckboxGroup",
                    left_column_label = "Please Confirm the Fusion Levels:",
                    text_align = question_text_align,
                    input_id = "fusion_levels_confirmed",
                    left_column_percent_width = question_label_column_width,
                    font_size = row_label_font_size,
                    checkboxes_inline = FALSE,
                    choices_vector = interbody_levels_df$level,
                    initial_value_selected = interbody_levels_df$level,
                    status_color = "success",
                  )
              )
  )
}



###################~~~~~~~~~~~~~~~~#############################    SURGICAL DETAILS MODAL  ##########~~~~~~~~~~~~~############################
################################################    SURGICAL DETAILS MODAL  ######################################
################################################    SURGICAL DETAILS MODAL  ######################################
################################################    SURGICAL DETAILS MODAL  ######################################
################################################    SURGICAL DETAILS MODAL  ######################################

###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   ADDITIONAL SURGICAL DETAILS MODAL  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
addition_surgical_details_modal_box_function <-
  function(required_options_missing = FALSE,
           editing_the_details = FALSE,
           row_label_font_size = 16,
           left_column_percent_width = 25,
           fade_appearance = TRUE,
           primary_surgeon_first_name_input = "",
           primary_surgeon_last_name_input = "",
           cosurgeon_yes_no = FALSE,
           cosurgeon = "",
           attending_assistant_yes_no  = FALSE,
           attending_assistant = "", 
           surgical_assistants = "",
           preoperative_diagnosis = " ",
           postoperative_diagnosis = " ",
           indications = "",
           asa_class = "",
           anesthesia = "",
           local_anesthesia = "None",
           neuromonitoring = c("SSEP", "tcMEP"),
           triggered_emg = "No",
           pre_positioning_motors = "Not obtained",
           neuromonitoring_signal_stability = "Neuromonitoring signals were stable throughout the case.",
           preop_antibiotics = c("Cefazolin (Ancef)"),
           preop_antibiotics_other = " ",
           anti_fibrinolytic = "",
           txa_loading = 20,
           txa_maintenance = 5,
           anterior_cervical_approach_details_checkbox = c()
  ) {
    
    if(editing_the_details == FALSE){
      footer_button <- actionBttn(
        inputId = "additional_surgical_details_1_complete",
        label = "Continue",
        icon = icon("fas fa-arrow-circle-right"), 
        style = "simple",
        color = "success"
      )
    }else{
      footer_button <- actionBttn(
        inputId = "editing_additional_surgical_details_1_complete",
        label = "Continue",
        icon = icon("fas fa-arrow-circle-right"), 
        style = "simple",
        color = "success"
      )
    }
    
    modalDialog(
      size = "l",
      easyClose = FALSE,
      fade = fade_appearance,
      footer = footer_button,
      box(
        width = 12,
        title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Additional Surgical Details:"),
        status = "info",
        solidHeader = TRUE,
        if (required_options_missing == TRUE) {
          div(style = "font-size:22px; font-weight:bold; font-style:italic; text-align:center; color:red", "*** Please Make Selections for Required Fields***")
        },
        tags$table(width = "100%",
                   tags$tr(width = "100%",
                           tags$td(width = paste0(left_column_percent_width, "%"), tags$div(style = "font-size:16px; font-weight:bold; text-align:left; margin-top:auto; margin-bottom:auto", "Primary Surgeon:")),
                           tags$td(width = paste0((100-left_column_percent_width)/2, "%"), textInput(inputId = "primary_surgeon_first_name", label = NULL, value = primary_surgeon_first_name_input, placeholder = "First Name", width = "80%")),
                           tags$td(width = paste0((100-left_column_percent_width)/2, "%"), textInput(inputId = "primary_surgeon_last_name", label = NULL, value = primary_surgeon_last_name_input, placeholder = "Last Name", width = "80%"))
                   ) 
        ),
        tags$table(width = "100%",
                   tags$tr(width = "100%",
                           tags$td(width = "30%",
                                   tags$div(style = "font-size:16px; font-weight:bold; text-align:right; margin-top:auto; margin-bottom:auto", "Was there a Co-Surgeon?")
                                   ),
                           tags$td(width = "10%", 
                                   switchInput(inputId = "cosurgeon_yes_no",
                                               label = NULL,
                                               onLabel = "Yes", 
                                               offLabel = "No",
                                               inline = TRUE, 
                                               value = cosurgeon_yes_no)
                                   ),
                           tags$td(width = "30%",
                                   tags$div(style = "font-size:16px; font-weight:bold; text-align:right; margin-top:auto; margin-bottom:auto", "Did an Attending Surgeon assist?")
                           ),
                           tags$td(width = "10%", 
                                   switchInput(inputId = "attending_assistant_yes_no",
                                               label = NULL,
                                               onLabel = "Yes", 
                                               offLabel = "No",
                                               inline = TRUE, 
                                               value = attending_assistant_yes_no)
                           )
                           )
        ),
        conditionalPanel(condition = "input.cosurgeon_yes_no == true",
                         jh_make_shiny_table_row_function(
                           left_column_percent_width = left_column_percent_width,
                           left_column_label = "Co-surgeon:",
                           font_size = row_label_font_size,
                           input_type = "text",
                           input_id = "cosurgeon", 
                           initial_value_selected = cosurgeon
                         )
                         ),
        conditionalPanel(condition = "input.attending_assistant_yes_no == true",
                         jh_make_shiny_table_row_function(
                           left_column_percent_width = left_column_percent_width,
                           left_column_label = "Attending Assistant:",
                           font_size = row_label_font_size,
                           input_type = "text",
                           input_id = "attending_assistant", 
                           initial_value_selected = attending_assistant
                         )
        ),
        jh_make_shiny_table_row_function(
          left_column_percent_width = left_column_percent_width,
          left_column_label = "Assistants:",
          font_size = row_label_font_size,
          input_type = "text",
          input_id = "surgical_assistants",
          initial_value_selected = surgical_assistants
        ),
        jh_make_shiny_table_row_function(
          left_column_percent_width = left_column_percent_width,
          left_column_label = "Preoperative Diagnosis:",
          font_size = row_label_font_size,
          input_type = "text",
          input_id = "preoperative_diagnosis",
          initial_value_selected = preoperative_diagnosis
        ),
        jh_make_shiny_table_row_function(
          left_column_percent_width = left_column_percent_width,
          left_column_label = "Postoperative Diagnosis:",
          font_size = row_label_font_size,
          input_type = "text",
          input_id = "postoperative_diagnosis",
          initial_value_selected = postoperative_diagnosis
        ),
        jh_make_shiny_table_row_function(
          left_column_percent_width = left_column_percent_width,
          left_column_label = "Surgical Indications:",
          font_size = row_label_font_size,
          input_type = "textAreaInput",
          input_id = "indications",
          initial_value_selected = indications
        ),
        hr(),
        jh_make_shiny_table_row_function(
          left_column_percent_width = left_column_percent_width,
          left_column_label = "Preprocedure ASA Classification:",
          font_size = row_label_font_size,
          input_type = "awesomeRadio",
          choices_vector = c("ASA I", "ASA II", "ASA III", "ASA IV", "ASA V", "ASA VI", "Emergent Surgery"),
          input_id = "asa_class",
          checkboxes_inline = TRUE,
          initial_value_selected = asa_class
        ),
        jh_make_shiny_table_row_function(
          left_column_percent_width = left_column_percent_width,
          left_column_label = "Anesthesia Type:",
          font_size = row_label_font_size,
          input_type = "checkbox",
          choices_vector = c(
            "General Endotracheal Anesthesia",
            "Spinal Anesthesia",
            "Epidural Anesthesia",
            "Monitored Anesthesia Care (MAC)"
          ),
          input_id = "anesthesia",
          checkboxes_inline = TRUE,
          initial_value_selected = anesthesia
        ),
        hr(),
        jh_make_shiny_table_row_function(
          left_column_percent_width = left_column_percent_width,
          left_column_label = "Local Anesthesia:",
          font_size = row_label_font_size,
          input_type = "awesomeRadio",
          choices_vector = c("None",
                             "During the EXPOSURE, Bupivicaine  was injected into the subcutaneous and deep tissues.",
                             "During the CLOSURE, Bupivicaine  was injected into the subcutaneous and deep tissues.",
                             "During the EXPOSURE, liposomal Bupivicaine was injected into the subcutaneous and deep tissues.",
                             "During CLOSURE, liposomal Bupivicaine was injected into the subcutaneous and deep tissues."
          ),
          input_id = "local_anesthesia", 
          checkboxes_inline = FALSE,
          initial_value_selected = local_anesthesia
        ),
        hr(),
        jh_make_shiny_table_row_function(
          left_column_percent_width = left_column_percent_width,
          left_column_label = "Neuromonitoring used:",
          font_size = row_label_font_size,
          input_type = "checkbox",
          input_id = "neuromonitoring",
          choices_vector = c("EMG", "SSEP", "tcMEP", "DNEP (Cord Stimulation)", "H reflex", "None"),
          checkboxes_inline = TRUE,
          initial_value_selected = neuromonitoring
        ),
        br(),
        conditionalPanel(condition = "input.neuromonitoring.indexOf('EMG') > -1",
                         jh_make_shiny_table_row_function(
                           left_column_percent_width = left_column_percent_width,
                           left_column_label = "Did you test screws with triggered EMG?",
                           font_size = row_label_font_size,
                           input_type = "awesomeRadio",
                           input_id = "triggered_emg", 
                           choices_vector = c("No", 
                                              "Triggered EMG was used to test screws and all responses were above 10mA.", 
                                              "Triggered EMG was used to test screws and all responses were above 20mA.", 
                                              "Triggered EMG was used to test screws and showed a response of *** at ***."),
                           checkboxes_inline = FALSE,
                           initial_value_selected = triggered_emg
                         )),
        br(),
        conditionalPanel(condition = "input.neuromonitoring.indexOf('tcMEP') > -1",
                         jh_make_shiny_table_row_function(
                           left_column_percent_width = left_column_percent_width,
                           left_column_label = "Pre-positioning motor signals were:",
                           font_size = row_label_font_size,
                           input_type = "checkbox",
                           input_id = "pre_positioning_motors", 
                           choices_vector = c("Not obtained", 
                                              "Present in the upper extremities",
                                              "Poor in the upper extremities",
                                              "Unreliable in the upper extremities",
                                              "Not detected in the upper extremities",
                                              "Present in the lower extremities",
                                              "Poor in the lower extremities",
                                              "Unreliable in the lower extremities",
                                              "Not detected in the lower extremities",
                                              "***"),
                           checkboxes_inline = FALSE,
                           initial_value_selected = pre_positioning_motors
                         )
        ),
        br(),
        conditionalPanel(condition = "input.neuromonitoring.indexOf('SSEP') > -1",
                         jh_make_shiny_table_row_function(
                           left_column_percent_width = left_column_percent_width,
                           left_column_label = "During the case:",
                           font_size = row_label_font_size,
                           input_type = "awesomeRadio",
                           input_id = "neuromonitoring_signal_stability", 
                           choices_vector = c("Neuromonitoring signals were stable throughout the case.", 
                                              "There were intermittent loss of neuromonitoring signals during the case due to ***.",
                                              "Following the decompression, neuromonitoring signals showed signs of improvement.",
                                              "During the case, neuromonitoring signals ***. ", 
                                              "***"),
                           checkboxes_inline = FALSE,
                           initial_value_selected = neuromonitoring_signal_stability
                         )
        ),
        hr(),
        jh_make_shiny_table_row_function(
          left_column_label = "Preop Antibiotics:",
          input_type = "checkbox",
          input_id = "preop_antibiotics",
          left_column_percent_width = left_column_percent_width,
          font_size = row_label_font_size,
          choices_vector = c(
            "None (Antibiotics were held)",
            "Cefazolin (Ancef)",
            "Vancomycin",
            "Ceftriaxone",
            "Gentamycin",
            "Clindamycin",
            "Aztreonam",
            "Cefepime",
            "Unknown",
            "Other"
          ),
          initial_value_selected = preop_antibiotics
        ),
        conditionalPanel(
          condition = "input.preop_antibiotics.indexOf('Other') > -1",
          jh_make_shiny_table_row_function(
            left_column_percent_width = 40,
            left_column_label = "Other Preop Antibiotics:",
            font_size = row_label_font_size,
            input_type = "text",
            input_id = "preop_antibiotics_other",
            initial_value_selected = preop_antibiotics_other
          )
        ),
        hr(),
        jh_make_shiny_table_row_function(
          left_column_label = "Antifibrinolytic:",
          input_type = "checkbox",
          input_id = "anti_fibrinolytic",
          left_column_percent_width = left_column_percent_width,
          font_size = row_label_font_size,
          choices_vector = c(
            "None",
            "Tranexamic Acid (TXA)",
            "Amicar",
            "Desmopressin (DDAVP)",
            "Other"
          ),
          initial_value_selected = anti_fibrinolytic,
        ),
        conditionalPanel(
          condition = "input.anti_fibrinolytic.indexOf('Tranexamic Acid (TXA)') > -1",
          jh_make_shiny_table_row_function(
            left_column_label = "TXA Loading (mg/kg):    ",
            input_type = "numeric",
            input_id = "txa_loading",
            left_column_percent_width = 50,
            font_size = row_label_font_size -
              1,
            min = 0,
            max = 200,
            initial_value_selected = txa_loading,
            step = 5,
            text_align = "right",
          ),
          jh_make_shiny_table_row_function(
            left_column_label = "TXA Maintenance (mg/kg/hr):    ",
            input_type = "numeric",
            input_id = "txa_maintenance",
            left_column_percent_width = 50,
            font_size = row_label_font_size -
              1,
            min = 0,
            max = 50,
            initial_value_selected = txa_maintenance,
            step = 5,
            text_align = "right",
          )
        ),
        hr(),
        jh_make_shiny_table_row_function(
          left_column_label = "Microscope",
          input_type = "checkbox",
          input_id = "anterior_cervical_approach_details_checkbox",
          left_column_percent_width = left_column_percent_width,
          font_size = row_label_font_size,
          choices_vector = c(
            "Microscope was utilized",
            "Caspar Pins were utilized"
          ),
          initial_value_selected = anterior_cervical_approach_details_checkbox,
        ),
      )
    )
  }


###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   ADDITIONAL SURGICAL DETAILS MODAL  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
addition_surgical_details_modal_box_2_function <-
  function(required_options_missing = FALSE,
           procedure_approach = "",
           row_label_font_size = 16,
           fade_appearance = TRUE,
           surgical_findings = "",
           specimens_removed = "",
           ebl = NULL,
           urine_output = NULL,
           crystalloids_administered = NULL,
           colloids_administered = NULL,
           transfusion = FALSE,
           cell_saver_transfused = 0,
           prbc_transfused = 0,
           ffp_transfused = 0,
           cryoprecipitate_transfused = 0,
           platelets_transfused = 0,
           intraoperative_complications_yes_no = "",
           intraoperative_complications_vector = NULL,
           other_intraoperative_complications = NULL,
           durotomy_timing_input = "",
           durotomy_instrument_input = "",
           durotomy_repair_method_input = "",
           
           head_positioning_anterior = "",
           deep_drains_anterior = 0,
           superficial_drains_anterior = 0,
           additional_end_procedure_details_anterior = NULL,
           closure_details_anterior = NULL,
           dressing_details_anterior = NULL,
           additional_procedures_choices_anterior = c(""),
           additional_procedures_anterior = NULL,
           additional_procedures_other_anterior = "",
           
           head_positioning_posterior = "",
           deep_drains_posterior = 1,
           superficial_drains_posterior = 0,
           additional_end_procedure_details_posterior = NULL,
           closure_details_posterior = NULL,
           dressing_details_posterior = NULL,
           additional_procedures_choices_posterior = c(""),
           additional_procedures_posterior = NULL,
           additional_procedures_other_posterior = "",

           postop_dispo = c(""),
           postop_abx = c("Ancef x 24hrs"),
           postop_map_goals = " ",
           postop_transfusion_threshold = " ",
           postop_imaging = c(""),
           postop_pain = c(""),
           postop_activity = c("PT/OT daily",
                               "please mobilize out of bed minimum 3x daily", 
                               "No bending, twisting, lifting > 10lbs"),
           postop_brace = c(""),
           postop_diet = c("Senokot-S daily + prn suppository daily if no BM in 24hrs + administer enema if no BM within 24hrs of suppository"),
           postop_foley = c("Discontinue foley once mobilizing with PT"),
           postop_dvt_ppx = c("SCD's while in bed", "Hold any chemical dvt ppx for minimum 72hrs"),
           postop_drains_dressing = c(""),
           postop_followup = c("")) {
    
    modalDialog(
      size = "l",
      easyClose = TRUE,
      fade = fade_appearance,
      footer = actionBttn(
        inputId = "additional_surgical_details_complete",
        label = "Continue",
        icon = icon("fas fa-arrow-circle-right"), 
        style = "simple",
        color = "success"
      ),
      box(
        width = 12,
        title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Additional Surgical Details:"),
        status = "info",
        solidHeader = TRUE,
        if (required_options_missing == TRUE) {
          div(style = "font-size:22px; font-weight:bold; font-style:italic; text-align:center; color:red", "*** Please Make/Confirm Selections for Required Fields***")
        },
        # if(procedure_approach == "combined"){
        #   jh_make_shiny_table_row_function(
        #     left_column_percent_width = 20,
        #     left_column_label = "Head Positioning:",
        #     font_size = row_label_font_size,
        #     input_type = "radioGroupButtons",
        #     input_id = "head_positioning", 
        #     individual_buttons = TRUE,
        #     required_option = TRUE,
        #     button_size = "xs",
        #     checkboxes_inline = TRUE,
        #     choices_vector = c(
        #       "Supine/Lateral",
        #       "Proneview Faceplate",
        #       "C-flex head positioner",
        #       "Cranial Tongs",
        #       "Halo",
        #       "Mayfield"
        #     ),
        #     initial_value_selected = head_positioning
        #   )
        # },
        div(style = "font-size:20px; font-weight:bold; text-align:center", "Additional Procedure & Closure Details:"),
        hr(),
        jh_make_shiny_table_row_function(
          left_column_percent_width = 30,
          left_column_label = "Intraoperative Findings:",
          font_size = row_label_font_size,
          input_type = "text",
          input_id = "surgical_findings",
          initial_value_selected = surgical_findings
        ),
        jh_make_shiny_table_row_function(
          left_column_percent_width = 30,
          left_column_label = "Specimens:",
          font_size = row_label_font_size,
          input_type = "text",
          input_id = "specimens_removed",
          initial_value_selected = specimens_removed
        ),
        hr(),
        jh_make_shiny_table_row_function(
          left_column_percent_width = 30,
          left_column_label = "Estimated Blood Loss:",
          font_size = row_label_font_size,
          input_type = "numeric",
          input_id = "ebl",
          initial_value_selected = ebl,
          min = 0,
          max = 50000,
          step = 100
        ),
        jh_make_shiny_table_row_function(
          left_column_percent_width = 30,
          left_column_label = "Urine Output:",
          font_size = row_label_font_size,
          input_type = "numeric",
          input_id = "urine_output",
          initial_value_selected = urine_output,
          min = 0,
          max = 50000,
          step = 100
        ),
        jh_make_shiny_table_row_function(
          left_column_percent_width = 30,
          left_column_label = "Crystalloids:",
          font_size = row_label_font_size,
          input_type = "numeric",
          input_id = "crystalloids_administered",
          initial_value_selected = crystalloids_administered,
          min = 0,
          max = 100000,
          step = 100
        ),
        jh_make_shiny_table_row_function(
          left_column_percent_width = 30,
          left_column_label = "Colloids:",
          font_size = row_label_font_size,
          input_type = "numeric",
          input_id = "colloids_administered",
          min = 0,
          initial_value_selected = colloids_administered,
          max = 100000,
          step = 100
        ),
        jh_make_shiny_table_row_function(
          left_column_percent_width = 60,
          left_column_label = "Transfusions/Cell Saver",
          font_size = row_label_font_size,
          input_type = "switch",
          input_id = "transfusion",
          switch_input_on_label = "Yes",
          switch_input_off_label = "No",
          initial_value_selected = transfusion
        ),
        conditionalPanel(
          condition = "input.transfusion == true",
          box(
            width = 12,
            jh_make_shiny_table_row_function(
              left_column_percent_width = 60,
              left_column_label = "Cell Saver Transfused (cc):",
              font_size = row_label_font_size,
              input_type = "numeric",
              input_id = "cell_saver_transfused",
              initial_value_selected = cell_saver_transfused,
              min = 0,
              max = 10000,
              step = 100
            ),
            jh_make_shiny_table_row_function(
              left_column_percent_width = 60,
              left_column_label = "pRBC units transfused:",
              font_size = row_label_font_size,
              input_type = "numeric",
              input_id = "prbc_transfused",
              initial_value_selected = prbc_transfused,
              min = 0,
              max = 100,
              step = 1
            ),
            jh_make_shiny_table_row_function(
              left_column_percent_width = 60,
              left_column_label = "FFP units transfused:",
              font_size = row_label_font_size,
              input_type = "numeric",
              input_id = "ffp_transfused",
              initial_value_selected = ffp_transfused,
              min = 0,
              max = 100,
              step = 1
            ),
            jh_make_shiny_table_row_function(
              left_column_percent_width = 60,
              left_column_label = "Cryoprecipitate units transfused:",
              font_size = row_label_font_size,
              input_type = "numeric",
              input_id = "cryoprecipitate_transfused",
              initial_value_selected = cryoprecipitate_transfused,
              min = 0,
              max = 100,
              step = 1
            ),
            jh_make_shiny_table_row_function(
              left_column_percent_width = 60,
              left_column_label = "Platelet units transfused:",
              font_size = row_label_font_size,
              input_type = "numeric",
              input_id = "platelets_transfused",
              initial_value_selected = platelets_transfused,
              min = 0,
              max = 100,
              step = 1
            ),
          )
        ),
        hr(),
        jh_make_shiny_table_row_function(required_option = TRUE, 
                                         left_column_label = "Complications?", 
                                         left_column_percent_width = 40,
                                         font_size = row_label_font_size, 
                                         input_type = "radioGroupButtons",
                                         input_id = "intraoperative_complications_yes_no", 
                                         initial_value_selected = intraoperative_complications_yes_no,  
                                         justified_radio_buttons = TRUE, 
                                         choices_vector = c("No", "Yes"), 
                                         status_color = "danger",
                                         # justified_radio_buttons = TRUE, 
                                         checkboxes_inline = TRUE, 
                                         individual_buttons = TRUE),
        br(),
        conditionalPanel(
          condition = "input.intraoperative_complications_yes_no == 'Yes'", 
          fluidRow(
            column(4, 
            ),
            column(8, 
                   box(
                     width = 12,
                     jh_make_shiny_table_row_function(
                       left_column_percent_width = 40,
                       left_column_label = "Select any:",
                       font_size = row_label_font_size,
                       input_type = "checkbox",
                       input_id = "intraoperative_complications_vector",
                       choices_vector = c(
                         "Durotomy",
                         "Nerve Root Injury",
                         "Loss of Neuromonitoring Data with Return",
                         "Loss of Neuromonitoring Data without Return", 
                         "Other"
                       ),
                       initial_value_selected = intraoperative_complications_vector
                     ),
                     conditionalPanel(
                       condition = "input.intraoperative_complications_vector.indexOf('Other') > -1",
                       jh_make_shiny_table_row_function(
                         left_column_percent_width = 40,
                         left_column_label = "Other Intraoperative Complications:",
                         font_size = row_label_font_size,
                         input_type = "text",
                         input_id = "other_intraoperative_complications",
                         initial_value_selected = other_intraoperative_complications
                       )
                       ),
                   ),
                   conditionalPanel(
                     condition = "input.intraoperative_complications_vector.indexOf('Durotomy') > -1",
                     box(
                       width = 12,
                       title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Durotomy Details:"),
                       status = "info",
                       solidHeader = TRUE,
                       jh_make_shiny_table_row_function(
                         left_column_label = "Durotomy Occurred During:",
                         input_type = "awesomeRadio",
                         input_id = "durotomy_timing",
                         left_column_percent_width = 45,
                         font_size = 18,
                         choices_vector = c("Exposure", "Decompression", "Other"),
                         initial_value_selected = durotomy_timing_input,
                         checkboxes_inline = FALSE,
                         return_as_full_table = TRUE
                       ),
                       br(),
                       jh_make_shiny_table_row_function(
                         left_column_label = "What Instrument was Involved?",
                         input_type = "awesomeRadio",
                         input_id = "durotomy_instrument",
                         left_column_percent_width = 45,
                         font_size = 18,
                         choices_vector = c("Burr", "Kerrison", "Rongeur", "Bovie", "Other"),
                         initial_value_selected = durotomy_instrument_input,
                         checkboxes_inline = FALSE,
                         return_as_full_table = TRUE
                       ),
                       jh_make_shiny_table_row_function(
                         left_column_label = "The dura was repaired using:",
                         input_type = "checkbox",
                         input_id = "durotomy_repair_method",
                         left_column_percent_width = 45,
                         font_size = 18,
                         choices_vector = c(
                           "Primarily Repaired using Suture",
                           "Dural Sealant",
                           "Tachosil",
                           "Dural Graft",
                           "No Repair Performed",
                           "Other Repair"
                         ),
                         initial_value_selected = durotomy_repair_method_input,
                         checkboxes_inline = FALSE,
                         return_as_full_table = TRUE
                       )
                     )
                   )
            )
            
          )
        ),
        br(),
        hr(),
        ############# ANTERIOR ############# ############# ANTERIOR ############# ############# ANTERIOR #############
        
        ############# ANTERIOR ############# ############# ANTERIOR ############# ############# ANTERIOR #############

        if(procedure_approach == "anterior" | procedure_approach == "combined"){
          div(style = "font-size:20px; font-weight:bold; text-align:center", "ANTERIOR Details:")
          },
        if(procedure_approach == "anterior" | procedure_approach == "combined"){
          jh_make_shiny_table_row_function(
            left_column_percent_width = 20,
            left_column_label = "Anterior Head Positioning:",
            font_size = row_label_font_size,
            input_type = "radioGroupButtons",
            input_id = "head_positioning_anterior",
            required_option = TRUE,
            individual_buttons = TRUE,
            button_size = "xs",
            checkboxes_inline = TRUE,
            choices_vector = c(
              "Supine/Lateral",
              "C-flex head positioner",
              "Cranial Tongs",
              "Halo",
              "Mayfield"
            ),
            initial_value_selected = head_positioning_anterior
          )
        },
        if(procedure_approach == "anterior" | procedure_approach == "combined"){br()},
        if(procedure_approach == "anterior" | procedure_approach == "combined"){
          jh_make_shiny_table_row_function(left_column_label = "Anterior Deep drains:", 
                                           input_type = "awesomeRadio",
                                           input_id = "deep_drains_anterior", 
                                           left_column_percent_width = 45, 
                                           font_size = 16, 
                                           initial_value_selected = deep_drains_anterior, 
                                           choices_vector = c("0", "1", "2", "3", "4", "5"), 
                                           checkboxes_inline = TRUE, return_as_full_table = TRUE)
        },
        if(procedure_approach == "anterior" | procedure_approach == "combined"){br()},
        if(procedure_approach == "anterior" | procedure_approach == "combined"){
          jh_make_shiny_table_row_function(left_column_label = "Anterior Superficial drains:", 
                                           input_type = "awesomeRadio",
                                           input_id = "superficial_drains_anterior", 
                                           left_column_percent_width = 45, 
                                           font_size = 16, 
                                           initial_value_selected = superficial_drains_anterior, 
                                           choices_vector = c("0", "1", "2", "3", "4", "5"), 
                                           checkboxes_inline = TRUE, return_as_full_table = TRUE)
        },
        if(procedure_approach == "anterior" | procedure_approach == "combined"){br()},
        if(procedure_approach == "anterior" | procedure_approach == "combined"){
          jh_make_shiny_table_row_function(
            left_column_label = "Select any used during anterior closure:",
            input_type = "checkbox",
            input_id = "additional_end_procedure_details_anterior",
            left_column_percent_width = 45,
            font_size = row_label_font_size,
            choices_vector = c("Vancomycin Powder",
                               "Antibiotic Beads"),
            initial_value_selected = additional_end_procedure_details_anterior,
            return_as_full_table = TRUE
          )
        },
        if(procedure_approach == "anterior" | procedure_approach == "combined"){br()},
        if(procedure_approach == "anterior" | procedure_approach == "combined"){
          jh_make_shiny_table_row_function(
            left_column_label = "Anterior Skin Closure:",
            input_type = "checkbox",
            input_id = "closure_details_anterior",
            left_column_percent_width = 45,
            font_size = row_label_font_size,
            required_option = TRUE,
            choices_vector = c("Subcutaneous suture",
                               "Nylon",
                               "Staples", 
                               "left open"),
            initial_value_selected = closure_details_anterior,
            return_as_full_table = TRUE
          )
        },
        if(procedure_approach == "anterior" | procedure_approach == "combined"){br()},
        if(procedure_approach == "anterior" | procedure_approach == "combined"){
          jh_make_shiny_table_row_function(
            left_column_label = "Anterior Skin/Dressing:",
            input_type = "checkbox",
            input_id = "dressing_details_anterior",
            required_option = TRUE,
            left_column_percent_width = 45,
            font_size = row_label_font_size,
            choices_vector = c(
              "Steristrips",
              "Dermabond",
              "Prineo",
              "an Incisional Wound Vac",
              "Wound Vac",
              "a water tight dressing"
            ),
            initial_value_selected = dressing_details_anterior,
            return_as_full_table = TRUE
          )
        },
        if(procedure_approach == "anterior" | procedure_approach == "combined"){br()},
        if(procedure_approach == "anterior" | procedure_approach == "combined"){h4("Confirm Any Additional Anterior Procedures Performed:")},
        if(procedure_approach == "anterior" | procedure_approach == "combined"){
          jh_make_shiny_table_row_function(
            left_column_label = "Additional Anterior Procedures:",
            font_size = row_label_font_size,
            input_id = "additional_procedures_anterior",
            left_column_percent_width = 35,
            checkboxes_inline = FALSE,
            input_type = "checkbox",
            choices_vector = additional_procedures_choices_anterior,
            initial_value_selected = additional_procedures_anterior
          )
        },
        if(procedure_approach == "anterior" | procedure_approach == "combined"){br()},
        if(procedure_approach == "anterior" | procedure_approach == "combined"){
          conditionalPanel(
            condition = "input.additional_procedures_anterior.indexOf('Other') > -1",
            tags$table(
              width = "90%" ,
              jh_make_shiny_table_row_function(
                left_column_label = "Other Procedures:",
                font_size = row_label_font_size - 1,
                input_type = "text",
                input_id = "additional_procedures_other_anterior",
                left_column_percent_width = 30,
                initial_value_selected = additional_procedures_other_anterior,
              )
            )
          )
        },
        ############# POSTERIOR ############# ############# POSTERIOR ############# ############# POSTERIOR #############
        
        ############# POSTERIOR ############# ############# POSTERIOR ############# ############# POSTERIOR #############
        
        hr(),
        if(procedure_approach == "posterior" | procedure_approach == "combined"){
          div(style = "font-size:20px; font-weight:bold; text-align:center", "POSTERIOR Details:")
        },
        if(procedure_approach == "posterior"  | procedure_approach == "combined"){
          jh_make_shiny_table_row_function(
            left_column_percent_width = 20,
            left_column_label = "Posterior Head Positioning:",
            font_size = row_label_font_size,
            input_type = "radioGroupButtons",
            input_id = "head_positioning_posterior",
            individual_buttons = TRUE,
            required_option = TRUE,
            button_size = "xs",
            checkboxes_inline = TRUE,
            choices_vector = c(
              "Proneview Faceplate",
              "C-flex head positioner",
              "Cranial Tongs",
              "Halo",
              "Mayfield"
            ),
            initial_value_selected = head_positioning_posterior
          )
        },
        if(procedure_approach == "posterior" | procedure_approach == "combined"){br()},
        if(procedure_approach == "posterior" | procedure_approach == "combined"){
          jh_make_shiny_table_row_function(left_column_label = "Posterior Deep drains:", 
                                           input_type = "awesomeRadio",
                                           input_id = "deep_drains_posterior", 
                                           left_column_percent_width = 45, 
                                           font_size = 16, 
                                           initial_value_selected = deep_drains_posterior, 
                                           choices_vector = c("0", "1", "2", "3", "4", "5"), 
                                           checkboxes_inline = TRUE, return_as_full_table = TRUE)
        },
        if(procedure_approach == "posterior" | procedure_approach == "combined"){
          jh_make_shiny_table_row_function(left_column_label = "Posterior Superficial drains:", 
                                           input_type = "awesomeRadio",
                                           input_id = "superficial_drains_posterior", 
                                           left_column_percent_width = 45, 
                                           font_size = 16, 
                                           initial_value_selected = superficial_drains_posterior, 
                                           choices_vector = c("0", "1", "2", "3", "4", "5"), 
                                           checkboxes_inline = TRUE, return_as_full_table = TRUE)
        },
        if(procedure_approach == "posterior" | procedure_approach == "combined"){br()},
        if(procedure_approach == "posterior" | procedure_approach == "combined"){
          jh_make_shiny_table_row_function(
            left_column_label = "Select any used during posterior closure:",
            input_type = "checkbox",
            input_id = "additional_end_procedure_details_posterior",
            left_column_percent_width = 45,
            font_size = row_label_font_size,
            choices_vector = c("Vancomycin Powder",
                               "Antibiotic Beads"),
            initial_value_selected = additional_end_procedure_details_posterior,
            return_as_full_table = TRUE
          )
        },
        if(procedure_approach == "posterior" | procedure_approach == "combined"){br()},
        if(procedure_approach == "posterior" | procedure_approach == "combined"){
          jh_make_shiny_table_row_function(
            left_column_label = "Posterior Skin Closure:",
            input_type = "checkbox",
            input_id = "closure_details_posterior",
            left_column_percent_width = 45,
            font_size = row_label_font_size,
            required_option = TRUE,
            choices_vector = c("Subcutaneous suture",
                               "Nylon",
                               "Staples", 
                               "left open"),
            initial_value_selected = closure_details_posterior,
            return_as_full_table = TRUE
          )
        },
        if(procedure_approach == "posterior" | procedure_approach == "combined"){br()},
        if(procedure_approach == "posterior" | procedure_approach == "combined"){
          jh_make_shiny_table_row_function(
            left_column_label = "Posterior Skin/Dressing:",
            input_type = "checkbox",
            input_id = "dressing_details_posterior",
            required_option = TRUE,
            left_column_percent_width = 45,
            font_size = row_label_font_size,
            choices_vector = c(
              "Steristrips",
              "Dermabond",
              "Prineo",
              "an Incisional Wound Vac",
              "Wound Vac",
              "a water tight dressing"
            ),
            initial_value_selected = dressing_details_posterior,
            return_as_full_table = TRUE
          )
        },
        if(procedure_approach == "posterior" | procedure_approach == "combined"){br()},
        if(procedure_approach == "Posterior" | procedure_approach == "combined"){h4("Confirm Any Additional Posterior Procedures Performed:")},
        if(procedure_approach == "posterior" | procedure_approach == "combined"){
          jh_make_shiny_table_row_function(
            left_column_label = "Additional Posterior Procedures:",
            font_size = row_label_font_size,
            input_id = "additional_procedures_posterior",
            left_column_percent_width = 20,
            checkboxes_inline = FALSE,
            input_type = "checkbox",
            choices_vector = additional_procedures_choices_posterior,
            initial_value_selected = additional_procedures_posterior
          )
        },
        br(),
        if(procedure_approach == "posterior" | procedure_approach == "combined"){
          conditionalPanel(
            condition = "input.additional_procedures_posterior.indexOf('Other') > -1",
            tags$table(
              width = "90%" ,
              jh_make_shiny_table_row_function(
                left_column_label = "Other Procedures:",
                font_size = row_label_font_size - 1,
                input_type = "text",
                input_id = "additional_procedures_other_posterior",
                left_column_percent_width = 30,
                initial_value_selected = additional_procedures_other_posterior,
              )
            )
          )
        },
        br(),
        hr(),
        h2("Postop Plan:"),
        jh_make_shiny_table_row_function(
          left_column_label = "Dispo:",
          font_size = row_label_font_size,
          input_id = "postop_dispo",
          left_column_percent_width = 20,
          checkboxes_inline = FALSE,
          input_type = "awesomeRadio",
          choices_vector = c("discharge to home. Monitor in recovery for 4hrs for difficulty swallowing, breathing, other issues.", 
                             "admit to floor.", 
                             "admit to ICU.",
                             "***"),
          initial_value_selected = postop_dispo
        ),
        br(),
        jh_make_shiny_table_row_function(
          left_column_label = "Antibiotics:",
          font_size = row_label_font_size,
          input_id = "postop_abx",
          left_column_percent_width = 20,
          checkboxes_inline = FALSE,
          input_type = "checkbox",
          choices_vector = c("Ancef x 24hrs",
                             "Ancef x 48hrs",
                             "Vancomycin x 48hrs",
                             "Continue broad spectrum antibiotics and f/u cultures",
                             "Per infectious disease team recommendations",
                             "Ancef until drains removed",
                             "***"),
          initial_value_selected = postop_abx
        ),
        br(),
        jh_make_shiny_table_row_function(
          left_column_label = "MAP Goals:",
          font_size = row_label_font_size,
          input_id = "postop_map_goals",
          left_column_percent_width = 20,
          checkboxes_inline = FALSE,
          input_type = "checkbox",
          choices_vector = c("No MAP goals needed",
                             "Please maintain MAPs 85-90 for a total of 7 days post-injury per AANS/CNS guidelines for incomplete spinal cord injury",
                             "Please maintain MAPs 85-90 for a total of 5 days post-injury",
                             "Please maintain MAPs 85-90 for a total of 2 days post-injury",
                             "***"),
          initial_value_selected = postop_map_goals
        ),
        br(),
        jh_make_shiny_table_row_function(
          left_column_label = "Acute Blood Loss Anemia:",
          font_size = row_label_font_size,
          input_id = "postop_transfusion_threshold",
          left_column_percent_width = 20,
          checkboxes_inline = FALSE,
          input_type = "checkbox",
          choices_vector = c("Please check daily CBC & transfuse for Hgb < 10 for first 48hrs",
                             "***"),
          initial_value_selected = postop_transfusion_threshold
        ),
        br(),
        jh_make_shiny_table_row_function(
          left_column_label = "Postop Imaging:",
          font_size = row_label_font_size,
          input_id = "postop_imaging",
          left_column_percent_width = 20,
          checkboxes_inline = FALSE,
          input_type = "checkbox",
          choices_vector = c("please obtain postop upright AP/Lateral C-spine xrays",
                             "please obtain postop upright AP/Lateral T-spine xrays prior to discharge when able to stand & after drains out",
                             "please obtain postop upright AP/Lateral L-spine xrays prior to discharge when able to stand & after drains out",
                             "please obtain postop upright AP/Lateral standing scoliosis xrays prior to discharge when able to stand & after drains out",
                             "no postop imaging needed.",
                             "***"),
          initial_value_selected = postop_imaging
        ),
        br(),
        jh_make_shiny_table_row_function(
          left_column_label = "Pain Control:",
          font_size = row_label_font_size,
          input_id = "postop_pain",
          left_column_percent_width = 20,
          checkboxes_inline = FALSE,
          input_type = "checkbox",
          choices_vector = c("Ice pack to affected area, PRN",
                             "Dilaudid PCA (in addition to all other pain medication orders) - DISCONTINUE on POD 1", 
                             "Oxycodone 5mg q4h, ok to give additional dose if needed", 
                             "Oxycodone 10mg q4h, ok to give additional dose if needed",
                             "Tylenol 1000mg q8h scheduled.", 
                             "Tylenol 650mg q8h scheduled",
                             "Gabapentin 100mg q8h scheduled",
                             "Gabapentin 300mg q8h scheduled",
                             "Toradol 15mg q8h for 48-72hrs",
                             "Toradol 30mg q8h for 48-72hrs",
                             "Celebrex 100mg BID",
                             "Celebrex 200mg BID",
                             "Meloxicam 15mg daily",
                             "Tizanidine 4mg q6h prn for muscle spasms (avoid in geriatric males/males w/ urinary retention)",
                             "Cyclobenzabrine 5mg q8h prn for muscle spasms (strong anticholinergic, minimize in geriatric)",
                             "Baclofen 10mg TID prn for muscle spasms",
                             "Diazepam 2.5mg q8h prn for muscle spasms",
                             "Pain team consult",
                             "Per Primary",
                             "Continue home pain medications ***",
                             "***"
          ),
          initial_value_selected = postop_pain
        ),
        br(),
        jh_make_shiny_table_row_function(
          left_column_label = "Activity:",
          font_size = row_label_font_size,
          input_id = "postop_activity",
          left_column_percent_width = 20,
          checkboxes_inline = FALSE,
          input_type = "checkbox",
          choices_vector = c("PT/OT daily",
                             "please mobilize out of bed minimum 3x daily", 
                             "No bending, twisting, lifting > 10lbs",
                             "***"),
          initial_value_selected = postop_activity
        ),
        br(),
        jh_make_shiny_table_row_function(
          left_column_label = "Bracing:",
          font_size = row_label_font_size,
          input_id = "postop_brace",
          left_column_percent_width = 20,
          checkboxes_inline = FALSE,
          input_type = "checkbox",
          choices_vector = c("C collar at all times",
                             "Soft Collar for comfort only",
                             "TLSO brace while ambulating (OK to remove while in bed, sitting, or bathing",
                             "No brace needed",
                             "***"),
          initial_value_selected = postop_brace
        ),
        br(),
        jh_make_shiny_table_row_function(
          left_column_label = "Diet/GI:",
          font_size = row_label_font_size,
          input_id = "postop_diet",
          left_column_percent_width = 20,
          checkboxes_inline = FALSE,
          input_type = "checkbox",
          choices_vector = c("OK to resume normal diet", 
                             "Start with clear liquid diet and advance as tolerated", 
                             "Start with clear liquid diet and advance when passing flatus",
                             "NPO until return of bowel sounds",
                             "Please place Nutrition Consult",
                             "Senokot-S daily + prn suppository daily if no BM in 24hrs + administer enema if no BM within 24hrs of suppository"),
          initial_value_selected = postop_diet
        ),
        br(),
        jh_make_shiny_table_row_function(
          left_column_label = "Foley:",
          font_size = row_label_font_size,
          input_id = "postop_foley",
          left_column_percent_width = 20,
          checkboxes_inline = FALSE,
          input_type = "checkbox",
          choices_vector = c("Discontinue foley when able",
                             "Discontinue foley once mobilizing with PT",
                             "***"
                             ),
          initial_value_selected = postop_foley
        ),
        br(),
        jh_make_shiny_table_row_function(
          left_column_label = "DVT ppx & anticoag/antiplatelet:",
          font_size = row_label_font_size,
          input_id = "postop_dvt_ppx",
          left_column_percent_width = 20,
          checkboxes_inline = FALSE,
          input_type = "checkbox",
          choices_vector = c("SCD's while in bed",
                             "OK to restart dvt ppx",
                             "Please hold any chemical dvt ppx for 48hrs",
                             "Please hold any chemical dvt ppx/anti-platelet/anti-coag for 5 days after surgery (High risk for hematoma/wound complications)",
                             "Please do not start any chemical dvt ppx without discussion with spine team",
                             "***"),
          initial_value_selected = postop_dvt_ppx
        ),
        br(),
        jh_make_shiny_table_row_function(
          left_column_label = "Drains & Dressing:",
          font_size = row_label_font_size,
          input_id = "postop_drains_dressing",
          left_column_percent_width = 20,
          checkboxes_inline = FALSE,
          input_type = "checkbox",
          choices_vector = c("Monitor and record drain output q12h", 
                             "Incisional wound vac continuous therapy",
                             "Wound vac to continuous therapy",
                             "Please keep a watertight dressing in place over wound and reinforce as needed with tegaderms",
                             "Please change to Silverlon dressing prior to discharge from hospital",
                             "Please change surgical dressing prior to discharge from hospital",
                             "***"),
          initial_value_selected = postop_drains_dressing
        ),
        br(),
        jh_make_shiny_table_row_function(
          left_column_label = "Follow-up:",
          font_size = row_label_font_size,
          input_id = "postop_followup",
          left_column_percent_width = 20,
          checkboxes_inline = FALSE,
          input_type = "awesomeRadio",
          choices_vector = c("Follow-up in spine clinic in 2wks.", 
                             "Follow-up in spine clinic in 3wks.", 
                             "Follow-up in spine clinic in 6wks.",
                             "Follow-up in spine clinic in ***wks.",
                             "Follow-up in spine clinic on ***.",
                             "Follow-up in spine trauma clinic in 2wks.", 
                             "Follow-up in ***", 
                             "Plan to return to the OR on ***"),
          initial_value_selected = postop_followup
        )
      )
    )
  }

### postop plan sections
postop_plan_sections_list <- list("Postop Destination",
                                  "Postop Abx",
                                  "Postop MAP goals", 
                                  "Postop Anemia", 
                                  "Postop Imaging", 
                                  "Pain Control", 
                                  "Activity", 
                                  "Bracing", 
                                  "Diet/GI", 
                                  "Foley",
                                  "DVT PPX/Anticoag/Antiplatelet",
                                  "Drains & Dressing",
                                  "Follow-up")


