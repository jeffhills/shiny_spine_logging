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
           redcap_token_last_8_input = "",
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
            4,
            awesomeRadio(
              inputId = "sex",
              label = "Sex:",
              choices = c("Male", "Female"),
              selected = starting_sex,
              inline = TRUE
            )
          ),
          column(
            4,
            dateInput(
              inputId = "date_of_birth",
              label = "Date of Birth (mm-dd-yyyy):",
              value = starting_dob,
              format = "mm-dd-yyyy",
              max = Sys.Date()
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
          )
        ),
        fluidRow(
          column(
            6,
            textInput(inputId = "redcap_token_last_8", label = "Redcap Token ID:", placeholder = "########"),
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
           stage_number_value = 1,
           staged_procedure_initial_value = FALSE,
           multiple_approach_initial_value = FALSE,
           spinal_regions_selected = c("Lumbar"),
           primary_or_revision = "Primary",
           levels_with_prior_decompression = "",
           prior_fusion_levels = "",
           prior_instrumentation = FALSE,
           left_prior_implants = "",
           left_prior_implants_removed = "",
           right_prior_implants = "",
           right_prior_implants_removed = "",
           left_rod_status = "retained_connected",
           left_implants_still_connected = "",
           right_rod_status = "retained_connected",
           right_implants_still_connected = ""
  ) {
    modalDialog(
      size = "l",
      easyClose = FALSE,
      footer = modalButton("Proceed"),
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
                  choices = jh_filter_icd_codes_generate_vector_function(section_input = "msk", spine_region_input = "lumbar"), #spine_icd_list_by_region$Lumbar$Degenerative,
                  options = pickerOptions(
                    liveSearch = TRUE,
                    virtualScroll = 50,
                    liveSearchNormalize = TRUE
                  ),
                  multiple = TRUE,
                  selected = ""
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
                  textInput(inputId = "symptoms_other", label = "Other:")
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
              conditionalPanel(
                "input.primary_revision.indexOf('Revision') > -1",
                jh_make_shiny_table_row_function(
                  left_column_label = "Select Levels with Prior Decompression:",
                  input_type = "picker",
                  input_id = "open_canal",
                  initial_value_selected = levels_with_prior_decompression,
                  left_column_percent_width = 60,
                  font_size = 14,
                  choices_vector = open_canal_df$level
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
                conditionalPanel(
                  condition = "input.prior_instrumentation == true",
                  box(
                    title = tags$div(style = "font-size:22px; font-weight:bold; text-align:center", "Prior Instrumentation"),
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
                            "Retained & Connected" = "retained_connected",
                            "Partially Retained & Connected" = "partially_retained_connected"
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
                        condition = "input.left_revision_rod_status.indexOf('partially_retained_connected') > -1",
                        pickerInput(
                          inputId = "left_revision_implants_connected_to_prior_rod",
                          label = "Select screws connected to the old rod:",
                          choices = c(""),
                          selected = left_implants_still_connected,
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
                            "Retained & Connected" = "retained_connected",
                            "Partially Retained & Connected" = "partially_retained_connected"
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
                        condition = "input.right_revision_rod_status.indexOf('partially_retained_connected') > -1",
                        pickerInput(
                          inputId = "right_revision_implants_connected_to_prior_rod",
                          label = "Select screws connected to the old rod:",
                          choices = c(""),
                          selected = right_implants_still_connected,
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
           fade_appearance = TRUE,
           primary_surgeon_first_name_input = "",
           primary_surgeon_last_name_input = "",
           surgical_assistants = "",
           preoperative_diagnosis = " ",
           postoperative_diagnosis = " ",
           indications = "",
           asa_class = "",
           anesthesia = "",
           neuromonitoring = c("SSEP", "tc MEP"),
           preop_antibiotics = c("Cefazolin (Ancef)"),
           anti_fibrinolytic = "",
           txa_loading = 20,
           txa_maintenance = 5) {
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
    
    additional_details_modal <-
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
                             tags$td(width = paste0(30, "%"), tags$div(style = "font-size:16px; font-weight:bold; text-align:left; margin-top:auto; margin-bottom:auto", "Primary Surgeon:")),
                             tags$td(width = paste0(70/2, "%"), textInput(inputId = "primary_surgeon_first_name", label = NULL, value = primary_surgeon_first_name_input, placeholder = "First Name", width = "80%")),
                             tags$td(width = paste0(70/2, "%"), textInput(inputId = "primary_surgeon_last_name", label = NULL, value = primary_surgeon_last_name_input, placeholder = "Last Name", width = "80%"))
                     ) 
          ),
          jh_make_shiny_table_row_function(
            left_column_percent_width = 30,
            left_column_label = "Assistants:",
            font_size = row_label_font_size,
            input_type = "text",
            input_id = "surgical_assistants",
            initial_value_selected = surgical_assistants
          ),
          jh_make_shiny_table_row_function(
            left_column_percent_width = 30,
            left_column_label = "Preoperative Diagnosis:",
            font_size = row_label_font_size,
            input_type = "text",
            input_id = "preoperative_diagnosis",
            initial_value_selected = preoperative_diagnosis
          ),
          jh_make_shiny_table_row_function(
            left_column_percent_width = 30,
            left_column_label = "Postoperative Diagnosis:",
            font_size = row_label_font_size,
            input_type = "text",
            input_id = "postoperative_diagnosis",
            initial_value_selected = postoperative_diagnosis
          ),
          jh_make_shiny_table_row_function(
            left_column_percent_width = 30,
            left_column_label = "Surgical Indications:",
            font_size = row_label_font_size,
            input_type = "textAreaInput",
            input_id = "indications",
            initial_value_selected = indications
          ),
          hr(),
          jh_make_shiny_table_row_function(
            left_column_percent_width = 30,
            left_column_label = "Preprocedure ASA Classification",
            font_size = row_label_font_size,
            input_type = "awesomeRadio",
            choices_vector = c("ASA I", "ASA II", "ASA III", "ASA IV", "ASA V", "ASA VI", "Emergent Surgery"),
            input_id = "asa_class",
            checkboxes_inline = TRUE,
            initial_value_selected = asa_class
          ),
          jh_make_shiny_table_row_function(
            left_column_percent_width = 30,
            left_column_label = "Anethesia Type:",
            font_size = row_label_font_size,
            input_type = "checkbox",
            choices_vector = c(
              "General Endotracheal Anesthseia",
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
            left_column_percent_width = 30,
            left_column_label = "Neuromonitoring used:",
            font_size = row_label_font_size,
            input_type = "checkbox",
            input_id = "neuromonitoring",
            choices_vector = c("EMG", "SSEP", "tc MEP", "DNEP (Cord Stimulation)", "H reflex", "None"),
            checkboxes_inline = TRUE,
            initial_value_selected = neuromonitoring
          ),
          hr(),
          jh_make_shiny_table_row_function(
            left_column_label = "Preop Antibiotics:",
            input_type = "checkbox",
            input_id = "preop_antibiotics",
            left_column_percent_width = 30,
            font_size = row_label_font_size,
            choices_vector = c(
              "None (Antibiotics were held)",
              "Cefazolin (Ancef)",
              "Vancomycin",
              "Ceftriaxone",
              "Gentamycin",
              "Clindamycin",
              "Aztreonam",
              "Unknown",
              "Other"
            ),
            initial_value_selected = preop_antibiotics
          ),
          hr(),
          jh_make_shiny_table_row_function(
            left_column_label = "Antifibrinolytic:",
            input_type = "checkbox",
            input_id = "anti_fibrinolytic",
            left_column_percent_width = 30,
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
          )
        )
      )
    
    return(additional_details_modal)
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
           head_positioning = "",
           additional_procedures_choices = c(""),
           additional_procedures = NULL,
           additional_procedures_other = "",
           additional_end_procedure_details = NULL,
           closure_details = NULL,
           dressing_details = NULL, 
           postop_dispo = c(""),
           postop_abx = c(""),
           postop_imaging = c(""),
           postop_pain = c(""),
           postop_activity = c(""),
           postop_brace = c(""),
           postop_diet = c(""),
           postop_dvt_ppx = c(""),
           postop_drains_dressing = c(""),
           postop_followup = c("")) {
    additional_details_modal <-
      modalDialog(
        size = "l",
        easyClose = FALSE,
        fade = fade_appearance,
        footer = actionBttn(
          inputId = "additional_surgical_details_complete",
          label = "Continue",
          icon = icon("fas fa-check-square"), 
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
          if(procedure_approach == "anterior"){
            jh_make_shiny_table_row_function(
              left_column_percent_width = 20,
              left_column_label = "Head Positioning:",
              font_size = row_label_font_size,
              input_type = "radioGroupButtons",
              input_id = "head_positioning",
              required_option = TRUE,
              button_size = "xs",
              checkboxes_inline = TRUE,
              choices_vector = c(
                "Supine/Lateral",
                "Cranial Tongs",
                "Halo",
                "Mayfield"
              ),
              initial_value_selected = head_positioning
            )
          },
          if(procedure_approach == "posterior"){
            jh_make_shiny_table_row_function(
              left_column_percent_width = 20,
              left_column_label = "Head Positioning:",
              font_size = row_label_font_size,
              input_type = "radioGroupButtons",
              input_id = "head_positioning",
              required_option = TRUE,
              button_size = "xs",
              checkboxes_inline = TRUE,
              choices_vector = c(
                "Proneview Faceplate",
                "Cranial Tongs",
                "Halo",
                "Mayfield"
              ),
              initial_value_selected = head_positioning
            )
            },
          if(procedure_approach == "combined"){
            jh_make_shiny_table_row_function(
              left_column_percent_width = 20,
              left_column_label = "Head Positioning:",
              font_size = row_label_font_size,
              input_type = "radioGroupButtons",
              input_id = "head_positioning",
              required_option = TRUE,
              button_size = "xs",
              checkboxes_inline = TRUE,
              choices_vector = c(
                "Supine/Lateral",
                "Proneview Faceplate",
                "Cranial Tongs",
                "Halo",
                "Mayfield"
              ),
              initial_value_selected = head_positioning
            )
          },
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
                                           input_type = "awesomeRadio", 
                                           # input_type = "radioGroupButtons", 
                                           input_id = "intraoperative_complications_yes_no", 
                                           initial_value_selected = intraoperative_complications_yes_no, 
                                           choices_vector = c("No", "Yes"), 
                                           status_color = "danger",
                                           justified_radio_buttons = TRUE, 
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
                           "Loss of Neuromonitoring Data without Return"
                         ),
                         initial_value_selected = intraoperative_complications_vector
                       ),
                       jh_make_shiny_table_row_function(
                         left_column_percent_width = 40,
                         left_column_label = "Other Intraoperative Complications:",
                         font_size = row_label_font_size,
                         input_type = "text",
                         input_id = "other_intraoperative_complications",
                         initial_value_selected = other_intraoperative_complications
                       )
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
                           choices_vector = c("Burr", "Kerrison", "Rongeur", "Electrocautery", "Other"),
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
                             "Other"
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
          div(style = "font-size:20px; font-weight:bold; text-align:center", "End of Procedure & Closure Details:"),
          if(procedure_approach == "anterior" | procedure_approach == "combined"){
            jh_make_shiny_table_row_function(left_column_label = "Anterior Deep drains:", 
                                                              input_type = "awesomeRadio",
                                                              input_id = "deep_drains_anterior", 
                                                              left_column_percent_width = 45, 
                                                              font_size = 16, 
                                                              initial_value_selected = 0, 
                                                              choices_vector = c("0", "1", "2", "3", "4", "5"), 
                                                              checkboxes_inline = TRUE, return_as_full_table = TRUE)
            },
          if(procedure_approach == "anterior" | procedure_approach == "combined"){
            jh_make_shiny_table_row_function(left_column_label = "Anterior Superficial drains:", 
                                                                     input_type = "awesomeRadio",
                                                                     input_id = "superficial_drains_anterior", 
                                                                     left_column_percent_width = 45, 
                                                                     font_size = 16, 
                                                                     initial_value_selected = 0, 
                                                                     choices_vector = c("0", "1", "2", "3", "4", "5"), 
                                                                     checkboxes_inline = TRUE, return_as_full_table = TRUE)
          },
          if(procedure_approach == "posterior" | procedure_approach == "combined"){
            jh_make_shiny_table_row_function(left_column_label = "Posterior Deep drains:", 
                                                               input_type = "awesomeRadio",
                                                               input_id = "deep_drains_posterior", 
                                                               left_column_percent_width = 45, 
                                                               font_size = 16, 
                                                               initial_value_selected = 1, 
                                                               choices_vector = c("0", "1", "2", "3", "4", "5"), 
                                                               checkboxes_inline = TRUE, return_as_full_table = TRUE)
            },
          if(procedure_approach == "posterior" | procedure_approach == "combined"){
            jh_make_shiny_table_row_function(left_column_label = "Posterior Superficial drains:", 
                                                                      input_type = "awesomeRadio",
                                                                      input_id = "superficial_drains_posterior", 
                                                                      left_column_percent_width = 45, 
                                                                      font_size = 16, 
                                                                      initial_value_selected = 1, 
                                                                      choices_vector = c("0", "1", "2", "3", "4", "5"), 
                                                                      checkboxes_inline = TRUE, return_as_full_table = TRUE)
            },
          # uiOutput(outputId = "drains_ui"),
          hr(),
          jh_make_shiny_table_row_function(
            left_column_label = "Select any used during closure:",
            input_type = "checkbox",
            input_id = "additional_end_procedure_details",
            left_column_percent_width = 45,
            font_size = row_label_font_size,
            choices_vector = c("Vancomycin Powder",
                               "Antibiotic Beads"),
            initial_value_selected = additional_end_procedure_details,
            return_as_full_table = TRUE
          ),
          hr(),
          jh_make_shiny_table_row_function(
            left_column_label = "Skin Closure:",
            input_type = "checkbox",
            input_id = "closure_details",
            left_column_percent_width = 45,
            font_size = row_label_font_size,
            required_option = TRUE,
            choices_vector = c("Subcutaneous suture",
                               "Nylon",
                               "Staples", 
                               "left open"),
            initial_value_selected = closure_details,
            return_as_full_table = TRUE
          ),
          hr(),
          jh_make_shiny_table_row_function(
            left_column_label = "Skin/Dressing:",
            input_type = "checkbox",
            input_id = "dressing_details",
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
            initial_value_selected = dressing_details,
            return_as_full_table = TRUE
          ),
          br(),
          h2("Confirm any additional Procedures Performed:"),
          jh_make_shiny_table_row_function(
            left_column_label = "Additional Procedures:",
            font_size = row_label_font_size,
            input_id = "additional_procedures",
            left_column_percent_width = 20,
            checkboxes_inline = FALSE,
            input_type = "checkbox",
            choices_vector = additional_procedures_choices,
            initial_value_selected = additional_procedures
          ),
          conditionalPanel(
            condition = "input.additional_procedures.indexOf('Other') > -1",
            tags$table(
              width = "90%" ,
              jh_make_shiny_table_row_function(
                left_column_label = "Other Procedures:",
                font_size = row_label_font_size - 1,
                input_type = "text",
                input_id = "additional_procedures_other",
                left_column_percent_width = 30,
                initial_value_selected = additional_procedures_other,
              )
            )
          ),
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
                               "Ancef until drains removed",
                               "***"),
            initial_value_selected = postop_abx
          ),
          br(),
          jh_make_shiny_table_row_function(
            left_column_label = "Postop Imaging:",
            font_size = row_label_font_size,
            input_id = "postop_abx",
            left_column_percent_width = 20,
            checkboxes_inline = FALSE,
            input_type = "checkbox",
            choices_vector = c("please obtain upright AP/Lateral C-spine xrays prior to discharge when able to stand & after drains out",
                               "please obtain upright AP/Lateral T-spine xrays prior to discharge when able to stand & after drains out",
                               "please obtain upright AP/Lateral L-spine xrays prior to discharge when able to stand & after drains out",
                               "please obtain upright AP/Lateral standing scoliosis xrays prior to discharge when able to stand & after drains out",
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
            choices_vector = c("PCA with plan to dc on POD 1 (in addition to all other pain medication orders)", 
                               "Oxycodone 5mg q4h, ok to give additional dose if needed", 
                               "Oxycodone 10mg q4h, ok to give additional dose if needed",
                               "Tylenol 1000mg q8h scheduled.", 
                               "Tylenol 650mg q8h scheduled",
                               "Gabapentin 100mg q8h scheduled",
                               "Gabapentin 300mg q8h scheduled",
                               "Toradol 15mg q8h for 48-72hrs",
                               "Meloxicam 15mg daily",
                               "Tizanidine 4mg q6h prn for muscle spasms (avoid in geriatric males/males w/ urinary retention)",
                               "Cyclobenzabrine 5mg q8h prn for muscle spasms (strong anticholinergic, minimize in geriatric)",
                               "Baclofen 10mg TID prn for muscle spasms",
                               "Diazepam 2.5mg q8h prn for muscle spasms",
                               "Pain team consult",
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
            choices_vector = c("PT/OT",
                               "please mobilize out of bed minimum 3x daily", 
                               "No bending, twisting, lifting > 10lbs",
                               "***"),
            initial_value_selected = c("PT/OT",
                                       "please mobilize out of bed minimum 3x daily", 
                                       "No bending, twisting, lifting > 10lbs")
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
                               "TLSO brace while ambulating (OK to remove while in bed, sitting, or bathing",
                               "No brace needed"),
            initial_value_selected = postop_brace
          ),
          br(),
          jh_make_shiny_table_row_function(
            left_column_label = "Diet:",
            font_size = row_label_font_size,
            input_id = "postop_diet",
            left_column_percent_width = 20,
            checkboxes_inline = FALSE,
            input_type = "checkbox",
            choices_vector = c("OK to resume normal diet", 
                               "Start with clear liquid diet and advance as tolerated", 
                               "NPO until return of bowel sounds",
                               "Senokit-S daily + prn suppository daily if no BM in 24hrs + administer enema if no BM within 24hrs of suppository"),
            initial_value_selected = postop_diet
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
                               "Hold any chemical dvt ppx for minimum 72hrs",
                               "Please do not start any chemical dvt ppx without discussion with spine team",
                               "Please hold any anti-platelet/anti-coag for minimum 7days after surgery",
                               "***"),
            initial_value_selected = c("SCD's while in bed", "Hold any chemical dvt ppx for minimum 72hrs")
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
                               "Please keep a watertight dressing in place over wound and reinforce as needed with tegaderms",
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
                               "Follow-up in spine clinic in 6wks."),
            initial_value_selected = postop_followup
          ),
        )
      )
    
    return(additional_details_modal)
  }

# ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   ADDITIONAL SURGICAL DETAILS MODAL  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
# addition_surgical_details_modal_box_function <-
#   function(required_options_missing = FALSE,
#            row_label_font_size = 16,
#            fade_appearance = TRUE,
#            primary_surgeon_first_name_input = "",
#            primary_surgeon_last_name_input = "",
#            surgical_assistants = "",
#            preoperative_diagnosis = " ",
#            postoperative_diagnosis = " ",
#            indications = "",
#            asa_class = "",
#            anesthesia = "",
#            neuromonitoring = c("SSEP", "tc MEP"),
#            preop_antibiotics = c("Cefazolin (Ancef)"),
#            anti_fibrinolytic = "",
#            txa_loading = 20,
#            txa_maintenance = 5,
#            surgical_findings = "",
#            specimens_removed = "",
#            ebl = NULL,
#            urine_output = NULL,
#            crystalloids_administered = NULL,
#            colloids_administered = NULL,
#            transfusion = FALSE,
#            cell_saver_transfused = 0,
#            prbc_transfused = 0,
#            ffp_transfused = 0,
#            cryoprecipitate_transfused = 0,
#            platelets_transfused = 0,
#            intraoperative_complications_true_false =
#              FALSE,
#            intraoperative_complications_vector = NULL,
#            other_intraoperative_complications = NULL,
#            durotomy_timing_input = "",
#            durotomy_instrument_input = "",
#            durotomy_repair_method_input = "",
#            head_positioning = "",
#            additional_procedures_choices = c(" "),
#            additional_procedures = NULL,
#            additional_procedures_other = "",
#            additional_end_procedure_details = NULL,
#            closure_details = NULL,
#            dressing_details = NULL) {
#     additional_details_modal <-
#       modalDialog(
#         size = "l",
#         easyClose = FALSE,
#         fade = fade_appearance,
#         footer = actionBttn(
#           inputId = "additional_surgical_details_complete",
#           label = "Continue",
#           icon = icon("fas fa-check-square"), 
#           style = "simple",
#           color = "success"
#         ),
#         box(
#           width = 12,
#           title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Additional Surgical Details:"),
#           status = "info",
#           solidHeader = TRUE,
#           if (required_options_missing == TRUE) {
#             div(style = "font-size:22px; font-weight:bold; font-style:italic; text-align:center; color:red", "*** Please Make Selections for Required Fields***")
#           },
#           tags$table(width = "100%",
#                      tags$tr(width = "100%",
#                              tags$td(width = paste0(30, "%"), tags$div(style = "font-size:16px; font-weight:bold; text-align:left; margin-top:auto; margin-bottom:auto", "Primary Surgeon:")),
#                              tags$td(width = paste0(70/2, "%"), textInput(inputId = "primary_surgeon_first_name", label = NULL, value = primary_surgeon_first_name_input, placeholder = "First Name", width = "80%")),
#                              tags$td(width = paste0(70/2, "%"), textInput(inputId = "primary_surgeon_last_name", label = NULL, value = primary_surgeon_last_name_input, placeholder = "Last Name", width = "80%"))
#                      ) 
#                      ),
#           jh_make_shiny_table_row_function(
#             left_column_percent_width = 30,
#             left_column_label = "Assistants:",
#             font_size = row_label_font_size,
#             input_type = "text",
#             input_id = "surgical_assistants",
#             initial_value_selected = surgical_assistants
#           ),
#           jh_make_shiny_table_row_function(
#             left_column_percent_width = 30,
#             left_column_label = "Preoperative Diagnosis:",
#             font_size = row_label_font_size,
#             input_type = "text",
#             input_id = "preoperative_diagnosis",
#             initial_value_selected = preoperative_diagnosis
#           ),
#           jh_make_shiny_table_row_function(
#             left_column_percent_width = 30,
#             left_column_label = "Postoperative Diagnosis:",
#             font_size = row_label_font_size,
#             input_type = "text",
#             input_id = "postoperative_diagnosis",
#             initial_value_selected = postoperative_diagnosis
#           ),
#           jh_make_shiny_table_row_function(
#             left_column_percent_width = 30,
#             left_column_label = "Surgical Indications:",
#             font_size = row_label_font_size,
#             input_type = "text",
#             input_id = "indications",
#             initial_value_selected = indications
#           ),
#           hr(),
#           jh_make_shiny_table_row_function(
#             left_column_percent_width = 30,
#             left_column_label = "Preprocedure ASA Classification",
#             font_size = row_label_font_size,
#             input_type = "awesomeRadio",
#             choices_vector = c("ASA I", "ASA II", "ASA III", "ASA IV", "ASA V", "ASA VI", "Emergent Surgery"),
#             input_id = "asa_class",
#             checkboxes_inline = TRUE,
#             initial_value_selected = asa_class
#           ),
#           jh_make_shiny_table_row_function(
#             left_column_percent_width = 30,
#             left_column_label = "Anethesia Type:",
#             font_size = row_label_font_size,
#             input_type = "checkbox",
#             choices_vector = c(
#               "General Endotracheal Anesthseia",
#               "Spinal Anesthesia",
#               "Epidural Anesthesia",
#               "Monitored Anesthesia Care (MAC)"
#             ),
#             input_id = "anesthesia",
#             checkboxes_inline = TRUE,
#             initial_value_selected = anesthesia
#           ),
#           hr(),
#           jh_make_shiny_table_row_function(
#             left_column_percent_width = 30,
#             left_column_label = "Neuromonitoring used:",
#             font_size = row_label_font_size,
#             input_type = "checkbox",
#             input_id = "neuromonitoring",
#             choices_vector = c("EMG", "SSEP", "tc MEP", "DNEP (Cord Stimulation)", "H reflex"),
#             checkboxes_inline = TRUE,
#             initial_value_selected = neuromonitoring
#           ),
#           hr(),
#           jh_make_shiny_table_row_function(
#             left_column_label = "Preop Antibiotics:",
#             input_type = "checkbox",
#             input_id = "preop_antibiotics",
#             left_column_percent_width = 30,
#             font_size = row_label_font_size,
#             choices_vector = c(
#               "None (Antibiotics were held)",
#               "Cefazolin (Ancef)",
#               "Vancomycin",
#               "Ceftriaxone",
#               "Gentamycin",
#               "Clindamycin",
#               "Aztreonam",
#               "Unknown",
#               "Other"
#             ),
#             initial_value_selected = preop_antibiotics
#           ),
#           hr(),
#           jh_make_shiny_table_row_function(
#             left_column_label = "Antifibrinolytic:",
#             input_type = "checkbox",
#             input_id = "anti_fibrinolytic",
#             left_column_percent_width = 30,
#             font_size = row_label_font_size,
#             choices_vector = c(
#               "None",
#               "Tranexamic Acid (TXA)",
#               "Amicar",
#               "Desmopressin (DDAVP)",
#               "Other"
#             ),
#             initial_value_selected = anti_fibrinolytic,
#           ),
#           conditionalPanel(
#             condition = "input.anti_fibrinolytic.indexOf('Tranexamic Acid (TXA)') > -1",
#             jh_make_shiny_table_row_function(
#               left_column_label = "TXA Loading (mg/kg):    ",
#               input_type = "numeric",
#               input_id = "txa_loading",
#               left_column_percent_width = 50,
#               font_size = row_label_font_size -
#                 1,
#               min = 0,
#               max = 200,
#               initial_value_selected = txa_loading,
#               step = 5,
#               text_align = "right",
#             ),
#             jh_make_shiny_table_row_function(
#               left_column_label = "TXA Maintenance (mg/kg/hr):    ",
#               input_type = "numeric",
#               input_id = "txa_maintenance",
#               left_column_percent_width = 50,
#               font_size = row_label_font_size -
#                 1,
#               min = 0,
#               max = 50,
#               initial_value_selected = txa_maintenance,
#               step = 5,
#               text_align = "right",
#             ),
#           ),
#           jh_make_shiny_table_row_function(
#             left_column_percent_width = 30,
#             left_column_label = "Findings:",
#             font_size = row_label_font_size,
#             input_type = "text",
#             input_id = "surgical_findings",
#             initial_value_selected = surgical_findings
#           ),
#           jh_make_shiny_table_row_function(
#             left_column_percent_width = 30,
#             left_column_label = "Specimens:",
#             font_size = row_label_font_size,
#             input_type = "text",
#             input_id = "specimens_removed",
#             initial_value_selected = specimens_removed
#           ),
#           hr(),
#           jh_make_shiny_table_row_function(
#             left_column_percent_width = 30,
#             left_column_label = "Estimated Blood Loss:",
#             font_size = row_label_font_size,
#             input_type = "numeric",
#             input_id = "ebl",
#             initial_value_selected = ebl,
#             min = 0,
#             max = 50000,
#             step = 100
#           ),
#           jh_make_shiny_table_row_function(
#             left_column_percent_width = 30,
#             left_column_label = "Urine Output:",
#             font_size = row_label_font_size,
#             input_type = "numeric",
#             input_id = "urine_output",
#             initial_value_selected = urine_output,
#             min = 0,
#             max = 50000,
#             step = 100
#           ),
#           jh_make_shiny_table_row_function(
#             left_column_percent_width = 30,
#             left_column_label = "Crystalloids:",
#             font_size = row_label_font_size,
#             input_type = "numeric",
#             input_id = "crystalloids_administered",
#             initial_value_selected = crystalloids_administered,
#             min = 0,
#             max = 100000,
#             step = 100
#           ),
#           jh_make_shiny_table_row_function(
#             left_column_percent_width = 30,
#             left_column_label = "Colloids:",
#             font_size = row_label_font_size,
#             input_type = "numeric",
#             input_id = "colloids_administered",
#             min = 0,
#             initial_value_selected = colloids_administered,
#             max = 100000,
#             step = 100
#           ),
#           jh_make_shiny_table_row_function(
#             left_column_percent_width = 60,
#             left_column_label = "Transfusions/Cell Saver",
#             font_size = row_label_font_size,
#             input_type = "switch",
#             input_id = "transfusion",
#             switch_input_on_label = "Yes",
#             switch_input_off_label = "No",
#             initial_value_selected = transfusion
#           ),
#           conditionalPanel(
#             condition = "input.transfusion == true",
#             box(
#               width = 12,
#               jh_make_shiny_table_row_function(
#                 left_column_percent_width = 60,
#                 left_column_label = "Cell Saver Transfused (cc):",
#                 font_size = row_label_font_size,
#                 input_type = "numeric",
#                 input_id = "cell_saver_transfused",
#                 initial_value_selected = cell_saver_transfused,
#                 min = 0,
#                 max = 10000,
#                 step = 100
#               ),
#               jh_make_shiny_table_row_function(
#                 left_column_percent_width = 60,
#                 left_column_label = "pRBC units transfused:",
#                 font_size = row_label_font_size,
#                 input_type = "numeric",
#                 input_id = "prbc_transfused",
#                 initial_value_selected = prbc_transfused,
#                 min = 0,
#                 max = 100,
#                 step = 1
#               ),
#               jh_make_shiny_table_row_function(
#                 left_column_percent_width = 60,
#                 left_column_label = "FFP units transfused:",
#                 font_size = row_label_font_size,
#                 input_type = "numeric",
#                 input_id = "ffp_transfused",
#                 initial_value_selected = ffp_transfused,
#                 min = 0,
#                 max = 100,
#                 step = 1
#               ),
#               jh_make_shiny_table_row_function(
#                 left_column_percent_width = 60,
#                 left_column_label = "Cryoprecipitate units transfused:",
#                 font_size = row_label_font_size,
#                 input_type = "numeric",
#                 input_id = "cryoprecipitate_transfused",
#                 initial_value_selected = cryoprecipitate_transfused,
#                 min = 0,
#                 max = 100,
#                 step = 1
#               ),
#               jh_make_shiny_table_row_function(
#                 left_column_percent_width = 60,
#                 left_column_label = "Platelet units transfused:",
#                 font_size = row_label_font_size,
#                 input_type = "numeric",
#                 input_id = "platelets_transfused",
#                 initial_value_selected = platelets_transfused,
#                 min = 0,
#                 max = 100,
#                 step = 1
#               ),
#             )
#           ),
#           hr(),
#           jh_make_shiny_table_row_function(
#             left_column_percent_width = 60,
#             left_column_label = "Intraoperative Complications (including durotomy)?",
#             font_size = row_label_font_size,
#             input_type = "switch",
#             input_id = "intraoperative_complications_true_false",
#             initial_value_selected = intraoperative_complications_true_false
#           ),
#           conditionalPanel(
#             condition = "input.intraoperative_complications_true_false == true",
#             box(
#               width = 12,
#               br(),
#               jh_make_shiny_table_row_function(
#                 left_column_percent_width = 40,
#                 left_column_label = "Select any:",
#                 font_size = row_label_font_size,
#                 input_type = "checkbox",
#                 input_id = "intraoperative_complications_vector",
#                 choices_vector = c(
#                   "Durotomy",
#                   "Nerve Root Injury",
#                   "Loss of Neuromonitoring Data with Return",
#                   "Loss of Neuromonitoring Data without Return"
#                 ),
#                 initial_value_selected = intraoperative_complications_vector
#               ),
#               jh_make_shiny_table_row_function(
#                 left_column_percent_width = 40,
#                 left_column_label = "Other Intraoperative Complications:",
#                 font_size = row_label_font_size,
#                 input_type = "text",
#                 input_id = "other_intraoperative_complications",
#                 initial_value_selected = other_intraoperative_complications
#               )
#             ),
#             conditionalPanel(
#               condition = "input.intraoperative_complications_vector.indexOf('Durotomy') > -1",
#               box(
#                 width = 12,
#                 title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Durotomy Details:"),
#                 status = "info",
#                 solidHeader = TRUE,
#                 jh_make_shiny_table_row_function(
#                   left_column_label = "Durotomy Occurred During:",
#                   input_type = "awesomeRadio",
#                   input_id = "durotomy_timing",
#                   left_column_percent_width = 45,
#                   font_size = 18,
#                   choices_vector = c("Exposure", "Decompression", "Other"),
#                   initial_value_selected = durotomy_timing_input,
#                   checkboxes_inline = FALSE,
#                   return_as_full_table = TRUE
#                 ),
#                 br(),
#                 jh_make_shiny_table_row_function(
#                   left_column_label = "What Instrument was Involved?",
#                   input_type = "awesomeRadio",
#                   input_id = "durotomy_instrument",
#                   left_column_percent_width = 45,
#                   font_size = 18,
#                   choices_vector = c("Burr", "Kerrison", "Rongeur", "Electrocautery", "Other"),
#                   initial_value_selected = durotomy_instrument_input,
#                   checkboxes_inline = FALSE,
#                   return_as_full_table = TRUE
#                 ),
#                 jh_make_shiny_table_row_function(
#                   left_column_label = "The dura was repaired using:",
#                   input_type = "checkbox",
#                   input_id = "durotomy_repair_method",
#                   left_column_percent_width = 45,
#                   font_size = 18,
#                   choices_vector = c(
#                     "Primarily Repaired using Suture",
#                     "Dural Sealant",
#                     "Tachosil",
#                     "Dural Graft",
#                     "No Repair Performed",
#                     "Other"
#                   ),
#                   initial_value_selected = durotomy_repair_method_input,
#                   checkboxes_inline = FALSE,
#                   return_as_full_table = TRUE
#                 )
#               )
#             )
#           ),
#           br(),
#           jh_make_shiny_table_row_function(
#             left_column_percent_width = 20,
#             left_column_label = "Head Positioning:",
#             font_size = row_label_font_size,
#             input_type = "radioGroupButtons",
#             input_id = "head_positioning",
#             required_option = TRUE,
#             button_size = "xs",
#             checkboxes_inline = TRUE,
#             choices_vector = c(
#               "Supine/Lateral",
#               "Proneview Faceplate",
#               "Cranial Tongs",
#               "Halo",
#               "Mayfield"
#             ),
#             initial_value_selected = head_positioning
#           ),
#           br(),
#           jh_make_shiny_table_row_function(
#             left_column_label = "Additional Procedures:",
#             font_size = row_label_font_size,
#             input_id = "additional_procedures",
#             left_column_percent_width = 20,
#             checkboxes_inline = FALSE,
#             input_type = "checkbox",
#             choices_vector = additional_procedures_choices,
#             initial_value_selected = additional_procedures
#           ),
#           conditionalPanel(
#             condition = "input.additional_procedures.indexOf('Other') > -1",
#             tags$table(
#               width = "90%" ,
#               jh_make_shiny_table_row_function(
#                 left_column_label = "Other Procedures:",
#                 font_size = row_label_font_size - 1,
#                 input_type = "text",
#                 input_id = "additional_procedures_other",
#                 left_column_percent_width = 30,
#                 initial_value_selected = additional_procedures_other,
#               )
#             )
#           ),
#           br(),
#           hr(),
#           div(style = "font-size:20px; font-weight:bold; text-align:center", "End of Procedure & Closure Details:"),
#           uiOutput(outputId = "drains_ui"),
#           hr(),
#           jh_make_shiny_table_row_function(
#             left_column_label = "Select any used during closure:",
#             input_type = "checkbox",
#             input_id = "additional_end_procedure_details",
#             left_column_percent_width = 45,
#             font_size = row_label_font_size,
#             choices_vector = c("Vancomycin Powder",
#                                "Antibiotic Beads"),
#             initial_value_selected = additional_end_procedure_details,
#             return_as_full_table = TRUE
#           ),
#           hr(),
#           jh_make_shiny_table_row_function(
#             left_column_label = "Skin Closure:",
#             input_type = "checkbox",
#             input_id = "closure_details",
#             left_column_percent_width = 45,
#             font_size = row_label_font_size,
#             required_option = TRUE,
#             choices_vector = c("Subcutaneous suture",
#                                "Nylon",
#                                "Staples"),
#             initial_value_selected = closure_details,
#             return_as_full_table = TRUE
#           ),
#           hr(),
#           jh_make_shiny_table_row_function(
#             left_column_label = "Skin/Dressing:",
#             input_type = "checkbox",
#             input_id = "dressing_details",
#             required_option = TRUE,
#             left_column_percent_width = 45,
#             font_size = row_label_font_size,
#             choices_vector = c(
#               "Steristrips",
#               "Dermabond",
#               "Prineo",
#               "an Incisional Wound Vac",
#               "a water tight dressing"
#             ),
#             initial_value_selected = dressing_details,
#             return_as_full_table = TRUE
#           ),
#           br()
#         )
#       )
#     
#     return(additional_details_modal)
#   }
