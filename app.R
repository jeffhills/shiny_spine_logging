####################### SURGICAL PLANNING V2 ###########################

library(shiny)
library(sf)
library(tidyverse)
library(ggplot2)
library(shinyWidgets)
library(shinyBS)
library(cowplot)
library(magick)
library(ggpattern)
library(glue)
library(rlist)
library(janitor)
library(lubridate)
library(redcapAPI)
library(ggpmisc)
library(rclipboard)
library(nngeo)
library(shinydashboard)

rcon <- redcapConnection(url = 'https://redcap.wustl.edu/redcap/api/', token = "4B2AA28A4D6EFBC6CA1F49EC0FB385F7")

source("short_shiny_functions.R", local = TRUE)
source("build_spine_objects_functions.R", local = TRUE)
source("load_coordinates_build_objects.R", local = TRUE)
source("anterior_posterior_operative_note_generator_functions.R", local = TRUE)

source("load_coordinates_build_objects_6_lumbar.R", local = TRUE)

#Dashboards:
rclipboardSetup()

ui <- dashboardPage(skin = "black",
    dashboardHeader(title = "Spine Operative Logging and Automated Report Generation", titleWidth = 650,
                    dropdownMenuOutput(outputId = "upload_to_redcap_task")
                    
    ),
    dashboardSidebar(width = 350,
                     sidebarMenu(id = "tabs",
                         menuItem(text = "Patient Details & Surgical Procedures", 
                                  tabName = "patient_details_procedures", 
                                  icon = icon("screwdriver")
                         ),
                         menuItem(text = "Implant & Additional Procedural Details",
                                  tabName = "implant_details",
                                  icon = icon("ruler")
                         ),
                         menuItem(text = "Upload Data & Generate Operative Note",
                                  tabName = "operative_note",
                                  icon = icon("clipboard")
                         ),
                         br(),
                         menuItem(text = "Review Data Tables",
                                  tabName = "tables",
                                  icon = icon("table")
                         )
                     )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "patient_details_procedures",
                    column(width = 4, 
                           box(width = 12, title = tags$div(style = "font-size:22px; font-weight:bold", "Patient Details:"), solidHeader = TRUE, status = "info",collapsible = TRUE,
                               fluidRow(column(4, 
                                               textInput(inputId = "patient_last_name", label = "Last Name:")),
                                        column(4, 
                                               textInput(inputId = "patient_first_name", label = "First Name:")),
                                        column(4,
                                               dateInput(inputId = "date_of_birth", label = "Date of Birth:", format = "mm-dd-yyyy", startview = "decade", autoclose = TRUE, max = Sys.Date(), value = "1900-01-01"))
                               ),
                               awesomeRadio(
                                   inputId = "sex",
                                   label = "Sex:", 
                                   choices = c("Male", "Female"),
                                   selected = "Female", 
                                   inline = TRUE 
                               ),
                               fluidRow(column(width = 5,
                                               h5(strong("Diagnosis Category:"))
                               ),
                               column(width = 7,
                                      radioGroupButtons(
                                          inputId = "primary_diagnosis_group",
                                          label = NULL,
                                          size = "sm",
                                          choices = c("Spinal Condition (Degen)" = "spinal_condition",
                                                      "Deformity" = "deformity", 
                                                      "Neoplasm" = "neoplastm", 
                                                      "Lesion (trauma, infection)" = "lesion"),
                                          justified = TRUE,
                                          direction = "vertical",
                                          checkIcon = list(
                                              yes = tags$i(class = "fa fa-circle", 
                                                           style = "color: steelblue"))
                                      )
                               )
                               ),
                               fluidRow(
                                   column(width = 5, 
                                          h5(strong("Diagnosis Subgroup:")) 
                                   ),
                                   column(width = 7,
                                          pickerInput(inputId = "diagnosis_subgroup", label = NULL,
                                                      choices = c("Spinal Stenosis",
                                                                  "Myelopathy",
                                                                  "Foraminal Stenosis",
                                                                  "Degenerative Spondylolisthesis",
                                                                  "Pseudarthrosis",
                                                                  "Lumbar Disc Herniation",
                                                                  "Other"), 
                                                      multiple = TRUE
                                                      # options = list(`actions-box` = TRUE)
                                          ),
                                          conditionalPanel(condition = "input.diagnosis_subgroup.indexOf('Other') > -1",
                                                           textInput(inputId = "diagnosis_subgroup_other", label = "Other:"))
                                   )
                               ),
                               fluidRow(
                                   column(width = 5, 
                                          h5(strong("Symptoms:")) 
                                   ),
                                   column(width = 7,
                                          pickerInput(
                                              inputId = "symptoms",
                                              label = NULL,
                                              choices = list('Neck & Uppers:' = c("Neck Pain", "Left Arm Pain", "Right Arm Pain", "Left Arm Weakness", "Right Arm Weakness"),
                                                             'Thoracic:' = c("Mid Back Pain", "Kyphosis"),
                                                             "Lower & Legs:" = c("Low Back Pain", "Left Leg Pain", "Right Leg Pain", "Left Leg Weakness", "Right Leg Weakness"), 
                                                             "Functional:" = c("Myelopathy: Nurick 1 (Root Symptomts)",
                                                                               "Myelopathy: Nurick 2 (Normal gait but symptoms of cord compression)",
                                                                               "Myelopathy: Nurick 3 (Gait Abnormalities)",
                                                                               "Myelopathy: Nurick 4 (Significant Gait Abnormalities, preventing employment)",
                                                                               "Myelopathy: Nurick 5 (Depended on Assistive Device for Ambulating)",
                                                                               "Myelopathy: Nurick 6 (Wheelchair bound)"),
                                                             "Deformity" = c("Coronal deformity", "Sagittal Imbalance", "Chin on chest deformity", "Flatback Syndrome"),
                                                             'Other' = c("Loss of bladder control", "Bowel Incontinence", "Other Symptoms")
                                              ),
                                              multiple = TRUE, 
                                              width = "100%"
                                          )
                                   )
                               ),
                               fluidRow(
                                   column(width = 5, 
                                          h5(strong("Bone Density or HU:")) 
                                   ),
                                   column(width = 7,
                                          textInput(inputId = "bone_density", label = NULL, width = "100%", placeholder = "T-score or HU")
                                   )
                               ),
                               textInput(inputId = "relevant_history", label = "Other Comments/History:"),
                           ),
                           box(width = 12, title = tags$div(style = "font-size:22px; font-weight:bold", "Surgical Details:"), solidHeader = TRUE, status = "info",collapsible = TRUE, 
                               h3(strong("Surgical Details:")),
                               jh_make_shiny_table_row_function(left_column_label = "Date of Surgery:", input_type = "date", input_id = "date_of_surgery", left_column_percent_width = 50, font_size = 14), 
                               jh_make_shiny_table_row_function(left_column_label = "Staged Procedure?", input_type = "switch", input_id = "staged_procedure", left_column_percent_width = 50, font_size = 14, switch_input_on_label = "Yes", switch_input_off_label = "No"), 
                               conditionalPanel(condition = "input.staged_procedure == true",
                                                jh_make_shiny_table_row_function(left_column_label = "Stage Number:",
                                                                                 input_type = "awesomeRadio",
                                                                                 input_id = "stage_number", 
                                                                                 left_column_percent_width = 50, font_size = 14, choices_vector = c(1,2,3,4,5), checkboxes_inline = TRUE, 
                                                                                 initial_value_selected = 1)
                               ),
                               jh_make_shiny_table_row_function(left_column_label = "Multiple Approach, single stage?", input_type = "switch", input_id = "multiple_approach", left_column_percent_width = 50, font_size = 14, switch_input_on_label = "Yes", switch_input_off_label = "No"),
                               jh_make_shiny_table_row_function(left_column_label = "Primary or Revision:", input_type = "radioGroupButtons", input_id = "primary_revision", left_column_percent_width = 50, font_size = 14, choices_vector = c("Primary", "Revision"), initial_value_selected = "Primary",checkboxes_inline = TRUE), 
                               conditionalPanel("input.primary_revision.indexOf('Revision') > -1",
                                                jh_make_shiny_table_row_function(left_column_label = "Indication for Revision:",
                                                                                 input_type = "picker", 
                                                                                 input_id = "revision_indication", 
                                                                                 left_column_percent_width = 60, 
                                                                                 font_size = 14, 
                                                                                 choices_vector = list('Implant' = c("Mal-Positioned", "Failure (pull-out/fracture)", "Pseudarthrosis"), 
                                                                                                       'Junctional' = c("Proximal", "Distal", "Vertebral Fracture"),
                                                                                                       'Infection' = c("Early Infection/Wound Drainage", "Late Infection (>6mo)"),
                                                                                                       'Malalignment' = c("Coronal", "Sagittal"),
                                                                                                       'Neurologic' = c("Motor Deficit", "Radicular Pain", "Neurogenic Claudication")
                                                                                 ) 
                                                                                 ),
                                                jh_make_shiny_table_row_function(left_column_label = "Select Levels with Prior Decompression:",
                                                                                 input_type = "picker", 
                                                                                 input_id = "open_canal", 
                                                                                 left_column_percent_width = 60, 
                                                                                 font_size = 14, 
                                                                                 choices_vector = open_canal_df$level
                                                                                 ),
                                                jh_make_shiny_table_row_function(left_column_label = "Select Prior Fusion Levels:",
                                                                                 input_type = "picker", 
                                                                                 input_id = "open_canal", 
                                                                                 left_column_percent_width = 60, 
                                                                                 font_size = 14, 
                                                                                 choices_vector = unique(interbody_levels_df$level)
                                                ),
                                                jh_make_shiny_table_row_function(left_column_label = "Select Levels to remove instrumentation:",
                                                                                 input_type = "picker", 
                                                                                 input_id = "removal_instrumentation_levels", 
                                                                                 left_column_percent_width = 60, 
                                                                                 font_size = 14, 
                                                                                 choices_vector = unique(labels_df$level)
                                                ),
                                                fluidRow(
                                                    column(width = 6,
                                                           h5(strong("Left Implants:"))
                                                    ),
                                                    column(width = 6,
                                                           h5(strong("Right Implants:"))
                                                    )
                                                ),
                                                fluidRow(
                                                    column(width = 6,
                                                           pickerInput(
                                                               inputId = "left_revision_implants",
                                                               label = NULL,
                                                               choices = unique((revision_implants_df %>% filter(x < 0.5))$level),
                                                               multiple = TRUE,
                                                               width = "100%"
                                                           )
                                                    ),
                                                    column(width = 6,
                                                           pickerInput(
                                                               inputId = "right_revision_implants",
                                                               label = NULL,
                                                               choices = unique((revision_implants_df %>% filter(x > 0.5))$level),
                                                               multiple = TRUE,
                                                               width = "100%"
                                                           )
                                                    )
                                                )
                               ),
                                                tags$hr(),
                                                column(12, 
                                                       jh_make_shiny_table_row_function(left_column_label = "Preop Antibiotics:",
                                                                                        input_type = "picker", 
                                                                                        input_id = "preop_antibiotics", 
                                                                                        left_column_percent_width = 60, 
                                                                                        font_size = 14, 
                                                                                        choices_vector = c("None (Antibiotics were held)", "Ancef", "Vancomycin", "Gentamycin", "Clindamycin", "Unknown", "Other")
                                                       ),
                                                       jh_make_shiny_table_row_function(left_column_label = "Antifibrinolytic:",
                                                                                        input_type = "picker", 
                                                                                        input_id = "anti_fibrinolytic", 
                                                                                        left_column_percent_width = 60, 
                                                                                        font_size = 14, 
                                                                                        choices_vector = c("None", "Tranexamic Acid (TXA)", "Amicar", "Desmopressin (DDAVP)", "Other")
                                                       ),
                                                       conditionalPanel(condition = "input.anti_fibrinolytic.indexOf('Tranexamic Acid (TXA)') > -1",
                                                                        jh_make_shiny_table_row_function(left_column_label = "TXA Loading (mg/kg):    ",
                                                                                                         input_type = "numeric", 
                                                                                                         input_id = "txa_loading", 
                                                                                                         left_column_percent_width = 60, 
                                                                                                         font_size = 13, min = 0, max = 200, initial_value_selected = 20, step = 5, text_align = "right",
                                                                        ),
                                                                        jh_make_shiny_table_row_function(left_column_label = "TXA Maintenance (mg/kg/hr):    ",
                                                                                                         input_type = "numeric", 
                                                                                                         input_id = "txa_maintenance", 
                                                                                                         left_column_percent_width = 60, 
                                                                                                         font_size = 13, min = 0, max = 50, initial_value_selected = 5, step = 5, text_align = "right",
                                                                        ),
                                                       )
                                                )
                               )
                    ),
                    column(width = 8, 
                           box(width = 12, title = tags$div(style = "font-size:22px; font-weight:bold", "Surgical Procedures:"), solidHeader = TRUE, status = "primary",
                               box(width = 7, 
                                   fluidRow(
                                       column(width =  11, 
                                              tags$table(
                                                  tags$tr(width = "100%",
                                                          tags$td(width = "30%", tags$div(style = "font-size:18px; font-weight:bold; text-align:left", "Select Approach:")),
                                                          tags$td(width = "70%", 
                                                                  radioGroupButtons(inputId = "spine_approach", 
                                                                                    label = NULL,
                                                                                    choiceNames = list(tags$span(icon("fas fa-smile-beam", style = "color: steelblue"), strong("Anterior")),
                                                                                                       tags$span(icon("fas fa-user", style = "color: steelblue"), strong("Posterior"))),
                                                                                    choiceValues = list("Anterior", "Posterior"),
                                                                                    selected = "Posterior", 
                                                                                    direction = "horizontal",
                                                                                    checkIcon = list(yes = icon("check")),
                                                                                    justified = TRUE
                                                                  )
                                                                  )
                                                  )
                                              ) 
                                       ),
                                       column(width = 1, 
                                              dropdownButton(
                                                  sliderInput(
                                                      inputId = "label_text_size",
                                                      label = "Label Text Size",
                                                      value = 14,
                                                      min = 9,
                                                      max = 20
                                                  ),
                                                  sliderInput(
                                                      inputId = "label_text_offset",
                                                      label = "Move Labels Lateral",
                                                      min = -10,
                                                      max = 10,
                                                      value = -8
                                                  ),
                                                  switchInput(onLabel = "Plot with Patterns (slower)", offLabel = "Plot for Speed",
                                                              inputId = "plot_with_patterns_true",
                                                              value = FALSE
                                                  ),
                                                  circle = TRUE,
                                                  icon = icon("gear"),
                                                  size = "sm",
                                                  inline = TRUE,
                                                  right = TRUE
                                              )
                                       )
                                   ),
                                   fluidRow(
                                       div(style = "font-size:16px; font-weight:bold; font-style:italic; text-align:center", "Add Procedures in Order Performed"),
                                       div(style = "font-size:12px; text-align:center", "Double click (with same procedure selected) to remove an item.")
                                   ),
                                   fluidRow(
                                       column(width = 2,
                                              br(),
                                              noUiSliderInput(inputId = "crop_y",
                                                              label = "Spine Region",
                                                              min = 0,
                                                              max = 1,
                                                              value = c(0.05,0.45), direction = "rtl",
                                                              behaviour = "drag",color = "#0036FD",
                                                              orientation = "vertical",
                                                              height = "600px", width = "3px",
                                                              inline = TRUE)
                                       ), 
                                       column(width = 10,
                                              plotOutput("spine_plan",
                                                         height = 750,
                                                         click = "plot_click",
                                                         dblclick = "plot_double_click"),
                                              textOutput(outputId = "currently_adding"),
                                              switchInput(
                                                  inputId = "lumbar_vertebrae_6",
                                                  label = "6 Lumbar Vertebrae:", 
                                                  value = FALSE, 
                                                  onLabel = "Yes", 
                                                  offLabel = "No", 
                                                  size = "small",
                                                  onStatus = "success"
                                              )
                                       )   
                                   )
                               ),
                               box(width = 5, ##### RIGHT COLUMN NEXT TO SPINE PLOT
                                   box(width = 12, title = div(style = "font-size:16px; font-weight:bold; text-align:left", "Approach & Technique Specifics:"),collapsible = TRUE,  
                                       conditionalPanel(condition = "input.spine_approach.indexOf('Posterior') > -1",
                                                    fluidRow(
                                                        prettyRadioButtons(
                                                                   inputId = "approach_specified_posterior",
                                                                   label = NULL, 
                                                                   inline = TRUE,
                                                                   choices = c("Midline",
                                                                               "Paraspinal or Paramedian", 
                                                                               "Stab"),
                                                                   icon = icon("check"), 
                                                                   bigger = TRUE,
                                                                   status = "info"
                                                               )
                                                        ),
                                                        fluidRow(
                                                            column(width = 6, 
                                                                   prettyRadioButtons(
                                                                       inputId = "approach_open_mis",
                                                                       label = NULL, 
                                                                       inline = TRUE,
                                                                       choices = c("Open",
                                                                                   "Minimally Invasive"),
                                                                       selected = "Open",
                                                                       icon = icon("check"), 
                                                                       bigger = TRUE,
                                                                       status = "success"
                                                                   )
                                                            ), 
                                                            column(width = 6, 
                                                                   prettyCheckboxGroup(
                                                                       inputId = "approach_robot_navigation",
                                                                       label = NULL, 
                                                                       inline = TRUE,
                                                                       choices = c("Robotic Assistance",
                                                                                   "Navigation Assistance"),
                                                                       icon = icon("check"), 
                                                                       bigger = TRUE,
                                                                       status = "success"
                                                                   )
                                                            )
                                                        )
                                   ),
                                   conditionalPanel(condition = "input.spine_approach.indexOf('Anterior') > -1",
                                                    prettyRadioButtons(
                                                        inputId = "approach_specified_anterior",
                                                        label = NULL,
                                                        inline = TRUE,
                                                        choices = c("Paramedian",
                                                                    "Lateral Transpsoas",
                                                                    "Lateral Antepsoas",
                                                                    "Thoracoabdominal",
                                                                    "Thoracotomy",
                                                                    "Transperitoneal",
                                                                    "Retroperitoneal",
                                                                    "Cervical"),
                                                        selected = "Paramedian",
                                                        icon = icon("check"),
                                                        bigger = TRUE,
                                                        status = "info"
                                                        )
                                                    )
                                   ),
                                   conditionalPanel(condition = "input.spine_approach.indexOf('Anterior') > -1",
                                                    div(style = "font-size:16px; font-weight:bold; text-align:left", "1. Select Procedure & Click Spine to Add:")
                                   ),
                                   conditionalPanel(condition = "input.spine_approach.indexOf('Posterior') > -1",
                                                    div(style = "font-size:16px; font-weight:bold; text-align:left", "1. Select Category:"),
                                                    actionBttn(
                                                        inputId = "add_implants",
                                                        size = "sm", 
                                                        block = TRUE,
                                                        label = "Add Surgical Implants (Screws, Hooks, Tethers, Structural Allograft, etc.)",
                                                        style = "simple",
                                                        color = "primary"
                                                        ),
                                                    br(),
                                                    actionBttn(
                                                        inputId = "add_decompressions",
                                                        size = "sm", block = TRUE,
                                                        label = "Add Decompressions",
                                                        style = "simple",
                                                        color = "primary"
                                                    ),
                                                    br(),
                                                    actionBttn(
                                                        inputId = "add_osteotomies",
                                                        size = "sm", block = TRUE,
                                                        label = "Add Osteotomies/Facetectomies",
                                                        style = "simple",
                                                        color = "primary"
                                                    ),
                                                    br(),
                                                    actionBttn(
                                                        inputId = "add_interbody",
                                                        size = "sm", block = TRUE,
                                                        label = "Add Interbody Fusion",
                                                        style = "simple",
                                                        color = "primary"
                                                    ),
                                                    br(),
                                                    uiOutput(outputId = "intervertebral_cage_ui"),
                                                    br(), 
                                                    div(style = "font-size:16px; font-weight:bold; text-align:left", "2. Select Implant/Procedure, & Click Spine to Add:"),
                                                    ),
                                   div(class = "form-group shiny-input-container shiny-input-radiogroup shiny-input-container-inline", style = "width: 100%;text-align: -webkit-center;", 
                                       radioGroupButtons(
                                           inputId = "object_to_add",
                                           direction = "vertical",
                                           width = "95%",
                                           justified = TRUE,
                                           individual = FALSE,
                                           label = NULL,
                                           choices = c(
                                               "Pedicle Screws" = "pedicle_screw",
                                               "Pelvic Screws" = "pelvic_screw",
                                               "Lateral Mass Screws" = "lateral_mass_screw",
                                               "Pars Screws" = "pars_screw",
                                               "Transarticular Screw" = "transarticular_screw",
                                               "Occipital Screws" = "occipital_screw",
                                               "Translaminar Screws" = "translaminar_screw",
                                               "TP Hook" = "tp_hook",
                                               "Laminar Hook (Downgoing)" = "laminar_downgoing_hook",
                                               "Laminar Hook (Upgoing)" = "laminar_upgoing_hook",
                                               "Pedicle Hook" = "pedicle_hook",
                                               "Tether (Spinous Process)" = "tether",
                                               "Sublaminar Wire" = "sublaminar_wire" ,
                                               "Cement Augmentation" = "cement_augmentation"
                                           ),
                                           checkIcon = list(
                                               yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                                           ),
                                           selected = "pedicle_screw"
                                       )
                                   ),
                                   br(),
                                   br(),
                                   actionBttn(
                                       inputId = "reset_all",
                                       size = "xs", block = TRUE,
                                       label = "Reset & Clear All",
                                       style = "simple",
                                       color = "danger"
                                   )
                               )## end of little box to the right of the spine
                           )### end of the full box
                    )
            ),
            tabItem(tabName = "implant_details",   
                    ###########################################
                    box(width = 7, status = "info", title = div(style = "font-size:22px; font-weight:bold; text-align:left", "Add Implant/Fusion Details:"),
                        box(width = 12, collapsible = TRUE, title = div(style = "font-size:20px; font-weight:bold; text-align:center", "Fusion Details:"),
                            fluidRow(
                                column(width = 5, 
                                       conditionalPanel(condition = "input.fusion_procedure_performed == true",
                                                        awesomeCheckboxGroup(inputId = "bone_graft", width = "100%",
                                                                             label = "Select any bone graft used:",
                                                                             choices = c("Morselized Allograft",
                                                                                         "Local Autograft",
                                                                                         "Morselized Autograft (separate fascial incision)",
                                                                                         "Structural Allograft")
                                                        )
                                       )
                                ),
                                column(width = 4, 
                                       conditionalPanel(condition = "input.fusion_procedure_performed == true",
                                                        jh_make_shiny_table_row_function(left_column_label = "BMP Kits:",
                                                                                         left_column_percent_width = 60, 
                                                                                         font_size = 16,
                                                                                         input_type = "numeric",
                                                                                         input_id = "bmp_number", initial_value_selected = 0, min = 0, max = 20, step = 1)
                                       ),
                                       conditionalPanel(condition = "input.bmp_number > 0",
                                                        radioGroupButtons(
                                                            inputId = "bmp_size",
                                                            label = NULL,
                                                            choices = c("XXS" = 1.05, "XS" = 2.1, "Sm" = 4.2, "M" = 8.4, "L" = 12),
                                                            size = "sm",
                                                            selected = 12,
                                                            justified = TRUE,
                                                            checkIcon = list(
                                                                yes = tags$i(class = "fa fa-check-square",
                                                                             style = "color: steelblue"),
                                                                no = tags$i(class = "fa fa-square-o",
                                                                            style = "color: steelblue"))
                                                        )
                                       ),
                                       conditionalPanel(
                                           condition = "input.bone_graft.indexOf('Morselized Allograft') > -1 & input.fusion_procedure_performed == true",
                                           jh_make_shiny_table_row_function(left_column_label = "Allograft (cc):",
                                                                            left_column_percent_width = 60, 
                                                                            font_size = 16,
                                                                            input_type = "numeric",
                                                                            input_id = "allograft_amount", initial_value_selected = 0, min = 0, max = 500, step = 30)
                                       ),
                                       conditionalPanel(
                                           condition = "input.bone_graft.indexOf('Structural Allograft') > -1",
                                           textInput(inputId = "structural_allograft_location",
                                                     label = "Structural allograft was inserted:",
                                                     value = NULL,
                                                     width = "100%")
                                       )
                                ),
                                column(width = 3,
                                       dropdownButton(size = "xs", label = NULL, 
                                                      switchInput(
                                                          inputId = "fusion_procedure_performed",
                                                          width = '100%',
                                                          inline = TRUE,
                                                          label = "Fusion:",
                                                          onLabel = "Yes",
                                                          offLabel = "No",
                                                          value = FALSE,
                                                          size = "mini"
                                                      ),
                                                      switchInput(label = "Left Supplemental Rods Eligible:",
                                                                  inputId = "left_supplemental_rods_eligible",
                                                                  size = "mini",
                                                                  onLabel = "Yes",
                                                                  offLabel = "No", 
                                                                  value = FALSE
                                                      ),
                                                      switchInput(label = "Right Supplemental Rods Eligible:",
                                                                  inputId = "right_supplemental_rods_eligible",
                                                                  size = "mini",
                                                                  onLabel = "Yes",
                                                                  offLabel = "No", 
                                                                  value = FALSE
                                                      )
                                       ),
                                       conditionalPanel(condition = "input.fusion_procedure_performed == true",
                                                        dropdown(icon = icon("link"),
                                                                 # size = "lg",
                                                                 width = "100%",
                                                                 label = "Confirm Fusion Levels",
                                                                 style = "unite",
                                                                 checkboxGroupButtons(inputId = "fusion_levels_confirmed",
                                                                                      label = "Select/Confirm Fusion Levels:",
                                                                                      choices = interbody_levels_df$level,
                                                                                      size = "sm",
                                                                                      direction = "vertical",
                                                                                      checkIcon = list(
                                                                                          yes = tags$i(class = "fa fa-check-square",
                                                                                                       style = "color: steelblue"),
                                                                                          no = tags$i(class = "fa fa-square-o",
                                                                                                      style = "color: steelblue"))
                                                                 )
                                                        )
                                       )
                                )
                            )
                        ),
                        ####
                        box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Interbody Implant Details:"), collapsible = TRUE,  
                            uiOutput(outputId = "interbody_implants_ui")
                        ),
                        conditionalPanel(condition = "input.fusion_procedure_performed == true & input.spine_approach.indexOf('Posterior') > -1",
                                         box(width = 12, title = div(style = "font-size:20px; font-weight:bold; text-align:center", "Rod Details:"), collapsible = TRUE,   
                                             jh_make_shiny_table_column_function(input_type = "title", left_label = "Left Rod(s):",right_label = "Right Rods(s):", font_size = 20, text_align = "left"),
                                             jh_make_shiny_table_column_function(input_type = "pickerInput", 
                                                                                 left_input_id = "left_main_rod_size", 
                                                                                 left_label = "Size:",
                                                                                 right_input_id = "right_main_rod_size", 
                                                                                 right_label = "Size:",
                                                                                 left_column_percent_width = 50,
                                                                                 right_column_percent_width = 50,
                                                                                 choices_vector = c("None", "Transition", "3.5mm", "4.0mm", "4.5mm", "4.75mm", "5.5mm", "6.0mm", "6.35mm/quarter in"),
                                                                                 initial_value_selected = "None",
                                                                                 picker_choose_multiple = FALSE),
                                             jh_make_shiny_table_column_function(input_type = "radioGroupButtons", 
                                                                                 left_input_id = "left_main_rod_material", 
                                                                                 left_label = "Material:",
                                                                                 right_input_id = "right_main_rod_material", 
                                                                                 right_label = "Material:",
                                                                                 left_column_percent_width = 50,
                                                                                 right_column_percent_width = 50,
                                                                                 checkboxes_inline = FALSE,
                                                                                 button_size = "normal",
                                                                                 choices_vector = c("Non-instrumented", "Titanium", "Cobalt Chrome", "Stainless Steel"),
                                                                                 initial_value_selected = "Non-instrumented",
                                                                                 picker_choose_multiple = FALSE),
                                             jh_make_shiny_table_column_function(input_type = "awesomeCheckbox", 
                                                                                 left_input_id = "add_left_accessory_rod", 
                                                                                 right_input_id = "add_right_accessory_rod", 
                                                                                 left_label = "Left Accessory Rod:",
                                                                                 right_label = "Right Accessory Rod:",
                                                                                 left_condition_statement = "input.left_supplemental_rods_eligible == true", 
                                                                                 right_condition_statement = "input.right_supplemental_rods_eligible == true", 
                                                                                 initial_value_selected = FALSE,
                                                                                 status = "success"),
                                             jh_make_shiny_table_column_function(input_type = "sliderTextInput", 
                                                                                 left_input_id = "left_accessory_rod", 
                                                                                 right_input_id = "right_accessory_rod", 
                                                                                 left_condition_statement = "input.add_left_accessory_rod == true", 
                                                                                 right_condition_statement = "input.add_right_accessory_rod == true", 
                                                                                 choices_vector = c("a", "b"),
                                                                                 initial_value_selected = c("a", "b")
                                                                                ),
                                             jh_make_shiny_table_column_function(input_type = "awesomeCheckbox", 
                                                                                 left_input_id = "add_left_satellite_rod", 
                                                                                 left_label = "Left Satellite Rod:",
                                                                                 right_input_id = "add_right_satellite_rod", 
                                                                                 right_label = "Right Satellite Rod:",
                                                                                 left_condition_statement = "input.left_supplemental_rods_eligible == true", 
                                                                                 right_condition_statement = "input.right_supplemental_rods_eligible == true", 
                                                                                 initial_value_selected = FALSE,
                                                                                 status = "success"),
                                             jh_make_shiny_table_column_function(input_type = "sliderTextInput", 
                                                                                 left_input_id = "left_satellite_rod", 
                                                                                 right_input_id = "right_satellite_rod", 
                                                                                 left_condition_statement = "input.add_left_satellite_rod == true", 
                                                                                 right_condition_statement = "input.add_right_satellite_rod == true", 
                                                                                 choices_vector = c("a", "b"),
                                                                                 initial_value_selected = c("a", "b")
                                             ),
                                             jh_make_shiny_table_column_function(input_type = "awesomeCheckbox", 
                                                                                 left_input_id = "add_left_intercalary_rod", 
                                                                                 left_label = "Left Intercalary Rod:",
                                                                                 right_input_id = "add_right_intercalary_rod", 
                                                                                 right_label = "Right Intercalary Rod:",
                                                                                 left_condition_statement = "input.left_supplemental_rods_eligible == true", 
                                                                                 right_condition_statement = "input.right_supplemental_rods_eligible == true", 
                                                                                 initial_value_selected = FALSE,
                                                                                 status = "success"),
                                             tags$table(width = "95%",
                                                        tags$tr(
                                                            tags$td(width = "5%"),
                                                            tags$td(width = "30%",
                                                                    conditionalPanel(condition = "input.add_left_intercalary_rod == true",
                                                                                     sliderTextInput(
                                                                                         inputId = "left_intercalary_rod",
                                                                                         label = NULL,
                                                                                         choices = c("a", "b"),
                                                                                         selected = c("a", "b")
                                                                                     )
                                                                    )),
                                                            tags$td(width = "2.5%"),
                                                            tags$td(width = "15%",
                                                                    conditionalPanel(condition = "input.add_left_intercalary_rod == true",
                                                                                     pickerInput(inputId = "left_intercalary_junction",
                                                                                                 label = "Junction Level:",
                                                                                                 choices = c("a", "b"),
                                                                                                 width = "fit",
                                                                                                 inline = TRUE)
                                                            )),
                                                            tags$td(width = "30%",
                                                                    conditionalPanel(condition = "input.add_right_intercalary_rod == true",
                                                                                     sliderTextInput(
                                                                                         inputId = "right_intercalary_rod",
                                                                                         label = NULL,
                                                                                         choices = c("a", "b"),
                                                                                         selected = c("a", "b")
                                                                                     )
                                                                    )),
                                                            tags$td(width = "2.5%"),
                                                            tags$td(width = "15%",
                                                                    conditionalPanel(condition = "input.add_right_intercalary_rod == true",
                                                                                     pickerInput(inputId = "right_intercalary_junction",
                                                                                                 label = "Junction Level:",
                                                                                                 choices = c("a", "b"),
                                                                                                 width = "fit",
                                                                                                 inline = TRUE)
                                                                    )),
                                                        )
                                             ),
                                             jh_make_shiny_table_column_function(input_type = "awesomeCheckbox", 
                                                                                 left_input_id = "add_left_linked_rods", 
                                                                                 left_label = "Left Linked Rods (Overlapping Region):",
                                                                                 right_input_id = "add_right_linked_rods", 
                                                                                 right_label = "Right Linked Rods (Overlapping Region):",
                                                                                 left_condition_statement = "input.left_supplemental_rods_eligible == true", 
                                                                                 right_condition_statement = "input.right_supplemental_rods_eligible == true", 
                                                                                 initial_value_selected = FALSE,
                                                                                 status = "success"),
                                             jh_make_shiny_table_column_function(input_type = "sliderTextInput", 
                                                                                 left_input_id = "left_linked_rods", 
                                                                                 right_input_id = "right_linked_rods", 
                                                                                 left_condition_statement = "input.add_left_linked_rods == true", 
                                                                                 right_condition_statement = "input.add_right_linked_rods == true", 
                                                                                 choices_vector = c("a", "b"),
                                                                                 initial_value_selected = c("a", "b")
                                                                                 ),
                                             jh_make_shiny_table_column_function(input_type = "awesomeCheckbox", 
                                                                                 left_input_id = "add_left_custom_rods", 
                                                                                 left_label = "Customize Left Rod Construct:",
                                                                                 right_input_id = "add_right_custom_rods", 
                                                                                 right_label = "Customize Right Rod Construct:",
                                                                                 left_condition_statement = "input.left_supplemental_rods_eligible == true", 
                                                                                 right_condition_statement = "input.right_supplemental_rods_eligible == true", 
                                                                                 initial_value_selected = FALSE,
                                                                                 status = "success"),
                                             fixedRow(
                                                 column(width = 6,
                                                        conditionalPanel(condition = "input.add_left_custom_rods == true",
                                                                         awesomeRadio(inputId = "left_custom_rods_number", label = "Number of Left Total Rods:",choices = c(2,3,4,5), inline = TRUE, selected = 2),
                                                                         uiOutput(outputId = "left_custom_rods_ui")
                                                        )
                                                 ),
                                                 column(width = 6,
                                                        conditionalPanel(condition = "input.add_right_custom_rods == true",
                                                                         awesomeRadio(inputId = "right_custom_rods_number", label = "Number of Right Total Rods:",choices = c(2,3,4,5), inline = TRUE, selected = 2),
                                                                         uiOutput(outputId = "right_custom_rods_ui")
                                                        )
                                                 )
                                             )
                                         )
                        ##################### NEW
                        ),
                        conditionalPanel(condition = "input.fusion_procedure_performed == true",
                                         box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Screw Details:"), collapsible = TRUE,
                                             uiOutput(outputId = "pedicle_screw_details_ui"),
                                             uiOutput(outputId = "pedicle_screw_types_ui")
                                             )
                        )
                    ),
                    box(width = 5, status = "primary",
                        plotOutput("spine_plot_for_implants_tab",
                                   height = 750)
                    )
                    ###########################################
            ),
            tabItem(tabName = "operative_note",
                    rclipboardSetup(),
                    ###########################################
                    box(width = 5, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Additional Surgical Details:"),status = "info", solidHeader = TRUE,
                        jh_make_shiny_table_row_function(left_column_percent_width = 30, left_column_label = "Primary Surgeon:", input_type = "text", input_id = "primary_surgeon", initial_value_selected = ""),
                        jh_make_shiny_table_row_function(left_column_percent_width = 30, left_column_label = "Assistants:", input_type = "text", input_id = "surgical_assistants", initial_value_selected = ""),
                        jh_make_shiny_table_row_function(left_column_percent_width = 30, left_column_label = "Preoperative Diagnosis:", input_type = "text", input_id = "preoperative_diagnosis"),
                        jh_make_shiny_table_row_function(left_column_percent_width = 30, left_column_label = "Postoperative Diagnosis:", input_type = "text", input_id = "postoperative_diagnosis"),
                        jh_make_shiny_table_row_function(left_column_percent_width = 30, left_column_label = "Surgical Indications:", input_type = "text", input_id = "indications", initial_value_selected = ""),
                        jh_make_shiny_table_row_function(left_column_percent_width = 30, left_column_label = "Findings:", input_type = "text", input_id = "surgical_findings", initial_value_selected = ""),
                        jh_make_shiny_table_row_function(left_column_percent_width = 30, left_column_label = "Specimens:", input_type = "text", input_id = "specimens_removed", initial_value_selected = ""),
                        jh_make_shiny_table_row_function(left_column_percent_width = 30, left_column_label = "Estimated Blood Loss:", input_type = "numeric", input_id = "ebl", initial_value_selected = 50, min = 0, max = 50000, step = 100),
                        jh_make_shiny_table_row_function(left_column_percent_width = 30, left_column_label = "Crystalloids:", input_type = "numeric", input_id = "crystalloids_administered", min = 0, max = 100000, step = 100),
                        jh_make_shiny_table_row_function(left_column_percent_width = 30, left_column_label = "Colloids:", input_type = "numeric", input_id = "colloids_administered", min = 0, max = 100000, step = 100),
                        jh_make_shiny_table_row_function(left_column_percent_width = 60, left_column_label = "Transfusions/Cell Saver", input_type = "switch", input_id = "transfusion", switch_input_on_label = "Yes", switch_input_off_label = "No"),
                        conditionalPanel(condition = "input.transfusion == true",
                                         box(width = 12, 
                                             jh_make_shiny_table_row_function(left_column_percent_width = 60, left_column_label = "Cell Saver Transfused (cc):", input_type = "numeric", input_id = "cell_saver_transfused", min = 0, max = 10000, step = 100),
                                             jh_make_shiny_table_row_function(left_column_percent_width = 60, left_column_label = "pRBC units transfused:", input_type = "numeric", input_id = "prbc_transfused", min = 0, max = 100, step = 1),
                                             jh_make_shiny_table_row_function(left_column_percent_width = 60, left_column_label = "FFP units transfused:", input_type = "numeric", input_id = "ffp_transfused", min = 0, max = 100, step = 1),
                                             jh_make_shiny_table_row_function(left_column_percent_width = 60, left_column_label = "Cryoprecipitate units transfused:", input_type = "numeric", input_id = "cryoprecipitate_transfused", min = 0, max = 100, step = 1),
                                             jh_make_shiny_table_row_function(left_column_percent_width = 60, left_column_label = "Platelet units transfused:", input_type = "numeric", input_id = "platelets_transfused", min = 0, max = 100, step = 1),
                                         )
                        ),
                        jh_make_shiny_table_row_function(left_column_percent_width = 60, left_column_label = "Intraoperative Complications (including durotomy)?", input_type = "switch", input_id = "intraoperative_complications_true_false"),        
                        conditionalPanel(condition = "input.intraoperative_complications_true_false == true",
                                         box(width = 12, 
                                             jh_make_shiny_table_row_function(left_column_percent_width = 40, left_column_label = "Select any:", input_type = "checkbox", input_id = "intraoperative_complications_vector", choices_vector = c("Durotomy", "Nerve Root Injury", "Loss of Neuromonitoring"), initial_value_selected = NULL),
                                             jh_make_shiny_table_row_function(left_column_percent_width = 40, left_column_label = "Other Intraoperative Complications:", input_type = "text", input_id = "other_intraoperative_complications", initial_value_selected = NULL)
                                         )
                        ),
                        br(),
                        div(class = "form-group shiny-input-container shiny-input-checkboxgroup shiny-input-container-inline", style = "width: 95%;text-align: -webkit-center;",
                            checkboxGroupButtons(inputId = "additional_procedures", 
                                             label = "Additional Procedures to Include:",
                                             # individual = TRUE,
                                             direction = "vertical",
                                             choices = c("Spinal Cord Monitoring",
                                                         "Intraoperative use of microscope for microdissection",
                                                         "Use of stereotactic navigation system for pedicle screw placement",
                                                         "Application of Cranial Tongs",
                                                         "Application of Halo",
                                                         "Application of Halo for thin skull osteology (e.g. pediatric)",
                                                         "Removal of tongs or Halo applied by another inidividual",
                                                         "Irrigation and Debridement",
                                                         "Open Biopsy of vertebral body",
                                                         "Repair of dural/CSF leak",
                                                         "Dural Graft",
                                                         "Removal of spinal instrumentation",
                                                         "Exploration of spinal prior fusion",
                                                         "Open treatment of vertebral fracture",
                                                         "Other"),
                                             selected = "Spinal Cord Monitoring", width = "80%",
                                             checkIcon = list(
                                                 yes = tags$i(class = "fas fa-check",
                                                              style = "color: steelblue")),
                                             justified = TRUE)
                        ),
                        conditionalPanel(condition = "input.additional_procedures.indexOf('Other') > -1",
                                         tags$table(width = "90%" ,
                                                    jh_make_shiny_table_row_function(left_column_label = "Other Procedures:", input_type = "text", input_id = "additional_procedures_other", left_column_percent_width = 30, font_size = 12, initial_value_selected = "",)
                                         )
                        ),
                        br(),
                        hr(),
                        div(style = "font-size:18px; font-weight:bold; text-align:center", "End of Procedure & Closure Details:"),
                        jh_make_shiny_table_row_function(left_column_label = "Deep drains:", input_type = "checkbox", input_id = "deep_drains", left_column_percent_width = 45, font_size = 14, initial_value_selected = 1, choices_vector = c("0", "1", "2", "3", "4", "5"), checkboxes_inline = TRUE, return_as_full_table = TRUE),       
                        jh_make_shiny_table_row_function(left_column_label = "Superficial drains:", input_type = "checkbox", input_id = "superficial_drains", left_column_percent_width = 45, font_size = 14, initial_value_selected = 1, choices_vector = c("0", "1", "2", "3", "4", "5"), checkboxes_inline = TRUE, return_as_full_table = TRUE),       
                        hr(),
                        jh_make_shiny_table_row_function(left_column_label = "Select any used during closure:", 
                                                         input_type = "checkbox", 
                                                         input_id = "additional_end_procedure_details",
                                                         left_column_percent_width = 45, 
                                                         font_size = 14, 
                                                         choices_vector = c("Vancomycin Powder",
                                                                            "Antibiotic Beads"), 
                                                         initial_value_selected = "Vancomycin Powder",
                                                         return_as_full_table = TRUE
                        ),
                        hr(),
                        jh_make_shiny_table_row_function(left_column_label = "Skin Closure:", 
                                                         input_type = "checkbox", 
                                                         input_id = "closure_details",
                                                         left_column_percent_width = 45, 
                                                         font_size = 14, 
                                                         choices_vector = c("Subcutaneous suture",
                                                                            "Nylon",
                                                                            "Staples"), 
                                                         initial_value_selected = "Subcutaneous suture", 
                                                         return_as_full_table = TRUE
                        ),
                        hr(),
                        jh_make_shiny_table_row_function(left_column_label = "Skin/Dressing:", 
                                                         input_type = "checkbox", 
                                                         input_id = "dressing_details",
                                                         left_column_percent_width = 45, 
                                                         font_size = 14, 
                                                         choices_vector = c(
                                                             "Steristrips",
                                                             "Dermabond",
                                                             "Prineo",
                                                             "an Incisional Wound Vac",
                                                             "a water tight dressing"), 
                                                         initial_value_selected = c("Steristrips", "a water tight dressing"), return_as_full_table = TRUE
                        ),
                        br()
                    ),
                    box(width = 7, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Data Upload:"),status = "success", solidHeader = TRUE,
                               dropdown(icon = icon("upload"),
                                        width = "500%",
                                        size = "sm",
                                        label = "Upload to Redcap Project",
                                        style = "fill",
                                        color = "success",
                                        animate = animateOptions(
                                            enter = animations$fading_entrances$fadeInLeftBig,
                                            exit = animations$fading_exits$fadeOutRightBig
                                        ),
                                        h3("Confirm the Data Here is Correct:"),
                                        tableOutput(outputId = "summary_table_for_redcap_preview"),
                                        actionBttn(inputId = "confirm_upload_1", label = "Confirmed, Upload to Redcap", style = "simple", color = "primary"),
                               )
                    ),
                    box(width = 7, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Operative Note Generator:"),status = "success", solidHeader = TRUE,
                        actionBttn(
                            inputId = "generate_operative_note",
                            block = TRUE,
                            size = "md",
                            label = "Click to Generate Operative Note",
                            style = "float",
                            color = "primary"
                        ),
                        br(),
                        textAreaInput(inputId = "operative_note_text", label = "Operative Note:", width = "100%", height = 750),
                        br(),
                        br(),
                        uiOutput("clipboard_ui")
                    )
                    ###########################################
            ),
            tabItem(tabName = "tables",
                    ###########################################
                    box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Procedure Summary Table:"),status = "success", collapsible = TRUE, solidHeader = TRUE,
                        tableOutput(outputId = "summary_table")
                    ),
                    box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Procedure Specifics"),status = "success", collapsible = TRUE,solidHeader = TRUE,
                        tableOutput(outputId = "upload_df")
                    ),
                    box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "All objects table:"),status = "success", collapsible = TRUE,solidHeader = TRUE,
                        tableOutput(outputId = "all_objects_table")
                    ),
                    box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Pedicle Screw Details:"),status = "success", collapsible = TRUE,solidHeader = TRUE,
                        tableOutput("pedicle_screw_details_table")
                    ),
                    box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Interbody implants:"),status = "success", collapsible = TRUE,solidHeader = TRUE,
                        tableOutput(outputId = "interbody_details_table")
                    )
                    ###########################################
            )
        )
    )
    )
# )


###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ########## SERVER STARTS  ###### ###### ###### ###### ###### ######  ###### ###### ###### ###### ###### ######
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ########## SERVER STARTS  ###### ###### ###### ###### ###### ######  ###### ###### ###### ###### ###### ######
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ########## SERVER STARTS  ###### ###### ###### ###### ###### ######  ###### ###### ###### ###### ###### ######
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ########## SERVER STARTS  ###### ###### ###### ###### ###### ######  ###### ###### ###### ###### ###### ######
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ########## SERVER STARTS  ###### ###### ###### ###### ###### ######  ###### ###### ###### ###### ###### ######


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observeEvent(input$lumbar_vertebrae_6, ignoreInit = TRUE,{
        if(input$lumbar_vertebrae_6 == TRUE){
            ## first backup L5
                    l5_levels_vector <<- levels_vector
                    l5_labels_df <<- labels_df
                    l5_vertebral_numbers_vector <<- vertebral_numbers_vector
                    l5_levels_numbered_df <<- levels_numbered_df
                    l5_jh_get_vertebral_number_function <<- jh_get_vertebral_number_function
                    l5_jh_get_vertebral_level_function <<- jh_get_vertebral_level_function
                    l5_spine_png <<- spine_png
                    l5_anterior_spine_jpg <<- anterior_spine_jpg
                    l5_interbody_levels_df <<-interbody_levels_df
                    l5_revision_implants_df <<- revision_implants_df
                    l5_anterior_df <<- anterior_df
                    l5_all_implants_constructed_df <<- all_implants_constructed_df
                    l5_spine_png <<- spine_png
                    l5_open_canal_df <<- open_canal_df
            
                    spine_png <<- l6_spine_png
                    
                    labels_df <<- l6_labels_df
                    levels_numbered_df <<- l6_levels_numbered_df
            
                    all_implants_constructed_df <<- all_implants_constructed_df %>%
                        filter(vertebral_number < 24.2) %>%
                        union_all(l6_all_implants_constructed_df)
                    
                    anterior_df <<- l6_anterior_df
                    # implant_starts_df <- l6_implant_starts_df
                    jh_get_vertebral_number_function <<- l6_jh_get_vertebral_number_function
                    jh_get_vertebral_level_function <<- l6_jh_get_vertebral_level_function
                    revision_implants_df <<- l6_revision_implants_df
                    open_canal_df <<- l6_open_canal_df
                    
        }
        
        if(input$lumbar_vertebrae_6 == FALSE){
            spine_png <<- l5_spine_png

            labels_df <<- l5_labels_df
            levels_numbered_df <<- l5_levels_numbered_df

            all_implants_constructed_df <<- all_implants_constructed_df %>%
                filter(vertebral_number < 24.2) %>%
                union_all(l5_all_implants_constructed_df)

            anterior_df <<- l5_anterior_df
            # implant_starts_df <- l5_implant_starts_df
            jh_get_vertebral_number_function <<- l5_jh_get_vertebral_number_function
            jh_get_vertebral_level_function <<- l5_jh_get_vertebral_level_function
            revision_implants_df <<- l5_revision_implants_df
            open_canal_df <<- l5_open_canal_df
        }
        
    })

    ########################################## RENDER UI'S    ########################################## 
    ########################################## RENDER UI'S    ########################################## 
    ########################################## RENDER UI'S    ########################################## 

    ################------------------  Approaches    ----------------------######################  
    
    spine_approaches_list_reactive <- reactive({
        if(input$spine_approach == "Anterior"){
            approach_choices <- c("Paramedian",
                                  "Lateral Transpsoas",
                                  "Lateral Antepsoas",
                                  "Thoracoabdominal",
                                  "Thoracotomy",
                                  "Transperitoneal",
                                  "Retroperitoneal",
                                  "Cervical")
            approach_selected <- "Paramedian"
        }
        if(input$spine_approach == "Posterior"){
            approach_choices <- c("Midline",
                                  "Paraspinal or Paramedian",
                                  "Stab")
            approach_selected <- "Midline"
        }
        
        list(approach_choices = approach_choices, 
             approach_selected = approach_selected)
    })
    
    observeEvent(input$spine_approach,  {
        updatePrettyRadioButtons(session = session, 
                                 inputId = "approach_specified",
                                 prettyOptions = list(bigger = TRUE, 
                                                      status = "info",
                                                      animation = "jelly"), 
                                 label = NULL, 
                                 inline = TRUE,
                                 choices = spine_approaches_list_reactive()$approach_choices,
                                 selected = spine_approaches_list_reactive()$approach_selected)
        
    })
    
    ################------------------  Diagnoses    ----------------------######################  
    
    observeEvent(input$primary_diagnosis_group,{
        if(input$primary_diagnosis_group == "spinal_condition"){
            updatePickerInput(session = session, inputId = "diagnosis_subgroup", label = NULL, 
                              choices = c("Spinal Stenosis", 
                                          "Myelopathy", 
                                          "Foraminal Stenosis",
                                          "Degenerative (acquired) Spondylolisthesis",
                                          "Spondylolysis",
                                          "Spondylolisthesis",
                                          "Pseudarthrosis",
                                          "Lumbar Disc Herniation",
                                          "Cauda Equina Syndrome",
                                          "Other"))
            
        }
        
        if(input$primary_diagnosis_group == "neoplasm"){
            updatePickerInput(session = session, inputId = "diagnosis_subgroup", label = NULL, 
                              choices = c("Metastatic Disease to the Spinal column",
                                          "Primary Neoplasm of the spine",
                                          "Other"))
            
        }
        
        if(input$primary_diagnosis_group == "lesion"){
            updatePickerInput(session = session, inputId = "diagnosis_subgroup", label = NULL, 
                              choices = c("Traumatic Burst Fracture", 
                                          "Fracture Dislocation", 
                                          "Osteomyelitis/Diskitis",
                                          "Traumatic Spondylolisthesis",
                                          "Cauda Equina Syndrome",
                                          "Other"))
            
        }
        
        if(input$primary_diagnosis_group == "deformity"){
            updatePickerInput(session = session, inputId = "diagnosis_subgroup", label = NULL, 
                              choices = c("Idiopathic Scoliosis",
                                          "Scoliosis associated with another condition",
                                          "Hemivertebra",
                                          "Secondary Kyphosis",
                                          "Scheurmanns Kyphosis",
                                          "Acquired Spondylolisthesis", 
                                          "Congenital Spondylolisthesis", 
                                          "Lumbar Disc Herniation",
                                          "Spinal Stenosis",
                                          "Flatback Syndrome",
                                          "Other"
                              ))
            
        }
    })
    
    
    ################------------------  Operative Report Details    ----------------------######################  
    
    
    observeEvent(list(input$diagnosis_subgroup, input$diagnosis_subgroup_other),{
        updateTextInput(session = session, inputId = "preoperative_diagnosis", 
                        value = glue_collapse(x = map(.x = input$diagnosis_subgroup, 
                                                      .f = ~ if_else(.x == "Other", input$diagnosis_subgroup_other, .x)),
                                              sep = ", ", last = " and "))
    })
    
    observeEvent(list(input$diagnosis_subgroup, input$diagnosis_subgroup_other),{
        updateTextInput(session = session, inputId = "postoperative_diagnosis", 
                        value = glue_collapse(x = map(.x = input$diagnosis_subgroup, 
                                                      .f = ~ if_else(.x == "Other", input$diagnosis_subgroup_other, .x)),
                                              sep = ", ", last = " and "))
    })
    
    observeEvent(input$symptoms,{
        symptoms <- paste0(glue_collapse(input$symptoms, sep = ", ", last = ' and '), 
                          ", resistant to conservative measures.")
        
        updateTextInput(session = session,
                        inputId = "indications", value = str_to_sentence(string = symptoms))
    })
    
    observeEvent(input$prior_fusion_levels, {
        if(length(input$prior_fusion_levels) > 0){
            updateCheckboxGroupButtons(session = session, 
                                       inputId = "additional_procedures", 
                                       selected = append(input$additional_procedures, "Exploration of spinal prior fusion"))
        }
    })
    observeEvent(input$removal_instrumentation_levels, {
        if(length(input$removal_instrumentation_levels) > 0){
            updateCheckboxGroupButtons(session = session, 
                                       inputId = "additional_procedures", 
                                       selected = append(input$additional_procedures, "Removal of spinal instrumentation"))
        }
    })
    
    observeEvent(input$fusion_procedure_performed, {
        if(input$fusion_procedure_performed == TRUE){
            updateAwesomeCheckboxGroup(session = session,
                                       inputId = "bone_graft", 
                                       selected = append(input$bone_graft, "Morselized Allograft"))
        }
    })
    
    
    observeEvent(input$diagnosis_subgroup, {
        if(any(str_detect(string = input$diagnosis_subgroup, pattern = "Fracture"))){
            updateCheckboxGroupButtons(session = session, 
                                       inputId = "additional_procedures", 
                                       selected = append(input$additional_procedures, "Open treatment of vertebral fracture"))
        }
    })
    
    
    
    ################------------------  ADDING OBJECTS TO PLOT    ----------------------######################  
    
    
    observeEvent(input$spine_approach, {
        if(input$spine_approach == "Anterior"){
            updateRadioGroupButtons(session = session, 
                                    inputId = "object_to_add",
                                    choices = c(
                                        "Disc Arthroplasty" = "anterior_disc_arthroplasty",
                                        "Decompression + Diskectomy & <br/>Fusion + Interbody Implant" = "decompression_diskectomy_fusion",
                                        "Diskectomy & Fusion + Interbody Implant" = "diskectomy_fusion",
                                        "Diskectomy & Interbody Fusion (No Implant)" = "diskectomy_fusion_no_interbody_device",
                                        "Corpectomy" = "corpectomy",
                                        "Corpectomy Cage" = "corpectomy_cage",
                                        "Anterior Plate (distinct from interbody)" = "anterior_plate",
                                        "Anterior Buttress Plate" = "anterior_buttress_plate",
                                        "Screw +/Washer" = "screw_washer"
                                    ),
                                    checkIcon = list(
                                        yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                                    ),
                                    selected = "diskectomy_fusion"
            )
        }
        
    })
    
    
    output$intervertebral_cage_ui <- renderUI({
        osteotomy_df <- all_objects_to_add_list$objects_df %>%
            filter(object == "grade_3" |
                       object == "grade_4" | 
                       object == "grade_5" |
                       object == "grade_6") %>%
            select(level, vertebral_number, approach, object) %>%
            distinct()
        
        if(nrow(osteotomy_df) > 0){
            actionBttn(
                inputId = "add_intervertebral_cage",
                size = "sm", block = TRUE,
                label = "Add Intervertebral Cage (After VCR/Corpectomy)",
                style = "simple",
                color = "primary"
            )
        }else{
            NULL
        }
    })
    observeEvent(input$add_intervertebral_cage, {
        updateRadioGroupButtons(session = session, 
                                inputId = "object_to_add",
                                choices = c(
                                    "Intervertebral Cage" = "intervertebral_cage"
                                ),
                                checkIcon = list(
                                    yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                                ),
                                selected = "intervertebral_cage"
        )
    })
    
    observeEvent(list(input$add_implants, input$spine_approach), {
        if(input$spine_approach == "Posterior"){
            updateRadioGroupButtons(session = session, 
                                    inputId = "object_to_add", 
                                    choices = c(
                                        "Pedicle Screws" = "pedicle_screw",
                                        "Pelvic Screws" = "pelvic_screw",
                                        "Lateral Mass Screws" = "lateral_mass_screw",
                                        "Pars Screws" = "pars_screw",
                                        "Transarticular Screw" = "transarticular_screw",
                                        "Occipital Screws" = "occipital_screw",
                                        "Translaminar Screws" = "translaminar_screw",
                                        "TP Hook" = "tp_hook",
                                        "Laminar Hook (Downgoing)" = "laminar_downgoing_hook",
                                        "Laminar Hook (Upgoing)" = "laminar_upgoing_hook",
                                        "Pedicle Hook" = "pedicle_hook",
                                        "Tether" = "tether",
                                        "Sublaminar Wire" = "sublaminar_wire" , 
                                        "Cement Augmentation" = "cement_augmentation"
                                    ),
                                    checkIcon = list(
                                        yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                                    ),
                                    selected = "pedicle_screw"
            )
        }
    })
    
    observeEvent(input$add_decompressions, {
        updateRadioGroupButtons(session = session, 
                                inputId = "object_to_add",
                                choices = c(
                                    "Laminoplasty" = "laminoplasty",
                                    "Decompression + Foraminotomies" = "sublaminar_decompression",
                                    "Central Laminectomy" = "laminectomy",
                                    "Laminotomy (Hemilaminectomy)" = "laminotomy",
                                    "Diskectomy" = "diskectomy",
                                    "Transpedicular Decompression" = "transpedicular_approach",
                                    "Costovertebral Decompression" = "costovertebral_approach"
                                ),
                                checkIcon = list(
                                    yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                                ),
                                selected = "sublaminar_decompression"
        )
    })
    
    observeEvent(input$add_osteotomies, {
        updateRadioGroupButtons(session = session, 
                                inputId = "object_to_add", 
                                choices = c("Grade 1 (Inferior Facetectomy)" = "grade_1",
                                            "Complete Facetectomy (Unilateral)" = "complete_facetectomy",
                                            "Grade 2 (PCO)" = "grade_2", 
                                            "Grade 3 (PSO)" = "grade_3",
                                            "Grade 4 (Extended PSO)" = "grade_4", 
                                            "Grade 5 (VCR)" = "grade_5", 
                                            # "Grade 6 (Multilevel VCR)" = "grade_6", 
                                            "Costotransversectomy" = "costotransversectomy"),
                                checkIcon = list(
                                    yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                                ),
                                selected = "grade_1"
        )
    })
    
    observeEvent(input$add_interbody, {
        updateRadioGroupButtons(session = session, 
                                inputId = "object_to_add", 
                                choices = c("TLIF" = "tlif",
                                            # "LLIF" = "llif",
                                            "PLIF" = "plif", 
                                            "Interbody Fusion, No Implant" = "no_implant_interbody_fusion"),
                                checkIcon = list(
                                    yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                                ),
                                selected = "TLIF"
        )
    })
    
    output$currently_adding <- renderText(paste("Currently Adding: ", str_to_title(string = str_replace_all(string = input$object_to_add, pattern = "_", replacement = " "))), sep = "")
    
    
    ################------------------  RODS    ----------------------######################  
    left_rod_implants_df_reactive <- reactive({
        all_objects_to_add_list$objects_df %>%
            filter(side == "left") %>%
            filter(approach == "posterior") %>%
            filter(str_detect(string = object, pattern = "screw") | str_detect(string = object, pattern = "hook") | str_detect(string = object, pattern = "wire")) %>%
            select(level, vertebral_number, x, y, side, object) %>%
            # distinct() %>%
            arrange(vertebral_number) 
    })
    
    right_rod_implants_df_reactive <- reactive({
        all_objects_to_add_list$objects_df %>%
            filter(side == "right") %>%
            filter(approach == "posterior") %>%
            filter(str_detect(string = object, pattern = "screw") | str_detect(string = object, pattern = "hook") | str_detect(string = object, pattern = "wire")) %>%
            select(level, vertebral_number, x, y, side, object) %>%
            distinct() %>%
            arrange(vertebral_number)
    })
    
    
    ######### ######### ROD SIZE AND ROD MATERIAL ######### #########
    
    observeEvent(left_rod_implants_df_reactive(), {
        if(nrow(left_rod_implants_df_reactive()) > 1){
            updatePickerInput(session = session, 
                              inputId = "left_main_rod_size", 
                              selected = if_else(input$left_main_rod_size == "None", "5.5mm", input$left_main_rod_size)
            )
            updateSliderTextInput(session = session, 
                                  inputId = "left_main_rod_material", 
                                  choices = c("Titanium", "Cobalt Chrome", "Stainless Steel"),
                                  selected = if_else(input$left_main_rod_material == "Non-instrumented", "Titanium", input$left_main_rod_material)
            )
        }
    })
    observeEvent(right_rod_implants_df_reactive(), {
        if(nrow(right_rod_implants_df_reactive()) > 1){
            updatePickerInput(session = session, 
                              inputId = "right_main_rod_size", 
                              selected = if_else(input$right_main_rod_size == "None", "5.5mm", input$right_main_rod_size)
            )
            updateSliderTextInput(session = session, 
                                  inputId = "right_main_rod_material", 
                                  choices = c("Titanium", "Cobalt Chrome", "Stainless Steel"),
                                  selected = if_else(input$right_main_rod_material == "Non-instrumented", "Titanium", input$right_main_rod_material)
            )
        }
    })
    
    ######### ######### sUPPLEMENTAL ROD OPTIONS ######### #########
    osteotomy_level_reactive <- reactive({
        if(any(any(all_objects_to_add_list$objects_df$object == "grade_3") |
               any(all_objects_to_add_list$objects_df$object == "grade_4") |
               any(all_objects_to_add_list$objects_df$object == "grade_5"))){
            
            osteotomy_df <- all_objects_to_add_list$objects_df %>%
                filter(object == "grade_3" | object == "grade_4" | object == "grade_5") %>%
                select(level, vertebral_number) 
            
            osteotomy_level <- head(osteotomy_df$level, n = 1)
        }else{
            osteotomy_level <- NULL
        }
        osteotomy_level
    })
    
    ################------------------  Left RODS    ----------------------######################  
    
    observeEvent(input$reset_all, {
        updateSwitchInput(session = session, inputId = "left_supplemental_rods_eligible", value = FALSE)
    })
    
    left_supplement_rod_starts_list_reactive <- reactive({
        all_added_objects_df <- all_objects_to_add_list$objects_df %>%
            select(level, vertebral_number, object, x, y, side) %>%
            filter(side == "left")
        
        jh_cranial_and_caudal_list_for_supplementary_rods_function(all_added_objects_df, osteotomy_site = osteotomy_level_reactive())
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, left_supplement_rod_starts_list_reactive()), {
        if(nrow(left_rod_implants_df_reactive()) > 2){
            updateSwitchInput(session = session, inputId = "left_supplemental_rods_eligible", value = TRUE)
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, left_supplement_rod_starts_list_reactive()), {
        if(input$left_supplemental_rods_eligible == TRUE){
            updateSliderTextInput(session = session,
                                  inputId = "left_accessory_rod",
                                  choices = left_supplement_rod_starts_list_reactive()$all_levels,
                                  selected = left_supplement_rod_starts_list_reactive()$accessory_starts)        
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, left_supplement_rod_starts_list_reactive()), {
        if(input$left_supplemental_rods_eligible == TRUE){
            updateSliderTextInput(session = session,
                                  inputId = "left_satellite_rod",
                                  choices = left_supplement_rod_starts_list_reactive()$all_levels,
                                  selected = left_supplement_rod_starts_list_reactive()$satellite_starts)
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, left_supplement_rod_starts_list_reactive()), {
        if(input$left_supplemental_rods_eligible == TRUE){
            updateSliderTextInput(session = session,
                                  inputId = "left_intercalary_rod",
                                  choices = left_supplement_rod_starts_list_reactive()$all_levels,
                                  selected = left_supplement_rod_starts_list_reactive()$intercalary_starts)
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, left_supplement_rod_starts_list_reactive()), {
        if(nrow(left_rod_implants_df_reactive())>3){
            choices_df <- tibble(vertebral_number = seq(from = min(left_rod_implants_df_reactive()$vertebral_number), to = max(left_rod_implants_df_reactive()$vertebral_number), by = 1)) %>%
                left_join(levels_numbered_df)
            
            if(!is.null(osteotomy_level_reactive())){
                updatePickerInput(session = session, inputId = "left_intercalary_junction",
                                  label = "Intercalary Junction:",
                                  choices = choices_df$level,
                                  selected = osteotomy_level_reactive())
            }else{
                updatePickerInput(session = session, inputId = "left_intercalary_junction",
                                  label = "Intercalary Junction:",
                                  choices = choices_df$level,
                                  selected = head(x = tail(choices_df$level, 3), 1)
                                  )
            }
            
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, left_supplement_rod_starts_list_reactive()), {
        if(input$left_supplemental_rods_eligible == TRUE){
            updateSliderTextInput(session = session,
                                  inputId = "left_linked_rods",
                                  choices = left_supplement_rod_starts_list_reactive()$all_levels,
                                  selected = left_supplement_rod_starts_list_reactive()$linked_starts)
        }
    })


    # 
    output$left_custom_rods_ui <- renderUI({
        if(input$add_left_custom_rods == TRUE){
            left_implants_df <- all_objects_to_add_list$objects_df %>%
                select(level, side, object, x, y) %>%
                filter(side == "left") %>%
                filter(str_detect(string = object, pattern = "screw") | str_detect(string = object, pattern = "hook") | str_detect(string = object, pattern = "wire")) %>%
                mutate(implant_label = glue("{level} {str_to_title(str_replace_all(object, '_', ' '))}"))

            if(input$left_custom_rods_number > 1){
                column(12,
                       pickerInput(inputId = "left_custom_rod_1",label = "Rod 1 Connects to:",
                                   choices = left_implants_df$implant_label,
                                   multiple = TRUE,
                                   options = list(`actions-box` = TRUE)),
                       pickerInput(inputId = "left_custom_rod_2",label = "Rod 2 Connects to:",
                                   choices = left_implants_df$implant_label,
                                   multiple = TRUE,
                                   options = list(`actions-box` = TRUE)),
                       if(input$left_custom_rods_number > 2){
                            pickerInput(inputId = "left_custom_rod_3",label = "Rod 3 Connects to:",
                                               choices = left_implants_df$implant_label,
                                               multiple = TRUE,
                                               options = list(`actions-box` = TRUE))
                       },
                       if(input$left_custom_rods_number > 3){
                           pickerInput(inputId = "left_custom_rod_4",label = "Rod 4 Connects to:",
                                                choices = left_implants_df$implant_label,
                                                multiple = TRUE,
                                                options = list(`actions-box` = TRUE))
                       },
                       if(input$left_custom_rods_number > 4){
                           pickerInput(inputId = "left_custom_rod_5",label = "Rod 5 Connects to:",
                                                choices = left_implants_df$implant_label,
                                                multiple = TRUE,
                                                options = list(`actions-box` = TRUE))
                       }
                )
            }
        }else{
            NULL
        }
    })
    # 
    # 
    # ################------------------  Right RODS    ----------------------######################  
    # 
    observeEvent(input$reset_all, {
        updateSwitchInput(session = session, inputId = "right_supplemental_rods_eligible", value = FALSE)
    })
    
    right_supplement_rod_starts_list_reactive <- reactive({
        all_added_objects_df <- all_objects_to_add_list$objects_df %>%
            select(level, vertebral_number, object, x, y, side) %>%
            filter(side == "right")
        
        jh_cranial_and_caudal_list_for_supplementary_rods_function(all_added_objects_df, osteotomy_site = osteotomy_level_reactive())
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, right_supplement_rod_starts_list_reactive()), {
        if(nrow(right_rod_implants_df_reactive()) > 3){
            updateSwitchInput(session = session, inputId = "right_supplemental_rods_eligible", value = TRUE)
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, right_supplement_rod_starts_list_reactive()), {
        if(input$right_supplemental_rods_eligible == TRUE){
            updateSliderTextInput(session = session,
                                  inputId = "right_accessory_rod",
                                  choices = right_supplement_rod_starts_list_reactive()$all_levels,
                                  selected = right_supplement_rod_starts_list_reactive()$accessory_starts)        
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, right_supplement_rod_starts_list_reactive()), {
        if(input$right_supplemental_rods_eligible == TRUE){
            updateSliderTextInput(session = session,
                                  inputId = "right_satellite_rod",
                                  choices = right_supplement_rod_starts_list_reactive()$all_levels,
                                  selected = right_supplement_rod_starts_list_reactive()$satellite_starts)
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, right_supplement_rod_starts_list_reactive()), {
        if(input$right_supplemental_rods_eligible == TRUE){
            updateSliderTextInput(session = session,
                                  inputId = "right_intercalary_rod",
                                  choices = right_supplement_rod_starts_list_reactive()$all_levels,
                                  selected = right_supplement_rod_starts_list_reactive()$intercalary_starts)
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, right_supplement_rod_starts_list_reactive()), {
        if(nrow(right_rod_implants_df_reactive())>3){
            choices_df <- tibble(vertebral_number = seq(from = min(right_rod_implants_df_reactive()$vertebral_number), to = max(right_rod_implants_df_reactive()$vertebral_number), by = 1)) %>%
                left_join(levels_numbered_df)
            
            if(!is.null(osteotomy_level_reactive())){
                updatePickerInput(session = session, inputId = "right_intercalary_junction",
                                  label = "Intercalary Junction:",
                                  choices = choices_df$level,
                                  selected = osteotomy_level_reactive())
            }else{
                updatePickerInput(session = session, inputId = "right_intercalary_junction",
                                  label = "Intercalary Junction:",
                                  choices = choices_df$level,
                                  selected = head(x = tail(choices_df$level, 3), 1)
                )
            }
            
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, left_supplement_rod_starts_list_reactive()), {
        if(input$left_supplemental_rods_eligible == TRUE){
            updateSliderTextInput(session = session,
                                  inputId = "left_linked_rods",
                                  choices = left_supplement_rod_starts_list_reactive()$all_levels,
                                  selected = left_supplement_rod_starts_list_reactive()$linked_starts)
        }
    })
    
    
    # 
    output$right_custom_rods_ui <- renderUI({
        if(input$add_right_custom_rods == TRUE){
            right_implants_df <- all_objects_to_add_list$objects_df %>%
                select(level, side, object, x, y) %>%
                filter(side == "right") %>%
                filter(str_detect(string = object, pattern = "screw") | str_detect(string = object, pattern = "hook") | str_detect(string = object, pattern = "wire")) %>%
                mutate(implant_label = glue("{level} {str_to_title(str_replace_all(object, '_', ' '))}"))

            if(input$right_custom_rods_number > 1){
                column(12,
                       pickerInput(inputId = "right_custom_rod_1",label = "Rod 1 Connects to:",
                                   choices = right_implants_df$implant_label,
                                   multiple = TRUE,
                                   options = list(`actions-box` = TRUE)),
                       pickerInput(inputId = "right_custom_rod_2",label = "Rod 2 Connects to:",
                                   choices = right_implants_df$implant_label,
                                   multiple = TRUE,
                                   options = list(`actions-box` = TRUE)),
                       if(input$right_custom_rods_number > 2){
                           pickerInput(inputId = "right_custom_rod_3",label = "Rod 3 Connects to:",
                                       choices = right_implants_df$implant_label,
                                       multiple = TRUE,
                                       options = list(`actions-box` = TRUE))
                       },
                       if(input$right_custom_rods_number > 3){
                           pickerInput(inputId = "right_custom_rod_4",label = "Rod 4 Connects to:",
                                       choices = right_implants_df$implant_label,
                                       multiple = TRUE,
                                       options = list(`actions-box` = TRUE))
                       },
                       if(input$right_custom_rods_number > 4){
                           pickerInput(inputId = "right_custom_rod_5",label = "Rod 5 Connects to:",
                                       choices = right_implants_df$implant_label,
                                       multiple = TRUE,
                                       options = list(`actions-box` = TRUE))
                       }
                )
            }
        }else{
            NULL
        }
    })

    ################------------------  Fusion Levels   ----------------------######################  
    
    
    
    observeEvent(fusion_levels_computed_reactive_df(), {
        updateCheckboxGroupButtons(session = session, 
                                   inputId = "fusion_levels_confirmed", 
                                   selected = fusion_levels_computed_reactive_df()$level)
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, fusion_levels_computed_reactive_df()) , {
        if(nrow(fusion_levels_computed_reactive_df()) > 0){
            updateSwitchInput(session = session, 
                              inputId = "fusion_procedure_performed", 
                              value = TRUE)
        }
    })
    
    ################------------------  Interbody Details (and generating results)    ----------------------######################  
    
    
    output$interbody_implants_ui <- renderUI({
        interbody_implants_df <- interbody_df_reactive()
        
        if(nrow(interbody_implants_df) >0){
            
            fixedRow(
                column(width = 12, 
                       # h4(strong("Interbody Implant Details:")),
                       column(width = 3,
                              h4(strong("Level:"))
                       ),
                       column(width = 9, 
                              fixedRow(
                                  column(width = 5, 
                                         h4(strong("Composition:")), 
                                  ), 
                                  column(width = 4,
                                         h4(strong("Device Name:"))
                                  ), 
                                  column(width = 3, 
                                         h4(strong("Height(mm):"))
                                  )
                              )
                       )
                ),
                map(.x = interbody_implants_df$level, .f = ~make_interbody_ui_function(level = .x))
            )
        }
        
    })
    
    interbody_df_reactive <- reactive({
        if(sum(str_count(string = all_objects_to_add_list$objects_df$object, pattern = "intervertebral_cage"))>0){
            cages_df <- all_objects_to_add_list$objects_df %>%
                filter(object == "intervertebral_cage") %>%
                arrange(vertebral_number)
            
            intervertebral_cage_df <- cages_df %>%
                filter(vertebral_number == min(vertebral_number)) %>%
                mutate(level = as.character(glue_collapse(x = cages_df$level, sep = "-"))) %>%
                select(level, vertebral_number, object, approach)
        }else{
            intervertebral_cage_df <- tibble(level = character(), vertebral_number = double(), object = character(), approach = character())
        }
        
        interbody_implants_df <- all_objects_to_add_list$objects_df %>%
            filter(
                object == "anterior_interbody_implant" |
                    object == "anterior_interbody_implant" |
                    object == "tlif" |
                    object == "plif" |
                    object == "llif" |
                    object == "anterior_disc_arthroplasty" |
                    object == "corpectomy_cage") %>%
            select(level, vertebral_number, object, approach) %>%
            union_all(intervertebral_cage_df) %>%
            distinct() %>%
            arrange(vertebral_number)
        
    })
    
    interbody_details_df_reactive <- reactive({
        
        interbody_implants_df <- interbody_df_reactive()
        
        if(nrow(interbody_implants_df) > 0){
            
            interbody_details_df <- interbody_implants_df %>%
                mutate(level_label = str_to_lower(string = str_replace_all(string = level, pattern = "-", replacement = "_"))) %>%
                mutate(composition_label = glue("{level_label}_interbody_composition")) %>%
                mutate(device_name_label = glue("{level_label}_interbody_device_name")) %>%
                mutate(height_label = glue("{level_label}_interbody_height")) %>%
                mutate(integrated_fixation_label = glue("{level_label}_interbody_integrated_fixation")) %>%
                mutate(expandable_label = glue("{level_label}_interbody_expandable")) %>%
                mutate(other_label = glue("{level_label}_interbody_other")) %>%
                # select(-level) %>%
                # left_join(levels_numbered_df) %>%
                mutate(composition = map(.x = composition_label, .f = ~input[[.x]])) %>%
                mutate(device_name = map(.x = device_name_label, .f = ~input[[.x]])) %>%
                mutate(height = map(.x = height_label, .f = ~input[[.x]])) %>%
                mutate(integrated_fixation = map(.x = integrated_fixation_label, .f = ~if_else(is.null(input[[.x]]), "xx", input[[.x]]))) %>%
                mutate(expandable = map(.x = expandable_label, .f = ~if_else(is.null(input[[.x]]), "xx", input[[.x]]))) %>%
                mutate(other = map(.x = other_label, .f = ~if_else(is.null(input[[.x]]), "xx", input[[.x]]))) %>%
                replace_na(list(composition = " ", other = "", device_name = " ")) %>%
                mutate(implant_statement = paste(glue("At the {level} interspace, a {height}mm height {composition}"), 
                                                 device_name,
                                                 if_else(other == "", glue(" "), glue(" ({other}) ")),
                                                 if_else(expandable == "Expandable", "expandable", " "), 
                                                 "implant", 
                                                 if_else(integrated_fixation == "Integrated Fixation", "with integrated fixation", " "),
                                                 "was selected.")) %>%
                mutate(implant_statement = str_squish(implant_statement)) %>%
                mutate(implant_statement = str_remove_all(string = implant_statement, pattern = "()")) %>% 
                mutate(integrated_fixation = if_else(integrated_fixation == "xx", "No integrated fixation", "Integrated Fixation")) %>%
                mutate(expandable = if_else(expandable == "xx", "Static", "Expandable")) %>%
                select(level, vertebral_number, approach, object, composition, device_name, height, integrated_fixation, expandable, other, implant_statement)
            
        }else{
            interbody_details_df <- tibble(level = character(), vertebral_number = double(), object = character(), approach = character(),  composition = character(), implant_statement = character())
        }
        
        
        interbody_details_df
        
    })
    
    output$interbody_details_table <- renderTable({
        interbody_details_df_reactive()
    })
    
    
    #################################################################### CONSTRUCT IMPLANTS, DECOMPRESSIONS AND PLOT     ####################################################################
    
    
    observeEvent(input$plot_click, {
        output$click_info <-renderText({
            
            paste("X = ", round(as.double(input$plot_click$x), 3), ".  Y = ", round(as.double(input$plot_click$y), 3))
        })
    })
    
    
    
    all_objects_to_add_list <- reactiveValues()
    
    all_objects_to_add_list$objects_df <- tibble(level = character(),
                                                 approach = character(),
                                                 category = character(),
                                                 vertebral_number = double(),
                                                 object = character(),
                                                 side = character(),
                                                 x = double(),
                                                 y = double(),
                                                 length = double(),
                                                 width = double(),
                                                 angle = double(),
                                                 left_x = double(), 
                                                 superior_y = double(),
                                                 right_x = double(),
                                                 inferior_y = double(),
                                                 object_constructed = list())
    
    
    observeEvent(input$reset_all, {
        all_objects_to_add_list$objects_df <- all_objects_to_add_list$objects_df %>%
            filter(level == "xxx")
    })
    
    
    ########################################### object DETAILS REACTIVE ###########################################
    
    objects_to_add_reactive_list <- reactiveValues()
    
    objects_to_add_reactive_list$object_to_add <- reactive({
        input$object_to_add
    })
    
    #### OBSERVE THE PLOT CLICK AND ADD APPROPRIATE object ####
    
    object_added_reactive_df <- reactive({
        
        if(input$object_to_add == "decompression_diskectomy_fusion" | input$object_to_add == "diskectomy_fusion"){
            object_type_filtered_df <- all_implants_constructed_df %>%
                filter(object == input$object_to_add) 
            
            fusion_df <- nearPoints(
                df = object_type_filtered_df,
                coordinfo = input$plot_click,
                xvar = "x",
                yvar = "y",
                maxpoints = 1,
                threshold = 25
            ) 
            
            implant_df <- fusion_df %>%
                union_all(fusion_df %>%
                              mutate(object = "anterior_interbody_implant", category = "anterior_interbody"))
            
        }else{
            object_type_filtered_df <- all_implants_constructed_df %>%
                filter(object == input$object_to_add) 
            
            implant_df <- nearPoints(
                df = object_type_filtered_df,
                coordinfo = input$plot_click,
                xvar = "x",
                yvar = "y",
                maxpoints = 1,
                threshold = 25
            ) 
        }
        
        implant_df
    })
    
    #### OBSERVE THE PLOT CLICK AND ADD APPROPRIATE object ####
    observeEvent(input$plot_click, {
        
        all_objects_to_add_list$objects_df <- all_objects_to_add_list$objects_df  %>%
            union_all(object_added_reactive_df()) %>%
            distinct()
        
        if(nrow(all_objects_to_add_list$objects_df %>% filter(category == "osteotomy")) > 1){
            
            all_objects_to_add_list$objects_df <- all_objects_to_add_list$objects_df %>%
                mutate(object_class = if_else(object %in% c("grade_1", "complete_facetectomy", "grade_2", "grade_3", "grade_4", "grade_5", "grade_6"), "osteotomy", object)) %>%
                mutate(object_rank = case_when(
                    object == "grade_1" ~ 1,
                    object == "complete_facetectomy" ~ 2,
                    object == "grade_2" ~ 3,
                    object == "grade_3" ~ 4,
                    object == "grade_4" ~ 5,
                    object == "grade_5" ~ 6,
                    object == "grade_6" ~ 7)
                ) %>%
                replace_na(list(object_rank = 1)) %>%
                # select(level, vertebral_number, approach, category, object, side, object_class, object_rank) %>%
                group_by(level, object_class) %>%
                filter(object_rank == max(object_rank)) %>%
                ungroup() %>%
                select(-object_rank, -object_class)
            
        }
    })
    
    observeEvent(input$plot_double_click, {
        
        object_type_filtered_df <- all_implants_constructed_df %>%
            filter(object == input$object_to_add) 
        
        implant_to_remove_df <- nearPoints(
            df = object_type_filtered_df,
            coordinfo = input$plot_double_click,
            xvar = "x",
            yvar = "y",
            maxpoints = 1,
            threshold = 20
        )
        
        all_objects_to_add_list$objects <- object_added_reactive_df () %>%
            union_all(all_objects_to_add_list$objects)
        
        all_objects_to_add_list$objects_df <- all_objects_to_add_list$objects_df %>%
            anti_join(implant_to_remove_df)
        
    })
    
    

    ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
    ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
    ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
    ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
    

    
    
    
    ############################################################ MAKE THE PLOT ###############################################################
    ############################################################ MAKE THE PLOT ###############################################################
    ############################################################ MAKE THE PLOT ###############################################################
    
    # output$spine_plan
    spine_plan_plot <- reactive({
        ## Create df for plan ##
        x_left_limit <- 0.3 - input$label_text_offset/100
        x_right_limit <- 1-x_left_limit

        plot_top_y <- input$crop_y[2]

        y_spacing <- 0.025*input$crop_y[2]

        y_start_with_text <- plot_top_y + nrow(plan_reactive_df())*y_spacing


        plan_table <- tibble(x = 0.5, y = y_start_with_text, tb = list(plan_reactive_df()))
        #
        ### anterior first, then lateral, then posterior

        if(input$spine_approach == "Anterior"){
            ## Anterior Objects
            all_anterior_objects_df <- all_objects_to_add_list$objects_df

            if(any(str_detect(string = all_anterior_objects_df$object, pattern = "anterior_disc_arthroplasty"))){

                anterior_disc_arthroplasty_df <- all_anterior_objects_df %>%
                    filter(object == "anterior_disc_arthroplasty") %>%
                    remove_empty() %>%
                    unnest()

            }else{
                # anterior_disc_arthroplasty_sf <- NULL
                anterior_disc_arthroplasty_df <- tibble(object_constructed = c(), color = c())
            }

            if(any(str_detect(string = all_anterior_objects_df$object, pattern = "anterior_buttress_plate"))){
                anterior_buttress_plate_sf <- st_multipolygon((all_anterior_objects_df %>% filter(object == "anterior_buttress_plate"))$object_constructed)
            }else{
                anterior_buttress_plate_sf <- NULL
            }

            if(any(str_detect(string = all_anterior_objects_df$object, pattern = "anterior_plate"))){
                anterior_plate_sf <- st_union(st_combine(st_multipolygon((all_anterior_objects_df %>% filter(object == "anterior_plate"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE)
            }else{
                anterior_plate_sf <- NULL
            }

            if(any(str_detect(string = all_anterior_objects_df$object, pattern = "corpectomy"))){
                corpectomy_sf <- st_union(st_combine(st_multipolygon((all_anterior_objects_df %>% filter(object == "corpectomy"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE)
            }else{
                corpectomy_sf <- NULL
            }

            if(any(str_detect(all_anterior_objects_df$object, pattern = "corpectomy_cage"))){
                corpectomy_cage_sf <- st_union(st_combine(st_multipolygon((all_anterior_objects_df %>% filter(object == "corpectomy_cage"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE)

            }else{
                corpectomy_cage_sf <- NULL
            }

            if(nrow(all_anterior_objects_df %>% filter(object == "diskectomy_fusion")) > 0){
                diskectomy_fusion_sf <- st_multipolygon((all_anterior_objects_df %>% filter(object == "diskectomy_fusion"))$object_constructed)
            }else{
                diskectomy_fusion_sf <- NULL
            }
            if(nrow(all_anterior_objects_df %>% filter(object == "decompression_diskectomy_fusion")) > 0){
                decompression_diskectomy_fusion_sf <- st_multipolygon((all_anterior_objects_df %>% filter(object == "decompression_diskectomy_fusion"))$object_constructed)
            }else{
                decompression_diskectomy_fusion_sf <- NULL
            }

            if(any(str_detect(string = all_anterior_objects_df$object, pattern = "diskectomy_fusion"))){

                anterior_fusions_selected_df <- all_anterior_objects_df %>%
                    filter(object == "diskectomy_fusion_no_interbody_device" | object == "diskectomy_fusion" | object == "decompression_diskectomy_fusion") %>%
                    distinct() %>%
                    select(-object_constructed) %>%
                    mutate(object = "diskectomy_fusion_no_interbody_device") %>%
                    left_join(anterior_objects_df) %>%
                    distinct()

                diskectomy_fusion_no_interbody_device_sf <- st_multipolygon(anterior_fusions_selected_df$object_constructed)
            }else{
                diskectomy_fusion_no_interbody_device_sf <- NULL
            }

            if(any(str_detect(string = all_anterior_objects_df$object, pattern = "screw_washer"))){
                screw_washer_sf <- st_multipolygon((all_anterior_objects_df %>% filter(object == "screw_washer"))$object_constructed)
                washer_df <- all_anterior_objects_df %>%
                    filter(object == "screw_washer") %>%
                    mutate(object_constructed = pmap(list(..1 = y), .f = ~ st_buffer(x = st_point(c(0.5, ..1)), dist = 0.005)))
                screw_washer_screw_sf <- st_multipolygon(washer_df$object_constructed)
            }else{
                screw_washer_sf <- NULL
                screw_washer_screw_sf <- NULL
            }

            ggdraw() +
                draw_image(
                    anterior_spine_jpg,
                    scale = 1,
                    y = 0,
                    valign = 0,
                    x = 0,
                    height = 1
                    # width = 1
                )  +
                draw_text(
                    text = labels_anterior_df$level,
                    x = x_left_limit + 0.05,
                    y = labels_anterior_df$y,
                    size = input$label_text_size,
                    fontface = "bold"
                ) +
                draw_text(
                    text = labels_anterior_df$level,
                    x = x_right_limit - 0.05,
                    y = labels_anterior_df$y,
                    size = input$label_text_size,
                    fontface = "bold"
                ) +
                ggpattern::geom_sf_pattern(
                    data =  diskectomy_fusion_no_interbody_device_sf,
                    pattern = "circle",
                    pattern_fill = "#F5A105",
                    color = "#F7B28F",
                    fill = "#A89E9E",
                    alpha = 0.6,
                    pattern_spacing = 0.005,
                    pattern_density = 0.7) +
                geom_sf(data = anterior_disc_arthroplasty_df,
                        aes(geometry = object_constructed, fill = color)) +
                ggpattern::geom_sf_pattern(
                    data =  decompression_diskectomy_fusion_sf,
                    pattern = "crosshatch",
                    pattern_fill = "grey90",
                    fill = "#7899F5",
                    alpha = 0.3,
                    pattern_spacing = 0.02,
                    pattern_density = 0.7
                ) +
                ggpattern::geom_sf_pattern(
                    data =  diskectomy_fusion_sf,
                    pattern = "crosshatch",
                    pattern_fill = "grey90",
                    fill = "#7899F5",
                    alpha = 0.3,
                    pattern_spacing = 0.02,
                    pattern_density = 0.7
                ) +
                ggpattern::geom_sf_pattern(
                    data =  corpectomy_sf,
                    pattern = "stripe",
                    pattern_colour = "red",
                    alpha = 0.6,
                    pattern_angle = 90,
                    pattern_spacing = 0.01,
                    pattern_density = 0.1
                ) +
                ggpattern::geom_sf_pattern(
                    data =  corpectomy_cage_sf,
                    pattern = "crosshatch",
                    pattern_fill = "grey90",
                    fill = "#7899F5",
                    alpha = 0.3,
                    pattern_spacing = 0.01,
                    pattern_density = 0.7
                ) +
                ggpattern::geom_sf_pattern(
                    data =  anterior_buttress_plate_sf,
                    pattern = "circle",
                    pattern_fill = "grey90",
                    fill = "#7899F5",
                    pattern_spacing = 0.01,
                    pattern_density = 0.7
                ) +
                ggpattern::geom_sf_pattern(
                    data =  anterior_plate_sf,
                    pattern = "circle",
                    pattern_fill = "grey60",
                    fill = "#3B86CC",
                    pattern_spacing = 0.01,
                    pattern_density = 0.7
                ) +
                geom_sf(data = screw_washer_sf, fill = "#3B86CC") +
                geom_sf(data = screw_washer_screw_sf, fill = "black") +
                geom_table(data = plan_table, aes(label = tb, x = x, y = y), size = (input$label_text_size - 3)/2.85, table.colnames = FALSE) +
                ylim(input$crop_y[1], y_start_with_text) +
                xlim(x_left_limit, x_right_limit) +
                scale_fill_identity()

        }else{
            ### POSTERIOR
            all_posterior_objects_df <- all_objects_to_add_list$objects_df %>%
                filter(approach == "posterior")

            ##########RODS ############
            ############# Left ROD #################
            left_rods_connectors_list <- list()

            # if(nrow(left_rod_implants_df_reactive()) > 0){
            if(input$add_left_accessory_rod == TRUE){
                accessory_vector <- input$left_accessory_rod
            }else{
                accessory_vector <- c("a", "b")
            }
            if(input$add_left_satellite_rod == TRUE){
                satellite_vector <- input$left_satellite_rod
            }else{
                satellite_vector <- c("a", "b")
            }
            if(input$add_left_intercalary_rod == TRUE){
                intercalary_vector <- input$left_intercalary_rod
                junction <- input$left_intercalary_junction
            }else{
                intercalary_vector <- c("a", "b")
                junction <- NULL
            }
            if(input$add_left_linked_rods == TRUE){
                linked_vector <- input$left_linked_rods
            }else{
                linked_vector <- c("a", "b")
            }

            left_rods_connectors_list <- build_unilateral_rods_list_function(accessory_rod_vector = accessory_vector,
                                                                             satellite_rods_vector = satellite_vector,
                                                                             intercalary_rods_vector = intercalary_vector,
                                                                             intercalary_junction = junction,
                                                                             linked_rods_vector = linked_vector,
                                                                             unilateral_full_implant_df = left_rod_implants_df_reactive())
            if(length(left_rods_connectors_list$rod_list) > 0){
                left_rod_list_sf <- st_multipolygon(left_rods_connectors_list$rod_list)
            }else{
                left_rod_list_sf <- NULL
            }
            if(length(left_rods_connectors_list$connector_list) > 0){
                left_connector_list_sf <- st_multipolygon(left_rods_connectors_list$connector_list)
            }else{
                left_connector_list_sf <- NULL
            }
            ############# RIGHT ROD #################

            right_rods_connectors_list <- list()
            if(input$add_right_accessory_rod == TRUE){
                accessory_vector <- input$right_accessory_rod
            }else{
                accessory_vector <- c("a", "b")
            }
            if(input$add_right_satellite_rod == TRUE){
                satellite_vector <- input$right_satellite_rod
            }else{
                satellite_vector <- c("a", "b")
            }
            if(input$add_right_intercalary_rod == TRUE){
                intercalary_vector <- input$right_intercalary_rod
                junction <- input$right_intercalary_junction
            }else{
                intercalary_vector <- c("a", "b")
                junction <- NULL
            }
            if(input$add_right_linked_rods == TRUE){
                linked_vector <- input$right_linked_rods
            }else{
                linked_vector <- c("a", "b")
            }

            right_rods_connectors_list <- build_unilateral_rods_list_function(accessory_rod_vector = accessory_vector,
                                                                             satellite_rods_vector = satellite_vector,
                                                                             intercalary_rods_vector = intercalary_vector,
                                                                             intercalary_junction = junction,
                                                                             linked_rods_vector = linked_vector,
                                                                             unilateral_full_implant_df = right_rod_implants_df_reactive())
            if(length(right_rods_connectors_list$rod_list) > 0){
                right_rod_list_sf <- st_multipolygon(right_rods_connectors_list$rod_list)
            }else{
                right_rod_list_sf <- NULL
            }
            if(length(right_rods_connectors_list$connector_list) > 0){
                right_connector_list_sf <- st_multipolygon(right_rods_connectors_list$connector_list)
            }else{
                right_connector_list_sf <- NULL
            }


            if((length(input$left_revision_implants) > 1 && (input$primary_revision == "Revision"))){
                left_revision_implants_constructed_df <- tibble(level = input$left_revision_implants) %>%
                    left_join(revision_implants_df %>% filter(x < 0.5))

                left_revision_rod_matrix <- left_revision_implants_constructed_df %>%
                    select(x, y) %>%
                    arrange(rev(y)) %>%
                    distinct() %>%
                    as.matrix()
                left_revision_implants_sf <- st_multipolygon(left_revision_implants_constructed_df$object_constructed)
                left_revision_rod_sf <- st_buffer(st_linestring(left_revision_rod_matrix), dist = 0.003, endCapStyle = "ROUND")

                left_revision_implants_rod_sf <- st_geometrycollection(x = list(left_revision_implants_sf, left_revision_rod_sf))
            }else{
                left_revision_implants_rod_sf <- NULL
            }
            if((length(input$right_revision_implants) > 1 && (input$primary_revision == "Revision"))){
                right_revision_implants_constructed_df <- tibble(level = input$right_revision_implants) %>%
                    left_join(revision_implants_df %>% filter(x > 0.5))

                right_revision_rod_matrix <- right_revision_implants_constructed_df %>%
                    select(x, y) %>%
                    arrange(rev(y)) %>%
                    distinct() %>%
                    as.matrix()
                right_revision_implants_sf <- st_multipolygon(right_revision_implants_constructed_df$object_constructed)
                right_revision_rod_sf <- st_buffer(st_linestring(right_revision_rod_matrix), dist = 0.003, endCapStyle = "ROUND")

                right_revision_implants_rod_sf <- st_geometrycollection(x = list(right_revision_implants_sf, right_revision_rod_sf))
            }else{
                right_revision_implants_rod_sf <- NULL
            }

            if(length(input$open_canal) > 0){
                open_df <- open_canal_df %>%
                    filter(level %in% input$open_canal)
                open_canal_sf <- st_union(st_combine(st_multipolygon(open_df$object_constructed)), by_feature = TRUE, is_coverage = TRUE)
            }else{
                open_canal_sf <- NULL
            }
            ## SCREWS
            if(any(str_detect((all_posterior_objects_df$object), pattern = "screw"))){
                screws_sf <- st_multipolygon((all_posterior_objects_df %>% filter(str_detect(string = object, pattern = "screw")))$object_constructed)
            }else{
                screws_sf <- NULL
            }

            ## HOOKS
            if(any(str_detect(all_posterior_objects_df$object, pattern = "hook"))){
                hook_df <- all_posterior_objects_df %>%
                    filter(str_detect(string = object, pattern = "hook"))

                hooks_sf <- st_multipolygon(hook_df$object_constructed)
            }else{
                hooks_sf <- NULL
            }
            ## SUBLAMINAR BANDS
            if(any(str_detect(all_posterior_objects_df$object, pattern = "wire"))){
                sublaminar_wires_sf <- st_multipolygon((all_posterior_objects_df %>% filter(str_detect(string = object, pattern = "wire")))$object_constructed)
            }else{
                sublaminar_wires_sf <- NULL
            }

            ## Tethers
            if(any(str_detect(all_posterior_objects_df$object, pattern = "tether"))){
                tethers_sf <- st_multipolygon((all_posterior_objects_df %>% filter(str_detect(string = object, pattern = "tether")))$object_constructed)
            }else{
                tethers_sf <- NULL
            }

            ## cement_augmentation
            if(any(str_detect(all_posterior_objects_df$object, pattern = "cement_augmentation"))){
                cement_augmentation_sf <- st_multipolygon((all_posterior_objects_df %>% filter(str_detect(string = object, pattern = "cement_augmentation")))$object_constructed)
            }else{
                cement_augmentation_sf <- NULL
            }

            ## OSTEOTOMIES
            if(any(str_detect(all_posterior_objects_df$object, pattern = "grade_1"))){
                osteotomy_1_sf <- st_geometrycollection((all_posterior_objects_df %>% filter(object == "grade_1"))$object_constructed)
            }else{
                osteotomy_1_sf <- NULL
            }
            if(any(str_detect(all_posterior_objects_df$object, pattern = "complete_facetectomy"))){
                osteotomy_facetectomy_sf <- st_multipolygon((all_posterior_objects_df %>% filter(object == "complete_facetectomy"))$object_constructed)
            }else{
                osteotomy_facetectomy_sf <- NULL
            }
            if(any(str_detect(all_posterior_objects_df$object, pattern = "grade_2"))){
                osteotomy_2_sf <-  st_union(st_combine(st_multipolygon((all_posterior_objects_df %>% filter(object == "grade_2"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE)
            }else{
                osteotomy_2_sf <- NULL
            }
            if(any(str_detect(all_posterior_objects_df$object, pattern = "grade_3"))){
                osteotomy_3_sf <- st_union(st_combine(st_multipolygon((all_posterior_objects_df %>% filter(object == "grade_3"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE)

            }else{
                osteotomy_3_sf <- NULL
            }
            if(any(str_detect(all_posterior_objects_df$object, pattern = "grade_4"))){
                osteotomy_4_sf <- st_union(st_combine(st_multipolygon((all_posterior_objects_df %>% filter(object == "grade_4"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE)
            }else{
                osteotomy_4_sf <- NULL
            }
            if(any(str_detect(all_posterior_objects_df$object, pattern = "grade_5"))){
                osteotomy_5_sf <- st_union(st_combine(st_multipolygon((all_posterior_objects_df %>% filter(object == "grade_5"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE)
            }else{
                osteotomy_5_sf <- NULL
            }


            ## Decompresions ##
            if(any(str_detect(all_posterior_objects_df$object, pattern = "laminectomy"))){
                laminectomy_sf <- st_union(st_combine(st_multipolygon((all_posterior_objects_df %>% filter(object == "laminectomy"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE)
            }else{
                laminectomy_sf <- NULL
            }
            if(any(str_detect(all_posterior_objects_df$object, pattern = "sublaminar_decompression"))){
                sublaminar_decompression_sf <- st_multipolygon((all_posterior_objects_df %>% filter(object == "sublaminar_decompression"))$object_constructed)
            }else{
                sublaminar_decompression_sf <- NULL
            }
            if(any(str_detect(all_posterior_objects_df$object, pattern = "laminotomy"))){
                laminotomy_sf <- st_multipolygon((all_posterior_objects_df %>% filter(object == "laminotomy"))$object_constructed)
            }else{
                laminotomy_sf <- NULL
            }
            if(any(str_detect(all_posterior_objects_df$object, pattern = "laminoplasty"))){
                laminoplasty_sf <- st_multipolygon((all_posterior_objects_df %>% filter(object == "laminoplasty"))$object_constructed)
                laminoplasty_cut_df <- tibble(x = 0.515, y =  c(max((all_posterior_objects_df %>% filter(object == "laminoplasty"))$superior_lamina_y), min((all_posterior_objects_df %>% filter(object == "laminoplasty"))$inferior_lamina_y) - 0.007))
            }else{
                laminoplasty_sf <- NULL
                laminoplasty_cut_df <- tibble(x = numeric(), y = numeric())
            }
            if(any(str_detect(all_posterior_objects_df$object, pattern = "transpedicular_approach"))){
                transpedicular_sf <- st_multipolygon((all_posterior_objects_df %>% filter(object == "transpedicular_approach"))$object_constructed)
            }else{
                transpedicular_sf <- NULL
            }
            if(any(str_detect(all_posterior_objects_df$object, pattern = "costo"))){
                costovertebral_sf <- st_multipolygon((all_posterior_objects_df %>% filter(object == "costovertebral_approach" | object == "costotransversectomy"))$object_constructed)
            }else{
                costovertebral_sf <- NULL
            }

            if(any(all_posterior_objects_df$object == "diskectomy")){
                diskectomy_sf <- st_multipolygon((all_posterior_objects_df %>% filter(object == "diskectomy"))$object_constructed)
            }else{
                diskectomy_sf <- NULL
            }

            ## INTERBODY
            if(any(all_posterior_objects_df$object == "tlif") || any(all_posterior_objects_df$object == "llif") ||any(all_posterior_objects_df$object == "plif")){
                interbody_device_sf <- st_multipolygon((all_posterior_objects_df %>% filter(object == "tlif" | object == "plif" | object == "llif"))$object_constructed)
            }else{
                interbody_device_sf <- NULL
            }

            if(any(all_posterior_objects_df$object == "no_implant_interbody_fusion")){
                no_implant_interbody_fusion_sf <- st_multipolygon((all_posterior_objects_df %>% filter(object == "no_implant_interbody_fusion"))$object_constructed)
            }else{
                no_implant_interbody_fusion_sf <- NULL
            }


            if(any(str_detect(all_posterior_objects_df$object, pattern = "intervertebral_cage"))){
                intervertebral_cage_sf <- st_union(st_combine(st_multipolygon((all_posterior_objects_df %>% filter(object == "intervertebral_cage"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE)

            }else{
                intervertebral_cage_sf <- NULL
            }


            # all_rods_sf <- st_geometrycollection(all_rods_reactive_list())

            if(input$plot_with_patterns_true == TRUE){
                screws_geom <- ggpattern::geom_sf_pattern(
                    data = screws_sf,
                    pattern = "stripe",
                    pattern_fill = "blue",
                    pattern_fill2 = "#445566",
                    alpha = 0.8,
                    pattern_colour = "blue",
                    pattern_density = 0.02,
                    pattern_spacing = 0.01,
                    pattern_angle = 80
                )
            }else{
                screws_geom <- geom_sf(data = screws_sf, fill = "blue")
            }
            
            if(input$lumbar_vertebrae_6 == TRUE){
                l6_statement <- "Note: 6 Lumbar Vertebrae"
            }else{
                l6_statement <- " "
            }
            
            # ggdraw() +
            #     draw_image(
            #         spine_png,
            #         scale = 1,
            #         y = 0,
            #         valign = 0,
            #         x = 0,
            #         height = 1
            #         # width = 1
            #     ) +
            #     draw_text(
            #         text = labels_df$level,
            #         x = x_left_limit + 0.05,
            #         y = labels_df$y,
            #         size = input$label_text_size,
            #         fontface = "bold"
            #     ) +
            #     draw_text(
            #         text = labels_df$level,
            #         x = x_right_limit - 0.05,
            #         y = labels_df$y,
            #         size = input$label_text_size,
            #         fontface = "bold"
            #     ) 
                figure_list <- list(geom_sf_pattern(data = cement_augmentation_sf,
                                pattern = "plasma",
                                pattern_alpha = 0.5,
                                alpha = 0.3,
                                color = "grey96"),
                ggpattern::geom_sf_pattern(
                    data =  open_canal_sf,
                    pattern_orientation = "radial",
                    pattern = "gradient",
                    fill = "grey50",
                    pattern_fill2 = NA,
                    colour = NA),
                geom_sf(data = left_revision_implants_rod_sf, fill = "black"),
                geom_sf(data = right_revision_implants_rod_sf, fill = "black"),
                geom_sf(data = no_implant_interbody_fusion_sf, fill = "#E66565"),
                geom_sf(data = interbody_device_sf, fill = "red"),
                ggpattern::geom_sf_pattern(
                    data = laminectomy_sf,
                    pattern = "stripe",
                    pattern_colour = "red",
                    alpha = 0.7,
                    pattern_spacing = 0.01
                ),
                ggpattern::geom_sf_pattern(
                    data = sublaminar_decompression_sf,
                    pattern = "stripe",
                    pattern_colour = "red",
                    alpha = 0.7,
                    pattern_spacing = 0.01
                ),
                ggpattern::geom_sf_pattern(
                    data = laminotomy_sf,
                    pattern = "stripe",
                    pattern_colour = "red",
                    alpha = 0.7,
                    pattern_spacing = 0.01
                ),
                ggpattern::geom_sf_pattern(
                    data = transpedicular_sf,
                    pattern = "stripe",
                    pattern_colour = "red",
                    alpha = 0.7,
                    pattern_spacing = 0.01
                ),
                ggpattern::geom_sf_pattern(
                    data = costovertebral_sf,
                    pattern = "stripe",
                    pattern_colour = "red",
                    alpha = 0.7,
                    pattern_spacing = 0.01
                ),
                geom_sf(data = diskectomy_sf, fill = "black", alpha = 0.7),
                geom_sf(data = osteotomy_1_sf, color = "red", size = 1),
                ggpattern::geom_sf_pattern(
                    data = osteotomy_facetectomy_sf,
                    pattern = "stripe",
                    pattern_colour = "red",
                    alpha = 0.5,
                    pattern_angle = 10,
                    pattern_spacing = 0.01,
                    pattern_density = 0.15,
                ),
                ggpattern::geom_sf_pattern(
                    data = osteotomy_2_sf,
                    pattern = "stripe",
                    pattern_colour = "red",
                    alpha = 0.5,
                    pattern_angle = 10,
                    pattern_spacing = 0.01,
                    pattern_density = 0.15,
                ),
                ggpattern::geom_sf_pattern(
                    data = osteotomy_3_sf,
                    pattern = "stripe",
                    pattern_colour = "red",
                    alpha = 0.6,
                    pattern_angle = 10,
                    pattern_spacing = 0.03,
                    pattern_density = 0.1,
                ),
                ggpattern::geom_sf_pattern(
                    data = osteotomy_4_sf,
                    pattern = "stripe",
                    pattern_colour = "red",
                    alpha = 0.6,
                    pattern_angle = 10,
                    pattern_spacing = 0.02,
                    pattern_density = 0.02,
                ),
                ggpattern::geom_sf_pattern(
                    data = osteotomy_5_sf,
                    pattern = "crosshatch",
                    pattern_colour = "red",
                    alpha = 0.6,
                    pattern_angle = 10,
                    pattern_spacing = 0.02,
                    pattern_density = 0.02,
                ),
                ggpattern::geom_sf_pattern(
                    data =  intervertebral_cage_sf,
                    pattern = "crosshatch",
                    pattern_fill = "grey90",
                    fill = "#7899F5",
                    alpha = 0.3,
                    pattern_spacing = 0.01,
                    pattern_density = 0.7
                ),
                screws_geom,
                geom_sf(data = laminoplasty_sf, fill  = "blue"),
                geom_line(data = laminoplasty_cut_df, aes(x = x, y = y), linetype = "dotted", size = 2, color = "red"),
                ggpattern::geom_sf_pattern(
                    data = hooks_sf,
                    pattern = "gradient",
                    pattern_fill = "blue",
                    pattern_fill2 = "#445566",
                    alpha = 0.8
                ),
                ggpattern::geom_sf_pattern(
                    data = sublaminar_wires_sf,
                    pattern = "gradient",
                    pattern_fill = "red",
                    pattern_fill2 = "#445566",
                    alpha = 0.8
                ),
                geom_sf(data = tethers_sf),
                geom_sf(data = left_rod_list_sf, alpha = 0.75),
                geom_sf(data = left_connector_list_sf, alpha = 0.75),
                geom_sf(data = right_rod_list_sf, alpha = 0.75),
                geom_sf(data = right_connector_list_sf, alpha = 0.75),
                geom_table(data = plan_table, aes(label = tb, x = x, y = y), size = (input$label_text_size - 3)/2.85, table.colnames = FALSE))
                # ,
                # annotate("text", x = 0.65, y = input$crop_y[1], 0.01, label = l6_statement),
                # ylim(input$crop_y[1], y_start_with_text),
                # xlim(x_left_limit, x_right_limit))


            # ggdraw() +
            #     draw_image(
            #         spine_png,
            #         scale = 1,
            #         y = 0,
            #         valign = 0,
            #         x = 0,
            #         height = 1
            #         # width = 1
            #     ) +
            #     draw_text(
            #         text = labels_df$level,
            #         x = x_left_limit + 0.05,
            #         y = labels_df$y,
            #         size = input$label_text_size,
            #         fontface = "bold"
            #     ) +
            #     draw_text(
            #         text = labels_df$level,
            #         x = x_right_limit - 0.05,
            #         y = labels_df$y,
            #         size = input$label_text_size,
            #         fontface = "bold"
            #     ) +
            #     geom_sf_pattern(data = cement_augmentation_sf,
            #                     pattern = "plasma",
            #                     pattern_alpha = 0.5,
            #                     alpha = 0.3,
            #                     color = "grey96") +
            #     ggpattern::geom_sf_pattern(
            #         data =  open_canal_sf,
            #         pattern_orientation = "radial",
            #         pattern = "gradient",
            #         fill = "grey50",
            #         pattern_fill2 = NA,
            #         colour = NA) +
            #     geom_sf(data = left_revision_implants_rod_sf, fill = "black") +
            #     geom_sf(data = right_revision_implants_rod_sf, fill = "black") +
            #     geom_sf(data = no_implant_interbody_fusion_sf, fill = "#E66565") +
            #     geom_sf(data = interbody_device_sf, fill = "red") +
            #     ggpattern::geom_sf_pattern(
            #         data = laminectomy_sf,
            #         pattern = "stripe",
            #         pattern_colour = "red",
            #         alpha = 0.7,
            #         pattern_spacing = 0.01
            #     ) +
            #     ggpattern::geom_sf_pattern(
            #         data = sublaminar_decompression_sf,
            #         pattern = "stripe",
            #         pattern_colour = "red",
            #         alpha = 0.7,
            #         pattern_spacing = 0.01
            #     ) +
            #     ggpattern::geom_sf_pattern(
            #         data = laminotomy_sf,
            #         pattern = "stripe",
            #         pattern_colour = "red",
            #         alpha = 0.7,
            #         pattern_spacing = 0.01
            #     ) +
            #     ggpattern::geom_sf_pattern(
            #         data = transpedicular_sf,
            #         pattern = "stripe",
            #         pattern_colour = "red",
            #         alpha = 0.7,
            #         pattern_spacing = 0.01
            #     ) +
            #     ggpattern::geom_sf_pattern(
            #         data = costovertebral_sf,
            #         pattern = "stripe",
            #         pattern_colour = "red",
            #         alpha = 0.7,
            #         pattern_spacing = 0.01
            #     ) +
            #     geom_sf(data = diskectomy_sf, fill = "black", alpha = 0.7) +
            #     geom_sf(data = osteotomy_1_sf, color = "red", size = 1) +
            #     ggpattern::geom_sf_pattern(
            #         data = osteotomy_facetectomy_sf,
            #         pattern = "stripe",
            #         pattern_colour = "red",
            #         alpha = 0.5,
            #         pattern_angle = 10,
            #         pattern_spacing = 0.01,
            #         pattern_density = 0.15,
            #     ) +
            #     ggpattern::geom_sf_pattern(
            #         data = osteotomy_2_sf,
            #         pattern = "stripe",
            #         pattern_colour = "red",
            #         alpha = 0.5,
            #         pattern_angle = 10,
            #         pattern_spacing = 0.01,
            #         pattern_density = 0.15,
            #     ) +
            #     ggpattern::geom_sf_pattern(
            #         data = osteotomy_3_sf,
            #         pattern = "stripe",
            #         pattern_colour = "red",
            #         alpha = 0.6,
            #         pattern_angle = 10,
            #         pattern_spacing = 0.03,
            #         pattern_density = 0.1,
            #     ) +
            #     ggpattern::geom_sf_pattern(
            #         data = osteotomy_4_sf,
            #         pattern = "stripe",
            #         pattern_colour = "red",
            #         alpha = 0.6,
            #         pattern_angle = 10,
            #         pattern_spacing = 0.02,
            #         pattern_density = 0.02,
            #     ) +
            #     ggpattern::geom_sf_pattern(
            #         data = osteotomy_5_sf,
            #         pattern = "crosshatch",
            #         pattern_colour = "red",
            #         alpha = 0.6,
            #         pattern_angle = 10,
            #         pattern_spacing = 0.02,
            #         pattern_density = 0.02,
            #     ) +
            #     ggpattern::geom_sf_pattern(
            #         data =  intervertebral_cage_sf,
            #         pattern = "crosshatch",
            #         pattern_fill = "grey90",
            #         fill = "#7899F5",
            #         alpha = 0.3,
            #         pattern_spacing = 0.01,
            #         pattern_density = 0.7
            #     ) +
            #     screws_geom +
            #     geom_sf(data = laminoplasty_sf, fill  = "blue") +
            #     geom_line(data = laminoplasty_cut_df, aes(x = x, y = y), linetype = "dotted", size = 2, color = "red") +
            #     ggpattern::geom_sf_pattern(
            #         data = hooks_sf,
            #         pattern = "gradient",
            #         pattern_fill = "blue",
            #         pattern_fill2 = "#445566",
            #         alpha = 0.8
            #     ) +
            #     ggpattern::geom_sf_pattern(
            #         data = sublaminar_wires_sf,
            #         pattern = "gradient",
            #         pattern_fill = "red",
            #         pattern_fill2 = "#445566",
            #         alpha = 0.8
            #     ) +
            #     geom_sf(data = tethers_sf) +
            #     geom_sf(data = left_rod_list_sf, alpha = 0.75) +
            #     geom_sf(data = left_connector_list_sf, alpha = 0.75) +
            #     geom_sf(data = right_rod_list_sf, alpha = 0.75) +
            #     geom_sf(data = right_connector_list_sf, alpha = 0.75) +
            #     geom_table(data = plan_table, aes(label = tb, x = x, y = y), size = (input$label_text_size - 3)/2.85, table.colnames = FALSE) +
            #     annotate("text", x = 0.65, y = input$crop_y[1] + 0.01, label = l6_statement) +
            #     ylim(input$crop_y[1], y_start_with_text) +
            #     xlim(x_left_limit, x_right_limit)

        }
        
        figure_list
    })

    
    # 
    # 
    # 
    ##################### REACTIVE UI AND FUNCTION TO GENERATE INPUT NAMES AND RETRIEVE VALUES ##################
    ##################### REACTIVE UI AND FUNCTION TO GENERATE INPUT NAMES AND RETRIEVE VALUES ##################
    ##################### REACTIVE UI AND FUNCTION TO GENERATE INPUT NAMES AND RETRIEVE VALUES ##################
    
    
    
    output$spine_plan <- renderPlot({
        # spine_plan_plot()
        
        x_left_limit <- 0.3 - input$label_text_offset/100
        x_right_limit <- 1-x_left_limit
        
        plot_top_y <- input$crop_y[2]
        
        y_spacing <- 0.025*input$crop_y[2]
        
        y_start_with_text <- plot_top_y + nrow(plan_reactive_df())*y_spacing
        
        ggdraw() +
            draw_image(
                spine_png,
                scale = 1,
                y = 0,
                valign = 0,
                x = 0,
                height = 1
                # width = 1
            ) +
            draw_text(
                text = labels_df$level,
                x = x_left_limit + 0.05,
                y = labels_df$y,
                size = input$label_text_size,
                fontface = "bold"
            ) +
            draw_text(
                text = labels_df$level,
                x = x_right_limit - 0.05,
                y = labels_df$y,
                size = input$label_text_size,
                fontface = "bold"
            ) +
            spine_plan_plot() +
            ylim(input$crop_y[1], y_start_with_text)+
            xlim(x_left_limit, x_right_limit)
    })
    
    # observeEvent(input$lumbar_vertebrae_6, {
    #     output$spine_plan <- renderPlot({
    #         spine_plan_plot()
    #     })
    # }) 
    # 
    # output$spine_plot_for_implants_tab <- renderPlot({
    #      spine_plan_plot()
    # })
    
    
    ##########################################  MAKE UI's FOR screw ##################### ##################### 
    ##########################################  MAKE UI's FOR screw ##################### ##################### 
    
    make_screw_sizes_ui_function <-  function(level = NULL, left_screw_level = "no_screw", right_screw_level = "no_screw", left_selected = "Unknown", right_selected = "Unknown"){
        if(left_screw_level != "no_screw"){
            left_diameter <- numericInput(inputId = glue("left_{str_to_lower(level)}_screw_diameter"), label = NULL, value = NULL, min = 1, max = 12, step = 0.5
            ) 
            left_length <- numericInput(inputId = glue("left_{str_to_lower(level)}_screw_length"), label = NULL, value = NULL, min = 1, max = 140, step = 5
            ) 
        }else{
            left_diameter <- NULL
            left_length <- NULL
        }
        if(right_screw_level != "no_screw"){
            right_diameter <- numericInput(inputId = glue("right_{str_to_lower(level)}_screw_diameter"), label = NULL, value = NULL, min = 1, max = 12, step = 0.5
            ) 
            right_length <- numericInput(inputId = glue("right_{str_to_lower(level)}_screw_length"), label = NULL, value = NULL, min = 1, max = 140, step = 5
            ) 
        }else{
            right_diameter <- NULL
            right_length <- NULL
        }
        
        tags$tr(width = "100%", 
                tags$td(width = "10%", div(style = "font-size:14px; font-weight:bold; text-align:center; padding-bottom:10px", paste(level))),
                tags$td(width = "10%", div(
                                           left_diameter
                )
                ),
                tags$td(width = "10%", div(
                                           left_length
                )
                ),
                tags$td(width = "10%", div( 
                                           right_diameter
                )
                ),
                tags$td(width = "10%", div(
                                           right_length
                )
                )
        )
        
    }
    ##########################################  MAKE UI's FOR screw type ##################### ##################### 
    ##########################################  MAKE UI's FOR screw type ##################### ##################### 
    make_screw_types_function <-  function(level = NULL, left_screw_level = "no_screw", right_screw_level = "no_screw", left_selected = "P", right_selected = "P"){
        if(left_screw_level != "no_screw"){
            left_ui <- radioGroupButtons(   #"option2",
                inputId = glue("left_{str_to_lower(level)}_screw_type"),
                label = NULL,
                choices = c("M", "U", "P", "Red", "Offset"),
                selected = left_selected,
                checkIcon = list(yes = icon("wrench")),
                size = "xs",
                justified = TRUE,
                width = "95%"
            )
        }else{
            left_ui <- NULL
        }
        if(right_screw_level != "no_screw"){
            right_ui <- radioGroupButtons(   #"option2",
                inputId = glue("right_{str_to_lower(level)}_screw_type"),
                label = NULL,
                choices = c("M", "U", "P", "Red", "Offset"),
                selected = right_selected,
                checkIcon = list(yes = icon("wrench")),
                justified = TRUE,
                width = "95%",
                size = "xs"
            )
        }else{
            right_ui <- NULL
        }
        
        tags$tr(width = "100%", 
                tags$td(width = "7%", div(style = "font-size:14px; font-weight:bold; text-align:center; padding-bottom:10px",   paste(level))),
                tags$td(width = "45%",
                        left_ui
                        # div(id = "my_small_button_input",
                        #     left_ui)
                ),
                tags$td(width = "45%",
                        right_ui
                        # div(id = "my_small_button_input",
                        #     right_ui)
                )
        )
        
    }
    
    ##########################################
    output$pedicle_screw_details_ui <- renderUI({
        
        all_implants <- all_objects_to_add_list$objects_df %>%
            select(level, vertebral_number, object, side) %>%
            distinct() %>%
            filter(str_detect(object, "screw")) %>%
            arrange(vertebral_number)
        
        
        if(nrow(all_implants) > 0){
            implants_wide_df <- all_implants %>%
                mutate(level_side = str_to_lower(paste(level, side, object, sep = "_"))) %>%
                select(level, level_side, side) %>%
                pivot_wider(names_from = side, values_from = level_side) %>%
                mutate(across(everything(), ~ replace_na(.x, "no_screw"))) 
            
            # df_names <- glue_collapse(names(implants_wide_df), sep = " ")
            
            level_vector <- implants_wide_df$level
            
            ## the data frame can't have any missing values, otherwise the vectors will be of different lengths when you run map()
            
            if(any(names(implants_wide_df) == "left")){
                left_vector <- implants_wide_df$left
                
            }else{
                left_vector <-(implants_wide_df %>% mutate(left = "no_screw"))$left
            }
            if(any(names(implants_wide_df) == "right")){
                right_vector <- implants_wide_df$right
            }else{
                right_vector <-(implants_wide_df %>% mutate(right = "no_screw"))$right
            }
            
            max_levels <- nrow(implants_wide_df)     
            levels_count <- seq(from = 1, to = max_levels, by = 1)
            
            column(width = 12,
                   # div(style = "font-size:20px; font-weight:bold; text-align:center", "Screw Details:")
                   h4("Screw Sizes:"),
                   tags$table(
                       tags$tr(width = "100%",
                               tags$th(width = "10%", div(style = "font-size:14px; font-weight:bold; text-align:center",  "Level")),
                               tags$th(width = "10%", div(style = "font-size:14px; font-weight:bold; text-align:center",  "Left Screw Diameter")),
                               tags$th(width = "10%", div(style = "font-size:14px; font-weight:bold; text-align:center",  "Left Screw Length")),
                               tags$th(width = "10%", div(style = "font-size:14px; font-weight:bold; text-align:center",  "Right Screw Diameter")),
                               tags$th(width = "10%", div(style = "font-size:14px; font-weight:bold; text-align:center",  "Right Screw Length"))
                       ),
                       pmap(.l = list(..1 = level_vector,
                                      ..2 = left_vector,
                                      ..3 = right_vector, 
                                      ..4 = levels_count),
                            .f = ~make_screw_sizes_ui_function(level = ..1, 
                                                               left_screw_level = ..2, 
                                                               right_screw_level = ..3 
                                                               # left_selected = input[[left_vector[..4]]],   ## using this subsetting
                                                               # right_selected = input[[right_vector[..4]]]
                            ))
                   )
            )
            
        }else{
            # fixedRow()
            NULL
        }
        
        
    })
    
    output$pedicle_screw_types_ui <- renderUI({
        
        all_implants <- all_objects_to_add_list$objects_df %>%
            filter(approach == "posterior") %>%
            select(level, vertebral_number, object, side) %>%
            distinct() %>%
            filter(str_detect(object, "screw")) %>%
            arrange(vertebral_number)
        
        
        if(nrow(all_implants) > 0){
            implants_wide_df <- all_implants %>%
                mutate(level_side = paste(level, side, object, sep = "_")) %>%
                select(level, level_side, side) %>%
                pivot_wider(names_from = side, values_from = level_side) %>%
                mutate(across(everything(), ~ replace_na(.x, "no_screw"))) 
            
            df_names <- glue_collapse(names(implants_wide_df), sep = " ")
            
            level_vector <- implants_wide_df$level
            
            ## the data frame can't have any missing values, otherwise the vectors will be of different lengths when you run map()
            
            if(any(names(implants_wide_df) == "left")){
                left_vector <- implants_wide_df$left
                
            }else{
                left_vector <-(implants_wide_df %>% mutate(left = "no_screw"))$left
            }
            if(any(names(implants_wide_df) == "right")){
                right_vector <- implants_wide_df$right
            }else{
                right_vector <-(implants_wide_df %>% mutate(right = "no_screw"))$right
            }
            
            max_levels <- nrow(implants_wide_df)     
            levels_count <- seq(from = 1, to = max_levels, by = 1)
            
            column(width = 12,
                   tags$hr(),
                   h4("Screw Types:"),
                   tags$table(
                       tags$tr(width = "100%",
                               tags$th(width = "10%", div(style = "font-size:14px; font-weight:bold; text-align:center",  "Level")),
                               tags$th(width = "45%", div(style = "font-size:14px; font-weight:bold; text-align:center",  "Left Screw Type")),
                               tags$th(width = "45%", div(style = "font-size:14px; font-weight:bold; text-align:center",  "Right Screw Type"))
                       ),
                       pmap(.l = list(..1 = level_vector,
                                      ..2 = left_vector,
                                      ..3 = right_vector, 
                                      ..4 = levels_count),
                            .f = ~make_screw_types_function(level = ..1, 
                                                            left_screw_level = ..2, 
                                                            right_screw_level = ..3 
                                                            # left_selected = input[[left_vector[..4]]],   ## using this subsetting
                                                            # right_selected = input[[right_vector[..4]]]
                            )), 
                       tags$tr(width = "100%", div(style = "font-size:12px; text-align:right",  "U= Uniaxial, M = Monoaxial, P = Polyaxial, Red = Reduction")),
                   )
            )
        }else{
            # fixedRow()
            NULL
        }
        
        
    })
    
    

    screw_labels_reactive_df <- reactive({
        
        if(nrow(all_objects_to_add_list$objects_df %>% filter(str_detect(object, "screw"))) > 0){
            screws_df <-  all_objects_to_add_list$objects_df %>%
                mutate(level_side = paste(level, side, object, sep = "_")) %>%
                select(vertebral_number, level, side, level_side, object) %>%
                filter(str_detect(object, "screw")) %>%
                mutate(screw_size_label = str_to_lower(paste(side, level, "screw_diameter", sep = "_"))) %>%
                mutate(screw_length_label = str_to_lower(paste(side, level, "screw_length", sep = "_"))) %>%
                mutate(screw_type_label = str_to_lower(paste(side, level, "screw_type", sep = "_"))) %>%
                distinct() %>%
                arrange(vertebral_number)
        }else{
            screws_df <-  tibble(level = character(), side = character(), object = character(), vertebral_number = double(), screw_size_label = character(), screw_length_label = character(), screw_type_label = character())
        }
        screws_df
    })
    
    
    screw_details_results_reactive_df <- reactive({
            if(nrow(screw_labels_reactive_df() > 1)){
                max_levels <- nrow(screw_labels_reactive_df())
                levels_count <- seq(from = 1, to = max_levels, by = 1)
                
                screw_levels_labels_df <- screw_labels_reactive_df() %>%
                    mutate(levels_count = row_number())
                
                screw_diameter_df <- screw_levels_labels_df %>%
                    select(level, vertebral_number, level_side, screw_size_label) %>%
                    # mutate(screw_diameter = map2(.x = levels_count, .y = screw_size_label, .f = ~input[[.y[.x]]])) %>%
                    mutate(screw_diameter = map(.x = levels_count, .f = ~input[[screw_labels_reactive_df()$screw_size_label[.x]]])) %>%
                    select(level, vertebral_number, level_side, screw_diameter, screw_size_label) %>%
                    unnest()
                
                screw_length_df <- screw_levels_labels_df %>%
                    select(level, vertebral_number, level_side, screw_length_label) %>%
                    # mutate(screw_length = map2(.x = levels_count, .y = screw_length_label, .f = ~input[[.y[.x]]])) %>%
                    mutate(screw_length = map(.x = levels_count, .f = ~input[[screw_labels_reactive_df()$screw_length_label[.x]]])) %>%
                    select(level, vertebral_number, level_side, screw_length_label, screw_length) %>%
                    unnest(screw_length)
                
                screw_type_df <- screw_levels_labels_df %>%
                    select(level, vertebral_number, level_side, screw_type_label) %>%
                    # mutate(screw_type = map2(.x = levels_count, .y = screw_type_label, .f = ~input[[.y[.x]]])) %>%
                    mutate(screw_type = map(.x = levels_count, .f = ~input[[screw_labels_reactive_df()$screw_type_label[.x]]])) %>%
                    select(level, vertebral_number, level_side, screw_type_label, screw_type) %>%
                    unnest(screw_type)
                
                screw_test_df <- screw_diameter_df %>%
                    left_join(screw_length_df) %>%
                    left_join(screw_type_df) 
                
                
                if("screw_diameter" %in% names(screw_test_df)){
                    screw_test_df <- screw_test_df
                }else{
                    screw_test_df$screw_diameter <- ""
                }
                if("screw_length" %in% names(screw_test_df)){
                    screw_test_df <- screw_test_df
                }else{
                    screw_test_df$screw_length <- ""
                }
                
                if("screw_type" %in% names(screw_test_df)){
                    screw_test_df <- screw_test_df
                }else{
                    screw_test_df$screw_type <- "P"
                }
                
                screw_details_df <- screw_test_df %>%
                    replace_na(list(screw_diameter = "")) %>%
                    replace_na(list(screw_length = "")) %>%
                    mutate(screw_size = glue("{screw_diameter}x{screw_length}mm")) %>%
                    mutate(side = if_else(str_detect(screw_type_label, "left"), "left", "right")) %>%
                    select(level, side, screw_type, screw_diameter, screw_size) %>%
                    mutate(screw_type = case_when(
                        screw_type == "U" ~ "Uniaxial",
                        screw_type == "M" ~ "Monoaxial",
                        screw_type == "P" ~ "Polyaxial", 
                        screw_type == "Red" ~ "Reduction", 
                        screw_type == "Offset" ~ "Offset"
                    )) %>%
                    mutate(screw_size_type = if_else(screw_size == "xNAmm", paste(screw_type), paste(screw_size, screw_type)))
                
                
                # if(any(names(screw_test_df) == "screw_diameter")){
                #     if(any(names(screw_test_df) == "screw_length")){
                #         screw_details_df <- screw_test_df %>%
                #             replace_na(list(screw_diameter = "")) %>%
                #             mutate(screw_size = glue("{screw_diameter}x{screw_length}mm")) %>%
                #             mutate(side = if_else(str_detect(screw_type_label, "left"), "left", "right")) %>%
                #             select(level, side, screw_type, screw_diameter, screw_size) %>%
                #             mutate(screw_type = case_when(
                #                 screw_type == "U" ~ "Uniaxial",
                #                 screw_type == "M" ~ "Monoaxial",
                #                 screw_type == "P" ~ "Polyaxial", 
                #                 screw_type == "Red" ~ "Reduction", 
                #                 screw_type == "Offset" ~ "Offset"
                #             )) %>%
                #             mutate(screw_size_type = if_else(screw_size == "xNAmm", paste(screw_type), paste(screw_size, screw_type)))
                #     }else{
                #         screw_details_df <- screw_test_df %>%
                #             replace_na(list(screw_diameter = "")) %>%
                #             mutate(screw_length = "") %>%
                #             mutate(screw_size = glue("{screw_diameter}x{screw_length}mm")) %>%
                #             mutate(side = if_else(str_detect(screw_type_label, "left"), "left", "right")) %>%
                #             select(level, side, screw_type, screw_diameter, screw_size) %>%
                #             mutate(screw_type = case_when(
                #                 screw_type == "U" ~ "Uniaxial",
                #                 screw_type == "M" ~ "Monoaxial",
                #                 screw_type == "P" ~ "Polyaxial", 
                #                 screw_type == "Red" ~ "Reduction", 
                #                 screw_type == "Offset" ~ "Offset"
                #             )) %>%
                #             mutate(screw_size_type = if_else(screw_size == "xNAmm", paste(screw_type), paste(screw_size, screw_type)))
                #     }
                #     
                # 
                # }else{
                #     if(any(names(screw_test_df) == "screw_length")){
                #         screw_details_df <- screw_test_df %>%
                #             mutate(screw_diameter = "") %>%
                #             mutate(screw_size = glue("{screw_length}mm")) %>%
                #             mutate(side = if_else(str_detect(screw_type_label, "left"), "left", "right")) %>%
                #             select(level, side, screw_type, screw_diameter, screw_size) %>%
                #             mutate(screw_type = case_when(
                #                 screw_type == "U" ~ "Uniaxial",
                #                 screw_type == "M" ~ "Monoaxial",
                #                 screw_type == "P" ~ "Polyaxial", 
                #                 screw_type == "Red" ~ "Reduction", 
                #                 screw_type == "Offset" ~ "Offset"
                #             )) %>%
                #             mutate(screw_size_type = if_else(screw_size == "xmm", paste(screw_type), paste(screw_size, screw_type)))
                #     }else{
                #         
                #         screw_details_df <- screw_test_df %>%
                #             replace_na(list(screw_diameter = "")) %>%
                #             mutate(screw_length = "") %>%
                #             mutate(screw_size = glue("{screw_diameter}x{screw_length}mm")) %>%
                #             mutate(side = if_else(str_detect(screw_type_label, "left"), "left", "right")) %>%
                #             select(level, side, screw_type, screw_diameter, screw_size) %>%
                #             mutate(screw_type = case_when(
                #                 screw_type == "U" ~ "Uniaxial",
                #                 screw_type == "M" ~ "Monoaxial",
                #                 screw_type == "P" ~ "Polyaxial", 
                #                 screw_type == "Red" ~ "Reduction", 
                #                 screw_type == "Offset" ~ "Offset"
                #             )) %>%
                #             mutate(screw_size_type = if_else(screw_size == "xNAmm", paste(screw_type), paste(screw_size, screw_type)))
                #     }
                #     
                # }

            }else{
                screw_details_df <- tibble(level = character(), side = character(), screw_diameter = character(), screw_length = character(), screw_type = character(), screw_size = character(), screw_size_type = character())
            }
        
        screw_details_df

    })
    
    ### PEDICLE SCREW DETAILS TABLE
    output$pedicle_screw_details_table <- renderTable({
        if(nrow(screw_details_results_reactive_df())>0){
            screw_details_results_reactive_df()
        }else{
            tibble(level = character(), side = character(), screw_type = character(), screw_size = character(), screw_size_type = character())
        }
    })  
    


    # Make the Table
    #################### MAKE THE TABLES ########################
    fusion_levels_computed_reactive_df <- reactive({
        all_fusion_implants_df <- all_objects_to_add_list$objects_df %>%
            select(level, vertebral_number, category, object) %>%
            filter(category == "implant") %>%
            filter(str_detect(string = object, pattern = "screw") | str_detect(string = object, pattern = "hook") | str_detect(string = object, pattern = "wire")) %>%
            filter(str_detect(level, pattern = "Iliac") == FALSE) %>%
            filter(str_detect(level, pattern = "S2AI") == FALSE) %>%
            arrange(vertebral_number) %>%
            select(level, vertebral_number) %>%
            distinct()
        
        if(nrow(all_fusion_implants_df) > 1){
            fusion_levels_estimated <- tibble(vertebral_number = seq(from = min(all_fusion_implants_df$vertebral_number) + 0.5, 
                                                                     to = max(all_fusion_implants_df$vertebral_number), by = 1)) %>%
                mutate(category = "fusion", object = "posterolateral", approach = "posterior") %>%
                left_join(interbody_levels_df)%>%
                select(level, vertebral_number, category, object, approach)
            
        }else{
            fusion_levels_estimated <- tibble(level = character(),
                                              vertebral_number = double(),
                                              category = character(),
                                              object = character(),
                                              approach = character())
        }
        if(input$spine_approach == "Anterior"){
            fusion_levels_estimated <- anterior_fusion_levels_function(all_objects_added_df = all_objects_to_add_list$objects_df) %>%
                mutate(category = "anterior_fusion") %>%
                mutate(apporach = "anterior")
        }
        
        fusion_levels_estimated
    })
    

    
    plan_reactive_df <- reactive({
        
        anti_fibrinolytic <- case_when(
            length(input$anti_fibrinolytic) == 0 ~ "--",
            length(input$anti_fibrinolytic) == 1 & "Tranexamic Acid (TXA)" %in% input$anti_fibrinolytic ~ paste(glue("TXA (Load: {input$txa_loading}mg/kg, Maint: {input$txa_maintenance}mg/kg/hr)")),
            length(input$anti_fibrinolytic) > 1 & "Tranexamic Acid (TXA)" %in% input$anti_fibrinolytic ~ paste(glue("{toString(setdiff(x = input$anti_fibrinolytic,
                                                                                                                       y = 'Tranexamic Acid (TXA)'))}, 
                                                                                                                       TXA (Load: {input$txa_loading}mg/kg, Maint: {input$txa_maintenance}mg/kg/hr)")),
            length(input$anti_fibrinolytic) > 0 & ("Tranexamic Acid (TXA)" %in% input$anti_fibrinolytic) == FALSE ~ toString(input$anti_fibrinolytic)
        )
        bmp_size <- case_when(input$bmp_size == 1.05 ~ "XXS", 
                              input$bmp_size == 2.1 ~ "XS", 
                              input$bmp_size == 4.2 ~ "Sm", 
                              input$bmp_size == 8.4 ~ "M", 
                              input$bmp_size == 12 ~ "L")
        
        bmp_mg_dose <- as.double(input$bmp_size)*as.double(input$bmp_number)
        bmp_text <- if_else(input$bmp_number == 0 | is.null(input$bmp_number == 0), "--", paste(input$bmp_number, bmp_size, "(", as.character(bmp_mg_dose), "mg)", sep = ""))
        bmp <- if_else(bmp_text == "None", "None", paste(bmp_text, "(", as.character(bmp_mg_dose), "mg)"))
        
        age <- if_else(paste(input$date_of_birth) == "1900-01-01", "", as.character(round(interval(start = paste(input$date_of_birth), end = paste(input$date_of_surgery))/years(1), 0)))
        
        plan_vector <- c("Patient:" = paste(input$patient_first_name, ",", input$patient_last_name, if_else(age == "", "", paste(age, "yo"), input$sex)), 
                         "Symptoms:" = toString(input$symptoms),
                         "Bone Density:" = input$bone_density,
                         "Relevant Hx:" = input$relevant_history, 
                         "---" = "---",
                         "Preop Abx:" = toString(input$preop_antibiotics), 
                         "Antifibrinolytic:" = anti_fibrinolytic,
                         "Allograft" = if_else(input$allograft_amount == 0, "--", paste(input$allograft_amount, "cc", sep = "")),
                         "BMP:" = bmp_text, 
                         "Left Rod:" = if_else(nrow(left_rod_implants_df_reactive()) > 1, paste(input$left_main_rod_size, input$left_main_rod_material, sep = " "), "--"), 
                         "Right Rod:" = if_else(nrow(right_rod_implants_df_reactive()) > 1, paste(input$right_main_rod_size, input$right_main_rod_material, sep = " "), "--"))
        
        
        enframe(plan_vector, name = "descriptor", value = "value") %>%
            filter(!is.null(value)) %>%
            filter(value != "--") %>%
            mutate(value = str_squish(string = value)) %>%
            filter(value != "")
        
    })
    
    
    
    
    
    ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
    ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
    ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
    ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
    ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
    
    added_rods_statement_reactive <- reactive({
        additional_rods_list <- list()
        
        if(input$add_left_accessory_rod == TRUE){
            additional_rods_list$left_accessory <- glue("On the left side, an accessory rod was connected from {input$left_accessory_rod[[1]]} to {input$left_accessory_rod[[2]]} to increase the overall stiffness of the construct.")
        }
        if(input$add_left_satellite_rod == TRUE){
            additional_rods_list$left_satellite <- glue("On the left side, a satellite rod construct was utilized, with the satellite rod spanning {input$left_satellite_rod[[1]]} to {input$left_satellite_rod[[2]]}.")
            
        }
        if(input$add_left_intercalary_rod == TRUE){
            additional_rods_list$left_intercalary <- glue("On the left side, an intercalary rod construct was utilized, with the intercalary rod spanning {input$left_intercalary_rod[[1]]} to {input$left_intercalary_rod[[2]]}.")
            
        }
        if(input$add_left_linked_rods == TRUE){
            additional_rods_list$left_linked <- glue("On the left side, a linked-rods construct was used, with the rods overlapping from {input$left_linked_rods[[1]]} to {input$left_linked_rods[[2]]}.")
        }
        
        if(input$add_right_accessory_rod == TRUE){
            additional_rods_list$right_accessory <- glue("On the right side, an accessory rod was connected from {input$right_accessory_rod[[1]]} to {input$right_accessory_rod[[2]]} to increase the overall stiffness of the construct.")
        }
        if(input$add_right_satellite_rod == TRUE){
            additional_rods_list$right_satellite <- glue("On the right side, a satellite rod construct was utilized, with the satellite rod spanning {input$right_satellite_rod[[1]]} to {input$right_satellite_rod[[2]]}.")
            
        }
        if(input$add_right_intercalary_rod == TRUE){
            additional_rods_list$right_intercalary <- glue("On the right side, an intercalary rod construct was utilized, with the intercalary rod spanning {input$right_intercalary_rod[[1]]} to {input$right_intercalary_rod[[2]]}.")
            
        }
        if(input$add_right_linked_rods == TRUE){
            additional_rods_list$right_linked <- glue("On the right side, a linked-rods construct was used, with the rods overlapping from {input$right_linked_rods[[1]]} to {input$right_linked_rods[[2]]}.")
        }
        
        if(length(additional_rods_list) > 0){
            
            added_rods_statement <- glue_collapse(additional_rods_list, sep = " ")
            
        }else{
            added_rods_statement <- ""
        }
        
        if(length(additional_rods_list) == 0){
            added_rods_statement <- ""
            added_rods_statement
        }else{
            added_rods_statement
        }
        
        added_rods_statement
    })
    
    
    op_note_text_reactive <- reactive({
        
        complication_df <- tibble(complication = append(input$intraoperative_complications_vector, input$other_intraoperative_complications)) %>%
            filter(complication != "") %>%
            filter(complication != " ") %>%
            remove_empty()
        
        if(nrow(complication_df) > 0){
            complication_df <- complication_df %>%
                mutate(row_count = row_number()) %>%
                mutate(statement = paste0(row_count, ". ", complication)) %>%
                mutate(statement = str_replace_all(statement, pattern = "_", replacement = " "))
            
            complication_statement <- glue_collapse(complication_df$statement, sep = '\n')
        }else{
            complication_statement <- "none"
        }
        
        ### FLuids/Transfusions ###
        fluids_transfusions_list <- list(crystalloids_statement =if_else(input$crystalloids_administered == 0, glue("none"), glue("-Crystalloids: {input$cell_saver_transfused}cc")),
                                         colloids_statement =if_else(input$colloids_administered == 0, glue("none"), glue("-Colloids: {input$colloids_administered}cc")) ,
                                         cell_saver_transfused_statement =if_else(input$cell_saver_transfused == 0, glue("none"), glue("-Cell Saver: {input$cell_saver_transfused}cc")),
                                         prbc_transfused_statement =if_else(input$prbc_transfused == 0, glue("none"), glue("-pRBC's transfused: {input$prbc_transfused} {if_else(input$prbc_transfused >1, 'units', 'unit')}")),
                                         ffp_transfused_statement =if_else(input$ffp_transfused == 0, glue("none"), glue("-FFP transfused: {input$ffp_transfused} {if_else(input$ffp_transfused >1, 'units', 'unit')}")), 
                                         cryo_transfused_statement =if_else(input$cryoprecipitate_transfused == 0, glue("none"), glue("-Cryoprecipitate transfused: {input$cryoprecipitate_transfused} {if_else(input$cryoprecipitate_transfused >1, 'units', 'unit')}")), 
                                         platelets_transfused_statement =if_else(input$platelets_transfused == 0, glue("none"), glue("-Platelets transfused: {input$platelets_transfused} {if_else(input$platelets_transfused >1, 'units', 'unit')}")))
        
        
        fluids_transfusions_list <- discard(.x = fluids_transfusions_list, .p = ~ .x == "none")
        
        if(length(discard(.x = fluids_transfusions_list, .p = ~ .x == "none")) == 0){
            fluids_transfusions_statement <- "See Anesthesia Records"
        }else{
            fluids_transfusions_statement <- glue_collapse(fluids_transfusions_list, sep = '\n')
        }
        
        
        ####### BUILD PROCEDURE PARAGRAPHS ########
        
        posterior_approach_objects_df <- all_objects_to_add_list$objects_df %>%
            filter(approach == "posterior") 
        
        anterior_approach_objects_df <- all_objects_to_add_list$objects_df %>%
            filter(approach == "anterior") 
        
        procedure_results_list <- list()
        
        if(length(input$fusion_levels_confirmed)>0){
            fusions_df <- tibble(level = input$fusion_levels_confirmed) %>%
                left_join(levels_numbered_df)
        }else{
            fusions_df <- tibble(level = character(), vertebral_number = double(), object = character())
        }

        if(nrow(posterior_approach_objects_df) > 0){
            
            if(any(str_detect(all_objects_to_add_list$objects_df$object, "screw"))){
                posterior_approach_objects_df <- all_objects_to_add_list$objects_df %>%
                    filter(approach == "posterior") %>%
                    left_join(screw_details_results_reactive_df() %>% select(level, side, screw_size_type)) %>%
                    filter(object != "intervertebral_cage") %>%
                    left_join(interbody_details_df_reactive()) %>%
                    union_all(interbody_details_df_reactive() %>% filter(object == "intervertebral_cage"))
            }else{
                posterior_approach_objects_df <- all_objects_to_add_list$objects_df %>%
                    filter(approach == "posterior") %>%
                    filter(object != "intervertebral_cage") %>%
                    left_join(interbody_details_df_reactive()) %>%
                    union_all(interbody_details_df_reactive() %>% filter(object == "intervertebral_cage")) %>%
                    mutate(implant_statement = "", screw_size_type = "")
            }
            
            if("screw_length" %in% names(posterior_approach_objects_df)) {
                posterior_approach_objects_df <- posterior_approach_objects_df
            }else{
                posterior_approach_objects_df <- posterior_approach_objects_df %>%
                    mutate(screw_length = " ")
            }
            
            procedure_results_list_posterior <- op_note_posterior_function(all_objects_to_add_df = posterior_approach_objects_df,
                                                                           fusion_levels_df = fusions_df,
                                                                           revision_decompression_vector = input$open_canal,
                                                                           left_main_rod_size = input$left_main_rod_size,
                                                                           left_main_rod_material = input$left_main_rod_material,
                                                                           right_main_rod_size = input$right_main_rod_size,
                                                                           right_main_rod_material = input$right_main_rod_material,
                                                                           additional_rods_statement = added_rods_statement_reactive(),
                                                                           antibiotics = input$preop_antibiotics,
                                                                           additional_procedures_vector = discard(append(input$additional_procedures, input$additional_procedures_other), .p = ~ (.x == "Other" | .x == "")),
                                                                           prior_fusion_levels_vector = input$prior_fusion_levels,
                                                                           instrumentation_removal_vector = input$removal_instrumentation_levels,
                                                                           bmp = if_else(input$bmp_number == 0, 0, (as.double(input$bmp_number)*as.double(input$bmp_size))),
                                                                           bone_graft_vector = input$bone_graft,
                                                                           morselized_allograft = input$allograft_amount,
                                                                           morselized_autograft_separate = 0,
                                                                           structural_allograft_location = input$structural_allograft_location,
                                                                           structural_autograft_harvest = input$structural_autograft_harvest_location,
                                                                           structural_autograft_location = input$structural_autograft_location,
                                                                           deep_drains = input$deep_drains,
                                                                           superficial_drains = input$superficial_drains,
                                                                           end_procedure_details = input$additional_end_procedure_details,
                                                                           closure = input$closure_details,
                                                                           dressing = input$dressing_details, 
                                                                           multiple_position_procedure = input$multiple_approach)
            
            
        }
        
        if(nrow(anterior_approach_objects_df) > 0){
            
            anterior_approach_objects_df <- all_objects_to_add_list$objects_df %>%
                filter(approach == "anterior") %>%
                select(-object_constructed) %>%
                distinct() %>%
                left_join(interbody_details_df_reactive())
            
            procedure_results_list_anterior <- op_note_anterior_function(all_objects_to_add_df = anterior_approach_objects_df,
                                                                         anterior_approach_laterality = "left",
                                                                         microscope_statement = "none", 
                                                                         antibiotics = input$preop_antibiotics, 
                                                                         additional_procedures_vector = input$additional_procedures, 
                                                                         bmp = if_else(input$bmp_number == 0, 0, (as.double(input$bmp_number)*as.double(input$bmp_size))), 
                                                                         bone_graft_vector = input$bone_graft,
                                                                         morselized_allograft = input$allograft_amount,
                                                                         # morselized_autograft_separate = 0,
                                                                         # structural_allograft_location = input$structural_allograft_location,
                                                                         # additional_procedural_details = input$additional_procedures, 
                                                                         deep_drains = input$deep_drains,
                                                                         superficial_drains = input$superficial_drains,
                                                                         end_procedure_details = input$additional_end_procedure_details,
                                                                         closure = input$closure_details,
                                                                         dressing = input$dressing_details,
                                                                         multiple_position_procedure = input$multiple_approach)
            
        }
        
        if(input$multiple_approach == TRUE){
            procedure_results_list$procedures_numbered_paragraph <- glue("Anterior:\n{procedure_results_list_anterior$procedures_numbered_paragraph} \n\nPosterior:\n{procedure_results_list_posterior$procedures_numbered_paragraph}") 
                                                                       
            procedure_results_list$procedure_details_paragraph <- glue("Anterior:\n{procedure_results_list_anterior$procedure_details_paragraph} \n\nWe then turned to the posterior portion of the case.\n\n{procedure_results_list_posterior$procedure_details_paragraph}")
            
        }else{
            if(input$spine_approach == "Posterior"){
                procedure_results_list <- procedure_results_list_posterior
            }else{
                procedure_results_list <- procedure_results_list_anterior
            }
        }
        
        ### Compile full op note:
        secion_headers_df <- tibble(section = c("\nPatient:",
                                                "\nDate of Surgery:",
                                                "\nPrimary Surgeon:",
                                                "\nSurgical Assistants:",
                                                "\nPre-operative Diagnosis:",
                                                "\nPost-operative Diagnosis:",
                                                "\nIndications:",
                                                "\nProcedures Performed:",
                                                "\nSurgical Findings:",
                                                "\nSpecimens Removed:",
                                                "\nEstimated Blood Loss:",
                                                "\nFluids/Transfusions:",
                                                "\nIntraoperative Complications:",
                                                "\nSurgery Description:"),
                                    result = c(paste(input$patient_first_name, input$patient_last_name),
                                               as.character(input$date_of_surgery),
                                               input$primary_surgeon,
                                               input$surgical_assistants,
                                               input$preoperative_diagnosis,
                                               input$postoperative_diagnosis,
                                               input$indications,
                                               procedure_results_list$procedures_numbered_paragraph,
                                               if_else(input$surgical_findings == "", "none", input$surgical_findings),
                                               if_else(input$specimens_removed == "", "none", input$specimens_removed),
                                               input$ebl,
                                               fluids_transfusions_statement,
                                               complication_statement,
                                               procedure_results_list$procedure_details_paragraph # glue_collapse(x = procedure_results$procedure_details_list, sep = '\n\n'),
                                    )) %>%
            mutate(row = row_number()) %>%
            pivot_longer(cols = c(section, result), names_to = "text", values_to = "full_text_vector") %>%
            select(row, full_text_vector)
        
        op_note_text <- glue_collapse(secion_headers_df$full_text_vector, sep = "\n")
        
        op_note_text  
        
    })
    
    
    observeEvent(input$generate_operative_note, {
        updateTextAreaInput(session = session, 
                            inputId = "operative_note_text", 
                            # value = HTML(div(style = "font-size:14px; font-weight:bold; text-align:left", op_note_text_reactive())))
                            value = HTML(op_note_text_reactive()))
        
    }
    )
    
    observeEvent(input$generate_operative_note, {
        output$clipboard_ui <- renderUI({
            rclipboard::rclipButton(inputId = "clip_button", label = "Copy To Clipboard", clipText = input$operative_note_text, icon = icon("clipboard"))
        })
    }
    )
    
    
    observeEvent(input$generate_operative_note, {
        output$operative_note_header_details <- renderUI({
            HTML(op_note_text_reactive())
        })
    }
    )
    
    
    ################# MAKE THE REDCAP AND SUMMARY TABLE $ FUSION TABLE
    # Procedure Specifics:
    output$upload_df <- renderTable({
        fusion_df <- fusion_levels_computed_reactive_df() %>%
            mutate(category = "fusion")
        
        data_wide <- all_objects_to_add_list$objects_df %>%
            as_tibble() %>%
            select(-object_constructed) %>%
            mutate(category = op_note_procedure_category_function(object = object)) %>%
            union_all(fusion_df) %>%
            arrange(vertebral_number) %>%
            select(level, approach, category, procedure = object, side) %>%
            group_by(level, side, category) %>%
            mutate(redcap_repeat_instance = row_number()) %>%
            pivot_wider(names_from = level, values_from = procedure) %>%
            mutate(across(everything(), ~ replace_na(.x, ""))) %>%
            # mutate(record_id = new_record_number) %>%
            ungroup() %>%
            mutate(redcap_repeat_instance = row_number()) %>%
            mutate(redcap_repeat_instrument = "surgical_construct_repeating") %>%
            select(redcap_repeat_instrument, redcap_repeat_instance, everything()) %>%
            clean_names()
        
        data_wide
    })
    
    
    summary_table_for_redcap_reactive <- reactive({
        
        # #################### Patient Details  #########################
        age <- if_else(paste(input$date_of_birth) == "1900-01-01", "--", as.character(round(interval(start = paste(input$date_of_birth), end = paste(input$date_of_surgery))/years(1), 0)))
        
        bone_density <- input$bone_density
        
        ################### Approach #########################
        approach_df <- all_objects_to_add_list$objects_df  %>%
            select(approach) %>%
            distinct()
        approach_statement <- glue_collapse(x = approach_df$approach, sep = " and ")
        
        approach_specified_statement <- paste(input$approach_specified_anterior, input$approach_specified_posterior, sep = " and ")
        
        ################### SPINE INSTRUMENTATIONS/VERTEBRAE TREATED #########################
        all_implants_df <- all_objects_to_add_list$objects_df  %>%
            filter(str_detect(string = object, pattern = "hook") |
                       str_detect(string = object, pattern = "screw") |
                       str_detect(string = object, pattern = "wire") |
                       str_detect(string = object, pattern = "anterior_interbody") |
                       str_detect(string = object, pattern = "plate") |
                       str_detect(string = object, pattern = "cage") |
                       str_detect(string = object, pattern = "laminoplasty") |
                       str_detect(string = object, pattern = "lif"))
        
        spine_instrumented <- if_else(nrow(all_implants_df) == 0, FALSE, TRUE)
        uiv <- if_else(spine_instrumented == TRUE, (all_implants_df %>% filter(vertebral_number == min(vertebral_number)))$level[1], "none")
        liv <- if_else(spine_instrumented == TRUE, (all_implants_df %>% filter(vertebral_number == max(vertebral_number)))$level[1], "none")
        
        spine_treated <- if_else(nrow(all_objects_to_add_list$objects_df) > 0, TRUE, FALSE)
        upper_treated_vertebrae <- if_else(spine_treated == TRUE, (all_objects_to_add_list$objects_df %>% filter(vertebral_number == min(vertebral_number)))$level[1], "none")
        lower_treated_vertebrae <- if_else(spine_treated == TRUE, (all_objects_to_add_list$objects_df %>% filter(vertebral_number == max(vertebral_number)))$level[1], "none")
        
        levels_fused <- length(input$fusion_levels_confirmed)
        
        pelvic_fixation <- any(str_detect(string = all_implants_df$object, pattern = "pelvic"))
        
        left_rod <- if_else(input$left_main_rod_size == "None", "None", paste(input$left_main_rod_size, input$left_main_rod_material))
        right_rod <- if_else(input$right_main_rod_size == "None", "None", paste(input$right_main_rod_size, input$right_main_rod_material))
        
        supplemental_rods_df <- tibble(supplemental_rod = c("accessory_rod",
                                                            "satellite_rod",
                                                            "intercalary_rod",
                                                            "linked_rods",
                                                            "accessory_rod",
                                                            "satellite_rod",
                                                            "intercalary_rod",
                                                            "linked_rods"),
                                       side = c("left", "left", "left", "left", "right", "right", "right", "right"),
                                       yes_no = c(input$add_left_accessory_rod,
                                                  input$add_left_satellite_rod,
                                                  input$add_left_intercalary_rod,
                                                  input$add_left_linked_rods,
                                                  input$add_right_accessory_rod,
                                                  input$add_right_satellite_rod,
                                                  input$add_right_intercalary_rod,
                                                  input$add_right_linked_rods)) %>%
            filter(yes_no == TRUE)
        
        left_supplemental_rods_vector <- if_else(nrow(supplemental_rods_df %>% filter(side == "left")) == 0, "None", toString((supplemental_rods_df %>% filter(side == "left"))$supplemental_rod))
        right_supplemental_rods_vector <- if_else(nrow(supplemental_rods_df %>% filter(side == "right")) == 0, "None", toString((supplemental_rods_df %>% filter(side == "right"))$supplemental_rod))
        
        ### INTERBODY FUSIONS ####
        corpectomy_fusion_df <- all_objects_to_add_list$objects_df%>%
            filter(object == "corpectomy_cage") %>%
            select(-vertebral_number) %>%
            mutate(level = map(.x = level, .f = ~ jh_get_cranial_caudal_interspace_body_list_function(level = .x)$cranial_interspace)) %>%
            union_all(all_objects_to_add_list$objects_df%>%
                          filter(object == "corpectomy_cage") %>%
                          select(-vertebral_number) %>%
                          mutate(level = map(.x = level, .f = ~ jh_get_cranial_caudal_interspace_body_list_function(level = .x)$caudal_interspace))) %>%
            unnest(level) %>%
            left_join(levels_numbered_df) %>%
            select(level, vertebral_number, approach, category, object) %>%
            distinct() 
        
        interbody_fusion_df <- all_objects_to_add_list$objects_df %>%
            filter(object == "tlif" | 
                       object == "plif" | 
                       object == "llif" | 
                       object == "diskectomy_fusion" | 
                       object == "anterior_interbody_implant" | 
                       object == "no_implant_interbody_fusion" |
                       object == "diskectomy_fusion_no_interbody_device" | 
                       object == "decompression_diskectomy_fusion" | 
                       object == "decompression_diskectomy_fusion" |
                       object == "corpectomy_cage") %>%
            select(level, vertebral_number, approach, category, object) %>%
            union_all(corpectomy_fusion_df) %>%
            arrange(vertebral_number) %>%
            distinct()
        
        interbody_fusions_vector <- interbody_fusion_df %>%
            select(level) %>%
            as_vector()
        
        interbody_fusions_levels_statement <- if_else(length(interbody_fusions_vector) >0, toString(interbody_fusions_vector), "none")
        
        
        #################### SPINE UIV PPX  #########################
        
        if(spine_instrumented == TRUE | spine_treated == TRUE){
            uiv_ppx_df <- all_objects_to_add_list$objects_df %>%
                filter(level == upper_treated_vertebrae | level == uiv) %>%
                filter(str_detect(string = object, pattern = "hook") | str_detect(string = object, pattern = "tether") | str_detect(string = object, pattern = "wire") | str_detect(string = object, pattern = "cement_augmentation")) %>%
                select(level, object) %>%
                distinct()
        }else{
            uiv_ppx_df <- tibble(level = character(), object = character())
        }
        
        #################### Additional Procedural Details  ########################
        
        three_column_osteotomy <- any(any(all_objects_to_add_list$objects_df$object == "grade_3") |
                                          any(all_objects_to_add_list$objects_df$object == "grade_4") |
                                          any(all_objects_to_add_list$objects_df$object == "grade_5") |
                                          any(all_objects_to_add_list$objects_df$object == "grade_6"))
        
        
        decompressions_df <- all_objects_to_add_list$objects_df %>%
            filter(category == "decompression" | object == "decompression_diskectomy_fusion" | str_detect(string = object, pattern = "decompression") | object == "anterior_disc_arthroplasty") %>%
            select(level, object) %>%
            distinct()
        
        decompression_levels_df <- decompressions_df %>%
            select(level) %>%
            distinct()
        
        bmp_mg_dose <-if_else(input$bmp_number == 0, "0", paste((as.double(input$bmp_number)*as.double(input$bmp_size))))
        
        allograft <- input$allograft
        
        ####### PROCEDURE FLUIDS/COMPLICATIONS #####
        complication_df <- tibble(complication = append(input$intraoperative_complications_vector, input$other_intraoperative_complications)) %>%
            filter(complication != "") %>%
            filter(complication != " ") %>%
            remove_empty()
        
        if(nrow(complication_df) > 0){
            complication_df <- complication_df %>%
                mutate(row_count = row_number()) %>%
                mutate(statement = paste0(row_count, ". ", complication)) %>%
                mutate(statement = str_replace_all(statement, pattern = "_", replacement = " "))
            
            complication_statement <- glue_collapse(complication_df$complication, sep = ', ++ ')
        }else{
            complication_statement <- "none"
        }
        
        
        summary_df <- tibble(attending = input$primary_surgeon,
                             assisting = input$surgical_assistants,
                             last_name = input$patient_last_name,
                             first_name = input$patient_first_name,
                             date_of_birth = as.character(input$date_of_birth),
                             age = age,
                             sex = input$sex,
                             diagnosis = toString(str_to_lower(input$diagnosis_subgroup)),
                             symptoms = toString(str_to_lower(input$symptoms)),
                             bone_density = bone_density,
                             date_of_surgery = as.character(input$date_of_surgery),
                             primary_revision = input$primary_revision,
                             revision_indication = toString(str_to_lower(input$revision_indication)),
                             prior_fusion_levels = toString(input$prior_fusion_levels),
                             levels_instrumentation_removed = toString(input$prior_fusion_levels),
                             staged_procedure = if_else(input$staged_procedure == TRUE, "Yes", "No"),
                             stage_number = input$stage_number,
                             approach = approach_statement,
                             approach_detail = approach_specified_statement,
                             bmp_dose_mg = bmp_mg_dose,
                             upper_treated_vertebrae = upper_treated_vertebrae,
                             uiv = uiv,
                             lower_treated_vertebrae= lower_treated_vertebrae,
                             liv = liv,
                             uiv_ppx_used = if_else(nrow(uiv_ppx_df) == 0, "No", "Yes"),
                             uiv_ppx = if_else(nrow(uiv_ppx_df) == 0, "None", toString(str_replace_all(string = uiv_ppx_df$object, pattern = "_", replacement = " "))),
                             interspaces_fused = levels_fused,
                             interbody_fusion_levels = interbody_fusions_levels_statement,
                             pelvic_fixation = if_else(pelvic_fixation == TRUE, "yes", "no"),
                             three_column_osteotomy = if_else(three_column_osteotomy == TRUE, "yes", "no"),
                             left_rod = left_rod,
                             right_rod = right_rod,
                             left_supplemental_rod = left_supplemental_rods_vector,
                             right_supplemental_rod = right_supplemental_rods_vector,
                             number_of_levels_decompressed = length(decompression_levels_df$level),
                             decompressions_performed = toString(decompression_levels_df$level),
                             ebl = input$ebl,
                             complications = complication_statement,
                             operative_note = input$operative_note_text
                             # operative_note = HTML(op_note_text_reactive())
        )
        
        
        summary_df
        
    })
    
    # All objects table:
    output$all_objects_table <- renderTable({
    
        all_objects_to_add_list$objects_df %>%
            as_tibble() %>%
            select(level, side, object) %>%
            distinct() %>%
            mutate(category = map(.x = object, .f = ~ op_note_procedure_performed_summary_classifier_function(object = .x)))
        
    })
    
    
    
    # "Procedure Summary Table:"
    output$summary_table <- renderTable({
        
        summary_table <- summary_table_for_redcap_reactive() %>%
            remove_empty()
        
        row_1 <- summary_table%>%
            slice(1) %>%
            as.character()
        
        tibble(variable = names(summary_table), 
               result = row_1) %>%
            filter(result != "") %>%
            filter(result != " ")
        
    })
    
    output$summary_table_for_redcap_preview <- renderTable({
        summary_table_for_redcap_reactive() %>%
            as.matrix() %>%
            t() %>%
            as_tibble(rownames = "variable") %>%
            rename(result = "V1")
        
        
    })
    
    
    
    
    ################################# REDCAP API #########################
    observeEvent(input$confirm_upload_1, {
        confirmSweetAlert(inputId = "confirm_upload_final",
                          session = session,
                          title = "Final Confirmation to Upload Data:", 
                          btn_labels = c("No, Cancel", "Yes, Upload"), text = "Select if you wish to upload.", closeOnClickOutside = TRUE)
    })
    
    observeEvent(input$confirm_upload_final, {
        
        new_record_number <- exportNextRecordName(rcon = rcon)
        
        df_for_upload <- summary_table_for_redcap_reactive() %>%
            rename_all(.funs = ~paste(., "_shiny", sep = "")) %>%
            mutate(record_id = new_record_number) %>%
            select(record_id, everything()) 
        
        
        
        importRecords(rcon = rcon, data = df_for_upload, returnContent = "count")
        
        
        data_wide <- all_objects_to_add_list$objects_df %>%
            as_tibble() %>%
            select(-object_constructed) %>%
            arrange(vertebral_number) %>%
            select(level, approach, category, procedure = object, side) %>%
            group_by(level, side, category) %>%
            mutate(redcap_repeat_instance = row_number()) %>%
            pivot_wider(names_from = level, values_from = procedure) %>%
            mutate(across(everything(), ~ replace_na(.x, ""))) %>%
            mutate(record_id = new_record_number) %>%
            ungroup() %>%
            mutate(redcap_repeat_instance = row_number()) %>%
            mutate(redcap_repeat_instrument = "surgical_construct_repeating") %>%
            select(record_id,redcap_repeat_instrument, redcap_repeat_instance, everything()) %>%
            clean_names()
        
        importRecords(rcon = rcon, data = data_wide, returnContent = "count")
        
        data_wide
        
        sendSweetAlert(
            session = session,
            title = "Success !!",
            text = "All in order",
            type = "success"
        )
        
        
    })
    
    task_items_reactive_list <- reactiveValues()
    
    task_items_reactive_list$upload_to_redcap <- 0
    
    observeEvent(input$confirm_upload_final, {
        task_items_reactive_list$upload_to_redcap <- 100
    })
    
    output$upload_to_redcap_task <- renderMenu({
        dropdownMenu(type = "tasks", 
                     badgeStatus = if_else(task_items_reactive_list$upload_to_redcap == 100, "success", "warning"), 
                     taskItem(text = "Upload Data to Redcap", 
                              value = task_items_reactive_list$upload_to_redecap))
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
