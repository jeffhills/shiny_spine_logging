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

rcon <- redcapConnection(url = 'https://redcap.wustl.edu/redcap/api/', token = "58C0BC0A6CA8B8DFB21A054C2F5A3C49")

source("short_shiny_functions.R", local = TRUE)
source("make_geoms_functions.R", local = TRUE)
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
        tags$head(
            tags$style(
                "#posterior_bmp_size {
                font-size: small;
                max-width: -webkit-fill-available;
                        text-align: left;
                }"),
            tags$style(
                "#posterior_bmp_number {
                        width: -webkit-fill-available;
                        text-align: center;
                }"),
            tags$style(
                "#posterior_bmp_dosage {
                        width: -webkit-fill-available;
                text-align: center;
                font-size: medium;
                font-weight: bold;
                border-style: solid;
                border-color: burlywood;
                        }"),
            tags$style(
                "#anterior_bmp_size {
                font-size: small;
                max-width: -webkit-fill-available;
                        text-align: left;
                }"),
            tags$style(
                "#anterior_bmp_number {
                        width: -webkit-fill-available;
                        text-align: center;
                }"),
            tags$style(
                "#anterior_bmp_dosage {
                        width: -webkit-fill-available;
                text-align: center;
                font-size: medium;
                font-weight: bold;
                border-style: solid;
                border-color: burlywood;
                        }"),
            tags$style(
                "#all_objects_table {
                        overflow-x: auto;
                }"),
            tags$style(
                "#redcap_details_wide_df {
                        overflow-x: auto;
                }"
            ),
        ),
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
                                                      "Neoplasm" = "neoplasm", 
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
                                                                                 input_id = "prior_fusion_levels", 
                                                                                 left_column_percent_width = 60, 
                                                                                 font_size = 14, 
                                                                                 choices_vector = unique(interbody_levels_df$level)
                                                ),
                                                fluidRow(
                                                    column(width = 6,
                                                           dropdownButton(icon = icon("fas fa-screwdriver"),
                                                                          up = TRUE, circle = FALSE, status = "info", label = "Left Revision Implants", tooltip = "Click to add Revision Implants", 
                                                                    width = "100%",
                                                                    style = "info",
                                                                    column(6, 
                                                                           awesomeCheckboxGroup(inputId = "left_revision_implants",
                                                                                                label = "Implants Present:",
                                                                                                choices = unique((revision_implants_df %>% filter(x < 0.5))$level)
                                                                           )
                                                                           ), 
                                                                    column(6, 
                                                                           awesomeCheckboxGroup(inputId = "left_revision_implants_removed",
                                                                                                label = "Implants Removed:",status = "danger",
                                                                                                choices = unique((revision_implants_df %>% filter(x < 0.5))$level)
                                                                           )
                                                                           )
                                                           )
                                                    ),
                                                    column(width = 6,
                                                           dropdownButton(icon = icon("fas fa-screwdriver"),
                                                                          up = TRUE, circle = FALSE, status = "info", label = "right Revision Implants", tooltip = "Click to add Revision Implants", 
                                                                          width = "100%",
                                                                          style = "info",
                                                                          column(6, 
                                                                                 awesomeCheckboxGroup(inputId = "right_revision_implants",
                                                                                                      label = "Implants Present:",
                                                                                                      choices = unique((revision_implants_df %>% filter(x < 0.5))$level)
                                                                                 )
                                                                          ), 
                                                                          column(6, 
                                                                                 awesomeCheckboxGroup(inputId = "right_revision_implants_removed",
                                                                                                      label = "Implants Removed:",status = "danger",
                                                                                                      choices = unique((revision_implants_df %>% filter(x < 0.5))$level)
                                                                                 )
                                                                          )
                                                           )
                                                    )
                                                )
                               ),
                                                tags$hr(),
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
                                                                                    choiceNames = list(tags$span(icon("fas fa-smile-beam", style = "color: steelblue"), strong("Anterior or Lateral")),
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
                                                  switchInput(label = "Plot with patterns (slower):",
                                                              onLabel = "Yes",
                                                              offLabel = "No",
                                                              inputId = "plot_with_patterns_true",
                                                              value = FALSE
                                                  ),
                                                  switchInput(label = "Plot Summary Table:",
                                                              onLabel = "Yes",
                                                              offLabel = "No",
                                                              inputId = "plot_summary_table",
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
                                                              value = c(0.05,0.42), direction = "rtl",
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
                                              fluidRow(
                                                  switchInput(
                                                      inputId = "lumbar_vertebrae_6",
                                                      label = "6 Lumbar Vertebrae:", 
                                                      value = FALSE, 
                                                      onLabel = "Yes", 
                                                      offLabel = "No", 
                                                      size = "small",
                                                      onStatus = "success"
                                                  )
                                              ),
                                              fluidRow(
                                                  actionBttn(
                                                      inputId = "implants_complete",
                                                      size = "sm", 
                                                      block = TRUE,
                                                      label = "Click when finished to Add Implant Details",
                                                      style = "simple",
                                                      color = "success", 
                                                      icon = icon("fas fa-arrow-circle-right")
                                                  )
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
                                                        choices = c("Left-sided", 
                                                                    "Right-sided",
                                                                    "Paramedian",
                                                                    "Lateral Transpsoas",
                                                                    "Lateral Antepsoas",
                                                                    "Thoracoabdominal",
                                                                    "Thoracotomy",
                                                                    "Transperitoneal",
                                                                    "Retroperitoneal"),
                                                        selected = "Left-sided",
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
                                                        label = "Add Surgical Implants (Screws, Hooks, Wires, Tethers, etc.)",
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
                                                        inputId = "add_special_approach",
                                                        size = "sm", block = TRUE,
                                                        label = "Add Special Approach (Costovertebral, etc)",
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
                                                    actionBttn(
                                                        inputId = "add_other",
                                                        size = "sm", block = TRUE,
                                                        label = "Add Other (cement, structural allograft, etc)",
                                                        style = "simple",
                                                        color = "primary"
                                                    ),
                                                    br(),
                                                    uiOutput(outputId = "intervertebral_cage_ui"),
                                                    uiOutput(outputId = "tumor_resection_ui"),
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
                                               "Occipital Screws" = "occipital_screw",
                                               "Transarticular Screw" = "transarticular_screw",
                                               "Pars Screws" = "pars_screw",
                                               "Translaminar Screws" = "translaminar_screw",
                                               "Lateral Mass Screws" = "lateral_mass_screw",
                                               "TP Hook" = "tp_hook",
                                               "Laminar Hook (Downgoing)" = "laminar_downgoing_hook",
                                               "Laminar Hook (Upgoing)" = "laminar_upgoing_hook",
                                               "Pedicle Hook" = "pedicle_hook",
                                               "Tether (Spinous Process)" = "tether",
                                               "Sublaminar Wire" = "sublaminar_wire"  
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
                    box(width = 7, status = "info", title = div(style = "font-size:22px; font-weight:bold; text-align:left", "Implant & Fusion Details:"),
                        fluidRow(
                            column(width = 4, 
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
                                                  switchInput(
                                                      inputId = "anterior_fusion_performed",
                                                      width = '100%',
                                                      inline = TRUE,
                                                      label = "Fusion:",
                                                      onLabel = "Yes",
                                                      offLabel = "No",
                                                      value = FALSE,
                                                      size = "mini"
                                                  ),
                                                  switchInput(
                                                      inputId = "posterior_fusion_performed",
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
                                   )
                            ),
                            column(width = 4, 
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
                        ),
                        conditionalPanel(condition = "input.posterior_fusion_performed == true",
                                         box(width = 12, collapsible = TRUE, title = div(style = "font-size:20px; font-weight:bold; text-align:center", "POSTERIOR Bone Graft & Biologics:"),
                                             ## NEW
                                             fluidRow(
                                                 column(6,
                                                        div(style = "font-size:16px; font-weight:bold; text-align:left", "Select any bone graft used:"),
                                                        prettyCheckboxGroup(inputId = "posterior_bone_graft",
                                                                            shape = "curve", outline = TRUE, status = "primary", width = "95%", bigger = TRUE,
                                                                            label = NULL,
                                                                            choices = c("Morselized Allograft",
                                                                                        "Local Autograft",
                                                                                        "Morselized Autograft (separate fascial incision)"
                                                                            )
                                                        )
                                                 ),
                                                 column(6,
                                                        div(style = "font-size:16px; font-weight:bold; text-align:left", "Other Biologics:"),
                                                        prettyCheckboxGroup(inputId = "posterior_biologics",
                                                                            shape = "curve", outline = TRUE, status = "primary", width = "95%",bigger = TRUE,
                                                                            label = NULL,
                                                                            choices = c("Bone Marrow Aspirate",
                                                                                        "Cell Based Allograft",
                                                                                        "DBM"
                                                                            )
                                                        )
                                                 )
                                             ),
                                             fluidRow(
                                                 column(6,
                                                        conditionalPanel(
                                                            condition = "input.posterior_bone_graft.indexOf('Morselized Allograft') > -1",
                                                            jh_make_shiny_table_row_function(left_column_label = "Allograft (cc):",bottom_margin = "5px",
                                                                                             left_column_percent_width = 60,
                                                                                             font_size = 16,
                                                                                             input_type = "numeric",
                                                                                             input_id = "posterior_allograft_amount",
                                                                                             initial_value_selected = 0,
                                                                                             min = 0, max = 500, step = 30)
                                                        )
                                                 ),
                                                 column(6,
                                                        conditionalPanel(
                                                            condition = "input.posterior_biologics.indexOf('Bone Marrow Aspirate') > -1",
                                                            jh_make_shiny_table_row_function(left_column_label = "Bone Marrow Aspirate Volume (cc):",bottom_margin = "5px",
                                                                                             left_column_percent_width = 60,
                                                                                             font_size = 16,
                                                                                             input_type = "numeric",
                                                                                             input_id = "posterior_bone_marrow_aspirate_volume",
                                                                                             initial_value_selected = 0,
                                                                                             min = 0,
                                                                                             max = 500,
                                                                                             step = 5)
                                                        ),
                                                        conditionalPanel(
                                                            condition = "input.posterior_biologics.indexOf('Cell Based Allograft') > -1",
                                                            jh_make_shiny_table_row_function(left_column_label = "Cell Based Allograft Volume (cc):",bottom_margin = "5px",
                                                                                             left_column_percent_width = 60,
                                                                                             font_size = 16,
                                                                                             input_type = "numeric",
                                                                                             input_id = "posterior_cell_based_allograft_volume",
                                                                                             initial_value_selected = 0,
                                                                                             min = 0,
                                                                                             max = 500,
                                                                                             step = 5)
                                                        ),
                                                        conditionalPanel(
                                                            condition = "input.posterior_biologics.indexOf('DBM') > -1",
                                                            jh_make_shiny_table_row_function(left_column_label = "DBM Volume (cc):",bottom_margin = "5px",
                                                                                             left_column_percent_width = 60,
                                                                                             font_size = 16,
                                                                                             input_type = "numeric",
                                                                                             input_id = "posterior_dbm_volume",
                                                                                             initial_value_selected = 0, min = 0, max = 500, step = 5)
                                                        )
                                                 )
                                             ),
                                             jh_make_bmp_ui_function(anterior_posterior = "posterior"),
                                         )
                        ),
                        conditionalPanel(condition = "input.anterior_fusion_performed == true",
                                         box(width = 12, collapsible = TRUE, title = div(style = "font-size:20px; font-weight:bold; text-align:center", "ANTERIOR Bone Graft & Biologics:"),
                                             fluidRow(
                                                 tags$table(width = "100%",
                                                            tags$tr(
                                                                tags$td(width = "50%",
                                                                        div(style = "font-size:16px; font-weight:bold; text-align:left", "Select any bone graft used:")
                                                                ),
                                                                tags$td(width = "50%",
                                                                        div(style = "font-size:16px; font-weight:bold; text-align:left", "Other Biologics:")
                                                                )
                                                            ),
                                                            tags$tr(
                                                                tags$td(width = "50%",
                                                                        prettyCheckboxGroup(inputId = "anterior_bone_graft", 
                                                                                            shape = "curve", outline = TRUE, status = "primary", width = "95%", bigger = TRUE,
                                                                                            label = NULL, 
                                                                                            choices = c("Morselized Allograft",
                                                                                                        "Local Autograft",
                                                                                                        "Morselized Autograft (separate fascial incision)"
                                                                                            )
                                                                        )
                                                                ),
                                                                tags$td(width = "50%",
                                                                        prettyCheckboxGroup(inputId = "anterior_biologics", 
                                                                                            shape = "curve", outline = TRUE, status = "primary", width = "95%",bigger = TRUE,
                                                                                            label = NULL,
                                                                                            choices = c("Bone Marrow Aspirate",
                                                                                                        "Cell Based Allograft",
                                                                                                        "DBM"
                                                                                            )
                                                                        )
                                                                )
                                                            )
                                                 )
                                             ),
                                             fluidRow(
                                                 column(width = 12, 
                                                        conditionalPanel(
                                                            condition = "input.anterior_bone_graft.indexOf('Morselized Allograft') > -1",
                                                            jh_make_shiny_table_row_function(left_column_label = "Allograft (cc):",
                                                                                             left_column_percent_width = 60, 
                                                                                             font_size = 16,
                                                                                             input_type = "numeric",
                                                                                             input_id = "anterior_allograft_amount", initial_value_selected = 0, min = 0, max = 500, step = 30)
                                                        ),
                                                        conditionalPanel(
                                                            condition = "input.anterior_biologics.indexOf('Bone Marrow Aspirate') > -1",
                                                            jh_make_shiny_table_row_function(left_column_label = "Bone Marrow Aspirate Volume (cc):",
                                                                                             left_column_percent_width = 60, 
                                                                                             font_size = 16,
                                                                                             input_type = "numeric",
                                                                                             input_id = "anterior_bone_marrow_aspirate_volume", 
                                                                                             initial_value_selected = 0, 
                                                                                             min = 0,
                                                                                             max = 500,
                                                                                             step = 5)
                                                        ),
                                                        conditionalPanel(
                                                            condition = "input.anterior_biologics.indexOf('Cell Based Allograft') > -1",
                                                            jh_make_shiny_table_row_function(left_column_label = "Cell Based Allograft Volume (cc):",
                                                                                             left_column_percent_width = 60, 
                                                                                             font_size = 16,
                                                                                             input_type = "numeric",
                                                                                             input_id = "anterior_cell_based_allograft_volume", 
                                                                                             initial_value_selected = 0,
                                                                                             min = 0,
                                                                                             max = 500,
                                                                                             step = 5)
                                                        ),
                                                        conditionalPanel(
                                                            condition = "input.anterior_biologics.indexOf('DBM') > -1",
                                                            jh_make_shiny_table_row_function(left_column_label = "DBM Volume (cc):",
                                                                                             left_column_percent_width = 60, 
                                                                                             font_size = 16,
                                                                                             input_type = "numeric",
                                                                                             input_id = "anterior_dbm_volume", initial_value_selected = 0, min = 0, max = 500, step = 5)
                                                        )
                                                 )
                                             ),
                                             jh_make_bmp_ui_function(anterior_posterior = "anterior")
                                         )
                        ),
                        conditionalPanel(condition = "input.fusion_procedure_performed == true",
                                         box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Interbody Implant Details:"), collapsible = TRUE,  
                                             uiOutput(outputId = "interbody_implants_ui")
                                         )
                        ),
                        conditionalPanel(condition = "input.fusion_procedure_performed == true & input.spine_approach.indexOf('Posterior') > -1",
                                         box(width = 12, title = div(style = "font-size:20px; font-weight:bold; text-align:center", "Rod Details:"), collapsible = TRUE,
                                             fluidRow(column(4), 
                                                      column(4, 
                                                             dropdown(icon = icon("link"),
                                                                      width = "60%",
                                                                      label = "Add Crosslink",
                                                                      style = "unite",
                                                                      checkboxGroupButtons(inputId = "crosslink_connectors",
                                                                                           label = "Add crosslinks at:",
                                                                                           choices = vertebral_bodies_vector,
                                                                                           individual = FALSE,
                                                                                           # justified = TRUE,
                                                                                           direction = "vertical",
                                                                                           checkIcon = list(
                                                                                               yes = tags$i(class = "fa fa-check-square",
                                                                                                            style = "color: steelblue"),
                                                                                               no = tags$i(class = "fa fa-square-o",
                                                                                                           style = "color: steelblue"))
                                                                      )
                                                             )
                                                      ),
                                                      column(4)
                                             ),
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
                                             jh_make_shiny_table_column_function(input_type = "awesomeRadio", 
                                                                                 left_input_id = "left_main_rod_material", 
                                                                                 left_label = "Material:",
                                                                                 right_input_id = "right_main_rod_material", 
                                                                                 right_label = "Material:",
                                                                                 left_column_percent_width = 50,
                                                                                 right_column_percent_width = 50,
                                                                                 checkboxes_inline = TRUE,
                                                                                 button_size = "normal",
                                                                                 choices_vector = c("Non-instrumented", "Titanium", "Cobalt Chrome", "Stainless Steel"),
                                                                                 initial_value_selected = "Non-instrumented",
                                                                                 picker_choose_multiple = FALSE),
                                             jh_make_supplemental_rod_ui_function(rod_type = "accessory_rod", input_label = "Accessory Rod"),
                                             jh_make_supplemental_rod_ui_function(rod_type = "satellite_rod", input_label = "Satellite Rod"),
                                             jh_make_supplemental_rod_ui_function(rod_type = "intercalary_rod", input_label = "Intercalary Rod"),
                                             jh_make_supplemental_rod_ui_function(rod_type = "linked_rods", input_label = "Linked Rods"),
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
                                             uiOutput(outputId = "screw_details_ui"),
                                             uiOutput(outputId = "screw_types_ui")
                                             )
                        )
                    ),
                    box(width = 5, status = "primary",
                        plotOutput("spine_plot_for_implants_tab",
                                   height = 750),
                        fluidRow(
                            actionBttn(
                                inputId = "implant_details_complete",
                                size = "sm", 
                                block = TRUE,
                                label = "Click when finished to Upload Data & Generate Operative Note",
                                style = "simple",
                                color = "success", 
                                icon = icon("fas fa-arrow-circle-right")
                            )
                        )
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
                        hr(),
                        jh_make_shiny_table_row_function(left_column_percent_width = 30, 
                                                         left_column_label = "Neuromonitoring used:",
                                                         input_type = "checkbox", 
                                                         input_id = "neuromonitoring", choices_vector = c("EMG", "SSEP", "tc MEP", "DNEP (Cord Stimulation)", "H reflex"),checkboxes_inline = TRUE, 
                                                         initial_value_selected = c("SSEP", "tc MEP")),
                        jh_make_shiny_table_row_function(left_column_label = "Preop Antibiotics:",
                                                         input_type = "picker", 
                                                         input_id = "preop_antibiotics", 
                                                         left_column_percent_width = 30, 
                                                         font_size = 14, 
                                                         choices_vector = c("None (Antibiotics were held)", "Cefazolin (Ancef)", "Vancomycin", "Ceftriaxone", "Gentamycin", "Clindamycin", "Aztreonam", "Unknown", "Other"), 
                                                         initial_value_selected = c("Cefazolin (Ancef)", "Vancomycin")
                        ),
                        jh_make_shiny_table_row_function(left_column_label = "Antifibrinolytic:",
                                                         input_type = "picker", 
                                                         input_id = "anti_fibrinolytic", 
                                                         left_column_percent_width = 30, 
                                                         font_size = 14, 
                                                         choices_vector = c("None", "Tranexamic Acid (TXA)", "Amicar", "Desmopressin (DDAVP)", "Other")
                        ),
                        conditionalPanel(condition = "input.anti_fibrinolytic.indexOf('Tranexamic Acid (TXA)') > -1",
                                         jh_make_shiny_table_row_function(left_column_label = "TXA Loading (mg/kg):    ",
                                                                          input_type = "numeric", 
                                                                          input_id = "txa_loading", 
                                                                          left_column_percent_width = 50, 
                                                                          font_size = 13, min = 0, max = 200, initial_value_selected = 20, step = 5, text_align = "right",
                                         ),
                                         jh_make_shiny_table_row_function(left_column_label = "TXA Maintenance (mg/kg/hr):    ",
                                                                          input_type = "numeric", 
                                                                          input_id = "txa_maintenance", 
                                                                          left_column_percent_width = 50, 
                                                                          font_size = 13, min = 0, max = 50, initial_value_selected = 5, step = 5, text_align = "right",
                                         ),
                        ),
                        jh_make_shiny_table_row_function(left_column_percent_width = 30, left_column_label = "Findings:", input_type = "text", input_id = "surgical_findings", initial_value_selected = ""),
                        jh_make_shiny_table_row_function(left_column_percent_width = 30, left_column_label = "Specimens:", input_type = "text", input_id = "specimens_removed", initial_value_selected = ""),
                        hr(),
                        jh_make_shiny_table_row_function(left_column_percent_width = 30, left_column_label = "Estimated Blood Loss:", input_type = "numeric", input_id = "ebl", initial_value_selected = NULL, min = 0, max = 50000, step = 100),
                        jh_make_shiny_table_row_function(left_column_percent_width = 30, left_column_label = "Urine Output:", input_type = "numeric", input_id = "urine_output", initial_value_selected = NULL, min = 0, max = 50000, step = 100),
                        jh_make_shiny_table_row_function(left_column_percent_width = 30, left_column_label = "Crystalloids:", input_type = "numeric", input_id = "crystalloids_administered", initial_value_selected = NULL, min = 0, max = 100000, step = 100),
                        jh_make_shiny_table_row_function(left_column_percent_width = 30, left_column_label = "Colloids:", input_type = "numeric", input_id = "colloids_administered", min = 0, initial_value_selected = NULL, max = 100000, step = 100),
                        jh_make_shiny_table_row_function(left_column_percent_width = 60, left_column_label = "Transfusions/Cell Saver", input_type = "switch", input_id = "transfusion", switch_input_on_label = "Yes", switch_input_off_label = "No"),
                        conditionalPanel(condition = "input.transfusion == true",
                                         box(width = 12, 
                                             jh_make_shiny_table_row_function(left_column_percent_width = 60, left_column_label = "Cell Saver Transfused (cc):", input_type = "numeric", input_id = "cell_saver_transfused",initial_value_selected = NULL, min = 0, max = 10000, step = 100),
                                             jh_make_shiny_table_row_function(left_column_percent_width = 60, left_column_label = "pRBC units transfused:", input_type = "numeric", input_id = "prbc_transfused", initial_value_selected = NULL, min = 0, max = 100, step = 1),
                                             jh_make_shiny_table_row_function(left_column_percent_width = 60, left_column_label = "FFP units transfused:", input_type = "numeric", input_id = "ffp_transfused", initial_value_selected = NULL, min = 0, max = 100, step = 1),
                                             jh_make_shiny_table_row_function(left_column_percent_width = 60, left_column_label = "Cryoprecipitate units transfused:", input_type = "numeric", input_id = "cryoprecipitate_transfused", initial_value_selected = NULL, min = 0, max = 100, step = 1),
                                             jh_make_shiny_table_row_function(left_column_percent_width = 60, left_column_label = "Platelet units transfused:", input_type = "numeric", input_id = "platelets_transfused", initial_value_selected = NULL, min = 0, max = 100, step = 1),
                                         )
                        ),
                        hr(),
                        jh_make_shiny_table_row_function(left_column_percent_width = 60, left_column_label = "Intraoperative Complications (including durotomy)?", input_type = "switch", input_id = "intraoperative_complications_true_false"),        
                        conditionalPanel(condition = "input.intraoperative_complications_true_false == true",
                                         box(width = 12, 
                                             br(),
                                             jh_make_shiny_table_row_function(left_column_percent_width = 40, left_column_label = "Select any:", input_type = "checkbox", input_id = "intraoperative_complications_vector", choices_vector = c("Durotomy", "Nerve Root Injury", "Loss of Neuromonitoring Data with Return", "Loss of Neuromonitoring Data without Return"), initial_value_selected = NULL),
                                             jh_make_shiny_table_row_function(left_column_percent_width = 40, left_column_label = "Other Intraoperative Complications:", input_type = "text", input_id = "other_intraoperative_complications", initial_value_selected = NULL)
                                         )
                        ),
                        br(),
                        jh_make_shiny_table_row_function(left_column_percent_width = 20, 
                                                         left_column_label = "Head Positioning:", 
                                                         input_type = "radioGroupButtons", 
                                                         input_id = "head_positioning",
                                                         button_size = "xs", 
                                                         checkboxes_inline = TRUE,
                                                         choices_vector = c("Supine/Lateral", "Proneview Faceplate", "Cranial Tongs", "Halo", "Mayfield"), initial_value_selected = "Cranial Tongs"),
                        br(),
                        jh_make_shiny_table_row_function(left_column_label = "Additional Procedures:",
                                                         input_id = "additional_procedures",  
                                                         left_column_percent_width = 20,
                                                         checkboxes_inline = FALSE,
                                                         input_type = "checkboxGroupButtons", 
                                                         choices_vector = c("Spinal Cord Monitoring",
                                                                            "Intraoperative use of microscope for microdissection",
                                                                            "Use of stereotactic navigation system for pedicle screw placement",
                                                                            # "Application of Cranial Tongs",
                                                                            # "Application of Halo",
                                                                            "Application of Halo for thin skull osteology (e.g. pediatric)",
                                                                            "Removal of tongs or Halo applied by another inidividual",
                                                                            "Irrigation and Debridement",
                                                                            "Open Biopsy of vertebral body",
                                                                            "Repair of dural/CSF leak",
                                                                            "Dural Graft",
                                                                            "Removal of spinal instrumentation",
                                                                            "Exploration of prior spinal fusion",
                                                                            "Open treatment of vertebral fracture",
                                                                            "Other"), 
                                                         initial_value_selected = "Spinal Cord Monitoring"),
                        conditionalPanel(condition = "input.additional_procedures.indexOf('Other') > -1",
                                         tags$table(width = "90%" ,
                                                    jh_make_shiny_table_row_function(left_column_label = "Other Procedures:", input_type = "text", input_id = "additional_procedures_other", left_column_percent_width = 30, font_size = 12, initial_value_selected = "",)
                                         )
                        ),
                        br(),
                        hr(),
                        div(style = "font-size:18px; font-weight:bold; text-align:center", "End of Procedure & Closure Details:"),
                        uiOutput(outputId = "drains_ui"),
                        hr(),
                        # hr(),
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
                    box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Patient Details Table:"),status = "success", collapsible = TRUE, solidHeader = TRUE,
                        tableOutput(outputId = "patient_details_redcap_df")
                    ),
                    box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Procedure Summary Table:"),status = "success", collapsible = TRUE, solidHeader = TRUE,
                        tableOutput(outputId = "summary_table")
                    ),
                    box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Intraoperative Details"),status = "success", collapsible = TRUE,solidHeader = TRUE,
                        tableOutput(outputId = "intraoperative_details_table")
                    ),
                    box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Procedure Specifics"),status = "success", collapsible = TRUE,solidHeader = TRUE,
                        tableOutput(outputId = "redcap_details_wide_df")
                    ),
                    box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Screw Details:"),status = "success", collapsible = TRUE,solidHeader = TRUE,
                        tableOutput("pedicle_screw_details_table")
                    ),
                    box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Interbody implants:"),status = "success", collapsible = TRUE, solidHeader = TRUE,
                        tableOutput(outputId = "interbody_details_table")
                    ),
                    box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "All objects table:"),status = "success", collapsible = TRUE,solidHeader = TRUE,
                        tableOutput(outputId = "all_objects_table")
                    ),
                    ###########################################
            )
        )
    )
    )
# )


###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## SERVER STARTS ###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## 
###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## SERVER STARTS ###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## 
###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## SERVER STARTS ###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## 
###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## SERVER STARTS ###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## 
###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## SERVER STARTS ###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## 

###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## SERVER STARTS ###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## 
###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## SERVER STARTS ###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## 
###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## SERVER STARTS ###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## 
###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## SERVER STARTS ###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## 
###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## SERVER STARTS ###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## 

###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## SERVER STARTS ###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## 
###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## SERVER STARTS ###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## 
###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## SERVER STARTS ###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## 
###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## SERVER STARTS ###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## 
###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## SERVER STARTS ###### ###### ###### ###### ~~~~~~~~~~~~~~~~~~~~~ ###### ###### ###### ########## 

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    ###### ######  -----------   ######### UPDATE TABS ###### ######  -----------   ######### 
    
    observeEvent(input$implants_complete,ignoreNULL = TRUE, ignoreInit = TRUE, {
        updateTabItems(session = session, inputId = "tabs", selected = "implant_details")
    })
    
    observeEvent(input$implant_details_complete,ignoreNULL = TRUE, ignoreInit = TRUE, {
        updateTabItems(session = session, inputId = "tabs", selected = "operative_note")
    })
    
    ###### ######  -----------   ######### UPDATE TABS ###### ######  -----------   ######### 
    
    observeEvent(input$crop_y,ignoreNULL = TRUE, {
        symptom_option_list <- list()
        if(input$crop_y[2] > 0.6){
            symptom_option_list$'Neck & Uppers:' <- c("Neck Pain", "Left Arm Pain", "Right Arm Pain", "Left Arm Weakness", "Right Arm Weakness")
        }
        if(input$crop_y[2] > 0.5 & input$crop_y[1] < 0.65){
            symptom_option_list$'Thoracic:' <- c("Mid Back Pain", "Kyphosis")
        }
        if(input$crop_y[1] < 0.3){
            symptom_option_list$'Lower & Legs:' = c("Low Back Pain", "Left Leg Pain", "Right Leg Pain", "Left Leg Weakness", "Right Leg Weakness")
        }
        
        if(input$crop_y[2] > 0.45){
            symptom_option_list$'Functional:' <- c("Myelopathy: Nurick 1 (Root Symptomts)",
                                                  "Myelopathy: Nurick 2 (Normal gait but symptoms of cord compression)",
                                                  "Myelopathy: Nurick 3 (Gait Abnormalities)",
                                                  "Myelopathy: Nurick 4 (Significant Gait Abnormalities, preventing employment)",
                                                  "Myelopathy: Nurick 5 (Depended on Assistive Device for Ambulating)",
                                                  "Myelopathy: Nurick 6 (Wheelchair bound)")
        }
        if(input$crop_y[1] < 0.5){
            symptom_option_list$'Deformity' = c("Coronal Imbalance", "Sagittal Imbalance", "Chin on chest deformity", "Flatback Syndrome")
        }

            symptom_option_list$'Other' = c("Loss of bladder control", "Bowel Incontinence", "Other Symptoms")

        updatePickerInput(session = session, inputId = "symptoms", label = NULL, choices = symptom_option_list)
    })
    
    ###### ######  -----------   ######### Change to 6 Lumbar Vertebrae ###### ######  -----------   ######### 
    
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
                    l5_labels_anterior_df <<- labels_anterior_df
                    
                    labels_anterior_df <<- labels_anterior_df  %>%
                        add_row(level = "L6", x = 0.5, y = 0.14)
                    
                    spine_png <<- l6_spine_png
                    
                    anterior_spine_jpg <<- l6_anterior_spine_png
                    
                    labels_df <<- l6_labels_df
                    levels_numbered_df <<- l6_levels_numbered_df
            
                    all_implants_constructed_df <<- all_implants_constructed_df %>%
                        filter(vertebral_number < 23.9) %>%
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
            anterior_spine_jpg <<-l5_anterior_spine_jpg 

            labels_df <<- l5_labels_df
            levels_numbered_df <<- l5_levels_numbered_df
            
            labels_anterior_df <<- l5_labels_anterior_df
            
            all_implants_constructed_df <<- l5_all_implants_constructed_df

            anterior_df <<- l5_anterior_df
            jh_get_vertebral_number_function <<- l5_jh_get_vertebral_number_function
            jh_get_vertebral_level_function <<- l5_jh_get_vertebral_level_function
            revision_implants_df <<- l5_revision_implants_df
            open_canal_df <<- l5_open_canal_df
        }
        
    })
    
    #########-------------------################   UPDATE MultiPosition    #########-------------------################     
    observeEvent(list(input$plot_click, all_objects_to_add_list$objects_df), {
        if(any(all_objects_to_add_list$objects_df$approach == "anterior") & any(all_objects_to_add_list$objects_df$approach == "posterior")){
            updateSwitchInput(session = session, inputId = "multiple_approach", value = TRUE)
        }
    })

    #########-------------------################   UPDATE CHOICES    #########-------------------################ 
    #########-------------------################   UPDATE CHOICES    #########-------------------################ 
    #########-------------------################   UPDATE CHOICES    #########-------------------################ 
    
    ################----------  Diagnoses    ------------######################  
    
    observeEvent(input$primary_diagnosis_group, ignoreNULL = TRUE, ignoreInit = TRUE,{
        if(input$primary_diagnosis_group == "spinal_condition"){
            updatePickerInput(session = session, 
                              inputId = "diagnosis_subgroup", 
                              label = NULL, 
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
            updatePickerInput(session = session,
                              inputId = "diagnosis_subgroup",
                              label = NULL, 
                              choices = c("Metastatic Disease to the Spinal column",
                                          "Primary Neoplasm of the spine",
                                          "Other"))
            
        }
        
        if(input$primary_diagnosis_group == "lesion"){
            updatePickerInput(session = session,
                              inputId = "diagnosis_subgroup", 
                              label = NULL, 
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
    
    output$tumor_resection_ui <- renderUI({
        if(input$primary_diagnosis_group == "neoplasm"){
            actionBttn(
                inputId = "add_tumor_resection",
                size = "sm", block = TRUE,
                label = "Add Tumor Resection",
                style = "simple",
                color = "primary"
            )
        }else{
            NULL
        }
    })
    
    output$drains_ui <- renderUI({
        drains_list <- list()
        
        if(any(all_objects_to_add_list$objects_df$approach == "anterior")){
            drains_list$anterior_deep <- jh_make_shiny_table_row_function(left_column_label = "Anterior Deep drains:", 
                                                                          input_type = "awesomeRadio",
                                                                          input_id = "deep_drains_anterior", 
                                                                          left_column_percent_width = 45, 
                                                                          font_size = 14, 
                                                                          initial_value_selected = 0, 
                                                                          choices_vector = c("0", "1", "2", "3", "4", "5"), 
                                                                          checkboxes_inline = TRUE, return_as_full_table = TRUE)
            drains_list$anterior_superficial <- jh_make_shiny_table_row_function(left_column_label = "Anterior Superficial drains:", 
                                                                          input_type = "awesomeRadio",
                                                                          input_id = "superficial_drains_anterior", 
                                                                          left_column_percent_width = 45, 
                                                                          font_size = 14, 
                                                                          initial_value_selected = 0, 
                                                                          choices_vector = c("0", "1", "2", "3", "4", "5"), 
                                                                          checkboxes_inline = TRUE, return_as_full_table = TRUE)
        }
        if(any(all_objects_to_add_list$objects_df$approach == "posterior")){
            drains_list$posterior_deep <- jh_make_shiny_table_row_function(left_column_label = "Posterior Deep drains:", 
                                                                          input_type = "awesomeRadio",
                                                                          input_id = "deep_drains_posterior", 
                                                                          left_column_percent_width = 45, 
                                                                          font_size = 14, 
                                                                          initial_value_selected = 1, 
                                                                          choices_vector = c("0", "1", "2", "3", "4", "5"), 
                                                                          checkboxes_inline = TRUE, return_as_full_table = TRUE)
            drains_list$posterior_superficial <- jh_make_shiny_table_row_function(left_column_label = "Posterior Superficial drains:", 
                                                                                 input_type = "awesomeRadio",
                                                                                 input_id = "superficial_drains_posterior", 
                                                                                 left_column_percent_width = 45, 
                                                                                 font_size = 14, 
                                                                                 initial_value_selected = 1, 
                                                                                 choices_vector = c("0", "1", "2", "3", "4", "5"), 
                                                                                 checkboxes_inline = TRUE, return_as_full_table = TRUE)
        }
        drains_list
    })
    
    
    ################----------  UPDATE CHOICES   ------------######################  
    ################----------  UPDATE CHOICES   ------------######################  
    ################----------  UPDATE CHOICES   ------------######################  
    
    observeEvent(input$spine_approach, ignoreNULL = TRUE, ignoreInit = TRUE, {
        if(input$spine_approach == "Anterior"){
            updateRadioGroupButtons(session = session, 
                                    inputId = "object_to_add",
                                    choices = c(
                                        "Disc Arthroplasty" = "anterior_disc_arthroplasty",
                                        "Diskectomy & Interbody Fusion (No Implant)" = "diskectomy_fusion_no_interbody_device",
                                        "Diskectomy & Fusion + Interbody Implant" = "diskectomy_fusion",
                                        "Decompression + Diskectomy & <br/>Fusion + Interbody Implant" = "decompression_diskectomy_fusion",
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
    
    observeEvent(input$add_intervertebral_cage, ignoreNULL = TRUE, ignoreInit = TRUE,{
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
    
    
    observeEvent(list(input$add_implants, input$spine_approach),ignoreNULL = TRUE, ignoreInit = TRUE, {
        if(input$spine_approach == "Posterior"){
            updateRadioGroupButtons(session = session, 
                                    inputId = "object_to_add", 
                                    choices = c(
                                        "Pedicle Screws" = "pedicle_screw",
                                        "Pelvic Screws" = "pelvic_screw",
                                        "Occipital Screws" = "occipital_screw",
                                        "Transarticular Screw" = "transarticular_screw",
                                        "Pars Screws" = "pars_screw",
                                        "Translaminar Screws" = "translaminar_screw",
                                        "Lateral Mass Screws" = "lateral_mass_screw",
                                        "TP Hook" = "tp_hook",
                                        "Laminar Hook (Downgoing)" = "laminar_downgoing_hook",
                                        "Laminar Hook (Upgoing)" = "laminar_upgoing_hook",
                                        "Pedicle Hook" = "pedicle_hook",
                                        "Tether (Spinous Process)" = "tether",
                                        "Sublaminar Wire" = "sublaminar_wire"                                     ),
                                    checkIcon = list(
                                        yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                                    ),
                                    selected = "pedicle_screw"
            )
        }
    })
    
    observeEvent(input$add_decompressions,ignoreNULL = TRUE, ignoreInit = TRUE, {
        updateRadioGroupButtons(session = session, 
                                inputId = "object_to_add",
                                choices = c(
                                    "Laminoplasty" = "laminoplasty",
                                    "Decompression + Foraminotomies" = "sublaminar_decompression",
                                    "Central Laminectomy" = "laminectomy",
                                    "Laminotomy (Hemilaminectomy)" = "laminotomy",
                                    "Diskectomy" = "diskectomy",
                                    "Transpedicular Decompression" = "transpedicular_approach",
                                    "Costovertebral Decompression" = "costovertebral_approach",
                                    "Lateral Extracavitary Approach (modified)" = "lateral_extracavitary_approach"
                                ),
                                checkIcon = list(
                                    yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                                ),
                                selected = "sublaminar_decompression"
        )
    })
    
    observeEvent(input$add_osteotomies,ignoreNULL = TRUE, ignoreInit = TRUE, {
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
    
    observeEvent(input$add_interbody, ignoreNULL = TRUE, ignoreInit = TRUE,{
        updateRadioGroupButtons(session = session, 
                                inputId = "object_to_add", 
                                choices = c("TLIF" = "tlif",
                                            "PLIF" = "plif", 
                                            "Interbody Fusion, No Implant" = "no_implant_interbody_fusion"),
                                checkIcon = list(
                                    yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                                ),
                                selected = "tlif"
        )
    })
    
    observeEvent(input$add_special_approach,ignoreNULL = TRUE, ignoreInit = TRUE, {
        updateRadioGroupButtons(session = session, 
                                inputId = "object_to_add", 
                                choices = c(                                
                                    "Transpedicular Decompression" = "transpedicular_approach",
                                    "Costovertebral Decompression" = "costovertebral_approach",
                                    "Lateral Extracavitary Approach (modified)" = "lateral_extracavitary_approach", 
                                    "Costotransversectomy" = "costotransversectomy"),
                                checkIcon = list(
                                    yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                                ),
                                selected = "lateral_extracavitary_approach"
        )
    })
    
    observeEvent(input$add_other,ignoreNULL = TRUE, ignoreInit = TRUE, {
        updateRadioGroupButtons(session = session, 
                                inputId = "object_to_add", 
                                choices = c("Vertebroplasty" = "vertebroplasty",
                                            "Vertebral Augmentation (cavity creation, then cement)" = "vertebral_cement_augmentation",
                                            "Structural Allograft Strut" = "structural_allograft"),
                                checkIcon = list(
                                    yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                                ),
                                selected = "vertebroplasty"
        )
    })
    
    output$currently_adding <- renderText(paste("Currently Adding: ", str_to_title(string = str_replace_all(string = input$object_to_add, pattern = "_", replacement = " "))), sep = "")
    
    
    observeEvent(input$add_tumor_resection,ignoreNULL = TRUE, ignoreInit = TRUE, {
        updateRadioGroupButtons(session = session, 
                                inputId = "object_to_add",
                                choices = c(
                                    "Partial Excision of Posterior Elements" = "excision_posterior_elements",
                                    "Partial Excision of Vertebral Body (w/o decompression)" = "excision_body_no_decompression",
                                    "Complete Vertebral Corpectomy" = "excision_complete"
                                ),
                                checkIcon = list(
                                    yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                                ),
                                selected = "intervertebral_cage"
        )
    })
    
    ######### ######### CROSSLINKS ################# #########
    
    observeEvent(list(input$plot_click, input$plot_double_click, input$reset_all),ignoreNULL = TRUE, ignoreInit = TRUE, {
        if(input$spine_approach == "Posterior"){
            crosslink_choices_df <- left_rod_implants_df_reactive() %>%
                union_all(right_rod_implants_df_reactive()) %>%
                arrange(vertebral_number) %>%
                select(level, vertebral_number) %>%
                distinct()
            
            if(nrow(crosslink_choices_df)>2){
                choices_df <- levels_numbered_df %>%
                    mutate(body_interspace = jh_check_body_or_interspace_function(level = level)) %>%
                    filter(body_interspace == "body") %>%
                    filter(vertebral_number >= min(crosslink_choices_df$vertebral_number)) %>%
                    filter(vertebral_number < max(crosslink_choices_df$vertebral_number))
                
                updateCheckboxGroupButtons(session = session, 
                                           inputId = "crosslink_connectors", 
                                           choices = choices_df$level,
                                           label = "Add crosslinks at:",
                                           selected = input$crosslink_connectors,
                                           checkIcon = list(
                                               yes = tags$i(class = "fa fa-check-square",
                                                            style = "color: steelblue"),
                                               no = tags$i(class = "fa fa-square-o",
                                                           style = "color: steelblue"))
                )   
            }
        }
    })
    
    ######### ######### ROD SIZE AND ROD MATERIAL ######### #########
    observeEvent(left_rod_implants_df_reactive(),ignoreNULL = TRUE, ignoreInit = TRUE, {
        if(nrow(left_rod_implants_df_reactive()) > 1){
            updatePickerInput(session = session, 
                              inputId = "left_main_rod_size", 
                              selected = if_else(input$left_main_rod_size == "None", "5.5mm", input$left_main_rod_size)
            )
            updateAwesomeRadio(session = session, 
                               inputId = "left_main_rod_material",
                               inline = TRUE,
                               choices = c("Titanium", "Cobalt Chrome", "Stainless Steel"),
                               selected = if_else(input$left_main_rod_material == "Non-instrumented", "Titanium", input$left_main_rod_material)
            )
        }
    })
    observeEvent(right_rod_implants_df_reactive(),ignoreNULL = TRUE, ignoreInit = TRUE, {
        if(nrow(right_rod_implants_df_reactive()) > 1){
            updatePickerInput(session = session, 
                              inputId = "right_main_rod_size", 
                              selected = if_else(input$right_main_rod_size == "None", "5.5mm", input$right_main_rod_size)
            )
            updateAwesomeRadio(session = session, 
                               inputId = "right_main_rod_material", 
                               inline = TRUE,
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
    
    observeEvent(input$reset_all,ignoreNULL = TRUE, ignoreInit = TRUE, {
        updateSwitchInput(session = session, inputId = "left_supplemental_rods_eligible", value = FALSE)
    })
    
    left_supplement_rod_starts_list_reactive <- reactive({
        all_added_objects_df <- all_objects_to_add_list$objects_df %>%
            select(level, vertebral_number, object, x, y, side) %>%
            filter(side == "left")
        
        starts_list <- jh_cranial_and_caudal_list_for_supplementary_rods_function(all_added_objects_df, osteotomy_site = osteotomy_level_reactive())
        starts_list
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, left_supplement_rod_starts_list_reactive()), ignoreNULL = TRUE, ignoreInit = TRUE,{
        if(nrow(left_rod_implants_df_reactive()) > 2){
            updateSwitchInput(session = session, inputId = "left_supplemental_rods_eligible", value = TRUE)
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, left_supplement_rod_starts_list_reactive()),ignoreNULL = TRUE, ignoreInit = TRUE, {
        if(input$left_supplemental_rods_eligible == TRUE){
            updateSliderTextInput(session = session,
                                  inputId = "left_accessory_rod",
                                  choices = left_supplement_rod_starts_list_reactive()$all_levels,
                                  selected = left_supplement_rod_starts_list_reactive()$accessory_starts)        
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, left_supplement_rod_starts_list_reactive()),ignoreNULL = TRUE, ignoreInit = TRUE, {
        if(input$left_supplemental_rods_eligible == TRUE){
            updateSliderTextInput(session = session,
                                  inputId = "left_satellite_rod",
                                  choices = left_supplement_rod_starts_list_reactive()$all_levels,
                                  selected = left_supplement_rod_starts_list_reactive()$satellite_starts)
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, left_supplement_rod_starts_list_reactive()),ignoreNULL = TRUE, ignoreInit = TRUE, {
        if(input$left_supplemental_rods_eligible == TRUE){
            updateSliderTextInput(session = session,
                                  inputId = "left_intercalary_rod",
                                  choices = left_supplement_rod_starts_list_reactive()$all_levels,
                                  selected = left_supplement_rod_starts_list_reactive()$intercalary_starts)
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, left_supplement_rod_starts_list_reactive()), ignoreNULL = TRUE, ignoreInit = TRUE,{
        if(nrow(left_rod_implants_df_reactive())>3){
            choices_df <- tibble(vertebral_number = seq(from = min(left_rod_implants_df_reactive()$vertebral_number), to = max(left_rod_implants_df_reactive()$vertebral_number), by = 1)) %>%
                left_join(levels_numbered_df)
            
            if(!is.null(osteotomy_level_reactive())){
                updatePickerInput(session = session, inputId = "left_intercalary_junction",
                                  label = "Junction:",
                                  choices = choices_df$level,
                                  selected = osteotomy_level_reactive())
            }else{
                updatePickerInput(session = session, inputId = "left_intercalary_junction",
                                  label = "Junction:",
                                  choices = choices_df$level,
                                  selected = head(x = tail(choices_df$level, 3), 1)
                                  )
            }
            
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, left_supplement_rod_starts_list_reactive()),ignoreNULL = TRUE, ignoreInit = TRUE, {
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

    # ################------------------  Right RODS    ----------------------######################  
    # 
    observeEvent(input$reset_all,ignoreNULL = TRUE, ignoreInit = TRUE, {
        updateSwitchInput(session = session, inputId = "right_supplemental_rods_eligible", value = FALSE)
    })
    
    right_supplement_rod_starts_list_reactive <- reactive({
        all_added_objects_df <- all_objects_to_add_list$objects_df %>%
            select(level, vertebral_number, object, x, y, side) %>%
            filter(side == "right")
        
        jh_cranial_and_caudal_list_for_supplementary_rods_function(all_added_objects_df, osteotomy_site = osteotomy_level_reactive())
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, right_supplement_rod_starts_list_reactive()),ignoreNULL = TRUE, ignoreInit = TRUE, {
        if(nrow(right_rod_implants_df_reactive()) > 2){
            updateSwitchInput(session = session, inputId = "right_supplemental_rods_eligible", value = TRUE)
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, right_supplement_rod_starts_list_reactive()),ignoreNULL = TRUE, ignoreInit = TRUE, {
        if(input$right_supplemental_rods_eligible == TRUE){
            updateSliderTextInput(session = session,
                                  inputId = "right_accessory_rod",
                                  choices = right_supplement_rod_starts_list_reactive()$all_levels,
                                  selected = right_supplement_rod_starts_list_reactive()$accessory_starts)        
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, right_supplement_rod_starts_list_reactive()),ignoreNULL = TRUE, ignoreInit = TRUE, {
        if(input$right_supplemental_rods_eligible == TRUE){
            updateSliderTextInput(session = session,
                                  inputId = "right_satellite_rod",
                                  choices = right_supplement_rod_starts_list_reactive()$all_levels,
                                  selected = right_supplement_rod_starts_list_reactive()$satellite_starts)
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, right_supplement_rod_starts_list_reactive()),ignoreNULL = TRUE, ignoreInit = TRUE, {
        if(input$right_supplemental_rods_eligible == TRUE){
            updateSliderTextInput(session = session,
                                  inputId = "right_intercalary_rod",
                                  choices = right_supplement_rod_starts_list_reactive()$all_levels,
                                  selected = right_supplement_rod_starts_list_reactive()$intercalary_starts)
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, right_supplement_rod_starts_list_reactive()),ignoreNULL = TRUE, ignoreInit = TRUE, {
        if(nrow(right_rod_implants_df_reactive())>3){
            choices_df <- tibble(vertebral_number = seq(from = min(right_rod_implants_df_reactive()$vertebral_number), to = max(right_rod_implants_df_reactive()$vertebral_number), by = 1)) %>%
                left_join(levels_numbered_df)
            
            if(!is.null(osteotomy_level_reactive())){
                updatePickerInput(session = session, inputId = "right_intercalary_junction",
                                  label = "Junction:",
                                  choices = choices_df$level,
                                  selected = osteotomy_level_reactive())
            }else{
                updatePickerInput(session = session, inputId = "right_intercalary_junction",
                                  label = "Junction:",
                                  choices = choices_df$level,
                                  selected = head(x = tail(choices_df$level, 3), 1)
                )
            }
            
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, right_supplement_rod_starts_list_reactive()),ignoreNULL = TRUE, ignoreInit = TRUE, {
        if(input$right_supplemental_rods_eligible == TRUE){
            updateSliderTextInput(session = session,
                                  inputId = "right_linked_rods",
                                  choices = right_supplement_rod_starts_list_reactive()$all_levels,
                                  selected = right_supplement_rod_starts_list_reactive()$linked_starts)
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

    observeEvent(fusion_levels_computed_reactive_df(),ignoreNULL = TRUE, ignoreInit = TRUE, {
        updateCheckboxGroupButtons(session = session, 
                                   inputId = "fusion_levels_confirmed", 
                                   selected = fusion_levels_computed_reactive_df()$level)
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, fusion_levels_computed_reactive_df(), input$plot_click, input$plot_double_click, input$reset_all), ignoreNULL = TRUE, ignoreInit = TRUE, {
        if(nrow(fusion_levels_computed_reactive_df()) > 0){
            updateSwitchInput(session = session, 
                              inputId = "fusion_procedure_performed", 
                              value = TRUE)
        }
    })
    
    observeEvent(list(all_objects_to_add_list$objects_df, input$plot_click, input$plot_double_click, input$reset_all), ignoreNULL = TRUE, ignoreInit = TRUE, {
        if(any((all_objects_to_add_list$objects_df %>% filter(approach == "anterior"))$fusion == "yes")){
            updateSwitchInput(session = session,
                              inputId = "anterior_fusion_performed",
                              value = TRUE)
        }
        
        if(any((all_objects_to_add_list$objects_df %>% filter(approach == "posterior"))$fusion == "yes")){
            updateSwitchInput(session = session, 
                              inputId = "posterior_fusion_performed", 
                              value = TRUE)
        }
    })
    
    ################# ------------  #############  ADDING PROCEDURES & BUILDING REACTIVE DATAFRAMES   ################# ------------  ############# 
    ################# ------------  #############  ADDING PROCEDURES & BUILDING REACTIVE DATAFRAMES   ################# ------------  ############# 
    ################# ------------  #############  ADDING PROCEDURES & BUILDING REACTIVE DATAFRAMES   ################# ------------  ############# 

    #########------------------------- ADD TO PLOT -------------------------###########
    
    all_objects_to_add_list <- reactiveValues()
    
    all_objects_to_add_list$objects_df <- tibble(level = character(),
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
                                                 fixation_uiv_liv = character(),
                                                 object_constructed = list())
    
    
    observeEvent(input$reset_all, ignoreNULL = TRUE, ignoreInit = TRUE, {
        all_objects_to_add_list$objects_df <- all_objects_to_add_list$objects_df %>%
            filter(level == "xxx")
    })
    
    
    ########################################### object DETAILS REACTIVE ###########################################
    #### OBSERVE THE PLOT CLICK AND ADD APPROPRIATE object ####
        object_added_reactive_df <- reactive({
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

        if(input$object_to_add == "decompression_diskectomy_fusion" | input$object_to_add == "diskectomy_fusion"){
            anterior_interbody_df <- implant_df %>%
                select(level, vertebral_number, side, object) %>%
                mutate(object = "anterior_interbody_implant") %>%
                left_join(all_implants_constructed_df)
            
            implant_df <- implant_df %>%
                union_all(anterior_interbody_df)
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
                group_by(level, object_class) %>%
                filter(object_rank == max(object_rank)) %>%
                ungroup() %>%
                select(-object_rank, -object_class)
        }
        
    })
    
    observeEvent(input$plot_double_click, ignoreNULL = TRUE, ignoreInit = TRUE, {
        implant_to_remove_df <- nearPoints(
            # df = object_type_filtered_df,
            df = all_objects_to_add_list$objects_df,
            coordinfo = input$plot_double_click,
            xvar = "x",
            yvar = "y",
            maxpoints = 1,
            threshold = 20
        )
        all_objects_to_add_list$objects_df <- all_objects_to_add_list$objects_df %>%
            anti_join(implant_to_remove_df)
    })
    
    observeEvent(input$crosslink_connectors, ignoreNULL = TRUE, ignoreInit = TRUE, {
        
        all_objects_to_add_list$objects_df <- all_objects_to_add_list$objects_df  %>%
            filter(object != "crosslink") %>%
            union_all(tibble(level = input$crosslink_connectors, object = "crosslink") %>%
                          left_join(all_implants_constructed_df)) %>%
            distinct()
    })
    
    ########################################### Build REACTIVE DATAFRAMES FOR IMPLANTS CONNECTING TO THE RODS ###########################################
    #################### FUSION LEVELS ########################
    fusion_levels_computed_reactive_df <- reactive({
        fusion_levels_estimated_df <- fusion_levels_df_function(all_objects_to_add_df = all_objects_to_add_list$objects_df) %>%
            filter(level != "Sacro-iliac")
        fusion_levels_estimated_df
    })
    
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
    
    
    ################# ------------  #############  REACTIVE UI's    ################# ------------  ############# 
    ################# ------------  #############  REACTIVE UI's    ################# ------------  ############# 
    ################# ------------  #############  REACTIVE UI's    ################# ------------  ############# 
    ################# ------------  #############  REACTIVE UI's    ################# ------------  ############# 
    
    ################------------------  Interbody Details (and generating results)    ----------------------######################  
    ################------------------  Interbody Details (and generating results)    ----------------------######################  
    
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
    
    output$interbody_implants_ui <- renderUI({
        interbody_implants_df <- interbody_df_reactive()
        if(nrow(interbody_implants_df) >0){
            fixedRow(
                column(width = 12, 
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
                mutate(composition = map(.x = composition_label, .f = ~input[[.x]])) %>%
                mutate(device_name = map(.x = device_name_label, .f = ~input[[.x]])) %>%
                mutate(height = map(.x = height_label, .f = ~input[[.x]])) %>%
                mutate(integrated_fixation = map(.x = integrated_fixation_label, .f = ~if_else(is.null(input[[.x]]), "xx", input[[.x]]))) %>%
                mutate(expandable = map(.x = expandable_label, .f = ~if_else(is.null(input[[.x]]), "xx", input[[.x]]))) %>%
                mutate(other = map(.x = other_label, .f = ~if_else(is.null(input[[.x]]), "xx", input[[.x]]))) %>%
                replace_na(list(composition = " ", other = " ", device_name = " ")) %>%
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
                select(level, vertebral_number, approach, object, composition, device_name, height, integrated_fixation, expandable, other, implant_statement) %>%
                mutate(across(everything(), ~ replace_na(.x, " "))) 

            
        }else{
            interbody_details_df <- tibble(level = character(), vertebral_number = double(), object = character(), approach = character(),  composition = character(), implant_statement = character())
        }
        interbody_details_df
    })
    

    ################------------------  Screw Size Details UI  ----------------------######################  
    ################------------------  Screw Size Details UI  ----------------------######################  
    output$screw_details_ui <- renderUI({
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
            NULL
        }
    })
    
    output$screw_types_ui <- renderUI({
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
                       tags$tr(width = "100%", div(style = "font-size:12px; text-align:right",  "M = Monoaxial, U= Uniplanar, P = Polyaxial, Red = Reduction")),
                   )
            )
        }else{
            NULL
        }
    })
    
    
    
    ################## ------------- POSTERIOR BMP UI AND RESULTS ---------------#######################
    ################## ------------- POSTERIOR BMP UI AND RESULTS ---------------#######################
    posterior_bmp_dose_list <- reactiveValues()
    posterior_bmp_dose_list$xxs <- 0
    posterior_bmp_dose_list$xs <- 0
    posterior_bmp_dose_list$sm <- 0
    posterior_bmp_dose_list$m <- 0
    posterior_bmp_dose_list$l <- 0
    
    posterior_bmp_kit_list <- reactiveValues()
    posterior_bmp_kit_list$"XXS Kits:" <- 0
    posterior_bmp_kit_list$"XS Kits:" <- 0
    posterior_bmp_kit_list$"Sm Kits:" <- 0
    posterior_bmp_kit_list$"M Kits:" <- 0
    posterior_bmp_kit_list$"L Kits:" <- 0
    
    observeEvent(input$add_posterior_xxs_bmp_button, {
        posterior_bmp_dose_list$xxs <- posterior_bmp_dose_list$xxs + 1.05
        posterior_bmp_kit_list$"XXS Kits:" <- posterior_bmp_kit_list$"XXS Kits:" + 1
    })
    observeEvent(input$add_posterior_xs_bmp_button, {
        posterior_bmp_dose_list$xs <- posterior_bmp_dose_list$xs + 2.1
        posterior_bmp_kit_list$"XS Kits:" <- posterior_bmp_kit_list$"XS Kits:" + 1
    })
    observeEvent(input$add_posterior_sm_bmp_button, {
        posterior_bmp_dose_list$sm <- posterior_bmp_dose_list$sm + 4.2
        posterior_bmp_kit_list$"Sm Kits:" <- posterior_bmp_kit_list$"Sm Kits:"+1
    })
    observeEvent(input$add_posterior_m_bmp_button, {
        posterior_bmp_dose_list$m <- posterior_bmp_dose_list$m + 8.04
        posterior_bmp_kit_list$"M Kits:" <- posterior_bmp_kit_list$"M Kits:" +1
    })
    observeEvent(input$add_posterior_l_bmp_button, {
        posterior_bmp_dose_list$l <- posterior_bmp_dose_list$l + 12
        posterior_bmp_kit_list$"L Kits:" <- posterior_bmp_kit_list$"L Kits:" + 1
    })
    
    observeEvent(input$reset_posterior_bmp, {
        posterior_bmp_dose_list$xxs <- 0
        posterior_bmp_dose_list$xs <- 0
        posterior_bmp_dose_list$sm <- 0
        posterior_bmp_dose_list$m <- 0
        posterior_bmp_dose_list$l <- 0
        posterior_bmp_kit_list$"XXS Kits:" <- 0
        posterior_bmp_kit_list$"XS Kits:" <- 0
        posterior_bmp_kit_list$"Sm Kits:" <- 0
        posterior_bmp_kit_list$"M Kits:" <- 0
        posterior_bmp_kit_list$"L Kits:" <- 0
    })
    
    output$posterior_bmp_kits <- renderUI({
        bmp_kit_list <- reactiveValuesToList(posterior_bmp_kit_list)
        if(length(bmp_kit_list) > 0){
            bmp_kits_df <- enframe(bmp_kit_list) %>%
                filter(value != 0) %>%
                mutate(statement = as.character(glue("{name} {value}")))
            kit_statement <- glue_collapse(bmp_kits_df$statement, sep = "<br>")
        }else{
            kit_statement <- "none"
        }
        HTML(kit_statement)
    })
    
    posterior_bmp_dose_reactive <- reactive({
        Reduce("+", reactiveValuesToList(posterior_bmp_dose_list))
    })
    output$posterior_bmp_dosage <- renderUI({
        posterior_dose_statement <- glue("{posterior_bmp_dose_reactive()}mg")
        
        (div(style = "font-size:18px; 
                   text-align: center;
                   font-weight: bold;
                   border-style: solid;
                   border-color: burlywood", 
             as.character(posterior_dose_statement)))
    })
    
    ################## ------------- anterior BMP UI AND RESULTS ---------------#######################
    ################## ------------- anterior BMP UI AND RESULTS ---------------#######################
    anterior_bmp_dose_list <- reactiveValues()
    anterior_bmp_dose_list$xxs <- 0
    anterior_bmp_dose_list$xs <- 0
    anterior_bmp_dose_list$sm <- 0
    anterior_bmp_dose_list$m <- 0
    anterior_bmp_dose_list$l <- 0
    
    anterior_bmp_kit_list <- reactiveValues()
    anterior_bmp_kit_list$"XXS Kits:" <- 0
    anterior_bmp_kit_list$"XS Kits:" <- 0
    anterior_bmp_kit_list$"Sm Kits:" <- 0
    anterior_bmp_kit_list$"M Kits:" <- 0
    anterior_bmp_kit_list$"L Kits:" <- 0
    
    observeEvent(input$add_anterior_xxs_bmp_button, {
        anterior_bmp_dose_list$xxs <- anterior_bmp_dose_list$xxs + 1.05
        anterior_bmp_kit_list$"XXS Kits:" <- anterior_bmp_kit_list$"XXS Kits:" + 1
    })
    observeEvent(input$add_anterior_xs_bmp_button, {
        anterior_bmp_dose_list$xs <- anterior_bmp_dose_list$xs + 2.1
        anterior_bmp_kit_list$"XS Kits:" <- anterior_bmp_kit_list$"XS Kits:" + 1
    })
    observeEvent(input$add_anterior_sm_bmp_button, {
        anterior_bmp_dose_list$sm <- anterior_bmp_dose_list$sm + 4.2
        anterior_bmp_kit_list$"Sm Kits:" <- anterior_bmp_kit_list$"Sm Kits:"+1
    })
    observeEvent(input$add_anterior_m_bmp_button, {
        anterior_bmp_dose_list$m <- anterior_bmp_dose_list$m + 8.04
        anterior_bmp_kit_list$"M Kits:" <- anterior_bmp_kit_list$"M Kits:" +1
    })
    observeEvent(input$add_anterior_l_bmp_button, {
        anterior_bmp_dose_list$l <- anterior_bmp_dose_list$l + 12
        anterior_bmp_kit_list$"L Kits:" <- anterior_bmp_kit_list$"L Kits:" + 1
    })
    
    observeEvent(input$reset_anterior_bmp, {
        anterior_bmp_dose_list$xxs <- 0
        anterior_bmp_dose_list$xs <- 0
        anterior_bmp_dose_list$sm <- 0
        anterior_bmp_dose_list$m <- 0
        anterior_bmp_dose_list$l <- 0
        anterior_bmp_kit_list$"XXS Kits:" <- 0
        anterior_bmp_kit_list$"XS Kits:" <- 0
        anterior_bmp_kit_list$"Sm Kits:" <- 0
        anterior_bmp_kit_list$"M Kits:" <- 0
        anterior_bmp_kit_list$"L Kits:" <- 0
    })
    
    output$anterior_bmp_kits <- renderUI({
        bmp_kit_list <- reactiveValuesToList(anterior_bmp_kit_list)
        if(length(bmp_kit_list) > 0){
            bmp_kits_df <- enframe(bmp_kit_list) %>%
                filter(value != 0) %>%
                mutate(statement = as.character(glue("{name} {value}")))
            kit_statement <- glue_collapse(bmp_kits_df$statement, sep = "<br>")
        }else{
            kit_statement <- "none"
        }
        HTML(kit_statement)
    })
    
    anterior_bmp_dose_reactive <- reactive({
        Reduce("+", reactiveValuesToList(anterior_bmp_dose_list))
    })
    output$anterior_bmp_dosage <- renderUI({
        anterior_dose_statement <- glue("{anterior_bmp_dose_reactive()}mg")
        
        (div(style = "font-size:18px; 
                   text-align: center;
                   font-weight: bold;
                   border-style: solid;
                   border-color: burlywood", 
             as.character(anterior_dose_statement)))
    })
    
    
    ################------------------  Screw Size RESULTS  ----------------------######################  
    ################------------------  Screw Size RESULTS  ----------------------######################  
    
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
                mutate(screw_diameter = map(.x = levels_count, .f = ~input[[screw_labels_reactive_df()$screw_size_label[.x]]])) %>%
                select(level, vertebral_number, level_side, screw_diameter, screw_size_label) %>%
                unnest()
            
            screw_length_df <- screw_levels_labels_df %>%
                select(level, vertebral_number, level_side, screw_length_label) %>%
                mutate(screw_length = map(.x = levels_count, .f = ~input[[screw_labels_reactive_df()$screw_length_label[.x]]])) %>%
                select(level, vertebral_number, level_side, screw_length_label, screw_length) %>%
                unnest(screw_length)
            
            screw_type_df <- screw_levels_labels_df %>%
                select(level, vertebral_number, level_side, screw_type_label) %>%
                mutate(screw_type = map(.x = levels_count, .f = ~input[[screw_labels_reactive_df()$screw_type_label[.x]]])) %>%
                select(level, vertebral_number, level_side, screw_type_label, screw_type) %>%
                unnest(screw_type)
            
            screw_size_type_df <- screw_diameter_df %>%
                left_join(screw_length_df) %>%
                left_join(screw_type_df) 
            
            if("screw_diameter" %in% names(screw_size_type_df)){
                screw_size_type_df <- screw_size_type_df
            }else{
                screw_size_type_df$screw_diameter <- ""
            }
            if("screw_length" %in% names(screw_size_type_df)){
                screw_size_type_df <- screw_size_type_df
            }else{
                screw_size_type_df$screw_length <- ""
            }
            
            if("screw_type" %in% names(screw_size_type_df)){
                screw_size_type_df <- screw_size_type_df
            }else{
                screw_size_type_df$screw_type <- "P"
            }
            
            screw_details_df <- screw_size_type_df %>%
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
                mutate(screw_size_type = if_else(screw_size == "xNAmm", paste(screw_type), paste(screw_size, screw_type))) %>%
                mutate(screw_size_type = str_remove_all(string = screw_size_type, pattern = "xmm "))
        }else{
            screw_details_df <- tibble(level = character(), side = character(), screw_diameter = character(), screw_length = character(), screw_type = character(), screw_size = character(), screw_size_type = character())
        }
        screw_details_df
    })

    
    #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE PLOTS    #############~~~~~~~~~~~~~~~~~~~ ##################### 
    #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE PLOTS    #############~~~~~~~~~~~~~~~~~~~ ##################### 
    #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE PLOTS    #############~~~~~~~~~~~~~~~~~~~ ##################### 

    #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE PLOTS    #############~~~~~~~~~~~~~~~~~~~ ##################### 
    #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE PLOTS    #############~~~~~~~~~~~~~~~~~~~ ##################### 
    #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE PLOTS    #############~~~~~~~~~~~~~~~~~~~ ##################### 
    
    ############# ~~~~~~~~~~~~~~ ################## MAKE THE SUMMARY TABLE FOR THE PLOT    ############# ~~~~~~~~~~~~~~ ################## 
     plan_reactive_df <- reactive({
        
        anti_fibrinolytic <- case_when(
            length(input$anti_fibrinolytic) == 0 ~ "--",
            length(input$anti_fibrinolytic) == 1 & "Tranexamic Acid (TXA)" %in% input$anti_fibrinolytic ~ paste(glue("TXA (Load: {input$txa_loading}mg/kg, Maint: {input$txa_maintenance}mg/kg/hr)")),
            length(input$anti_fibrinolytic) > 1 & "Tranexamic Acid (TXA)" %in% input$anti_fibrinolytic ~ paste(glue("{toString(setdiff(x = input$anti_fibrinolytic,
                                                                                                                       y = 'Tranexamic Acid (TXA)'))}, 
                                                                                                                       TXA (Load: {input$txa_loading}mg/kg, Maint: {input$txa_maintenance}mg/kg/hr)")),
            length(input$anti_fibrinolytic) > 0 & ("Tranexamic Acid (TXA)" %in% input$anti_fibrinolytic) == FALSE ~ toString(input$anti_fibrinolytic)
        )
        
        # if(input$spine_approach == "Anterior"){
        #     bmp_size <- case_when(input$anterior_bmp_size == 1.05 ~ "XXS", 
        #                           input$anterior_bmp_size == 2.1 ~ "XS", 
        #                           input$anterior_bmp_size == 4.2 ~ "Sm", 
        #                           input$anterior_bmp_size == 8.4 ~ "M", 
        #                           input$anterior_bmp_size == 12 ~ "L")
        #     bmp_mg_dose <- as.double(input$anterior_bmp_size)*as.double(input$anterior_bmp_number)
        #     bmp_text <- if_else(input$anterior_bmp_number == 0 | is.null(input$anterior_bmp_number), "--", paste(input$anterior_bmp_number, input$anterior_bmp_size, "(", as.character(bmp_mg_dose), "mg)", sep = ""))
        #     bmp <- if_else(bmp_text == "None", "None", paste(bmp_text, "(", as.character(bmp_mg_dose), "mg)"))
        # }else{
        #     bmp_size <- case_when(input$posterior_bmp_size == 1.05 ~ "XXS", 
        #                           input$posterior_bmp_size == 2.1 ~ "XS", 
        #                           input$posterior_bmp_size == 4.2 ~ "Sm", 
        #                           input$posterior_bmp_size == 8.4 ~ "M", 
        #                           input$posterior_bmp_size == 12 ~ "L")  
        #     
        #     bmp_mg_dose <- as.double(input$posterior_bmp_size)*as.double(input$posterior_bmp_number)
        #     bmp_text <- if_else(input$posterior_bmp_number == 0 | is.null(input$posterior_bmp_number), "--", paste(input$posterior_bmp_number, input$posterior_bmp_size, "(", as.character(bmp_mg_dose), "mg)", sep = ""))
        #     bmp <- if_else(bmp_text == "None", "None", paste(bmp_text, "(", as.character(bmp_mg_dose), "mg)"))
            # }

        
        age <- if_else(paste(input$date_of_birth) == "1900-01-01", "", as.character(round(interval(start = paste(input$date_of_birth), end = paste(input$date_of_surgery))/years(1), 0)))
        
        allograft_statement <- if(input$spine_approach == "Anterior"){
            if_else(input$anterior_allograft_amount == 0, "--", paste(input$anterior_allograft_amount, "cc", sep = ""))
        }else{
            if_else(input$posterior_allograft_amount == 0, "--", paste(input$posterior_allograft_amount, "cc", sep = ""))
        }
        
        plan_vector <- c("Patient:" = paste(input$patient_first_name, ",", input$patient_last_name, if_else(age == "", "", paste(age, "yo"), input$sex)), 
                         "Symptoms:" = toString(input$symptoms),
                         "Relevant Hx:" = input$relevant_history, 
                         "---" = "---",
                         "Preop Abx:" = toString(input$preop_antibiotics), 
                         "Antifibrinolytic:" = anti_fibrinolytic,
                         "Allograft" = allograft_statement,
                         # "BMP:" = bmp_text, 
                         "Left Rod:" = if_else(nrow(left_rod_implants_df_reactive()) > 1, paste(input$left_main_rod_size, input$left_main_rod_material, sep = " "), "--"), 
                         "Right Rod:" = if_else(nrow(right_rod_implants_df_reactive()) > 1, paste(input$right_main_rod_size, input$right_main_rod_material, sep = " "), "--"))

        enframe(plan_vector, name = "descriptor", value = "value") %>%
            filter(!is.null(value)) %>%
            filter(value != "--") %>%
            mutate(value = str_squish(string = value)) %>%
            filter(value != "")
        
    })
    
    ######### ~~~~~~~~~~~~~~  ############# POSTERIOR OBJECTS     ######### ~~~~~~~~~~~~~~  ############# 
    ######### ~~~~~~~~~~~~~~  ############# POSTERIOR OBJECTS     ######### ~~~~~~~~~~~~~~  ############# 
    
    geoms_list_posterior <- reactiveValues()
    geoms_list_revision_posterior <- reactiveValues()
     
    ######## ~~~~~~~~~~~ PRIOR DECOMPRESSIONS ---
    observeEvent(input$open_canal, ignoreNULL = TRUE, ignoreInit = TRUE, {
        if(length(input$open_canal) > 0){
            open_df <- open_canal_df %>%
                filter(level %in% input$open_canal)
            
            geoms_list_revision_posterior$open_canal_sf <- ggpattern::geom_sf_pattern(
                data =  st_union(st_combine(st_multipolygon(open_df$object_constructed)), by_feature = TRUE, is_coverage = TRUE),
                pattern_orientation = "radial",
                pattern = "gradient",
                fill = "grey50",
                pattern_fill2 = NA,
                colour = NA)
        }
    }
    )
    ######## ~~~~~~~~~~~ PRIOR Implants ---
    
    observeEvent(list(input$left_revision_implants, input$primary_revision, input$left_revision_implants_removed), ignoreNULL = TRUE, ignoreInit = TRUE, {
        if((length(input$left_revision_implants) > 1 && (input$primary_revision == "Revision"))){
            
            revision_implants_retained_vector <- keep(.x = input$left_revision_implants, .p = ~ .x %in% input$left_revision_implants_removed == FALSE)
            
            if(length(input$left_revision_implants_removed)>0){
                revision_implants_removed_vector <- discard(.x = input$left_revision_implants_removed, .p = ~ .x %in% input$left_revision_implants == FALSE)
                
                left_revision_implants_removed_df <- tibble(level = revision_implants_removed_vector) %>%
                    left_join(revision_implants_df %>% filter(side == "left"))
                
                left_revision_implants_removed_sf <- st_multipolygon(left_revision_implants_removed_df$object_constructed)
                
                geoms_list_revision_posterior$left_revision_implants_removed_sf_geom <- geom_sf(data = left_revision_implants_removed_sf, color = "black", fill = "grey98")  
            }
            
            if(length(revision_implants_retained_vector)>0){
                
                left_revision_implants_constructed_df <- tibble(level = revision_implants_retained_vector) %>%
                    left_join(revision_implants_df %>% filter(side == "left"))
                
                left_revision_implants_sf <- st_multipolygon(left_revision_implants_constructed_df$object_constructed)
                
                left_revision_rod_matrix <- left_revision_implants_constructed_df %>%
                    select(x, y) %>%
                    arrange(rev(y)) %>%
                    distinct() %>%
                    as.matrix()
                
                if(nrow(left_revision_rod_matrix) > 1){
                    left_revision_rod_sf <- st_buffer(st_linestring(left_revision_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
                    geoms_list_revision_posterior$left_revision_implants_sf_geom <- geom_sf(data = st_geometrycollection(x = list(left_revision_implants_sf, left_revision_rod_sf)), fill = "black") 
                }else{
                    geoms_list_revision_posterior$left_revision_implants_sf_geom <- geom_sf(data = left_revision_implants_sf, fill = "black") 
                }
            }
        }
    })

    observeEvent(list(input$right_revision_implants, input$primary_revision, input$right_revision_implants_removed), ignoreNULL = TRUE, ignoreInit = TRUE, {
        if((length(input$right_revision_implants) > 1 && (input$primary_revision == "Revision"))){
            
            revision_implants_retained_vector <- keep(.x = input$right_revision_implants, .p = ~ .x %in% input$right_revision_implants_removed == FALSE)
            
            if(length(input$right_revision_implants_removed)>0){
                revision_implants_removed_vector <- discard(.x = input$right_revision_implants_removed, .p = ~ .x %in% input$right_revision_implants == FALSE)
                
                right_revision_implants_removed_df <- tibble(level = revision_implants_removed_vector) %>%
                    left_join(revision_implants_df %>% filter(side == "right"))
                
                right_revision_implants_removed_sf <- st_multipolygon(right_revision_implants_removed_df$object_constructed)
                
                geoms_list_revision_posterior$right_revision_implants_removed_sf_geom <- geom_sf(data = right_revision_implants_removed_sf, color = "black", fill = "grey98")  
            }
            
            if(length(revision_implants_retained_vector)>0){
                
                right_revision_implants_constructed_df <- tibble(level = revision_implants_retained_vector) %>%
                    left_join(revision_implants_df %>% filter(side == "right"))
                
                right_revision_implants_sf <- st_multipolygon(right_revision_implants_constructed_df$object_constructed)
                
                right_revision_rod_matrix <- right_revision_implants_constructed_df %>%
                    select(x, y) %>%
                    arrange(rev(y)) %>%
                    distinct() %>%
                    as.matrix()
                
                if(nrow(right_revision_rod_matrix) > 1){
                    right_revision_rod_sf <- st_buffer(st_linestring(right_revision_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
                    geoms_list_revision_posterior$right_revision_implants_sf_geom <- geom_sf(data = st_geometrycollection(x = list(right_revision_implants_sf, right_revision_rod_sf)), fill = "black") 
                }else{
                    geoms_list_revision_posterior$right_revision_implants_sf_geom <- geom_sf(data = right_revision_implants_sf, fill = "black") 
                } 
            }
        }
    })

    ######## ~~~~~~~~~~~ POSTERIOR GEOMS  ---
    observeEvent(list(input$plot_click,
                      input$plot_double_click,
                      input$reset_all,
                      all_objects_to_add_list$objects_df), {
        all_posterior_df <- all_objects_to_add_list$objects_df %>%
            filter(approach == "posterior")
                      
        geoms_list_posterior$geoms <- jh_make_posterior_geoms_function(all_posterior_objects_df = all_posterior_df, plot_with_patterns = input$plot_with_patterns_true)
        })

    rods_list <- reactiveValues()
    
    observeEvent(list(input$plot_click, 
                      input$plot_double_click,
                      input$reset_all,
                      left_rod_implants_df_reactive(),
                      input$add_left_accessory_rod, 
                      input$left_accessory_rod,
                      input$add_left_satellite_rod,
                      input$left_satellite_rod,
                      input$add_left_intercalary_rod,
                      input$left_intercalary_rod,
                      input$left_intercalary_junction,
                      input$add_left_linked_rods,
                      input$left_linked_rods),{
                          ##########RODS ############
                          ############# Left ROD #################
                          left_rods_connectors_list <- list()
                          
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
                              rods_list$left_rod_list_sf_geom <- geom_sf(data = st_multipolygon(left_rods_connectors_list$rod_list), alpha = 0.75)
                          }else{
                              rods_list$left_rod_list_sf_geom <- NULL
                          }
                          
                          if(length(left_rods_connectors_list$connector_list) > 0){
                              rods_list$left_connector_list_sf_geom <- geom_sf(data = st_multipolygon(left_rods_connectors_list$connector_list), alpha = 0.75)
                          }else{
                              rods_list$left_connector_list_sf_geom <- NULL
                          }
                          if(nrow(left_rod_implants_df_reactive()) == 0){
                              rods_list$left_connector_list_sf_geom <- NULL
                              rods_list$left_rod_list_sf_geom <- NULL
                              }
                      })
    
    observeEvent(list(input$plot_click, 
                      input$plot_double_click,
                      input$reset_all,
                      right_rod_implants_df_reactive(),
                      input$add_right_accessory_rod, 
                      input$right_accessory_rod,
                      input$add_right_satellite_rod,
                      input$right_satellite_rod,
                      input$add_right_intercalary_rod,
                      input$right_intercalary_rod,
                      input$right_intercalary_junction,
                      input$add_right_linked_rods,
                      input$right_linked_rods),ignoreNULL = TRUE, ignoreInit = TRUE,{
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
            rods_list$right_rod_list_sf_geom <- geom_sf(data = st_multipolygon(right_rods_connectors_list$rod_list), alpha = 0.75)
        }else{
            rods_list$right_rod_list_sf_geom <- NULL
        }
        
        if(length(right_rods_connectors_list$connector_list) > 0){
            rods_list$right_connector_list_sf_geom <- geom_sf(data = st_multipolygon(right_rods_connectors_list$connector_list), alpha = 0.75)
        }else{
            rods_list$right_connector_list_sf_geom <- NULL
        }
        
        if(nrow(right_rod_implants_df_reactive()) == 0){
            rods_list$right_rod_list_sf_geom <- NULL
            rods_list$right_connector_list_sf_geom <- NULL
        }
    })
    
    observeEvent(input$crosslink_connectors, ignoreNULL = TRUE, ignoreInit = TRUE, {
        rods_list$crosslinks <- geom_sf(data = st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "crosslink"))$object_constructed), alpha = 0.75, fill = "gold") 
    })

    ######### ~~~~~~~~~~~~~~  ############# ANTERIOR OBJECTS     ######### ~~~~~~~~~~~~~~  ############# 
    ######### ~~~~~~~~~~~~~~  ############# ANTERIOR OBJECTS     ######### ~~~~~~~~~~~~~~  ############# 

    geoms_list_anterior_diskectomy <- reactiveValues()
    geoms_list_anterior_interbody <- reactiveValues()
    geoms_list_anterior_instrumentation <- reactiveValues()
    
    observeEvent(list(input$plot_click,
                      input$plot_double_click,
                      input$reset_all,
                      all_objects_to_add_list$objects_df), {
                          
                              anterior_df <- all_objects_to_add_list$objects_df %>%
                                  filter(approach == "anterior")
                              
                              anterior_geoms_list <- jh_make_anterior_geoms_function(all_anterior_objects_df = anterior_df)
                              
                              geoms_list_anterior_diskectomy$geoms <- anterior_geoms_list$geoms_list_anterior_diskectomy
                              geoms_list_anterior_interbody$geoms <- anterior_geoms_list$geoms_list_anterior_interbody
                              geoms_list_anterior_instrumentation$geoms <- anterior_geoms_list$geoms_list_anterior_instrumentation
                              
                          })
    
    
    ######### ~~~~~~~~~~~~~~  ############# MAKE REACTIVE PLOT    ######### ~~~~~~~~~~~~~~  ############# 
    ######### ~~~~~~~~~~~~~~  ############# MAKE REACTIVE PLOT    ######### ~~~~~~~~~~~~~~  ############# 
    spine_plan_plot <- reactive({
        x_left_limit <- 0.3 - input$label_text_offset/100
        x_right_limit <- 1-x_left_limit
        plot_top_y <- input$crop_y[2]
        y_spacing <- 0.025*input$crop_y[2]
        
        if(input$plot_summary_table == TRUE){
            y_start_with_text <- plot_top_y + nrow(plan_reactive_df())*y_spacing
            plan_table <- tibble(x = 0.5, y = y_start_with_text, tb = list(plan_reactive_df()))
            
            plan_table_geom <- geom_table(data = plan_table, aes(label = tb, x = x, y = y), size = (input$label_text_size - 3)/2.85, table.colnames = FALSE) 
        }else{
            y_start_with_text <- plot_top_y
            # plan_table <- tibble(x = 0.5, y = y_start_with_text, tb = list(plan_reactive_df()))
            plan_table_geom <- geom_sf(data = NULL)
        }
        
        if(input$lumbar_vertebrae_6 == TRUE){
            l6_statement <- "Note: 6 Lumbar Vertebrae"
        }else{
            l6_statement <- " "
        }

        if(input$spine_approach == "Anterior"){
            labels_anterior_cropped_df <- labels_anterior_df %>%
                filter(between(y, input$crop_y[1], y_start_with_text)) %>% 
                mutate(x_left = x_left_limit + 0.05) %>%
                mutate(x_right = x_right_limit - 0.05) %>%
                select(level, x_left, x_right, y)
            
            labels_anterior_cropped_df <- labels_anterior_cropped_df %>%
                union_all(tibble(level = " ", 
                                 x_left = min(labels_anterior_cropped_df$x_left) - 0.03,
                                 x_right = max(labels_anterior_cropped_df$x_right) + 0.03, 
                                 y = min(labels_anterior_cropped_df$y) - 0.075)) %>%
                union_all(tibble(level = " ", 
                                 x_left = min(labels_anterior_cropped_df$x_left) - 0.03,
                                 x_right = max(labels_anterior_cropped_df$x_right) + 0.03, 
                                 y = max(labels_anterior_cropped_df$y) + 0.05))
            
            
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
                    text = labels_anterior_cropped_df$level,
                    x = labels_anterior_cropped_df$x_left,
                    y = labels_anterior_cropped_df$y,
                    size = input$label_text_size,
                    fontface = "bold"
                ) +
                draw_text(
                    text = labels_anterior_cropped_df$level,
                    x = labels_anterior_cropped_df$x_right,
                    y = labels_anterior_cropped_df$y,
                    size = input$label_text_size,
                    fontface = "bold"
                ) +
                reactiveValuesToList(geoms_list_anterior_diskectomy) +
                reactiveValuesToList(geoms_list_anterior_interbody) +
                reactiveValuesToList(geoms_list_anterior_instrumentation) +
                geom_sf(data = NULL) + #this is needed so that plot starts cropped correctly 
                plan_table_geom 

        }else{
            ### POSTERIOR
            labels_posterior_df <- labels_df %>%
                filter(between(y, input$crop_y[1], y_start_with_text)) %>%
                mutate(x_left = x_left_limit + 0.05) %>%
                mutate(x_right = x_right_limit - 0.05) %>%
                select(-vertebral_number)
            
            labels_posterior_df <- labels_posterior_df %>%
                union_all(tibble(level = " ", 
                                 x_left = min(labels_posterior_df$x_left) - 0.03,
                                 x_right = max(labels_posterior_df$x_right) + 0.03, 
                                 y = min(labels_posterior_df$y) - 0.03)) %>%
                union_all(tibble(level = " ", 
                                 x_left = min(labels_posterior_df$x_left) - 0.03,
                                 x_right = max(labels_posterior_df$x_right) + 0.03, 
                                 y = max(labels_posterior_df$y) + 0.03))

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
                reactiveValuesToList(geoms_list_revision_posterior) +
                reactiveValuesToList(geoms_list_posterior) +
                reactiveValuesToList(rods_list) +
                draw_text(
                    text = labels_posterior_df$level,
                    x = labels_posterior_df$x_left,
                    y = labels_posterior_df$y,
                    size = input$label_text_size,
                    fontface = "bold"
                ) +
                draw_text(
                    text = labels_posterior_df$level,
                    x = labels_posterior_df$x_right,
                    y = labels_posterior_df$y,
                    size = input$label_text_size,
                    fontface = "bold"
                ) +
                plan_table_geom +
                annotate("text", x = 0.5, y = input$crop_y[1] + 0.01, label = l6_statement) 
                # ylim(input$crop_y[1], y_start_with_text)  +
                # xlim(x_left_limit, x_right_limit)
        }
    })

    ##################### ~~~~~~~~~~~~~~~~ RENDER PLOTS ~~~~~~~~~~~~~~~~~~~ ##################
    ##################### ~~~~~~~~~~~~~~~~ RENDER PLOTS ~~~~~~~~~~~~~~~~~~~ ##################
    ##################### ~~~~~~~~~~~~~~~~ RENDER PLOTS ~~~~~~~~~~~~~~~~~~~ ##################
    
    output$spine_plan <- renderPlot({
        spine_plan_plot() 
    })
    
    observeEvent(input$lumbar_vertebrae_6, {
        output$spine_plan <- renderPlot({
            spine_plan_plot()
        })
    })

    output$spine_plot_for_implants_tab <- renderPlot({
         spine_plan_plot()
    })
    
    
    ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
    ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
    ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  

    ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
    ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
    ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
    
    ################------------------  FIRST UPDATE THE OPTIONS USING THE DETAILS ALREADY INPUTTED    ----------------------######################  
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
                                       selected = append(input$additional_procedures, "Exploration of prior spinal fusion"))
        }
    })
    
    observeEvent(list(input$left_revision_implants, input$left_revision_implants_removed), {
        
        if(length(input$left_revision_implants_removed)>0){
            revision_implants_removed_vector <- discard(.x = input$left_revision_implants_removed, .p = ~ .x %in% input$left_revision_implants == FALSE)
            
            updateAwesomeCheckboxGroup(session = session, 
                                       inputId = "left_revision_implants_removed", 
                                       selected = revision_implants_removed_vector)
        }
        
 
        
    })
    
    # observeEvent(list(input$left_revision_implants_removed, input$right_revision_implants_removed), {
    #     if(length(input$left_revision_implants_removed) > 0 | length(input$left_revision_implants_removed) > 0){
    #         updateCheckboxGroupButtons(session = session, 
    #                                    inputId = "additional_procedures", 
    #                                    selected = append(input$additional_procedures, "Removal of spinal instrumentation"))
    #     }
    # })
    
    observeEvent(input$fusion_procedure_performed, {
        if(input$fusion_procedure_performed == TRUE){
            updateAwesomeCheckboxGroup(session = session,
                                       inputId = "posterior_bone_graft", 
                                       selected = append(input$posterior_bone_graft, "Morselized Allograft"))
        }
    })
    
    observeEvent(input$diagnosis_subgroup, {
        if(any(str_detect(string = input$diagnosis_subgroup, pattern = "Fracture"))){
            updateCheckboxGroupButtons(session = session, 
                                       inputId = "additional_procedures", 
                                       selected = append(input$additional_procedures, "Open treatment of vertebral fracture"))
        }
    })
    
    
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
    
    additional_procedures_vector_reactive <- reactive({
        # additional_procedures_vector <-list()
        additional_procedures_vector <- discard(input$additional_procedures, .p = ~ (.x == "Other"))

        age <- as.double(if_else(paste(input$date_of_birth) == "1900-01-01", "0", as.character(round(interval(start = paste(input$date_of_birth), end = paste(input$date_of_surgery))/years(1), 0))))

        if(length(input$left_revision_implants_removed) > 0 | length(input$right_revision_implants_removed) > 0){
            additional_procedures_vector$removal_instrumentation <- "Removal of spinal instrumentation"
        }

        if(any(str_detect(string = input$head_positioning, pattern = "Tongs"))){
            additional_procedures_vector$head_positioning <- "Application of Cranial Tongs"
        }
        if(any(str_detect(string = input$head_positioning, pattern = "Mayfield"))){
            additional_procedures_vector$head_positioning <- "Application of Cranial Tongs using Mayfield head holder"
        }

        if(any(str_detect(string = input$head_positioning, pattern = "Halo"))){
            if(age < 18 & age > 0){
                additional_procedures_vector$head_positioning <- "Application of Halo for thin skull osteology (pediatric)"
            }else{
                additional_procedures_vector$head_positioning <- "Application of Halo"
            }
        }
        as_vector(additional_procedures_vector)
    })

    
    op_note_text_reactive <- reactive({
        ################# COMPLICATIONS ################3
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
        
        ### FLuids/Transfusions ###if_else(is.na(input$ebl), "See anesthesia records", paste(input$ebl)),
        fluids_transfusions_list <- list(crystalloids_statement =if_else(is.na(input$crystalloids_administered), glue("none"), glue("-Crystalloids: {input$cell_saver_transfused}cc")),
                                         colloids_statement =if_else(is.na(input$colloids_administered), glue("none"), glue("-Colloids: {input$colloids_administered}cc")) ,
                                         cell_saver_transfused_statement =if_else(is.na(input$cell_saver_transfused), glue("none"), glue("-Cell Saver: {input$cell_saver_transfused}cc")),
                                         prbc_transfused_statement =if_else(is.na(input$prbc_transfused), glue("none"), glue("-pRBC's transfused: {input$prbc_transfused} {if_else(input$prbc_transfused >1, 'units', 'unit')}")),
                                         ffp_transfused_statement =if_else(is.na(input$ffp_transfused), glue("none"), glue("-FFP transfused: {input$ffp_transfused} {if_else(input$ffp_transfused >1, 'units', 'unit')}")), 
                                         cryo_transfused_statement =if_else(is.na(input$cryoprecipitate_transfused), glue("none"), glue("-Cryoprecipitate transfused: {input$cryoprecipitate_transfused} {if_else(input$cryoprecipitate_transfused >1, 'units', 'unit')}")), 
                                         platelets_transfused_statement =if_else(is.na(input$platelets_transfused), glue("none"), glue("-Platelets transfused: {input$platelets_transfused} {if_else(input$platelets_transfused >1, 'units', 'unit')}")))
        
        
        fluids_transfusions_list <- discard(.x = fluids_transfusions_list, .p = ~ .x == "none")
        
        if(length(discard(.x = fluids_transfusions_list, .p = ~ .x == "none")) == 0){
            fluids_transfusions_statement <- "See Anesthesia Records"
        }else{
            fluids_transfusions_statement <- glue_collapse(fluids_transfusions_list, sep = '\n')
        }
        
        
        ####### BUILD PROCEDURE PARAGRAPHS ########
        posterior_approach_objects_df <- all_objects_to_add_list$objects_df %>%
            filter(approach == "posterior")  %>%
            select(-object_constructed)
        
        
        if(nrow(interbody_details_df_reactive()) > 0){
            posterior_approach_objects_df <- posterior_approach_objects_df %>%
                left_join(interbody_details_df_reactive() %>% select(level, approach, object, implant_statement)) %>%
                replace_na(list(implant_statement = " "))
        }else{
            posterior_approach_objects_df %>%
                mutate(implant_statement = " ")
        }

        anterior_approach_objects_df <- all_objects_to_add_list$objects_df %>%
            filter(approach == "anterior") %>%
            select(-object_constructed)
        
        procedure_results_list <- list()
        
        if(length(input$fusion_levels_confirmed)>0){
            fusions_df <- tibble(level = input$fusion_levels_confirmed) %>%
                left_join(levels_numbered_df)
        }else{
            fusions_df <- tibble(level = character(), vertebral_number = double(), object = character())
        }
        
    

        if(nrow(posterior_approach_objects_df) > 0){

            
            if(nrow(screw_details_results_reactive_df()) > 0){
                posterior_screws_df <- posterior_approach_objects_df %>%
                    filter(approach == "posterior") %>%
                    filter(str_detect(object, "screw")) %>%
                    left_join(screw_details_results_reactive_df()) %>%
                    select(level, approach, side, object, screw_size_type) %>%
                    replace_na(list(screw_size_type = " "))
                
                posterior_approach_objects_df <- posterior_approach_objects_df%>%
                    left_join(posterior_screws_df) %>%
                    replace_na(list(screw_size_type = " "))
            }
            
            procedure_results_list_posterior <- list()
            
            procedure_results_list_posterior <- op_note_posterior_function(all_objects_to_add_df = posterior_approach_objects_df,
                                                                           fusion_levels_df = fusions_df,
                                                                           head_position = input$head_positioning,
                                                                           revision_decompression_vector = input$open_canal,
                                                                           left_main_rod_size = input$left_main_rod_size,
                                                                           left_main_rod_material = input$left_main_rod_material,
                                                                           right_main_rod_size = input$right_main_rod_size,
                                                                           right_main_rod_material = input$right_main_rod_material,
                                                                           additional_rods_statement = added_rods_statement_reactive(),
                                                                           antibiotics = input$preop_antibiotics,
                                                                           additional_procedures_vector = additional_procedures_vector_reactive(),
                                                                           prior_fusion_levels_vector = input$prior_fusion_levels,
                                                                           instrumentation_removal_vector = unique(c(input$left_revision_implants_removed, input$right_revision_implants_removed)),
                                                                           bmp = posterior_bmp_dose_reactive(),
                                                                           bone_graft_vector = input$posterior_bone_graft,
                                                                           morselized_allograft = input$posterior_allograft_amount,
                                                                           morselized_autograft_separate = 0,
                                                                           deep_drains = input$deep_drains_posterior,
                                                                           superficial_drains = input$superficial_drains_posterior,
                                                                           end_procedure_details = input$additional_end_procedure_details,
                                                                           closure = input$closure_details,
                                                                           dressing = input$dressing_details, 
                                                                           multiple_position_procedure = input$multiple_approach)
            
            
        }
        
        if(nrow(anterior_approach_objects_df) > 0){
            
            if(nrow(interbody_details_df_reactive())>0){
                anterior_approach_objects_df <- anterior_approach_objects_df %>%
                    left_join(interbody_details_df_reactive() %>% select(level, approach, object, implant_statement)) %>%
                    replace_na(list(implant_statement = " ")) 
            }else{
                anterior_approach_objects_df <- anterior_approach_objects_df %>%
                    mutate(implant_statement = " ")
            }
            
            procedure_results_list_anterior <- op_note_anterior_function(all_objects_to_add_df = anterior_approach_objects_df,
                                                                         anterior_approach_laterality = input$approach_specified_anterior,
                                                                         microscope_statement = "none", 
                                                                         antibiotics = input$preop_antibiotics, 
                                                                         additional_procedures_vector = input$additional_procedures, 
                                                                         bmp = anterior_bmp_dose_reactive(),
                                                                         # bmp = if_else(input$anterior_bmp_number == 0, 0, (as.double(input$anterior_bmp_number)*as.double(input$anterior_bmp_size))), 
                                                                         bone_graft_vector = input$anterior_bone_graft,
                                                                         morselized_allograft = input$anterior_allograft_amount,
                                                                         # morselized_autograft_separate = 0,
                                                                         # structural_allograft_location = input$structural_allograft_location,
                                                                         # additional_procedural_details = input$additional_procedures, 
                                                                         deep_drains = input$deep_drains_anterior,
                                                                         superficial_drains = input$superficial_drains_anterior,
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
                                               if_else(is.na(input$ebl), "See anesthesia records", paste(input$ebl)),
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
    
    #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE TABLES    #############~~~~~~~~~~~~~~~~~~~ ##################### 
    #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE TABLES    #############~~~~~~~~~~~~~~~~~~~ ##################### 
    #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE TABLES    #############~~~~~~~~~~~~~~~~~~~ ##################### 
    
    #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE TABLES    #############~~~~~~~~~~~~~~~~~~~ ##################### 
    #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE TABLES    #############~~~~~~~~~~~~~~~~~~~ ##################### 
    #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE TABLES    #############~~~~~~~~~~~~~~~~~~~ #####################
    
    ############# ~~~~~~~~~~~~~ Patient Details TABLE ('patient_details' instrument in redcap) ~~~~~~~~~~~~~~~~~~~ ######################
    
    patient_details_redcap_df_reactive <- reactive({
        patient_details_df <- tibble(last_name = input$patient_last_name,
               first_name = input$patient_first_name,
               date_of_birth = if_else(paste(input$date_of_birth) == "1900-01-01", "--", paste(input$date_of_birth)),
               sex = input$sex)
        
        patient_details_df
    })
    
    ######## Render "Procedure Summary Table:"    ######## 
    output$patient_details_redcap_df <- renderTable({
        row_1 <- patient_details_redcap_df_reactive() %>%
            slice(1) %>%
            as.character()
        
        tibble(Variable = names(patient_details_redcap_df_reactive()), 
               Result = row_1) 

    })
    
    
    ############# ~~~~~~~~~~~~~ PROCEDURE SUMMARY TABLE ~~~~~~~~~~~~~~~~~~~ ######################
    
    summary_table_for_redcap_reactive <- reactive({
        surgery_details_list <- list()
        
        ##########   date_of_surgery #############
        surgery_details_list$date_of_surgery <- as.character(input$date_of_surgery)
        
        ##########   age #############
        surgery_details_list$age <- if_else(paste(input$date_of_birth) == "1900-01-01", "--", as.character(round(interval(start = paste(input$date_of_birth), end = paste(input$date_of_surgery))/years(1), 0)))
        
        ##########   attending #############
        surgery_details_list$attending <- input$primary_surgeon
        
        ##########   assisting #############
        surgery_details_list$assisting <- input$surgical_assistants
        
        ##########   symptoms #############
        if(length(input$symptoms)>0){
           surgery_details_list$symptoms <- glue_collapse(str_to_lower(input$symptoms), sep = "; ") 
        }

        ##########   diagnosis_category #############
        surgery_details_list$diagnosis_category <- input$primary_diagnosis_group
        
        ##########   diagnosis #############
        if(length(input$diagnosis_subgroup) >0){
            surgery_details_list$diagnosis <- glue_collapse(str_to_lower(input$diagnosis_subgroup), sep = "; ")
        }
        
        
        ##########   prior_fusion_levels #############
        if(length(input$prior_fusion_levels)>0){
            surgery_details_list$prior_fusion_levels <- glue_collapse(input$prior_fusion_levels, sep = "; ")   
        }
        
        # ##########   levels_instrumentation_removed #############
        # if(length(input$left_revision_implants_removed) > 0){
        #     removal_df <- tibble(levels_removed = input$left_revision_implants_removed)
        # }
        if(length(input$left_revision_implants_removed) > 0 | length(input$right_revision_implants_removed) > 0){
            removal_df <- tibble(levels_removed = input$left_revision_implants_removed) %>%
                union_all(tibble(levels_removed = input$right_revision_implants_removed)) %>%
                distinct() %>%
                mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = levels_removed)) %>%
                arrange(vertebral_number)

            surgery_details_list$levels_instrumentation_removed <- glue_collapse(removal_df$levels_removed, sep = ",")

        }

        ##########   staged_procedure #############
        surgery_details_list$staged_procedure <- if_else(input$staged_procedure == TRUE, "Yes", "No")
        
        ##########   stage_number #############
        if(input$staged_procedure == TRUE){
            surgery_details_list$stage_number <- input$stage_number
        }
        
        
        ##########   approach #############
        surgery_details_list$main_approach <- case_when(
            any(all_objects_to_add_list$objects_df$approach == "anterior") & any(all_objects_to_add_list$objects_df$approach == "posterior")  ~ "combined anterior posterior",
            any(all_objects_to_add_list$objects_df$approach == "anterior") == FALSE & any(all_objects_to_add_list$objects_df$approach == "posterior")  ~ "posterior",
            any(all_objects_to_add_list$objects_df$approach == "anterior") & any(all_objects_to_add_list$objects_df$approach == "posterior") == FALSE  ~ "anterior"
        )
        if(str_detect(surgery_details_list$main_approach, "anterior")){
            surgery_details_list$anterior_approach <- input$approach_specified_anterior
        }
        if(str_detect(surgery_details_list$main_approach, "posterior")){
            surgery_details_list$posterior_approach <- input$approach_specified_posterior
        }

        ##########   fusion performed #############
        surgery_details_list$fusion <- if_else(length(input$fusion_levels_confirmed) > 0, "yes", "no")
        
        ##########   number of fused vertebrae  #############
        surgery_details_list$number_of_fusion_levels <- if_else(length(input$fusion_levels_confirmed) >0, paste(length(input$fusion_levels_confirmed) + 1), "0")
        
        ##########   interspaces_fused #############
        if(length(input$fusion_levels_confirmed) >0){
            surgery_details_list$interspaces_fused <- glue_collapse(input$fusion_levels_confirmed, sep = "; ")
        }

        ##########   interbody_fusion #############
        surgery_details_list$interbody_fusion <- if_else("yes" %in% all_objects_to_add_list$objects_df$interbody_fusion, "yes", "no")
        
        ##########   interbody_fusion_levels #############
        if(surgery_details_list$interbody_fusion == "yes"){
            interbody_fusion_df <- all_objects_to_add_list$objects_df %>%
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
        
        
        ##########   interspaces decompressed  #############
        decompressions_df <- all_objects_to_add_list$objects_df %>%
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

        ##########   UIV  #############
        all_vertebrae_fixation_df <- all_objects_to_add_list$objects_df %>%
            filter(level != "S2AI") %>%
            filter(level != "Iliac") %>%
            filter(fixation_uiv_liv == "yes") %>%
            select(level, vertebral_number, body_interspace) %>%
            distinct() %>%
            arrange(vertebral_number)

        if(nrow(all_vertebrae_fixation_df) > 0){
            surgery_details_list$uiv <- (all_vertebrae_fixation_df %>% filter(vertebral_number == min(vertebral_number)) %>% select(level) %>% distinct())$level

            if(jh_check_body_or_interspace_function(surgery_details_list$uiv) == "interspace"){
                surgery_details_list$uiv <- jh_get_cranial_caudal_interspace_body_list_function(level = surgery_details_list$uiv)$cranial_level
            }
        }else{
            surgery_details_list$uiv <- "not instrumented"
        }

        ##########   LIV  #############
        if(nrow(all_vertebrae_fixation_df) > 0){
            surgery_details_list$liv <- (all_vertebrae_fixation_df %>% filter(vertebral_number == max(vertebral_number)) %>% select(level) %>% distinct())$level

            if(jh_check_body_or_interspace_function(surgery_details_list$liv) == "interspace"){
                surgery_details_list$uiv <- jh_get_cranial_caudal_interspace_body_list_function(level = surgery_details_list$liv)$caudal_level
            }
        }else{
            surgery_details_list$liv <- "not instrumented"
        }

        ##########   UPPER & LOWER TREATED  #############

        spine_treated_df <- all_objects_to_add_list$objects_df %>%
            filter(level != "S2AI") %>%
            filter(level != "Iliac") %>%
            filter(level != "Occiput")

        spine_treated <- if_else(nrow(spine_treated_df) > 0, TRUE, FALSE)

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
        surgery_details_list$pelvic_fixation <- if_else(any(str_detect(string = all_objects_to_add_list$objects_df$object, pattern = "pelvic_screw")), "yes", "no")
        
        if(surgery_details_list$pelvic_fixation == "yes"){
            surgery_details_list$pelvic_fixation_screws <- glue_collapse((all_objects_to_add_list$objects_df %>% filter(object == "pelvic_screw"))$level, sep = "; ")
        }

        ##########   THREE COLUMN OSTEOTOMY #############
        surgery_details_list$three_column_osteotomy <- if_else(any(all_objects_to_add_list$objects_df$object == "grade_3") |
                                                  any(all_objects_to_add_list$objects_df$object == "grade_4") |
                                                  any(all_objects_to_add_list$objects_df$object == "grade_5") |
                                                  any(all_objects_to_add_list$objects_df$object == "grade_6"), "yes", "no")
        if(surgery_details_list$three_column_osteotomy == "yes"){
            surgery_details_list$three_column_osteotomy_level <- glue_collapse(x = (all_objects_to_add_list$objects_df %>%
                                                                                        filter(object == "grade_3" | object == "grade_4" | object == "grade_5") %>%
                                                                                        select(level) %>%
                                                                                        distinct() %>%
                                                                                        as_vector()), sep = "; ")
            
        }
        
        ##########   RODS  #############
        
        if(str_detect(surgery_details_list$main_approach, "posterior")){
            surgery_details_list$left_rod <- if_else(input$left_main_rod_size == "None", "None", paste(input$left_main_rod_size, input$left_main_rod_material))
            surgery_details_list$right_rod <- if_else(input$right_main_rod_size == "None", "None", paste(input$right_main_rod_size, input$right_main_rod_material))
            
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
            
            if(nrow(supplemental_rods_df %>% filter(side == "left")) >0){
                surgery_details_list$left_supplemental_rods <- glue_collapse((supplemental_rods_df %>% filter(side == "left"))$supplemental_rod, sep = "; ")
            }else{
                surgery_details_list$left_supplemental_rods <- "none"
            }
            
            if(nrow(supplemental_rods_df %>% filter(side == "right")) >0){
                surgery_details_list$right_supplemental_rods <- glue_collapse((supplemental_rods_df %>% filter(side == "right"))$supplemental_rod, sep = "; ")
            }else{
                surgery_details_list$right_supplemental_rods <- "none"
            }
            
            ############# CROSSLINKS #############
            
            if(length(input$crosslink_connectors) > 0){
                surgery_details_list$crosslink_connector_levels <- glue_collapse(input$crosslink_connectors, sep = "; ")
            }else{
                surgery_details_list$crosslink_connector_levels <- "none"
            }
            
            #################### SPINE UIV PPX  #########################
            if(nrow(all_vertebrae_fixation_df) > 0){
                uiv_ppx_df <- all_objects_to_add_list$objects_df %>%
                    filter(level == surgery_details_list$upper_treated_vertebrae | level == surgery_details_list$uiv) %>%
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
        }

        #################### BMP & ALLOGRAFT  #######################
        if(str_detect(surgery_details_list$main_approach, "anterior") & surgery_details_list$fusion == "yes"){
            # surgery_details_list$anterior_bmp_mg_dose <- if_else(input$anterior_bmp_number == 0, "0", paste((as.double(input$anterior_bmp_number)*as.double(input$anterior_bmp_size))))
            surgery_details_list$anterior_bmp_mg_dose <-  anterior_bmp_dose_reactive()
            if(length(input$anterior_bone_graft) > 0){
                surgery_details_list$anterior_bone_graft <- glue_collapse(input$anterior_bone_graft, sep = "; ")
                
                if(str_detect(string = surgery_details_list$anterior_bone_graft, pattern = "Morselized Allograft")){
                    surgery_details_list$anterior_allograft_amount <- paste(input$anterior_allograft_amount)
                }
                    
            }

            if(length(input$anterior_biologics)>0){
                
                anterior_bma <- if_else("Bone Marrow Aspirate" %in% input$anterior_biologics, glue("Bone Marrow Aspirate ({input$anterior_bone_marrow_aspirate_volume})cc"), glue("xx"))
                anterior_cell_based <- if_else("Cell Based Allograft" %in% input$anterior_biologics, glue("Cell Based Allograft ({input$anterior_cell_based_allograft_volume})cc"), glue("xx"))
                anterior_dbm <- if_else("DBM" %in% input$anterior_biologics, glue("DBM ({input$anterior_dbm_volume})cc"), glue("xx"))
                
                anterior_biologics_vector <- discard(c(as.character(anterior_bma), as.character(anterior_cell_based), as.character(anterior_dbm)), .p = ~ .x == "xx")
                
                if(length(anterior_biologics_vector) > 0){
                    surgery_details_list$anterior_biologics <- glue_collapse(anterior_biologics_vector, sep = "; ") 
                }else{
                    surgery_details_list$anterior_biologics <- "xx"
                }
            }
            
        }
        
        if(str_detect(surgery_details_list$main_approach, "posterior") & surgery_details_list$fusion == "yes"){
            # surgery_details_list$posterior_bmp_mg_dose <- if_else(input$posterior_bmp_number == 0, "0", paste((as.double(input$posterior_bmp_number)*as.double(input$posterior_bmp_size))))
            surgery_details_list$posterior_bmp_mg_dose <-  posterior_bmp_dose_reactive()

            if(length(input$posterior_bone_graft) > 0){
                surgery_details_list$posterior_bone_graft <- glue_collapse(input$posterior_bone_graft, sep = "; ")
                
                if(str_detect(string = surgery_details_list$posterior_bone_graft, pattern = "Morselized Allograft")){
                    surgery_details_list$anterior_allograft_amount <- paste(input$posterior_allograft_amount)
                }
                
            }
 
            if(length(input$posterior_biologics)>0){
                posterior_bma <- if_else("Bone Marrow Aspirate" %in% input$posterior_biologics, glue("Bone Marrow Aspirate ({input$posterior_bone_marrow_aspirate_volume})cc"), glue("xx"))
                posterior_cell_based <- if_else("Cell Based Allograft" %in% input$posterior_biologics, glue("Cell Based Allograft ({input$posterior_cell_based_allograft_volume})cc"), glue("xx"))
                posterior_dbm <- if_else("DBM" %in% input$posterior_biologics, glue("DBM ({input$posterior_dbm_volume})cc"), glue("xx"))
                
                posterior_biologics_vector <- discard(c(as.character(posterior_bma), as.character(posterior_cell_based), as.character(posterior_dbm)), .p = ~ .x == "xx")
                
                if(length(posterior_biologics_vector) > 0){
                    surgery_details_list$posterior_biologics <- glue_collapse(posterior_biologics_vector, sep = "; ") 
                }else{
                    surgery_details_list$posterior_biologics <- "xx"
                }
            }
        }

        ####### complications  #####
        complication_df <- tibble(complication = append(input$intraoperative_complications_vector, input$other_intraoperative_complications)) %>%
            filter(complication != "") %>%
            filter(complication != " ") %>%
            remove_empty()

        if(nrow(complication_df) > 0){
            surgery_details_list$complications <- glue_collapse(complication_df$complication, sep = '; ')
        }else{
            surgery_details_list$complications <- "none"
        }
        
        surgery_details_list$operative_note <- paste(input$operative_note_text)
        
        ####### FULL TABLE  #####
        
        surgery_details_df <- enframe(surgery_details_list) %>%
            mutate(across(everything(), ~ as.character(.x))) 
        
        surgery_details_df
        
    })
    
    ######## Render "Procedure Summary Table:"    ######## 
    output$summary_table <- renderTable({
        summary_table_for_redcap_reactive()
    })
    
    
    ################## GENERATE INTRAOPERATIVE DETAILS TABLE #############
    intraoperative_details_table_reactive <- reactive({
        intraop_details_list <- list()
        
        ##########   date_of_surgery #############
        intraop_details_list$dos_intraop_repeating <- as.character(input$date_of_surgery)
        
        ################### Abx  #########################
        intraop_details_list$antibiotics <- glue_collapse(input$preop_antibiotics, sep = '; ')
        
        intraop_details_list$neuromonitoring <- if_else(length(input$neuromonitoring) >0, glue_collapse(input$neuromonitoring, sep = '; '), glue("none"))
        
        ################### antifibrinolytic  #########################
        if(length(input$anti_fibrinolytic) > 0){
            
            antifibrinolytics_vector <- str_to_lower(as.character(glue_collapse(input$anti_fibrinolytic, sep = "; ")))
            
            intraop_details_list$anti_fibrinolytic <- str_replace_all(string = antifibrinolytics_vector,
                                                          pattern = "tranexamic acid \\(txa\\)",
                                                          replacement = glue("tranexamic acid (txa) Loading: {input$txa_loading}mg/kg, Maint: {input$txa_maintenance}mg/kg/hr"))
        }else{
            intraop_details_list$anti_fibrinolytic <- "none"
        }

        ####### surgical findings #####
        intraop_details_list$surgical_findings <- if_else(input$surgical_findings == "", "none", input$surgical_findings)
        
        ####### Specimens  #####
        intraop_details_list$specimens <- if_else(input$specimens_removed == "", "none", input$specimens_removed)
        
        ####### EBL  #####
        intraop_details_list$ebl_ml <- if_else(is.na(input$ebl), "xx", paste(input$ebl))
        
        ####### Urine Output  #####
        intraop_details_list$urine_output <- if_else(is.na(input$urine_output), "xx", paste(input$urine_output)) 
        
        ####### Crystalloids  #####
        intraop_details_list$crystalloids_ml <- if_else(is.na(input$crystalloids_administered), "xx", paste(input$crystalloids_administered))
        
        ####### Colloids  #####
        intraop_details_list$colloids_ml <- if_else(is.na(input$colloids_administered), "xx", paste(input$colloids_administered)) 
        
        ####### Transfusion  #####
        intraop_details_list$transfusion <- if_else(input$transfusion == TRUE, "yes", "no")
        
        ####### cell_saver  #####
        intraop_details_list$cell_saver_cc <- if_else(is.na(input$cell_saver_transfused), "xx", paste(input$cell_saver_transfused)) 
        
        ####### prbc  #####
        intraop_details_list$prbc_units <- if_else(is.na(input$prbc_transfused), "xx", paste(input$prbc_transfused)) 
        
        ####### ffp  #####
        intraop_details_list$ffp_units <- if_else(is.na(input$ffp_transfused), "xx", paste(input$ffp_transfused)) 
        
        ####### cryoprecipitate  #####
        intraop_details_list$cryoprecipitate_units <- if_else(is.na(input$cryoprecipitate_transfused), "xx", paste(input$cryoprecipitate_transfused)) 
        
        ####### platelets  #####
        intraop_details_list$platelets_units <- if_else(is.na(input$platelets_transfused), "xx", paste(input$platelets_transfused))  
        
        ####### complications  #####
        complication_df <- tibble(complication = append(input$intraoperative_complications_vector, input$other_intraoperative_complications)) %>%
            filter(complication != "") %>%
            filter(complication != " ") %>%
            remove_empty()
        
        if(nrow(complication_df) > 0){
            intraop_details_list$intraoperative_complications <- glue_collapse(complication_df$complication, sep = '; ')
        }else{
            intraop_details_list$intraoperative_complications <- "none"
        }
        
        ####### other procedures  #####
        if(any(all_objects_to_add_list$objects_df$approach == "anterior")){
            intraop_details_list$deep_drains_anterior <- paste(input$deep_drains_anterior)
            intraop_details_list$superficial_drains_anterior <- paste(input$superficial_drains_anterior) 
        }
        if(any(all_objects_to_add_list$objects_df$approach == "posterior")){
            intraop_details_list$deep_drains_posterior <- paste(input$deep_drains_posterior)
            intraop_details_list$superficial_drains_posterior <- paste(input$superficial_drains_posterior) 
        }
        intraop_details_list$end_procedure_details <- glue_collapse(input$additional_end_procedure_details, sep = "; ")
        intraop_details_list$closure_details <- glue_collapse(input$closure_details, sep = "; ")
        intraop_details_list$dressing_details <- glue_collapse(input$dressing_details, sep = "; ")
        
        ####### GENERATE DATAFRAME #####
        
        intraop_details_df <- enframe(intraop_details_list) %>%
            mutate(across(everything(), ~ as.character(.x))) %>%
            filter(value != "xx")
        
        intraop_details_df
        
        
    })
    
    
    ######## Render "Intraoperative Details Table:"    ######## 
    output$intraoperative_details_table <- renderTable({
        intraoperative_details_table_reactive() 
    })
    
    ################# MAKE THE WIDE PROCEDURE SPECIFICS DATAFRAME ##################
    procedures_by_level_repeating_df_reactive <- reactive({
        fusion_df <- jh_fusion_category_function(fusion_vector = input$fusion_levels_confirmed, 
                                                 all_objects_df = all_objects_to_add_list$objects_df)
        
        data_wide <- all_objects_to_add_list$objects_df %>%
            as_tibble() %>%
            select(-object_constructed) %>%
            mutate(category = str_to_lower(op_note_procedure_performed_summary_classifier_function(object = object))) %>%
            union_all(fusion_df) %>%
            arrange(vertebral_number) %>%
            select(level, approach, category, procedure = object, side) %>%
            group_by(level, side, category) %>%
            mutate(redcap_repeat_instance = row_number()) %>%
            pivot_wider(names_from = level, values_from = procedure) %>%
            mutate(across(everything(), ~ replace_na(.x, ""))) %>%
            ungroup() %>%
            mutate(redcap_repeat_instance = row_number()) %>%
            mutate(redcap_repeat_instrument = "procedures_by_level_repeating") %>%
            mutate(dos_surg_repeating = as.character(input$date_of_surgery)) %>%
            select(redcap_repeat_instrument, redcap_repeat_instance, dos_surg_repeating, approach_repeating = approach, everything()) %>%
            clean_names() %>%
            mutate(across(everything(), ~ replace_na(.x, " "))) 
        
        data_wide
    })
    output$redcap_details_wide_df <- renderTable({
        procedures_by_level_repeating_df_reactive()
    })
    
    ################# PEDICLE SCREW DETAILS TABLE ##################
    pedicle_screw_details_table_redcap_reactive <- reactive({
        if(nrow(screw_details_results_reactive_df())>0){
            screw_details_df <- screw_details_results_reactive_df() %>%
                mutate(dos_screws_repeating = as.character(input$date_of_surgery)) %>%
                select(dos_screws_repeating, screw_level = level, screw_side = side, screw_type, screw_diameter, screw_size, screw_size_type) %>%
                mutate(redcap_repeat_instance = row_number()) %>%
                mutate(redcap_repeat_instrument = "screw_details_repeating") %>%
                select(redcap_repeat_instrument, redcap_repeat_instance, everything()) 
            
        }else{
            screw_details_df <- tibble(redcap_repeat_instance = character(), redcap_repeat_instrument = character(), screw_level = character(), screw_side = character(), screw_type = character(), screw_size = character(), screw_size_type = character())
        }
        screw_details_df
    })
    
    output$pedicle_screw_details_table <- renderTable({
        if(nrow(screw_details_results_reactive_df())>0){
            pedicle_screw_details_table_redcap_reactive()
        }else{
            tibble(redcap_repeat_instance = character(), redcap_repeat_instrument = character(), screw_level = character(), screw_side = character(), screw_type = character(), screw_size = character(), screw_size_type = character())
        }
    })  
    
    
    ################# INTERBODY  DETAILS TABLE ##################
    
    interbody_details_table_redcap_reactive <- reactive({
        if(nrow(interbody_details_df_reactive())>0){
            interbody_df <- interbody_details_df_reactive() %>%
                mutate(dos_interbody_repeating = as.character(input$date_of_surgery)) %>%
                select(dos_interbody_repeating, interbody_level = level, interbody_approach = approach, object, composition, device_name, height, integrated_fixation, expandable, other, implant_statement) %>%
                mutate(redcap_repeat_instance = row_number()) %>%
                mutate(redcap_repeat_instrument = "interbody_implant_repeating") %>%
                select(redcap_repeat_instrument, redcap_repeat_instance, everything()) 

        }else{
            interbody_df <-tibble(redcap_repeat_instance = character(), redcap_repeat_instrument = character(),
                   interbody_level = character(), 
                   interbody_approach = character(),
                   composition = character(),
                   device_name = character(), 
                   height = character(), 
                   integrated_fixation = character(),
                   expandable = character(),
                   other = character(),
                   implant_statement = character())
        }
        interbody_df
    })
    
    output$interbody_details_table <- renderTable({
        interbody_details_table_redcap_reactive()
    })
    
    #################  ALL OBJECTS TABLE ##################
    output$all_objects_table <- renderTable({
        all_objects_df <- all_objects_to_add_list$objects_df %>%
            as_tibble() %>%
            select(-object_constructed) %>%
            distinct() %>%
            mutate(proc_category = map(.x = object, .f = ~ op_note_procedure_performed_summary_classifier_function(object = .x))) %>%
            unnest()
        
        if(nrow(interbody_details_df_reactive()) > 0){
            all_objects_df <- all_objects_df %>%
                left_join(interbody_details_df_reactive() %>% select(level, approach, object, implant_statement)) %>%
                mutate(across(everything(), ~ replace_na(.x, " "))) 
        }else{
            all_objects_df <- all_objects_df %>%
                mutate(implant_statement = "")
        }
        
        if(nrow(screw_details_results_reactive_df()) > 0){
            posterior_screws_df <- all_objects_df %>%
                filter(approach == "posterior") %>%
                filter(str_detect(object, "screw")) %>%
                left_join(screw_details_results_reactive_df()) %>%
                select(level, approach, side, object, screw_size_type) 
            
            all_objects_df <- all_objects_df%>%
                left_join(posterior_screws_df) %>%
                mutate(across(everything(), ~ replace_na(.x, " "))) 
        }
        
        all_objects_df
        
    })


    
    #############~~~~~~~~~~~~~~~~~~~ ##################### REDCAP UPLOAD  #############~~~~~~~~~~~~~~~~~~~ ##################### 
    #############~~~~~~~~~~~~~~~~~~~ ##################### REDCAP UPLOAD  #############~~~~~~~~~~~~~~~~~~~ ##################### 
    #############~~~~~~~~~~~~~~~~~~~ ##################### REDCAP UPLOAD  #############~~~~~~~~~~~~~~~~~~~ ##################### 
    
    #############~~~~~~~~~~~~~~~~~~~ ##################### REDCAP UPLOAD  #############~~~~~~~~~~~~~~~~~~~ ##################### 
    #############~~~~~~~~~~~~~~~~~~~ ##################### REDCAP UPLOAD  #############~~~~~~~~~~~~~~~~~~~ ##################### 
    #############~~~~~~~~~~~~~~~~~~~ ##################### REDCAP UPLOAD  #############~~~~~~~~~~~~~~~~~~~ ##################### 
    
    output$summary_table_for_redcap_preview <- renderTable({
        summary_table_for_redcap_reactive()
    })
    
    observeEvent(input$confirm_upload_1, {
        confirmSweetAlert(inputId = "confirm_upload_final",
                          session = session,
                          title = "Final Confirmation to Upload Data:", 
                          btn_labels = c("No, Cancel", "Yes, Upload"), text = "Select if you wish to upload.", closeOnClickOutside = TRUE)
    })
    
    observeEvent(input$confirm_upload_final, {
        
        all_patient_ids_df <- exportRecords(rcon = rcon, fields = c("record_id", "last_name", "first_name", "date_of_birth"), events = "enrollment_arm_1") %>%
            type.convert() %>%
            select(record_id, last_name, first_name, date_of_birth) %>%
            mutate(last_name = str_to_lower(last_name), 
                   first_name = str_to_lower(first_name))
        
        joined_df <- patient_details_redcap_df_reactive() %>%
            select(last_name, first_name, date_of_birth) %>%
            mutate(last_name = str_to_lower(last_name), 
                   first_name = str_to_lower(first_name)) %>%
            left_join(all_patient_ids_df)
        
        match_found <- if_else(!is.na(joined_df$record_id[[1]]), TRUE, FALSE)
        
        if(match_found == TRUE){
            record_number <- joined_df$record_id[[1]]
            
            max_repeat_instances_df <- exportRecords(rcon = rcon, records = record_id) %>%
                as_tibble() %>%
                select(redcap_repeat_instrument, redcap_repeat_instance) %>%
                remove_missing() %>%
                group_by(redcap_repeat_instrument) %>%
                filter(redcap_repeat_instance == max(redcap_repeat_instance)) %>%
                ungroup()
            
            repeat_list <- as.list(deframe(max_repeat_instances_df))
            
            if("surgical_details" %in% max_repeat_instances_df$redcap_repeat_instrument){
                surgical_details_instance_add <- repeat_list$surgical_details
            }else{
                surgical_details_instance_add <- 0
            }
            if("procedures_by_level_repeating" %in% max_repeat_instances_df$redcap_repeat_instrument){
                procedures_by_level_repeating_instance_add <- repeat_list$procedures_by_level_repeating 
            }else{
                procedures_by_level_repeating_instance_add <- 0
            }
            if("screw_details_repeating" %in% max_repeat_instances_df$redcap_repeat_instrument){
                screw_details_repeating_instance_add <- repeat_list$screw_details_repeating 
            }else{
                screw_details_repeating_instance_add <- 0
            }
            if("interbody_implant_repeating" %in% max_repeat_instances_df$redcap_repeat_instrument){
                interbody_implant_repeating_instance_add <- repeat_list$interbody_implant_repeating 
            }else{
                interbody_implant_repeating_instance_add <- 0
            }

            surgical_details_instance_start <- repeat_list$surgical_details + 1
            max_procedures_by_level_repeating <- repeat_list$procedures_by_level_repeating
            max_screw_details_repeating <- repeat_list$screw_details_repeating
            
        }else{
            record_number <- exportNextRecordName(rcon = rcon)
            surgical_details_instance_add <- 0
            procedures_by_level_repeating_instance_add <- 0
            screw_details_repeating_instance_add <- 0
            interbody_implant_repeating_instance_add <- 0
        }
        

        ##### uploaded patient details #######
        patient_df_for_upload <- patient_details_redcap_df_reactive() %>%
            mutate(record_id = record_number) %>%
            mutate(patient_details_complete = "Complete") %>%
            select(record_id, everything()) 
        
        importRecords(rcon = rcon, data = patient_df_for_upload, returnContent = "count")
        
        
        ##### uploaded surgical details #######
        surgical_details_instrument <- summary_table_for_redcap_reactive() %>%
            pivot_wider(names_from = name, values_from = value) %>%
            mutate(record_id = record_number) %>%
            mutate(redcap_event_name = "surgery_arm_1") %>%
            mutate(redcap_repeat_instance = row_number() + surgical_details_instance_add) %>%
            mutate(redcap_repeat_instrument = "surgical_details") %>%
            mutate(surgical_details_complete = "Complete") %>%
            select(record_id, redcap_event_name, everything()) 

        importRecords(rcon = rcon, data = surgical_details_instrument, returnContent = "count")

        ###### Upload repeating objects for all levels ####
        procedures_by_level_repeating_instrument <- procedures_by_level_repeating_df_reactive() %>%
            mutate(record_id = record_number) %>%
            mutate(redcap_event_name = "surgery_arm_1") %>%
            mutate(redcap_repeat_instance = row_number() + procedures_by_level_repeating_instance_add) %>%
            mutate(redcap_repeat_instrument = "procedures_by_level_repeating") %>%
            mutate(procedures_by_level_repeating_complete = "Complete") %>%
            select(record_id, redcap_event_name, everything())
        
        importRecords(rcon = rcon, data = procedures_by_level_repeating_instrument, returnContent = "count")

        
        ##### uploaded screw details #######
        if(nrow(pedicle_screw_details_table_redcap_reactive())>0){
            screw_details_repeating <- pedicle_screw_details_table_redcap_reactive() %>%
                mutate(record_id = record_number) %>%
                mutate(redcap_event_name = "surgery_arm_1") %>%
                mutate(redcap_repeat_instance = row_number() +screw_details_repeating_instance_add) %>%
                mutate(redcap_repeat_instrument = "screw_details_repeating") %>%
                mutate(screw_details_repeating_complete = "Complete") %>%
                select(record_id, redcap_event_name, everything()) 
            
            importRecords(rcon = rcon, data = screw_details_repeating, returnContent = "count")
        }

                
        ##### uploaded interbody details #######
        if(nrow(interbody_details_table_redcap_reactive())>0){
            interbody_implant_repeating <- interbody_details_table_redcap_reactive() %>%
                mutate(record_id = record_number) %>%
                select(record_id, everything()) %>%
                mutate(redcap_event_name = "surgery_arm_1") %>%
                mutate(redcap_repeat_instance = row_number() + interbody_implant_repeating_instance_add) %>%
                mutate(redcap_repeat_instrument = "interbody_implant_repeating") %>%
                mutate(interbody_implant_repeating_complete = "Complete") %>%
                mutate(across(everything(), ~ paste0(as.character(.x)))) %>%
                select(record_id, redcap_event_name, everything()) 
            
            importRecords(rcon = rcon, data = interbody_implant_repeating, returnContent = "count")   
        }
        
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
