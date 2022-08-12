####################### SURGICAL PLANNING V2 ###########################
library(shiny)
library(shinyWidgets) 
library(sf)
library(tidyverse)
library(ggplot2)
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

# packageList <- c("shiny", "shinyWidgets", "sf", "tidyverse", "shinyBS", "cowplot", "magick", "ggpattern", "glue", "rlist",
#                  "janitor", "lubridate", "redcapAPI", "ggpmisc", "rclipboard", "nngeo", "shinydashboard")
# for(package in packageList){
#     if(!require(package,character.only = TRUE)){
#         install.packages(package);require(package,character.only = TRUE);}
# }


# sudo su - -c "R -e \"install.packages('devtools', repos='http://cran.rstudio.com/')\""

source("short_shiny_functions.R", local = TRUE)
source("load_icd_codes.R", local = TRUE)
source("modal_functions.R", local = TRUE)
source("make_geoms_functions.R", local = TRUE)
source("build_spine_objects_functions.R", local = TRUE)
source("load_coordinates_build_objects.R", local = TRUE)
source("anterior_posterior_operative_note_generator_functions.R", local = TRUE)
source("load_coordinates_build_objects_6_lumbar.R", local = TRUE)
source("screw_size_type_inputs.R", local = TRUE)
source("load_icd_codes.R", local = TRUE)
# source("no_implants_added_op_note.R", local = TRUE)
 
# jh_build_test_implant_function <- function(side_level_object_list = list("left_L3_pedicle_screw", "left_l4_pedicle_screw", "left_l5_pedicle_screw", 
#                                                                          "right_L3_pedicle_screw", "right_l5_pedicle_screw")){
#   
#   test_df <- all_implants_constructed_df %>%
#     mutate(side_level_object = str_to_lower(paste0(side, "_", level, "_", object))) %>%
#     select(side_level_object, everything()) %>%
#     filter(side_level_object %in% str_to_lower(side_level_object_list)) %>%
#     select(-side_level_object)
#   
#   return(test_df)
# }
# ui <- shinyUI(basicPage(
#   dateInput(inputId = "input_date", label = "Date:", value = ""),
#   textOutput(outputId = "date_text_output")
# ))
# 
# server <- function(input, output) {
#   output$date_text_output <- renderText({
#   })
# }
# 
# shinyApp(ui = ui, server = server)

#Dashboards:
rclipboardSetup()

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Spine Operative Logging and Automated Report Generation", titleWidth = 650,
                                    dropdownMenuOutput(outputId = "upload_to_redcap_task")
                                    
                    ),
                    dashboardSidebar(width = 350,collapsed = TRUE,
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
                  "#surgical_details_redcap_df_modal_tab {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#procedures_by_level_redcap_df_modal_tab {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#interbody_details_redcap_df_modal_tab {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#all_objects_table {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#anterior_objects_passed_to_op_note_df {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#procedures_by_level_redcap_df_sidetab {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#posterior_approach_objects_for_op_note_df {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#all_inputs {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#all_inputs_printed {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#patient_details_redcap_df_sidetab {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#screw_details_redcap_df_modal_tab {
                        overflow-x: auto;
                }"),
                tags$style(".nav-tabs-custom>.nav-tabs { width: max-content}"),
                      ),
                tabItems(
                  tabItem(tabName = "patient_details_procedures",
                          column(width = 3, 
                                 box(width = 12, title = tags$div(style = "font-size:22px; font-weight:bold", "Patient Details:"), solidHeader = TRUE, status = "info",collapsible = TRUE,
                                     uiOutput(outputId = "patient_details_ui"),
                                     br(),
                                     actionBttn(inputId = "open_patient_details_modal", label = "Edit Patient Details", icon = icon("fas fa-user-edit"), size = "sm", block = TRUE)
                                 ),
                                 br(),
                                 box(width = 12, title = tags$div(style = "font-size:22px; font-weight:bold", "Diagnosis, Symptoms, Procedure Details:"), solidHeader = TRUE, status = "info",collapsible = TRUE,
                                     uiOutput(outputId = "diagnosis_symptoms_ui"),
                                     br(),
                                     actionBttn(inputId = "open_diagnosis_symptoms_procedure_modal", label = "Edit", icon = icon("fas fa-user-edit"), size = "sm", block = TRUE)
                                 )
                          ),
                          column(width = 9, 
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
                                                    icon = icon("fas fa-cog"),
                                                    size = "sm",
                                                    inline = TRUE,
                                                    right = TRUE
                                                  )
                                           )
                                         ),
                                         fluidRow(
                                           column(width = 2,
                                                  fluidRow(
                                                    column(width = 9,
                                                           div(style = "font-size:12px; text-align:center", "Change Lumbar Anatomy")
                                                    ),
                                                    column(width = 3,
                                                           dropdownButton(circle = TRUE,size = "xs",
                                                                          label = "Transitional Anatomy",
                                                                          icon = icon("fas fa-asterisk"), 
                                                                          awesomeRadio(
                                                                            inputId = "lumbar_vertebrae_count",
                                                                            label = "Number of Lumbar Vertebrae:", 
                                                                            choices = c("4", "5", "6"),
                                                                            selected = "5",
                                                                            inline = TRUE, 
                                                                            status = "success"
                                                                          )
                                                           )
                                                    )
                                                  ),
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
                                                  div(style = "font-size:16px; font-weight:bold; font-style:italic; text-align:center", "Add Procedures in Order Performed"),
                                                  div(style = "font-size:12px; text-align:center", "Double click to remove an item."),
                                                  plotOutput("spine_plan",
                                                             height = 750,
                                                             click = "plot_click",
                                                             dblclick = "plot_double_click")
                                           )   
                                         )
                                     ),
                                     box(width = 5, ##### RIGHT COLUMN NEXT TO SPINE PLOT
                                         # box(width = 12, title = div(style = "font-size:16px; font-weight:bold; text-align:left", "Approach & Technique Specifics:"),collapsible = TRUE,  
                                         #     conditionalPanel(condition = "input.spine_approach.indexOf('Posterior') > -1",
                                         #                      fluidRow(
                                         #                        prettyRadioButtons(
                                         #                          inputId = "approach_specified_posterior",
                                         #                          label = NULL, 
                                         #                          inline = TRUE,
                                         #                          choices = c("Midline",
                                         #                                      "Paraspinal or Paramedian", 
                                         #                                      "Stab"),
                                         #                          icon = icon("check"), 
                                         #                          bigger = TRUE,
                                         #                          status = "info"
                                         #                        )
                                         #                      ),
                                         #                      fluidRow(
                                         #                        column(width = 6, 
                                         #                               prettyRadioButtons(
                                         #                                 inputId = "approach_open_mis",
                                         #                                 label = NULL, 
                                         #                                 inline = TRUE,
                                         #                                 choices = c("Open",
                                         #                                             "Tubular", 
                                         #                                             "Endoscopic", 
                                         #                                             "Mini Open",
                                         #                                             "Percutaneous Screw"
                                         #                                 ),
                                         #                                 selected = "Open",
                                         #                                 icon = icon("check"), 
                                         #                                 bigger = TRUE,
                                         #                                 status = "success"
                                         #                               )
                                         #                        ), 
                                         #                        column(width = 6, 
                                         #                               prettyCheckboxGroup(
                                         #                                 inputId = "approach_robot_navigation",
                                         #                                 label = NULL, 
                                         #                                 inline = TRUE,
                                         #                                 choices = c("Microscopic", 
                                         #                                             "Navigated", 
                                         #                                             "Robotic"),
                                         #                                 icon = icon("check"), 
                                         #                                 bigger = TRUE,
                                         #                                 status = "success"
                                         #                               )
                                         #                        )
                                         #                      )
                                         #     ),
                                         #     conditionalPanel(condition = "input.spine_approach.indexOf('Anterior') > -1",
                                         #                      prettyRadioButtons(
                                         #                        inputId = "approach_specified_anterior",
                                         #                        label = NULL,
                                         #                        inline = TRUE,
                                         #                        choices = c("Left-sided", 
                                         #                                    "Right-sided",
                                         #                                    "Paramedian",
                                         #                                    "Lateral Transpsoas",
                                         #                                    "Lateral Antepsoas",
                                         #                                    "Thoracoabdominal",
                                         #                                    "Thoracotomy",
                                         #                                    "Transperitoneal",
                                         #                                    "Retroperitoneal"),
                                         #                        selected = "Left-sided",
                                         #                        icon = icon("check"),
                                         #                        bigger = TRUE,
                                         #                        status = "info"
                                         #                      )
                                         #     )
                                         # ),
                                         uiOutput(outputId = "currently_adding_ui"),
                                         conditionalPanel(condition = "input.spine_approach.indexOf('Anterior') > -1",
                                                          div(style = "font-size:20px; font-weight:bold; text-align:left", "1. Select Procedure & Click Spine to Add:")
                                         ),
                                         conditionalPanel(condition = "input.spine_approach.indexOf('Posterior') > -1",
                                                          div(style = "font-size:20px; font-weight:bold; text-align:left", "1. Select Category:"),
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
                                                            inputId = "add_special_approach",
                                                            size = "sm", block = TRUE,
                                                            label = "Add Special Approach (Costovertebral, etc)",
                                                            style = "simple",
                                                            color = "primary"
                                                          ),
                                                          br(),
                                                          actionBttn(
                                                            inputId = "add_other",
                                                            size = "sm", block = TRUE,
                                                            label = "Add Other (cement, structural allograft, I&D, etc)",
                                                            style = "simple",
                                                            color = "primary"
                                                          ),
                                                          uiOutput(outputId = "intervertebral_cage_ui"),
                                                          br(),
                                                          uiOutput(outputId = "tumor_resection_ui"),
                                                          br(), 
                                                          div(style = "font-size:20px; font-weight:bold; text-align:left", "2. Select Implant/Procedure & Click Spine to Add:"),
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
                                                 "Pedicle Screw" = "pedicle_screw",
                                                 "Pelvic Screw" = "pelvic_screw",
                                                 "Occipital Screw" = "occipital_screw",
                                                 "Transarticular Screw" = "transarticular_screw",
                                                 "Pars Screw" = "pars_screw",
                                                 "Translaminar Screw" = "translaminar_screw",
                                                 "Lateral Mass Screw" = "lateral_mass_screw",
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
                                         actionBttn(
                                           inputId = "reset_all",
                                           size = "xs", block = TRUE,
                                           label = "Reset & Clear All",
                                           style = "simple",
                                           color = "danger"
                                         )
                                     )## end of little box to the right of the spine
                                 ),### end of the full box
                                 actionBttn(
                                   inputId = "implants_complete",
                                   size = "md", 
                                   block = TRUE,
                                   label = "Click when finished to Add Implant Details",
                                   style = "simple",
                                   color = "success", 
                                   icon = icon("fas fa-arrow-circle-right")
                                 )
                          ) ## closes the right column 
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
                                                                                            "DBM", 
                                                                                            "iFactor"
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
                                                            ),
                                                            conditionalPanel(
                                                              condition = "input.posterior_biologics.indexOf('iFactor') > -1",
                                                              jh_make_shiny_table_row_function(left_column_label = "iFactor Volume (cc):",bottom_margin = "5px",
                                                                                               left_column_percent_width = 60,
                                                                                               font_size = 16,
                                                                                               input_type = "numeric",
                                                                                               input_id = "posterior_ifactor_volume",
                                                                                               initial_value_selected = 0, min = 0, max = 50, step = 5)
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
                                                                                                          "DBM",
                                                                                                          "iFactor"
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
                                                                                               input_id = "anterior_allograft_amount", 
                                                                                               initial_value_selected = 0, min = 0, max = 500, step = 30)
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
                                                            ),
                                                            conditionalPanel(
                                                              condition = "input.anterior_biologics.indexOf('iFactor') > -1",
                                                              jh_make_shiny_table_row_function(left_column_label = "iFactor Volume (cc):",bottom_margin = "5px",
                                                                                               left_column_percent_width = 60,
                                                                                               font_size = 16,
                                                                                               input_type = "numeric",
                                                                                               input_id = "anterior_ifactor_volume",
                                                                                               initial_value_selected = 0, min = 0, max = 50, step = 5)
                                                            )
                                                     )
                                                   ),
                                                   jh_make_bmp_ui_function(anterior_posterior = "anterior")
                                               )
                              ),
                              conditionalPanel(condition = "input.fusion_procedure_performed == true",
                                               uiOutput(outputId = "interbody_implants_ui")
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
                              ### this input gets updated based on the laterality of screws to be used in the next conditional panel steps
                              conditionalPanel(condition = "input.spine_approach.indexOf('Never True') > -1",
                                               checkboxGroupInput(inputId = "level_object_for_screw_details",
                                                                  label = "Screw Levels:",
                                                                  choices = all_screw_size_type_inputs_df$level_object_label,
                                                                  selected = ""),
                                               checkboxGroupInput(inputId = "left_level_object_for_screw_details",
                                                                  label = "Screw Levels:",
                                                                  choices = all_screw_size_type_inputs_df$left_object,
                                                                  selected = ""),
                                               checkboxGroupInput(inputId = "right_level_object_for_screw_details",
                                                                  label = "Screw Levels:",
                                                                  choices = all_screw_size_type_inputs_df$right_object,
                                                                  selected = "")),
                              conditionalPanel(condition = "input.fusion_procedure_performed == true",
                                               # uiOutput(outputId = "screw_size_types_ui")
                                               box(width = 12,
                                                   title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Screw Details:"), collapsible = TRUE,
                                                   jh_make_shiny_table_row_function(left_column_label = "Screw/Rod Manufacturer:",
                                                                                    left_column_percent_width = 30,
                                                                                    input_type = "checkbox",
                                                                                    font_size = 16,
                                                                                    checkboxes_inline = TRUE,
                                                                                    input_id = "implant_manufacturer", choices_vector = c("Alphatec", "Depuy Synthes", "Globus Medical", "K2 Stryker", "Medicrea", "Medtronic", "NuVasive", "Orthofix", "Zimmer Bioment", "Other")
                                                   ),
                                                   h4("Screw Sizes:"),
                                                   fluidRow(
                                                     column(4,
                                                            "Implant"),
                                                     column(2,
                                                            "Left Diameter"),
                                                     column(2,
                                                            "Left Length"),
                                                     column(2,
                                                            "Right Diameter"),
                                                     column(2,
                                                            "Right Length")
                                                   ),
                                                   map(.x = c(1:length(all_screw_size_type_inputs_df$level_object_label)),
                                                       .f = ~
                                                         conditionalPanel(condition = glue("input.level_object_for_screw_details.indexOf('{all_screw_size_type_inputs_df$level_object_label[[.x]]}') >-1"),
                                                                          fluidRow(
                                                                            column(4,
                                                                                   all_screw_size_type_inputs_df$level_object_label[[.x]]
                                                                            ),
                                                                            column(2,
                                                                                   conditionalPanel(condition = glue("input.left_level_object_for_screw_details.indexOf('{all_screw_size_type_inputs_df$left_object[[.x]]}') >-1"),
                                                                                                    all_screw_size_type_inputs_df$left_diameter_input[[.x]]
                                                                                   )
                                                                            ),
                                                                            column(2,
                                                                                   conditionalPanel(condition = glue("input.left_level_object_for_screw_details.indexOf('{all_screw_size_type_inputs_df$left_object[[.x]]}') >-1"),
                                                                                                    all_screw_size_type_inputs_df$left_length_input[[.x]]
                                                                                   )
                                                                            ),
                                                                            column(2,
                                                                                   conditionalPanel(condition = glue("input.right_level_object_for_screw_details.indexOf('{all_screw_size_type_inputs_df$right_object[[.x]]}') >-1"),
                                                                                                    all_screw_size_type_inputs_df$right_diameter_input[[.x]]
                                                                                   )
                                                                            ),
                                                                            column(2,
                                                                                   conditionalPanel(condition = glue("input.right_level_object_for_screw_details.indexOf('{all_screw_size_type_inputs_df$right_object[[.x]]}') >-1"),
                                                                                                    all_screw_size_type_inputs_df$right_length_input[[.x]]
                                                                                   )
                                                                            )
                                                                          )
                                                         )
                                                   ),
                                                   hr(),
                                                   h4("Screw Types:"),
                                                   fluidRow(
                                                     column(4,
                                                            "Implant"),
                                                     column(4,
                                                            "Left Screw Type"),
                                                     column(4,
                                                            "Right Screw Type"),
                                                   ),
                                                   map(.x = c(1:length(all_screw_size_type_inputs_df$level_object_label)),
                                                       .f = ~
                                                         conditionalPanel(condition = glue("input.level_object_for_screw_details.indexOf('{all_screw_size_type_inputs_df$level_object_label[.x]}') >-1"),
                                                                          fluidRow(
                                                                            column(4,
                                                                                   all_screw_size_type_inputs_df$level_object_label[[.x]]
                                                                            ),
                                                                            column(4,
                                                                                   conditionalPanel(condition = glue("input.left_level_object_for_screw_details.indexOf('{all_screw_size_type_inputs_df$left_object[[.x]]}') >-1"),
                                                                                                    all_screw_size_type_inputs_df$left_type_input[[.x]]
                                                                                   )
                                                                            ),
                                                                            column(4,
                                                                                   conditionalPanel(condition = glue("input.right_level_object_for_screw_details.indexOf('{all_screw_size_type_inputs_df$right_object[[.x]]}') >-1"),
                                                                                                    all_screw_size_type_inputs_df$right_type_input[[.x]]
                                                                                   )
                                                                            ),
                                                                          )
                                                         )
                                                   )
                                               )
                              )
                          ),
                          box(width = 5, status = "primary",
                              fluidRow(
                                actionBttn(
                                  inputId = "return_to_add_implants_tab",
                                  size = "sm", 
                                  block = TRUE,
                                  label = "Click to Return",
                                  style = "simple",
                                  color = "primary", 
                                  icon = icon("fas fa-arrow-circle-left")
                                )
                              ),
                              plotOutput("spine_plot_for_implants_tab",
                                         height = 725),
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
                          box(width = 12, 
                              fluidRow(
                                actionBttn(
                                  inputId = "return_to_add_implant_details_tab",
                                  size = "sm", 
                                  block = TRUE,
                                  label = "Click to Return",
                                  style = "simple",
                                  color = "primary", 
                                  icon = icon("fas fa-arrow-circle-left")
                                )
                              )),
                          box(width = 4, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Additional Surgical Details:"),status = "info", solidHeader = TRUE,
                              actionBttn(
                                inputId = "edit_additional_surgical_details",
                                size = "sm", 
                                block = TRUE,
                                label = "Click to Edit",
                                style = "simple",
                                color = "royal", 
                                icon = icon("fas fa-user-edit"),
                              ),
                              tableOutput(outputId = "additional_surgical_details_table"),
                          ),
                          box(width = 8, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Data Upload:"),status = "success", solidHeader = TRUE,
                              actionBttn(inputId = "preview_redcap_upload", label = "Upload to Redcap Project", icon = icon("upload"), style = "jelly", color = "primary", size = "md"),
                          ),
                          box(width = 8, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Operative Note Generator:"),status = "success", solidHeader = TRUE,
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
                          #         ###########################################
                          #         box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Patient Details Table:"),status = "success", collapsible = TRUE, solidHeader = TRUE,
                          #             tableOutput(outputId = "patient_details_redcap_df_sidetab")
                          #         ),
                          #         box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Procedure Summary Table:"),status = "success", collapsible = TRUE, solidHeader = TRUE,
                          #             tableOutput(outputId = "surgical_details_redcap_df_sidetab")
                          #         ),
                          #         box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Intraoperative Details"),status = "success", collapsible = TRUE,solidHeader = TRUE,
                          #             tableOutput(outputId = "intraoperative_details_redcap_df_sidetab")
                          #         ),
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Procedure Specifics"),status = "success", collapsible = TRUE,solidHeader = TRUE,
                              tableOutput(outputId = "procedures_by_level_redcap_df_sidetab")
                          ),
                          #         box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Screw Details:"),status = "success", collapsible = TRUE,solidHeader = TRUE,
                          #             tableOutput("screw_details_redcap_df_sidetab")
                          #         ),
                          #         box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Interbody implants:"),status = "success", collapsible = TRUE, solidHeader = TRUE,
                          #             tableOutput(outputId = "interbody_details_redcap_df_sidetab")
                          #         ),
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "All objects table:"),status = "success", collapsible = TRUE,solidHeader = TRUE,
                                      tableOutput(outputId = "all_objects_table")
                                  ),
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Anterior Objects Passed to Op Note:"),status = "success", collapsible = TRUE,solidHeader = TRUE,
                              tableOutput(outputId = "anterior_objects_passed_to_op_note_df")
                          ),
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Revision Implants table:"), status = "success", collapsible = TRUE,solidHeader = TRUE,
                              tableOutput(outputId = "revision_implants_table")
                          ),
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Posterior Approach Objects for Op Note:"), status = "success", collapsible = TRUE,solidHeader = TRUE, 
                               tableOutput(outputId = "posterior_approach_objects_for_op_note_df")),
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "All Inputs:"), status = "success", collapsible = TRUE, solidHeader = TRUE, 
                              tableOutput(outputId = "all_inputs")),
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "All Inputs:"), status = "success", collapsible = TRUE, solidHeader = TRUE, 
                              tableOutput(outputId = "all_inputs_removed"))
                          # box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "All Inputs:"), status = "success", collapsible = TRUE, solidHeader = TRUE, 
                          #     tableOutput(outputId = "all_inputs_printed")),
                          #         ###########################################
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

server <- function(input, output, session) {
  
  rcon_reactive <- reactiveValues()
  
  observeEvent(input$redcap_token, {
    # redcap_token <- paste0("2A930AE845C92CBF95467E59", input$redcap_token)
    
    rcon_reactive$rcon <- redcapConnection(url = 'https://redcap.uthscsa.edu/REDCap/api/', token = input$redcap_token)    
    
  })
  
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   Startup  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   Startup  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   Startup  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  
  ################################################    INITIAL STARTUP MODAL ######################################
  ################################################    INITIAL STARTUP MODAL ######################################
  ################################################    INITIAL STARTUP MODAL ######################################
  
  showModal(startup_modal_box(starting_first_name = "", starting_last_name = "", starting_dob = "", starting_dos = ""))
  
  
  observeEvent(input$test_patient_button, {
    updateTextInput(session = session, inputId = "patient_last_name", value = "TestLAST")
    updateTextInput(session = session, inputId = "patient_first_name", value = "TestFIRST")
    updateDateInput(session = session, inputId = "date_of_birth", value = date("1970-01-05"))
    updateDateInput(session = session, inputId = "date_of_surgery", value = Sys.Date())
    updateAwesomeRadio(session = session, inputId = "sex", selected = "Male")
  })
  
  
  ## now make a reactive modal for editing and if not all required info is entered:
  modal_box_patient_details_reactive <- reactive({
    startup_modal_box(starting_first_name = input$patient_first_name, 
                      starting_last_name = input$patient_last_name, 
                      starting_dob = input$date_of_birth,
                      starting_dos = input$date_of_surgery, 
                      starting_sex = input$sex, 
                      hospital_input = input$hospital, redcap_token_input = input$redcap_token,
                      button_proceed = "exit"
    )
  })
  
  observeEvent(input$open_patient_details_modal, ignoreInit = TRUE, {
    showModal(modal_box_patient_details_reactive())
  })
  
  
  ############### RETRIEVE EXISTING PATIENT #########################
  existing_patient_data <- reactiveValues()
  existing_patient_data$match_found <- FALSE
  existing_patient_data$patient_df <- tibble(level = character())
  
  
  observeEvent(input$search_for_prior_patient, ignoreInit = TRUE,
               {
                 all_patient_ids_df <- exportRecords(rcon = rcon_reactive$rcon, fields = c("record_id", "last_name", "first_name", "date_of_birth"), events = "enrollment_arm_1") %>%
                   type.convert() %>%
                   select(record_id, last_name, first_name, date_of_birth) %>%
                   mutate(last_name = str_to_lower(last_name),
                          first_name = str_to_lower(first_name))
                 
                 if(nrow(all_patient_ids_df)>0){
                   joined_df <- patient_details_redcap_df_reactive() %>%
                     select(last_name, first_name, date_of_birth) %>%
                     mutate(last_name = str_to_lower(last_name), 
                            first_name = str_to_lower(first_name)) %>%
                     left_join(all_patient_ids_df)
                   
                   match_found <- if_else(!is.na(joined_df$record_id[[1]]), TRUE, FALSE)
                   
                   if(match_found == TRUE){
                     record_number <- joined_df$record_id[[1]]
                     
                     existing_patient_data$patient_df <- exportRecords(rcon = rcon_reactive$rcon, records = record_number, fields = append(c("record_id", "dos_surg_repeating", "approach_repeating", "side", "object"), str_to_lower(str_replace_all(levels_vector, pattern = "-", replacement = "_")))) %>%                    as_tibble()  %>%
                       filter(redcap_repeat_instrument == "procedures_by_level_repeating")  %>%
                       type.convert() %>%
                       select(record_id, dos_surg_repeating, approach_repeating, side, str_replace_all(string = str_to_lower(levels_numbered_df$level), pattern = "-", replacement = "_")) %>%
                       pivot_longer(cols = str_replace_all(string = str_to_lower(levels_numbered_df$level), pattern = "-", replacement = "_"), names_to = "level", values_to = "object") %>%
                       filter(!is.na(object)) %>%
                       rename(approach = approach_repeating) %>%
                       mutate(level = str_to_title(str_replace_all(string = level, pattern = "_", replacement = "-"))) %>%
                       rename(date_of_surgery = dos_surg_repeating) 
                     
                     if(length(unique(existing_patient_data$patient_df$approach))>1){
                       approach_to_use <- (all_implants_constructed_df %>%
                                             count(approach) %>%
                                             filter(n == max(n)))$approach[[1]]
                       
                       existing_patient_data$patient_df <- existing_patient_data$patient_df %>%
                         filter(approach == approach_to_use)
                     }
                     
                     existing_patient_data$match_found <- match_found
                     
                   }
                 }
               })
  
  output$patient_prior_data <- renderTable({
    if(existing_patient_data$match_found == TRUE){
      existing_patient_data$patient_df
    }
  })
  
  output$prior_patient_match <- renderUI({
    if(existing_patient_data$match_found == TRUE){
      column(12, 
             tags$div(style = "font-size:24px; font-weight:bold; color:darkblue; font-family:sans-serif; font-style:italic", "Match found!"), 
             tableOutput(outputId = "patient_prior_data"))
    }else{
      tags$div(style = "font-size:12px; font-weight:bold; color:darkblue; font-family:sans-serif;", "No Match")
    }
  })
  
  
  ################################################    MODAL BOX 2 ######################################
  ################################################    MODAL BOX 2 ######################################
  ################################################    MODAL BOX 2 ######################################
  
  observeEvent(input$close_startup_modal, ignoreInit = TRUE, {
    if(length(input$date_of_birth) == 0 | length(input$date_of_surgery) == 0 | is.null(input$sex)){
      
      showModal(modal_box_patient_details_reactive())
      
    }else{
      removeModal()
      
      spine_region <- NULL
      diagnosis_category <- NULL
      dx <- NULL
      symptoms <- NULL
      
      showModal(startup_modal_box_diagnosis_symptoms(diagnosis_category_value = diagnosis_category,
                                                     primary_diagnosis_value = dx,
                                                     symptoms_initial_value = symptoms,
                                                     stage_number_value = input$stage_number,
                                                     staged_procedure_initial_value = FALSE,
                                                     multiple_approach_initial_value = FALSE,
                                                     spinal_regions_selected = spine_region,
                                                     ##
                                                     primary_or_revision = input$primary_revision,
                                                     levels_with_prior_decompression = input$open_canal,
                                                     prior_fusion_levels = input$prior_fusion_levels,
                                                     prior_instrumentation = input$prior_instrumentation,
                                                     left_prior_implants = input$left_revision_implants,
                                                     left_prior_implants_removed = input$left_revision_implants_removed,
                                                     right_prior_implants = input$right_revision_implants,
                                                     right_prior_implants_removed = input$right_revision_implants_removed,
                                                     left_rod_status = input$left_revision_rod_status,
                                                     left_implants_still_connected = input$left_revision_implants_connected_to_prior_rod,
                                                     right_rod_status = input$right_revision_rod_status,
                                                     right_implants_still_connected = input$right_revision_implants_connected_to_prior_rod
      ))
    }
  })
  
  modal_box_diagnosis_symptoms_procedure_reactive <- reactive({
    startup_modal_box_diagnosis_symptoms(diagnosis_category_value = input$diagnosis_category,
                                         primary_diagnosis_value = input$primary_diagnosis,
                                         symptoms_initial_value = input$symptoms,
                                         stage_number_value = input$stage_number,
                                         staged_procedure_initial_value = input$staged_procedure,
                                         multiple_approach_initial_value = input$multiple_approach,
                                         spinal_regions_selected = input$spinal_regions,
                                         ##
                                         primary_or_revision = input$primary_revision,
                                         levels_with_prior_decompression = input$open_canal,
                                         prior_fusion_levels = input$prior_fusion_levels,
                                         prior_instrumentation = input$prior_instrumentation,
                                         left_prior_implants = input$left_revision_implants,
                                         left_prior_implants_removed = input$left_revision_implants_removed,
                                         right_prior_implants = input$right_revision_implants,
                                         right_prior_implants_removed = input$right_revision_implants_removed,
                                         left_rod_status = input$left_revision_rod_status,
                                         left_implants_still_connected = input$left_revision_implants_connected_to_prior_rod,
                                         right_rod_status = input$right_revision_rod_status,
                                         right_implants_still_connected = input$right_revision_implants_connected_to_prior_rod
    )
  })
  
  observeEvent(input$open_diagnosis_symptoms_procedure_modal, ignoreInit = TRUE, {
    showModal(modal_box_diagnosis_symptoms_procedure_reactive())
  })
  
  
  #################~~~~~~~~ UPDATE FIELDS BASED ON PRIOR PATIENT FOUND #############
  observeEvent(list(existing_patient_data$patient_df, input$close_startup_modal), {
    if(existing_patient_data$match_found == TRUE){
      updateRadioGroupButtons(session = session, 
                              inputId = "primary_revision", 
                              choices = c("Primary", "Revision"), 
                              selected = "Revision", 
                              checkIcon = list(
                                yes = tags$i(class = "fas fa-check",
                                             style = "color: steelblue")),
                              label = NULL)
    }
  } )
  
  observeEvent(list(existing_patient_data$patient_df, input$close_startup_modal), {
    if(existing_patient_data$match_found == TRUE){
      if(nrow(existing_patient_data$patient_df %>% filter(str_detect(object, "screw") | str_detect(object, "hook"))) > 0){
        updateSwitchInput(session = session, inputId = "prior_instrumentation", value = TRUE)
      }
    }
  } )
  
  observeEvent(list(existing_patient_data$patient_df, input$close_startup_modal), ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(existing_patient_data$match_found == TRUE){
      left_prior_implants_df <- existing_patient_data$patient_df %>%
        filter(str_detect(object, "screw") | str_detect(object, "hook")) %>%
        filter(side == "left") %>%
        distinct()%>%
        mutate(level = if_else(level == "S2ai", "S2AI", level))
      
      if(nrow(left_prior_implants_df)>0){
        updateAwesomeCheckboxGroup(session = session, inputId = "left_revision_implants", selected = left_prior_implants_df$level)
      }
    }
  })
  
  observeEvent(list(existing_patient_data$patient_df, input$close_startup_modal), ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(existing_patient_data$match_found == TRUE){
      right_prior_implants_df <- existing_patient_data$patient_df %>%
        filter(str_detect(object, "screw") | str_detect(object, "hook")) %>%
        filter(side == "right") %>%
        distinct() %>%
        mutate(level = if_else(level == "S2ai", "S2AI", level))
      
      if(nrow(right_prior_implants_df)>0){
        updateAwesomeCheckboxGroup(session = session, inputId = "right_revision_implants", selected = right_prior_implants_df$level)
      }
    }
  })
  
  observeEvent(list(existing_patient_data$patient_df, input$close_startup_modal), ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(existing_patient_data$match_found == TRUE){
      prior_fusion_df <- existing_patient_data$patient_df %>%
        filter(str_detect(object, "fusion")) %>%
        distinct()
      if(nrow(prior_fusion_df)>0){
        updatePickerInput(session = session, inputId = "prior_fusion_levels", selected =  prior_fusion_df$level)
      }
    }
  })
  observeEvent(list(existing_patient_data$patient_df, input$close_startup_modal), ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(existing_patient_data$match_found == TRUE){
      prior_decompression_df <- existing_patient_data$patient_df %>%
        filter(str_detect(object, "decompression")) %>%
        distinct()
      if(nrow(prior_decompression_df)>0){
        updatePickerInput(session = session, inputId = "open_canal", 
                          selected = jh_convert_interspace_to_body_vector_function(prior_decompression_df$level))
      }
    }
  })
  
  
  ############ UPDATE DIAGNOSIS & SYMPTOMS OPTIONS ##############
  
  
  observeEvent(list(input$spinal_regions, input$diagnosis_category, input$open_diagnosis_symptoms_procedure_modal), ignoreInit = TRUE, {
    
    if(length(input$date_of_birth) > 0 & length(input$date_of_surgery)> 0){
      age <- round(interval(start = input$date_of_birth, end = input$date_of_surgery)/years(1))
    }else{
      age <- 999
    }
    
    spine_diagnosis_choices_list <- jh_filter_icd_codes_generate_vector_function(section_input = input$diagnosis_category, spine_region_input = input$spinal_regions, age = age)
    
    updatePickerInput(session = session,
                      inputId = "primary_diagnosis",
                      label = "Diagnosis Search:", 
                      choices = spine_diagnosis_choices_list,
                      options = pickerOptions(liveSearch = TRUE, 
                                              liveSearchNormalize = TRUE, 
                                              virtualScroll = 200))
  })
  
  
  
  
  ### UPDATE SYMPTOMS OPTIONS
  
  observeEvent(list(input$spinal_regions, input$diagnosis_category, input$open_diagnosis_symptoms_procedure_modal), ignoreInit = TRUE, {
    spine_regions_text <- str_to_lower(paste(input$spinal_regions, collapse = ", "))
    
    spine_dx_categories <- str_to_lower(paste(input$diagnosis_category, collapse = ", "))
    
    symptom_option_list <- list()
    if(str_detect(string = spine_regions_text, pattern = "cerv")){
      symptom_option_list$'Neck & Arms:' <- c("Neck Pain", "Left Arm Pain", "Right Arm Pain", "Left Arm Weakness", "Right Arm Weakness")
    }
    
    if(str_detect(string = spine_regions_text, pattern = "cerv") | str_detect(string = spine_regions_text, pattern = "thoracic")){
      symptom_option_list$'Myelopathy:' <- c("Myelopathy: Nurick 1 (Root Symptomts)",
                                             "Myelopathy: Nurick 2 (Normal gait but symptoms of cord compression)",
                                             "Myelopathy: Nurick 3 (Gait Abnormalities)",
                                             "Myelopathy: Nurick 4 (Significant Gait Abnormalities, preventing employment)",
                                             "Myelopathy: Nurick 5 (Depended on Assistive Device for Ambulating)",
                                             "Myelopathy: Nurick 6 (Wheelchair bound)")
    }
    
    
    if(str_detect(string = spine_regions_text, pattern = "thor")){
      symptom_option_list$'Thoracic:' <- c("Mid Back Pain", "Kyphosis")
    }
    
    if(str_detect(string = spine_regions_text, pattern = "lumb")){
      symptom_option_list$'Low Back & Legs:' = c("Low Back Pain", "Neurogenic Claudication", "Left Leg Pain", "Right Leg Pain", "Left Leg Weakness", "Right Leg Weakness")
    }
    
    if(str_detect(string = spine_dx_categories, pattern = "deformity")){
      symptom_option_list$'Deformity' <- c("Coronal Imbalance", 
                                           "Sagittal Imbalance (Inability to stand up, debilitating fatigue, difficulty maintaining horizontal gaze)", 
                                           "Poor Self Image",
                                           "Chin on chest with inability to maintain horizontal gaze")
    }
    
    symptom_option_list$'Urgent' = c("Loss of bladder control", 
                                     "Bowel Incontinence", 
                                     "Complete Loss of Motor & Sensory Function (Spinal Cord Injury)", 
                                     "Incomplete Loss of Motor & Sensory Function (Spinal Cord Injury)")
    
    symptom_option_list$'Other' = c("Other")
    
    updatePickerInput(session = session, 
                      inputId = "symptoms", 
                      label = NULL, 
                      choices = symptom_option_list, 
                      selected = input$symptoms)
    
  })
  
  
  ####     ####     #### TEXT UI  ###    ####     ####     #### 
  output$patient_details_ui <- renderUI({
    age <- round(interval(start = input$date_of_birth, end = input$date_of_surgery)/years(1))
    details <- glue("{input$patient_first_name} {input$patient_last_name}, {age}yo {input$sex}")
    
    tags$div(style = "font-size:24px; font-weight:bold; color:darkblue; font-family:sans-serif; font-style:italic", details)
  })
  
  output$diagnosis_symptoms_ui <- renderUI({
    if(length(input$date_of_surgery)>0){
      dos <- glue("Date of Surgery: {month(input$date_of_surgery, label = TRUE)} {day(input$date_of_surgery)}, {year(input$date_of_surgery)}")  
    }else{
      dos <- "Date of Surgery:"
    }
    
    if(!is.null(input$multiple_approach)){
      if(input$multiple_approach == TRUE){
        staged_procedure_text <- "Multiple Approach, Single Stage"   
      }else if(input$staged_procedure == FALSE && input$multiple_approach == FALSE){
        staged_procedure_text <- "Single Stage"   
      }else{
        staged_procedure_text <- paste("Stage", input$stage_number)
      }
    }else{
      staged_procedure_text <- " "
    }
    
    if(length(input$primary_diagnosis)>0){
      diagnosis <- glue("Diagnosis: {glue_collapse(x = input$primary_diagnosis, sep = ', ', last = ' and ')}")
    }else{
      diagnosis <- "Diagnosis:"
    }
    
    if(length(input$symptoms)>0){
      symptoms <- glue("Symptoms: {glue_collapse(x = input$symptoms, sep = ', ', last = ' and ')}")
    }else{
      symptoms <- "Symptoms:"
    }
    
    column(12, 
           tags$div(style = "font-size:24px; font-weight:bold; color:darkblue; font-family:sans-serif; font-style:italic", dos),
           tags$div(style = "font-size:20; font-weight:bold; color:darkblue; font-family:sans-serif; font-style:italic", staged_procedure_text),
           br(),
           tags$div(style = "font-size:18px; font-weight:bold; color:darkblue; font-family:sans-serif; font-style:italic", diagnosis),
           br(),
           tags$div(style = "font-size:18px; font-weight:bold; color:darkblue; font-family:sans-serif; font-style:italic", symptoms)
    )
  })
  
  
  output$currently_adding_ui <- renderUI({
    tags$table(
      tags$tr(
        tags$td(
          tags$div(style = "font-size:18px; font-weight:bold; font-family:sans-serif", "Currently Adding:  ")
        ),
        tags$td(
          tags$div(style = "font-size:18px; font-weight:bold; color:red; font-family:sans-serif; font-style:italic", str_to_title(string = str_replace_all(string = input$object_to_add, pattern = '_', replacement = ' ')))
        )
      )
    )
  })
  
  
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   Startup COMPLETE  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   Startup COMPLETE  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   Startup COMPLETE  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  
  
  
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   ADDITIONAL SURGICAL DETAILS MODAL UPDATES  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   ADDITIONAL SURGICAL DETAILS MODAL UPDATES #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  
  
  
  observeEvent(input$intraoperative_complications_vector, ignoreInit = TRUE, {
    if(any(str_detect(string = str_to_lower(input$intraoperative_complications_vector), pattern = "dur"))){
      updateTextInput(session = session, 
                      inputId = "postoperative_diagnosis", 
                      value = paste(input$postoperative_diagnosis,
                                    "Accidental puncture or laceration of dura during a procedure (G97.41)", sep = "; "))
      
    }
  }
  )
  
  additional_procedures_options_reactive_vector <- reactive({
    additional_procedures_choices <- list()
    if(length(input$prior_fusion_levels)>0){
      additional_procedures_choices$exploration_prior_fusion <- "Exploration of prior spinal fusion"
    }
    
    # additional_procedures_choices$irrigation_debridement <- "Incision and Drainage"
    
    additional_procedures_choices$open_biopsy_body <- "Open Biopsy of vertebral body"
    
    additional_procedures_choices$open_biopsy <- "Open Biopsy of extradural spinal lesion"
    
    additional_procedures_choices$dural_repair <- "Repair of dural/CSF leak"
    
    additional_procedures_choices$dural_graft <- "Dural Graft"
    
    if(nrow(left_revision_implants_reactive_list()$revision_implants_status_df)>0 || nrow(right_revision_implants_reactive_list()$revision_implants_status_df)>0){
      additional_procedures_choices$removal_instrumentation <- "Removal of spinal instrumentation"
    }
    
    additional_procedures_choices$vertebral_fx_open_tx <- "Open treatment of vertebral fracture"
    
    additional_procedures_choices$microscope <- "Intraoperative use of microscope for microdissection"
    
    if(nrow(all_objects_to_add_list$objects_df)>0){
      additional_procedures_choices$navigation <- "Use of stereotactic navigation system for screw placement"
    }
    
    additional_procedures_choices$head_positioning <- "Application of Cranial Tongs"
    
    ##nonsense
    if(any(str_detect(string = input$head_positioning, pattern = "Tongs"))){
      additional_procedures_choices$head_positioning <- "Application of Cranial Tongs"
    }
    if(any(str_detect(string = input$head_positioning, pattern = "Mayfield"))){
      additional_procedures_choices$head_positioning <- "Application of Cranial Tongs using Mayfield head holder"
    }
    
    if(any(str_detect(string = input$head_positioning, pattern = "Halo"))){
      age <- as.double(if_else(paste(input$date_of_birth) == "1900-01-01", "0", as.character(round(interval(start = paste(input$date_of_birth), end = paste(input$date_of_surgery))/years(1), 0))))
      if(age < 18 & age > 0){
        additional_procedures_choices$head_positioning <- "Application of Halo for thin skull osteology (pediatric)"
      }else{
        additional_procedures_choices$head_positioning <- "Application of Halo"
      }
    }
    
    additional_procedures_choices$halo_removal <- "Removal of tongs or Halo applied by another inidividual"
    additional_procedures_choices$neuromonitoring <- "Spinal Cord Monitoring"
    additional_procedures_choices$other <- "Other"
    
    unlist(additional_procedures_choices, use.names = FALSE)
  })
  
  observeEvent(list(input$left_revision_implants_removed, 
                    input$right_revision_implants_removed, 
                    input$prior_fusion_levels, 
                    input$head_positioning, 
                    input$approach_robot_navigation,
                    input$durotomy_repair_method,
                    input$primary_diganosis), ignoreInit = TRUE, {
                      
                      additional_procedures_list <- as.list(input$additional_procedures)
                      
                      if("Robotic" %in% input$approach_robot_navigation){
                        additional_procedures_list$robot <- "Robotic Assisted Spine Surgery"
                      }
                      if("Navigated" %in% input$approach_robot_navigation){
                        additional_procedures_list$navigation <- "Use of stereotactic navigation system for screw placement"
                      }
                      
                      if("Microscopic" %in% input$approach_robot_navigation){
                        additional_procedures_list$microscope <- "Intraoperative use of microscope for microdissection"
                      }
                      
                      if(length(input$left_revision_implants_removed) > 0 | length(input$right_revision_implants_removed) > 0){
                        additional_procedures_list$removal_instrumentation <- "Removal of spinal instrumentation"
                      }
                      if(length(input$prior_fusion_levels)>0){
                        additional_procedures_list$exploration_prior_fusion <- "Exploration of prior spinal fusion"
                      }
                      
                      if(any(str_detect(string = input$head_positioning, pattern = "Tongs"))){
                        additional_procedures_list$head_positioning <- "Application of Cranial Tongs"
                      }
                      if(any(str_detect(string = input$head_positioning, pattern = "Mayfield"))){
                        additional_procedures_list$head_positioning <- "Application of Cranial Tongs using Mayfield head holder"
                      }
                      
                      if(any(str_detect(string = input$head_positioning, pattern = "Halo"))){
                        age <- as.double(if_else(paste(input$date_of_birth) == "1900-01-01", "0", as.character(round(interval(start = paste(input$date_of_birth), end = paste(input$date_of_surgery))/years(1), 0))))
                        if(age < 18 & age > 0){
                          additional_procedures_list$head_positioning <- "Application of Halo for thin skull osteology (pediatric)"
                        }else{
                          additional_procedures_list$head_positioning <- "Application of Halo"
                        }
                      }
                      
                      if(length(input$durotomy_repair_method)>0){
                        if(str_detect(string = toString(input$durotomy_repair_method), pattern = "No Repair") == FALSE){
                          additional_procedures_list$dural_repair <- "Repair of dural/CSF leak"
                        }
                      }
                      
                      if(jh_determine_if_section_dx_function(diagnosis_vector = input$primary_diagnosis, section_to_determine = "trauma")){
                        additional_procedures_list$fracture <- "Open treatment of vertebral fracture"
                      }
                      if(jh_determine_if_section_dx_function(diagnosis_vector = input$primary_diagnosis, section_to_determine = "infection")){
                        additional_procedures_list$incision_drainage <- "Incision and Drainage"
                      }
                      if(jh_determine_if_section_dx_function(diagnosis_vector = input$primary_diagnosis, section_to_determine = "tumor")){
                        additional_procedures_list$tumor_biopsy <- "Open Biopsy of extradural spinal lesion"
                      }
                      
                      updateAwesomeCheckboxGroup(session = session, 
                                                 inputId = "additional_procedures", 
                                                 choices = additional_procedures_options_reactive_vector(),
                                                 selected = unlist(additional_procedures_list, use.names = FALSE))
                    })
  
  
  
  additional_procedures_vector_reactive <- reactive({
    additional_procedures_list <- as.list(input$additional_procedures)
    
    if("Other" %in% input$additional_procedures){
      additional_procedures_list$other <- input$additional_procedures_other
    }
    additional_procedures_list <- discard(additional_procedures_list, .p = ~ (.x == "Other"))
    
    if(any(input$dressing_details == "Wound Vac")){
      additional_procedures_list$wound_vac <- "Application of Wound Vac (negative pressure wound therapy; CPT = 97605)"    
    }
    
    unlist(additional_procedures_list, use.names = FALSE)
  })
  
  ######################################## RUN ADDITIONAL SURGICAL DETAILS MODAL AFTER ADVANCING TO NEXT TAB ###############################
  ######################################## RUN ADDITIONAL SURGICAL DETAILS MODAL AFTER ADVANCING TO NEXT TAB ###############################
  ######################################## RUN ADDITIONAL SURGICAL DETAILS MODAL AFTER ADVANCING TO NEXT TAB ###############################
  ######################################## RUN ADDITIONAL SURGICAL DETAILS MODAL AFTER ADVANCING TO NEXT TAB ###############################
  
  
  
  observeEvent(input$implant_details_complete, ignoreInit = TRUE, once = TRUE, {
    if(length(input$primary_diagnosis) >0){
      preop_dx <- glue_collapse(x = jh_add_codes_to_diagnosis_function(input$primary_diagnosis), sep = "; ")
      postop_dx <- glue_collapse(x = jh_add_codes_to_diagnosis_function(input$primary_diagnosis), sep = "; ")
    }else{
      preop_dx <- " "
      postop_dx <- " "
    }
    
    if(length(input$symptoms) >0){
      symptoms <- glue_collapse(x = map(.x = input$symptoms, 
                                        .f = ~ if_else(.x == "Other", input$symptoms_other, .x)),
                                sep = ", ", last = " and ")
    }else{
      symptoms <- " "
    }
    
    age <- round(interval(start = input$date_of_birth, end = input$date_of_surgery)/years(1))
    
    indications_list <- list()
    indications_list$opening <- glue("This is a {age} year-old {input$sex} that presented with ")
    
    if(symptoms != " "){
      indications_list$symptoms <- glue("{glue_collapse(x = symptoms, sep = ', ', last = ' and ')}")
    }else{
      indications_list$symptoms <- "***"
    }
    
    if(preop_dx != " "){
      indications_list$diagnosis <- glue("related to {preop_dx}.")
    }else{
      indications_list$diagnosis <- "***."
    }
    
    indications_list$risks <- "Risks and benefits of operative and nonoperative interventions were considered and discussed in detail and the patient has elected to proceed with surgery. I believe adequate informed consent was obtained."
    
    procedure_indications <- str_remove_all(str_remove_all(string = str_to_sentence(glue_collapse(indications_list, sep = " ")), 
                                                           pattern = " initial encounter"), 
                                            pattern = " initial encounter for")
    
    showModal(
      addition_surgical_details_modal_box_function(preoperative_diagnosis = preop_dx, 
                                                   postoperative_diagnosis = postop_dx,
                                                   indications = procedure_indications,
                                                   neuromonitoring = c("SSEP", "tc MEP"),
                                                   preop_antibiotics = c("Cefazolin (Ancef)"))
    )
  })
  
  ### CREATE VARIABLE FOR PROCEDURE APPROACH FOR GUIDING MODAL BOX CHOICES
  procedure_approach_reactive <- reactive({
    if(any(all_objects_to_add_list$objects_df$approach == "anterior")){
      anterior_approach <- TRUE
    }else{
      anterior_approach <- FALSE
    }
    
    if(any(all_objects_to_add_list$objects_df$approach == "posterior")){
      posterior_approach <- TRUE
    }else{
      posterior_approach <- FALSE
    }
    procedure_approach <- case_when(
      anterior_approach == TRUE & posterior_approach == TRUE ~ "combined", 
      anterior_approach == TRUE & posterior_approach == FALSE ~ "anterior",
      anterior_approach == FALSE & posterior_approach == TRUE ~ "posterior",
    )
    procedure_approach
  })
  
  ## NOW make a reactive modal for editing the info if needed ###
  modal_box_surgical_details_reactive <- reactive({
    addition_surgical_details_modal_box_function(editing_the_details = TRUE, ## this shows a different button at the footer
                                                 primary_surgeon_first_name_input = input$primary_surgeon_first_name,
                                                 primary_surgeon_last_name_input = input$primary_surgeon_last_name,
                                                 surgical_assistants = input$surgical_assistants,
                                                 preoperative_diagnosis = input$preoperative_diagnosis,
                                                 postoperative_diagnosis = input$postoperative_diagnosis, 
                                                 asa_class = input$asa_class,
                                                 anesthesia = input$anesthesia,
                                                 indications = input$indications,
                                                 neuromonitoring = input$neuromonitoring,
                                                 triggered_emg = input$triggered_emg,
                                                 pre_positioning_motors = input$pre_positioning_motors,
                                                 neuromonitoring_signal_stability = input$neuromonitoring_signal_stability,
                                                 preop_antibiotics = input$preop_antibiotics,
                                                 anti_fibrinolytic = input$anti_fibrinolytic,
                                                 txa_loading = input$txa_loading,
                                                 txa_maintenance = input$txa_maintenance, 
                                                 anterior_cervical_approach_details_checkbox = input$anterior_cervical_approach_details_checkbox)
  })
  
  ## NOW OBSERVE THE COMPLETION OF MODAL BOX 1 AND THEN SHOW MODAL BOX 2
  
  observeEvent(input$additional_surgical_details_1_complete, {
    add_procedures_list <- list()
    
    if(length(input$prior_fusion_levels)>0){
      add_procedures_list$exploration_of_fusion <- "Exploration of prior spinal fusion"
    }
    if(str_detect(string = str_to_lower(toString(input$primary_diagnosis)), pattern = "myelitis|infecti|bacteria|coccal|meningitis")){
      add_procedures_list$irrigation_debridement <- "Irrigation and Debridement"
    }
    
    showModal(
      addition_surgical_details_modal_box_2_function(required_options_missing = FALSE, 
                                                     additional_procedures_choices = additional_procedures_options_reactive_vector(),
                                                     additional_procedures = unlist(add_procedures_list, use.names = FALSE), 
                                                     procedure_approach = procedure_approach_reactive())
    )
  })
  

  
  observeEvent(input$additional_surgical_details_complete, ignoreInit = TRUE, {
    removeModal()
  })
  
  ### NOW SHOW MODAL 2 IF THERE ARE INCOMPLETE VALUES ###
  observeEvent(input$additional_surgical_details_complete, ignoreInit = TRUE,
               {
                 if(length(input$head_positioning) == 0 | length(input$closure_details) == 0 | length(input$dressing_details) == 0 | length(input$intraoperative_complications_yes_no) == 0){
                   showModal(
                     addition_surgical_details_modal_box_2_function(required_options_missing = TRUE,
                                                                    procedure_approach = procedure_approach_reactive(),
                                                                    head_positioning = input$head_positioning,
                                                                    surgical_findings = input$surgical_findings,
                                                                    specimens_removed = input$specimens_removed,
                                                                    ebl = input$ebl,
                                                                    urine_output = input$urine_output,
                                                                    crystalloids_administered = input$crystalloids_administered,
                                                                    colloids_administered = input$colloids_administered,
                                                                    transfusion = input$transfusion,
                                                                    cell_saver_transfused = input$cell_saver_transfused,
                                                                    prbc_transfused = input$prbc_transfused,
                                                                    ffp_transfused = input$ffp_transfused,
                                                                    cryoprecipitate_transfused = input$cryoprecipitate_transfused,
                                                                    platelets_transfused = input$platelets_transfused,
                                                                    intraoperative_complications_yes_no = input$intraoperative_complications_yes_no,
                                                                    intraoperative_complications_vector = input$intraoperative_complications_vector,
                                                                    other_intraoperative_complications = input$other_intraoperative_complications,
                                                                    durotomy_timing_input = input$durotomy_timing,
                                                                    durotomy_instrument_input = input$durotomy_instrument,
                                                                    durotomy_repair_method_input = input$durotomy_repair_method,
                                                                    additional_procedures_choices = additional_procedures_options_reactive_vector(),
                                                                    additional_procedures = input$additional_procedures,
                                                                    additional_procedures_other = input$additional_procedures_other,
                                                                    additional_end_procedure_details = input$additional_end_procedure_details,
                                                                    closure_details = input$closure_details,
                                                                    dressing_details = input$dressing_details,
                                                                    postop_dispo = input$postop_dispo,
                                                                    postop_abx = input$postop_abx,
                                                                    postop_map_goals = input$postop_map_goals,
                                                                    postop_imaging = input$postop_imaging,
                                                                    postop_pain = input$postop_pain,
                                                                    postop_activity = input$postop_activity,
                                                                    postop_brace = input$postop_brace,
                                                                    postop_diet = input$postop_diet,
                                                                    postop_dvt_ppx = input$postop_dvt_ppx,
                                                                    postop_drains_dressing = input$postop_drains_dressing,
                                                                    postop_followup = input$postop_followup,
                     )
                   )
                 }
               })
  
  
  ## NOW make a reactive modal for editing the info if needed ###
  modal_box_surgical_details_2_reactive <- reactive({
    
    addition_surgical_details_modal_box_2_function(
      procedure_approach = procedure_approach_reactive(),
      surgical_findings = input$surgical_findings,
      specimens_removed = input$specimens_removed,
      ebl = input$ebl,
      urine_output = input$urine_output,
      crystalloids_administered = input$crystalloids_administered,
      colloids_administered = input$colloids_administered,
      transfusion = input$transfusion,
      cell_saver_transfused = input$cell_saver_transfused,
      prbc_transfused = input$prbc_transfused,
      ffp_transfused = input$ffp_transfused,
      cryoprecipitate_transfused = input$cryoprecipitate_transfused,
      platelets_transfused = input$platelets_transfused,
      intraoperative_complications_yes_no = input$intraoperative_complications_yes_no,
      intraoperative_complications_vector = input$intraoperative_complications_vector,
      other_intraoperative_complications = input$other_intraoperative_complications,
      durotomy_timing_input = input$durotomy_timing,
      durotomy_instrument_input = input$durotomy_instrument,
      durotomy_repair_method_input = input$durotomy_repair_method,
      head_positioning = input$head_positioning,
      additional_procedures_choices = additional_procedures_options_reactive_vector(),
      additional_procedures = input$additional_procedures,
      additional_procedures_other = input$additional_procedures_other,
      additional_end_procedure_details = input$additional_end_procedure_details,
      closure_details = input$closure_details,
      dressing_details = input$dressing_details, 
      postop_dispo = input$postop_dispo,
      postop_abx = input$postop_abx,
      postop_map_goals = input$postop_map_goals,
      postop_imaging = input$postop_imaging,
      postop_pain = input$postop_pain,
      postop_activity = input$postop_activity,
      postop_brace = input$postop_brace,
      postop_diet = input$postop_diet,
      postop_dvt_ppx = input$postop_dvt_ppx,
      postop_drains_dressing = input$postop_drains_dressing,
      postop_followup = input$postop_followup
    )
  })
  
  ### NOw show Modal 1 if 'edit additional surgical details' is clicked: ###
  observeEvent(input$edit_additional_surgical_details,  {
    showModal(modal_box_surgical_details_reactive())
    
  })
  
  ### NOw show Modal 2 if 'edit additional surgical details' is clicked: ###
  
  observeEvent(input$editing_additional_surgical_details_1_complete, {
    removeModal() ## removes the current modal
    showModal(
      modal_box_surgical_details_2_reactive()
    )
  })
  
  
  
  
  ##### TEXT ON LEFT COLUM #####
  
  additional_surgical_details_reactive_table <- reactive({
    details_list <- list()
    
    details_list$'Primary Surgeon' <- paste(input$primary_surgeon_first_name, input$primary_surgeon_last_name)
    details_list$'Assistants' <- input$surgical_assistants
    details_list$'Preoperative Diagnosis:' <- input$preoperative_diagnosis
    details_list$'Postoperative Diagnosis:' <- input$postoperative_diagnosis
    details_list$'Surgical Indications:' <- input$indications
    details_list$'--' <- "--"
    details_list$'ASA Class:' <- input$asa_class
    details_list$'Anesthesia Type:' <- input$anesethesia
    details_list$'Neuromonitoring used:' <- toString(input$neuromonitoring)
    details_list$'Preoperative Antibiotics:' <- toString(input$preop_antibiotics)
    details_list$'Antifibrinolytic:' <- toString(input$anti_fibrinolytic)
    details_list$'Findings:' <- input$surgical_findings
    details_list$'Specimens:' <- input$specimens_removed
    details_list$'--' <- "--"
    details_list$'Estimated Blood Loss:' <- paste(input$ebl)     
    details_list$'Urine Output:' <- paste(input$urine_output)
    details_list$'Crystalloids:' <- paste(input$crystalloids_administered)
    details_list$'Colloids:' <- paste(input$colloids_administered)
    details_list$'Transfusions/Cell Saver:' <- if_else(input$transfusion == TRUE, "Yes", "No")
    
    if(!is.null(input$cell_saver_transfused) && input$cell_saver_transfused > 0){
      details_list$'Cell Saver Transfused (cc):' <- paste(input$cell_saver_transfused)
    }
    if(!is.null(input$prbc_transfused) && input$prbc_transfused > 0){
      details_list$'FFP units transfused:' <- paste(input$ffp_transfused)
    }
    if(!is.null(input$cryoprecipitate_transfused) && input$cryoprecipitate_transfused > 0){
      details_list$'Cryoprecipitate units transfused:' <- paste(input$cryoprecipitate_transfused)
    }
    if(!is.null(input$platelets_transfused) && input$platelets_transfused > 0){
      details_list$'Platelet units transfused:' <- paste(input$platelets_transfused)
    }
    
    details_list$'- -' <- "- -"
    # details_list$'Intraoperative Complications:' <- if_else(length(input$intraoperative_complications_yes_no)>0, 
    #                                                         as.character(input$intraoperative_complications_yes_no), 
    #                                                         "NA")
    if(!is.null(input$intraoperative_complications_yes_no) && input$intraoperative_complications_yes_no == "Yes" && length(input$intraoperative_complications_vector) > 0){
      details_list$'Intraoperative Complications:' <- as.character(glue_collapse(x = input$intraoperative_complications_vector, sep = "; "))
    }
    
    details_list$'Head Position:' <- paste(input$head_positioning)
    details_list$'Additional Procedures:' <- as.character(glue_collapse(x = additional_procedures_vector_reactive(), sep = "; "))
    details_list$'End of Procedure & Closure Details:' <- "---"
    
    if(!is.null(input$deep_drains_anterior) && input$deep_drains_anterior > 0){
      details_list$'Anterior Deep Drains:' <- paste(input$deep_drains_anterior)
    }
    if(!is.null(input$superficial_drains_anterior) && input$superficial_drains_anterior > 0){
      details_list$'Anterior Superficial Drains:' <- paste(input$superficial_drains_anterior)
    }
    
    if(!is.null(input$deep_drains_posterior) && input$deep_drains_posterior > 0){
      details_list$'Posterior Deep Drains:' <- paste(input$deep_drains_posterior)
    }
    if(!is.null(input$superficial_drains_posterior) && input$superficial_drains_posterior > 0){
      details_list$'Posterior Superficial Drains:' <- paste(input$superficial_drains_posterior)
    }
    
    details_list$'Used During Closure:' <- toString(input$additional_end_procedure_details)
    details_list$'Skin Closure:' <- toString(input$closure_details)
    details_list$'Skin/Dressing:' <- toString(input$dressing_details)
    
    details_list <- map(details_list, .f = ~ as.character(.x)) 
    
    enframe(details_list, name = "Variable", value = "Input") %>%
      unnest("Input") %>%
      mutate(Input = if_else(Input == "NA", "No Value Entered", Input))
      # replace_na(list(Input = "No Value Entered"))
    
      # replace_na(list(Input = "No Value Entered"))
    
  })
  
  output$additional_surgical_details_table <- renderTable({
    additional_surgical_details_reactive_table()
  })
  
  
  
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   ADDITIONAL SURGICAL DETAILS MODAL COMPLETE  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   ADDITIONAL SURGICAL DETAILS MODAL COMPLETE  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   ADDITIONAL SURGICAL DETAILS MODAL COMPLETE  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  
  ################################
  observeEvent(input$implants_complete,ignoreNULL = TRUE, ignoreInit = TRUE, {
    updateTabItems(session = session, inputId = "tabs", selected = "implant_details")
  })
  
  ############### generate modal to confirm the fusion levels #######
  
  
  observeEvent(input$implant_details_complete, ignoreNULL = TRUE, ignoreInit = TRUE, {
    updateTabItems(session = session, inputId = "tabs", selected = "operative_note")
  })
  
  observeEvent(input$return_to_add_implants_tab,ignoreNULL = TRUE, ignoreInit = TRUE, {
    updateTabItems(session = session, inputId = "tabs", selected = "patient_details_procedures")
  })
  
  observeEvent(input$return_to_add_implant_details_tab,ignoreNULL = TRUE, ignoreInit = TRUE, {
    updateTabItems(session = session, inputId = "tabs", selected = "implant_details")
  })
  
  
  observeEvent(input$implants_complete,ignoreNULL = TRUE, ignoreInit = TRUE, {
    
      showModal(
        modalDialog(title = "Confirm Fusion Levels", easyClose = TRUE, footer = modalButton(label = "Confirmed"),
                    box(width = 12, title = div(style = "font-size:16px; font-weight:bold; text-align:left", "Approach & Technique Specifics:"),
                        if(length(fusion_levels_computed_reactive_df()$level)>0){
                          prettyCheckboxGroup(
                          inputId = "fusion_levels_confirmed",
                          label = "Please Confirm The Fusion Levels:", 
                          bigger = TRUE,
                          choices = interbody_levels_df$level, 
                          selected = fusion_levels_computed_reactive_df()$level,
                          icon = icon("check"), 
                          status = "success"
                        )},
                        conditionalPanel(condition = "input.spine_approach.indexOf('Posterior') > -1",
                                         fluidRow(
                                           prettyRadioButtons(
                                             inputId = "approach_specified_posterior",
                                             label = "Posterior approach was:", 
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
                                           prettyRadioButtons(
                                             inputId = "approach_open_mis",
                                             label = "The procedure was performed:", 
                                             inline = TRUE,
                                             choices = c("Open",
                                                         "Tubular", 
                                                         "Endoscopic", 
                                                         "Mini Open",
                                                         "Percutaneous Screw"
                                             ),
                                             selected = "Open",
                                             icon = icon("check"), 
                                             bigger = TRUE,
                                             status = "success"
                                           )
                                         ),
                                         fluidRow(
                                           prettyCheckboxGroup(
                                             inputId = "approach_robot_navigation",
                                             label = "Select any modality used:", 
                                             inline = TRUE,
                                             choices = c("Microscopic", 
                                                         "Navigated", 
                                                         "Robotic"),
                                             icon = icon("check"), 
                                             bigger = TRUE,
                                             status = "success"
                                           )
                                         )
                        ),
                        conditionalPanel(condition = "input.spine_approach.indexOf('Anterior') > -1",
                                         prettyRadioButtons(
                                           inputId = "approach_specified_anterior",
                                           label = "Approach was:",
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
                    # column(6, 
                    #        prettyCheckboxGroup(
                    #          inputId = "fusion_levels_confirmed",
                    #          label = "Please Confirm The Fusion Levels:", 
                    #          bigger = TRUE,
                    #          choices = interbody_levels_df$level, 
                    #          selected = fusion_levels_computed_reactive_df()$level,
                    #          icon = icon("check"), 
                    #          status = "success"
                    #        )
                    # )
        )
      )
  })
  
  ###### ######  -----------   ######### UPDATE TABS ###### ######  -----------   ######### 
  
  observeEvent(input$spinal_regions, {
    spine_regions_text <- str_to_lower(paste(input$spinal_regions, collapse = ", "))
    
    upper_y <- if_else(str_detect(spine_regions_text, "cervical|occi"), 0.94,
                       if_else(str_detect(spine_regions_text, "cervicothoracic"), 0.94,
                               if_else(str_detect(spine_regions_text, "thoracic"), 0.77, 
                                       if_else(str_detect(spine_regions_text, "thoracolumbar"), 0.55, 
                                               if_else(str_detect(spine_regions_text, "lumbar"), 0.4, 
                                                       if_else(str_detect(spine_regions_text, "sacral"), 0.32, 0.4))))))
    
    lower_y <- if_else(str_detect(spine_regions_text, "lumbosacral"), 0.1,
                       if_else(str_detect(spine_regions_text, "lumbar"), 0.1, 
                               if_else(str_detect(spine_regions_text, "thoracolumbar"), 0.1, 
                                       if_else(str_detect(spine_regions_text, "cervicothoracic"), 0.6, 
                                               if_else(str_detect(spine_regions_text, "thoracic"), 0.35,
                                                       if_else(str_detect(spine_regions_text, "cervical|occ"), 0.71, 0.1))))))
    
    updateNoUiSliderInput(session = session, inputId = "crop_y", value = c(lower_y, upper_y))
  }
  )
  
  
  
  
  
  ###### ######  -----------   ######### Change to 6 Lumbar Vertebrae ###### ######  -----------   ######### 
  
  observeEvent(input$lumbar_vertebrae_count, ignoreInit = TRUE,{
    if(input$lumbar_vertebrae_count == "6"){
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
    
    if(input$lumbar_vertebrae_count == "5" | input$lumbar_vertebrae_count == "4"){
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
  
  output$tumor_resection_ui <- renderUI({
    
    if(jh_determine_if_section_dx_function(diagnosis_vector = input$primary_diagnosis, section_to_determine = "tumor")){
      actionBttn(
        inputId = "add_tumor_resection",
        size = "sm", block = TRUE,
        label = "Add Resection & Decompression for TUMOR",
        style = "simple",
        color = "primary"
      )
    }
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
                                "Partial Corpectomy" = "partial_corpectomy",
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
    ui_list <- list()
    if(nrow(osteotomy_df) > 0){
      ui_list$br <- br()
      
      ui_list$button <- actionBttn(
        inputId = "add_intervertebral_cage",
        size = "sm", block = TRUE,
        label = "Add Intervertebral Cage (After VCR/Corpectomy)",
        style = "simple",
        color = "primary"
      )
      ui_list
      
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
  
  observeEvent(list(input$add_implants, input$spinal_regions, input$spine_approach, input$crop_y),ignoreNULL = TRUE, {
    if(input$spine_approach == "Posterior"){
      
      implants_vector <-  c(
        'Pedicle Screw' =  'pedicle_screw', 
        'Pelvic Screw' =  'pelvic_screw',
        'Occipital Screw' =  'occipital_screw',
        'Pars Screw' =  'pars_screw',
        'Transarticular Screw' =  'transarticular_screw',
        'Lateral Mass Screw' =  'lateral_mass_screw',
        'Translaminar Screw' =  'translaminar_screw', 
        'Laminar Downgoing Hook' =  'laminar_downgoing_hook',
        'Laminar Upgoing Hook' =  'laminar_upgoing_hook',
        'Pedicle Hook' =  'pedicle_hook',
        'Tp Hook' =  'tp_hook',
        'Sublaminar Wire' =  'sublaminar_wire',
        'Tether (Spinous Process)' =  'tether'
      )
      
      implant_options <- keep(.x = implants_vector, .p = ~ any(str_detect((all_objects_y_range_df %>%
                                                                             filter(between(y, input$crop_y[1], input$crop_y[2])))$object, .x)))
      
      
      updateRadioGroupButtons(session = session, 
                              inputId = "object_to_add", 
                              choices = implant_options,
                              checkIcon = list(
                                yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                              ),
                              selected = "pedicle_screw"
      )
    }
  })
  
  observeEvent(input$add_decompressions,ignoreNULL = TRUE, ignoreInit = TRUE, {
    decompressions_vector <- c(
      "Laminoplasty" = "laminoplasty",
      "Decompression + Foraminotomies" = "sublaminar_decompression",
      "Central Laminectomy" = "laminectomy",
      "Laminectomy for Cyst Excision" = "laminectomy_for_facet_cyst",
      "Cervical Foraminotomy  " = "cervical_foraminotomy",
      "Laminotomy (Hemilaminectomy)" = "laminotomy",
      "Complete Facetectomy (Unilateral)" = "complete_facetectomy",
      "Diskectomy" = "diskectomy",
      "Transpedicular Decompression" = "transpedicular_approach",
      "Lateral Extraforaminal Approach for Decompression" = "lateral_extraforaminal_approach",
      "Costovertebral Decompression" = "costovertebral_approach",
      "Lateral Extracavitary Approach (modified)" = "lateral_extracavitary_approach"
    )
    
    decompressions_options <- jh_filter_objects_by_y_range_function(y_min = input$crop_y[1], y_max = input$crop_y[2], object_vector = decompressions_vector)
    
    updateRadioGroupButtons(session = session, 
                            inputId = "object_to_add",
                            choices = decompressions_options,
                            checkIcon = list(
                              yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                            ),
                            selected = "sublaminar_decompression"
    )
  })
  
  observeEvent(input$add_osteotomies,ignoreNULL = TRUE, ignoreInit = TRUE, {
    
    osteotomies_vector <- c("Grade 1 (Inferior Facetectomy)" = "grade_1",
                            # "Complete Facetectomy (Unilateral)" = "complete_facetectomy",
                            "Grade 2 (PCO)" = "grade_2", 
                            "Grade 3 (PSO)" = "grade_3",
                            "Grade 4 (Extended PSO)" = "grade_4", 
                            "Grade 5 (VCR)" = "grade_5", 
                            "Costotransversectomy" = "costotransversectomy")
    
    
    osteotomy_options <- jh_filter_objects_by_y_range_function(y_min = input$crop_y[1], y_max = input$crop_y[2], object_vector = osteotomies_vector)
    
    updateRadioGroupButtons(session = session, 
                            inputId = "object_to_add", 
                            choices = osteotomy_options,
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
    special_approach_vector <- c(                                
      "Transpedicular Decompression" = "transpedicular_approach",
      "Lateral Extraforaminal Approach for Decompression" = "lateral_extraforaminal_approach",
      "Costovertebral Decompression" = "costovertebral_approach",
      "Lateral Extracavitary Approach (modified)" = "lateral_extracavitary_approach", 
      "Costotransversectomy" = "costotransversectomy")
    
    
    special_approach_options <- jh_filter_objects_by_y_range_function(y_min = input$crop_y[1], y_max = input$crop_y[2], object_vector = special_approach_vector)
    
    updateRadioGroupButtons(session = session, 
                            inputId = "object_to_add", 
                            choices = special_approach_options,
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
                                        "Structural Allograft Strut" = "structural_allograft",
                                        "Incision & Drainage" = "incision_drainage"
                            ),
                            checkIcon = list(
                              yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                            ),
                            selected = "vertebroplasty"
    )
  })
  
  
  observeEvent(input$add_tumor_resection,ignoreNULL = TRUE, ignoreInit = TRUE, {
    updateRadioGroupButtons(session = session, 
                            inputId = "object_to_add",
                            choices = c(
                              "Laminectomy for biopsy & excision of extradural tumor" = "laminectomy_for_tumor",
                              # "Partial Excision of Vertebral Body (w/o decompression)" = "excision_body_no_decompression",
                              "Partial Vertebral Corpectomy via Lateral Extracavitary Aproach" = "corpectomy_extracavitary_tumor"
                            ),
                            checkIcon = list(
                              yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                            ),
                            selected = "excision_posterior_elements"
    )
  })
  
  
  ################----------  UPDATE ROD OPTIONS   ------------######################  
  ################----------  UPDATE ROD OPTIONS   ------------######################  
  ################----------  UPDATE ROD OPTIONS   ------------######################  
  
  ################----------  UPDATE ROD OPTIONS   ------------######################  
  ################----------  UPDATE ROD OPTIONS   ------------######################  
  ################----------  UPDATE ROD OPTIONS   ------------######################  
  
  ######### ######### CROSSLINKS ################# #########
  
  observeEvent(input$implants_complete, ignoreInit = TRUE, {
    implants_df <- all_objects_to_add_list$objects_df %>%
      filter(str_detect(object, "screw") | str_detect(object, "hook") | str_detect(object, "wire"))
    
    if(nrow(implants_df) > 2){
      level_options_vector <- jh_get_level_range_vector_function(object_df = implants_df, interspace_or_body_or_all = "body")
      
      updateCheckboxGroupButtons(session = session, 
                                 inputId = "crosslink_connectors", 
                                 choices = level_options_vector,
                                 label = "Add crosslink at:",
                                 selected = input$crosslink_connectors,
                                 checkIcon = list(
                                   yes = tags$i(class = "fa fa-check-square",
                                                style = "color: steelblue"),
                                   no = tags$i(class = "fa fa-square-o",
                                               style = "color: steelblue"))
      ) 
      
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
  ################----------  UPDATE ROD OPTIONS END  ------------######################  
  ################----------  UPDATE ROD OPTIONS END  ------------######################  
  ################----------  UPDATE ROD OPTIONS END  ------------######################  
  
  ################------------------  Fusion Levels   ----------------------######################  
  
  
  observeEvent(input$implants_complete, ignoreInit = TRUE, ignoreNULL = TRUE, {
    if(nrow(fusion_levels_computed_reactive_df()) > 0){
      updateSwitchInput(session = session, 
                        inputId = "fusion_procedure_performed", 
                        value = TRUE)
    }
  })
  
  observeEvent(input$implants_complete, ignoreInit = TRUE, ignoreNULL = TRUE, {
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
  
  
  
  ### RESET ALL AND REMOVE EVERYTHING
  observeEvent(input$reset_all, ignoreNULL = TRUE, ignoreInit = TRUE, {
    all_objects_to_add_list$objects_df <- all_objects_to_add_list$objects_df %>%
      filter(level == "xxx")
  })
  
  
  ########################################### object DETAILS REACTIVE ###########################################
  #### OBSERVE THE PLOT CLICK AND ADD APPROPRIATE object ####
  object_added_reactive_df <- reactive({
    
    if(input$object_to_add == "pelvic_screw"){
      object_currently_selected_to_add <- c("pelvic_screw_1", "pelvic_screw_2")
    }else{
      object_currently_selected_to_add <- input$object_to_add
    }
    # object_type_filtered_df <- implant_starts_df %>%
    #     filter(object %in% object_currently_selected_to_add)
    # 
    
    object_type_filtered_df <- all_implants_constructed_df %>%
      filter(object %in% object_currently_selected_to_add)
    
    implant_df <- nearPoints(
      df = object_type_filtered_df,
      coordinfo = input$plot_click,
      xvar = "x",
      yvar = "y",
      maxpoints = 1,
      threshold = 45
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
    
    if(any(str_detect(object_added_reactive_df()$object, "grade_"))){
      if(length(unique((all_objects_to_add_list$objects_df %>% filter(str_detect(object, "grade_")))$object)) > 1){
        all_objects_to_add_list$objects_df <- jh_filter_osteotomies_function(full_df_to_filter = all_objects_to_add_list$objects_df)
      }
    }
    
    
    
  })
  
  observeEvent(input$plot_double_click, ignoreNULL = TRUE, ignoreInit = TRUE, {
    implant_to_remove_df <- nearPoints(
      df = all_objects_to_add_list$objects_df,
      coordinfo = input$plot_double_click,
      xvar = "x",
      yvar = "y",
      maxpoints = 1,
      threshold = 50
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
      box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Interbody Implant Details:"), collapsible = TRUE, 
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
        unnest(composition) %>%
        mutate(device_name = map(.x = device_name_label, .f = ~input[[.x]])) %>%
        unnest(device_name) %>%
        mutate(height = map(.x = height_label, .f = ~input[[.x]])) %>%
        unnest(height) %>%
        mutate(integrated_fixation = map(.x = integrated_fixation_label, .f = ~if_else(is.null(input[[.x]]), "xx", input[[.x]]))) %>%
        unnest(integrated_fixation) %>%
        mutate(expandable = map(.x = expandable_label, .f = ~if_else(is.null(input[[.x]]), "xx", input[[.x]]))) %>%
        unnest(expandable) %>%
        mutate(other = map(.x = other_label, .f = ~if_else(is.null(input[[.x]]), "xx", input[[.x]]))) %>%
        unnest(other) %>%
        mutate(composition = as.character(composition), 
               other = as.character(other), 
               device_name = as.character(device_name)) %>%
        mutate(composition = if_else(is.na(composition), " ", composition)) %>%
        mutate(other = if_else(is.na(other), " ", other)) %>%
        mutate(device_name = if_else(is.na(device_name), " ", device_name)) %>%
        # replace_na(list(composition = " ", other = " ", device_name = " ")) %>%
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
        mutate(across(everything(), ~ as.character(.x))) %>%
        mutate(across(everything(), ~ replace_na(.x, " ")))
        
      
      
    }else{
      interbody_details_df <- tibble(level = character(), vertebral_number = double(), object = character(), approach = character(),  composition = character(), implant_statement = character())
    }
    interbody_details_df
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
  
  
  #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE REVISION IMPLANTS DF   #############~~~~~~~~~~~~~~~~~~~ ##################### 
  #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE REVISION IMPLANTS DF   #############~~~~~~~~~~~~~~~~~~~ ##################### 
  
  
  left_revision_implants_reactive_list <- reactive({
    if(length(input$left_revision_implants_removed)>0){
      removed_df <- tibble(level = input$left_revision_implants_removed, side = "left") %>%
        left_join(revision_implants_df) ## this is generated in Load coordinates
    }else{
      removed_df <- tibble(level = character(), vertebral_number = double(), x = double(), y = double())
    }
    
    if(length(input$left_revision_implants)>0){
      retained_df <- tibble(level = input$left_revision_implants, side = "left") %>%
        filter(level %in% input$left_revision_implants_removed == FALSE) %>%
        left_join(revision_implants_df) ## this is generated in Load coordinates
      
      ### create full summary table describing what is happening with the revision implants and rods
      if(length(input$left_revision_implants_removed)>0){
        revision_implants_status_df <- tibble(level = input$left_revision_implants, side = "left") %>%
          mutate(remove_retain = if_else(level %in% input$left_revision_implants_removed, "remove", "retain")) %>%
          left_join(revision_implants_df) %>%
          select(-object_constructed)
      }else{
        revision_implants_status_df <- tibble(level = input$left_revision_implants, side = "left") %>%
          mutate(remove_retain = "retain") %>%
          left_join(revision_implants_df)%>%
          select(-object_constructed)
      }
      if(input$left_revision_rod_status == "partially_retained_connected"){
        retained_df <- retained_df %>%
          mutate(prior_rod_connected = if_else(level %in% input$left_revision_implants_connected_to_prior_rod, "yes", "no"))
        
        revision_implants_status_df <- revision_implants_status_df %>%
          mutate(prior_rod_connected = if_else(level %in% input$left_revision_implants_connected_to_prior_rod, "yes", "no"))
      }
      if(input$left_revision_rod_status == "retained" | input$left_revision_rod_status == "retained_connected"){
        retained_df <- retained_df %>%
          mutate(prior_rod_connected = "yes")
        
        revision_implants_status_df <- revision_implants_status_df%>%
          mutate(prior_rod_connected = "yes")
      }
      if(input$left_revision_rod_status == "removed"){
        retained_df <- retained_df %>%
          mutate(prior_rod_connected = "no")
        
        revision_implants_status_df <- revision_implants_status_df%>%
          mutate(prior_rod_connected = "no")
      }
      
      revision_implants_status_df <- revision_implants_status_df %>%
        select(level, vertebral_number, side, remove_retain, prior_rod_connected, object, x, y)
      
    }else{
      retained_df <- tibble(level = character(), side = character(), vertebral_number = double(), object = character(), x = double(), y = double())
      revision_implants_status_df <- tibble(level = character(), vertebral_number = double(), object = character(), x = double(), y = double(), prior_rod_connected = character(), remove_retain = character())
    }
    list(retained_df = retained_df,
         removed_df = removed_df, 
         revision_implants_status_df = revision_implants_status_df)
  })
  
  observeEvent(left_revision_implants_reactive_list(), ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(nrow(left_revision_implants_reactive_list()$retained_df) < 2){
      updateAwesomeRadio(session = session, inputId = "left_revision_rod_status", selected = "removed")
    }
  })
  
  observeEvent(input$left_revision_rod_status, ignoreNULL = TRUE, {
    
    if(input$left_revision_rod_status == "partially_retained_connected"){
      updatePickerInput(session = session, inputId = "left_revision_implants_connected_to_prior_rod", 
                        choices = left_revision_implants_reactive_list()$retained_df$level, 
                        selected = left_revision_implants_reactive_list()$retained_df$level
      )
    }
    
    if(input$left_revision_rod_status == "partially_retained_connected"){
      updatePickerInput(session = session, inputId = "left_revision_implants_connected_to_prior_rod", 
                        choices = left_revision_implants_reactive_list()$retained_df$level, 
                        selected = left_revision_implants_reactive_list()$retained_df$level
      )
    }
  })
  
  ####### RIGHT REVISION IMPLANTS ----
  right_revision_implants_reactive_list <- reactive({
    if(length(input$right_revision_implants_removed)>0){
      removed_df <- tibble(level = input$right_revision_implants_removed, side = "right") %>%
        left_join(revision_implants_df) ## this is generated in Load coordinates
    }else{
      removed_df <- tibble(level = character(), vertebral_number = double(), x = double(), y = double())
    }
    
    if(length(input$right_revision_implants)>0){
      retained_df <- tibble(level = input$right_revision_implants, side = "right") %>%
        filter(level %in% input$right_revision_implants_removed == FALSE) %>%
        left_join(revision_implants_df) ## this is generated in Load coordinates
      
      ### create full summary table describing what is happening with the revision implants and rods
      if(length(input$right_revision_implants_removed)>0){
        revision_implants_status_df <- tibble(level = input$right_revision_implants, side = "right") %>%
          mutate(remove_retain = if_else(level %in% input$right_revision_implants_removed, "remove", "retain")) %>%
          left_join(revision_implants_df) %>%
          select(-object_constructed)
      }else{
        revision_implants_status_df <- tibble(level = input$right_revision_implants, side = "right") %>%
          mutate(remove_retain = "retain") %>%
          left_join(revision_implants_df)%>%
          select(-object_constructed)
      }
      if(input$right_revision_rod_status == "partially_retained_connected"){
        retained_df <- retained_df %>%
          mutate(prior_rod_connected = if_else(level %in% input$right_revision_implants_connected_to_prior_rod, "yes", "no"))
        
        revision_implants_status_df <- revision_implants_status_df %>%
          mutate(prior_rod_connected = if_else(level %in% input$right_revision_implants_connected_to_prior_rod, "yes", "no"))
      }
      if(input$right_revision_rod_status == "retained" | input$right_revision_rod_status == "retained_connected"){
        retained_df <- retained_df %>%
          mutate(prior_rod_connected = "yes")
        
        revision_implants_status_df <- revision_implants_status_df%>%
          mutate(prior_rod_connected = "yes")
      }
      if(input$right_revision_rod_status == "removed"){
        retained_df <- retained_df %>%
          mutate(prior_rod_connected = "no")
        
        revision_implants_status_df <- revision_implants_status_df%>%
          mutate(prior_rod_connected = "no")
      }
      
      revision_implants_status_df <- revision_implants_status_df %>%
        select(level, vertebral_number, side, remove_retain, prior_rod_connected, object, x, y)
      
    }else{
      retained_df <- tibble(level = character(), vertebral_number = double(), side = character(), object = character(), x = double(), y = double())
      revision_implants_status_df <- tibble(level = character(), vertebral_number = double(), object = character(), x = double(), y = double(), prior_rod_connected = character(), remove_retain = character())
    }
    list(retained_df = retained_df,
         removed_df = removed_df, 
         revision_implants_status_df = revision_implants_status_df)
  })
  
  output$revision_implants_table <- renderTable({
      revision_implants_df <- left_revision_implants_reactive_list()$revision_implants_status_df %>%
          union_all(right_revision_implants_reactive_list()$revision_implants_status_df)
      revision_implants_df
  })
  
  observeEvent(right_revision_implants_reactive_list(), ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(nrow(right_revision_implants_reactive_list()$retained_df) < 2){
      updateAwesomeRadio(session = session, inputId = "right_revision_rod_status", selected = "removed")
    }
  })
  
  observeEvent(input$right_revision_rod_status, ignoreNULL = TRUE, {
    
    if(input$right_revision_rod_status == "partially_retained_connected"){
      updatePickerInput(session = session, inputId = "right_revision_implants_connected_to_prior_rod", 
                        choices = right_revision_implants_reactive_list()$retained_df$level, 
                        selected = right_revision_implants_reactive_list()$retained_df$level
      )
    }
    
    if(input$right_revision_rod_status == "partially_retained_connected"){
      updatePickerInput(session = session, inputId = "right_revision_implants_connected_to_prior_rod", 
                        choices = right_revision_implants_reactive_list()$retained_df$level, 
                        selected = right_revision_implants_reactive_list()$retained_df$level
      )
    }
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
      # open_df <- open_canal_df %>%
      #     filter(level %in% input$open_canal)
      
      open_df <- implant_starts_df %>%
        filter(object == "laminectomy") %>%
        filter(level %in% input$open_canal) %>%
        mutate(category = "revision") %>%
        mutate(object_constructed = pmap(list(..1 = left_x,
                                              ..2 = superior_y,
                                              ..3 = right_x,
                                              ..4 = inferior_y,
                                              ..5 = width, 
                                              ..6 = object,
                                              ..7 = lateral_pars_x,
                                              ..8 = superior_tp_y,
                                              ..9 = side, 
                                              ..10 = inferior_pedicle_y,
                                              ..11 = inferior_facet_superior_border_y), 
                                         .f = ~ build_decompression_function(left_x = ..1,
                                                                             superior_y = ..2,
                                                                             right_x = ..3, 
                                                                             inferior_y = ..4, 
                                                                             top_width = ..5, 
                                                                             object = ..6, 
                                                                             x_lateral_pars = ..7, 
                                                                             y_inferior_tp = ..8,
                                                                             side = ..9, 
                                                                             inferior_pedicle_y = ..10,
                                                                             inferior_facet_superior_border_y = ..11)))
      
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
  observeEvent(left_revision_implants_reactive_list(), ignoreInit = TRUE, ignoreNULL = TRUE, {
    if(nrow(left_revision_implants_reactive_list()$removed_df)>0){
      geoms_list_revision_posterior$left_revision_implants_removed_sf_geom <- geom_sf(data = st_multipolygon(left_revision_implants_reactive_list()$removed_df$object_constructed), color = "black", fill = "grey99")
    }
    
    if(nrow(left_revision_implants_reactive_list()$retained_df)>0){
      geoms_list_revision_posterior$left_revision_implants_sf_geom <- geom_sf(data = st_multipolygon(left_revision_implants_reactive_list()$retained_df$object_constructed), fill = "black")
    }
  })
  
  observeEvent(right_revision_implants_reactive_list(), ignoreInit = TRUE, ignoreNULL = TRUE, {
    if(nrow(right_revision_implants_reactive_list()$removed_df)>0){
      geoms_list_revision_posterior$right_revision_implants_removed_sf_geom <- geom_sf(data = st_multipolygon(right_revision_implants_reactive_list()$removed_df$object_constructed), color = "black", fill = "grey99")
    }
    
    if(nrow(right_revision_implants_reactive_list()$retained_df)>0){
      geoms_list_revision_posterior$right_revision_implants_sf_geom <- geom_sf(data = st_multipolygon(right_revision_implants_reactive_list()$retained_df$object_constructed), fill = "black")
    }
  })
  
  
  ###### REVISION RODS ---
  observeEvent(list(left_revision_implants_reactive_list(), req(input$left_revision_rod_status), input$left_revision_implants_connected_to_prior_rod), ignoreInit = TRUE, ignoreNULL = TRUE, {
    
    if(nrow(left_revision_implants_reactive_list()$retained_df)>1){
      if(input$left_revision_rod_status == "removed"){
        geoms_list_revision_posterior$left_revision_rod_sf <- geom_sf(data = NULL)
      }else if(input$left_revision_rod_status == "retained_connected" | input$left_revision_rod_status == "retained"){
        # if(nrow(left_revision_implants_reactive_list()$retained_df)>1){
        left_revision_rod_matrix <- left_revision_implants_reactive_list()$retained_df %>%
          select(x, y) %>%
          mutate(y = if_else(y == max(y), y + 0.005, y)) %>%
          mutate(y = if_else(y == min(y), y - 0.005, y)) %>%
          arrange(rev(y)) %>%
          distinct() %>%
          as.matrix()
        
        geoms_list_revision_posterior$left_revision_rod_sf <- geom_sf(data = st_buffer(st_linestring(left_revision_rod_matrix), dist = 0.003, endCapStyle = "ROUND"), fill = "black")
      }else if(input$left_revision_rod_status == "partially_retained_connected"){
        
        if(nrow(left_revision_implants_reactive_list()$retained_df %>% filter(prior_rod_connected == "yes"))>1){
          left_revision_rod_matrix <- left_revision_implants_reactive_list()$retained_df %>%
            filter(prior_rod_connected == "yes") %>%
            select(x, y) %>%
            mutate(y = if_else(y == max(y), y + 0.005, y)) %>%
            mutate(y = if_else(y == min(y), y - 0.005, y)) %>%
            arrange(rev(y)) %>%
            distinct() %>%
            as.matrix() 
          geoms_list_revision_posterior$left_revision_rod_sf <- geom_sf(data = st_buffer(st_linestring(left_revision_rod_matrix), dist = 0.003, endCapStyle = "ROUND"), fill = "black")   
        }else{
          geoms_list_revision_posterior$left_revision_rod_sf <- geom_sf(data = NULL)
        }
        
      } else{
        geoms_list_revision_posterior$left_revision_rod_sf <- geom_sf(data = NULL)
      }
    }else{
      geoms_list_revision_posterior$left_revision_rod_sf <- geom_sf(data = NULL)
    }
  })
  
  observeEvent(list(right_revision_implants_reactive_list(), req(input$right_revision_rod_status), input$right_revision_implants_connected_to_prior_rod), ignoreInit = TRUE, ignoreNULL = TRUE, {
    
    if(nrow(right_revision_implants_reactive_list()$retained_df)>1){
      if(input$right_revision_rod_status == "removed"){
        geoms_list_revision_posterior$right_revision_rod_sf <- geom_sf(data = NULL)
      }else if(input$right_revision_rod_status == "retained_connected" | input$right_revision_rod_status == "retained"){
        # if(nrow(right_revision_implants_reactive_list()$retained_df)>1){
        right_revision_rod_matrix <- right_revision_implants_reactive_list()$retained_df %>%
          select(x, y) %>%
          mutate(y = if_else(y == max(y), y + 0.005, y)) %>%
          mutate(y = if_else(y == min(y), y - 0.005, y)) %>%
          arrange(rev(y)) %>%
          distinct() %>%
          as.matrix()
        
        geoms_list_revision_posterior$right_revision_rod_sf <- geom_sf(data = st_buffer(st_linestring(right_revision_rod_matrix), dist = 0.003, endCapStyle = "ROUND"), fill = "black")
      }else if(input$right_revision_rod_status == "partially_retained_connected"){
        
        if(nrow(right_revision_implants_reactive_list()$retained_df %>% filter(prior_rod_connected == "yes"))>1){
          right_revision_rod_matrix <- right_revision_implants_reactive_list()$retained_df %>%
            filter(prior_rod_connected == "yes") %>%
            select(x, y) %>%
            mutate(y = if_else(y == max(y), y + 0.005, y)) %>%
            mutate(y = if_else(y == min(y), y - 0.005, y)) %>%
            arrange(rev(y)) %>%
            distinct() %>%
            as.matrix() 
          geoms_list_revision_posterior$right_revision_rod_sf <- geom_sf(data = st_buffer(st_linestring(right_revision_rod_matrix), dist = 0.003, endCapStyle = "ROUND"), fill = "black")   
        }else{
          geoms_list_revision_posterior$right_revision_rod_sf <- geom_sf(data = NULL)
        }
        
      } else{
        geoms_list_revision_posterior$right_revision_rod_sf <- geom_sf(data = NULL)
      }
    }else{
      geoms_list_revision_posterior$right_revision_rod_sf <- geom_sf(data = NULL)
    }
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
                    input$left_linked_rods,
                    input$left_revision_rod_status,
                    left_revision_implants_reactive_list()), ignoreInit = TRUE, ignoreNULL = TRUE,{
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
                      
                      ############# MAKE THE RODS #############
                      left_rods_connectors_list <- build_unilateral_rods_list_function(accessory_rod_vector = accessory_vector,
                                                                                       satellite_rods_vector = satellite_vector,
                                                                                       intercalary_rods_vector = intercalary_vector,
                                                                                       intercalary_junction = junction,
                                                                                       linked_rods_vector = linked_vector,
                                                                                       revision_rods_retained_df = left_revision_implants_reactive_list()$retained_df,
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
                    input$right_linked_rods,
                    input$right_revision_rod_status,
                    right_revision_implants_reactive_list()), ignoreInit = TRUE, ignoreNULL = TRUE,{
                      ##########RODS ############
                      ############# right ROD #################
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
                      
                      ############# REVISION RODS #############
                      if((input$right_revision_rod_status) == "removed"){
                        retained_rod_df <- right_revision_implants_reactive_list()$retained_df
                      }else{
                        retained_rod_df <- tibble(level = character(), vertebral_number = double(), x = double(), y = double())
                      }
                      
                      ############# MAKE THE RODS #############
                      right_rods_connectors_list <- build_unilateral_rods_list_function(accessory_rod_vector = accessory_vector,
                                                                                        satellite_rods_vector = satellite_vector,
                                                                                        intercalary_rods_vector = intercalary_vector,
                                                                                        intercalary_junction = junction,
                                                                                        linked_rods_vector = linked_vector,
                                                                                        revision_rods_retained_df = right_revision_implants_reactive_list()$retained_df, # retained_rod_df, 
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
                        rods_list$right_connector_list_sf_geom <- NULL
                        rods_list$right_rod_list_sf_geom <- NULL
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
                      if(input$spine_approach == "Anterior"){
                        anterior_df <- all_objects_to_add_list$objects_df %>%
                          filter(approach == "anterior")
                        
                        anterior_geoms_list <- jh_make_anterior_geoms_function(all_anterior_objects_df = anterior_df)
                        
                        geoms_list_anterior_diskectomy$geoms <- anterior_geoms_list$geoms_list_anterior_diskectomy
                        geoms_list_anterior_interbody$geoms <- anterior_geoms_list$geoms_list_anterior_interbody
                        geoms_list_anterior_instrumentation$geoms <- anterior_geoms_list$geoms_list_anterior_instrumentation
                      }else{
                        all_posterior_df <- all_objects_to_add_list$objects_df %>%
                          filter(approach == "posterior")
                        
                        geoms_list_posterior$geoms <- jh_make_posterior_geoms_function(all_posterior_objects_df = all_posterior_df, plot_with_patterns = input$plot_with_patterns_true)
                      }
                    })
  
  
  ######### ~~~~~~~~~~~~~~  ############# MAKE REACTIVE PLOT    ######### ~~~~~~~~~~~~~~  ############# 
  ######### ~~~~~~~~~~~~~~  ############# MAKE REACTIVE PLOT    ######### ~~~~~~~~~~~~~~  ############# 
  spine_plan_plot_anterior <- reactive({
    if(input$spine_approach == "Anterior"){
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
      
      if(input$lumbar_vertebrae_count == "6"){
        l6_statement <- "Note: 6 Lumbar Vertebrae"
      }else{
        l6_statement <- " "
      }
      
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
    }
    
  })
  

  
  ##################### ~~~~~~~~~~~~~~~~ RENDER PLOTS ~~~~~~~~~~~~~~~~~~~ ##################
  ##################### ~~~~~~~~~~~~~~~~ RENDER PLOTS ~~~~~~~~~~~~~~~~~~~ ##################
  ##################### ~~~~~~~~~~~~~~~~ RENDER PLOTS ~~~~~~~~~~~~~~~~~~~ ##################
  
  output$spine_plan <- renderPlot({
    if(input$spine_approach == "Anterior"){
      spine_plan_plot_anterior() 
    }else{
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
        plan_table_geom <- geom_sf(data = NULL)
      }
      
      if(input$lumbar_vertebrae_count == "6"){
        l6_statement <- "Note: 6 Lumbar Vertebrae"
      }else{
        l6_statement <- " "
      }
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
      # spine_plan_plot_posterior() 
    }
  })
  
  observeEvent(input$lumbar_vertebrae_count, {
    if(input$spine_approach == "Anterior"){
      spine_plan_plot_anterior() 
    }else{
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
        plan_table_geom <- geom_sf(data = NULL)
      }
      
      if(input$lumbar_vertebrae_count == "6"){
        l6_statement <- "Note: 6 Lumbar Vertebrae"
      }else{
        l6_statement <- " "
      }
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
      # spine_plan_plot_posterior() 
    }
    
  })
  
  output$spine_plot_for_implants_tab <- renderPlot({
    if(input$multiple_approach == TRUE){
      # plot_grid(spine_plan_plot_anterior(), NULL, spine_plan_plot_posterior(), nrow = 1, rel_widths = c(1, -.1, 1))
    }else{
      if(input$spine_approach == "Anterior"){
        spine_plan_plot_anterior() 
      }else{
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
          plan_table_geom <- geom_sf(data = NULL)
        }
        
        if(input$lumbar_vertebrae_count == "6"){
          l6_statement <- "Note: 6 Lumbar Vertebrae"
        }else{
          l6_statement <- " "
        }
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
        # spine_plan_plot_posterior() 
      }
    }
    
  })
  
  
  ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
  ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
  ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
  
  ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
  ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
  ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
  
  ################------------------  FIRST UPDATE THE OPTIONS USING THE DETAILS ALREADY INPUTTED    ----------------------######################  
  
  observeEvent(list(input$left_revision_implants, input$left_revision_implants_removed), {
    
    if(length(input$left_revision_implants)>0){
      revision_implants_removed_vector <- discard(.x = input$left_revision_implants_removed, .p = ~ .x %in% input$left_revision_implants == FALSE)
      
      updateAwesomeCheckboxGroup(session = session, 
                                 inputId = "left_revision_implants_removed", 
                                 selected = revision_implants_removed_vector)
    }
  })
  
  observeEvent(input$fusion_procedure_performed, {
    if(input$fusion_procedure_performed == TRUE){
      updateAwesomeCheckboxGroup(session = session,
                                 inputId = "posterior_bone_graft", 
                                 selected = append(input$posterior_bone_graft, "Morselized Allograft"))
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
  
  
  postop_plan_list_reactive <- reactive({
    postop_plan <- list()
    ##########   dispo  #############
    if(length(input$postop_dispo) >0){
      postop_plan$postop_dispo_label <- paste("  -Postop Destination: ", glue_collapse(input$postop_dispo, sep = "; ", last = "; and "))
    }
    ##########   postop_abx  #############
    if(length(input$postop_abx) >0){
      postop_plan$postop_abx_label <- paste("  -Postop Abx: ", glue_collapse(input$postop_abx, sep = "; ", last = "; and "))
    }
    ##########   postop_map_goals  #############
    if(length(input$postop_map_goals) > 0){
      postop_plan$postop_map_goals_label <- paste("  -Postop MAP goals: ", glue_collapse(input$postop_map_goals, sep = "; ", last = "; and "))
    }
    ##########   postop_imaging  #############
    if(length(input$postop_imaging) >0){
      postop_plan$postop_imaging_label <- paste("  -Postop Imaging: ", glue_collapse(input$postop_imaging, sep = "; ", last = "; and "))
    }
    ##########   postop_pain  #############
    if(length(input$postop_pain) >0){
      postop_plan$postop_pain_label <- paste("  -Pain Control: ", glue_collapse(input$postop_pain, sep = "; ", last = "; and "))
    }
    ##########   postop_activity  #############
    if(length(input$postop_activity) >0){
      postop_plan$postop_activity_label <- paste("  -Activity: ", glue_collapse(input$postop_activity, sep = "; ", last = "; and "))
    }
    ##########   postop_brace  #############
    if(length(input$postop_brace) >0){
      postop_plan$postop_brace_label <- paste("  -Bracing: ", glue_collapse(input$postop_brace, sep = "; ", last = "; and "))
    }
    ##########   postop_diet  #############
    if(length(input$postop_diet) >0){
      postop_plan$postop_diet_label <- paste("  -Diet: ", glue_collapse(input$postop_diet, sep = "; ", last = "; and "))
    }
    ##########   postop_dvt_ppx  #############
    if(length(input$postop_dvt_ppx) >0){
      postop_plan$postop_dvt_label <- paste("  -DVT PPX/Anticoag/Antiplatelet: ", glue_collapse(input$postop_dvt_ppx, sep = "; ", last = "; and "))
    }
    ##########   postop_drains_dressing  #############
    if(length(input$postop_drains_dressing) >0){
      postop_plan$postop_drains_dressing_label <- paste("  -Drains & Dressing: ", glue_collapse(input$postop_drains_dressing, sep = "; ", last = "; and "))
    }
    ##########   postop_followup  #############
    if(length(input$postop_followup) >0){
      postop_plan$postop_followup_label <- paste("  -Follow-up: ", glue_collapse(input$postop_followup, sep = "; ", last = "; and "))
    }
    postop_plan
    
  })
  
  
 ##posterior approach objects for testing 
  
  output$posterior_approach_objects_for_op_note_df <- renderTable({
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
    
    posterior_screws_df <- posterior_approach_objects_df %>%
      filter(approach == "posterior") %>%
      filter(str_detect(object, "screw")) %>%
      mutate(screw_implant = str_to_lower(paste(level, object, sep = "_"))) %>%
      left_join(screw_details_redcap_df_reactive() %>% rename(side = screw_side)) %>%
      select(level, approach, side, object, screw_size_type) %>%
      mutate(screw_size_type = as.character(screw_size_type)) %>%
      replace_na(list(screw_size_type = " "))
    
    posterior_approach_objects_df <- posterior_approach_objects_df%>%
      left_join(posterior_screws_df) %>%
      mutate(screw_size_type = as.character(screw_size_type)) %>%
      replace_na(list(screw_size_type = " "))
    
    posterior_approach_objects_df
    
  })
  
  ######################
  
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
    fluids_transfusions_list <- list(crystalloids_statement =if_else(is.na(input$crystalloids_administered), glue("none"), glue("-Crystalloids: {input$crystalloids_administered}cc")),
                                     colloids_statement =if_else(is.na(input$colloids_administered), glue("none"), glue("-Colloids: {input$colloids_administered}cc")) ,
                                     cell_saver_transfused_statement =if_else(is.na(input$cell_saver_transfused) | input$cell_saver_transfused == 0, glue("none"), glue("-Cell Saver: {input$cell_saver_transfused}cc")),
                                     prbc_transfused_statement =if_else(is.na(input$prbc_transfused)| input$prbc_transfused == 0, glue("none") , glue("-pRBC's transfused: {input$prbc_transfused} {if_else(input$prbc_transfused >1, 'units', 'unit')}")),
                                     ffp_transfused_statement =if_else(is.na(input$ffp_transfused)| input$ffp_transfused == 0, glue("none") , glue("-FFP transfused: {input$ffp_transfused} {if_else(input$ffp_transfused >1, 'units', 'unit')}")), 
                                     cryo_transfused_statement =if_else(is.na(input$cryoprecipitate_transfused) | input$cryoprecipitate_transfused == 0, glue("none"), glue("-Cryoprecipitate transfused: {input$cryoprecipitate_transfused} {if_else(input$cryoprecipitate_transfused >1, 'units', 'unit')}")), 
                                     platelets_transfused_statement =if_else(is.na(input$platelets_transfused) | input$platelets_transfused == 0, glue("none"), glue("-Platelets transfused: {input$platelets_transfused} {if_else(input$platelets_transfused >1, 'units', 'unit')}")))
    
    
    fluids_transfusions_list <- discard(.x = fluids_transfusions_list, .p = ~ .x == "none")
    
    if(length(discard(.x = fluids_transfusions_list, .p = ~ .x == "none")) == 0){
      fluids_transfusions_statement <- "See Anesthesia Records"
    }else{
      fluids_transfusions_statement <- glue_collapse(fluids_transfusions_list, sep = '\n')
    }
    
    ################# BUILD ANTERIOR OR POSTERIOR OR COMBINED OP NOTES ############
    
    #### NO OBJECTS ADDED OP NOTE: ###
    if(nrow(all_objects_to_add_list$objects_df) == 0){
      procedure_results_list <- list()
      revision_implants_df <- left_revision_implants_reactive_list()$revision_implants_status_df %>%
        union_all(right_revision_implants_reactive_list()$revision_implants_status_df)

      
      op_note_list <- list()
      
      op_note_list$"\nPatient:" <- paste(input$patient_first_name, input$patient_last_name)
      op_note_list$"\nDate of Surgery:" <- as.character(input$date_of_surgery)
      op_note_list$"\nPrimary Surgeon:" <- if_else(!is.null(input$primary_surgeon_last_name), as.character(paste(input$primary_surgeon_first_name, input$primary_surgeon_last_name)), " ")
      op_note_list$"\nSurgical Assistants:" <- if_else(!is.null(input$surgical_assistants), as.character(input$surgical_assistants), " ") 
      op_note_list$"\nPre-operative Diagnosis:" <- if_else(!is.null(input$preoperative_diagnosis), paste0("-", as.character(input$preoperative_diagnosis)), " ")
      op_note_list$"\nPost-operative Diagnosis:" <- if_else(!is.null(input$postoperative_diagnosis), glue_collapse(x = unlist(str_split(input$postoperative_diagnosis, pattern = "; ")), sep = "\n"), " ")
      op_note_list$"\nIndications:" <- if_else(!is.null(input$indications), as.character(input$indications), " ") 
      op_note_list$"\nProcedures Performed:" <- if_else(!is.null(procedure_results_list$procedures_numbered_paragraph), as.character(procedure_results_list$procedures_numbered_paragraph), " ")
      op_note_list$"\nSurgical Findings:" <- if_else(!is.na(input$surgical_finding), as.character(input$surgical_finding), " ") 
      op_note_list$"\nSpecimens Removed:" <- if_else(!is.na(input$specimens_removed), as.character(input$specimens_removed), " ") 
      op_note_list$"\nEstimated Blood Loss:" <- if_else(!is.na(input$ebl), as.character(input$ebl), " ") 
      op_note_list$"\nFluids/Transfusions:" <- if_else(!is.na(fluids_transfusions_statement), as.character(fluids_transfusions_statement), " ")
      op_note_list$"\nIntraoperative Complications:" <- if_else(!is.na(complication_statement), as.character(complication_statement), " ")
      op_note_list$"\nSurgery Description:" <- "***"
      # op_note_list$"\nSurgery Description:" <- if_else(!is.null(procedure_results_list$procedure_details_paragraph), as.character(procedure_results_list$procedure_details_paragraph), " ") 
      op_note_list$"\nPostop Plan:" <- if_else(length(postop_plan_list_reactive()) >0, glue_collapse(postop_plan_list_reactive(), sep = "\n"), " ")
      
      secion_headers_df <- enframe(op_note_list, name = "section", value = "result") %>%
        unnest() %>%
        mutate(row = row_number()) %>%
        pivot_longer(cols = c(section, result), names_to = "text", values_to = "full_text_vector") %>%
        select(row, full_text_vector)
      
    }else{
      
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
      
      if(any(anterior_approach_objects_df$object == "anterior_plate")){
        anterior_approach_objects_df <- anterior_approach_objects_df %>% 
          filter(object == "anterior_plate") %>%
          select(level, vertebral_number, side, object)%>%
          separate(col = level, into = c("cranial_level", "caudal_level"), sep = "-") %>%
          mutate(side_left = "left", side_right = "right") %>%
          select(-side, vertebral_number) %>%
          pivot_longer(cols = c(cranial_level, caudal_level), names_to = "cranial_caudal", values_to = "level") %>%
          select(level, object, side_left, side_right) %>%
          pivot_longer(cols = c(side_left, side_right), names_to = "remove", values_to = "side") %>%
          distinct() %>%
          mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
          select(level, vertebral_number, side, object) %>%
          mutate(object = "anterior_plate_screw") %>%
          union_all(anterior_approach_objects_df)
          
        anterior_screws_df <- anterior_approach_objects_df %>%
          filter(str_detect(object, "screw")) %>%
          mutate(screw_implant = str_to_lower(paste(level, object, sep = "_"))) %>%
          left_join(screw_details_redcap_df_reactive() %>% rename(side = screw_side)) %>%
          select(level, approach, side, object, screw_size_type) %>%
          mutate(screw_size_type = as.character(screw_size_type)) %>%
          replace_na(list(screw_size_type = " "))
        
        anterior_approach_objects_df  <- anterior_approach_objects_df%>%
          left_join(anterior_screws_df) %>%
          mutate(screw_size_type = as.character(screw_size_type)) %>%
          replace_na(list(screw_size_type = " "))
        
      }


      
      procedure_results_list <- list()
      
      if(length(input$fusion_levels_confirmed)>0){
        fusions_df <- tibble(level = input$fusion_levels_confirmed) %>%
          left_join(levels_numbered_df)
      }else{
        fusions_df <- tibble(level = character(), vertebral_number = double(), object = character())
      }
      
      if(nrow(posterior_approach_objects_df) > 0){
        if(nrow(screw_details_redcap_df_reactive()) > 0){
          posterior_screws_df <- posterior_approach_objects_df %>%
            filter(approach == "posterior") %>%
            filter(str_detect(object, "screw")) %>%
            mutate(screw_implant = str_to_lower(paste(level, object, sep = "_"))) %>%
            left_join(screw_details_redcap_df_reactive() %>% rename(side = screw_side)) %>%
            select(level, approach, side, object, screw_size_type) %>%
            mutate(screw_size_type = as.character(screw_size_type)) %>%
            replace_na(list(screw_size_type = " "))
          
          posterior_approach_objects_df <- posterior_approach_objects_df%>%
            left_join(posterior_screws_df) %>%
            mutate(screw_size_type = as.character(screw_size_type)) %>%
            replace_na(list(screw_size_type = " "))
        }
        
        revision_implants_df <- left_revision_implants_reactive_list()$revision_implants_status_df %>%
          union_all(right_revision_implants_reactive_list()$revision_implants_status_df)
        
        
        ### make neuromonitoring list
        neuromonitoring_input_list <- list()
        neuromonitoring_input_list$modalities <- input$neuromonitoring
        neuromonitoring_input_list$emg <- if_else(str_detect(string = paste(input$neuromonitoring, collapse = ", "),pattern = "EMG"), input$triggered_emg, "")
        if(neuromonitoring_input_list$emg == "No"){
          neuromonitoring_input_list$emg <- ""
        }
        neuromonitoring_input_list$pre_positioning_motors <- if_else(str_detect(string = paste(input$neuromonitoring, collapse = ", "), pattern = "tcMEP"), input$pre_positioning_motors, "")
        if(neuromonitoring_input_list$pre_positioning_motors == "Pre-positioning motors not obtained"){
          neuromonitoring_input_list$pre_positioning_motors <- ""
        }
        neuromonitoring_input_list$neuromonitoring_signal_stability <- if_else(str_detect(string = paste(input$neuromonitoring, collapse = ", "), pattern = "SSEP"),
                                                                               as.character(input$neuromonitoring_signal_stability),
                                                                               "")

        ### NOW MAKE PROCEDURES LIST
        procedure_results_list_posterior <- list()
        
        procedure_results_list_posterior <- op_note_posterior_function(all_objects_to_add_df = posterior_approach_objects_df,
                                                                       fusion_levels_df = fusions_df,
                                                                       head_position = input$head_positioning,
                                                                       neuromonitoring_list = neuromonitoring_input_list, ## this is a named list with names: modalities, emg, and pre_positioning_motors
                                                                       revision_decompression_vector = input$open_canal,
                                                                       revision_implants_df = revision_implants_df,
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
        
        anterior_cervical_approach_details <- list()
        if(any(str_detect(str_to_lower(input$anterior_cervical_approach_details_checkbox), "caspar"))){
          anterior_cervical_approach_details$caspar_pins <- "A small start point was created using a burr, and Caspar pins were placed into the cranial and caudal vertebrae to allow for distraction across the disc space."
        }
        
        if(any(str_detect(str_to_lower(input$anterior_cervical_approach_details_checkbox), "microscop"))){
          anterior_cervical_approach_details$microscope_use <- "The microscope was then brought into the field for the next steps in the procedure."
        }
        if(length(anterior_cervical_approach_details)>0){
          anterior_cervical_approach_details <- as.character(glue_collapse(x = anterior_cervical_approach_details, sep = " "))
        }else{
          anterior_cervical_approach_details <- "none"
        }
        
        #### BONE GRAFT DATAFRAME
        anterior_biologics_list <- list()
        if(any(input$anterior_bone_graft == "Morselized Allograft")){
          anterior_biologics_list$'Morselized Allograft' <- input$anterior_allograft_amount
        }else{
          anterior_biologics_list$'Morselized Allograft' <- 0
        }
        if(any(input$anterior_biologics == "Bone Marrow Aspirate")){
          anterior_biologics_list$'Bone Marrow Aspirate' <- input$anterior_bone_marrow_aspirate_volume
        }else{
          anterior_biologics_list$'Bone Marrow Aspirate' <- 0
        }
        if(any(input$anterior_biologics == "Cell Based Allograft")){
          anterior_biologics_list$'Cell Based Allograft' <- input$anterior_cell_based_allograft_volume
        }else{
          anterior_biologics_list$'Cell Based Allograft' <- 0
        }
        if(any(input$anterior_biologics == "DBM")){
          anterior_biologics_list$'DBM' <- input$anterior_dbm_volume
        }else{
          anterior_biologics_list$'DBM' <- 0
        }
        if(any(input$anterior_biologics == "iFactor")){
          anterior_biologics_list$'iFactor' <- input$anterior_ifactor_volume
        }else{
          anterior_biologics_list$'iFactor' <- 0
        }
        anterior_biologics_df_formatted <- enframe(anterior_biologics_list) %>%
          unnest() %>%
          filter(value != 0)
        
        ### make neuromonitoring list
        neuromonitoring_input_list <- list()
        neuromonitoring_input_list$modalities <- input$neuromonitoring
        neuromonitoring_input_list$emg <- if_else(str_detect(string = paste(input$neuromonitoring, collapse = ", "),pattern = "EMG"), input$triggered_emg, "")
        if(neuromonitoring_input_list$emg == "No"){
          neuromonitoring_input_list$emg <- ""
        }
        neuromonitoring_input_list$pre_positioning_motors <- if_else(str_detect(string = paste(input$neuromonitoring, collapse = ", "), pattern = "tcMEP"), input$pre_positioning_motors, "")
        if(neuromonitoring_input_list$pre_positioning_motors == "Pre-positioning motors not obtained"){
          neuromonitoring_input_list$pre_positioning_motors <- ""
        }
        neuromonitoring_input_list$neuromonitoring_signal_stability <- if_else(str_detect(string = paste(input$neuromonitoring, collapse = ", "), pattern = "SSEP"),
                                                                               as.character(input$neuromonitoring_signal_stability),
                                                                               "")        
        
        procedure_results_list_anterior <- op_note_anterior_function(all_objects_to_add_df = anterior_approach_objects_df,
                                                                     anterior_approach_laterality = input$approach_specified_anterior,
                                                                     approach_statement = anterior_cervical_approach_details, 
                                                                     antibiotics = input$preop_antibiotics, 
                                                                     additional_procedures_vector = input$additional_procedures, 
                                                                     neuromonitoring_list = neuromonitoring_input_list,
                                                                     bmp = anterior_bmp_dose_reactive(),
                                                                     anterior_biologics_df = anterior_biologics_df_formatted,
                                                                     bone_graft_vector = input$anterior_bone_graft,
                                                                     morselized_allograft = input$anterior_allograft_amount,
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
      
      
      op_note_list <- list()
      
      op_note_list$"\n*Patient:" <- paste(input$patient_first_name, input$patient_last_name)
      op_note_list$"\n*Date of Surgery:" <- as.character(input$date_of_surgery)
      op_note_list$"\n*Primary Surgeon:" <- paste(input$primary_surgeon_first_name, input$primary_surgeon_last_name)
      op_note_list$"\n*Surgical Assistants:" <- input$surgical_assistants
      op_note_list$"\n*Preprocedure ASA Class:" <- input$asa_class
      op_note_list$"\n*Anesthesia:" <- input$anesthesia
      op_note_list$"\n*Intraoperative Complications:" <- complication_statement
      op_note_list$"\n*Pre-operative Diagnosis:" <- glue_collapse(unlist(str_split(paste0("  -", input$preoperative_diagnosis), pattern = "; ")), sep = "\n  -")
      op_note_list$"\n*Post-operative Diagnosis:" <- glue_collapse(unlist(str_split(paste0("  -", input$postoperative_diagnosis), pattern = "; ")), sep = "\n  -")
      op_note_list$"\n*Indications:" <- input$indications
      op_note_list$"\n*Estimated Blood Loss:" <- if_else(is.na(input$ebl), "See anesthesia records", paste(input$ebl))
      op_note_list$"\n*Fluids/Transfusions:" <- fluids_transfusions_statement
      op_note_list$"\n*Surgical Findings:" <- if_else(input$surgical_findings == "", "none", input$surgical_findings)
      op_note_list$"\n*Specimens Taken:" <- if_else(input$specimens_removed == "", "none", input$specimens_removed)
      op_note_list$"\n*Procedures Performed:" <- procedure_results_list$procedures_numbered_paragraph
      op_note_list$"\n*Procedure  Description:" <- procedure_results_list$procedure_details_paragraph
      op_note_list$"\n*Postop Plan:" <- if_else(length(postop_plan_list_reactive()) >0, glue_collapse(postop_plan_list_reactive(), sep = "\n"), " ")
      
      secion_headers_df <- enframe(op_note_list, name = "section", value = "result") %>%
        unnest() %>%
        mutate(row = row_number()) %>%
        pivot_longer(cols = c(section, result), names_to = "text", values_to = "full_text_vector") %>%
        select(row, full_text_vector)
      
    }
    
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
  
  ##############################~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE TABLES    #############~~~~~~~~~~~~~~~~~~~ ##################### #################
  ##############################~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE TABLES    #############~~~~~~~~~~~~~~~~~~~ ##################### #################
  ##############################~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE TABLES    #############~~~~~~~~~~~~~~~~~~~ ##################### #################
  
  ##############################~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE TABLES    #############~~~~~~~~~~~~~~~~~~~ ##################### #################
  ##############################~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE TABLES    #############~~~~~~~~~~~~~~~~~~~ ##################### #################
  ##############################~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE TABLES    #############~~~~~~~~~~~~~~~~~~~ ##################### #################
  
  ##############################~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE TABLES    #############~~~~~~~~~~~~~~~~~~~ ##################### #################
  ##############################~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE TABLES    #############~~~~~~~~~~~~~~~~~~~ ##################### #################
  ##############################~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE TABLES    #############~~~~~~~~~~~~~~~~~~~ ##################### #################
  
  ############### ############### FIRST, COMPILE EACH OF THE TABLES THAT WILL BE USED FOR UPLOADING TO REDCAP:    ###############     ############### 
  ############### ############### FIRST, COMPILE EACH OF THE TABLES THAT WILL BE USED FOR UPLOADING TO REDCAP:    ###############     ############### 
  ############### ############### FIRST, COMPILE EACH OF THE TABLES THAT WILL BE USED FOR UPLOADING TO REDCAP:    ###############     ############### 
  ###### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ALL INPUTS  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ######## 
  
  all_inputs_reactive_list <- reactive({
    all_inputs_list <- reactiveValuesToList(input, all.names = FALSE)
    
    all_inputs_list <- keep(.x = all_inputs_list, .p = ~ !is.null(.x))

  })
  

  all_inputs_trimmed_reactive_df <- reactive({
    enframe(all_inputs_reactive_list()) %>%
      mutate(result = map(.x = value, .f = ~ as.character(glue_collapse(.x, sep = "-AND-")))) %>%
      select(-value) %>%
      unnest(result) %>%
      filter(str_detect(string = name, pattern = "screw_length", negate = TRUE)) %>%
      filter(str_detect(string = name, pattern = "screw_diameter", negate = TRUE)) %>%
      filter(str_detect(string = name, pattern = "screw_type", negate = TRUE)) %>%
      filter(str_detect(string = name, pattern = "rod_material", negate = TRUE)) %>%
      filter(str_detect(string = name, pattern = "rods_eligible", negate = TRUE)) %>%
      filter(str_detect(string = name, pattern = "reset", negate = TRUE)) %>%
      filter(str_detect(string = name, pattern = "button", negate = TRUE)) %>%
      filter(str_detect(string = name, pattern = "rod", negate = TRUE)) %>%
      filter(str_detect(string = name, pattern = "level_object_for_screw_details", negate = TRUE)) %>%
      filter(str_detect(string = name, pattern = "pelvic_screw", negate = TRUE)) %>%
      filter(str_detect(string = name, pattern = "modal", negate = TRUE)) %>%
      filter((str_starts(string = name, pattern = "drop") & result == "0") == FALSE) %>%
      filter(name != "object_to_add") %>%
      filter(!is.na(result)) %>%
      filter(result != "") %>%
      filter(result != " ") %>%
      filter(name != "tabs") %>%
      filter(name != "label_text_offset") %>%
      filter(name != "search_for_prior_patient") %>%
      filter(str_detect(string = name, pattern = "[:upper:]", negate = TRUE)) %>%
      filter(str_detect(string = name, pattern = "\\s", negate = TRUE) ) %>%
      filter(str_detect(string = name, pattern = "\\W", negate = TRUE)) %>%
      filter(str_detect(string = name, pattern = "\\t", negate = TRUE))
  })
  
  output$all_inputs <- renderTable({
    all_inputs_trimmed_reactive_df()
    
  })
  
  output$all_inputs_removed <- renderTable({
    enframe(all_inputs_reactive_list()) %>%
      mutate(result = map(.x = value, .f = ~ as.character(glue_collapse(.x, sep = ";")))) %>%
      select(-value) %>%
      unnest(result) %>%
      anti_join(all_inputs_trimmed_reactive_df())
    
    # enframe(all_inputs_reactive_list()) %>%
    #   mutate(result = map(.x = value, .f = ~ as.character(glue_collapse(.x, sep = ";")))) %>%
    #   select(-value) %>%
    #   unnest(result) %>%
    #   filter(str_detect(string = name, pattern = "[:upper:]" | 
    #                       str_detect(string = name, pattern = "\\s") |
    #                     str_detect(string = name, pattern = "\\W") |
    #                     str_detect(string = name, pattern = "\\t") )) 
    
    #     data.frame(
    #   names = names(all_inputs_reactive_list()), 
    #   values = inputs_list,
    #   # values = unlist(all_inputs_reactive_list(),
    #                   use.names = TRUE
    # ) %>%
    #       as_tibble() %>%
    #       filter(!is.na(values)) 
    
  })
  
  # output$all_inputs_printed <- renderPrint({
  #   paste(all_inputs_reactive_list())
  # })
  
  ###### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Patient Details TABLE ('patient_details' instrument in redcap)  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ######## 
  
  patient_details_redcap_df_reactive <- reactive({
    patient_details_df <- tibble(last_name = input$patient_last_name,
                                 first_name = input$patient_first_name,
                                 date_of_birth = as.character(paste(input$date_of_birth)),
                                 # date_of_birth = if_else(paste(input$date_of_birth) == "1900-01-01", "--", paste(input$date_of_birth)),
                                 sex = input$sex)
    patient_details_df
  })
  
  
  
  ###### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PROCEDURE SUMMARY TABLE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ########
  
  surgical_details_redcap_df_reactive <- reactive({
    surgery_details_list <- list()
    
    ##########   date_of_surgery #############
    surgery_details_list$date_of_surgery <- as.character(input$date_of_surgery)
    
    ##########   Hospital #############
    surgery_details_list$hospital <- as.character(input$hospital)
    
    surgery_details_list$hospital_mrn <- as.character(input$hospital_mrn)
    
    ##########   age #############
    surgery_details_list$age <- if_else(paste(input$date_of_birth) == "1900-01-01", "--", as.character(round(interval(start = paste(input$date_of_birth), end = paste(input$date_of_surgery))/years(1), 0)))
    
    ##########   attending #############
    surgery_details_list$attending <- paste(input$primary_surgeon_first_name, input$primary_surgeon_last_name)
    
    ##########   assisting #############
    surgery_details_list$assisting <- input$surgical_assistants
    
    ##########   symptoms #############
    if(length(input$symptoms)>0){
      surgery_details_list$symptoms <- glue_collapse(str_to_lower(input$symptoms), sep = "; ") 
    }
    
    ##########   DIAGNOSIS #############
    
    if(length(input$primary_diagnosis) >0){
      
      surgery_details_list$diagnosis_category <- glue_collapse((tibble(diagnosis = input$primary_diagnosis) %>%
                                                                  left_join(spine_codes_df) %>%
                                                                  select(section) %>%
                                                                  mutate(str_to_title(section)))$section, sep = "; ")
      
      surgery_details_list$diagnosis <- glue_collapse(str_to_lower(input$primary_diagnosis), sep = "; ")
      
      
      surgery_details_list$diagnosis_icd10_code <- glue_collapse((tibble(diagnosis = input$primary_diagnosis) %>%
                                                                    left_join(spine_codes_df) %>%
                                                                    select(icd_10_code))$icd_10_code, sep = "; ")
      
    }
    
    
    
    ##########   indications #############
    if(input$indications != " "){
      surgery_details_list$indications <- input$indications
    }
    
    ##########   asa_class #############
    if(length(input$asa_class) >0){
      surgery_details_list$asa_class <- input$asa_class
    }
    
    ##########   anesthesia  #############
    if(length(input$anesthesia) >0){
      surgery_details_list$anesthesia <- input$anesthesia
    }
    
    
    ##########   prior_fusion_levels #############
    if(length(input$prior_fusion_levels)>0){
      surgery_details_list$prior_fusion_levels <- glue_collapse(input$prior_fusion_levels, sep = "; ")   
    }
    
    # ##########   levels_instrumentation_removed #############
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
    if(nrow(all_objects_to_add_list$objects_df)>0){
      surgery_details_list$main_approach <- case_when(
        any(all_objects_to_add_list$objects_df$approach == "anterior") & any(all_objects_to_add_list$objects_df$approach == "posterior")  ~ "combined anterior posterior",
        any(all_objects_to_add_list$objects_df$approach == "anterior") == FALSE & any(all_objects_to_add_list$objects_df$approach == "posterior")  ~ "posterior",
        any(all_objects_to_add_list$objects_df$approach == "anterior") & any(all_objects_to_add_list$objects_df$approach == "posterior") == FALSE  ~ "anterior"
      )
    }else{
      surgery_details_list$main_approach <- str_to_lower(input$spine_approach)
    }
    
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
    if(any(all_objects_to_add_list$objects_df$interbody_fusion == "yes")){
      surgery_details_list$interbody_fusion <- "yes"
    }else{
      surgery_details_list$interbody_fusion <- "no"
    }
    
    # surgery_details_list$interbody_fusion <- if_else("yes" %in% all_objects_to_add_list$objects_df$interbody_fusion, "yes", "no")
    
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
    
    if(any(all_objects_to_add_list$objects_df$object == "occipital_screw")){
      surgery_details_list$uiv <- "Occiput"
    }else{
      if(nrow(all_vertebrae_fixation_df) > 0){
        surgery_details_list$uiv <- (all_vertebrae_fixation_df %>% filter(vertebral_number == min(vertebral_number)) %>% select(level) %>% distinct())$level
        
        if(jh_check_body_or_interspace_function(surgery_details_list$uiv) == "interspace"){
          surgery_details_list$uiv <- jh_get_cranial_caudal_interspace_body_list_function(level = surgery_details_list$uiv)$cranial_level
        }
      }else{
        surgery_details_list$uiv <- "not instrumented"
      }    
    }
    
    ##########   LIV  #############
    if(nrow(all_vertebrae_fixation_df) > 0){
      surgery_details_list$liv <- (all_vertebrae_fixation_df %>% filter(vertebral_number == max(vertebral_number)) %>% select(level) %>% distinct())$level
      
      if(jh_check_body_or_interspace_function(surgery_details_list$liv) == "interspace"){
        surgery_details_list$liv <- jh_get_cranial_caudal_interspace_body_list_function(level = surgery_details_list$liv)$caudal_level
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
    
    ###### SPINE CERVICAL VS LUMBAR FOR PRO CAPTURE #####
    if(surgery_details_list$lower_treated_vertebrae %in% c("Occiput", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "T1", "T2", "T3", "T4", "T5", "T6")){
      surgery_details_list$spine_region <- "cervical"
    }else{
      surgery_details_list$spine_region <- "lumbar"
    }
    
    
    ##########   PELVIC FIXATION  #############
    surgery_details_list$pelvic_fixation <- if_else(any(str_detect(string = all_objects_to_add_list$objects_df$object, pattern = "pelvic_screw")), "yes", "no")
    
    if(surgery_details_list$pelvic_fixation == "yes"){
      surgery_details_list$pelvic_fixation_screws <- glue_collapse((all_objects_to_add_list$objects_df %>% filter(str_detect(object, "pelvic_screw")))$level, sep = "; ")
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
        anterior_ifactor <- if_else("iFactor" %in% input$posterior_biologics, glue("iFactor ({input$anterior_ifactor_volume})cc"), glue("xx"))
        
        anterior_biologics_vector <- discard(c(as.character(anterior_bma), as.character(anterior_cell_based), as.character(anterior_dbm), as.character(anterior_ifactor)), .p = ~ .x == "xx")
        
        if(length(anterior_biologics_vector) > 0){
          surgery_details_list$anterior_biologics <- glue_collapse(anterior_biologics_vector, sep = "; ") 
        }else{
          surgery_details_list$anterior_biologics <- "xx"
        }
      }
    }
    
    if(str_detect(surgery_details_list$main_approach, "posterior") & surgery_details_list$fusion == "yes"){
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
        posterior_ifactor <- if_else("iFactor" %in% input$posterior_biologics, glue("iFactor ({input$posterior_ifactor_volume})cc"), glue("xx"))
        
        posterior_biologics_vector <- discard(c(as.character(posterior_bma), as.character(posterior_cell_based), as.character(posterior_dbm),  as.character(posterior_ifactor)), .p = ~ .x == "xx")
        
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
    
    if(length(input$implant_manufacturer)>0){
      surgery_details_list$implant_manufacturer <- glue_collapse(input$implant_manufacturer, sep = ", ")
    }
    
    surgery_details_list$operative_note <- paste(input$operative_note_text)
    
    ####### FULL TABLE  #####
    
    surgery_details_df <- enframe(surgery_details_list) %>%
      mutate(across(everything(), ~ as.character(.x))) 
    
    surgery_details_df
  })
  
  
  
  
  ################## GENERATE INTRAOPERATIVE DETAILS TABLE #############
  intraoperative_details_redcap_df_reactive <- reactive({
    intraop_details_list <- list()
    
    ##########   date_of_surgery #############
    intraop_details_list$dos_intraop_repeating <- as.character(input$date_of_surgery)
    
    ################### Abx  #########################
    intraop_details_list$antibiotics <- glue_collapse(input$preop_antibiotics, sep = '; ')
    
    
    ################### NEUROMONINTORING  #########################
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
    
    if(length(input$additional_end_procedure_details)>0){
      intraop_details_list$end_procedure_details <- glue_collapse(input$additional_end_procedure_details, sep = "; ")
    }else{
      intraop_details_list$end_procedure_details <- " "
    }
    intraop_details_list$closure_details <- glue_collapse(input$closure_details, sep = "; ")
    intraop_details_list$dressing_details <- glue_collapse(input$dressing_details, sep = "; ")
    
    ####### GENERATE DATAFRAME #####
    
    intraop_details_df <- enframe(intraop_details_list) %>%
      mutate(across(everything(), ~ as.character(.x))) %>%
      filter(value != "xx")
    
    intraop_details_df
    
    
  })
  
  
  ################# MAKE THE procedures by level DATAFRAME ##################
  procedures_by_level_redcap_df_reactive <- reactive({
    
    fusion_df <- jh_fusion_category_function(fusion_vector = input$fusion_levels_confirmed, 
                                             all_objects_df = all_objects_to_add_list$objects_df)%>%
      mutate(side = "central")
    
    
    # data_wide <- all_objects_to_add_list$objects_df %>%
    #   select(level, vertebral_number, body_interspace, approach, category, implant, object, side, x, y, fusion, interbody_fusion, fixation_uiv_liv) %>%
    #   mutate(proc_category = map(.x = object, .f = ~ op_note_procedure_performed_summary_classifier_function(object = .x))) %>%
    #   unnest() %>%
    #   select(proc_category, everything()) %>%
    #   union_all(fusion_df) %>%
    #   arrange(vertebral_number) %>%
    #   select(level, approach, category, procedure = object, side) %>%
    #   group_by(level, side, category) %>%
    #   mutate(redcap_repeat_instance = row_number()) %>%
    #   pivot_wider(names_from = level, values_from = procedure) %>%
    #   mutate(across(everything(), ~ replace_na(.x, ""))) %>%
    #   ungroup() %>%
    #   mutate(redcap_repeat_instance = row_number()) %>%
    #   mutate(redcap_repeat_instrument = "procedures_by_level_repeating") %>%
    #   mutate(dos_surg_repeating = as.character(input$date_of_surgery)) %>%
    #   select(redcap_repeat_instrument, redcap_repeat_instance, dos_surg_repeating, approach_repeating = approach, everything()) %>%
    #   clean_names() %>%
    #   mutate(across(everything(), ~ replace_na(.x, " ")))
    
    data_wide <- all_objects_to_add_list$objects_df %>%
      select(level, vertebral_number, body_interspace, approach, category, implant, object, side, x, y, fusion, interbody_fusion, fixation_uiv_liv) %>%
      mutate(proc_category = map(.x = object, .f = ~ op_note_procedure_performed_summary_classifier_function(object = .x))) %>%
      unnest(proc_category) %>%
      union_all(fusion_df) %>%
      select(level, approach, category, object, side) %>%
      group_by(level, category, side) %>%
      mutate(repeat_count = row_number()) %>%
      ungroup() %>%
      pivot_wider(names_from = level, values_from = object) %>%
      select(-repeat_count) %>%
      clean_names() %>%
      mutate(across(everything(), ~ replace_na(.x, " "))) %>%
      mutate(redcap_repeat_instance = row_number()) %>%
      mutate(redcap_repeat_instrument = "procedures_by_level_repeating") %>%
      mutate(dos_surg_repeating = as.character(input$date_of_surgery)) %>%
      select(redcap_repeat_instrument, redcap_repeat_instance, dos_surg_repeating, approach_repeating = approach, everything()) 
  
    data_wide
  })
  
  
  ################# PEDICLE SCREW DETAILS TABLE ##################
  ### OLD METHOD
  ################------------------  Screw Size RESULTS  ----------------------######################  
  ################------------------  Screw Size RESULTS  ----------------------######################  
  
  ### first update the bilaterality input for the conditional panel
  screws_selected_df_reactive <- reactive({
    if(nrow(all_objects_to_add_list$objects_df %>% filter(str_detect(object, "screw") | str_detect(object, "anterior_plate")))>0){
      
      implants_added_df <- jh_generate_df_for_screening_screw_inputs_function(all_objects_to_add_list$objects_df) %>%
        left_join(all_screw_size_type_inputs_df %>% select(implant_row_id, level_object_label)) %>%
        arrange(implant_row_id) %>%
        select(implant_row_id, level, level_object_label, left_object, right_object)
      
    }else{
      implants_added_df <- tibble(level_object_label = character(), level = character(), left_object = character(), right_object =  character(), implant_row_id = integer())
    }
    implants_added_df
    
  })
  
  ### NOW UPDATE THE INPUT TO HAVE THE Correct levels FOR EACH IMPLANT  implants_complete
  observeEvent(list(input$implants_complete, input$return_to_add_implant_details_tab, input$implant_details, screws_selected_df_reactive()), ignoreInit = TRUE, {
   
     if(nrow(screws_selected_df_reactive())>0){
      updateCheckboxGroupInput(session = session, 
                               inputId = "level_object_for_screw_details",
                               choices = screws_selected_df_reactive()$level_object_label,
                               selected = screws_selected_df_reactive()$level_object_label) 
    }
  })
  
  observeEvent(list(input$implants_complete, input$return_to_add_implant_details_tab, input$implant_details, screws_selected_df_reactive()), ignoreInit = TRUE, {
    if(nrow(screws_selected_df_reactive())>0){
      updateCheckboxGroupInput(session = session, 
                               inputId = "left_level_object_for_screw_details",
                               choices = screws_selected_df_reactive()$left_object,
                               selected = screws_selected_df_reactive()$left_object) 
    }
  })
  
  ### NOW UPDATE THE INPUT TO HAVE THE Correct levels FOR EACH IMPLANT 
  observeEvent(list(input$implants_complete, input$return_to_add_implant_details_tab, input$implant_details, screws_selected_df_reactive()), ignoreInit = TRUE, {
    if(nrow(screws_selected_df_reactive())>0){
      updateCheckboxGroupInput(session = session, 
                               inputId = "right_level_object_for_screw_details",
                               choices = screws_selected_df_reactive()$right_object,
                               selected = screws_selected_df_reactive()$right_object) 
    }
  })
  
  ## NEW
  screw_details_redcap_df_reactive <- reactive({
    
    if(nrow(screws_selected_df_reactive())>0){
      screw_details_full_df <- screws_selected_df_reactive() %>%
        select(implant_row_id, level, left_object, right_object) %>%
        pivot_longer(cols = c(left_object, right_object), names_to = "side", values_to = "level_side_object") %>%
        mutate(side = str_remove_all(side, "_object")) %>%
        filter(level_side_object != "no_screw") %>%
        mutate(diameter_input_name = paste(level_side_object, "diameter", sep = "_")) %>%
        mutate(length_input_name = paste(level_side_object, "length", sep = "_")) %>%
        mutate(type_input_name = paste(level_side_object, "type", sep = "_")) %>%
        mutate(screw_diameter = map(.x = diameter_input_name, .f = ~ input[[.x]])) %>%
        unnest(screw_diameter) %>%
        mutate(screw_length = map(.x = length_input_name, .f = ~ input[[.x]])) %>%
        unnest(screw_length) %>%
        mutate(screw_type = map(.x = type_input_name, .f = ~ input[[.x]])) %>%
        unnest(screw_type) %>%
        mutate(screw_type = case_when(
          screw_type == "U" ~ "Uniaxial",
          screw_type == "M" ~ "Monoaxial",
          screw_type == "P" ~ "Polyaxial",
          screw_type == "Red" ~ "Reduction",
          screw_type == "Offset" ~ "Offset",
          screw_type == "Anterior" ~ "Anterior"
        )) %>%
        mutate(screw_diameter = as.character(screw_diameter)) %>%
        mutate(screw_length = as.character(screw_length)) %>%
        mutate(screw_diameter = if_else(is.na(screw_diameter), "na", screw_diameter)) %>%
        mutate(screw_length = if_else(is.na(screw_length), "na", screw_length)) %>%
        # replace_na(list(screw_diameter = "na", screw_length = "na")) %>%
        mutate(screw_size = case_when(
          screw_diameter == "na" & screw_length == "na" ~ "",
          screw_diameter != "na" & screw_length == "na" ~ paste0(screw_diameter, "mm"),
          screw_diameter == "na" & screw_length != "na" ~ paste0(screw_length, "mm"),
          screw_diameter != "na" & screw_length != "na" ~ paste0(screw_diameter, "x", screw_length, "mm"))
        )  %>%
        mutate(screw_size_type = paste(screw_size, screw_type)) %>%
        mutate(implant = str_remove_all(str_remove_all(level_side_object, "right_"), "left_")) %>%
        arrange(implant_row_id) %>%
        mutate(redcap_repeat_instrument = "screw_details_repeating") %>%
        mutate(redcap_repeat_instance = row_number()) %>%
        mutate(dos_screws_repeating = as.character(input$date_of_surgery)) %>%
        select(redcap_repeat_instrument, 
               redcap_repeat_instance, 
               dos_screws_repeating,
               screw_level = level,
               screw_implant = implant,
               screw_side = side,
               screw_diameter, 
               screw_length, 
               screw_type,
               screw_size,
               screw_size_type) 
      
      
      
    }else{
      screw_details_full_df <- tibble(redcap_repeat_instrument = character(), 
                                      redcap_repeat_instance = character(), 
                                      dos_screws_repeating = character(),
                                      screw_level = character(),
                                      screw_implant = character(),
                                      screw_side = character(),
                                      screw_diameter = character(),
                                      screw_length = character(),
                                      screw_type = character(), 
                                      screw_size = character(), 
                                      screw_size_type = character())
    }
    screw_details_full_df
    
  })
  
  
  ################# INTERBODY  DETAILS TABLE ##################
  
  interbody_details_redcap_df_reactive <- reactive({
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
  
  
  
  #################  ALL OBJECTS TABLE ##################
  output$all_objects_table <- renderTable({
    

      all_objects_to_add_list$objects_df %>%
          select(level, vertebral_number, body_interspace, approach, category, implant, object, side, x, y, fusion, interbody_fusion, fixation_uiv_liv) %>%
          mutate(proc_category = map(.x = object, .f = ~ op_note_procedure_performed_summary_classifier_function(object = .x))) %>%
          unnest() %>%
          select(proc_category, everything())


  })
  
  
  
  
  ############### ############### NOW RENDER EACH OF THE TABLES FOR THE SIDE TABULAR VIEW:    ###############     ############### 
  ############### ############### NOW RENDER EACH OF THE TABLES FOR THE SIDE TABULAR VIEW:    ###############     ############### 
  ############### ############### NOW RENDER EACH OF THE TABLES FOR THE SIDE TABULAR VIEW:    ###############     ############### 
  
  ########################
  
  ######## Render "Patient Details Table for side tab:"    ######## 
  # output$patient_details_redcap_df_sidetab <- renderTable({
  #     row_1 <- patient_details_redcap_df_reactive() %>%
  #         slice(1) %>%
  #         as.character()
  #     
  #     tibble(Variable = names(patient_details_redcap_df_reactive()), 
  #            Result = row_1) 
  #     
  # })
  
  
  ######## Render "Procedure Summary Table for side tab:"    ######## 
  # output$surgical_details_redcap_df_sidetab <- renderTable({
  #     surgical_details_redcap_df_reactive()
  # })
  
  
  ######## Render "Intraoperative Details Table for side tab:"    ######## 
  # output$intraoperative_details_redcap_df_sidetab <- renderTable({
  #     intraoperative_details_redcap_df_reactive() 
  # })
  # 
  
  ####### Render "Procedur Specifics" for side tab:"    ########
  output$procedures_by_level_redcap_df_sidetab <- renderTable({
    procedures_by_level_redcap_df_reactive() %>%
      pivot_longer(cols = c(-redcap_repeat_instrument, 
                            -redcap_repeat_instance, 
                            -dos_surg_repeating,
                            -approach_repeating,
                            -category,
                            -side))%>%
      filter(value != " ")
  })
  
  
  ######## Render "Screw Details Table for side tab:"    ######## 
  # output$screw_details_redcap_df_sidetab <- renderTable({
  #     
  #     screw_details_redcap_df_reactive()
  #     
  # })  
  # 
  
  ######## Render "Interbody Details Table for side tab:"    ######## 
  # output$interbody_details_redcap_df_sidetab <- renderTable({
  #     interbody_details_redcap_df_reactive()
  # })
  # 
  
  ### anterior objects for op note:
  anterior_objects_passed_to_op_note_df_reactive <- reactive({
    anterior_approach_objects_df <- all_objects_to_add_list$objects_df %>%
      filter(approach == "anterior") %>%
      select(-object_constructed)
    
    if(any(anterior_approach_objects_df$object == "anterior_plate")){
      anterior_approach_objects_df <- anterior_approach_objects_df %>% 
        filter(object == "anterior_plate") %>%
        select(level, vertebral_number, side, object)%>%
        separate(col = level, into = c("cranial_level", "caudal_level"), sep = "-") %>%
        mutate(side_left = "left", side_right = "right") %>%
        select(-side, vertebral_number) %>%
        pivot_longer(cols = c(cranial_level, caudal_level), names_to = "cranial_caudal", values_to = "level") %>%
        select(level, object, side_left, side_right) %>%
        pivot_longer(cols = c(side_left, side_right), names_to = "remove", values_to = "side") %>%
        distinct() %>%
        mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level)) %>%
        select(level, vertebral_number, side, object) %>%
        mutate(object = "anterior_plate_screw") %>%
        union_all(anterior_approach_objects_df)
      
      anterior_screws_df <- anterior_approach_objects_df %>%
        filter(str_detect(object, "screw")) %>%
        mutate(screw_implant = str_to_lower(paste(level, object, sep = "_"))) %>%
        left_join(screw_details_redcap_df_reactive() %>% rename(side = screw_side)) %>%
        select(level, approach, side, object, screw_size_type) %>%
        mutate(screw_size_type = as.character(screw_size_type)) %>%
        replace_na(list(screw_size_type = " "))
      
      anterior_approach_objects_df  <- anterior_approach_objects_df%>%
        left_join(anterior_screws_df) %>%
        mutate(screw_size_type = as.character(screw_size_type)) %>%
        replace_na(list(screw_size_type = " "))
      
    }
    
    if(nrow(interbody_details_df_reactive())>0){
      anterior_approach_objects_df <- anterior_approach_objects_df %>%
        left_join(interbody_details_df_reactive() %>% select(level, approach, object, implant_statement)) %>%
        replace_na(list(implant_statement = " ")) 
    }else{
      anterior_approach_objects_df <- anterior_approach_objects_df %>%
        mutate(implant_statement = " ")
    }
  })
  
  
  ######## Render "Anterior Objects Passed to Op Note:"    ######## 
  output$anterior_objects_passed_to_op_note_df <- renderTable({
    anterior_objects_passed_to_op_note_df_reactive()
  })
  
  ############### ############### NOW RENDER EACH OF THE TABLES FOR THE FINAL REVIEW IN THE MODAL:    ###############     ############### 
  ############### ############### NOW RENDER EACH OF THE TABLES FOR THE FINAL REVIEW IN THE MODAL:    ###############     ############### 
  ############### ############### NOW RENDER EACH OF THE TABLES FOR THE FINAL REVIEW IN THE MODAL:    ###############     ############### 
  
  output$patient_details_redcap_df_modal_tab <- renderTable({
    row_1 <- patient_details_redcap_df_reactive() %>%
      slice(1) %>%
      as.character()
    
    tibble(Variable = names(patient_details_redcap_df_reactive()), 
           Result = row_1) 
  })
  
  output$surgical_details_redcap_df_modal_tab <- renderTable({
    surgical_details_redcap_df_reactive()
  })
  
  output$intraoperative_details_redcap_df_modal_tab <- renderTable({
    intraoperative_details_redcap_df_reactive() 
  })
  
  
  output$procedures_by_level_redcap_df_modal_tab <- renderTable({
    procedures_by_level_redcap_df_reactive()
  })
  
  output$interbody_details_redcap_df_modal_tab <- renderTable({
    interbody_details_redcap_df_reactive()
  })
  
  output$screw_details_redcap_df_modal_tab <- renderTable({
    screw_details_redcap_df_reactive()
  })  
  
  
  #############~~~~~~~~~~~~~~~~~~~ ##################### REDCAP UPLOAD  #############~~~~~~~~~~~~~~~~~~~ ##################### 
  #############~~~~~~~~~~~~~~~~~~~ ##################### REDCAP UPLOAD  #############~~~~~~~~~~~~~~~~~~~ ##################### 
  #############~~~~~~~~~~~~~~~~~~~ ##################### REDCAP UPLOAD  #############~~~~~~~~~~~~~~~~~~~ ##################### 
  
  #############~~~~~~~~~~~~~~~~~~~ ##################### REDCAP UPLOAD  #############~~~~~~~~~~~~~~~~~~~ ##################### 
  #############~~~~~~~~~~~~~~~~~~~ ##################### REDCAP UPLOAD  #############~~~~~~~~~~~~~~~~~~~ ##################### 
  #############~~~~~~~~~~~~~~~~~~~ ##################### REDCAP UPLOAD  #############~~~~~~~~~~~~~~~~~~~ ##################### 
  
  #### CREATE MODAL BOX
  
  observeEvent(input$preview_redcap_upload, {
    showModal(
      modalDialog(footer = "Redcap Upload", easyClose = TRUE,  size = "l",  
                  box(width = 12, title = "Upload Data to Redcap", footer = NULL, 
                      fluidRow(
                        actionBttn(inputId = "confirm_upload_final",
                                   label = "Confirmed, Upload to Redcap",
                                   style = "simple", color = "primary")
                      ),
                      br(),
                      textOutput(outputId = "redcap_upload_status"),
                      fluidRow(
                        tabBox(width = 12,
                               tabPanel(title = "Patient Demographics",
                                        tableOutput(outputId = "patient_details_redcap_df_modal_tab")
                               ),
                               tabPanel(title = "Surgical Summary",
                                        tableOutput(outputId = "surgical_details_redcap_df_modal_tab")
                               ),
                               tabPanel(title = "Intraoperative Details", 
                                        tableOutput(outputId = "intraoperative_details_redcap_df_modal_tab")
                               ),
                               tabPanel(title = "Procedures by Level",
                                        tableOutput(outputId = "procedures_by_level_redcap_df_modal_tab")
                               ),
                               tabPanel(title = "Interbodies",
                                        tableOutput(outputId = "interbody_details_redcap_df_modal_tab")
                               ),
                               tabPanel(title = "Screw Details",
                                        tableOutput(outputId = "screw_details_redcap_df_modal_tab")
                               )
                        )
                      )
                  )
      )
      
    )
  })
  
  
  observeEvent(input$confirm_upload_final, once = TRUE, {
    
    if(redcapAPI::exportNextRecordName(rcon = rcon_reactive$rcon)>1){
      all_patient_ids_df <- exportRecords(rcon = rcon_reactive$rcon, fields = c("record_id", "last_name", "first_name", "date_of_birth"), events = "enrollment_arm_1") %>%
        type.convert() %>%
        select(record_id, last_name, first_name, date_of_birth) %>%
        mutate(last_name = str_to_lower(last_name),
               first_name = str_to_lower(first_name))   
    }else{
      all_patient_ids_df <- tibble()
    }
    
    if(nrow(all_patient_ids_df)>0){
      joined_df <- patient_details_redcap_df_reactive() %>%
        select(last_name, first_name, date_of_birth) %>%
        mutate(last_name = str_to_lower(last_name),
               first_name = str_to_lower(first_name)) %>%
        left_join(all_patient_ids_df)
      
      match_found <- if_else(!is.na(joined_df$record_id[[1]]), TRUE, FALSE)
      
      if(match_found == TRUE){
        record_number <- joined_df$record_id[[1]]
        
        max_repeat_instances_df <- exportRecords(rcon = rcon_reactive$rcon, records = record_number) %>%
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
        
        if("all_inputs_repeating" %in% max_repeat_instances_df$redcap_repeat_instrument){
          all_inputs_repeating_instance_add <- repeat_list$all_inputs_repeating
        }else{
          all_inputs_repeating_instance_add <- 0
        }
        
        surgical_details_instance_start <- repeat_list$surgical_details + 1
        max_procedures_by_level_repeating <- repeat_list$procedures_by_level_repeating
        max_screw_details_repeating <- repeat_list$screw_details_repeating
        
      }else{
        record_number <- exportNextRecordName(rcon = rcon_reactive$rcon)
        surgical_details_instance_add <- 0
        procedures_by_level_repeating_instance_add <- 0
        screw_details_repeating_instance_add <- 0
        interbody_implant_repeating_instance_add <- 0
        all_inputs_repeating_instance_add <- 0
      }
    }else{
      record_number <- exportNextRecordName(rcon = rcon_reactive$rcon)
      surgical_details_instance_add <- 0
      procedures_by_level_repeating_instance_add <- 0
      screw_details_repeating_instance_add <- 0
      interbody_implant_repeating_instance_add <- 0
      all_inputs_repeating_instance_add <- 0
    }
    
    ##### uploaded patient details #######
    
    withProgress(message = 'Uploading Data', value = 0, {
      number_of_steps <- 8
      
      incProgress(1/number_of_steps, detail = paste("Uploading Patient Details"))
      
      ##### uploaded patient details #######
      patient_df_for_upload <- patient_details_redcap_df_reactive() %>%
        mutate(record_id = record_number) %>%
        mutate(patient_details_complete = "Complete") %>%
        select(record_id, everything())
      
      importRecords(rcon = rcon_reactive$rcon, data = patient_df_for_upload, returnContent = "count")
      
      incProgress(1/number_of_steps, detail = paste("Uploading Surgical Details"))
      
      ##### uploaded surgical details #######
      surgical_details_instrument <- surgical_details_redcap_df_reactive() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(record_id = record_number) %>%
        mutate(redcap_event_name = "surgery_arm_1") %>%
        mutate(redcap_repeat_instance = row_number() + surgical_details_instance_add) %>%
        mutate(redcap_repeat_instrument = "surgical_details") %>%
        mutate(surgical_details_complete = "Complete") %>%
        select(record_id, redcap_event_name, everything())
      
      importRecords(rcon = rcon_reactive$rcon, data = surgical_details_instrument, returnContent = "count")
      
      incProgress(1/number_of_steps, detail = paste("Uploading Intraoperative Details"))
      
      ###### Upload Intraoperative Details ####
      intraoperative_details_redcap_upload_df <- intraoperative_details_redcap_df_reactive() %>%
        pivot_wider(names_from = name, values_from = value) %>%
        mutate(record_id = record_number) %>%
        mutate(redcap_event_name = "surgery_arm_1") %>%
        mutate(redcap_repeat_instance = row_number() + surgical_details_instance_add) %>%
        mutate(redcap_repeat_instrument = "intraoperative_details") %>%
        mutate(intraoperative_details_complete = "Complete") %>%
        select(record_id, redcap_event_name, everything())
      
      importRecords(rcon = rcon_reactive$rcon, data = intraoperative_details_redcap_upload_df, returnContent = "count")
      
      incProgress(1/number_of_steps, detail = paste("Uploading Data per Level"))
      
      ###### Upload repeating objects for all levels ####
      procedures_by_level_repeating_instrument <- procedures_by_level_redcap_df_reactive() %>%
        mutate(record_id = record_number) %>% 
        mutate(redcap_event_name = "surgery_arm_1") %>%
        arrange(category) %>%
        mutate(redcap_repeat_instance = row_number() + procedures_by_level_repeating_instance_add) %>%
        mutate(redcap_repeat_instrument = "procedures_by_level_repeating") %>%
        mutate(procedures_by_level_repeating_complete = "Complete") %>%
        select(record_id, redcap_event_name, everything())
      
      importRecords(rcon = rcon_reactive$rcon, data = procedures_by_level_repeating_instrument, returnContent = "count")
      
      incProgress(1/number_of_steps, detail = paste("Uploading Implant Data"))
      
      ##### uploaded screw details #######
      if(nrow(screw_details_redcap_df_reactive())>0){
        screw_details_repeating <- screw_details_redcap_df_reactive() %>%
          mutate(record_id = record_number) %>%
          mutate(redcap_event_name = "surgery_arm_1") %>%
          mutate(redcap_repeat_instance = row_number() +screw_details_repeating_instance_add) %>%
          mutate(redcap_repeat_instrument = "screw_details_repeating") %>%
          mutate(screw_details_repeating_complete = "Complete") %>%
          select(record_id, redcap_event_name, everything())
        
        importRecords(rcon = rcon_reactive$rcon, data = screw_details_repeating, returnContent = "count")
      }
      
      incProgress(1/number_of_steps, detail = paste("Uploading Interbody Implant Data"))
      
      ##### uploaded interbody details #######
      if(nrow(interbody_details_redcap_df_reactive())>0){
        interbody_implant_repeating <- interbody_details_redcap_df_reactive() %>%
          mutate(record_id = record_number) %>%
          select(record_id, everything()) %>%
          mutate(redcap_event_name = "surgery_arm_1") %>%
          mutate(redcap_repeat_instance = row_number() + interbody_implant_repeating_instance_add) %>%
          mutate(redcap_repeat_instrument = "interbody_implant_repeating") %>%
          mutate(interbody_implant_repeating_complete = "Complete") %>%
          mutate(across(everything(), ~ paste0(as.character(.x)))) %>%
          select(record_id, redcap_event_name, everything())
        
        importRecords(rcon = rcon_reactive$rcon, data = interbody_implant_repeating, returnContent = "count")
      }
      
      incProgress(1/number_of_steps, detail = paste("Uploading All Data Inputs"))
      
      ##### upload ALL INPUTS details #######
      if(nrow(all_inputs_trimmed_reactive_df())>0){
        
        # "record_id, redcap_event_name, redcap_repeat_instrument, redcap_repeat_instance, dos_all_inputs_repeating, variable_input_name, variable_input_result, all_inputs_complete"
        # enframe(all_inputs_reactive_list()) %>%
        #   mutate(result = map(.x = value, .f = ~ as.character(glue_collapse(.x, sep = ";")))) %>%
        #   select(-value) %>%
        #   unnest(result) %>%
        #   anti_join(all_inputs_trimmed_reactive_df())
        
        all_inputs_repeating_df <- all_inputs_trimmed_reactive_df() %>%
          mutate(record_id = record_number) %>%
          select(record_id, everything()) %>%
          rename(variable_input_name = name, variable_input_result = result) %>%
          mutate(redcap_event_name = "surgery_arm_1") %>%
          mutate(redcap_repeat_instance = row_number() + all_inputs_repeating_instance_add) %>%
          mutate(dos_all_inputs_repeating = as.character(input$date_of_surgery)) %>%
          mutate(redcap_repeat_instrument = "all_inputs_repeating") %>%
          mutate(all_inputs_repeating_complete = "Complete") %>%
          mutate(across(everything(), ~ paste0(as.character(.x)))) %>%
          select(record_id, redcap_event_name, everything())
        
        importRecords(rcon = rcon_reactive$rcon, data = all_inputs_repeating_df, returnContent = "count")
      }
      
      incProgress(1/number_of_steps, detail = paste("Complete"))
      
    })
    
    sendSweetAlert(
      session = session,
      title = "Success !!",
      text = "All in order",
      type = "success"
    )
  })
  
  
  
  
  ############################################    ##    ############################################    ############################################    ############################################    ############################################
  
  task_items_reactive_list <- reactiveValues()
  
  task_items_reactive_list$upload_to_redcap <- 0
  
  observeEvent(input$confirm_upload_final, {
    task_items_reactive_list$upload_to_redcap <- 100
  })
  
  
  output$upload_to_redcap_task <- renderMenu({
    dropdownMenu(type = "tasks", 
                 badgeStatus = if_else(task_items_reactive_list$upload_to_redcap == 100, "success", "warning"), 
                 taskItem(text = "Upload Data to Redcap", 
                          value = task_items_reactive_list$upload_to_redcap))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)