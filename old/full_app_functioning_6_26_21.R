####################### SURGICAL PLANNING V2 ###########################

library(shiny)
library(sf)
library(tidyverse)
library(ggplot2)
library(shinyWidgets)
library(shinyBS)
# library(colourpicker)
# library(kableExtra)
library(cowplot)
# library(ggpubr)
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

library(DT)

rcon <- redcapConnection(url = 'https://redcap.wustl.edu/redcap/api/', token = "4B2AA28A4D6EFBC6CA1F49EC0FB385F7")


source("hook_function.R", local = TRUE)
source("screw_function.R", local = TRUE)
source("osteotomies_decompressions_functions.R", local = TRUE)
source("load_coordinates.R", local = TRUE)
source("operative_note_posterior_generator.R", local = TRUE)
source("operative_note_updated_anterior_generator.R", local = TRUE)
# source("implant_sizes_functions.R", local = TRUE)
# remotes::install_github("coolbutuseless/ggpattern")
# spine_png <- image_read(path = "posterior_spine_figure.png")



# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  
  rclipboardSetup(),
  
  tags$style(type = "text/css",
             "#option2 .radio label {font-size: 5px}"
  ),
  tags$style(type = "text/css", 
             "#my_small_text_input .form-control {text-align:center; font-size:12px; height:auto; padding:2px 1px}"
  ),
  # tags$style(type = "text/css", 
  #          "#my_very_small_text_input .awesome-checkbox {width: 5; height: 5px; line-height:auto; padding:10px 10px: font-size:8px} .body {text-align:center; font-size:6px; height:auto; padding:2px 1px}"
  #          ),
  tags$style(type = "text/css", 
             "#my_small_button_input .btn {text-align:center; font-size:10px; line-height:1; height:auto; padding:2px 1px}"
  ),
  tags$style(type = "text/css", "vertical-align:top"),
  titlePanel("Spine Templating"),
  sidebarLayout(
    sidebarPanel(
      tags$style(
        "#sidebarItemExpanded {
            overflow: auto;
            max-height: 150vh;
        }"
      ),
      width = 3,
      fluidRow(
        h3(strong("Patient Details:")),
        fluidRow(column(6, 
                        textInput(inputId = "patient_name", label = "Name:")),
                 column(6,
                        dateInput(inputId = "date_of_birth", label = "Date of Birth:", format = "mm-dd-yyyy", startview = "decade", autoclose = TRUE, max = Sys.Date(), value = "1900-01-01"))
        ),
        awesomeRadio(
          inputId = "sex",
          label = "Sex:", 
          choices = c("Male", "Female"),
          selected = "Female", 
          inline = TRUE, 
        ),
        column(12, 
               fluidRow(column(width = 5, 
                               h5(strong("Diagnosis Category:")) 
               ),
               column(width = 7,
                      pickerInput(
                        inputId = "primary_diagnosis_group",
                        label = NULL,
                        choices = c("Spinal Condition (E.g. Degenerative)", "Deformity",  "Neoplasm", "Lesion (trauma, infection, etc)"),
                        multiple = FALSE,
                        width = "100%"
                      )
               )
               ),
               fluidRow(
                 column(width = 6, 
                        h5(strong("Diagnosis Subgroup:")) 
                 ),
                 column(width = 6,
                        checkboxGroupButtons(
                          inputId = "spine_dx_subgroup",
                          label = NULL,
                          size = "xs",
                          justified = TRUE,
                          width = "100%", 
                          individual = TRUE,
                          direction = "vertical",
                          choices = c("Spinal Stenosis", 
                                      "Myelopathy",
                                      "Foraminal Stenosis",
                                      "Degenerative Spondylolisthesis", 
                                      "Pseudarthrosis", 
                                      "Lumbar Disc Herniation",
                                      "Other"),
                          checkIcon = list(
                            yes = tags$i(class = "fa fa-check-square",
                                         style = "color: steelblue"),
                            no = tags$i(class = "fa fa-square-o",
                                        style = "color: steelblue"))
                        ),
                        conditionalPanel(condition = "input.spine_dx_subgroup.indexOf('Other') > -1",
                                         textInput(inputId = "spine_dx_subgroup_other", label = "Other:"))
                 )
               ),
               tags$table(
                 tags$tr(width = "100%",
                         tags$td(width = "50%", tags$div(style = "font-size:14px; font-weight:bold; text-align:left","Symptoms:")),
                         tags$td(width = "50%",align = "right",
                                 pickerInput(
                                   inputId = "symptoms",
                                   label = NULL,
                                   choices = list('Axial Pain' = c("Neck Pain", "Thoracic Pain", "Low Back Pain"), 
                                                  'Arm Pain' = c("Left Arm Pain", "Right Arm Pain"),
                                                  'Leg Pain' = c("Left Leg Pain", "Right Leg Pain"),
                                                  'Myelopathy' = c("Nurick Grade 1",
                                                                   "Nurick Grade 2",
                                                                   "Nurick Grade 3",
                                                                   "Nurick Grade 4",
                                                                   "Nurick Grade 5"),
                                                  'Functional' = c("Sagittal Imbalance"),
                                                  'Other' = c("Other Symptoms")),
                                   multiple = TRUE,
                                   width = "100%"
                                 ))),
                 tags$tr(width = "100%",
                         tags$td(width = "50%", div(style = "font-size:14px; font-weight:bold; text-align:left", "Bone Density:")),
                         tags$td(width = "50%", 
                                 textInput(inputId = "bone_density", label = NULL, width = "100%", placeholder = "T-score or HU"))),
               )
        ),
        textInput(inputId = "relevant_history", label = "Other Comments/History:"),
        h3(strong("Surgical Details:")),
        fluidRow(column(width = 6,
                        h5(strong("Date of Surgery:"))
        ),
        column(width = 6,
               dateInput(inputId = "date_of_surgery", label = NULL, format = "mm-dd-yyyy", autoclose = TRUE))
        ),
        fluidRow(column(width = 6,
                        h5(strong("Staged Procedure?"))
        ),
        column(width = 6,
               switchInput(
                 inputId = "staged_procedure",
                 size = "small",
                 width = "100%",
                 onLabel = "Yes",
                 offLabel = "No"
               ))
        ),
        conditionalPanel(condition = "input.staged_procedure == true",
                         fluidRow(column(width = 6,
                                         h5(strong("Stage Number:"))
                         ),
                         column(width = 6,
                                numericInput(inputId = "stage_number",
                                             width = "100%",
                                             step = 1,
                                             value = 1,
                                             min = 1, 
                                             max = 5,
                                             label = NULL)
                         )
                         )
        ),
        fluidRow(column(width = 6,
                        h5(strong("Primary or Revision:"))
        ),
        column(width = 6,
               radioGroupButtons(
                 inputId = "primary_revision",
                 label = NULL,
                 choices = c("Primary", "Revision"),
                 justified = TRUE,
                 selected = "Primary",
                 size = "sm"
               )
        )
        ),
        conditionalPanel("input.primary_revision.indexOf('Revision') > -1",
                         tags$table(
                           tags$tr(width = "100%",
                                   tags$td(width = "50%", tags$div(style = "font-size:12px; font-weight:bold; text-align:left","Select Levels with Open Canal:")),
                                   tags$td(width = "50%",align = "right",
                                           pickerInput(
                                             inputId = "open_canal",
                                             label = NULL, 
                                             choices = open_canal_df$level,
                                             multiple = TRUE
                                           )
                                   )),
                           tags$tr(width = "100%",
                                   tags$td(width = "50%", tags$div(style = "font-size:12px; font-weight:bold; text-align:left","Select Prior Fusion Levels:")),
                                   tags$td(width = "50%",align = "right",
                                           pickerInput(
                                             inputId = "prior_fusion_levels",
                                             label = NULL, 
                                             choices = unique(interbody_levels_df$level),
                                             multiple = TRUE
                                           )
                                   )),
                           tags$tr(width = "100%",
                                   tags$td(width = "50%", tags$div(style = "font-size:12px; font-weight:bold; text-align:left","Select Levels to remove instrumentation:")),
                                   tags$td(width = "50%",align = "right",
                                           pickerInput(
                                             inputId = "removal_instrumentation_levels",
                                             label = NULL, 
                                             choices = unique(labels_df$level),
                                             multiple = TRUE
                                           )
                                   )),
                           tags$tr(width = "100%",
                                   tags$td(width = "50%", tags$div(style = "font-size:12px; font-weight:bold; text-align:center","Left Implants:")),
                                   tags$td(width = "50%", tags$div(style = "font-size:12px; font-weight:bold; text-align:center","Right Implants:"))),
                           tags$tr(width = "100%",
                                   tags$td(width = "50%",align = "center",
                                           pickerInput(
                                             inputId = "left_revision_implants",
                                             label = NULL,
                                             choices = unique((revision_implants_df %>% filter(x < 0.5))$level),
                                             multiple = TRUE,
                                             width = "100%"
                                           )),
                                   tags$td(width = "50%",align = "center",
                                           pickerInput(
                                             inputId = "right_revision_implants",
                                             label = NULL,
                                             choices = unique((revision_implants_df %>% filter(x > 0.5))$level),
                                             multiple = TRUE,
                                             width = "100%"
                                           )))
                         )
        ),
        tags$hr(),
        column(12, 
               tags$table(
                 tags$tr(width = "100%",
                         tags$td(width = "60%", div(style = "font-size:14px; font-weight:bold; text-align:left", "Preop Antibiotics:")),
                         tags$td(width = "40%", 
                                 pickerInput(
                                   inputId = "preop_antibiotics",
                                   label = NULL, 
                                   choices = c("None (Antibiotics were held)", "Ancef", "Vancomycin", "Gentamycin", "Clindamycin", "Unknown", "Other"),
                                   multiple = TRUE, width = "100%"
                                 ))),
                 tags$tr(width = "100%",
                         tags$td(width = "60%", tags$div(style = "font-size:14px; font-weight:bold; text-align:left",  "Antifibrinolytic:")),
                         tags$td(width = "40%", 
                                 pickerInput(
                                   inputId = "anti_fibrinolytic", 
                                   label = NULL, 
                                   choices = c("None", "Tranexamic Acid (TXA)", "Amicar", "Desmopressin (DDAVP)", "Other"),
                                   multiple = TRUE, width = "100%"
                                 )))
               ),
               conditionalPanel(condition = "input.anti_fibrinolytic.indexOf('Tranexamic Acid (TXA)') > -1",
                                tags$table(
                                  tags$tr(width = "100%",
                                          tags$td(width = "70%", div(style = "font-size:12px; font-weight:bold; text-align:left",  "TXA Loading (mg/kg):")),
                                          tags$td(width = "30%", 
                                                  numericInput(inputId = "txa_loading", label = NULL, value = 30, min = 0, max = 200, step = 5, width = "100%"))),
                                  tags$tr(width = "100%",
                                          tags$td(width = "70%", tags$div(style = "font-size:12px; font-weight:bold; text-align:left",  "TXA Maintenance (mg/kg/hr):")),
                                          tags$td(width = "30%", align = "right",
                                                  numericInput(inputId = "txa_maintenance", label = NULL, value = 5, min = 0, max = 50, step = 5, width = "100%")))
                                )
               )
        )
      )
    ),
    mainPanel(width = 9,
              tabsetPanel(type = "tabs",
                          tabPanel(title = "Spine Plan",
                                   column(
                                     width = 4,
                                     br(),
                                     br(),
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
                                     ),
                                     hr(),
                                     br(),
                                     switchInput(
                                       inputId = "fusion_procedure_performed",width = '100%', inline = TRUE,
                                       label = "Fusion:",
                                       onLabel = "Yes",
                                       offLabel = "No",
                                       value = FALSE,
                                       size = "mini"
                                     ),
                                     uiOutput(outputId = "interbody_implants_ui"),
                                     # conditionalPanel(condition = "input.spine_approach.indexOf('Anterior') > -1",
                                     #                  fixedRow(column(width = 12, 
                                     #                                  h5(strong("Interbody Implant Details:")),
                                     #                                  uiOutput(outputId = "interbody_implants_ui")
                                     #                                  )
                                     #                           )
                                     #                  ),
                                     conditionalPanel(condition = "input.fusion_procedure_performed == true & input.spine_approach.indexOf('Posterior') > -1",
                                                      fixedRow(
                                                        h4(strong("Primary Rod Details:")),
                                                        column(width = 6,
                                                               strong("Left Rod:"),
                                                               pickerInput(inputId = "left_main_rod_size", 
                                                                           label = "Size:", 
                                                                           choices = c("None", "Transition", "3.5mm", "4.0mm", "4.5mm", "4.75mm", "5.5mm", "6.0mm", "6.35mm/quarter in"),
                                                                           selected = "None"),
                                                               radioGroupButtons(
                                                                 inputId = "left_main_rod_material", size = "xs", justified = TRUE,direction = "vertical",
                                                                 label = "Material:",
                                                                 choices = c("Non-instrumented", "Titanium", "Cobalt Chrome", "Stainless Steel"), 
                                                                 selected = "Non-instrumented"
                                                               )
                                                        ), 
                                                        column(width = 6,
                                                               strong("Right Rod:"),
                                                               pickerInput(inputId = "right_main_rod_size", 
                                                                           label = "Size:", 
                                                                           choices = c("None", "Transition", "3.5mm", "4.0mm", "4.5mm", "4.75mm", "5.5mm", "6.0mm", "6.35mm/quarter in"),
                                                                           selected = "None"),
                                                               radioGroupButtons(
                                                                 inputId = "right_main_rod_material",
                                                                 size = "xs", justified = TRUE,direction = "vertical",
                                                                 label = "Material:",
                                                                 choices = c("Non-instrumented", "Titanium", "Cobalt Chrome", "Stainless Steel"), 
                                                                 selected = "Non-instrumented"
                                                               )
                                                        )
                                                      )
                                     ),
                                     hr(),
                                     conditionalPanel(condition = "input.fusion_procedure_performed == true & input.spine_approach.indexOf('Posterior') > -1",
                                                      dropdown(icon = icon("grip-lines-vertical"), 
                                                               width = "250%",
                                                               size = "xs",
                                                               label = "Add Additional Rods",
                                                               style = "unite", 
                                                               fixedRow(
                                                                 column(width = 6, 
                                                                        awesomeCheckbox(
                                                                          inputId = "add_left_accessory_rod",
                                                                          label = "Left Accessory Rod:",
                                                                          value = FALSE,
                                                                          status = "success"
                                                                        )
                                                                 ),
                                                                 column(width = 6,
                                                                        awesomeCheckbox(
                                                                          inputId = "add_right_accessory_rod",
                                                                          label = "Right Accessory Rod:",
                                                                          value = FALSE,
                                                                          status = "success"
                                                                        )
                                                                 )
                                                               ),
                                                               fixedRow(
                                                                 column(width = 6,
                                                                        conditionalPanel(condition = "input.add_left_accessory_rod == true",
                                                                                         sliderTextInput(
                                                                                           inputId = "left_accessory_rod",
                                                                                           label = NULL,
                                                                                           choices = c("-", "--", "---"),
                                                                                           selected = c("-", "---"))
                                                                        )
                                                                 ),
                                                                 column(width = 6,
                                                                        conditionalPanel(condition = "input.add_right_accessory_rod == true",
                                                                                         sliderTextInput(
                                                                                           inputId = "right_accessory_rod",
                                                                                           label = NULL,
                                                                                           choices = c("-", "--", "---"),
                                                                                           selected = c("-", "---"))
                                                                        )
                                                                 )
                                                               ),
                                                               tags$hr(),
                                                               fixedRow(
                                                                 column(width = 6, 
                                                                        awesomeCheckbox(
                                                                          inputId = "add_left_satellite_rod",
                                                                          label = "Left Satellite Rod:",
                                                                          value = FALSE,
                                                                          status = "success"
                                                                        )
                                                                 ),
                                                                 column(width = 6,
                                                                        awesomeCheckbox(
                                                                          inputId = "add_right_satellite_rod",
                                                                          label = "Right Satellite Rod:",
                                                                          value = FALSE,
                                                                          status = "success"
                                                                        )
                                                                 )
                                                               ),
                                                               fixedRow(
                                                                 column(width = 6,
                                                                        conditionalPanel(condition = "input.add_left_satellite_rod == true",
                                                                                         sliderTextInput(
                                                                                           inputId = "left_satellite_rod",
                                                                                           label = NULL,
                                                                                           choices = c("-", "--", "---"),
                                                                                           selected = c("-", "---"))
                                                                        )
                                                                 ),
                                                                 column(width = 6,
                                                                        conditionalPanel(condition = "input.add_right_satellite_rod == true",
                                                                                         sliderTextInput(
                                                                                           inputId = "right_satellite_rod",
                                                                                           label = NULL,
                                                                                           choices = c("-", "--", "---"),
                                                                                           selected = c("-", "---"))
                                                                        )
                                                                 )
                                                               ),
                                                               tags$hr(),
                                                               fixedRow(
                                                                 column(width = 6, 
                                                                        awesomeCheckbox(
                                                                          inputId = "add_left_intercalary_rod",
                                                                          label = "Left Intercalary Rod:",
                                                                          value = FALSE,
                                                                          status = "success"
                                                                        )
                                                                 ),
                                                                 column(width = 6,
                                                                        awesomeCheckbox(
                                                                          inputId = "add_right_intercalary_rod",
                                                                          label = "Right Intercalary Rod:",
                                                                          value = FALSE,
                                                                          status = "success"
                                                                        )
                                                                 )
                                                               ),
                                                               fixedRow(
                                                                 column(width = 6,
                                                                        conditionalPanel(condition = "input.add_left_intercalary_rod == true",
                                                                                         sliderTextInput(
                                                                                           inputId = "left_intercalary_rod",
                                                                                           label = NULL,
                                                                                           choices = c("-", "--", "---"),
                                                                                           selected = c("-", "---"))
                                                                        )
                                                                 ),
                                                                 column(width = 6,
                                                                        conditionalPanel(condition = "input.add_right_intercalary_rod == true",
                                                                                         sliderTextInput(
                                                                                           inputId = "right_intercalary_rod",
                                                                                           label = NULL,
                                                                                           choices = c("-", "--", "---"),
                                                                                           selected = c("-", "---"))
                                                                        )
                                                                 )
                                                               ),
                                                               tags$hr(),
                                                               fixedRow(
                                                                 column(width = 6, 
                                                                        awesomeCheckbox(
                                                                          inputId = "add_left_linked_rods",
                                                                          label = "Left Linked Rods Overlap:",
                                                                          value = FALSE,
                                                                          status = "success"
                                                                        )
                                                                 ),
                                                                 column(width = 6,
                                                                        awesomeCheckbox(
                                                                          inputId = "add_right_linked_rods",
                                                                          label = "Right Linked Rods Overlap:",
                                                                          value = FALSE,
                                                                          status = "success"
                                                                        )
                                                                 )
                                                               ),
                                                               fixedRow(
                                                                 column(width = 6,
                                                                        conditionalPanel(condition = "input.add_left_linked_rods == true",
                                                                                         sliderTextInput(
                                                                                           inputId = "left_linked_rods_overlap",
                                                                                           label = NULL,
                                                                                           choices = c("-", "--", "---"),
                                                                                           selected = c("-", "---"))
                                                                        )
                                                                 ),
                                                                 column(width = 6,
                                                                        conditionalPanel(condition = "input.add_right_linked_rods == true",
                                                                                         sliderTextInput(
                                                                                           inputId = "right_linked_rods_overlap",
                                                                                           label = NULL,
                                                                                           choices = c("-", "--", "---"),
                                                                                           selected = c("-", "---"))
                                                                        )
                                                                 )
                                                               )
                                                      )
                                     ), 
                                     hr(),
                                     conditionalPanel(condition = "input.fusion_procedure_performed == true & input.spine_approach.indexOf('Posterior') > -1",
                                                      dropdown(icon = icon("screwdriver"),
                                                               size = "xs", 
                                                               width = "400%",
                                                               label = "Add Pedicle Screw Details",
                                                               style = "unite", 
                                                               uiOutput(outputId = "pedicle_screw_details_ui"),
                                                               uiOutput(outputId = "pedicle_screw_types_ui")
                                                      )
                                     ),
                                     br(),
                                     hr(),
                                     conditionalPanel(condition = "input.fusion_procedure_performed == true",
                                                      dropdown(icon = icon("link"),
                                                               size = "xs", 
                                                               width = "125%",
                                                               label = "Confirm Fusion Levels",
                                                               style = "unite", 
                                                               checkboxGroupButtons(inputId = "fusion_levels_confirmed", 
                                                                                    label = "Select/Confirm Fusion Levels:", 
                                                                                    choices = interbody_levels_df$level, 
                                                                                    size = "xs", 
                                                                                    direction = "vertical", 
                                                                                    checkIcon = list(
                                                                                      yes = tags$i(class = "fa fa-check-square", 
                                                                                                   style = "color: steelblue"),
                                                                                      no = tags$i(class = "fa fa-square-o", 
                                                                                                  style = "color: steelblue"))
                                                               )
                                                      )
                                     ),
                                     hr(),
                                     br(),
                                     fixedRow(column(width = 12,
                                                     conditionalPanel(condition = "input.fusion_procedure_performed == true",
                                                                      awesomeCheckboxGroup(inputId = "bone_graft", width = "100%",
                                                                                           label = "Select any bone graft used:",
                                                                                           choices = c("Morselized Allograft",
                                                                                                       "Local Autograft",
                                                                                                       "Morselized Autograft (separate fascial incision)",
                                                                                                       "Structural Allograft",
                                                                                                       "Structural Autograft"), 
                                                                                           selected = "Morselized Allograft")
                                                     )
                                     )
                                     ),
                                     conditionalPanel(condition = "input.fusion_procedure_performed == true",
                                                      tags$table(
                                                        tags$tr(width = "100%",
                                                                tags$td(width = "60%", div(style = "font-size:14px; font-weight:bold; text-align:left", "BMP Kits:")),
                                                                tags$td(width = "40%", 
                                                                        numericInput(inputId = "bmp_number", label = NULL, value = 0, min = 0, max = 20, step = 1, width = "100%"))),
                                                      )
                                     ),
                                     conditionalPanel(condition = "input.bmp_number > 0",
                                                      radioGroupButtons(
                                                        inputId = "bmp_size",
                                                        label = NULL,
                                                        choices = c("XXS" = 1.05, "XS" = 2.1, "Sm" = 4.2, "M" = 8.4, "L" = 12),
                                                        size = "sm",
                                                        selected = 12,
                                                        checkIcon = list(
                                                          yes = tags$i(class = "fa fa-check-square", 
                                                                       style = "color: steelblue"),
                                                          no = tags$i(class = "fa fa-square-o", 
                                                                      style = "color: steelblue"))
                                                      )
                                     ),
                                     conditionalPanel(
                                       condition = "input.bone_graft.indexOf('Morselized Allograft') > -1 & input.fusion_procedure_performed == true",
                                       numericInput(inputId = "allograft_amount", 
                                                    label = "Allograft (cc):", 
                                                    value = 0, 
                                                    min = 0,
                                                    max = 500, 
                                                    step = 30, 
                                                    width = "100%")
                                     ), 
                                     conditionalPanel(
                                       condition = "input.bone_graft.indexOf('Structural Allograft') > -1",
                                       textInput(inputId = "structural_allograft_location",
                                                 label = "Structural allograft was inserted:", 
                                                 value = NULL, 
                                                 width = "100%")
                                     ),
                                     conditionalPanel(
                                       condition = "input.bone_graft.indexOf('Structural Autograft') > -1",
                                       textInput(inputId = "structural_autograft_harvest_location",
                                                 label = "Structural autograft was harvested from:", 
                                                 value = NULL, 
                                                 width = "100%")
                                     ),
                                     conditionalPanel(
                                       condition = "input.bone_graft.indexOf('Structural Autograft') > -1",
                                       textInput(inputId = "structural_autograft_location",
                                                 label = "Structural autograft was inserted:", 
                                                 value = NULL, 
                                                 width = "100%")
                                     )
                                   ),
                                   column(
                                     width = 8,
                                     align = "center",
                                     column(9, 
                                            fluidRow(column(9, 
                                                            h4(strong("Select Approach:"))),
                                                     column(3,
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
                                                                          value = TRUE
                                                              ),
                                                              circle = TRUE,
                                                              icon = icon("gear"),
                                                              size = "sm",
                                                              inline = TRUE,
                                                              right = TRUE
                                                            ))),
                                            fluidRow(column(12, 
                                                            radioGroupButtons(
                                                              inputId = "spine_approach",
                                                              label = NULL,
                                                              choices = c("Anterior", 
                                                                          "Lateral", 
                                                                          "Posterior"),
                                                              individual = TRUE,
                                                              selected = "Posterior", 
                                                              justified = TRUE, 
                                                              direction = "horizontal"
                                                            )
                                            )
                                            ),
                                            fluidRow(column(12,
                                                            radioGroupButtons(inputId = "approach_specified",
                                                                              label = NULL, 
                                                                              choices = c("Midline",
                                                                                          "Paraspinal or Paramedian", 
                                                                                          "Stab"),   
                                                                              selected = "Midline",
                                                                              size = 'xs', 
                                                                              justified = TRUE,
                                                                              width = "100%")
                                            )
                                            ),
                                            conditionalPanel(condition = "input.spine_approach.indexOf('Posterior') > -1",
                                                             fluidRow(column(6, 
                                                                             radioGroupButtons(inputId = "approach_open_mis",
                                                                                               label = NULL, 
                                                                                               choices = c("Open",
                                                                                                           "Minimally Invasive"),
                                                                                               selected = "Open",
                                                                                               size = 'xs', 
                                                                                               justified = TRUE,
                                                                                               width = "100%")
                                                             ),
                                                             column(6, 
                                                                    checkboxGroupButtons(inputId = "approach_robot_navigation",
                                                                                         label = NULL,
                                                                                         choices = c("Robotic Assistance",
                                                                                                     "Navigation Assistance"),
                                                                                         selected = "Open",
                                                                                         size = 'xs',
                                                                                         justified = TRUE,
                                                                                         width = "100%"))
                                                             )
                                            ),
                                            h4("Add procedures & implants in the order they were performed."),
                                            fluidRow(
                                              column(width = 2, 
                                                     br(),
                                                     br(),
                                                     br(),
                                                     noUiSliderInput(inputId = "crop_y",
                                                                     label = "Crop",
                                                                     min = 0,
                                                                     max = 1,
                                                                     value = c(0.05,0.45), direction = "rtl",
                                                                     behaviour = "drag",color = "#0036FD",
                                                                     orientation = "vertical",
                                                                     height = "600px", width = "5px",
                                                                     inline = TRUE)
                                              ),
                                              column(width = 10,
                                                     plotOutput("spine_plan",
                                                                height = 750,
                                                                click = "plot_click", 
                                                                dblclick = "plot_double_click")
                                              )
                                            ),
                                            textOutput(outputId = "currently_adding")
                                            
                                     ),
                                     column(width = 3, 
                                            br(),
                                            br(),
                                            conditionalPanel(condition = "input.spine_approach.indexOf('Posterior') > -1",
                                                             actionBttn(
                                                               inputId = "add_implants",
                                                               size = "xs", block = TRUE,
                                                               label = "Add Surgical Implants (Screws, Hooks, Tethers, etc.)", 
                                                               style = "simple",
                                                               color = "primary"
                                                             ),
                                                             br(),
                                                             actionBttn(
                                                               inputId = "add_decompressions",
                                                               size = "xs", block = TRUE,
                                                               label = "Add Decompressions", 
                                                               style = "simple",
                                                               color = "primary"
                                                             ),
                                                             br(),
                                                             actionBttn(
                                                               inputId = "add_osteotomies",
                                                               size = "xs", block = TRUE,
                                                               label = "Add Osteotomies/Facetectomies", 
                                                               style = "simple",
                                                               color = "primary"
                                                             ),
                                                             br(),
                                                             actionBttn(
                                                               inputId = "add_interbody",
                                                               size = "xs", block = TRUE,
                                                               label = "Add Interbody Fusion", 
                                                               style = "simple",
                                                               color = "primary"
                                                             )),
                                            br(),
                                            radioGroupButtons(
                                              inputId = "object_to_add",
                                              direction = "vertical", 
                                              justified = FALSE,
                                              individual = FALSE, 
                                              width = "120%",
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
                                                "Tether" = "tether",
                                                "Sublaminar Wire" = "sublaminar_wire" , 
                                                "Cement Augmentation" = "cement_augmentation"
                                              ),
                                              checkIcon = list(
                                                yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                                              ),
                                              selected = "pedicle_screw"
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
                                     )
                                   )
                          ),
                          tabPanel(title = "Procedure Tables",
                                   column(12, 
                                          h3("Procedure Summary Table:"),
                                          tableOutput(outputId = "summary_table"),
                                          h3("Procedure Specifics:"),
                                          tableOutput(outputId = "upload_df"),
                                          h3("All objects table:"),
                                          tableOutput(outputId = "all_objects_table"),
                                          h3("Pedicle Screw Details"),
                                          tableOutput("pedicle_screw_details_table"),
                                          hr(),
                                          h4("Interbody implants"),
                                          tableOutput(outputId = "interbody_details_table")
                                   )
                          ),
                          tabPanel(title = "Operative Note",
                                   column(5,
                                          h3("Additional Surgical Details:"),
                                          tags$table(
                                            tags$tr(width = "90%",
                                                    tags$td(width = "30%", tags$div(style = "font-size:12px; font-weight:bold; text-align:left","Primary Surgeon:")),
                                                    tags$td(width = "60%", textInput(inputId = "primary_surgeon", label = NULL))
                                            ),
                                            tags$tr(width = "90%",
                                                    tags$td(width = "30%", tags$div(style = "font-size:12px; font-weight:bold; text-align:left","Assistants:")),
                                                    tags$td(width = "60%", textInput(inputId = "surgical_assistants", label = NULL))
                                            ),
                                            tags$tr(width = "90%",
                                                    tags$td(width = "30%", tags$div(style = "font-size:12px; font-weight:bold; text-align:left","Preoperative Diagnosis:")),
                                                    tags$td(width = "60%", textInput(inputId = "preoperative_diagnosis", label = NULL))
                                            ),
                                            tags$tr(width = "90%",
                                                    tags$td(width = "30%", tags$div(style = "font-size:12px; font-weight:bold; text-align:left","Postoperative Diagnosis:")),
                                                    tags$td(width = "60%", textInput(inputId = "postoperative_diagnosis", label = NULL))
                                            ),
                                            tags$tr(width = "90%",
                                                    tags$td(width = "30%", tags$div(style = "font-size:12px; font-weight:bold; text-align:left","Surgical Indications:")),
                                                    tags$td(width = "60%", textInput(inputId = "indications", label = NULL))
                                            ),
                                            tags$tr(width = "90%",
                                                    tags$td(width = "30%", tags$div(style = "font-size:12px; font-weight:bold; text-align:left","Findings:")),
                                                    tags$td(width = "60%", textInput(inputId = "surgical_findings:", value = "None", label = NULL)),
                                            ),
                                            tags$tr(width = "90%",
                                                    tags$td(width = "30%", tags$div(style = "font-size:12px; font-weight:bold; text-align:left","Specimens:")),
                                                    tags$td(width = "60%", textInput(inputId = "specimens_removed:", value = "None", label = NULL))
                                            ),
                                            tags$tr(width = "90%",
                                                    tags$td(width = "30%", tags$div(style = "font-size:12px; font-weight:bold; text-align:left","Estimated Blood Loss:")),
                                                    tags$td(width = "60%", textInput(inputId = "ebl:", label = NULL))
                                            ),
                                            tags$tr(width = "90%",
                                                    tags$td(width = "30%", tags$div(style = "font-size:12px; font-weight:bold; text-align:left","Intraoperative Complications:")),
                                                    tags$td(width = "60%", textInput(inputId = "intraoperative_complications:", value = "None", label = NULL))
                                            )
                                          ),
                                          br(),
                                          checkboxGroupButtons(inputId = "additional_procedures", size = "sm",
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
                                                                           "***Other***"),
                                                               selected = "Spinal Cord Monitoring", width = "80%",
                                                               checkIcon = list(
                                                                 yes = tags$i(class = "fas fa-check", 
                                                                              style = "color: steelblue")),
                                                               justified = TRUE),
                                          br(), 
                                          h4("___________________________________"),
                                          h4("End of Procedure & Closure Details"),
                                          tags$table(
                                            tags$tr(width = "90%",
                                                    tags$td(width = "30%", tags$div(style = "font-size:12px; font-weight:bold; text-align:left","Number of Deep Drains:")),
                                                    tags$td(width = "60%", numericInput(inputId = "deep_drains", label = NULL, value = 1))
                                            ),
                                            tags$tr(width = "90%",
                                                    tags$td(width = "30%", tags$div(style = "font-size:12px; font-weight:bold; text-align:left","Number of Superficial Drains:")),
                                                    tags$td(width = "60%", numericInput(inputId = "superficial_drains", label = NULL, value = 1))
                                            ),
                                            tags$tr(width = "90%",
                                                    tags$td(width = "30%", tags$div(style = "font-size:12px; font-weight:bold; text-align:left","Select any that were used during closure:")),
                                                    tags$td(width = "60%", checkboxGroupButtons(inputId = "additional_end_procedure_details", size = "sm",
                                                                                                label = NULL, 
                                                                                                # individual = TRUE,
                                                                                                direction = "vertical", 
                                                                                                choices = c("Vancomycin Powder",
                                                                                                            "Antibiotic Beads"),
                                                                                                selected = "Vancomycin Powder", width = "80%",
                                                                                                checkIcon = list(
                                                                                                  yes = tags$i(class = "fas fa-check", 
                                                                                                               style = "color: steelblue")),
                                                                                                justified = TRUE),
                                                    )
                                            ),
                                            tags$tr(width = "90%",
                                                    tags$td(width = "30%", tags$div(style = "font-size:12px; font-weight:bold; text-align:left","Skin Closure:")),
                                                    tags$td(width = "60%", checkboxGroupButtons(inputId = "closure_details", size = "sm",
                                                                                                label = NULL, 
                                                                                                direction = "vertical", 
                                                                                                choices = c("Subcutaneous suture",
                                                                                                            "Nylon", 
                                                                                                            "Staples"),
                                                                                                selected = "Subcutaneous suture", width = "80%",
                                                                                                checkIcon = list(
                                                                                                  yes = tags$i(class = "fas fa-check", 
                                                                                                               style = "color: steelblue")),
                                                                                                justified = TRUE),
                                                    )
                                            ),
                                            tags$tr(width = "90%",
                                                    tags$td(width = "30%", tags$div(style = "font-size:12px; font-weight:bold; text-align:left","Skin/Dressing:")),
                                                    tags$td(width = "60%", checkboxGroupButtons(inputId = "dressing_details", size = "sm",
                                                                                                label = NULL, 
                                                                                                direction = "vertical", 
                                                                                                choices = c("Dermabond", 
                                                                                                            "Prineo",
                                                                                                            "Steristrips", 
                                                                                                            "an Incisional Wound Vac", 
                                                                                                            "a water tight dressing"),
                                                                                                selected = c("Dermabond", "water tight dressing"), width = "80%",
                                                                                                checkIcon = list(
                                                                                                  yes = tags$i(class = "fas fa-check", 
                                                                                                               style = "color: steelblue")),
                                                                                                justified = TRUE),
                                                    )
                                            )
                                          ),
                                          br(),
                                          checkboxGroupButtons(inputId = "additional_surgical_details",size = "sm", 
                                                               choices = c("Decompression for Spondylolisthesis")),
                                          br(),
                                   ),
                                   column(7, 
                                          h3("Operative Note Generator"),
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
                                          # htmlOutput(outputId = "operative_note_header_details", ),
                                          br(),
                                          # uiOutput(outputId = "operative_note_text"),
                                          br(),
                                          uiOutput("clipboard_ui")
                                   )
                                   )
              )
    )
  )
)



###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ########## SERVER STARTS  ###### ###### ###### ###### ###### ######  ###### ###### ###### ###### ###### ######
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ########## SERVER STARTS  ###### ###### ###### ###### ###### ######  ###### ###### ###### ###### ###### ######
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ########## SERVER STARTS  ###### ###### ###### ###### ###### ######  ###### ###### ###### ###### ###### ######
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ########## SERVER STARTS  ###### ###### ###### ###### ###### ######  ###### ###### ###### ###### ###### ######
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ########## SERVER STARTS  ###### ###### ###### ###### ###### ######  ###### ###### ###### ###### ###### ######


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ################     ################     ################  RENDER UI'S ######################################
  
  observeEvent(input$spine_approach, {
    if(input$spine_approach == "Anterior"){
      updateRadioGroupButtons(session = session, 
                              inputId = "approach_specified", 
                              choices = c("Paramedian",
                                          "Lateral Transpsoas",
                                          "Lateral Antepsoas",
                                          "Thoracoabdominal",
                                          "Thoracotomy",
                                          "Transperitoneal",
                                          "Retroperitoneal",
                                          "Cervical"), 
                              selected = "Paramedian", 
                              size = 'xs')
    }
    if(input$spine_approach == "Posterior"){
      updateRadioGroupButtons(session = session, 
                              inputId = "approach_specified", 
                              choices = c("Midline",
                                          "Paraspinal or Paramedian", 
                                          "Stab"), 
                              selected = "Midline", 
                              size = 'xs')
    }
    
  })
  
  observeEvent(input$primary_diagnosis_group,{
    if(input$primary_diagnosis_group == "Spinal Condition (E.g. Degenerative)"){
      updateCheckboxGroupButtons(session = session, 
                                 inputId = "spine_dx_subgroup", 
                                 size = "xs",
                                 choices = c("Spinal Stenosis", 
                                             "Myelopathy", 
                                             "Foraminal Stenosis",
                                             "Degenerative Spondylolisthesis",
                                             "Pseudarthrosis",
                                             "Lumbar Disc Herniation",
                                             "Other"),
                                 checkIcon = list(
                                   yes = tags$i(class = "fa fa-check-square",
                                                style = "color: steelblue; align:left"),
                                   no = tags$i(class = "fa fa-square-o",
                                               style = "color: steelblue; align:left")))
    }
    
    if(input$primary_diagnosis_group == "Neoplasm"){
      updateCheckboxGroupButtons(session = session, 
                                 inputId = "spine_dx_subgroup", 
                                 size = "xs",
                                 choices = c("Metastatic Disease to the Spinal column",
                                             "Primary Neoplasm of the spine",
                                             "Other"),
                                 checkIcon = list(
                                   yes = tags$i(class = "fa fa-check-square",
                                                style = "color: steelblue"),
                                   no = tags$i(class = "fa fa-square-o",
                                               style = "color: steelblue")))
    }
    
    if(input$primary_diagnosis_group == "Lesion (trauma, infection, etc)"){
      updateCheckboxGroupButtons(session = session, 
                                 inputId = "spine_dx_subgroup", 
                                 size = "xs",
                                 choices = c("Traumatic Burst Fracture", 
                                             "Fracture Dislocation", 
                                             "Osteomyelitis/Diskitis", 
                                             "Other"),
                                 checkIcon = list(
                                   yes = tags$i(class = "fa fa-check-square",
                                                style = "color: steelblue"),
                                   no = tags$i(class = "fa fa-square-o",
                                               style = "color: steelblue")))
    }
    
    if(input$primary_diagnosis_group == "Deformity"){
      updateCheckboxGroupButtons(session = session, 
                                 inputId = "spine_dx_subgroup", 
                                 size = "xs",
                                 choices = c("Idiopathic Scoliosis",
                                             "Scoliosis associated with another condition", 
                                             "Secondary Kyphosis",
                                             "Scheurmanns Kyphosis",
                                             "Flatback Syndrome",
                                             "Acquired Spondylolisthesis", 
                                             "Congenital Spondylolisthesis", 
                                             "Lumbar Disc Herniation",
                                             "Spinal Stenosis",
                                             "Other"
                                 ),
                                 checkIcon = list(
                                   yes = tags$i(class = "fa fa-check-square",
                                                style = "color: steelblue"),
                                   no = tags$i(class = "fa fa-square-o",
                                               style = "color: steelblue")))
    }
  })
  
  
  observeEvent(list(input$spine_dx_subgroup, input$spine_dx_subgroup_other),{
    updateTextInput(session = session, inputId = "preoperative_diagnosis", 
                    value = glue_collapse(x = map(.x = input$spine_dx_subgroup, 
                                                  .f = ~ if_else(.x == "Other", input$spine_dx_subgroup_other, .x)),
                                          sep = ", ", last = " and "))
  })
  
  observeEvent(list(input$spine_dx_subgroup, input$spine_dx_subgroup_other),{
    updateTextInput(session = session, inputId = "postoperative_diagnosis", 
                    value = glue_collapse(x = map(.x = input$spine_dx_subgroup, 
                                                  .f = ~ if_else(.x == "Other", input$spine_dx_subgroup_other, .x)),
                                          sep = ", ", last = " and "))
  })
  
  observeEvent(input$symptoms,{
    updateTextInput(session = session, inputId = "indications", value = glue_collapse(input$symptoms, sep = ", ", last = " and "))
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
  
  
  observeEvent(input$spine_dx_subgroup, {
    if(any(str_detect(string = input$spine_dx_subgroup, pattern = "Fracture"))){
      updateCheckboxGroupButtons(session = session, 
                                 inputId = "additional_procedures", 
                                 selected = append(input$additional_procedures, "Open treatment of vertebral fracture"))
    }
  })
  
  observeEvent(input$spine_approach, {
    if(input$spine_approach == "Anterior"){
      updateRadioGroupButtons(session = session, 
                              inputId = "object_to_add", 
                              choices = c(
                                "Disc Arthroplasty" = "anterior_disc_arthroplasty",
                                "Decompression, Diskectomy & Fusion + Interbody Implant" = "decompression_diskectomy_fusion",
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
  
  observeEvent(input$add_implants, {
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
  })
  
  observeEvent(input$add_decompressions, {
    updateRadioGroupButtons(session = session, 
                            inputId = "object_to_add", 
                            choices = c(
                              "Laminoplasty" = "laminoplasty",
                              "Central Laminectomy" = "laminectomy",
                              "Sublaminar Decompression (+ foraminotomy)" = "sublaminar_decompression",
                              "Laminotomy (Hemilaminectomy)" = "laminotomy",
                              "Diskectomy" = "diskectomy",
                              "Transpedicular Decompression" = "transpedicular_approach",
                              "Costovertebral Decompression" = "costovertebral_approach"
                            ),
                            checkIcon = list(
                              yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                            ),
                            selected = "laminectomy"
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
                                        "Grade 6 (Multilevel VCR)" = "grade_6"),
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
                                        "LLIF" = "llif",
                                        "PLIF" = "plif", 
                                        "Interbody Fusion, No Implant" = "no_implant_interbody_fusion"),
                            checkIcon = list(
                              yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                            ),
                            selected = "TLIF"
    )
  })
  
  output$currently_adding <- renderText(paste("Currently Adding: ", str_to_title(string = str_replace_all(string = input$object_to_add, pattern = "_", replacement = " "))), sep = "")
  
  
  ###  RODS
  
  
  observeEvent(left_main_rod_reactive_sf(), {
    if(nrow(left_rod_implants_df()) > 1){
      updatePickerInput(session = session, 
                        inputId = "left_main_rod_size", 
                        # choices = c("Transition", "3.5mm", "4.5mm", "4.75mm", "5.5mm", "6.0mm", "6.34mm/quarter in"),
                        selected = if_else(input$left_main_rod_size == "None", "5.5mm", input$left_main_rod_size)
                        # selected = "5.5mm"
      )
      updateSliderTextInput(session = session, 
                            inputId = "left_main_rod_material", 
                            choices = c("Titanium", "Cobalt Chrome", "Stainless Steel"),
                            selected = if_else(input$left_main_rod_material == "Non-instrumented", "Titanium", input$left_main_rod_material)
                            # selected = "Titanium"
      )
    }
  })
  observeEvent(right_main_rod_reactive_sf(), {
    if(nrow(right_rod_implants_df()) > 1){
      updatePickerInput(session = session, 
                        inputId = "right_main_rod_size", 
                        # choices = c("Transition", "3.5mm", "4.5mm", "4.75mm", "5.5mm", "6.0mm", "6.34mm/quarter in"),
                        selected = if_else(input$right_main_rod_size == "None", "5.5mm", input$right_main_rod_size)
                        # selected = "5.5mm"
      )
      updateSliderTextInput(session = session, 
                            inputId = "right_main_rod_material", 
                            choices = c("Titanium", "Cobalt Chrome", "Stainless Steel"),
                            selected = if_else(input$right_main_rod_material == "Non-instrumented", "Titanium", input$right_main_rod_material)
                            # selected = "Titanium"
      )
    }
  })
  
  observeEvent(input$add_left_satellite_rod, {
    if(input$add_left_satellite_rod == TRUE){
      middle_selected_vector <- (left_rod_implants_df() %>% filter(between(vertebral_number, median(vertebral_number)-1, median(vertebral_number + 1))))$level
      
      left_uninstrumented_levels_df <- tibble(vertebral_number = as.double(seq(from = min(left_rod_implants_df()$vertebral_number), to = max(left_rod_implants_df()$vertebral_number)))) %>%
        left_join(left_rod_implants_df()) %>%
        filter(is.na(level)) 
      
      if(nrow(left_uninstrumented_levels_df) > 0){
        left_uninstrumented_level_df <- left_uninstrumented_levels_df %>%
          select(vertebral_number) %>%
          filter(vertebral_number == max(vertebral_number)) %>%
          left_join(levels_numbered_df)
        
        satellite_rod_spanning_list <- identify_cranial_caudal_interspace_body_list_function(level = left_uninstrumented_level_df$level)
        
        updateSliderTextInput(session = session, 
                              inputId = "left_satellite_rod", 
                              choices = left_rod_implants_df()$level,
                              selected = c(satellite_rod_spanning_list$cranial_level, satellite_rod_spanning_list$caudal_level)
        ) 
      }else{
        updateSliderTextInput(session = session, 
                              inputId = "left_satellite_rod", 
                              choices = left_rod_implants_df()$level,
                              selected = c(middle_selected_vector[1], middle_selected_vector[2])
        ) 
      }
    }
    
  })
  
  observeEvent({list(left_rod_implants_df(), input$add_left_accessory_rod, input$add_left_satellite_rod, input$add_left_intercalary_rod, input$add_left_linked_rods)},ignoreInit = TRUE, {
    
    middle_selected_vector <- (left_rod_implants_df() %>% filter(between(vertebral_number, median(vertebral_number)-1, median(vertebral_number + 1))))$level
    
    if(input$add_left_accessory_rod == TRUE){
      updateSliderTextInput(session = session, 
                            inputId = "left_accessory_rod", 
                            choices = left_rod_implants_df()$level,
                            selected = c(middle_selected_vector[1], middle_selected_vector[2])
      )
    }
    
    if(input$add_left_intercalary_rod == TRUE){
      updateSliderTextInput(session = session, 
                            inputId = "left_intercalary_rod", 
                            choices = left_rod_implants_df()$level,
                            selected = c(middle_selected_vector[1], middle_selected_vector[2])
      )
    }
    if(input$add_left_linked_rods == TRUE){
      updateSliderTextInput(session = session,
                            inputId = "left_linked_rods_overlap",
                            choices = left_rod_implants_df()$level,
                            selected = c(middle_selected_vector[1], middle_selected_vector[2])
      )
    }
    
  })
  
  observeEvent(input$add_right_satellite_rod, {
    if(input$add_right_satellite_rod == TRUE){
      middle_selected_vector <- (right_rod_implants_df() %>% filter(between(vertebral_number, median(vertebral_number)-1, median(vertebral_number + 1))))$level
      
      right_uninstrumented_levels_df <- tibble(vertebral_number = as.double(seq(from = min(right_rod_implants_df()$vertebral_number), to = max(right_rod_implants_df()$vertebral_number)))) %>%
        left_join(right_rod_implants_df()) %>%
        filter(is.na(level)) 
      
      if(nrow(right_uninstrumented_levels_df) > 0){
        right_uninstrumented_level_df <- right_uninstrumented_levels_df %>%
          select(vertebral_number) %>%
          filter(vertebral_number == max(vertebral_number)) %>%
          left_join(levels_numbered_df)
        
        satellite_rod_spanning_list <- identify_cranial_caudal_interspace_body_list_function(level = right_uninstrumented_level_df$level)
        
        updateSliderTextInput(session = session, 
                              inputId = "right_satellite_rod", 
                              choices = right_rod_implants_df()$level,
                              selected = c(satellite_rod_spanning_list$cranial_level, satellite_rod_spanning_list$caudal_level)
        ) 
      }else{
        updateSliderTextInput(session = session, 
                              inputId = "right_satellite_rod", 
                              choices = right_rod_implants_df()$level,
                              selected = c(middle_selected_vector[1], middle_selected_vector[2])
        ) 
      }
    }
  })
  
  
  observeEvent({list(right_rod_implants_df(), input$add_right_accessory_rod, input$add_right_intercalary_rod, input$add_right_linked_rods)},ignoreInit = TRUE, {
    middle_selected_vector <- (right_rod_implants_df() %>% filter(between(vertebral_number, median(vertebral_number)-1, median(vertebral_number + 1))))$level
    
    
    if(input$add_right_accessory_rod == TRUE){
      updateSliderTextInput(session = session, 
                            inputId = "right_accessory_rod", 
                            choices = right_rod_implants_df()$level,
                            selected = c(middle_selected_vector[1], middle_selected_vector[2])
      )
    }
    
    if(input$add_right_intercalary_rod == TRUE){
      updateSliderTextInput(session = session, 
                            inputId = "right_intercalary_rod", 
                            choices = right_rod_implants_df()$level,
                            selected = c(middle_selected_vector[1], middle_selected_vector[2])
      )
    }
    if(input$add_right_linked_rods == TRUE){
      updateSliderTextInput(session = session, 
                            inputId = "right_linked_rods_overlap", 
                            choices = right_rod_implants_df()$level,
                            selected = c(middle_selected_vector[1], middle_selected_vector[2])
      )
    }
  })
  
  observeEvent(fusion_levels_computed_reactive_df(), {
    updateCheckboxGroupButtons(session = session, 
                               inputId = "fusion_levels_confirmed", 
                               selected = fusion_levels_computed_reactive_df()$level)
  })
  
  observeEvent(fusion_levels_computed_reactive_df(), {
    if(nrow(fusion_levels_computed_reactive_df()) > 0){
      updateSwitchInput(session = session, 
                        inputId = "fusion_procedure_performed", 
                        value = TRUE)
    }
  })
  
  
  make_interbody_ui_function <-  function(level = NULL){
    level_input_id <- str_to_lower(string = str_replace_all(string = level, pattern = "-", replacement = "_"))
    
    fluidRow(
      hr(),
      column(width = 2,
             h4(strong(paste0(level, ":")))
      ),
      column(10, 
             fluidRow(
               column(width = 5, 
                      pickerInput(
                        inputId = glue("{level_input_id}_interbody_composition"),
                        label = NULL,
                        inline = "auto",
                        options = list(
                          title = "Choose Implant Type"),
                        choices = c("Allograft",
                                    "Autograft",
                                    "Carbon Fiber",
                                    "Coated PEEK",
                                    "PEEK", "Hybrid",
                                    "Titanium",
                                    "3D/Porous Titanium",
                                    "Other"),
                      ), 
               ), 
               column(width = 4,
                      textInput(inputId = glue("{level_input_id}_interbody_device_name"),
                                label = NULL, placeholder = "Cage Name")
               ), 
               column(width = 3, 
                      numericInput(inputId = glue("{level_input_id}_interbody_height"),
                                   label = NULL, value = 8,min = 5, max = 30,step = 1))
             ),
             fluidRow(
               column(width = 3, 
                      h5("Other Comments")),
               column(width = 6,
                      textInput(inputId = glue("{level_input_id}_interbody_other"),
                                label = NULL,
                                placeholder = "Details",
                                width = "100%")
               ),
               column(width = 3, 
                      prettyCheckboxGroup(
                        inputId = glue("{level_input_id}_interbody_integrated_fixation"),
                        label = NULL, choices = c("Integrated Fixation"),
                        fill = TRUE,
                        # value = FALSE,
                        status = "danger",
                        shape = "curve"
                      ),
                      prettyCheckboxGroup(
                        inputId = glue("{level_input_id}_interbody_expandable"),
                        label = NULL,
                        choices = c("Expandable"),
                        fill = TRUE,
                        status = "danger",
                        shape = "curve"
                      )
               )
             )
      ),
      hr()
    )
  }
  
  # "{level_input_id}_interbody_composition"
  # "{level_input_id}_interbody_device_name"
  # "{level_input_id}_interbody_height"
  # "{level_input_id}_interbody_integrated_fixation"
  # "{level_input_id}_interbody_other"
  # "{level_input_id}_interbody_expandable"
  
  
  output$interbody_implants_ui <- renderUI({
    
    interbody_implants_df <- all_objects_to_add_list$objects_df %>%
      # filter(approach == "anterior") %>%
      filter(
        object == "anterior_interbody_implant" |
          object == "anterior_interbody_implant" |
          object == "tlif" |
          object == "plif" |
          object == "llif" |
          object == "anterior_disc_arthroplasty" |
          object == "corpectomy_cage") %>%
      select(level, vertebral_number, object, approach) %>%
      distinct() %>%
      arrange(vertebral_number)
    
    if(nrow(interbody_implants_df) >0){
      
      fixedRow(
        column(width = 12, 
               h4(strong("Interbody Implant Details:")),
               # )
               column(width = 2,
                      h4(strong("Level:"))
               ),
               column(10, 
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
    interbody_df <- all_objects_to_add_list$objects_df %>%
      filter(object == "anterior_interbody_implant" |
               object == "tlif" | 
               object == "llif" |
               object == "plif" |
               # object == "anterior_disc_arthroplasty" |
               object == "corpectomy_cage") %>%
      select(level, vertebral_number, approach, object) %>%
      distinct()
    
    if(nrow(interbody_df) > 0){
      
      interbody_details_df <- interbody_df %>%
        mutate(level = str_to_lower(string = str_replace_all(string = level, pattern = "-", replacement = "_"))) %>%
        mutate(composition_label = glue("{level}_interbody_composition")) %>%
        mutate(device_name_label = glue("{level}_interbody_device_name")) %>%
        mutate(height_label = glue("{level}_interbody_height")) %>%
        mutate(integrated_fixation_label = glue("{level}_interbody_integrated_fixation")) %>%
        mutate(expandable_label = glue("{level}_interbody_expandable")) %>%
        mutate(other_label = glue("{level}_interbody_other")) %>%
        select(-level) %>%
        left_join(levels_numbered_df) %>%
        mutate(composition = map(.x = composition_label, .f = ~input[[.x]])) %>%
        mutate(device_name = map(.x = device_name_label, .f = ~input[[.x]])) %>%
        mutate(height = map(.x = height_label, .f = ~input[[.x]])) %>%
        mutate(integrated_fixation = map(.x = integrated_fixation_label, .f = ~if_else(is.null(input[[.x]]), "xx", input[[.x]]))) %>%
        mutate(expandable = map(.x = expandable_label, .f = ~if_else(is.null(input[[.x]]), "xx", input[[.x]]))) %>%
        mutate(other = map(.x = other_label, .f = ~if_else(is.null(input[[.x]]), "xx", input[[.x]]))) %>%
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
      interbody_details_df <- tibble(level = character(), approach = character(), object = character(), composition = character(), implant_statement = character())
    }
    
    
    interbody_details_df
    
  })
  
  output$interbody_details_table <- renderTable({
    interbody_df_reactive()
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
                                               # rod = character(),
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
      
      # interbody_df <- fusion_df %>%
      #     mutate(obect = "interbody_implant")
      
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
  
  #### OBSERVE THE PLOT CLICK AND ADD APPROPRIATE OBJECT TO A LIST IN THE ORDER IT WAS ADDED ####
  
  objects_added_in_sequence_list <- reactiveValues()
  
  objects_added_in_sequence_list$sequence_order_list <- list()
  
  
  observeEvent(input$plot_click, {
    
    object_added_df <- object_added_reactive_df() %>%
      select(level, side, object)
    
    objects_added_in_sequence_list$sequence_order_list <- append(objects_added_in_sequence_list$sequence_order_list, object_added_df)
    
  }
  )
  
  
  
  #### OBSERVE THE PLOT CLICK AND ADD APPROPRIATE object ####
  observeEvent(input$plot_click, {
    
    all_objects_to_add_list$objects_df <- all_objects_to_add_list$objects_df  %>%
      union_all(object_added_reactive_df()) %>%
      distinct()
    
    if(nrow(all_objects_to_add_list$objects_df %>% filter(category == "osteotomy")) > 1){
      all_objects_to_add_list$objects_df <- all_objects_to_add_list$objects_df %>%
        filter(category == "osteotomy") %>%
        mutate(object = fct_relevel(as_factor(object), c("grade_1", "complete_facetectomy", "grade_2", "grade_3", "grade_4", "grade_5"))) %>%
        arrange(object) %>%
        mutate(implant_rank = as.numeric(object)) %>%
        select(level, implant_rank, everything())  %>% 
        mutate(side_value = case_when(
          side == "central" ~ 3,
          side == "left" ~1,
          side == "right" ~ 1
        )) %>%
        group_by(level, side) %>%
        filter(implant_rank == max(implant_rank)) %>%
        group_by(level) %>%
        filter(side_value == max(side_value)) %>%
        select(-implant_rank, -side_value) %>%
        union_all(all_objects_to_add_list$objects_df %>% filter(category != "osteotomy")) %>%
        ungroup()
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
  
  
  left_rod_implants_df <- reactive({
    all_objects_to_add_list$objects_df %>%
      filter(!is.na(x)) %>%
      filter(x < 0.5) %>%
      filter(str_detect(string = object, pattern = "screw") | str_detect(string = object, pattern = "hook") | str_detect(string = object, pattern = "wire")) %>%
      select(level, vertebral_number, x, y) %>%
      distinct() %>%
      arrange(vertebral_number) 
  })
  
  right_rod_implants_df <- reactive({
    all_objects_to_add_list$objects_df %>%
      filter(!is.na(x)) %>%
      filter(x > 0.5) %>%
      filter(str_detect(string = object, pattern = "screw") | str_detect(string = object, pattern = "hook") | str_detect(string = object, pattern = "wire")) %>%
      select(level, vertebral_number, x, y) %>%
      distinct() %>%
      arrange(vertebral_number)
  })
  
  
  
  ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
  ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
  ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
  ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
  
  ## Revision Implants 
  
  ################ FUNCTION TO MAKE RODS ####################
  left_main_rod_reactive_sf <- reactive({
    if(nrow(left_rod_implants_df()) > 1){
      left_rod_matrix <- left_rod_implants_df() %>%
        select(x, y) %>%
        arrange(rev(y)) %>%
        distinct() %>%
        as.matrix()  
      
      left_main_rod_sf <- st_buffer(st_linestring(left_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
    }else{
      left_main_rod_sf <-  NULL
    }
    left_main_rod_sf
  })
  
  left_accessory_rod_reactive_sf <- reactive({
    if(nrow(left_rod_implants_df()) > 2 & input$add_left_accessory_rod == TRUE){
      left_accessory_rod_sf <- build_additional_rod_function(rod_type = "accessory", new_rod_vector = input$left_accessory_rod, full_implant_df = left_rod_implants_df())
    }else{
      left_accessory_rod_sf <-  NULL
    }
    left_accessory_rod_sf
  })
  
  left_satellite_rod_reactive_sf <- reactive({
    if(nrow(left_rod_implants_df()) > 2 & input$add_left_satellite_rod == TRUE){
      
      left_satellite_rod_sf <- build_additional_rod_function(rod_type = "satellite", new_rod_vector = input$left_satellite_rod, full_implant_df = left_rod_implants_df())
    }else{
      left_satellite_rod_sf <-  NULL
    }
    left_satellite_rod_sf
  })
  
  left_intercalary_rod_reactive_sf <- reactive({
    if(nrow(left_rod_implants_df()) > 2 & input$add_left_intercalary_rod == TRUE){
      
      left_intercalary_rod_sf <- build_additional_rod_function(rod_type = "intercalary", new_rod_vector = input$left_intercalary_rod, full_implant_df = left_rod_implants_df())
    }else{
      left_intercalary_rod_sf <-  NULL
    }
    left_intercalary_rod_sf
  })
  
  left_linked_rods_reactive_sf <- reactive({
    if(nrow(left_rod_implants_df()) > 2 & input$add_left_linked_rods == TRUE){
      
      left_linked_rods_reactive_sf <- build_additional_rod_function(rod_type = "linked", new_rod_vector = input$left_linked_rods_overlap, full_implant_df = left_rod_implants_df())
    }else{
      left_linked_rods_reactive_sf <-  NULL
    }
    left_linked_rods_reactive_sf
  })
  
  ############## RIGHT RODS
  
  right_main_rod_reactive_sf <- reactive({
    if(nrow(right_rod_implants_df()) > 1 & input$add_right_linked_rods == FALSE & input$add_right_intercalary_rod == FALSE){
      right_rod_matrix <- right_rod_implants_df() %>%
        select(x, y) %>%
        arrange(rev(y)) %>%
        distinct() %>%
        as.matrix()  
      
      right_main_rod_sf <- st_buffer(st_linestring(right_rod_matrix), dist = 0.003, endCapStyle = "ROUND")
    }else{
      right_main_rod_sf <-  NULL
    }
    right_main_rod_sf
  })
  
  right_accessory_rod_reactive_sf <- reactive({
    if(nrow(right_rod_implants_df()) > 2 & input$add_right_accessory_rod == TRUE){
      right_accessory_rod_sf <- build_additional_rod_function(rod_type = "accessory", new_rod_vector = input$right_accessory_rod, full_implant_df = right_rod_implants_df())
    }else{
      right_accessory_rod_sf <-  NULL
    }
    right_accessory_rod_sf
  })
  
  right_satellite_rod_reactive_sf <- reactive({
    if(nrow(right_rod_implants_df()) > 2 & input$add_right_satellite_rod == TRUE){
      right_satellite_rod_sf <- build_additional_rod_function(rod_type = "satellite", new_rod_vector = input$right_satellite_rod, full_implant_df = right_rod_implants_df())
    }else{
      right_satellite_rod_sf <-  NULL
    }
    right_satellite_rod_sf
  })
  
  right_intercalary_rod_reactive_sf <- reactive({
    if(nrow(right_rod_implants_df()) > 2 & input$add_right_intercalary_rod == TRUE){
      right_intercalary_rod_sf <- build_additional_rod_function(rod_type = "intercalary", new_rod_vector = input$right_intercalary_rod, full_implant_df = right_rod_implants_df())
    }else{
      right_intercalary_rod_sf <-  NULL
    }
    right_intercalary_rod_sf
  })
  
  right_linked_rods_reactive_sf <- reactive({
    if(nrow(right_rod_implants_df()) > 2 & input$add_right_linked_rods == TRUE){
      right_linked_rods_reactive_sf <- build_additional_rod_function(rod_type = "linked", new_rod_vector = input$right_linked_rods_overlap, full_implant_df = right_rod_implants_df())
    }else{
      right_linked_rods_reactive_sf <-  NULL
    }
    right_linked_rods_reactive_sf
  })
  
  all_rods_reactive_list <- reactive({
    ### RODS 
    if(input$add_left_intercalary_rod == TRUE | input$add_left_linked_rods == TRUE | input$add_left_satellite_rod == TRUE){
      left_main_rod_sf <- NULL
    }else{
      left_main_rod_sf <- left_main_rod_reactive_sf()
    }
    if(input$add_right_intercalary_rod == TRUE | input$add_right_linked_rods == TRUE | input$add_right_satellite_rod == TRUE){
      right_main_rod_sf <- NULL
    }else{
      right_main_rod_sf <- right_main_rod_reactive_sf()
    }
    
    all_rods_list <- compact(list(left_main_rod_sf,
                                  left_accessory_rod_reactive_sf(),
                                  left_satellite_rod_reactive_sf(),
                                  left_intercalary_rod_reactive_sf(),
                                  left_linked_rods_reactive_sf(),
                                  right_main_rod_sf,
                                  right_accessory_rod_reactive_sf(),
                                  right_satellite_rod_reactive_sf(),
                                  right_intercalary_rod_reactive_sf(),
                                  right_linked_rods_reactive_sf()))
    all_rods_list
    
  })
  
  #################### MAKE THE PLOT #######################
  #################### MAKE THE PLOT #######################
  #################### MAKE THE PLOT #######################
  
  output$spine_plan <- renderPlot({
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
      
      if(any(str_detect(string = all_objects_to_add_list$objects_df$object, pattern = "anterior_disc_arthroplasty"))){
        
        # anterior_disc_arthroplasty_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "anterior_disc_arthroplasty")%>% unnest())$object_constructed)
        
        anterior_disc_arthroplasty_df <- all_objects_to_add_list$objects_df %>%
          filter(object == "anterior_disc_arthroplasty") %>%
          remove_empty() %>%
          unnest()
        
      }else{
        # anterior_disc_arthroplasty_sf <- NULL
        anterior_disc_arthroplasty_df <- tibble(object_constructed = c(), color = c())
      }
      
      if(any(str_detect(string = all_objects_to_add_list$objects_df$object, pattern = "anterior_buttress_plate"))){
        anterior_buttress_plate_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "anterior_buttress_plate"))$object_constructed)
      }else{
        anterior_buttress_plate_sf <- NULL
      }
      
      if(any(str_detect(string = all_objects_to_add_list$objects_df$object, pattern = "anterior_plate"))){
        anterior_plate_sf <- st_union(st_combine(st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "anterior_plate"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE)
      }else{
        anterior_plate_sf <- NULL
      }
      
      if(any(str_detect(string = all_objects_to_add_list$objects_df$object, pattern = "corpectomy"))){
        corpectomy_sf <- st_union(st_combine(st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "corpectomy"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE)
      }else{
        corpectomy_sf <- NULL
      }
      
      if(any(str_detect(all_objects_to_add_list$objects_df$object, pattern = "corpectomy_cage"))){
        corpectomy_cage_sf <- st_union(st_combine(st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "corpectomy_cage"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE)
        
      }else{
        corpectomy_cage_sf <- NULL
      }
      
      if(nrow(all_objects_to_add_list$objects_df %>% filter(object == "diskectomy_fusion")) > 0){
        diskectomy_fusion_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "diskectomy_fusion"))$object_constructed)
      }else{
        diskectomy_fusion_sf <- NULL
      }
      if(nrow(all_objects_to_add_list$objects_df %>% filter(object == "decompression_diskectomy_fusion")) > 0){
        decompression_diskectomy_fusion_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "decompression_diskectomy_fusion"))$object_constructed)
      }else{
        decompression_diskectomy_fusion_sf <- NULL
      }
      
      if(any(str_detect(string = all_objects_to_add_list$objects_df$object, pattern = "diskectomy_fusion"))){
        
        anterior_fusions_selected_df <- all_objects_to_add_list$objects_df %>% 
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
      
      if(any(str_detect(string = all_objects_to_add_list$objects_df$object, pattern = "screw_washer"))){
        screw_washer_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "screw_washer"))$object_constructed)
        washer_df <- all_objects_to_add_list$objects_df %>%
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
        # geom_sf(data = text_square, fill = "grey98") +
        # draw_text(text = plan_df$descriptor, x = x_left_limit + 0.01, y = plan_df$y, size = input$label_text_size - 2, hjust = 0, vjust = 0.5) +
        # draw_text(text = str_wrap(string = plan_df$value, width = 40 +input$label_text_offset, exdent = 5), x = x_left_limit + 0.12, y = plan_df$y, size = input$label_text_size - 2, hjust = 0, vjust = 0.5, lineheight = 0.7) +
        ylim(input$crop_y[1], y_start_with_text) +
        xlim(x_left_limit, x_right_limit) + 
        scale_fill_identity()
      
    }else{
      if(input$spine_approach == "Lateral"){
        NULL
      }else{
        ### POSTERIOR
        
        #### REVISION IMPLANTS:
        # if(input$primary_revision == "Revision"){
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
        if(any(str_detect((all_objects_to_add_list$objects_df %>% filter(category == "implant"))$object, pattern = "screw"))){
          screws_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(category == "implant" & str_detect(string = object, pattern = "screw")))$object_constructed)
        }else{
          screws_sf <- NULL
        }
        
        ## HOOKS
        if(any(str_detect(all_objects_to_add_list$objects_df$object, pattern = "hook"))){
          hook_df <- all_objects_to_add_list$objects_df %>% 
            filter(str_detect(string = object, pattern = "hook"))
          
          hooks_sf <- st_multipolygon(hook_df$object_constructed)
        }else{
          hooks_sf <- NULL
        }
        ## SUBLAMINAR BANDS
        if(any(str_detect(all_objects_to_add_list$objects_df$object, pattern = "wire"))){
          sublaminar_wires_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(str_detect(string = object, pattern = "wire")))$object_constructed)
        }else{
          sublaminar_wires_sf <- NULL
        }
        
        ## Tethers
        if(any(str_detect(all_objects_to_add_list$objects_df$object, pattern = "tether"))){
          tethers_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(str_detect(string = object, pattern = "tether")))$object_constructed)
        }else{
          tethers_sf <- NULL
        }
        
        ## cement_augmentation
        if(any(str_detect(all_objects_to_add_list$objects_df$object, pattern = "cement_augmentation"))){
          cement_augmentation_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(str_detect(string = object, pattern = "cement_augmentation")))$object_constructed)
        }else{
          cement_augmentation_sf <- NULL
        }
        
        ## OSTEOTOMIES
        if(any(str_detect(all_objects_to_add_list$objects_df$object, pattern = "grade_1"))){
          osteotomy_1_sf <- st_geometrycollection((all_objects_to_add_list$objects_df %>% filter(object == "grade_1"))$object_constructed)
        }else{
          osteotomy_1_sf <- NULL
        }
        if(any(str_detect(all_objects_to_add_list$objects_df$object, pattern = "complete_facetectomy"))){
          osteotomy_facetectomy_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "complete_facetectomy"))$object_constructed)
        }else{
          osteotomy_facetectomy_sf <- NULL
        }
        if(any(str_detect(all_objects_to_add_list$objects_df$object, pattern = "grade_2"))){
          osteotomy_2_sf <-  st_union(st_combine(st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "grade_2"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE)
        }else{
          osteotomy_2_sf <- NULL
        }
        if(any(str_detect(all_objects_to_add_list$objects_df$object, pattern = "grade_3"))){
          osteotomy_3_sf <- st_union(st_combine(st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "grade_3"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE)
          
        }else{
          osteotomy_3_sf <- NULL
        }
        if(any(str_detect(all_objects_to_add_list$objects_df$object, pattern = "grade_4"))){
          osteotomy_4_sf <- st_union(st_combine(st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "grade_4"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE) 
        }else{
          osteotomy_4_sf <- NULL
        }
        if(any(str_detect(all_objects_to_add_list$objects_df$object, pattern = "grade_5"))){
          osteotomy_5_sf <- st_union(st_combine(st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "grade_5"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE) 
        }else{
          osteotomy_5_sf <- NULL
        }
        
        
        ## Decompresions ##
        if(any(str_detect(all_objects_to_add_list$objects_df$object, pattern = "laminectomy"))){
          laminectomy_sf <- st_union(st_combine(st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "laminectomy"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE) 
        }else{
          laminectomy_sf <- NULL
        }
        if(any(str_detect(all_objects_to_add_list$objects_df$object, pattern = "sublaminar_decompression"))){
          sublaminar_decompression_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "sublaminar_decompression"))$object_constructed)
        }else{
          sublaminar_decompression_sf <- NULL
        }
        if(any(str_detect(all_objects_to_add_list$objects_df$object, pattern = "laminotomy"))){
          laminotomy_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "laminotomy"))$object_constructed)
        }else{
          laminotomy_sf <- NULL
        }
        if(any(str_detect(all_objects_to_add_list$objects_df$object, pattern = "laminoplasty"))){
          laminoplasty_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "laminoplasty"))$object_constructed)
          laminoplasty_cut_df <- tibble(x = 0.515, y =  c(max((all_objects_to_add_list$objects_df %>% filter(object == "laminoplasty"))$superior_lamina_y), min((all_objects_to_add_list$objects_df %>% filter(object == "laminoplasty"))$inferior_lamina_y) - 0.007))
        }else{
          laminoplasty_sf <- NULL
          laminoplasty_cut_df <- tibble(x = numeric(), y = numeric())
        }
        if(any(str_detect(all_objects_to_add_list$objects_df$object, pattern = "transpedicular_approach"))){
          transpedicular_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "transpedicular_approach"))$object_constructed)
        }else{
          transpedicular_sf <- NULL
        }
        if(any(str_detect(all_objects_to_add_list$objects_df$object, pattern = "costovertebral_approach"))){
          costovertebral_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "costovertebral_approach"))$object_constructed)
        }else{
          costovertebral_sf <- NULL
        }
        
        if(any(all_objects_to_add_list$objects_df$object == "diskectomy")){
          diskectomy_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "diskectomy"))$object_constructed)
        }else{
          diskectomy_sf <- NULL
        }
        
        ## INTERBODY
        if(any(all_objects_to_add_list$objects_df$object == "tlif") || any(all_objects_to_add_list$objects_df$object == "llif") ||any(all_objects_to_add_list$objects_df$object == "plif")){
          interbody_device_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "tlif" | object == "plif" | object == "llif"))$object_constructed)
        }else{
          interbody_device_sf <- NULL
        }
        
        if(any(all_objects_to_add_list$objects_df$object == "no_implant_interbody_fusion")){
          no_implant_interbody_fusion_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "no_implant_interbody_fusion"))$object_constructed)
        }else{
          no_implant_interbody_fusion_sf <- NULL
        }
        
        
        all_rods_sf <- st_geometrycollection(all_rods_reactive_list())
        
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
        
        # left_screw_sizes_df <- pedicle_screw_details_selected_reactive() %>%
        #     select(level, side, screw_size_type) %>%
        #     filter(side == "left") 
        # 
        # right_screw_sizes_df <- pedicle_screw_details_selected_reactive() %>%
        #     select(level, side, screw_size_type) %>%
        #     filter(side == "right")
        
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
          # draw_text(
          #     text = screw_sizes_df$screw_size_left,
          #     x = x_left_limit + 0.08,
          #     y = screw_sizes_df$y,
          #     size = input$label_text_size,
          #     fontface = "bold"
          # ) +
          draw_text(
            text = labels_df$level,
            x = x_right_limit - 0.05,
            y = labels_df$y,
            size = input$label_text_size,
            fontface = "bold"
          ) + 
          # draw_text(
          #     text = screw_sizes_df$screw_size_right,
          #     x = x_left_limit - 0.08,
          #     y = screw_sizes_df$y,
          #     size = input$label_text_size,
          #     fontface = "bold"
          # ) +
          geom_sf_pattern(data = cement_augmentation_sf,
                          pattern = "plasma", 
                          pattern_alpha = 0.5, 
                          alpha = 0.3,
                          color = "grey96") +
          ggpattern::geom_sf_pattern(
            data =  open_canal_sf, 
            pattern_orientation = "radial", 
            pattern = "gradient", 
            fill = "grey50", 
            pattern_fill2 = NA,
            colour = NA) +
          geom_sf(data = left_revision_implants_rod_sf, fill = "black") + 
          geom_sf(data = right_revision_implants_rod_sf, fill = "black") + 
          geom_sf(data = no_implant_interbody_fusion_sf, fill = "#E66565") +
          geom_sf(data = interbody_device_sf, fill = "red") +
          ggpattern::geom_sf_pattern(
            data = laminectomy_sf,
            pattern = "stripe", 
            pattern_colour = "red",
            alpha = 0.7,
            pattern_spacing = 0.01
          ) +
          ggpattern::geom_sf_pattern(
            data = sublaminar_decompression_sf,
            pattern = "stripe", 
            pattern_colour = "red",
            alpha = 0.7,
            pattern_spacing = 0.01
          ) +
          ggpattern::geom_sf_pattern(
            data = laminotomy_sf,
            pattern = "stripe", 
            pattern_colour = "red",
            alpha = 0.7,
            pattern_spacing = 0.01
          ) +
          ggpattern::geom_sf_pattern(
            data = transpedicular_sf,
            pattern = "stripe", 
            pattern_colour = "red",
            alpha = 0.7,
            pattern_spacing = 0.01
          ) +
          ggpattern::geom_sf_pattern(
            data = costovertebral_sf,
            pattern = "stripe", 
            pattern_colour = "red",
            alpha = 0.7,
            pattern_spacing = 0.01
          ) +
          geom_sf(data = diskectomy_sf, fill = "black", alpha = 0.7) +
          geom_sf(data = osteotomy_1_sf, color = "red", size = 1) +
          ggpattern::geom_sf_pattern(
            data = osteotomy_facetectomy_sf, 
            pattern = "stripe", 
            pattern_colour = "red", 
            alpha = 0.5,
            pattern_angle = 10,
            pattern_spacing = 0.01,
            pattern_density = 0.15,
          ) +
          ggpattern::geom_sf_pattern(
            data = osteotomy_2_sf, 
            pattern = "stripe", 
            pattern_colour = "red", 
            alpha = 0.5,
            pattern_angle = 10,
            pattern_spacing = 0.01,
            pattern_density = 0.15,
          ) +
          ggpattern::geom_sf_pattern(
            data = osteotomy_3_sf, 
            pattern = "stripe", 
            pattern_colour = "red",
            alpha = 0.6,
            pattern_angle = 10,
            pattern_spacing = 0.03,
            pattern_density = 0.1,
          ) +
          ggpattern::geom_sf_pattern(
            data = osteotomy_4_sf, 
            pattern = "stripe", 
            pattern_colour = "red",
            alpha = 0.6,
            pattern_angle = 10,
            pattern_spacing = 0.02,
            pattern_density = 0.02,
          ) +
          ggpattern::geom_sf_pattern(
            data = osteotomy_5_sf, 
            pattern = "crosshatch",
            pattern_colour = "red",
            alpha = 0.6,
            pattern_angle = 10,
            pattern_spacing = 0.02,
            pattern_density = 0.02,
          ) +
          screws_geom +
          geom_sf(data = laminoplasty_sf, fill  = "blue") + 
          geom_line(data = laminoplasty_cut_df, aes(x = x, y = y), linetype = "dotted", size = 2, color = "red") + 
          ggpattern::geom_sf_pattern(
            data = hooks_sf,
            pattern = "gradient",
            pattern_fill = "blue",
            pattern_fill2 = "#445566",
            alpha = 0.8
          ) +
          ggpattern::geom_sf_pattern(
            data = sublaminar_wires_sf,
            pattern = "gradient",
            pattern_fill = "red",
            pattern_fill2 = "#445566",
            alpha = 0.8
          ) +
          geom_sf(data = tethers_sf) +
          geom_sf(data = all_rods_sf, alpha = 0.75) +
          geom_table(data = plan_table, aes(label = tb, x = x, y = y), size = (input$label_text_size - 3)/2.85, table.colnames = FALSE) +
          ylim(input$crop_y[1], y_start_with_text) +
          xlim(x_left_limit, x_right_limit)
        
        
        
      }
    }
  })
  
  
  # 
  # 
  # 
  ##################### REACTIVE UI AND FUNCTION TO GENERATE INPUT NAMES AND RETRIEVE VALUES ##################
  ##################### REACTIVE UI AND FUNCTION TO GENERATE INPUT NAMES AND RETRIEVE VALUES ##################
  ##################### REACTIVE UI AND FUNCTION TO GENERATE INPUT NAMES AND RETRIEVE VALUES ##################
  make_screw_sizes_ui_function <-  function(level = NULL, left_screw_level = "no_screw", right_screw_level = "no_screw", left_selected = "Unknown", right_selected = "Unknown"){
    
    tags$tr(width = "100%", 
            tags$td(width = "10%", div(style = "font-size:14px; font-weight:bold; text-align:center; padding-bottom:10px",   paste(level))),
            tags$td(width = "10%", div(id = "my_small_text_input", 
                                       textInput(inputId = glue("left_{str_to_lower(level)}_screw_diameter"), 
                                                 label = NULL, 
                                                 placeholder = "D",
                                                 width = "90%"))
            ),
            tags$td(width = "10%", div(id = "my_small_text_input", 
                                       textInput(inputId = glue("left_{str_to_lower(level)}_screw_length"), 
                                                 label = NULL, 
                                                 placeholder = "L",
                                                 width = "90%"))
            ),
            tags$td(width = "10%", div(id = "my_small_text_input", 
                                       textInput(inputId = glue("right_{str_to_lower(level)}_screw_diameter"), 
                                                 label = NULL, 
                                                 placeholder = "D",
                                                 width = "90%"))
            ),
            tags$td(width = "10%", div(id = "my_small_text_input", 
                                       textInput(inputId = glue("right_{str_to_lower(level)}_screw_length"), 
                                                 label = NULL, 
                                                 placeholder = "L",
                                                 width = "90%"))
            )
    )
    
  }
  
  make_screw_types_function <-  function(level = NULL, left_screw_level = "no_screw", right_screw_level = "no_screw", left_selected = "Unknown", right_selected = "Unknown"){
    
    
    if(left_screw_level != "no_screw"){
      left_ui <- radioGroupButtons("option2",
                                   inputId = glue("left_{str_to_lower(level)}_screw_type"),
                                   label = NULL,
                                   choices = c("M", "U", "P", "Red", "Offset"),
                                   selected = left_selected,
                                   size = "xs",
                                   justified = TRUE,
                                   width = "95%"
      )
    }else{
      left_ui <- NULL
    }
    
    if(right_screw_level != "no_screw"){
      right_ui <- radioGroupButtons("option2",
                                    inputId = glue("right_{str_to_lower(level)}_screw_type"),
                                    label = NULL,
                                    choices = c("M", "U", "P", "Red", "Offset"),
                                    selected = right_selected,
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
                    div(id = "my_small_button_input",
                        left_ui)
            ),
            tags$td(width = "45%",
                    div(id = "my_small_button_input",
                        right_ui)
            )
    )
    
  }
  
  pedicle_screw_level_reactive_df <- reactive({
    
    if(nrow(all_objects_to_add_list$objects_df %>% filter(str_detect(object, "screw"))) > 1){
      all_objects_to_add_list$objects_df %>%
        mutate(level_side = paste(level, side, object, sep = "_")) %>%
        select(vertebral_number, level, side, level_side, object) %>%
        filter(str_detect(object, "screw")) %>%
        mutate(screw_size_label = str_to_lower(paste(side, level, "screw_diameter", sep = "_"))) %>%
        mutate(screw_length_label = str_to_lower(paste(side, level, "screw_length", sep = "_"))) %>%
        mutate(screw_type_label = str_to_lower(paste(side, level, "screw_type", sep = "_"))) %>%
        distinct() %>%
        arrange(vertebral_number)
    }else{
      all_objects_to_add_list$objects_df %>% filter(str_detect(object, "screw"))
    }
    
    
  })
  
  output$pedicle_screw_details_ui <- renderUI({
    
    all_implants <- all_objects_to_add_list$objects_df %>%
      select(level, vertebral_number, object, side) %>%
      distinct() %>%
      filter(str_detect(object, "screw")) %>%
      arrange(vertebral_number)
    
    
    if(nrow(all_implants) > 1){
      implants_wide_df <- all_implants %>%
        mutate(level_side = str_to_lower(paste(level, side, object, sep = "_"))) %>%
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
                                                       right_screw_level = ..3, 
                                                       left_selected = input[[left_vector[..4]]],   ## using this subsetting
                                                       right_selected = input[[right_vector[..4]]]
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
      select(level, vertebral_number, object, side) %>%
      distinct() %>%
      filter(str_detect(object, "screw")) %>%
      arrange(vertebral_number)
    
    
    if(nrow(all_implants) > 1){
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
                                                    right_screw_level = ..3, 
                                                    left_selected = input[[left_vector[..4]]],   ## using this subsetting
                                                    right_selected = input[[right_vector[..4]]])), 
               tags$tr(width = "100%", div(style = "font-size:8px; text-align:right",  "U= Uniaxial, M = Monoaxial, P = Polyaxial, Red = Reduction")),
             )
      )
    }else{
      # fixedRow()
      NULL
    }
    
    
  })
  
  
  
  # ## THIS RETRIEVES THE results from the input$____ that are created by the reactive UI. The reactive UI's use a wide dataframe but the names are the same. 
  
  pedicle_screw_details_selected_reactive <- reactive({
    if(nrow(pedicle_screw_level_reactive_df() > 1)){
      max_levels <- nrow(pedicle_screw_level_reactive_df())
      levels_count <- seq(from = 1, to = max_levels, by = 1)
      
      screw_diameter_df <- pedicle_screw_level_reactive_df() %>%
        select(level, vertebral_number, level_side, screw_size_label, screw_length_label) %>%
        mutate(screw_diameter = map(.x = levels_count, .f = ~input[[pedicle_screw_level_reactive_df()$screw_size_label[.x]]])) %>%
        unnest()
      
      screw_length_df <- pedicle_screw_level_reactive_df() %>%
        select(level, vertebral_number, level_side, screw_size_label, screw_length_label) %>%
        mutate(screw_length = map(.x = levels_count, .f = ~input[[pedicle_screw_level_reactive_df()$screw_length_label[.x]]])) %>%
        unnest()
      
      screw_type_df <- pedicle_screw_level_reactive_df() %>%
        select(level, vertebral_number, level_side, screw_type_label) %>%
        mutate(screw_type = map(.x = levels_count, .f = ~input[[pedicle_screw_level_reactive_df()$screw_type_label[.x]]])) %>%
        unnest()
      
      screw_sizes_df <- screw_diameter_df %>%
        left_join(screw_length_df) %>%
        left_join(screw_type_df) %>%
        mutate(screw_size = glue("{screw_diameter} x {screw_length}")) %>%
        mutate(side = if_else(str_detect(screw_type_label, "left"), "left", "right")) %>%
        select(level, side, screw_type, screw_size) %>%
        mutate(screw_detail = case_when(
          screw_type == "U" ~ "Uniaxial",
          screw_type == "M" ~ "Monoaxial",
          screw_type == "P" ~ "Polyaxial", 
          screw_type == "Red" ~ "Reduction", 
          screw_type == "Offset" ~ "Offset"
        )) %>%
        mutate(screw_size_type = paste(screw_size, screw_detail))
      
      
    }else{
      screw_details_df <-pedicle_screw_level_reactive_df()
    }
    
    screw_sizes_df
    # GOOD:
    # map(.x = levels_count, .f = ~input[[pedicle_screw_level_reactive_df()$level_side[.x]]])
  })
  
  
  output$pedicle_screw_details_table <- renderTable({
    pedicle_screw_details_selected_reactive()
  })  
  
  
  ########################################  RECORD A LIST OF POINTS ## ############################
  
  output$spine_plot <- renderPlot({
    spine_png2 <- spine_png
    ggdraw() +
      draw_image(
        spine_png2,
        scale = 1,
        y = 0,
        valign = 0,
        x = 0,
        width = 1
      ) +
      draw_text(
        text = labels_df$level,
        x = 0.3 - input$label_text_offset/100,
        y = labels_df$y,
        size = input$label_text_size,
        fontface = "bold"
      ) +
      draw_text(
        text = labels_df$level,
        x = 0.7 + input$label_text_offset/100,
        y = labels_df$y,
        size = input$label_text_size,
        fontface = "bold"
      ) +
      ylim(input$crop_y_coordinate_plot[1], input$crop_y_coordinate_plot[2]) +
      xlim(0.25- input$label_text_offset/100, 0.75 + input$label_text_offset/100)
    
  })
  
  point_clicks <- reactiveValues()
  
  point_clicks$click_coordinates_final_df <- tibble(coordinate_type = character(), 
                                                    side = character(),
                                                    x = numeric(),
                                                    y = numeric())
  
  
  observeEvent(input$plot_click_coordinate, {
    nearest_label <- nearPoints(
      df = reference_points_df,
      coordinfo = input$plot_click_coordinate,
      xvar = "x",
      yvar = "y",
      maxpoints = 1,
      threshold = 40
    )
    
    
    add_coordinate_df <- tibble(level = nearest_label$level, x = input$plot_click_coordinate$x, y = input$plot_click_coordinate$y) %>%
      mutate(coordinate_type = input$coordinate_type) %>%
      mutate(side = if_else(x < 0.5, "left", "right")) %>%
      select(level, coordinate_type, side, x, y, everything())
    
    point_clicks$click_coordinates_final_df <- union_all(x = point_clicks$click_coordinates_final_df, y = add_coordinate_df) %>%
      mutate(order_added = row_number())
    
  })
  
  output$coordinates_table_2 <- renderTable({
    # clicks_list
    point_clicks$click_coordinates_final_df
    
    input$save_coordinates_table_button
    isolate(write_csv(x = point_clicks$click_coordinates_final_df, file = input$file_name))
  })
  
  observeEvent(input$remove_last_coordinate, {
    point_clicks$click_coordinates_final_df <- point_clicks$click_coordinates_final_df %>%
      filter(order_added != max(order_added))
  })
  
  
  # Make the Table
  #################### MAKE THE TABLES ########################
  fusion_levels_computed_reactive_df <- reactive({
    all_fusion_implants_df <- all_objects_to_add_list$objects_df %>%
      select(level, vertebral_number, category, object) %>%
      filter(category == "implant") %>%
      filter(str_detect(string = object, pattern = "screw") | str_detect(string = object, pattern = "hook") | str_detect(string = object, pattern = "wire")) %>%
      filter(vertebral_number < 26) %>%
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
  
  
  output$objects_added_table <- renderTable({
    all_objects_to_add_list$objects_df %>%
      as_tibble() %>%
      select(level, object = object, side, x, y) %>%
      arrange(rev(y))
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
    
    plan_vector <- c("Patient:" = paste(input$patient_name, if_else(age == "", "", paste(age, "yo"), input$sex)), 
                     "Symptoms:" = toString(input$symptoms),
                     "Bone Density:" = input$bone_density,
                     "Relevant Hx:" = input$relevant_history, 
                     "---" = "---",
                     "Preop Abx:" = toString(input$preop_antibiotics), 
                     "Antifibrinolytic:" = anti_fibrinolytic,
                     "Allograft" = if_else(input$allograft_amount == 0, "--", paste(input$allograft_amount, "cc", sep = "")),
                     "BMP:" = bmp_text, 
                     "Left Rod:" = if_else(nrow(left_rod_implants_df()) > 1, paste(input$left_main_rod_size, input$left_main_rod_material, sep = " "), "--"), 
                     "Right Rod:" = if_else(nrow(right_rod_implants_df()) > 1, paste(input$right_main_rod_size, input$right_main_rod_material, sep = " "), "--"))
    
    
    enframe(plan_vector, name = "descriptor", value = "value") %>%
      filter(!is.null(value)) %>%
      filter(value != "--") %>%
      mutate(value = str_squish(string = value)) %>%
      filter(value != "")
    
  })
  
  
  
  
  
  
  ############## OPERATIVE NOTE GENERATOR ################
  
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
      additional_rods_list$left_linked <- glue("On the left side, a linked-rods construct was used, with the rods overlapping from {input$left_linked_rods_overlap[[1]]} to {input$left_linked_rods_overlap[[2]]}.")
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
      additional_rods_list$right_linked <- glue("On the right side, a linked-rods construct was used, with the rods overlapping from {input$right_linked_rods_overlap[[1]]} to {input$right_linked_rods_overlap[[2]]}.")
    }
    
    if(length(additional_rods_list) > 0){
      
      added_rods_statement <- glue_collapse(additional_rods_list, sep = " ")
      
    }else{
      added_rods_statement <- ""
    }
    
    added_rods_statement
  })
  
  
  op_note_text_reactive <- reactive({
    
    
    posterior_approach_objects_df <- all_objects_to_add_list$objects_df %>%
      filter(approach == "posterior") %>%
      left_join(interbody_df_reactive())
    
    anterior_approach_objects_df <- all_objects_to_add_list$objects_df %>%
      filter(approach == "anterior") %>%
      select(-object_constructed) %>%
      distinct() %>%
      left_join(interbody_df_reactive())
    
    procedure_results_list <- list()
    
    if(length(input$fusion_levels_confirmed)>0){
      fusions_df <- tibble(level = input$fusion_levels_confirmed) %>%
        left_join(levels_numbered_df)
    }else{
      fusions_df <- tibble(level = character()) %>%
        mutate(vertebral_number = double())
    }
    
    
    
    if(nrow(posterior_approach_objects_df) > 0){
      procedure_results_list <- op_note_posterior_function(all_objects_to_add_df = posterior_approach_objects_df,
                                                           fusion_levels_df = fusions_df,
                                                           # fusion_levels_df = fusion_levels_computed_reactive_df(),
                                                           revision_decompression_vector = input$open_canal,
                                                           left_main_rod_size = input$left_main_rod_size,
                                                           left_main_rod_material = input$left_main_rod_material,
                                                           right_main_rod_size = input$right_main_rod_size,
                                                           right_main_rod_material = input$right_main_rod_material,
                                                           additional_rods_statement = added_rods_statement_reactive(),
                                                           antibiotics = input$preop_antibiotics,
                                                           additional_procedures_vector = input$additional_procedures,
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
                                                           dressing = input$dressing_details)
      
      
    }
    
    if(nrow(anterior_approach_objects_df) > 0){
      
      
      procedure_results_list <- op_note_anterior_function(all_objects_to_add_df = anterior_approach_objects_df,
                                                          anterior_approach_laterality = "left",
                                                          acdf_interbody_height = "9mm", 
                                                          interbody_device = "cage",
                                                          microscope_statement = "none", 
                                                          antibiotics = input$preop_antibiotics, 
                                                          additional_procedures_vector = input$additional_procedures, 
                                                          bmp = if_else(input$bmp_number == 0, 0, (as.double(input$bmp_number)*as.double(input$bmp_size))), 
                                                          bone_graft_vector = input$bone_graft,
                                                          morselized_allograft = input$allograft_amount,
                                                          morselized_autograft_separate = 0,
                                                          structural_allograft_location = input$structural_allograft_location,
                                                          structural_autograft_harvest = input$structural_autograft_harvest_location,
                                                          structural_autograft_location = input$structural_autograft_location,
                                                          additional_procedural_details = input$additional_procedures, 
                                                          deep_drains = input$deep_drains,
                                                          superficial_drains = input$superficial_drains,
                                                          end_procedure_details = input$additional_end_procedure_details,
                                                          closure = input$closure_details,
                                                          dressing = input$dressing_details)
      
      
      # procedures_performed_df <- tibble(procedure = procedure_results$procedures_performed_summary_list) %>%
      #     unnest() %>%
      #     union_all(tibble(procedure = input$additional_procedures)) %>%
      #     mutate(procedure = paste(row_number(), procedure, sep = ". "))
    }
    
    
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
                                            "\nIntraoperative Complications:", 
                                            "\nSurgery Description:"), 
                                result = c(input$patient_name, 
                                           as.character(input$date_of_surgery), 
                                           input$primary_surgeon,
                                           input$surgical_assistants, 
                                           input$preoperative_diagnosis, 
                                           input$postoperative_diagnosis, 
                                           glue("{input$indications} {toString(input$symptoms)} resistant to conservative treatment"),
                                           procedure_results_list$procedures_numbered_paragraph,
                                           input$surgical_findings,
                                           input$specimens_removed,
                                           input$ebl,
                                           input$intraoperative_complications, 
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
  
  
  ################# MAKE THE REDCAP AND SUMMARY TABLE $ FUSION TABLE
  
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
    
    all_implants_df <- all_objects_to_add_list$objects_df %>%
      filter(category == "implant") %>%
      filter(str_detect(string = object, pattern = "hook") | str_detect(string = object, pattern = "screw") | str_detect(string = object, pattern = "wire"))
    
    spine_instrumented <- if_else(nrow(all_implants_df) == 0, FALSE, TRUE)
    spine_treated <- if_else(nrow(all_objects_to_add_list$objects_df) == 0, FALSE, TRUE)
    
    dos <- round(as.integer(input$date_of_surgery), 0) 
    upper_treated_vertebrae <- if_else(spine_treated == TRUE, (all_objects_to_add_list$objects_df %>% filter(vertebral_number == min(vertebral_number)))$level[1], "none")
    uiv <- if_else(spine_instrumented == TRUE, (all_implants_df %>% filter(vertebral_number == min(vertebral_number)))$level[1], "none")
    lower_treated_vertebrae <- if_else(spine_treated == TRUE, (all_objects_to_add_list$objects_df %>% filter(vertebral_number == max(vertebral_number)))$level[1], "none")
    liv <- if_else(spine_instrumented == TRUE, (all_implants_df %>% filter(vertebral_number == max(vertebral_number)))$level[1], "none")
    
    uiv_tether <- any(str_detect(string = (all_objects_to_add_list$objects_df %>% filter(level == upper_treated_vertebrae))$object, "tether"))
    uiv_hook <- any(str_detect(string = (all_objects_to_add_list$objects_df %>% filter(level == uiv))$object, "hook"))
    
    if(spine_instrumented == TRUE | spine_treated == TRUE){
      uiv_ppx_df <- all_objects_to_add_list$objects_df %>%
        filter(level == upper_treated_vertebrae | level == uiv) %>%
        filter(str_detect(string = object, pattern = "hook") | str_detect(string = object, pattern = "tether") | str_detect(string = object, pattern = "wire") | str_detect(string = object, pattern = "cement_augmentation"))
    }else{
      uiv_ppx_df <- tibble(level = character(), object = character())
    }
    
    
    levels_fused <- length(input$fusion_levels_confirmed)
    
    pelvic_fixation <- any(str_detect(string = all_implants_df$object, pattern = "pelvic"))
    
    three_column_osteotomy <- any(any(all_objects_to_add_list$objects_df$object == "grade_3") | any(all_objects_to_add_list$objects_df$object == "grade_4") | any(all_objects_to_add_list$objects_df$object == "grade_5") | any(all_objects_to_add_list$objects_df$object == "grade_6"))
    
    age <- if_else(paste(input$date_of_birth) == "1900-01-01", "--", as.character(round(interval(start = paste(input$date_of_birth), end = paste(input$date_of_surgery))/years(1), 0)))
    symptoms <- if_else(length(input$symptoms) == 0, "", toString(input$symptoms))
    bone_density <- input$bone_density
    
    bmp_mg_dose <-if_else(input$bmp_number == 0, "0", paste((as.double(input$bmp_number)*as.double(input$bmp_size))))
    
    allograft <- input$allograft
    
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
    
    decompressions_df <- all_objects_to_add_list$objects_df %>%
      filter(category == "decompression" | object == "decompression_diskectomy_fusion" | object == "disk") %>%
      select(level, object) %>%
      distinct()
    
    decompressions_vector <- if_else(nrow(decompressions_df) == 0, "None", toString(decompressions_df$object))
    
    summary_df <- tibble(attending = input$primary_surgeon,
                         name = input$patient_name,
                         date_of_surgery = as.character(input$date_of_surgery),
                         age = age,
                         sex = input$sex,
                         symptoms = symptoms, 
                         bone_density = bone_density,
                         staged_procedure = if_else(input$staged_procedure == TRUE, "Yes", "No"),
                         stage_number = input$stage_number,
                         approach = input$spine_approach,
                         approach_detail = input$approach_specified,
                         bmp_dose_mg = bmp_mg_dose,
                         upper_treated_vertebrae = upper_treated_vertebrae, 
                         uiv = uiv,
                         lower_treated_vertebrae= lower_treated_vertebrae,
                         liv = liv,
                         uiv_ppx_used = if_else(nrow(uiv_ppx_df) == 0, "No", "Yes"),
                         uiv_ppx = if_else(nrow(uiv_ppx_df) == 0, "None", toString(uiv_ppx_df$object)),
                         levels_fused = levels_fused,
                         pelvic_fixation = if_else(pelvic_fixation == TRUE, "yes", "no"),
                         three_column_osteotomy = if_else(three_column_osteotomy == TRUE, "yes", "no"),
                         left_rod = left_rod,
                         right_rod = right_rod,
                         left_supplemental_rod = left_supplemental_rods_vector,
                         right_supplemental_rod = right_supplemental_rods_vector,
                         number_of_levels_decompressed = nrow(decompressions_df),
                         decompressions_performed = decompressions_vector, 
                         operative_note = HTML(op_note_text_reactive()) 
    )
    
    
    
  })
  
  output$all_objects_table <- renderTable({
    
    enframe(objects_added_in_sequence_list$sequence_order_list) %>%
      unnest() %>%
      pivot_wider(names_from = name, values_from = value) %>%
      unnest()
    
  })
  
  output$summary_table <- renderTable({
    summary_table_for_redcap_reactive() %>%
      as.matrix() %>%
      t() %>%
      as_tibble(rownames = "variable") %>%
      rename(result = "V1")
    
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
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
