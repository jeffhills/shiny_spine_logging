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
# library(ggpmisc)
library(rclipboard)

library(DT)

rcon <- redcapConnection(url = 'https://redcap.wustl.edu/redcap/api/', token = "4B2AA28A4D6EFBC6CA1F49EC0FB385F7")


source("hook_function.R", local = TRUE)
source("screw_function.R", local = TRUE)
source("osteotomies_decompressions_functions.R", local = TRUE)
source("load_coordinates.R", local = TRUE)
source("operative_note_generator.R", local = TRUE)
source("operative_note_anterior_generator.R", local = TRUE)
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
             "#my_small_text_input .form-control {text-align:center; font-size:10px; height:auto; padding:2px 1px}"
  ),
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
          choices = c("Male", "Female", "Unknown"),
          selected = "Unknown", inline = TRUE, 
        ),
        column(12, 
               tags$table(
                 tags$tr(width = "100%",
                         tags$td(width = "50%", tags$div(style = "font-size:14px; font-weight:bold; text-align:left","Diagnosis Category:")),
                         tags$td(width = "50%",align = "right",
                                 pickerInput(
                                   inputId = "diagnosis",
                                   label = NULL,
                                   choices = c("Neoplasm", "Lesion (trauma, infection, etc)", "Deformity", "Spinal Condition (E.g. Degenerative)"),
                                   multiple = TRUE,
                                   width = "100%"
                                 ))),
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
        tags$table(
          tags$tr(width = "100%",
                  tags$td(width = "50%", tags$div(style = "font-size:14px; font-weight:bold; text-align:left","Date of Surgery:")),
                  tags$td(width = "50%",align = "right",
                          dateInput(inputId = "date_of_surgery", label = NULL, format = "mm-dd-yyyy", autoclose = TRUE)
                  )),
          tags$tr(width = "100%",
                  tags$td(width = "50%", tags$div(style = "font-size:14px; font-weight:bold; text-align:left","Primary or Revision:")),
                  tags$td(width = "50%",align = "right",
                          radioGroupButtons(
                            inputId = "primary_revision",
                            label = NULL,
                            choices = c("Primary", "Revision"),
                            justified = TRUE,
                            selected = "Primary",
                            size = "sm"
                          )
                  ))
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
                                   tags$td(width = "50%", tags$div(style = "font-size:12px; font-weight:bold; text-align:center","Left Implants:")),
                                   tags$td(width = "50%", tags$div(style = "font-size:12px; font-weight:bold; text-align:center","Right Implants:"))),
                           tags$tr(width = "100%",
                                   tags$td(width = "50%",align = "center",
                                           pickerInput(
                                             inputId = "left_revision_implants",
                                             label = NULL,
                                             choices = (revision_implants_df %>% filter(x < 0.5))$level,
                                             multiple = TRUE,
                                             width = "100%"
                                           )),
                                   tags$td(width = "50%",align = "center",
                                           pickerInput(
                                             inputId = "right_revision_implants",
                                             label = NULL,
                                             choices = (revision_implants_df %>% filter(x > 0.5))$level,
                                             multiple = TRUE,
                                             width = "100%"
                                           )))
                         )
        ),
        tags$hr(),
        column(12, #style='background-color:#f2f2f2;min-width: 300px;',
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
        ),
        # column(12,
        #        tags$table(
        #            tags$tr(width = "100%",
        #                    tags$td(width = "60%", tags$div(style = "font-size:14px; font-weight:bold; text-align:left","Allograft:")),
        #                    tags$td(width = "40%",align = "right",
        #                            numericInput(inputId = "allograft", label = NULL, value = NULL, min = 0, max = 500, step = 10, width = "100%"))),
        #            tags$tr(width = "100%",
        #                    tags$td(width = "60%", div(style = "font-size:14px; font-weight:bold; text-align:left", "BMP Kits:")),
        #                    tags$td(width = "40%", 
        #                            numericInput(inputId = "bmp_number", label = NULL, value = 0, min = 0, max = 20, step = 1, width = "100%"))),
        #        ),
        #        conditionalPanel(condition = "input.bmp_number > 0",
        #                         radioGroupButtons(
        #                             inputId = "bmp_size",
        #                             label = NULL,
        #                             choices = c("XXS" = 1.05, "XS" = 2.1, "Sm" = 4.2, "M" = 8.4, "L" = 12),
        #                             size = "sm",
        #                             selected = 12,
        #                             checkIcon = list(
        #                                 yes = tags$i(class = "fa fa-check-square", 
        #                                              style = "color: steelblue"),
        #                                 no = tags$i(class = "fa fa-square-o", 
        #                                             style = "color: steelblue"))
        #                         )
        #        )
        # )
      )
    ),
    mainPanel(width = 9,
              tabsetPanel(type = "tabs",
                          tabPanel(title = "Spine Plan",
                                   column(
                                     width = 2,
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
                                     dropdown(icon = icon("grip-lines-vertical"), 
                                              width = "250%",
                                              size = "xs",
                                              label = "Add or Modifiy Rod Details",
                                              style = "unite",    
                                              fixedRow(
                                                column(width = 6,
                                                       strong("Left Primary Rod:"),
                                                       radioGroupButtons(
                                                         inputId = "left_main_rod_size",size = "xs", justified = TRUE, direction = "vertical",
                                                         label = "Size:",
                                                         choices = c("None", "3.5mm", "4.5mm", "4.75mm", "5.5mm", "6.0mm", "6.34mm/quarter in"),
                                                         selected = "None"
                                                       ),
                                                       radioGroupButtons(
                                                         inputId = "left_main_rod_material", size = "xs", justified = TRUE,direction = "vertical",
                                                         label = "Material:",
                                                         choices = c("Non-instrumented", "Titanium", "Cobalt Chrome", "Stainless Steel"), 
                                                         selected = "Non-instrumented"
                                                       )
                                                ), 
                                                column(width = 6,
                                                       strong("Right Primary Rod:"),
                                                       radioGroupButtons(
                                                         inputId = "right_main_rod_size", size = "xs", justified = TRUE,direction = "vertical",
                                                         label = "Size:",
                                                         choices = c("None", "Transition", "3.5mm", "4.5mm", "4.75mm", "5.5mm", "6.0mm", "6.34mm/quarter in"),
                                                         selected = "None"
                                                       ),
                                                       radioGroupButtons(
                                                         inputId = "right_main_rod_material",size = "xs", justified = TRUE,direction = "vertical",
                                                         label = "Material:",
                                                         choices = c("Non-instrumented", "Titanium", "Cobalt Chrome", "Stainless Steel"), 
                                                         selected = "Non-instrumented"
                                                       )
                                                )
                                              ),
                                              hr(),
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
                                              ),
                                     ), 
                                     hr(),
                                     dropdown(icon = icon("screwdriver"),
                                              size = "xs", 
                                              width = "400%",
                                              label = "Add Pedicle Screw Details",
                                              style = "unite", 
                                              uiOutput(outputId = "pedicle_screw_details_ui")
                                     ),
                                     br(),
                                     hr(),
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
                                     ),
                                     hr(),
                                     br(),
                                     br(), 
                                     uiOutput(outputId = "bone_graft_used_ui"),
                                     conditionalPanel(condition = "input.bone_graft_used == true",
                                                      checkboxGroupButtons(
                                                        inputId = "bone_graft",
                                                        label = "Select any bone graft used:",
                                                        justified = TRUE, individual = TRUE,
                                                        width = "100%",
                                                        size = "xs",
                                                        direction = "vertical",
                                                        choices = c("Morselized Allograft",
                                                                    "Local Autograft",
                                                                    "Morselized Autograft (separate fascial incision)",
                                                                    "Structural Allograft", 
                                                                    "Structural Autograft"),
                                                        selected = "Morselized Allograft",
                                                        checkIcon = list(
                                                          yes = tags$i(class = "fa fa-check-square", 
                                                                       style = "color: steelblue"),
                                                          no = tags$i(class = "fa fa-square-o", 
                                                                      style = "color: steelblue"))
                                                      )
                                     ),
                                     conditionalPanel(condition = "input.bone_graft_used == true",
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
                                       condition = "input.bone_graft.indexOf('Morselized Allograft') > -1",
                                       numericInput(inputId = "allograft_amount", 
                                                    label = "Allograft (cc):", 
                                                    value = NULL, 
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
                                     width = 10,
                                     align = "center",
                                     column(width = 1, 
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
                                     column(8, 
                                            tags$table(
                                              tags$tr(width = "100%",
                                                      tags$td(width = "80%", tags$div(style = "font-size:14px; font-weight:bold; text-align:left","Select Approach:")),
                                                      tags$td(width = "20%", 
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
                                                                  value = -6
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
                                                              ))
                                              ),
                                              tags$tr(width = "100%",
                                                      tags$td(width = "100%", 
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
                                                              ))
                                              ),
                                              tags$tr(width = "100%",
                                                      tags$td(width = "100%",
                                                              conditionalPanel(
                                                                condition = "input.spine_approach.indexOf('Anterior') > -1",
                                                                radioGroupButtons(inputId = "anterior_approach_specified",
                                                                                  label = NULL,
                                                                                  choices = c("Anterior/Lateral", 
                                                                                              "Transthoracic", 
                                                                                              "Throacolumbar", 
                                                                                              "Retroperitoneal"),
                                                                                  selected = "Anterior/Lateral",  
                                                                                  width = "100%")
                                                              ),
                                                              conditionalPanel(
                                                                condition = "input.spine_approach.indexOf('Posterior') > -1",
                                                                radioGroupButtons(inputId = "anterior_approach_specified",
                                                                                  label = NULL, 
                                                                                  choices = c("Open",
                                                                                              "Minimally Invasive", 
                                                                                              "Percutaneous", 
                                                                                              "Endoscopic"),   
                                                                                  selected = "Open",
                                                                                  width = "100%")
                                                              )
                                                      ))
                                            ),
                                            h4("Add procedures & implants in the order they were performed."),
                                            plotOutput("spine_plan",
                                                       height = 750,
                                                       click = "plot_click", 
                                                       dblclick = "plot_double_click"),
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
                                              checkIcon = list(
                                                yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                                              ),
                                              label = "Select & then Click Spine to Add to Plan",
                                              choices = c("")
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
                                          h4("Pedicle_screws2"),
                                          tableOutput("pedicle_screw_sizes_editable_table")
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
                                                                           "Application of Cranial Tongs", 
                                                                           "Application of Halo", 
                                                                           "Application of Halo for thin skull osteology (e.g. pediatric)",
                                                                           "Removal of tongs or Halo applied by another inidividual",
                                                                           "Irrigation and Debridement",
                                                                           "Open Biopsy of vertebral body",
                                                                           "Repair of dural/CSF leak",
                                                                           "Dural Graft",
                                                                           "Removal of spinal instrumentation",
                                                                           "Exploration of spinal prior fusion"),
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
                                                               choices = c("Decompression for Spondylolisthesis"))
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
                                   ))
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
                              inputId = "object_to_add", 
                              choices = c(
                                "Disk Arthroplasty" = "disk_arthroplasty",
                                "Diskectomy & Fusion + Interbody object" = "diskectomy_fusion",
                                "Diskectomy & Fusion (No object)" = "diskectomy_fusion_no_interbody_device",
                                "Corpectomy" = "corpectomy",
                                "Corpectomy Cage" = "corpectomy_cage",
                                "Anterior Plate" = "anterior_plate",
                                "Anterior Buttress Plate" = "anterior_buttress_plate",
                                "Screw +/Washer" = "screw_washer"
                              ),
                              selected = "diskectomy_fusion"
      )
    }else{
      updateRadioGroupButtons(session = session, 
                              inputId = "object_to_add",
                              choices = "")
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
                              # "Foraminotomy" = "foraminotomy",
                              "Laminotomy (Hemilaminectomy)" = "laminotomy",
                              "Diskectomy" = "diskectomy",
                              "Transpedicular Decompression" = "transpedicular_approach",
                              "Costovertebral Decompression" = "costovertebral_approach"
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
                            selected = "TLIF"
    )
  })
  
  output$currently_adding <- renderText(paste("Currently Adding: ", str_to_title(string = str_replace_all(string = input$object_to_add, pattern = "_", replacement = " "))), sep = "")
  
  
  ###  RODS
  
  
  observeEvent(left_main_rod_reactive_sf(), {
    if(nrow(left_rod_implants_df()) > 1){
      updateSliderTextInput(session = session, 
                            inputId = "left_main_rod_size", 
                            choices = c("Transition", "3.5mm", "4.5mm", "4.75mm", "5.5mm", "6.0mm", "6.34mm/quarter in"),
                            selected = "5.5mm"
      )
      updateSliderTextInput(session = session, 
                            inputId = "left_main_rod_material", 
                            choices = c("Titanium", "Cobalt Chrome", "Stainless Steel"),
                            selected = "Titanium"
      )
    }
  })
  observeEvent(right_main_rod_reactive_sf(), {
    if(nrow(right_rod_implants_df()) > 1){
      updateSliderTextInput(session = session, 
                            inputId = "right_main_rod_size", 
                            choices = c("Transition", "3.5mm", "4.5mm", "4.75mm", "5.5mm", "6.0mm", "6.34mm/quarter in"),
                            selected = "5.5mm"
      )
      updateSliderTextInput(session = session, 
                            inputId = "right_main_rod_material", 
                            choices = c("Titanium", "Cobalt Chrome", "Stainless Steel"),
                            selected = "Titanium"
      )
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
    if(input$add_left_satellite_rod == TRUE){
      updateSliderTextInput(session = session, 
                            inputId = "left_satellite_rod", 
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
  
  observeEvent({list(right_rod_implants_df(), input$add_right_accessory_rod, input$add_right_satellite_rod, input$add_right_intercalary_rod, input$add_right_linked_rods)}, {
    middle_selected_vector <- (right_rod_implants_df() %>% filter(between(vertebral_number, median(vertebral_number)-1, median(vertebral_number + 1))))$level
    
    if(input$add_right_accessory_rod == TRUE){
      updateSliderTextInput(session = session, 
                            inputId = "right_accessory_rod", 
                            choices = right_rod_implants_df()$level,
                            selected = c(middle_selected_vector[1], middle_selected_vector[2])
      )
    }
    if(input$add_right_satellite_rod == TRUE){
      updateSliderTextInput(session = session, 
                            inputId = "right_satellite_rod", 
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
  
  output$bone_graft_used_ui <- renderUI({
    if(nrow(fusion_levels_computed_reactive_df()) > 0){
      switchInput(
        inputId = "bone_graft_used",width = '100%', inline = TRUE, 
        label = "Graft:",
        onLabel = "Yes",
        offLabel = "No", 
        value = TRUE, 
        size = "mini",
      )
    }else{
      switchInput(
        inputId = "bone_graft_used", width = '100%', inline = TRUE,
        label = "Graft:",
        onLabel = "Yes",
        offLabel = "No",
        value = FALSE,
        size = "mini",
      )
    }
  })
  
  
  #################################################################### CONSTRUCT IMPLANTS, DECOMPRESSIONS AND PLOT     ####################################################################
  
  # output$click_info <- renderPrint({
  #     cat("input$plot_click:\n")
  #     str(input$plot_click)
  # })
  
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
    
    implant_df
  })
  
  #### OBSERVE THE PLOT CLICK AND ADD APPROPRIATE OBJECT TO A LIST IN THE ORDER IT WAS ADDED ####
  
  objects_added_in_sequence_list <- reactiveValues()
  
  objects_added_in_sequence_list$sequence_order_list <- list()
  
  # objects_added_in_sequence_list$sequence_order <- tibble(level = character(),
  #                                              approach = character(),
  #                                              category = character(),
  #                                              vertebral_number = double(),
  #                                              object = character(),
  #                                              side = character()
  # )
  
  observeEvent(input$plot_click, {
    
    object_added_df <- object_added_reactive_df() %>%
      select(level, side, object)
    
    objects_added_in_sequence_list$sequence_order_list <- append(objects_added_in_sequence_list$sequence_order_list, object_added_df)
    
  }
  )
  
  
  
  #### OBSERVE THE PLOT CLICK AND ADD APPROPRIATE object ####
  observeEvent(input$plot_click, {
    
    all_objects_to_add_list$objects_df <- object_added_reactive_df() %>%
      union_all(all_objects_to_add_list$objects_df) %>%
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
      
      if(any(str_detect(string = all_objects_to_add_list$objects_df$object, pattern = "diskectomy_fusion"))){
        
        anterior_fusions_selected_df <- all_objects_to_add_list$objects_df %>% 
          filter(object == "diskectomy_fusion_no_interbody_device" | object == "diskectomy_fusion") %>%
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
        ggpmisc::geom_table(data = plan_table, aes(label = tb, x = x, y = y), size = (input$label_text_size - 3)/2.85, table.colnames = FALSE) +
        # geom_sf(data = text_square, fill = "grey98") +
        # draw_text(text = plan_df$descriptor, x = x_left_limit + 0.01, y = plan_df$y, size = input$label_text_size - 2, hjust = 0, vjust = 0.5) +
        # draw_text(text = str_wrap(string = plan_df$value, width = 40 +input$label_text_offset, exdent = 5), x = x_left_limit + 0.12, y = plan_df$y, size = input$label_text_size - 2, hjust = 0, vjust = 0.5, lineheight = 0.7) +
        ylim(input$crop_y[1], y_start_with_text) +
        xlim(x_left_limit, x_right_limit)
      
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
        
        # screw_sizes_df <- pedicle_screw_details_selected_reactive() %>%
        #     select(level, screw_size_label, screw_size) %>%
        #     mutate(side = if_else(str_detect(screw_size_label, pattern = "right"), "right", "left")) %>%
        #     select(level, side, screw_size) %>%
        #     pivot_wider(names_from = side, names_prefix = "screw_size", values_from = screw_size) %>%
        #     full_join(labels_df)
        
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
          ggpmisc::geom_table(data = plan_table, aes(label = tb, x = x, y = y), size = (input$label_text_size - 3)/2.85, table.colnames = FALSE) +
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
                                       textInput(inputId = glue("left_{level}_screw_diameter"), 
                                                 label = NULL, 
                                                 placeholder = "D",
                                                 width = "90%"))
            ),
            tags$td(width = "10%", div(id = "my_small_text_input", 
                                       textInput(inputId = glue("left_{level}_screw_length"), 
                                                 label = NULL, 
                                                 placeholder = "L",
                                                 width = "90%"))
            ),
            tags$td(width = "10%", div(id = "my_small_text_input", 
                                       textInput(inputId = glue("right_{level}_screw_diameter"), 
                                                 label = NULL, 
                                                 placeholder = "D",
                                                 width = "90%"))
            ),
            tags$td(width = "10%", div(id = "my_small_text_input", 
                                       textInput(inputId = glue("right_{level}_screw_length"), 
                                                 label = NULL, 
                                                 placeholder = "L",
                                                 width = "90%"))
            )
    )
    
  }
  
  make_screw_types_function <-  function(level = NULL, left_screw_level = "no_screw", right_screw_level = "no_screw", left_selected = "Unknown", right_selected = "Unknown"){
    
    
    if(left_screw_level != "no_screw"){
      left_ui <- radioGroupButtons("option2",
                                   inputId = left_screw_level,
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
                                    inputId = right_screw_level,
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
  
  ####
  # make_ui_rows <-  function(level = NULL, left_screw_level = "no_screw", right_screw_level = "no_screw", left_selected = "Unknown", right_selected = "Unknown"){
  #     
  # 
  #     if(left_screw_level != "no_screw"){
  #         left_ui <- radioGroupButtons("option2",
  #             inputId = left_screw_level,
  #             label = NULL,
  #             choices = c("M", "U", "P", "Red", "Offset"),
  #             selected = left_selected,
  #             size = "xs",
  #             justified = TRUE,
  #             width = "95%"
  #         )
  #     }else{
  #         left_ui <- NULL
  #     }
  #     
  #     if(right_screw_level != "no_screw"){
  #         right_ui <- radioGroupButtons("option2",
  #             inputId = right_screw_level,
  #             label = NULL,
  #             choices = c("M", "U", "P", "Red", "Offset"),
  #             selected = right_selected,
  #             justified = TRUE, 
  #             width = "95%",
  #             size = "xs"
  #         )
  #     }else{
  #         right_ui <- NULL
  #     }
  # 
  #     tags$tr(width = "100%", 
  #             tags$td(width = "7%", div(style = "font-size:14px; font-weight:bold; text-align:center; padding-bottom:10px",   paste(level))),
  #             tags$td(width = "9%", div(id = "my_small_text_input", 
  #                                       textInput(inputId = glue("left_{level}_screw_size"), 
  #                                             label = NULL, 
  #                                             placeholder = "D x L",
  #                                             width = "90%"))
  #                     ),
  #             tags$td(width = "37.5%",
  #                     div(id = "my_small_button_input",
  #                         left_ui)
  #                     ),
  #             tags$td(width = "9%", div(id = "my_small_text_input", 
  #                                       textInput(inputId = glue("right_{level}_screw_size"), 
  #                                                 label = NULL, 
  #                                                 placeholder = "D x L",
  #                                                 width = "90%"))
  #                     ),
  #             tags$td(width = "37.5%",
  #                     div(id = "my_small_button_input",
  #                         right_ui)
  #             )
  #     )
  #     
  # }
  # 
  
  pedicle_screw_level_reactive_df <- reactive({
    
    if(nrow(all_objects_to_add_list$objects_df %>% filter(object == "pedicle_screw")) > 1){
      all_objects_to_add_list$objects_df %>%
        mutate(level_side = paste(level, side, object, sep = "_")) %>%
        select(vertebral_number, level, side, level_side, object) %>%
        filter(object == "pedicle_screw") %>%
        mutate(screw_size_label = paste(side, level, "screw_size", sep = "_")) %>%
        distinct() %>%
        arrange(vertebral_number)
    }else{
      all_objects_to_add_list$objects_df %>% filter(object == "pedicle_screw")
    }
    
    
  })
  
  # output$pedicle_screw_details_ui <- renderUI({
  #     
  #     all_implants <- all_objects_to_add_list$objects_df %>%
  #         select(level, vertebral_number, object, side) %>%
  #         distinct() %>%
  #         filter(object == "pedicle_screw") %>%
  #         arrange(vertebral_number)
  #     
  #     if(nrow(all_implants) > 1){
  #         implants_wide_df <- all_implants %>%
  #             mutate(level_side = paste(level, side, object, sep = "_")) %>%
  #             select(level, level_side, side) %>%
  #             pivot_wider(names_from = side, values_from = level_side) %>%
  #             mutate(across(everything(), ~ replace_na(.x, "no_screw"))) 
  #         
  #         df_names <- glue_collapse(names(implants_wide_df), sep = " ")
  #         
  #         level_vector <- implants_wide_df$level
  #         
  #         ## the data frame can't have any missing values, otherwise the vectors will be of different lengths when you run map()
  #         
  #         if(any(names(implants_wide_df) == "left")){
  #             left_vector <- implants_wide_df$left
  #         }else{
  #             left_vector <-(implants_wide_df %>% mutate(left = "no_screw"))$left
  #         }
  #         if(any(names(implants_wide_df) == "right")){
  #             right_vector <- implants_wide_df$right
  #         }else{
  #             right_vector <-(implants_wide_df %>% mutate(right = "no_screw"))$right
  #         }
  #         
  #         max_levels <- nrow(implants_wide_df)     
  #         levels_count <- seq(from = 1, to = max_levels, by = 1)
  #         
  #         tags$table(
  #             tags$tr(width = "100%",
  #                     tags$th(width = "7%", div(style = "font-size:14px; font-weight:bold; text-align:center",  "Level")),
  #                     tags$th(width = "9%", div(style = "font-size:14px; font-weight:bold; text-align:center",  "Size")),
  #                     tags$th(width = "37.5%", div(style = "font-size:14px; font-weight:bold; text-align:center",  "Left")),
  #                     tags$th(width = "9%", div(style = "font-size:14px; font-weight:bold; text-align:center",  "Size")),
  #                     tags$th(width = "37.5%", div(style = "font-size:14px; font-weight:bold; text-align:center",  "Right"))
  #             ),
  #             pmap(.l = list(..1 = level_vector,
  #                            ..2 = left_vector,
  #                            ..3 = right_vector, 
  #                            ..4 = levels_count),
  #                  .f = ~make_ui_rows(level = ..1, 
  #                                     left_screw_level = ..2, 
  #                                     right_screw_level = ..3, 
  #                                     left_selected = input[[left_vector[..4]]],   ## using this subsetting
  #                                     right_selected = input[[right_vector[..4]]])), 
  #             tags$tr(width = "100%", div(style = "font-size:8px; text-align:right",  "U= Uniaxial, M = Monoaxial, P = Polyaxial, Red = Reduction")),
  #         )
  # 
  #     }else{
  #         # fixedRow()
  #         NULL
  #     }
  #     
  #     
  # })
  
  output$pedicle_screw_details_ui <- renderUI({
    
    all_implants <- all_objects_to_add_list$objects_df %>%
      select(level, vertebral_number, object, side) %>%
      distinct() %>%
      filter(object == "pedicle_screw") %>%
      arrange(vertebral_number)
    
    if(nrow(all_implants) > 1){
      implants_wide_df <- all_implants %>%
        mutate(level_side = paste(level, side, object, sep = "_")) %>%
        select(level, level_side, side) %>%
        pivot_wider(names_from = side, values_from = level_side) %>%
        mutate(across(everything(), ~ replace_na(.x, "no_screw"))) 
      
      # lengths_wide_df <- all_implants %>%
      #     mutate(level_length = paste(level, side, object, sep = "_")) %>%
      #     select(level, level_side, side) %>%
      #     pivot_wider(names_from = side, values_from = level_length) %>%
      #     mutate(across(everything(), ~ replace_na(.x, "no_screw"))) 
      
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
             ),
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
             ),
      )
      
    }else{
      # fixedRow()
      NULL
    }
    
    
  })
  
  screw_sizes <- reactiveValues()
  
  
  
  observe({
    screw_sizes$screw_sizes_df <-  all_objects_to_add_list$objects_df %>%
      filter(object == "pedicle_screw") %>%
      arrange(vertebral_number) %>%
      select(Level = level) %>%
      distinct() %>%
      mutate('Left Diameter' = "",
             'Left Length' = "",
             'Right Diameter' = "",
             'Right Length' = "")
  }
  )
  
  
  
  # 
  # output$pedicle_screw_sizes_dt_ui <- renderUI({
  #     DTOutput(outputId = "pedicle_screw_sizes_dt")
  # })
  
  
  # output$pedicle_screw_sizes_dt <- renderDataTable({
  #         datatable(data = screw_sizes$screw_sizes_input_df,
  #               editable = list(target = "all",
  #                               disable = list(columns = c(0))),
  #               rownames = FALSE, 
  #               options = list("searching" = FALSE, 
  #                              paging = FALSE))
  #         
  # })
  
  # observeEvent(input$pedicle_screw_sizes_dt_cell_edit, {
  #     screw_sizes$screw_sizes_df[input$pedicle_screw_sizes_dt_cell_edit$row,input$pedicle_screw_sizes_dt_cell_edit$col] <<- input$pedicle_screw_sizes_dt_cell_edit$value
  # })
  
  # ## THIS RETRIEVES THE results from the input$____ that are created by the reactive UI. The reactive UI's use a wide dataframe but the names are the same. 
  
  pedicle_screw_details_selected_reactive <- reactive({
    if(nrow(pedicle_screw_level_reactive_df() > 1)){
      max_levels <- nrow(pedicle_screw_level_reactive_df())
      levels_count <- seq(from = 1, to = max_levels, by = 1)
      
      screw_sizes_df <- pedicle_screw_level_reactive_df() %>%
        select(level, vertebral_number, level_side, screw_size_label) %>%
        mutate(screw_size = map(.x = levels_count, .f = ~input[[pedicle_screw_level_reactive_df()$screw_size_label[.x]]])) %>%
        unnest()
      
      screw_details_df <- pedicle_screw_level_reactive_df() %>%
        select(level, vertebral_number, level_side) %>%
        mutate(screw_details = map(.x = levels_count, .f = ~input[[pedicle_screw_level_reactive_df()$level_side[.x]]])) %>%
        unnest() %>%
        left_join(screw_sizes_df)
      
    }else{
      screw_details_df <-pedicle_screw_level_reactive_df()
    }
    
    screw_details_df
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
    
    fusion_levels_estimated
  })
  
  
  output$objects_added_table <- renderTable({
    all_objects_to_add_list$objects_df %>%
      as_tibble() %>%
      select(level, object = object, side, x, y) %>%
      arrange(rev(y))
  })
  
  output$upload_df <- renderTable({
    data_wide <- all_objects_to_add_list$objects_df %>%
      as_tibble() %>%
      select(-object_constructed) %>%
      union_all(fusion_levels_computed_reactive_df()) %>%
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
    bmp_text <- if_else(input$bmp_number == 0 | is.null(input$bmp_number == 0), "None", paste(input$bmp_number, bmp_size, "(", as.character(bmp_mg_dose), "mg)", sep = ""))
    bmp <- if_else(bmp_text == "None", "None", paste(bmp_text, "(", as.character(bmp_mg_dose), "mg)"))
    
    age <- if_else(paste(input$date_of_birth) == "1900-01-01", "", as.character(round(interval(start = paste(input$date_of_birth), end = paste(input$date_of_surgery))/years(1), 0)))
    
    plan_vector <- c("Patient:" = paste(input$patient_name, if_else(age == "", "", paste(age, "yo"), input$sex)), 
                     "Symptoms:" = toString(input$symptoms),
                     "Bone Density:" = input$bone_density,
                     "Relevant Hx:" = input$relevant_history, 
                     "---" = "---",
                     "Preop Abx:" = toString(input$preop_antibiotics), 
                     "Antifibrinolytic:" = anti_fibrinolytic,
                     "Allograft" = if_else(input$allograft == 0, "None", paste(input$allograft, "cc", sep = "")),
                     "BMP:" = bmp_text, 
                     "Left Rod:" = if_else(nrow(left_rod_implants_df()) > 1, paste(input$left_main_rod_size, input$left_main_rod_material, sep = " "), "--"), 
                     "Right Rod:" = if_else(nrow(right_rod_implants_df()) > 1, paste(input$right_main_rod_size, input$right_main_rod_material, sep = " "), "--"))
    
    
    enframe(plan_vector, name = "descriptor", value = "value") %>%
      filter(!is.null(value)) %>%
      filter(value != "--") %>%
      # mutate(value = str_trim(value, side = "both")) %>%
      mutate(value = str_squish(string = value)) %>%
      filter(value != "")
    
  })
  
  
  ################# MAKE THE REDCAP AND SUMMARY TABLE $ FUSION TABLE
  
  
  
  
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
    
    
    levels_fused <- if_else(spine_instrumented == FALSE, 0, max(all_implants_df$vertebral_number) - min(all_implants_df$vertebral_number) + 1)
    pelvic_fixation <- any(str_detect(string = all_implants_df$object, pattern = "pelvic"))
    three_column_osteotomy <- any(str_detect(string = all_implants_df$object, pattern = "grade_3") | str_detect(string = all_implants_df$object, pattern = "grade_4")  | str_detect(string = all_implants_df$object, pattern = "grade_5") )
    
    age <- if_else(paste(input$date_of_birth) == "1900-01-01", "--", as.character(round(interval(start = paste(input$date_of_birth), end = paste(input$date_of_surgery))/years(1), 0)))
    symptoms <- if_else(length(input$symptoms) == 0, "", toString(input$symptoms))
    bone_density <- input$bone_density
    
    bmp_mg_dose <-if_else(input$bmp_number == 0, "0", paste((as.double(input$bmp_number)*as.double(input$bmp_size))))
    # bmp_mg_dose <- if_else(is.null(input$input$bmp_size), 0, (as.double(input$bmp_size))*(as.double(input$bmp_number)))
    
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
      filter(category == "decompression") %>%
      select(level, object) %>%
      distinct()
    
    decompressions_vector <- if_else(nrow(decompressions_df) == 0, "None", toString(decompressions_df$object))
    
    summary_df <- tibble(name = input$patient_name,
                         date_of_surgery = as.character(input$date_of_surgery),
                         age = age,
                         sex = input$sex,
                         symptoms = symptoms, 
                         bone_density = bone_density,
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
                         decompressions_performed = decompressions_vector
    )
    
    
    
  })
  
  output$all_objects_table <- renderTable({
    
    enframe(objects_added_in_sequence_list$sequence_order_list) %>%
      unnest() %>%
      pivot_wider(names_from = name, values_from = value) %>%
      unnest()
    
    # all_objects_to_add_list$objects_df %>%
    #     select(-object_constructed) %>%
    #     union_all(fusion_levels_computed_reactive_df())
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
  
  output$pedicle_screw_sizes_editable_table <- renderTable({
    screw_sizes$screw_sizes_df
  })
  
  
  
  
  ############## OPERATIVE NOTE GENERATOR ################
  
  added_rods_statement_reactive <- reactive({
    additional_rods_list <- list()
    
    if(input$add_left_accessory_rod == TRUE){
      additional_rods_list$left_accessory <- glue("On the left side, an accessory rod was connected from {input$left_accessory_rod[[1]]} to {input$left_accessory_rod[[2]]} to increase the overall stiffness of the construct. ")
    }
    if(input$add_left_satellite_rod == TRUE){
      additional_rods_list$left_satellite <- glue("On the left side, a satellite rod construct was utilized, with the satellite rod spanning {input$left_satellite_rod[[1]]} to {input$left_satellite_rod[[2]]}. ")
      
    }
    if(input$add_left_intercalary_rod == TRUE){
      additional_rods_list$left_intercalary <- glue("On the left side, an intercalary rod construct was utilized, with the intercalary rod spanning {input$left_intercalary_rod[[1]]} to {input$left_intercalary_rod[[2]]}. ")
      
    }
    if(input$add_left_linked_rods == TRUE){
      additional_rods_list$left_linked <- glue("On the left side, a linked-rods construct was used, with the rods overlapping from {input$left_linked_rods_overlap[[1]]} to {input$left_linked_rods_overlap[[2]]}. ")
    }
    
    if(input$add_right_accessory_rod == TRUE){
      additional_rods_list$right_accessory <- glue("On the right side, an accessory rod was connected from {input$right_accessory_rod[[1]]} to {input$right_accessory_rod[[2]]} to increase the overall stiffness of the construct. ")
    }
    if(input$add_right_satellite_rod == TRUE){
      additional_rods_list$right_satellite <- glue("On the right side, a satellite rod construct was utilized, with the satellite rod spanning {input$right_satellite_rod[[1]]} to {input$right_satellite_rod[[2]]}. ")
      
    }
    if(input$add_right_intercalary_rod == TRUE){
      additional_rods_list$right_intercalary <- glue("On the right side, an intercalary rod construct was utilized, with the intercalary rod spanning {input$right_intercalary_rod[[1]]} to {input$right_intercalary_rod[[2]]}. ")
      
    }
    if(input$add_right_linked_rods == TRUE){
      additional_rods_list$right_linked <- glue("On the right side, a linked-rods construct was used, with the rods overlapping from {input$right_linked_rods_overlap[[1]]} to {input$right_linked_rods_overlap[[2]]}. ")
    }
    
    
    
    if(length(additional_rods_list) > 0){
      
      added_rods_statement <- glue_collapse(additional_rods_list, sep = "")
      
    }else{
      added_rods_statement <- ""
    }
    
    added_rods_statement
  })
  
  op_note_text_reactive <- reactive({
    
    posterior_approach_objects_df <- all_objects_to_add_list$objects_df %>%
      filter(approach == "posterior")
    
    anterior_approach_objects_df <- all_objects_to_add_list$objects_df %>%
      filter(approach == "anterior")
    
    
    if(nrow(posterior_approach_objects_df) > 0){
      procedure_results <- full_posterior_op_note_generator_function(all_objects_to_add_df = all_objects_to_add_list$objects_df,
                                                                     fusion_levels_df = fusion_levels_computed_reactive_df(), 
                                                                     open_canal_vector = input$open_canal,
                                                                     left_main_rod_size = input$left_main_rod_size,
                                                                     left_main_rod_material = input$left_main_rod_material,
                                                                     right_main_rod_size = input$right_main_rod_size,
                                                                     right_main_rod_material = input$right_main_rod_material,
                                                                     additional_rods_statement = added_rods_statement_reactive(),
                                                                     antibiotics = input$preop_antibiotics, 
                                                                     bmp = if_else(input$bmp_number == 0, 0, (as.double(input$bmp_number)*as.double(input$bmp_size))),
                                                                     allograft = input$allograft,
                                                                     additional_procedural_details = input$additional_procedures,
                                                                     deep_drains = input$deep_drains, 
                                                                     superficial_drains = input$superficial_drains, 
                                                                     end_procedure_details = input$additional_end_procedure_details,
                                                                     closure = input$closure_details)
      
      procedures_performed_df <- tibble(procedure = procedure_results$procedures_performed_summary_list) %>%
        unnest() %>%
        union_all(tibble(procedure = input$additional_procedures)) %>%
        mutate(procedure = paste(row_number(), procedure, sep = ". "))
    }
    
    if(nrow(anterior_approach_objects_df) > 0){
      procedure_results <- full_anterior_op_note_generator_function(all_objects_to_add_df = all_objects_to_add_list$objects_df,
                                                                    anterior_approach_laterality = "left", 
                                                                    deep_drains = input$deep_drains, 
                                                                    superficial_drains = input$superficial_drains, 
                                                                    end_procedure_details = input$additional_end_procedure_details,
                                                                    closure = input$closure_details)
      
      procedures_performed_df <- tibble(procedure = procedure_results$procedures_performed_summary_list) %>%
        unnest() %>%
        union_all(tibble(procedure = input$additional_procedures)) %>%
        mutate(procedure = paste(row_number(), procedure, sep = ". "))
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
                                           glue_collapse(procedures_performed_df$procedure, sep = "\n"),
                                           input$surgical_findings,
                                           input$specimens_removed,
                                           input$ebl,
                                           input$intraoperative_complications, 
                                           glue_collapse(x = procedure_results$procedure_details_list, sep = '\n\n'))) %>%
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
