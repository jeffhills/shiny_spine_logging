####################### SURGICAL PLANNING V2 ###########################

library(shiny)
library(sf)
library(tidyverse)
library(ggplot2)
library(shinyWidgets)
library(shinyBS)
library(colourpicker)
library(kableExtra)
library(cowplot)
library(ggpubr)
library(magick)
library(ggpattern)
library(glue)
library(rlist)
library(janitor)
library(lubridate)
library(redcapAPI)

# install.packages("htmlwidgets")
# library(htmlwidgets)

# source("functions.R", local = TRUE)
# source("labels_screws_functions.R", local = TRUE)
rcon <- redcapConnection(url = 'https://redcap.wustl.edu/redcap/api/', token = "4B2AA28A4D6EFBC6CA1F49EC0FB385F7")


source("hook_function.R", local = TRUE)
source("screw_function.R", local = TRUE)
source("osteotomies_decompressions_functions.R", local = TRUE)
source("load_coordinates.R", local = TRUE)

# source("implant_sizes_functions.R", local = TRUE)
# remotes::install_github("coolbutuseless/ggpattern")
# spine_png <- image_read(path = "posterior_spine_figure.png")



# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
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
        # fluidRow(
        #     column(6, 
        #            pickerInput(
        #                inputId = "symptoms",
        #                label = "Symptoms:",
        #                choices = list('Axial Pain' = c("Neck Pain", "Thoracic Pain", "Low Back Pain"), 
        #                               'Arm Pain' = c("Left Arm Pain", "Right Arm Pain"),
        #                               'Leg Pain' = c("Left Leg Pain", "Right Leg Pain"),
        #                               'Myelopathy' = c("Nurick Grade 1",
        #                                                "Nurick Grade 2",
        #                                                "Nurick Grade 3",
        #                                                "Nurick Grade 4",
        #                                                "Nurick Grade 5"),
        #                               'Other' = c("Other Symptoms")),
        #                multiple = TRUE
        #            )),
        #     column(6, 
        #            textInput(inputId = "bone_density", label = "Bone Density (T-score or HU):"))
        # ),
        textInput(inputId = "relevant_history", label = "Other Comments/History:"),
        h3(strong("Surgical Details:")),
        fluidRow(
          column(6,
                 dateInput(inputId = "date_of_surgery", label = "Date of Surgery:", format = "mm-dd-yyyy", autoclose = TRUE)),
          column(6, 
                 radioGroupButtons(
                   inputId = "primary_revision",
                   label = "Primary or Revision:",
                   choices = c("Primary", "Revision"),
                   justified = TRUE,
                   selected = "Primary",
                   size = "sm"
                 )),
          conditionalPanel("input.primary_revision.indexOf('Revision') > -1",
                           pickerInput(
                             inputId = "open_canal",
                             label = "Select Levels with Open Canal", 
                             choices = open_canal_df$level,
                             multiple = TRUE
                           )
          )
        ),
        # fluidRow(
        #     column(6,
        #            pickerInput(
        #                inputId = "preop_antibiotics",
        #                label = "Preop Antibiotics:", 
        #                choices = c("None", "Ancef", "Vancomycin", "Gentamycin", "Clindamycin", "Unknown", "Other"),
        #            )
        #            ),
        #     column(6,
        #            pickerInput(
        #                inputId = "anti_fibrinolytic", 
        #                label = "Antifibrinolytic:", 
        #                choices = c("None", "Tranexamic Acid (TXA)", "Amicar", "Desmopressin (DDAVP)", "Other"), 
        #            )
        #     ),
        #     conditionalPanel(condition = "input.anti_fibrinolytic.indexOf('Tranexamic Acid (TXA)') > -1",
        #                      fluidRow(column(8, 
        #                                      "TXA Loading (mg/kg):"), 
        #                               column(4, 
        #                                      numericInput(inputId = "txa_loading", label = NULL, value = 30, min = 0, max = 200, step = 5))), 
        #                      fluidRow(column(8, 
        #                                      "TXA Maintenance (mg/kg/hr):"), 
        #                               column(4, 
        #                                      numericInput(inputId = "txa_maintenance", label = NULL, value = 5, min = 0, max = 50, step = 5))),
        #     )
        # ), 
        # tags$hr(),
        # column(6, 
        #        strong("Allograft (cc):")
        #        ), 
        # column(6, 
        #        numericInput(inputId = "allograft", label = NULL, value = 0, min = 0, max = 500, step = 10)),
        # column(6, 
        #        strong("BMP Kits:")
        # ), 
        # column(6, 
        #        numericInput(inputId = "bmp_number", label = NULL, value = 0, min = 0, max = 20, step = 1)
        #        ),
        # conditionalPanel(condition = "input.bmp_number > 0",
        #                  radioGroupButtons(
        #                      inputId = "bmp_size",
        #                      label = NULL,
        #                      choices = c("XXS" = 1.05, "XS" = 2.1, "Sm" = 4.2, "M" = 8.4, "L" = 12),
        #                      size = "sm",
        #                      selected = "L",
        #                      checkIcon = list(
        #                          yes = tags$i(class = "fa fa-check-square", 
        #                                       style = "color: steelblue"),
        #                          no = tags$i(class = "fa fa-square-o", 
        #                                      style = "color: steelblue"))
        #                  )
        #                  ),
        tags$hr(),
        column(12, #style='background-color:#f2f2f2;min-width: 300px;',
               tags$table(
                 tags$tr(width = "100%",
                         tags$td(width = "60%", div(style = "font-size:14px; font-weight:bold; text-align:left", "Preop Antibiotics:")),
                         tags$td(width = "40%", 
                                 pickerInput(
                                   inputId = "preop_antibiotics",
                                   label = NULL, 
                                   choices = c("None", "Ancef", "Vancomycin", "Gentamycin", "Clindamycin", "Unknown", "Other"),
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
        column(12,
               tags$table(
                 tags$tr(width = "100%",
                         tags$td(width = "60%", tags$div(style = "font-size:14px; font-weight:bold; text-align:left","Allograft:")),
                         tags$td(width = "40%",align = "right",
                                 numericInput(inputId = "allograft", label = NULL, value = 0, min = 0, max = 500, step = 10, width = "100%"))),
                 tags$tr(width = "100%",
                         tags$td(width = "60%", div(style = "font-size:14px; font-weight:bold; text-align:left", "BMP Kits:")),
                         tags$td(width = "40%", 
                                 numericInput(inputId = "bmp_number", label = NULL, value = 0, min = 0, max = 20, step = 1, width = "100%"))),
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
               )
        )
      )
    ),
    # ),
    # column(5,style='background-color:#f2f2f2;min-width: 300px;',
    #        h4("Label Issue"),
    #        br(),
    #        tags$table(
    #            tags$tr(width = "100%",
    #                    tags$td(width = "60%", div(style = "font-size:10px;", "This is label1. I want all labels on left")),
    #                    tags$td(width = "40%", textInput(inputId = "a", label = NULL))),
    #            tags$tr(width = "100%",
    #                    tags$td(width = "60%", tags$div(style = "font-size:10pX;", "label2")),
    #                    tags$td(width = "40%", textInput(inputId = "b", label = NULL)))
    #        )
    # )
    # h4("Spinal Anatomy:"),
    # sliderInput(
    #     inputId = "number_of_lumbar_vertebrae",
    #     label = "Number of Lumbar Vertebrae:",
    #     min = 4,
    #     max = 6,
    #     value = 5,
    #     step = 1
    # ),
    # sliderInput(
    #     inputId = "number_of_thoracic_vertebrae",
    #     label = "Number of Thoracic (Ribbed) Vertebrae:",
    #     min = 11,
    #     max = 13,
    #     value = 12,
    #     step = 1
    # )
    # ),
    mainPanel(width = 9,
              tabsetPanel(type = "tabs",
                          tabPanel(title = "Spine Plan",
                                   column(
                                     width = 2,
                                     br(),
                                     br(),
                                     dropdown(icon = icon("grip-lines-vertical"), 
                                              width = "250%",
                                              size = "sm",
                                              label = "Add or Modifiy Rod Details",
                                              style = "unite",    
                                              # animate = animateOptions(
                                              #     enter = animations$fading_entrances$fadeInLeftBig,
                                              #     exit = animations$fading_exits$fadeOutRightBig
                                              # ),
                                              fixedRow(
                                                column(width = 6,
                                                       strong("Left Primary Rod:"),
                                                       radioGroupButtons(
                                                         inputId = "left_main_rod_size",size = "xs", justified = TRUE, direction = "vertical",
                                                         label = "Size:",
                                                         choices = c("None", "4.5mm", "4.75mm", "5.5mm", "6.0mm", "6.34mm/quarter in"),
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
                                                         choices = c("None", "Transition", "4.5mm", "4.75mm", "5.5mm", "6.0mm", "6.34mm/quarter in"),
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
                                              size = "sm", 
                                              width = "250%",
                                              label = "Add Pedicle Screw Details",
                                              style = "unite", 
                                              uiOutput(outputId = "pedicle_screw_details_ui")
                                     ),
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
                                     )
                                   ),
                                   column(
                                     width = 10,
                                     # uiOutput("open_canal_ui"),
                                     align = "center",
                                     column(width = 1, 
                                            br(),
                                            br(),
                                            br(),
                                            noUiSliderInput(inputId = "crop_y", 
                                                            label = "Crop", 
                                                            min = 0, 
                                                            max = 1, 
                                                            value = c(0,1), direction = "rtl",
                                                            behaviour = "drag",
                                                            orientation = "vertical",
                                                            height = "600px", width = "5px",
                                                            inline = TRUE)
                                     ), 
                                     column(8, 
                                            dropdownButton(
                                              sliderInput(
                                                inputId = "label_text_size",
                                                label = "Label Text Size",
                                                value = 12,
                                                min = 9,
                                                max = 20
                                              ),
                                              sliderInput(
                                                inputId = "label_text_offset",
                                                label = "Move Labels Lateral",
                                                min = -10,
                                                max = 10,
                                                value = 0
                                              ),
                                              circle = TRUE,
                                              icon = icon("gear"),
                                              size = "sm",
                                              inline = TRUE,
                                              right = TRUE
                                            ),
                                            plotOutput("spine_plan",
                                                       height = 750,
                                                       click = "plot_click", 
                                                       dblclick = "plot_double_click"),
                                            textOutput(outputId = "currently_adding")
                                            
                                     ),
                                     column(width = 3, 
                                            br(),
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
                                              label = "Add Osteotomies", 
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
                                            ),
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
                                          tableOutput(outputId = "all_objects_table")
                                   )
                          )
                          # tabPanel(title = "Capture Coordinates",
                          #     column(width = 12,    
                          #            column(4,
                          #                   textInput(inputId = "coordinate_type", label = "Coordinate Type"),
                          #                   actionButton(inputId = "save_coordinates_table_button", label = "Click to save coordinates table"),
                          #                   textInput(inputId = "file_name", label = "Name of File (add .csv)"),
                          #                   tableOutput(outputId = "coordinates_table_2")
                          #                   ),
                          #         column(width = 2, 
                          #                 noUiSliderInput(inputId = "crop_y_coordinate_plot", 
                          #                                 label = "Crop", 
                          #                                 min = 0, 
                          #                                 max = 1, 
                          #                                 value = c(0,1), direction = "rtl",
                          #                                 behaviour = "drag",
                          #                                 orientation = "vertical",
                          #                                 height = "600px", width = "5px",
                          #                                 inline = TRUE)),
                          #          column(width = 6, 
                          #                 plotOutput(outputId = "spine_plot",
                          #                            height = 750,
                          #                            click = "plot_click_coordinate"),
                          #                 actionBttn(inputId = "remove_last_coordinate", label = "Remove last")
                          #          )
                          #     )
                          # )
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
  
  observeEvent(input$add_implants, {
    updateRadioGroupButtons(session = session, 
                            inputId = "object_to_add", 
                            choices = c(
                              "Pedicle Screws" = "pedicle_screw",
                              "Pelvic Screws" = "pelvic_screw",
                              "Lateral Mass Screws" = "lateral_mass_screw",
                              "Pars Screws" = "pars_screw",
                              "Occipital Screws" = "occipital_screw",
                              "Translaminar Screws" = "translaminar_screw",
                              "TP Hook" = "tp_hook",
                              "Laminar Hook (Downgoing)" = "laminar_downgoing_hook",
                              "Laminar Hook (Upgoing)" = "laminar_upgoing_hook",
                              "Pedicle Hook" = "pedicle_hook",
                              "Tether" = "tether",
                              "Sublaminar Wire" = "sublaminar_wire" , 
                              "Kyphoplasty/Cement" = "kyphoplasty"
                            ),
                            selected = "Pediicle Screws"
    )
  })
  
  observeEvent(input$add_decompressions, {
    updateRadioGroupButtons(session = session, 
                            inputId = "object_to_add", 
                            choices = c(
                              "Laminectomy" = "laminectomy",
                              "Sublaminar Decompression" = "sublaminar_decompression",
                              "Laminotomy/Foraminotomy" = "foraminotomy",
                              "Hemilaminectomy" = "hemilaminectomy",
                              "Diskectomy" = "diskectomy"
                            ),
                            selected = "Laminectomy"
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
                            selected = "Grade 1 (Facetectomy)"
    )
  })
  
  observeEvent(input$add_interbody, {
    updateRadioGroupButtons(session = session, 
                            inputId = "object_to_add", 
                            choices = c("ACDF",
                                        "ALIF",
                                        "TLIF",
                                        "LLIF",
                                        "PLIF"),
                            selected = "TLIF"
    )
  })
  
  output$currently_adding <- renderText(paste("Currently Adding: ", str_to_title(string = str_replace_all(string = input$object_to_add, pattern = "_", replacement = " "))), sep = "")
  
  
  ###  RODS
  #     inputId = "right_main_rod_size", size = "xs", justified = TRUE,direction = "vertical",
  #     label = "Size:",
  #     choices = c("None", "Transition", "4.5mm", "4.75mm", "5.5mm", "6.0mm", "6.34mm/quarter in"),
  #     selected = "None"
  #     ),
  # radioGroupButtons(
  #     inputId = "right_main_rod_material",size = "xs", justified = TRUE,direction = "vertical",
  #     label = "Material:",
  #     choices = c("Non-instrumented", "Titanium", "Cobalt Chrome", "Stainless Steel"), 
  #     selected = "Non-instrumented"
  
  observeEvent(left_main_rod_reactive_sf(), {
    if(nrow(left_rod_implants_df()) > 1){
      updateSliderTextInput(session = session, 
                            inputId = "left_main_rod_size", 
                            choices = c("Transition", "4.5mm", "4.75mm", "5.5mm", "6.0mm", "6.34mm/quarter in"),
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
                            choices = c("Transition", "4.5mm", "4.75mm", "5.5mm", "6.0mm", "6.34mm/quarter in"),
                            selected = "5.5mm"
      )
      updateSliderTextInput(session = session, 
                            inputId = "right_main_rod_material", 
                            choices = c("Titanium", "Cobalt Chrome", "Stainless Steel"),
                            selected = "Titanium"
      )
    }
  })
  
  
  observeEvent({list(left_rod_implants_df, input$add_left_accessory_rod, input$add_left_satellite_rod, input$add_left_intercalary_rod, input$add_left_linked_rods)},ignoreInit = TRUE, {
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
  
  observeEvent({list(right_rod_implants_df, input$add_right_accessory_rod, input$add_right_satellite_rod, input$add_right_intercalary_rod, input$add_right_linked_rods)},ignoreInit = TRUE, {
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
                                               category = character(),
                                               vertebral_number = double(),
                                               implant = character(),
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
  
  
  ########################################### IMPLANT DETAILS REACTIVE ###########################################
  
  objects_to_add_reactive_list <- reactiveValues()
  
  objects_to_add_reactive_list$object_to_add <- reactive({
    input$object_to_add
  })
  
  #### OBSERVE THE PLOT CLICK AND ADD APPROPRIATE IMPLANT ####
  
  object_added_reactive_df <- reactive({
    
    object_type_filtered_df <- all_implants_constructed_df %>%
      filter(implant == input$object_to_add) 
    
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
  
  #### OBSERVE THE PLOT CLICK AND ADD APPROPRIATE IMPLANT ####
  observeEvent(input$plot_click, {
    
    all_objects_to_add_list$objects_df <- object_added_reactive_df() %>%
      union_all(all_objects_to_add_list$objects_df) %>%
      distinct()
    
    if(nrow(all_objects_to_add_list$objects_df %>% filter(category == "osteotomy")) > 1){
      all_objects_to_add_list$objects_df <- all_objects_to_add_list$objects_df %>%
        filter(category == "osteotomy") %>%
        mutate(implant = fct_relevel(as_factor(implant), c("grade_1", "complete_facetectomy", "grade_2", "grade_3", "grade_4", "grade_5"))) %>%
        arrange(implant) %>%
        mutate(implant_rank = as.numeric(implant)) %>%
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
      filter(implant == input$object_to_add) 
    
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
      filter(str_detect(string = implant, pattern = "screw") | str_detect(string = implant, pattern = "hook") | str_detect(string = implant, pattern = "wire")) %>%
      select(level, vertebral_number, x, y) %>%
      distinct() %>%
      arrange(vertebral_number) 
  })
  
  right_rod_implants_df <- reactive({
    all_objects_to_add_list$objects_df %>%
      filter(!is.na(x)) %>%
      filter(x > 0.5) %>%
      filter(str_detect(string = implant, pattern = "screw") | str_detect(string = implant, pattern = "hook") | str_detect(string = implant, pattern = "wire")) %>%
      select(level, vertebral_number, x, y) %>%
      distinct() %>%
      arrange(vertebral_number)
  })
  
  
  
  ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
  ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
  ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
  ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
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
      left_accessory_rod_sf <- build_additional_rod_function(rod_type = "accessory", new_rod_vector = input$left_accessory_rod, full_implant_vector = left_rod_implants_df())
    }else{
      left_accessory_rod_sf <-  NULL
    }
    left_accessory_rod_sf
  })
  
  left_satellite_rod_reactive_sf <- reactive({
    if(nrow(left_rod_implants_df()) > 2 & input$add_left_satellite_rod == TRUE){
      
      left_satellite_rod_sf <- build_additional_rod_function(rod_type = "satellite", new_rod_vector = input$left_satellite_rod, full_implant_vector = left_rod_implants_df())
    }else{
      left_satellite_rod_sf <-  NULL
    }
    left_satellite_rod_sf
  })
  
  left_intercalary_rod_reactive_sf <- reactive({
    if(nrow(left_rod_implants_df()) > 2 & input$add_left_intercalary_rod == TRUE){
      
      left_intercalary_rod_sf <- build_additional_rod_function(rod_type = "intercalary", new_rod_vector = input$left_intercalary_rod, full_implant_vector = left_rod_implants_df())
    }else{
      left_intercalary_rod_sf <-  NULL
    }
    left_intercalary_rod_sf
  })
  
  left_linked_rods_reactive_sf <- reactive({
    if(nrow(left_rod_implants_df()) > 2 & input$add_left_linked_rods == TRUE){
      
      left_linked_rods_reactive_sf <- build_additional_rod_function(rod_type = "linked", new_rod_vector = input$left_linked_rods_overlap, full_implant_vector = left_rod_implants_df())
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
      right_accessory_rod_sf <- build_additional_rod_function(rod_type = "accessory", new_rod_vector = input$right_accessory_rod, full_implant_vector = right_rod_implants_df())
    }else{
      right_accessory_rod_sf <-  NULL
    }
    right_accessory_rod_sf
  })
  
  right_satellite_rod_reactive_sf <- reactive({
    if(nrow(right_rod_implants_df()) > 2 & input$add_right_satellite_rod == TRUE){
      right_satellite_rod_sf <- build_additional_rod_function(rod_type = "satellite", new_rod_vector = input$right_satellite_rod, full_implant_vector = right_rod_implants_df())
    }else{
      right_satellite_rod_sf <-  NULL
    }
    right_satellite_rod_sf
  })
  
  right_intercalary_rod_reactive_sf <- reactive({
    if(nrow(right_rod_implants_df()) > 2 & input$add_right_intercalary_rod == TRUE){
      right_intercalary_rod_sf <- build_additional_rod_function(rod_type = "intercalary", new_rod_vector = input$right_intercalary_rod, full_implant_vector = right_rod_implants_df())
    }else{
      right_intercalary_rod_sf <-  NULL
    }
    right_intercalary_rod_sf
  })
  
  right_linked_rods_reactive_sf <- reactive({
    if(nrow(right_rod_implants_df()) > 2 & input$add_right_linked_rods == TRUE){
      right_linked_rods_reactive_sf <- build_additional_rod_function(rod_type = "linked", new_rod_vector = input$right_linked_rods_overlap, full_implant_vector = right_rod_implants_df())
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
  
  # Make the Table
  #################### MAKE THE TABLE ########################
  
  output$objects_added_table <- renderTable({
    all_objects_to_add_list$objects_df %>%
      as_tibble() %>%
      select(level, object = implant, side, x, y) %>%
      arrange(rev(y))
  })
  
  output$upload_df <- renderTable({
    data_wide <- all_objects_to_add_list$objects_df %>%
      as_tibble() %>%
      select(-object_constructed) %>%
      arrange(vertebral_number) %>%
      select(level, category, procedure = implant, side) %>%
      pivot_wider(names_from = level, values_from = procedure) %>%
      mutate(across(everything(), ~ replace_na(.x, "")))
    
    data_wide
  })
  
  plan_reactive_df <- reactive({
    
    patient_name <- if_else(input$patient_name == "", "--", input$patient_name)
    age <- if_else(paste(input$date_of_birth) == "1900-01-01", "--", as.character(round(interval(start = paste(input$date_of_birth), end = paste(input$date_of_surgery))/years(1), 0)))
    patient_name_age <- case_when(
      patient_name == "--" ~ "--",
      patient_name != "--" & age == "--" ~ patient_name,
      patient_name != "--" & age != "--" ~ paste(patient_name, ", ", age, "yrs", sep = ""),
    )
    symptoms <- if_else(length(input$symptoms) == 0, "--", toString(input$symptoms))
    bone_density <- if_else(input$bone_density == "", "--", input$bone_density)
    relevant_history <- if_else(input$relevant_history == "", "--", input$relevant_history)
    preop_antibiotics <- if_else(length(input$preop_antibiotics) == 0, "--", toString(input$preop_antibiotics))
    anti_fibrinolytic <- case_when(
      length(input$anti_fibrinolytic) == 0 ~ "--",
      length(input$anti_fibrinolytic) == 1 & "Tranexamic Acid (TXA)" %in% input$anti_fibrinolytic ~ paste(glue("TXA (Load: {input$txa_loading}, Maint: {input$txa_maintenance})")),
      length(input$anti_fibrinolytic) > 1 & "Tranexamic Acid (TXA)" %in% input$anti_fibrinolytic ~ paste(glue("{toString(setdiff(x = input$anti_fibrinolytic,
                                                                                                                       y = 'Tranexamic Acid (TXA)'))}, 
                                                                                                                       TXA (Loading: {input$txa_loading}, Maint: {input$txa_maintenance})")),
      length(input$anti_fibrinolytic) > 0 & ("Tranexamic Acid (TXA)" %in% input$anti_fibrinolytic) == FALSE ~ toString(input$anti_fibrinolytic)
    )
    
    bmp_text <- if_else(input$bmp_number == 0, "None", paste(input$bmp_number, input$bmp_size, sep = " "))
    bmp_mg_dose <- as.double(input$bmp_size)*as.double(input$bmp_number)
    bmp <- if_else(bmp_text == "None", "None", paste(bmp_text, "(", as.character(bmp_mg_dose), "mg)"))
    
    allograft <- if_else(input$allograft == 0, "None", paste(input$allograft, "cc", sep = ""))
    left_rod <- if_else(nrow(left_rod_implants_df()) > 1, paste(input$left_main_rod_size, input$left_main_rod_material, sep = " "), "--")
    right_rod <- if_else(nrow(right_rod_implants_df()) > 1, paste(input$right_main_rod_size, input$right_main_rod_material, sep = " "), "--")
    
    tibble(descriptor = c("Patient:", "Symptoms:", "Bone Density:", "Relevant Hx:", "---", "Preop Abx:", "Antifibrinolytic:", "Allograft", "BMP:", "Left Rod:", "Right Rod:"), 
           value = c(patient_name_age, symptoms, bone_density, relevant_history, "---", preop_antibiotics, anti_fibrinolytic, allograft, bmp, left_rod, right_rod)
    ) %>% 
      filter(!is.null(value)) %>%
      filter(value != "--")
    
  })
  
  ################# MAKE THE REDCAP AND SUMMARY TABLE 
  summary_table_for_redcap_reactive <- reactive({
    all_implants_df <- all_objects_to_add_list$objects_df %>%
      filter(category == "implant") %>%
      filter(str_detect(string = implant, pattern = "hook") | str_detect(string = implant, pattern = "screw") | str_detect(string = implant, pattern = "wire"))
    
    spine_instrumented <- if_else(nrow(all_implants_df) == 0, FALSE, TRUE)
    spine_treated <- if_else(nrow(all_objects_to_add_list$objects_df) == 0, FALSE, TRUE)
    
    dos <- round(as.integer(input$date_of_surgery), 0) 
    upper_treated_vertebrae <- if_else(spine_treated == TRUE, (all_objects_to_add_list$objects_df %>% filter(vertebral_number == min(vertebral_number)))$level[1], "none")
    uiv <- if_else(spine_instrumented == TRUE, (all_implants_df %>% filter(vertebral_number == min(vertebral_number)))$level[1], "none")
    lower_treated_vertebrae <- if_else(spine_treated == TRUE, (all_objects_to_add_list$objects_df %>% filter(vertebral_number == max(vertebral_number)))$level[1], "none")
    liv <- if_else(spine_instrumented == TRUE, (all_implants_df %>% filter(vertebral_number == max(vertebral_number)))$level[1], "none")
    
    uiv_tether <- any(str_detect(string = (all_objects_to_add_list$objects_df %>% filter(level == upper_treated_vertebrae))$implant, "tether"))
    uiv_hook <- any(str_detect(string = (all_objects_to_add_list$objects_df %>% filter(level == uiv))$implant, "hook"))
    
    if(spine_instrumented == TRUE | spine_treated == TRUE){
      uiv_ppx_df <- all_objects_to_add_list$objects_df %>%
        filter(level == upper_treated_vertebrae | level == uiv) %>%
        filter(str_detect(string = implant, pattern = "hook") | str_detect(string = implant, pattern = "tether") | str_detect(string = implant, pattern = "wire") | str_detect(string = implant, pattern = "kyphoplasty"))
    }else{
      uiv_ppx_df <- tibble(level = character(), implant = character())
    }
    
    
    levels_fused <- if_else(spine_instrumented == FALSE, 0, max(all_implants_df$vertebral_number) - min(all_implants_df$vertebral_number) + 1)
    pelvic_fixation <- any(str_detect(string = all_implants_df$implant, pattern = "pelvic"))
    three_column_osteotomy <- any(str_detect(string = all_implants_df$implant, pattern = "grade_3") | str_detect(string = all_implants_df$implant, pattern = "grade_4")  | str_detect(string = all_implants_df$implant, pattern = "grade_5") )
    
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
      select(level, implant) %>%
      distinct()
    
    decompressions_vector <- if_else(nrow(decompressions_df) == 0, "None", toString(decompressions_df$implant))
    
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
                         uiv_ppx = if_else(nrow(uiv_ppx_df) == 0, "None", toString(uiv_ppx_df$implant)),
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
    all_objects_to_add_list$objects_df %>%
      select(-object_constructed)
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
  
  
  
  #################### MAKE THE PLOT #######################
  #################### MAKE THE PLOT #######################
  #################### MAKE THE PLOT #######################
  
  output$spine_plan <- renderPlot({
    #################### MAKE THE PLOT #######################
    ## OPEN CANAL
    if(length(input$open_canal) > 0){
      open_df <- open_canal_df %>%
        filter(level %in% input$open_canal)
      open_canal_sf <- st_union(st_combine(st_multipolygon(open_df$object_constructed)), by_feature = TRUE, is_coverage = TRUE)
    }else{
      open_canal_sf <- NULL
    }
    
    ## SCREWS
    if(any(str_detect(all_objects_to_add_list$objects_df$implant, pattern = "screw"))){
      screws_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(str_detect(string = implant, pattern = "screw")))$object_constructed)
    }else{
      screws_sf <- NULL
    }
    
    ## HOOKS
    if(any(str_detect(all_objects_to_add_list$objects_df$implant, pattern = "hook"))){
      hook_df <- all_objects_to_add_list$objects_df %>% 
        filter(str_detect(string = implant, pattern = "hook"))
      hooks_sf <- st_multipolygon(hook_df$object_constructed)
    }else{
      hooks_sf <- NULL
    }
    ## SUBLAMINAR BANDS
    if(any(str_detect(all_objects_to_add_list$objects_df$implant, pattern = "wire"))){
      sublaminar_wires_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(str_detect(string = implant, pattern = "wire")))$object_constructed)
    }else{
      sublaminar_wires_sf <- NULL
    }
    
    ## Tethers
    if(any(str_detect(all_objects_to_add_list$objects_df$implant, pattern = "tether"))){
      tethers_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(str_detect(string = implant, pattern = "tether")))$object_constructed)
    }else{
      tethers_sf <- NULL
    }
    
    ## Kyphoplasty
    if(any(str_detect(all_objects_to_add_list$objects_df$implant, pattern = "kyphoplasty"))){
      kyphoplasty_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(str_detect(string = implant, pattern = "kyphoplasty")))$object_constructed)
    }else{
      kyphoplasty_sf <- NULL
    }
    
    ## OSTEOTOMIES
    if(any(str_detect(all_objects_to_add_list$objects_df$implant, pattern = "grade_1"))){
      osteotomy_1_sf <- st_geometrycollection((all_objects_to_add_list$objects_df %>% filter(implant == "grade_1"))$object_constructed)
    }else{
      osteotomy_1_sf <- NULL
    }
    if(any(str_detect(all_objects_to_add_list$objects_df$implant, pattern = "complete_facetectomy"))){
      osteotomy_facetectomy_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(implant == "complete_facetectomy"))$object_constructed)
    }else{
      osteotomy_facetectomy_sf <- NULL
    }
    if(any(str_detect(all_objects_to_add_list$objects_df$implant, pattern = "grade_2"))){
      osteotomy_2_sf <-  st_union(st_combine(st_multipolygon((all_objects_to_add_list$objects_df %>% filter(implant == "grade_2"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE)
    }else{
      osteotomy_2_sf <- NULL
    }
    if(any(str_detect(all_objects_to_add_list$objects_df$implant, pattern = "grade_3"))){
      osteotomy_3_sf <- st_union(st_combine(st_multipolygon((all_objects_to_add_list$objects_df %>% filter(implant == "grade_3"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE)
      
    }else{
      osteotomy_3_sf <- NULL
    }
    if(any(str_detect(all_objects_to_add_list$objects_df$implant, pattern = "grade_4"))){
      osteotomy_4_sf <- st_union(st_combine(st_multipolygon((all_objects_to_add_list$objects_df %>% filter(implant == "grade_4"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE) 
    }else{
      osteotomy_4_sf <- NULL
    }
    if(any(str_detect(all_objects_to_add_list$objects_df$implant, pattern = "grade_5"))){
      osteotomy_5_sf <- st_union(st_combine(st_multipolygon((all_objects_to_add_list$objects_df %>% filter(implant == "grade_5"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE) 
    }else{
      osteotomy_5_sf <- NULL
    }
    
    
    ## Decompresions ##
    if(any(str_detect(all_objects_to_add_list$objects_df$implant, pattern = "laminectomy"))){
      laminectomy_sf <- st_union(st_combine(st_multipolygon((all_objects_to_add_list$objects_df %>% filter(implant == "laminectomy"))$object_constructed)), by_feature = TRUE, is_coverage = TRUE) 
    }else{
      laminectomy_sf <- NULL
    }
    if(any(str_detect(all_objects_to_add_list$objects_df$implant, pattern = "sublaminar_decompression"))){
      sublaminar_decompression_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(implant == "sublaminar_decompression"))$object_constructed)
    }else{
      sublaminar_decompression_sf <- NULL
    }
    if(any(str_detect(all_objects_to_add_list$objects_df$implant, pattern = "hemilaminectomy"))){
      hemilaminectomy_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(implant == "hemilaminectomy"))$object_constructed)
    }else{
      hemilaminectomy_sf <- NULL
    }
    if(any(str_detect(all_objects_to_add_list$objects_df$implant, pattern = "foraminotomy"))){
      foraminotomy_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(implant == "foraminotomy"))$object_constructed)
    }else{
      foraminotomy_sf <- NULL
    }
    if(any(str_detect(all_objects_to_add_list$objects_df$implant, pattern = "diskectomy"))){
      diskectomy_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(implant == "diskectomy"))$object_constructed)
    }else{
      diskectomy_sf <- NULL
    }
    if(any(str_detect(all_objects_to_add_list$objects_df$implant, pattern = "diskectomy"))){
      diskectomy_sf <- st_multipolygon((all_objects_to_add_list$objects_df %>% filter(implant == "diskectomy"))$object_constructed)
    }else{
      diskectomy_sf <- NULL
    }
    
    ### RODS 
    # if(input$add_left_intercalary_rod == TRUE | input$add_left_linked_rods == TRUE | input$add_left_satellite_rod == TRUE){
    #     left_main_rod_sf <- NULL
    # }else{
    #     left_main_rod_sf <- left_main_rod_reactive_sf()
    # }
    # if(input$add_right_intercalary_rod == TRUE | input$add_right_linked_rods == TRUE | input$add_right_satellite_rod == TRUE){
    #     right_main_rod_sf <- NULL
    # }else{
    #     right_main_rod_sf <- right_main_rod_reactive_sf()
    # }
    # 
    # all_rods_list <- compact(list(left_main_rod_sf,
    #                               left_accessory_rod_reactive_sf(),
    #                               left_satellite_rod_reactive_sf(),
    #                               left_intercalary_rod_reactive_sf(),
    #                               left_linked_rods_reactive_sf(),
    #                               right_main_rod_sf,
    #                               right_accessory_rod_reactive_sf(),
    #                               right_satellite_rod_reactive_sf(),
    #                               right_intercalary_rod_reactive_sf(),
    #                               right_linked_rods_reactive_sf()))
    
    all_rods_sf <- st_geometrycollection(all_rods_reactive_list())
    
    
    # This works nicely if you don't want the pattern to fill in for a multipolygon... but is much slower
    # screws_ggpattern <- map(.x = screws_df$object_constructed, .f = ~ ggpattern::geom_sf_pattern(
    #     data = .x,
    #     pattern = "stripe",
    #     pattern_fill = "blue",
    #     pattern_fill2 = "#445566",
    #     alpha = 0.8,
    #     pattern_colour = "blue",
    #     pattern_density = 0.02,
    #     pattern_spacing = 0.01,
    # ))
    
    ## Create df for plan ##
    x_left_limit <- 0.3 - input$label_text_offset/100
    x_right_limit <- 1-x_left_limit
    
    plot_top_y <- input$crop_y[2]
    
    y_spacing <- 0.025*input$crop_y[2]
    
    y_start_with_text <- plot_top_y + nrow(plan_reactive_df())*y_spacing
    
    text_square <- st_buffer(st_polygon(list(rbind(c(x_left_limit+0.01, y_start_with_text + 0.01),
                                                   c(x_left_limit+0.01, plot_top_y),
                                                   c(x_right_limit-0.01, plot_top_y),
                                                   c(x_right_limit-0.01, y_start_with_text+ 0.01),
                                                   c(x_left_limit+0.01, y_start_with_text+ 0.01)))), dist = 0.01, endCapStyle = "ROUND")
    
    plan_df <- plan_reactive_df() %>%
      mutate(y = seq(from = y_start_with_text, to = plot_top_y + y_spacing, by = -y_spacing))
    
    spine_plot <- ggdraw() +
      draw_image(
        spine_png,
        scale = 1,
        y = 0,
        valign = 0,
        x = 0, height = 1
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
      geom_sf_pattern(data = kyphoplasty_sf,
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
        data = hemilaminectomy_sf,
        pattern = "stripe", 
        pattern_colour = "red",
        alpha = 0.7,
        pattern_spacing = 0.01
      ) +
      ggpattern::geom_sf_pattern(
        data = foraminotomy_sf,
        pattern = "stripe", 
        pattern_colour = "red",
        alpha = 0.7,
        pattern_spacing = 0.01
      ) +
      ggpattern::geom_sf_pattern(
        data = diskectomy_sf,
        pattern = "gradient",
        pattern_orientation = "vertical",
        pattern_fill = "black",
        pattern_fill2 = "#445566",
        alpha = 0.7,
      ) +
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
      ggpattern::geom_sf_pattern(
        data = screws_sf,
        pattern = "stripe",
        pattern_fill = "blue",
        pattern_fill2 = "#445566",
        alpha = 0.8,
        pattern_colour = "blue",
        pattern_density = 0.02,
        pattern_spacing = 0.01,
        pattern_angle = 80
      ) +
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
      geom_sf(data = text_square, fill = "grey98") +
      draw_text(text = plan_df$descriptor, x = x_left_limit + 0.01, y = plan_df$y, size = input$label_text_size - 2, hjust = 0, vjust = 0.5) +
      draw_text(text = str_wrap(string = plan_df$value, width = 40 +input$label_text_offset, exdent = 5), x = x_left_limit + 0.12, y = plan_df$y, size = input$label_text_size - 2, hjust = 0, vjust = 0.5, lineheight = 0.7) +
      ylim(input$crop_y[1], y_start_with_text) +
      xlim(x_left_limit, x_right_limit)
    
    spine_plot
  })
  
  
  
  ##################### REACTIVE UI AND FUNCTION TO GENERATE INPUT NAMES AND RETRIEVE VALUES ##################
  ##################### REACTIVE UI AND FUNCTION TO GENERATE INPUT NAMES AND RETRIEVE VALUES ##################
  ##################### REACTIVE UI AND FUNCTION TO GENERATE INPUT NAMES AND RETRIEVE VALUES ##################
  
  make_ui_rows <-  function(level = NULL, left_screw_level = "no_screw", right_screw_level = "no_screw", left_selected = NULL, right_selected = NULL){
    
    # level <- str_to_title(level)
    
    if(left_screw_level != "no_screw"){
      left_ui <- radioGroupButtons(
        inputId = left_screw_level,
        label = NULL,
        choices = c("Poly", "Uni", "Mono", "Reduction"),
        selected = left_selected,
        size = "xs"
      )
    }else{
      left_ui <- NULL
    }
    
    if(right_screw_level != "no_screw"){
      right_ui <- radioGroupButtons(
        inputId = right_screw_level,
        label = NULL,
        choices = c("Poly", "Uni", "Mono", "Reduction"),
        selected = right_selected,
        size = "xs"
      )
    }else{
      right_ui <- NULL
    }
    
    
    fixedRow(
      column(width = 2,
             strong(level)),
      column(width = 5,
             left_ui
      ),
      column(width = 5,
             right_ui
      )
    )
    
  }
  
  
  pedicle_screw_level_reactive_df <- reactive({
    
    if(nrow(implants_list$implants_df) > 1){
      implants_list$implants_df %>%
        mutate(level_side = paste(level, side, implant, sep = "_")) %>%
        select(vertebral_number, level, side, level_side, implant) %>%
        filter(implant == "pedicle_screw") %>%
        distinct() %>%
        arrange(vertebral_number)
    }else{
      implants_list$implants_df
    }
    
    
  })
  
  output$pedicle_screw_details_ui <- renderUI({
    
    all_implants <- all_objects_to_add_list$objects_df %>%
      select(level, vertebral_number, implant, side) %>%
      distinct() %>%
      filter(implant == "pedicle_screw") %>%
      arrange(vertebral_number)
    
    if(nrow(all_implants) > 1){
      implants_wide_df <- all_implants %>%
        mutate(level_side = paste(level, side, implant, sep = "_")) %>%
        select(level, level_side, side) %>%
        pivot_wider(names_from = side, values_from = level_side)
      implants_wide_df
      
      df_names <- glue_collapse(names(implants_wide_df), sep = " ")
      
      level_vector <- implants_wide_df$level
      
      ## the data frame can't have any missing values, otherwise the vectors will be of different lengths when you run map()
      if(str_detect(string = df_names, pattern = "left")){
        implants_wide_df <- implants_wide_df %>%
          mutate(left = if_else(is.na(left), "no_screw", left))
        
        left_vector <- implants_wide_df$left
      }else{
        implants_wide_df <- implants_wide_df %>%
          mutate(left = "no_screw")
        
        left_vector <- implants_wide_df$left
      }
      
      if(str_detect(string = df_names, pattern = "right")){
        implants_wide_df <- implants_wide_df %>%
          mutate(right = if_else(is.na(right), "no_screw", right))
        
        right_vector <- implants_wide_df$right
      }else{
        implants_wide_df <- implants_wide_df %>%
          mutate(right = "no_screw")
        
        right_vector <- implants_wide_df$right
      }
      
      max_levels <- nrow(implants_wide_df)     
      levels_count <- seq(from = 1, to = max_levels, by = 1)
      
      fixedRow(
        h4("Pedicle Screw Details"),
        column(width = 2,
               NULL),
        column(width = 5,
               h4("Left")),
        column(width = 5,
               h4("Right")),
        pmap(.l = list(..1 = level_vector,
                       ..2 = left_vector,
                       ..3 = right_vector, 
                       ..4 = levels_count),
             .f = ~make_ui_rows(level = ..1, 
                                left_screw_level = ..2, 
                                right_screw_level = ..3, 
                                left_selected = input[[left_vector[..4]]],   ## using this subsetting
                                right_selected = input[[right_vector[..4]]]))
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
      # pedicle_screw_level_reactive_df()
      pedicle_screw_level_reactive_df() %>%
        select(level, vertebral_number, level_side) %>%
        mutate(screw_details = map(.x = levels_count, .f = ~input[[pedicle_screw_level_reactive_df()$level_side[.x]]])) %>%
        unnest()
    }else{
      pedicle_screw_level_reactive_df()
    }
    
    # GOOD:
    # map(.x = levels_count, .f = ~input[[pedicle_screw_level_reactive_df()$level_side[.x]]])
  })
  
  
  
  # 
  #     
  #     
  #     output$surgical_summary <- renderUI({
  #         all_implants <- implants_list$left_implants_df %>%
  #             union_all(implants_list$right_implants_df) %>%
  #             select(level, vertebral_number, implant, side) %>%
  #             distinct() %>%
  #             arrange(vertebral_number)
  #         
  #         all_implants
  #         
  #         uiv_df <- all_implants %>%
  #             filter(implant != "tether") %>%
  #             filter(vertebral_number == min(vertebral_number)) 
  #         
  #         liv_df <- all_implants %>%
  #             filter(implant != "tether") %>%
  #             filter(vertebral_number == max(vertebral_number))
  #         
  #         HTML(paste(paste("UIV = ", uiv_df$level[[1]]), paste("LIV = ", liv_df$level[[1]]), sep = "<br/>"))
  #     })
  #     
  #     
  #     
  
  
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
  
  
  
  ################################# REDCAP API #########################
  observeEvent(input$confirm_upload_1, {
    confirmSweetAlert(inputId = "confirm_upload_final",
                      session = session,
                      title = "Final Confirmation to Upload Data:", 
                      btn_labels = c("No, Cancel", "Yes, Upload"), text = "This Case has Been Uploaded", closeOnClickOutside = TRUE)
  })
  
  observeEvent(input$confirm_upload_final, {
    
    # rcon <- redcapConnection(url = 'https://redcap.wustl.edu/redcap/api/', token = "4B2AA28A4D6EFBC6CA1F49EC0FB385F7")
    
    # redcap_exported_df <- exportRecords(rcon = rcon)
    # 
    # exportNextRecordName()
    # 
    # number_of_records <- redcap_exported_df %>%
    #     select(record_id) %>%
    #     mutate(record_id = as.double(record_id)) %>%
    #     filter(record_id == max(record_id)) %>%
    #     distinct()
    # 
    # new_record_number <- number_of_records[[1]] +1
    
    new_record_number <- exportNextRecordName(rcon = rcon)
    
    df_for_upload <- summary_table_for_redcap_reactive() %>%
      rename_all(.funs = ~paste(., "_shiny", sep = "")) %>%
      mutate(record_id = new_record_number) %>%
      select(record_id, everything())
    
    importRecords(rcon = rcon, data = df_for_upload, returnContent = "count")
    
    sendSweetAlert(
      session = session,
      title = "Success !!",
      text = "All in order",
      type = "success",
    )
    
    
  })
  
  # observeEvent(input$confirm_upload_final, {
  #     sendSweetAlert(
  #         session = session,
  #         title = "Success !!",
  #         text = "All in order",
  #         type = "success",
  #     )
  # })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
