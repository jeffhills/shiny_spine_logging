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
library(data.table)
# library(shinyjs)



library(profvis)



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
source("load_coordinates_build_objects_new.R", local = TRUE)
source("anterior_posterior_operative_note_generator_functions.R", local = TRUE)
source("operative_note_paragraph_functions.R", local = TRUE)
# source("load_coordinates_build_objects_6_lumbar.R", local = TRUE)
source("screw_size_type_inputs.R", local = TRUE)
source("redcap_table_functions.R", local = TRUE)

# anterior_body_constructed_df <- all_object_ids_df %>%
#   filter(category == "anterior_body") %>%
#   left_join(fread("coordinates/anterior_body.csv") %>%
#               group_by(object_id) %>%
#               nest() %>%
#               mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
#               select(object_id, object_constructed))
# 
# anterior_disc_constructed_df <- all_object_ids_df %>%
#   filter(category == "anterior_disc") %>%
#   filter(object != "anterior_disc_arthroplasty") %>%
#   left_join(fread("coordinates/anterior_disc.csv") %>% 
#               group_by(object_id) %>%
#               nest() %>%
#               mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
#               select(object_id, object_constructed))
# 
# anterior_interbody_fusion_constructed_df <- all_object_ids_df %>%
#   filter(category == "anterior_interbody_fusion") %>%
#   left_join(fread("coordinates/anterior_interbody_fusion.csv") %>%
#               group_by(object_id) %>%
#               nest() %>%
#               mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
#               select(object_id, object_constructed))
# 
# arthroplasty_constructed_df <- all_object_ids_df %>%
#   filter(str_detect(object, "arthropl")) %>%
#   mutate(inferior_endplate_y = c(0.965, 0.925, 0.897, 0.87, 0.846, 0.821, 0.795, 0.77, 0.74, 0.712, 0.682, 0.65, 0.62, 0.591, 0.56, 0.53, 0.495, 0.457, 0.415, 0.367, 0.317, 0.263, 0.215, 0.168), 
#          superior_endplate_y = c(0.955, 0.918, 0.888, 0.865, 0.838, 0.815, 0.79, 0.76, 0.732, 0.705, 0.675, 0.645, 0.613, 0.584, 0.555, 0.525, 0.49, 0.45, 0.406, 0.357, 0.305, 0.255, 0.205, 0.16), 
#          width = c(0.0175, 0.021, 0.02275, 0.02275, 0.0245, 0.0245, 0.0245, 0.02625, 0.02625, 0.028, 0.028, 0.02975, 0.02975, 0.0315, 0.0315, 0.0315, 0.03325, 0.035, 0.035, 0.035, 0.03675, 0.0385, 0.04025, 0.042)
#   ) %>%
#   mutate(object_constructed = pmap(.l = list(..1 = inferior_endplate_y,
#                                              ..2 = superior_endplate_y, 
#                                              ..3 = width),
#                                    .f = ~ arthroplasty_function(y_for_inferior_endplate = ..1, 
#                                                                 y_for_superior_endplate = ..2,
#                                                                 endplate_width = ..3)))%>%
#   select(names(all_implants_constructed_df))
# 
# all_implants_constructed_df <<- all_implants_constructed_df %>%
#   bind_rows(anterior_body_constructed_df) %>%
#   bind_rows(anterior_disc_constructed_df) %>%
#   bind_rows(anterior_interbody_fusion_constructed_df) %>%
#   bind_rows(arthroplasty_constructed_df) %>%
#   distinct() 

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

##########################  #################################### UI ####################################  ####################################
##########################  #################################### UI ####################################  ####################################
##########################  #################################### UI ####################################  ####################################
##########################  #################################### UI ####################################  ####################################
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Spine Operative Logging and Automated Report Generation", titleWidth = 650
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
                                                 ), 
                                                 br(), 
                                                 textOutput(outputId = "timer")
                                     )
                    ),
                    dashboardBody(
                      tags$head(
                #         tags$style(HTML(
                #           "#alignment_correction_method {
                #           inline-size: 250px;
                #           overflow-wrap: break-word;
                # }")),
                        tags$style(type="text/css", "#spine_plan.recalculating { opacity: 1.0; }"),
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
                  "#patient_details_redcap_table {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#surgical_details_redcap_table {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#intraoperative_details_redcap_table {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#rods_crossing_by_level_table {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#procedures_by_level_redcap_table {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#screw_details_redcap_table {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#interbody_details_redcap_table {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#all_inputs_trimmed_table {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#patient_details_redcap_table_modal {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#surgical_details_redcap_table_modal {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#intraoperative_details_redcap_table_modal {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#rods_crossing_by_level_table_modal {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#procedures_by_level_redcap_table_modal {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#screw_details_redcap_table_modal {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#interbody_details_redcap_table_modal {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#all_inputs_trimmed_table_modal {
                        overflow-x: auto;
                }"),
                
                tags$style(
                  "#complication_for_redcap_upload_table {
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
                  "#posterior_approach_objects_for_op_note_df {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#full_objects_passed_to_posterior_op_note {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#anterior_approach_objects_for_op_note_df {
                        overflow-x: auto;
                }"),
                tags$style(
                  "#full_objects_passed_to_anterior_op_note {
                        overflow-x: auto;
                }"),
                tags$style(".nav-tabs-custom>.nav-tabs { width: max-content}"),
                      ),
                tabItems(
                  tabItem(tabName = "patient_details_procedures",
                          column(width = 3, 
                                 box(width = 12, title = tags$div(style = "font-size:22px; font-weight:bold", "Patient Details:"), solidHeader = TRUE, status = "info",collapsible = TRUE,
                                     # uiOutput(outputId = "patient_details_ui"),
                                     tags$div(style = "font-size:24px; font-weight:bold; color:darkblue; font-family:sans-serif; font-style:italic", 
                                              htmlOutput(outputId = "patient_details_text")
                                     ),
                                     br(),
                                     actionBttn(inputId = "open_patient_details_modal", label = "Edit Patient Details", icon = icon("fas fa-user-edit", verify_fa = FALSE), size = "sm", block = TRUE)
                                 ),
                                 br(),
                                 box(width = 12, title = tags$div(style = "font-size:22px; font-weight:bold", "Diagnosis, Symptoms, Procedure Details:"), solidHeader = TRUE, status = "info",collapsible = TRUE,
                                     # uiOutput(outputId = "diagnosis_symptoms_ui"),
                                     tags$div(style = "font-size:20px; font-weight:bold; color:darkblue; font-family:sans-serif; font-style:italic", 
                                              htmlOutput(outputId = "diagnosis_symptoms_text")
                                              ),
                                     br(),
                                     actionBttn(inputId = "open_diagnosis_symptoms_procedure_modal", label = "Edit", icon = icon("fas fa-user-edit", verify_fa = FALSE), size = "sm", block = TRUE)
                                 ),
                                 switchInput(
                                   inputId = "fusion_procedure_performed",
                                   width = '100%',
                                   inline = TRUE,
                                   label = "Implants:",
                                   onLabel = "Yes",
                                   offLabel = "No",
                                   value = FALSE,
                                   size = "mini"
                                 ),
                                 dropdownButton(size = "xs",
                                   switchInput(label = "tumor diagnosis:",
                                               onLabel = "Yes",
                                               offLabel = "No",
                                               inputId = "tumor_diagnosis_true_false",
                                               value = FALSE
                                   ),
                                   switchInput(label = "intervertebral_cage True false:",
                                               onLabel = "Yes",
                                               offLabel = "No",
                                               inputId = "intervertebral_cage_true_false",
                                               value = FALSE
                                   ),
                                   switchInput(label = "Left C1 Lateral Mass Screw, True false:",
                                               onLabel = "Yes",
                                               offLabel = "No",
                                               inputId = "left_c1_lateral_mass_screw_true_false",
                                               value = FALSE
                                   ),
                                   switchInput(label = "Right C1 Lateral Mass Screw, True false:",
                                               onLabel = "Yes",
                                               offLabel = "No",
                                               inputId = "right_c1_lateral_mass_screw_true_false",
                                               value = FALSE
                                   ),
                                   circle = TRUE
                                 )
                          ),
                          column(width = 9, 
                                 box(width = 12, title = tags$div(style = "font-size:22px; font-weight:bold", "Surgical Procedures:"), solidHeader = TRUE, status = "primary",
                                     fluidRow(
                                       box(width = 7, 
                                           fluidRow(
                                             column(width =  11, 
                                                    tags$table(
                                                      tags$tr(width = "100%",
                                                              tags$td(width = "30%", tags$div(style = "font-size:18px; font-weight:bold; text-align:left", "Select Approach:")),
                                                              tags$td(width = "70%", 
                                                                      radioGroupButtons(inputId = "spine_approach", 
                                                                                        label = NULL,
                                                                                        choiceNames = list(tags$span(icon("fas fa-smile-beam", style = "color: steelblue", verify_fa = FALSE), strong("Anterior or Lateral")),
                                                                                                           tags$span(icon("fas fa-user", style = "color: steelblue", verify_fa = FALSE), strong("Posterior"))),
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
                                                        max = 28
                                                      ),
                                                      sliderInput(
                                                        inputId = "label_text_offset",
                                                        label = "Move Labels Lateral",
                                                        min = -20,
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
                                                      icon = icon("fas fa-cog", verify_fa = FALSE),
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
                                                                            icon = icon("fas fa-asterisk", verify_fa = FALSE), 
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
                                                                    height = "600px",
                                                                    width = "3px",
                                                                    inline = TRUE)
                                             ), 
                                             column(width = 10,
                                                    div(style = "font-size:16px; font-weight:bold; font-style:italic; text-align:center", "Add Procedures in Order Performed. Double click to remove an item."),
                                                    tags$table(
                                                      tags$tr(
                                                        tags$td(
                                                          tags$div(style = "font-size:16px; font-weight:bold; font-family:sans-serif", "Currently Adding: ")
                                                        ),
                                                        tags$td(
                                                          # htmlOutput(outputId = "currently_adding_text")
                                                          tags$div(style = "font-size:16px; font-weight:bold; color:red; font-family:sans-serif; font-style:italic",
                                                                   textOutput(outputId = "currently_adding_text")
                                                          )
                                                        )
                                                      )
                                                    ),
                                                    plotOutput("spine_plan",
                                                               height = 750,
                                                               click = "plot_click",
                                                               dblclick = "plot_double_click")
                                             )   
                                           )
                                       ), # end of left box with spine plot
                                       box(width = 5, ##### RIGHT COLUMN NEXT TO SPINE PLOT
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
                                                              label = "Add Other (cement, structural allograft, I&D, Nerve transection, etc)",
                                                              style = "simple",
                                                              color = "primary"
                                                            ),
                                                            br(),
                                                            # uiOutput(outputId = "intervertebral_cage_ui"),
                                                            conditionalPanel(condition = "input.intervertebral_cage_true_false == true",
                                                                             actionBttn(
                                                                               inputId = "add_intervertebral_cage",
                                                                               size = "sm", block = TRUE,
                                                                               label = "Add Intervertebral Cage (After VCR/Corpectomy)",
                                                                               style = "simple",
                                                                               color = "primary"
                                                                             )),
                                                            br(),
                                                            conditionalPanel(condition = "input.tumor_diagnosis_true_false == true",
                                                                             actionBttn(
                                                                               inputId = "add_tumor_resection",
                                                                               size = "sm", 
                                                                               block = TRUE,
                                                                               label = "Add Resection & Decompression for TUMOR",
                                                                               style = "simple",
                                                                               color = "primary"
                                                                             )),
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
                                                 yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue", verify_fa = FALSE)
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
                                     ), # end of the fluid row for the 2 boxes containing spine and procedure inputs
                                     fluidRow(
                                       box(width = 7, status = "info", 
                                           title = div(style = "font-size:22px; font-weight:bold; text-align:left", 
                                                       "Construct Details:"),
                                           fluidRow(
                                             column(width = 4, 
                                                    dropdownButton(size = "xs", 
                                                                   label = NULL, 
                                                                   switchInput(label = "Left Supplemental Rods Eligible:",
                                                                               inputId = "left_supplemental_rods_eligible",
                                                                               size = "mini",
                                                                               onLabel = "Yes",
                                                                               offLabel = "No", 
                                                                               value = FALSE
                                                                   ),
                                                                   switchInput(
                                                                     inputId = "left_revision_rod_present",
                                                                     value = FALSE,
                                                                     onLabel = "Prior",
                                                                     size = "mini"
                                                                   ),
                                                                   switchInput(label = "Right Supplemental Rods Eligible:",
                                                                               inputId = "right_supplemental_rods_eligible",
                                                                               size = "mini",
                                                                               onLabel = "Yes",
                                                                               offLabel = "No", 
                                                                               value = FALSE
                                                                   ),
                                                                   switchInput(
                                                                     inputId = "right_revision_rod_present",
                                                                     value = FALSE,
                                                                     onLabel = "Prior",
                                                                     size = "mini"
                                                                   )
                                                    )
                                             )
                                           ),
                                           conditionalPanel(condition = "input.fusion_procedure_performed == true & input.spine_approach.indexOf('Posterior') > -1",
                                                            fluidRow(
                                                              actionBttn(
                                                                inputId = "add_rods",
                                                                size = "md", 
                                                                block = TRUE,
                                                                label = "ADD RODS",
                                                                style = "bordered",
                                                                color = "primary", 
                                                                icon = icon("grip-lines-vertical")
                                                              )
                                                            )
                                           ),
                                           br(),
                                           # tableOutput(outputId = "revision_screws_df"),
                                           conditionalPanel(condition = "input.add_rods >0 & input.fusion_procedure_performed == true & input.spine_approach.indexOf('Posterior') > -1",
                                                            fluidRow(column(4, 
                                                                            conditionalPanel(condition = "input.left_revision_rod_present == true", 
                                                                                             materialSwitch(
                                                                                               inputId = "link_left_revision_rods_true_false",
                                                                                               label = "Link Prior Left Rod to new rod?", 
                                                                                               value = FALSE,
                                                                                               status = "success"
                                                                                             )
                                                                                             )
                                                                            ), 
                                                                     column(4, 
                                                                            dropdown(icon = icon("link"),
                                                                                     inputId = "add_crosslink_button", 
                                                                                     width = "100%",
                                                                                     label = "Add Crosslink",
                                                                                     style = "unite",
                                                                                     column(6, 
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
                                                                                            )),
                                                                                     column(6, 
                                                                                            actionBttn(
                                                                                              inputId = "remove_all_crosslinks",
                                                                                              label = "Remove All",
                                                                                              style = "simple", 
                                                                                              size = "xs",
                                                                                              color = "danger", 
                                                                                              icon = icon("undo-alt", verify_fa = FALSE)
                                                                                            )
                                                                                     )
                                                                                     
                                                                            )
                                                                     ),
                                                                     column(4,
                                                                            conditionalPanel(condition = "input.right_revision_rod_present == true", 
                                                                                             materialSwitch(
                                                                                               inputId = "link_right_revision_rods_true_false",
                                                                                               label = "Link Prior Right Rod to new rod?", 
                                                                                               value = FALSE,
                                                                                               status = "success"
                                                                                             )
                                                                            )

                                                                     )
                                                            ),
                                                            jh_make_shiny_table_column_function(input_type = "title", 
                                                                                                left_label = "Left Rod(s):",
                                                                                                right_label = "Right Rods(s):", 
                                                                                                font_size = 20, 
                                                                                                text_align = "left"),
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
                                                            jh_make_shiny_table_column_function(input_type = "awesomeRadio", 
                                                                                                left_input_id = "left_main_rod_contour", 
                                                                                                left_label = "Rod was:",
                                                                                                right_input_id = "right_main_rod_contour", 
                                                                                                right_label = "Material:",
                                                                                                left_column_percent_width = 50,
                                                                                                right_column_percent_width = 50,
                                                                                                checkboxes_inline = TRUE,
                                                                                                button_size = "normal",
                                                                                                choices_vector = c("Cut to length and contoured", "Pre-contoured"),
                                                                                                initial_value_selected = "Cut to length and contoured",
                                                                                                picker_choose_multiple = FALSE),
                                                            jh_make_supplemental_rod_ui_function(rod_type = "accessory_rod", input_label = "Accessory Rod"),
                                                            jh_make_supplemental_rod_ui_function(rod_type = "satellite_rod", input_label = "Satellite Rod"),
                                                            jh_make_supplemental_rod_ui_function(rod_type = "intercalary_rod", input_label = "Intercalary Rod"),
                                                            jh_make_supplemental_rod_ui_function(rod_type = "linked_rod", input_label = "Linked Rods"),
                                                            jh_make_supplemental_rod_ui_function(rod_type = "kickstand_rod", input_label = "Kickstand Rod"),
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
                                                                                      awesomeRadio(inputId = "left_custom_rods_number",
                                                                                                   label = "Number of Left Total Rods:",
                                                                                                   choices = c(2,3,4,5), 
                                                                                                   inline = TRUE, selected = 2),
                                                                                      column(12,
                                                                                             pickerInput(inputId = "left_custom_rod_1",label = "Rod 1 Connects to:",
                                                                                                         choices = c("a"),
                                                                                                         multiple = TRUE,
                                                                                                         options = list(`actions-box` = TRUE)),
                                                                                             pickerInput(inputId = "left_custom_rod_2",label = "Rod 2 Connects to:",
                                                                                                         choices = c("a"),
                                                                                                         multiple = TRUE,
                                                                                                         options = list(`actions-box` = TRUE)),
                                                                                             conditionalPanel(condition = "input.left_custom_rods_number > 2",
                                                                                                              pickerInput(inputId = "left_custom_rod_3",label = "Rod 3 Connects to:",
                                                                                                                          choices = c("a"),
                                                                                                                          multiple = TRUE,
                                                                                                                          options = list(`actions-box` = TRUE))
                                                                                             ),
                                                                                             conditionalPanel(condition = "input.left_custom_rods_number > 3",
                                                                                                              pickerInput(inputId = "left_custom_rod_4",label = "Rod 4 Connects to:",
                                                                                                                          choices = c("a"),
                                                                                                                          multiple = TRUE,
                                                                                                                          options = list(`actions-box` = TRUE))
                                                                                             ),
                                                                                             conditionalPanel(condition = "input.left_custom_rods_number > 4",
                                                                                                              pickerInput(inputId = "left_custom_rod_5",label = "Rod 5 Connects to:",
                                                                                                                          choices = c("a"),
                                                                                                                          multiple = TRUE,
                                                                                                                          options = list(`actions-box` = TRUE))
                                                                                             )
                                                                                      )
                                                                                      # uiOutput(outputId = "left_custom_rods_ui")
                                                                     )
                                                              ),
                                                              column(width = 6,
                                                                     conditionalPanel(condition = "input.add_right_custom_rods == true",
                                                                                      awesomeRadio(inputId = "right_custom_rods_number", 
                                                                                                   label = "Number of Right Total Rods:",
                                                                                                   choices = c(2,3,4,5), 
                                                                                                   inline = TRUE, 
                                                                                                   selected = 2),
                                                                                      column(12,
                                                                                             pickerInput(inputId = "right_custom_rod_1",label = "Rod 1 Connects to:",
                                                                                                         choices = c("a"),
                                                                                                         multiple = TRUE,
                                                                                                         options = list(`actions-box` = TRUE)),
                                                                                             pickerInput(inputId = "right_custom_rod_2",label = "Rod 2 Connects to:",
                                                                                                         choices = c("a"),
                                                                                                         multiple = TRUE,
                                                                                                         options = list(`actions-box` = TRUE)),
                                                                                             conditionalPanel(condition = "input.right_custom_rods_number > 2",
                                                                                                              pickerInput(inputId = "right_custom_rod_3",label = "Rod 3 Connects to:",
                                                                                                                          choices = c("a"),
                                                                                                                          multiple = TRUE,
                                                                                                                          options = list(`actions-box` = TRUE))
                                                                                                              ),
                                                                                             conditionalPanel(condition = "input.right_custom_rods_number > 3",
                                                                                                              pickerInput(inputId = "right_custom_rod_4",label = "Rod 4 Connects to:",
                                                                                                                          choices = c("a"),
                                                                                                                          multiple = TRUE,
                                                                                                                          options = list(`actions-box` = TRUE))
                                                                                             ),
                                                                                             conditionalPanel(condition = "input.right_custom_rods_number > 4",
                                                                                                              pickerInput(inputId = "right_custom_rod_5",label = "Rod 5 Connects to:",
                                                                                                                          choices = c("a"),
                                                                                                                          multiple = TRUE,
                                                                                                                          options = list(`actions-box` = TRUE))
                                                                                             )
                                                                                      )
                                                                                      # uiOutput(outputId = "right_custom_rods_ui")
                                                                     )
                                                              )
                                                            )
                                                            # )
                                                            ##################### NEW
                                           )
                                       ) ## end of the construct details box
                                     ) ## end of the fluid row
                                 ),### end of the full box
                                 conditionalPanel(condition = "input.multiple_approach == true",
                                                  actionBttn(
                                                    inputId = "proceed_to_other_approach",
                                                    size = "md", 
                                                    block = TRUE,
                                                    label = "Done with Posterior, Switch to Anterior Approach",
                                                    style = "simple",
                                                    color = "success", 
                                                    icon = icon("fas fa-arrow-circle-right", verify_fa = FALSE)
                                                  )
                                                  ),
                                 br(),
                                 actionBttn(
                                   inputId = "implants_complete",
                                   size = "md", 
                                   block = TRUE,
                                   label = "Click when finished to Add Implant Details",
                                   style = "simple",
                                   color = "success", 
                                   icon = icon("fas fa-arrow-circle-right", verify_fa = FALSE)
                                 )
                          ) ## closes the right column 
                  ),
                  tabItem(tabName = "implant_details",   
                          ###########################################
                          box(width = 7, status = "info", title = div(style = "font-size:22px; font-weight:bold; text-align:left", "Implant & Fusion Details:"),
                              switchInput(
                                inputId = "posterior_fusion_performed",
                                width = '200%',
                                inline = TRUE,
                                label = "Posterior Fusion:",
                                onLabel = "Yes",
                                offLabel = "No",
                                value = FALSE,
                                size = "mini"
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
                                                                                            "iFactor", 
                                                                                            "Other"
                                                                                )
                                                            ),
                                                            conditionalPanel(
                                                              condition = "input.posterior_biologics.indexOf('Other') > -1",
                                                              jh_make_shiny_table_row_function(left_column_label = "Amount & Name of Biologic:",
                                                                                               bottom_margin = "5px",
                                                                                               left_column_percent_width = 50,
                                                                                               font_size = 16,
                                                                                               input_type = "text",
                                                                                               input_id = "posterior_biologics_other")
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
                                                                                               min = 0, 
                                                                                               max = 500, 
                                                                                               step = 30)
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
                                                                                               initial_value_selected = 0, 
                                                                                               min = 0, 
                                                                                               max = 500, 
                                                                                               step = 2.5)
                                                            ),
                                                            conditionalPanel(
                                                              condition = "input.posterior_biologics.indexOf('iFactor') > -1",
                                                              jh_make_shiny_table_row_function(left_column_label = "iFactor Volume (cc):",bottom_margin = "5px",
                                                                                               left_column_percent_width = 60,
                                                                                               font_size = 16,
                                                                                               input_type = "numeric",
                                                                                               input_id = "posterior_ifactor_volume",
                                                                                               initial_value_selected = 0, 
                                                                                               min = 0, 
                                                                                               max = 50, 
                                                                                               step = 1)
                                                            )
                                                     )
                                                   ),
                                                   jh_make_bmp_ui_function(anterior_posterior = "posterior"),
                                               )
                              ),
                              conditionalPanel(condition = "input.anterior_fusion_performed == true",
                                               box(width = 12, collapsible = TRUE, title = div(style = "font-size:20px; font-weight:bold; text-align:center",
                                                                                               "ANTERIOR Bone Graft & Biologics:"),
                                                   fluidRow(
                                                     column(6,
                                                            div(style = "font-size:16px; font-weight:bold; text-align:left", "Select any bone graft used:"),
                                                            prettyCheckboxGroup(inputId = "anterior_bone_graft", 
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
                                                            prettyCheckboxGroup(inputId = "anterior_biologics", 
                                                                                shape = "curve", outline = TRUE, status = "primary", width = "95%",bigger = TRUE,
                                                                                label = NULL,
                                                                                choices = c("Bone Marrow Aspirate",
                                                                                            "Cell Based Allograft",
                                                                                            "DBM",
                                                                                            "iFactor",
                                                                                            "Other"
                                                                                )
                                                            ),
                                                            conditionalPanel(
                                                              condition = "input.anterior_biologics.indexOf('Other') > -1",
                                                              jh_make_shiny_table_row_function(left_column_label = "Enter: '(Amount) of (Name of Biologic)':",
                                                                                               bottom_margin = "5px",
                                                                                               left_column_percent_width = 50,
                                                                                               font_size = 16,
                                                                                               input_type = "text",
                                                                                               input_id = "anterior_biologics_other")
                                                            )
                                                     )
                                                   ),
                                                   fluidRow(
                                                     column(6,
                                                            conditionalPanel(
                                                              condition = "input.anterior_bone_graft.indexOf('Morselized Allograft') > -1",
                                                              jh_make_shiny_table_row_function(left_column_label = "Allograft (cc):",
                                                                                               left_column_percent_width = 60, 
                                                                                               font_size = 16,
                                                                                               input_type = "numeric",
                                                                                               input_id = "anterior_allograft_amount", 
                                                                                               initial_value_selected = 0, 
                                                                                               min = 0, 
                                                                                               max = 500, 
                                                                                               step = 30)
                                                            ),
                                                     ),
                                                     column(6,
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
                                                                                               input_id = "anterior_dbm_volume", 
                                                                                               initial_value_selected = 0,
                                                                                               min = 0, 
                                                                                               max = 500,
                                                                                               step = 5)
                                                            ),
                                                            conditionalPanel(
                                                              condition = "input.anterior_biologics.indexOf('iFactor') > -1",
                                                              jh_make_shiny_table_row_function(left_column_label = "iFactor Volume (cc):",bottom_margin = "5px",
                                                                                               left_column_percent_width = 60,
                                                                                               font_size = 16,
                                                                                               input_type = "numeric",
                                                                                               input_id = "anterior_ifactor_volume",
                                                                                               initial_value_selected = 0, 
                                                                                               min = 0, 
                                                                                               max = 50,
                                                                                               step = 1)
                                                            )
                                                     )
                                                   ),
                                                   jh_make_bmp_ui_function(anterior_posterior = "anterior")
                                               )
                              ),
                              switchInput(
                                inputId = "anterior_fusion_performed",
                                width = '100%',
                                inline = TRUE,
                                label = "Anterior Fusion:",
                                onLabel = "Yes",
                                offLabel = "No",
                                value = FALSE,
                                size = "mini"
                              ),
                              hr(),
                              dropdownButton(size = "xs",circle = TRUE,
                                pickerInput(inputId = "interbody_implant_picker",
                                            label = "Interbodies", 
                                            choices = c("a"), 
                                            multiple = TRUE,
                                            width = "fit"), 
                                checkboxGroupInput(inputId = "screws_implanted_picker_for_ui", 
                                            label = "Screws:", 
                                            choices = c("")
                                            )
                                ),
                              uiOutput(outputId = "interbody_details_ui"),
                              hr(),
                              uiOutput(outputId = "screw_details_ui"),
                              hr(),
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
                                  icon = icon("fas fa-arrow-circle-left", verify_fa = FALSE)
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
                                  icon = icon("fas fa-arrow-circle-right", verify_fa = FALSE)
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
                                  icon = icon("fas fa-arrow-circle-left", verify_fa = FALSE)
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
                                icon = icon("fas fa-user-edit", verify_fa = FALSE),
                              ),
                              tableOutput(outputId = "additional_surgical_details_table"),
                          ),
                          box(width = 8, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Data Upload:"),status = "success", solidHeader = TRUE,
                              column(width = 5, 
                                     actionBttn(inputId = "preview_redcap_upload", label = "Upload to Redcap Project", icon = icon("upload"), style = "jelly", color = "primary", size = "md")
                                     ),
                              column(width = 7, 
                                     tags$div(style = "font-size:24px; font-weight:bold; color:darkblue; font-family:sans-serif; font-style:italic", 
                                              htmlOutput(outputId = "upload_completed_text")
                                     )
                                     #UPLOAD NOT COMPLETED
                              )
                          ),
                          box(width = 8, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Operative Note Generator:"),status = "success", solidHeader = TRUE,
                              conditionalPanel(condition = "input.approach_sequence == 'posterior' || input.approach_sequence == 'posterior-anterior' || input.approach_sequence == 'posterior-anterior-posterior' || input.approach_sequence == 'anterior-posterior'", 
                                               jh_make_shiny_table_row_function(
                                                 left_column_percent_width = 25,
                                                 left_column_label = "Confirm & Edit POSTERIOR Procedures Performed:",
                                                 font_size = 14,
                                                 input_type = "textAreaInput",
                                                 input_id = "procedures_numbered_confirm_edit_posterior",
                                                 initial_value_selected = "***"
                                               )
                                               ),
                              conditionalPanel(condition = "input.approach_sequence == 'anterior' || input.approach_sequence == 'posterior-anterior' || input.approach_sequence == 'posterior-anterior-posterior' || input.approach_sequence == 'anterior-posterior'", 
                                               jh_make_shiny_table_row_function(
                                                 left_column_percent_width = 25,
                                                 left_column_label = "Confirm & Edit ANTERIOR Procedures Performed:",
                                                 font_size = 14,
                                                 input_type = "textAreaInput",
                                                 input_id = "procedures_numbered_confirm_edit_anterior",
                                                 initial_value_selected = "***"
                                               )
                              ),
                              actionBttn(
                                inputId = "generate_operative_note",
                                block = TRUE,
                                size = "md",
                                label = "Click to Generate Operative Note",
                                style = "float",
                                color = "primary"
                              ),
                              br(),
                              textAreaInput(inputId = "operative_note_text", label = "Operative Note:", width = "100%", height = 500),
                              uiOutput("clipboard_ui"),
                              br(), 
                              hr(),
                              h3("Formatted Operative Report:"),
                              p(em("Edits above will be automatically reflected here.")),
                              br(),
                              htmlOutput(outputId = "operative_note_formatted"),
                              br(), 
                              hr(),
                              p(em("To copy the formatted text, you must highlight the formatted text and copy, otherwise you can click below:"))
                          )
                          ###########################################
                  ),
                  tabItem(tabName = "tables",
                          #         ###########################################
                          h3("Tables for Redcap Upload:"),
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Patient Details"),status = "success", collapsible = TRUE,solidHeader = TRUE,
                              column(6, 
                                     tableOutput(outputId = "patient_details_redcap_table")
                                     ),
                              column(6, 
                                     pickerInput(
                                       inputId = "approach_sequence",
                                       label = "Spine Approach/Sequence:", 
                                       choices = list("Posterior" = "posterior", 
                                                      "Anterior/Lateral" = "anterior", 
                                                      "Posterior-Anterior/Lateral" = "posterior-anterior",
                                                      "Anterior/Lateral-Posterior" = "anterior-posterior",
                                                      "Posterior-Anterior/Lateral-Posterior" = "posterior-anterior-posterior" 
                                       ),
                                       selected = "posterior", 
                                     )
                              )
                              
                          ),
                          fluidRow(
                            column(4, 
                                   box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Surgical Details"),status = "success", collapsible = TRUE,solidHeader = TRUE,
                                       tableOutput(outputId = "surgical_details_redcap_table")
                                   )
                            ),
                            column(4,
                                   box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Rods Crossing by Level:"),status = "success", collapsible = TRUE,solidHeader = TRUE,
                                       tableOutput(outputId = "rods_crossing_by_level_table")
                                   ),
                                   box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Intraoperative Details"),status = "success", collapsible = TRUE,solidHeader = TRUE,
                                       tableOutput(outputId = "intraoperative_details_redcap_table")
                                   )
                            ),
                            column(4, 
                                   plotOutput("spine_plot_for_tables_tab",
                                              height = 600)
                            )
                          ),
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Procedures by Level:"),status = "success", collapsible = TRUE,solidHeader = TRUE,
                              tableOutput(outputId = "procedures_by_level_redcap_table")
                          ),
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Screw Size Details:"), status = "success", collapsible = TRUE,solidHeader = TRUE,
                              tableOutput(outputId = "screw_details_redcap_table")
                          ),
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Interbody Details:"), status = "success", collapsible = TRUE,solidHeader = TRUE,
                              tableOutput(outputId = "interbody_details_redcap_table")
                          ),
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "All Inputs:"), status = "success", collapsible = TRUE, solidHeader = TRUE, 
                              tableOutput(outputId = "all_inputs_trimmed_table")),
                          hr(),
                          hr(),
                          h4("Other Tables (not for Redcap Upload)"),
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "All objects table:"),status = "success", collapsible = TRUE,solidHeader = TRUE,
                              tableOutput(outputId = "all_objects_table")
                          ),
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Revision Implants table:"), status = "success", collapsible = TRUE,solidHeader = TRUE,
                              tableOutput(outputId = "revision_implants_table")
                          ),
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Anterior Revision Implants table:"), status = "success", collapsible = TRUE,solidHeader = TRUE,
                              tableOutput(outputId = "anterior_revision_implants_table")
                          ),

                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Posterior Approach Objects for Op Note:"), status = "success", collapsible = TRUE,solidHeader = TRUE, 
                              tableOutput(outputId = "posterior_approach_objects_for_op_note_df")),
                          
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Posterior: Objects passed to Op Note Generater:"), status = "success", collapsible = TRUE, solidHeader = TRUE, 
                              verbatimTextOutput(outputId = "full_objects_passed_to_posterior_op_note")),
                          
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Anterior Approach Objects for Op Note:"), status = "success", collapsible = TRUE,solidHeader = TRUE, 
                              tableOutput(outputId = "anterior_approach_objects_for_op_note_df")),
                          
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Anterior: Objects passed to Op Note Generater:"), status = "success", collapsible = TRUE, solidHeader = TRUE, 
                              verbatimTextOutput(outputId = "full_objects_passed_to_anterior_op_note")),
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Anterior: Objects passed to Op Note Generater/DATAFRAME:"), status = "success", collapsible = TRUE, solidHeader = TRUE, 
                              tableOutput(outputId = "full_objects_passed_to_anterior_op_note_df")),
                          box(width = 12, title = div(style = "font-size:22px; font-weight:bold; text-align:center", "All Inputs Not Logged:"), status = "success", collapsible = TRUE, solidHeader = TRUE, 
                              tableOutput(outputId = "all_inputs_removed")
                              )
                          #         ###########################################
                  )
                )
                    )
)
# )

##########################  #################################### UI END ####################################  ####################################
##########################  #################################### UI END ####################################  ####################################
##########################  #################################### UI END ####################################  ####################################
##########################  #################################### UI END ####################################  ####################################


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
  # options(shiny.trace = TRUE, shiny.error = browser, shiny.fullstacktrace = TRUE)
  # options(shiny.reactlog=TRUE) 
  
  logging_timer_reactive_list <- reactiveValues()
  logging_timer_reactive_list$elapsed_time <- 0
  

  observeEvent(input$close_startup_modal, ignoreInit = TRUE, once = TRUE, {
    logging_timer_reactive_list$start_time <- as.numeric(Sys.time())
  })
  
  observeEvent(input$preview_redcap_upload, ignoreInit = TRUE, {
    logging_timer_reactive_list$elapsed_time <- round(as.numeric(Sys.time()) - logging_timer_reactive_list$start_time, 1)
    
  })
  
  output$timer <- renderText({
    paste(logging_timer_reactive_list$elapsed_time, "seconds elapsed")
  })
  

  
  observeEvent(input$multi_approach_starting_position, ignoreInit = TRUE, {
    if(input$multiple_approach == TRUE){
      if(input$multi_approach_starting_position != "Posterior"){
        updateActionButton(session = session, inputId = "proceed_to_other_approach", label = "Done with Anterior, Switch to Posterior")
      }
    }
  })
  
  
  observeEvent(input$proceed_to_other_approach, ignoreInit = TRUE, {
    if(input$spine_approach == "Anterior"){
      updateRadioGroupButtons(session = session,
                              inputId = "spine_approach", 
                              label = NULL,
                              choiceNames = list(tags$span(icon("fas fa-smile-beam", style = "color: steelblue"), strong("Anterior or Lateral")),
                                                 tags$span(icon("fas fa-user", style = "color: steelblue"), strong("Posterior"))),
                              choiceValues = list("Anterior", "Posterior"),
                              selected = "Posterior", 
                              checkIcon = list(yes = icon("check"))
                              )
    }else{
      updateRadioGroupButtons(session = session,
                              inputId = "spine_approach", 
                              label = NULL,
                              choiceNames = list(tags$span(icon("fas fa-smile-beam", style = "color: steelblue"), strong("Anterior or Lateral")),
                                                 tags$span(icon("fas fa-user", style = "color: steelblue"), strong("Posterior"))),
                              choiceValues = list("Anterior", "Posterior"),
                              selected = "Anterior", 
                              checkIcon = list(yes = icon("check"))
      )
    }
  })
  

  rcon_reactive <- reactiveValues()
  
  observeEvent(input$redcap_token, ignoreInit = TRUE, {
    
    redcap_url <- case_when(
      input$redcap_institution == "UTHSCSA" ~ 'https://redcap.uthscsa.edu/REDCap/api/',
      input$redcap_institution == "UCSD" ~ 'https://redcap.ucsd.edu/api/'
    )
    
    rcon_reactive$rcon <- redcapConnection(url = redcap_url, token = input$redcap_token)    
    
  })
  
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
  # jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left") <- tibble(level = character(),
  #                                              approach = character(),
  #                                              category = character(),
  #                                              vertebral_number = double(),
  #                                              implant = character(),
  #                                              object = character(),
  #                                              side = character(),
  #                                              x = double(),
  #                                              y = double(),
  #                                              fusion = character(),
  #                                              interbody_fusion = character(),
  #                                              body_interspace = character(),
  #                                              fixation_uiv_liv = character(),
  #                                              object_constructed = list())
  # jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right") <- tibble(level = character(),
  #                                              approach = character(),
  #                                              category = character(),
  #                                              vertebral_number = double(),
  #                                              implant = character(),
  #                                              object = character(),
  #                                              side = character(),
  #                                              x = double(),
  #                                              y = double(),
  #                                              fusion = character(),
  #                                              interbody_fusion = character(),
  #                                              body_interspace = character(),
  #                                              fixation_uiv_liv = character(),
  #                                              object_constructed = list())
  
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   Startup  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   Startup  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   Startup  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  
  
  ################################################    INITIAL STARTUP MODAL ######################################
  ################################################    INITIAL STARTUP MODAL ######################################
  ################################################    INITIAL STARTUP MODAL ######################################
  
  
  # showModal(
  #   startup_modal_box(starting_first_name = "",
  #                     starting_last_name = "",
  #                     starting_dob = "",
  #                     starting_dos = "")
  #   )
  
  showModal(
    passcode_modal_box()
    )
  
  # observeEvent(input$passcode_correct, {
  observeEvent(input$passcode, {
    if(input$passcode == "kennypowers"){
      removeModal()
      showModal(
        startup_modal_box(starting_first_name = "",
                          starting_last_name = "",
                          starting_dob = "",
                          starting_dos = "")
      ) 
    }
  }
  )

  observeEvent(input$test_patient_button, {
    updateTextInput(session = session, inputId = "patient_last_name", value = paste0("TestLAST", sample(1:1000, 1)))
    updateTextInput(session = session, inputId = "patient_first_name", value = "TestFIRST")
    updateDateInput(session = session, inputId = "date_of_birth", value = date("1970-01-05"))
    updateDateInput(session = session, inputId = "date_of_surgery", value = Sys.Date())
    updateAwesomeRadio(session = session, inputId = "sex", selected = "Male")
    updateRadioGroupButtons(session = session, inputId = "head_positioning_posterior", selected = "Cranial Tongs")
    updateRadioGroupButtons(session = session, inputId = "intraoperative_complications_yes_no", selected = "No")
    updateAwesomeCheckboxGroup(session = session, inputId = "closure_details_posterior", selected = "Staples")
    updateAwesomeCheckboxGroup(session = session, inputId = "dressing_details_posterior", selected = "Steristrips")
  })
  
  

  observeEvent(input$open_patient_details_modal, ignoreInit = TRUE, ignoreNULL = TRUE, {
    if(input$open_patient_details_modal > 0){
      showModal(startup_modal_box(starting_first_name = input$patient_first_name, 
                                  starting_last_name = input$patient_last_name, 
                                  starting_dob = input$date_of_birth,
                                  starting_dos = input$date_of_surgery, 
                                  starting_sex = input$sex, 
                                  hospital_input = input$hospital, 
                                  redcap_token_input = input$redcap_token,
                                  button_proceed = "exit"
      )) 
    }
  })
  
  
  ############### RETRIEVE EXISTING PATIENT #########################
  existing_patient_data <- reactiveValues()
  existing_patient_data$match_found <- FALSE
  existing_patient_data$patient_df_full <- tibble(record_id = double(), 
                                                  date_of_surgery = character(), 
                                                  approach = character(), 
                                                  side = character(),
                                                  level = character(), 
                                                  object = character())
  existing_patient_data$patient_df <- tibble(record_id = double(), 
                                             date_of_surgery = character(), 
                                             approach = character(), 
                                             side = character(),
                                             level = character(), 
                                             object = character())
  
  observeEvent(input$search_for_prior_patient, ignoreInit = TRUE, {
    
    all_patient_ids_df <- exportRecords(rcon = rcon_reactive$rcon, fields = c("record_id", "last_name", "first_name", "date_of_birth"), 
                                        events = "enrollment_arm_1") %>%
      type.convert(as.is = TRUE) %>%
      select(record_id, last_name, first_name, date_of_birth) %>%
      mutate(last_name = str_to_lower(last_name),
             first_name = str_to_lower(first_name))
    
    if(nrow(all_patient_ids_df)>0){
      joined_df <- all_patient_ids_df %>%
        filter(last_name == str_to_lower(input$patient_last_name),  
               first_name == str_to_lower(input$patient_first_name),
               date_of_birth == paste(input$date_of_birth))
      
      if(nrow(joined_df)>0){
        match_found <- TRUE
      }else{
        match_found <- FALSE
      }

      if(match_found == TRUE){
        record_number <- joined_df$record_id[[1]]
        
        existing_patient_data$patient_df_full <- exportRecords(rcon = rcon_reactive$rcon, records = record_number, fields = append(c("record_id", "dos_surg_repeating", "approach_repeating", "side", "object"), str_to_lower(str_replace_all(levels_vector, pattern = "-", replacement = "_")))) %>%                    as_tibble()  %>%
          type.convert(as.is = TRUE) %>%
          as_tibble() %>%
          filter(redcap_repeat_instrument == "procedures_by_level_repeating")  %>%
          mutate(across(.cols = everything(), .fns = ~ as.character(.x))) %>%
          select(-redcap_event_name,
                 -redcap_repeat_instrument,
                 -redcap_repeat_instance, 
                 -redcap_survey_identifier,
                 -patient_details_complete, 
                 -patient_details_timestamp, 
                 -procedures_by_level_repeating_complete) %>%
          pivot_longer(cols = c(-record_id, -approach_repeating, -side, -dos_surg_repeating), names_to = "level", values_to = "object") %>%
          filter(!is.na(object)) %>%
          rename(date_of_surgery = dos_surg_repeating)%>%
          mutate(level = str_to_title(str_replace_all(string = level, pattern = "_", replacement = "-"))) %>%
          filter(object != " ") %>%
          filter(object != "") %>%
          mutate(level = str_replace_all(string = level, pattern = "S2ai", replacement = "S2AI")) %>%
          rename(approach = approach_repeating)
        
        
        
        existing_patient_data$patient_df <- existing_patient_data$patient_df_full
        
        existing_patient_data$match_found <- match_found
        
        existing_patient_data$record_id <- record_number
        
        existing_patient_data$surgical_dates_df <- exportRecords(rcon = rcon_reactive$rcon, 
                                                                 records = existing_patient_data$record_id) %>%
          type.convert(as.is = TRUE) %>%
          mutate(last_name = str_to_lower(last_name),
                 first_name = str_to_lower(first_name)) %>%
          select(record_id, date_of_surgery, stage_number) %>%
          filter(!is.na(date_of_surgery)) %>%
          mutate(stage_number = as.double(stage_number)) %>%
          mutate(stage_number = if_else(is.na(stage_number), 1, stage_number)) %>%
          filter(stage_number == 1)
        
        existing_patient_data$prior_surgical_summary <- exportRecords(rcon = rcon_reactive$rcon, 
                                  records = existing_patient_data$record_id, 
                                  fields = c("record_id", 
                                             "date_of_surgery", 
                                             "main_approach", 
                                             "fusion", 
                                             "uiv",
                                             "upper_treated_vertebrae",
                                             "liv", "lower_treated_vertebrae",
                                             "pelvic_fixation")) %>%      
          as_tibble() %>%
          filter(redcap_repeat_instrument == "surgical_details") %>%
          remove_empty() %>%
          mutate(date_of_surgery = ymd(date_of_surgery))
        
        
      }
    }
    
  })
  
  observe({
    if(existing_patient_data$match_found == TRUE){
      updateSwitchInput(session = session, inputId = "prior_patient_match_located", value = TRUE, label = "Prior Patient Found")
    }
  }) %>%
    bindEvent(input$search_for_prior_patient,
              # existing_patient_data$match_found,
              ignoreInit = TRUE)
  
  
  output$patient_prior_data <- renderTable({
    if(existing_patient_data$match_found == TRUE){
      existing_patient_data$patient_df
    }else{
      tibble(patient = character())
    }
  })
  
  observeEvent(list(existing_patient_data$patient_df_full, input$prior_instrumentation), ignoreInit = TRUE,{
    if(existing_patient_data$match_found == TRUE){
      if(length(unique(existing_patient_data$patient_df_full$approach))==1){
        updateAwesomeRadio(session = session,
                           inputId = "revision_approach",
                           selected = unique(existing_patient_data$patient_df_full$approach))
      }
    }
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
  
  observeEvent(list(existing_patient_data$patient_df, input$close_startup_modal), ignoreInit = TRUE, {
    if(existing_patient_data$match_found == TRUE){
      if(nrow(existing_patient_data$patient_df %>% filter(str_detect(object, "screw") | str_detect(object, "hook") | str_detect(object, "plate"))) > 0){
        updateSwitchInput(session = session,
                          inputId = "prior_instrumentation",
                          value = TRUE)
      }
    }
  } )
  
  
  ########### UPDATE PRIOR IMPLANTS PRESENT WHEN PRIOR PATIENT FOUND #########
  observeEvent(list(existing_patient_data$patient_df, input$close_startup_modal), ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(existing_patient_data$match_found == TRUE){
      if(any(str_detect(existing_patient_data$patient_df$object, "plate")) == TRUE){
        
        anterior_plate_levels_df <- existing_patient_data$patient_df %>%
          filter(approach == "anterior") %>%
          filter(str_detect(object, "plate")) %>%
          distinct()
        
        updateAwesomeCheckboxGroup(session = session, 
                                   inputId = "prior_anterior_plate_levels", 
                                   selected = anterior_plate_levels_df$level)
      }
      }
  })
  
  observeEvent(list(existing_patient_data$patient_df, input$close_startup_modal), ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(existing_patient_data$match_found == TRUE){
      left_prior_implants_df <- existing_patient_data$patient_df %>%
        filter(str_detect(object, "screw") | str_detect(object, "hook")) %>%
        filter(side == "left") %>%
        distinct()%>%
        mutate(level = if_else(level == "S2ai", "S2AI", level))
      
      if(nrow(left_prior_implants_df)>0){
        updateAwesomeCheckboxGroup(session = session,
                                   inputId = "left_revision_implants", 
                                   selected = left_prior_implants_df$level)
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
        updateAwesomeCheckboxGroup(session = session, 
                                   inputId = "right_revision_implants",
                                   selected = right_prior_implants_df$level)
      }
    }
  })
  
  observeEvent(list(existing_patient_data$patient_df, input$close_startup_modal), ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(existing_patient_data$match_found == TRUE){
      prior_fusion_df <- existing_patient_data$patient_df %>%
        filter(str_detect(object, "fusion")) %>%
        distinct()
      if(nrow(prior_fusion_df)>0){
        updatePickerInput(session = session, 
                          inputId = "prior_fusion_levels", 
                          selected =  prior_fusion_df$level)
      }
    }
  })
  observeEvent(list(existing_patient_data$patient_df, input$close_startup_modal), ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(existing_patient_data$match_found == TRUE){
      prior_decompression_df <- existing_patient_data$patient_df %>%
        filter(str_detect(object, "complete_facetectomy|costovertebral_approach|costotransversectomy|diskectomy|laminectomy|laminoplasty|cervical_foraminotomy|laminotomy|sublaminar_decompression|transpedicular_approach|lateral_extracavitary_approach|lateral_extraforaminal_approach|laminectomy_for_facet_cyst")) %>%
        distinct()
      if(nrow(prior_decompression_df)>0){
        updatePickerInput(session = session, inputId = "open_canal", 
                          selected = jh_convert_interspace_to_body_vector_function(prior_decompression_df$level))
      }
    }
  })
  
  
  ################################################  COMPLICATION RECORDING  MODAL BOX ######################################
  ################################################  COMPLICATION RECORDING  MODAL BOX ######################################
  ################################################  COMPLICATION RECORDING  MODAL BOX ######################################
  ################################################  COMPLICATION RECORDING  MODAL BOX ######################################
  
  observeEvent(input$record_complication_button, ignoreInit = TRUE, ignoreNULL = TRUE, {
    removeModal()
    showModal(complication_modal_function(date_of_surgery_vector = existing_patient_data$surgical_dates_df$date_of_surgery))
  }
  )
  
  complication_recording_reactive_df <- reactive({
    
    if(input$complication_description == "Other"){
      complication_description <- input$complication_other
    }else{
      complication_description <- input$complication_description
    }
    
    if(complication_description == "Neurologic Deficit"){
      complication_neuro <- input$complication_neuro_deficit
    }else{
      complication_neuro <- "NA"
    }
    
    complication_count_df <- exportRecords(rcon = rcon_reactive$rcon, 
                                           records = existing_patient_data$record_id) %>%
      type.convert(as.is = TRUE) %>%
      filter(redcap_event_name == "complication_arm_1")
    if(nrow(complication_count_df)>0){
      complication_repeat_instance <- max(complication_count_df$redcap_repeat_instance) + 1
    }else{
      complication_repeat_instance <- 1
    }
    
    redcap_complication_df <- tibble(record_id = existing_patient_data$record_id,
                                     redcap_event_name = "complication_arm_1",
                                     redcap_repeat_instrument = "complications",
                                     redcap_repeat_instance = complication_repeat_instance,
                                     complication_date_of_surgery = input$complication_date_of_surgery, 
                                     complication_date = paste(as.character(input$complication_date)), 
                                     complication_description = complication_description, 
                                     complication_neuro = complication_neuro,
                                     complication_comment = input$complication_comment, 
                                     complications_complete = "Complete")
    
    redcap_complication_df
    
  })
  
  observeEvent(input$complication_submit_to_redcap, {
    withProgress(message = 'Uploading Data', value = 0, {
      
      incProgress(1/2, detail = paste("Uploading Complication Data"))
      importRecords(rcon = rcon_reactive$rcon, data = complication_recording_reactive_df(), returnContent = "count")
      
      incProgress(1/2, detail = paste("Upload Complete"))
    }
    )
    
    sendSweetAlert(
      session = session,
      title = "Success!!",
      text = "The complication has been recorded.",
      type = "success"
    )
  }
  )
  

  ################################################  COMPLICATION RECORDING  MODAL BOX END ######################################
  ################################################  COMPLICATION RECORDING  MODAL BOX END ######################################
  
  ################################################    MODAL BOX 2 ######################################
  ################################################    MODAL BOX 2 ######################################
  ################################################    MODAL BOX 2 ######################################
  observeEvent(input$close_startup_modal, ignoreInit = TRUE, ignoreNULL = TRUE,{
    if(length(input$date_of_birth) == 0 | length(input$date_of_surgery) == 0 | is.null(input$sex)){
      showModal(startup_modal_box(starting_first_name = input$patient_first_name, 
                                  starting_last_name = input$patient_last_name, 
                                  starting_dob = input$date_of_birth,
                                  starting_dos = input$date_of_surgery, 
                                  starting_sex = input$sex, 
                                  hospital_input = input$hospital, 
                                  redcap_token_input = input$redcap_token,
                                  button_proceed = "exit"
      ))
    }else{
      removeModal()
      showModal(startup_modal_box_diagnosis_symptoms(diagnosis_category_value = NULL,
                                                     # primary_diagnosis_value = NULL,
                                                     symptoms_initial_value = NULL,
                                                     stage_number_value = input$stage_number,
                                                     staged_procedure_initial_value = FALSE,
                                                     multiple_approach_initial_value = FALSE,
                                                     multi_approach_starting_position = input$multi_approach_starting_position,
                                                     spinal_regions_selected = NULL,
                                                     ##
                                                     primary_or_revision = input$primary_revision,
                                                     revision_indication = input$revision_indication,
                                                     levels_with_prior_decompression = input$open_canal,
                                                     prior_fusion_levels = input$prior_fusion_levels,
                                                     prior_instrumentation = input$prior_instrumentation,
                                                     left_prior_implants = input$left_revision_implants,
                                                     right_prior_implants = input$right_revision_implants,
                                                     left_rod_status = input$left_revision_rod_status,
                                                     right_rod_status = input$right_revision_rod_status
      ))
    }
  })
  
  observeEvent(input$complication_done_button, ignoreInit = TRUE, ignoreNULL = TRUE,{
    if(length(input$date_of_birth) == 0 | length(input$date_of_surgery) == 0 | is.null(input$sex)){
      showModal(startup_modal_box(starting_first_name = input$patient_first_name, 
                                  starting_last_name = input$patient_last_name, 
                                  starting_dob = input$date_of_birth,
                                  starting_dos = input$date_of_surgery, 
                                  starting_sex = input$sex, 
                                  hospital_input = input$hospital, 
                                  redcap_token_input = input$redcap_token,
                                  button_proceed = "exit"
      ))
    }else{
      removeModal()
      showModal(startup_modal_box_diagnosis_symptoms(diagnosis_category_value = NULL,
                                                     # primary_diagnosis_value = NULL,
                                                     symptoms_initial_value = NULL,
                                                     stage_number_value = input$stage_number,
                                                     staged_procedure_initial_value = FALSE,
                                                     multiple_approach_initial_value = FALSE,
                                                     multi_approach_starting_position = input$multi_approach_starting_position,
                                                     spinal_regions_selected = NULL,
                                                     ##
                                                     primary_or_revision = input$primary_revision,
                                                     revision_indication = input$revision_indication,
                                                     levels_with_prior_decompression = input$open_canal,
                                                     prior_fusion_levels = input$prior_fusion_levels,
                                                     prior_instrumentation = input$prior_instrumentation,
                                                     left_prior_implants = input$left_revision_implants,
                                                     right_prior_implants = input$right_revision_implants,
                                                     left_rod_status = input$left_revision_rod_status,
                                                     right_rod_status = input$right_revision_rod_status
      ))
    }
  })

  modal_box_diagnosis_symptoms_procedure_reactive <- reactive({
    if(!is.null(input$left_revision_rod_status) && input$left_revision_rod_status == "retained_cut" && length(input$left_revision_implants)>0){
      left_revision_rod_cut_level_choices_vector <- (all_screw_coordinates_df %>%
                                 filter(side == "left") %>%
                                 filter(str_detect(level, "-") == TRUE) %>%
                                 filter(str_detect(level, "O-|Sacr") == FALSE) %>%
                                 filter(between(vertebral_number,
                                                min(jh_get_vertebral_number_function(input$left_revision_implants)),
                                                max(jh_get_vertebral_number_function(input$left_revision_implants)))))$level
    }else{
      left_revision_rod_cut_level_choices_vector <- (all_screw_coordinates_df %>%
                                                       filter(side == "left") %>%
                                                       filter(str_detect(level, "-") == TRUE) %>%
                                                       filter(str_detect(level, "O-|Sacr") == FALSE))$level
    }
    
    left_retained_implants <- setdiff(input$left_revision_implants, input$left_revision_implants_removed)
    
    if(!is.null(input$right_revision_rod_status) && input$right_revision_rod_status == "retained_cut" && length(input$right_revision_implants)>0){
      right_revision_rod_cut_level_choices_vector <- (all_screw_coordinates_df %>%
                                                       filter(side == "right") %>%
                                                       filter(str_detect(level, "-") == TRUE) %>%
                                                       filter(str_detect(level, "O-|Sacr") == FALSE) %>%
                                                       filter(between(vertebral_number,
                                                                      min(jh_get_vertebral_number_function(input$right_revision_implants)),
                                                                      max(jh_get_vertebral_number_function(input$right_revision_implants)))))$level
    }else{
      right_revision_rod_cut_level_choices_vector <- (all_screw_coordinates_df %>%
                                                       filter(side == "right") %>%
                                                       filter(str_detect(level, "-") == TRUE) %>%
                                                       filter(str_detect(level, "O-|Sacr") == FALSE))$level
    }
    
    right_retained_implants <- setdiff(input$right_revision_implants, input$right_revision_implants_removed)
    
    startup_modal_box_diagnosis_symptoms(spinal_regions_selected = input$spinal_regions,
                                         diagnosis_category_value = input$diagnosis_category, 
                                         primary_diagnosis_value = input$primary_diagnosis,
                                         symptoms_initial_value = input$symptoms, 
                                         symptoms_other = input$symptoms_other,
                                         stage_number_value = input$stage_number,
                                         staged_procedure_initial_value = input$staged_procedure,
                                         multiple_approach_initial_value = input$multiple_approach,
                                         multi_approach_starting_position = input$multi_approach_starting_position,
                                         ##
                                         primary_or_revision = input$primary_revision,
                                         revision_approach = input$revision_approach, 
                                         prior_anterior_plate_levels = input$prior_anterior_plate_levels, 
                                         prior_anterior_plate_removed_levels = input$prior_anterior_plate_removed_levels,
                                         revision_indication = input$revision_indication,
                                         levels_with_prior_decompression = input$open_canal,
                                         prior_fusion_levels = input$prior_fusion_levels,
                                         prior_instrumentation = input$prior_instrumentation,
                                         left_prior_implants = input$left_revision_implants,
                                         left_prior_implants_removed = input$left_revision_implants_removed,
                                         left_rod_status = input$left_revision_rod_status,
                                         left_revision_rod_cut_level_choices = left_revision_rod_cut_level_choices_vector,
                                         left_revision_rod_cut_level = input$left_revision_rod_cut_level,
                                         left_revision_implants_connected_to_prior_rod_choices = left_retained_implants,
                                         left_revision_implants_connected_to_prior_rod = input$left_revision_implants_connected_to_prior_rod,
                                         right_prior_implants = input$right_revision_implants,
                                         right_prior_implants_removed = input$right_revision_implants_removed,
                                         right_rod_status = input$right_revision_rod_status, 
                                         right_revision_rod_cut_level_choices = right_revision_rod_cut_level_choices_vector,
                                         right_revision_rod_cut_level = input$right_revision_rod_cut_level,
                                         right_revision_implants_connected_to_prior_rod_choices = right_retained_implants,
                                         right_revision_implants_connected_to_prior_rod = input$right_revision_implants_connected_to_prior_rod
    )
  })
  
  observeEvent(input$open_diagnosis_symptoms_procedure_modal, ignoreInit = TRUE, {
    showModal(modal_box_diagnosis_symptoms_procedure_reactive())
  })
  
  observeEvent(input$close_startup_modal_2, ignoreInit = TRUE, {
    removeModal()
  })
  
  

  ################################### UPDATE DIAGNOSIS & SYMPTOMS OPTIONS #####################################
  ################################### UPDATE DIAGNOSIS & SYMPTOMS OPTIONS #####################################
  ################################### UPDATE DIAGNOSIS & SYMPTOMS OPTIONS #####################################
  ################################### UPDATE DIAGNOSIS & SYMPTOMS OPTIONS #####################################
  
  observeEvent(list(input$spinal_regions,
                    input$diagnosis_category, 
                    input$open_diagnosis_symptoms_procedure_modal), ignoreInit = TRUE, {
                      
                      if(length(input$date_of_birth) > 0 & length(input$date_of_surgery)> 0){
                        age <- round(interval(start = input$date_of_birth, end = input$date_of_surgery)/years(1))
                      }else{
                        age <- 999
                      }
                      
                      spine_diagnosis_choices_list <- jh_filter_icd_codes_generate_vector_function(section_input = input$diagnosis_category,
                                                                                                   spine_region_input = input$spinal_regions, 
                                                                                                   age = age)
                      
                      if(length(input$primary_diagnosis)>0){
                        diagnosis_selected <- input$primary_diagnosis
                      }else{
                        diagnosis_selected <- NULL
                      }
                      
                      updatePickerInput(session = session,
                                        inputId = "primary_diagnosis",
                                        label = "Diagnosis Search:", 
                                        choices = spine_diagnosis_choices_list,
                                        options = pickerOptions(liveSearch = TRUE, 
                                                                liveSearchNormalize = TRUE, 
                                                                virtualScroll = 200), 
                                        selected = diagnosis_selected)
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
                                     "Incomplete Loss of Motor & Sensory Function (Spinal Cord Injury)", 
                                     "Wound Drainage and concern for infection")
    
    symptom_option_list$'Other' = c("Other")
    
    updatePickerInput(session = session, 
                      inputId = "symptoms", 
                      label = NULL, 
                      choices = symptom_option_list, 
                      selected = input$symptoms)
    
  })
  ################################### UPDATE DIAGNOSIS & SYMPTOMS OPTIONS END #####################################
  ################################### UPDATE DIAGNOSIS & SYMPTOMS OPTIONS END #####################################
  ################################### UPDATE DIAGNOSIS & SYMPTOMS OPTIONS END #####################################
  
  
  
  ################################### RENDER TEXT FOR MAIN PAGE #####################################
  ################################### RENDER TEXT FOR MAIN PAGE #####################################
  ################################### RENDER TEXT FOR MAIN PAGE #####################################
  
  ####     ####     #### TEXT   ###    ####     ####     #### 

  output$patient_details_text <- renderText({
    age <- round(interval(start = input$date_of_birth, end = input$date_of_surgery)/years(1))
    details <- glue("{input$patient_first_name} {input$patient_last_name}, {age}yo {input$sex}")
    HTML("<div>", details, "</div>")
  })
  
  output$diagnosis_symptoms_text <- renderText({
    if(length(input$date_of_surgery)>0){
      dos <- glue("Date of Surgery: {month(input$date_of_surgery)} {day(input$date_of_surgery)}, {year(input$date_of_surgery)}")  
    }else{
      dos <- "Date of Surgery:"
    }
    
    if(!is.null(input$multiple_approach)){
      if(input$multiple_approach == TRUE && input$staged_procedure == FALSE){
        staged_procedure_text <- "Multiple Approach, Single Stage"   
      }else if(input$staged_procedure == FALSE && input$multiple_approach == FALSE){
        staged_procedure_text <- "Single Stage"   
      }else if(input$staged_procedure == TRUE && input$multiple_approach == TRUE){
        staged_procedure_text <- paste("Multi-approach, multi-stage, Stage", input$stage_number)
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
    
    HTML("<div>", dos, "</div>",
         "<div>", staged_procedure_text, "</div>",
         "<div>", diagnosis, "</div>", 
         "<div>", symptoms, "</div>"
         )
  })
  
  output$currently_adding_text <- renderText({
    if(input$object_to_add == "sublaminar_decompression"){
      paste("Decompression + Foraminotomy")
    }else{
      str_to_title(string = str_replace_all(string = input$object_to_add, pattern = '_', replacement = ' '))
    }
    
  })
  
  
  observeEvent(input$revision_approach, ignoreInit = TRUE, {
    if(input$revision_approach == "anterior"){
      updateRadioGroupButtons(session = session, inputId = "spine_approach", selected = "Anterior")  
      
      if(any(all_implants_constructed_df$approach == "anterior") == FALSE){
        anterior_body_constructed_df <- all_object_ids_df %>%
          filter(category == "anterior_body") %>%
          left_join(fread("coordinates/anterior_body.csv") %>%
                      group_by(object_id) %>%
                      nest() %>%
                      mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
                      select(object_id, object_constructed))
        
        anterior_disc_constructed_df <- all_object_ids_df %>%
          filter(category == "anterior_disc") %>%
          filter(object != "anterior_disc_arthroplasty") %>%
          left_join(fread("coordinates/anterior_disc.csv") %>% 
                      group_by(object_id) %>%
                      nest() %>%
                      mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
                      select(object_id, object_constructed))
        
        anterior_interbody_fusion_constructed_df <- all_object_ids_df %>%
          filter(category == "anterior_interbody_fusion") %>%
          left_join(fread("coordinates/anterior_interbody_fusion.csv") %>%
                      group_by(object_id) %>%
                      nest() %>%
                      mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
                      select(object_id, object_constructed))
        
        arthroplasty_constructed_df <- all_object_ids_df %>%
          filter(str_detect(object, "arthropl")) %>%
          mutate(inferior_endplate_y = c(0.965, 0.925, 0.897, 0.87, 0.846, 0.821, 0.795, 0.77, 0.74, 0.712, 0.682, 0.65, 0.62, 0.591, 0.56, 0.53, 0.495, 0.457, 0.415, 0.367, 0.317, 0.263, 0.215, 0.168), 
                 superior_endplate_y = c(0.955, 0.918, 0.888, 0.865, 0.838, 0.815, 0.79, 0.76, 0.732, 0.705, 0.675, 0.645, 0.613, 0.584, 0.555, 0.525, 0.49, 0.45, 0.406, 0.357, 0.305, 0.255, 0.205, 0.16), 
                 width = c(0.0175, 0.021, 0.02275, 0.02275, 0.0245, 0.0245, 0.0245, 0.02625, 0.02625, 0.028, 0.028, 0.02975, 0.02975, 0.0315, 0.0315, 0.0315, 0.03325, 0.035, 0.035, 0.035, 0.03675, 0.0385, 0.04025, 0.042)
          ) %>%
          mutate(object_constructed = pmap(.l = list(..1 = inferior_endplate_y,
                                                     ..2 = superior_endplate_y, 
                                                     ..3 = width),
                                           .f = ~ arthroplasty_function(y_for_inferior_endplate = ..1, 
                                                                        y_for_superior_endplate = ..2,
                                                                        endplate_width = ..3)))%>%
          select(names(all_implants_constructed_df))
        
        all_implants_constructed_df <<- all_implants_constructed_df %>%
          bind_rows(anterior_body_constructed_df) %>%
          bind_rows(anterior_disc_constructed_df) %>%
          bind_rows(anterior_interbody_fusion_constructed_df) %>%
          bind_rows(arthroplasty_constructed_df) %>%
          distinct() 
      }
      
    }
    if(input$revision_approach == "posterior"){
      updateRadioGroupButtons(session = session, inputId = "spine_approach", selected = "Posterior")  
    }
    
  })
  

  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   Startup COMPLETE  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   Startup COMPLETE  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   Startup COMPLETE  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  
  observeEvent(input$multi_approach_starting_position, {
    if(input$multi_approach_starting_position == "Anterior" | input$multi_approach_starting_position == "Lateral"){
      updateRadioGroupButtons(session = session, inputId = "spine_approach", selected = "Anterior")
    }else{
      updateRadioGroupButtons(session = session, inputId = "spine_approach", selected = "Posterior")
    }
  }
  )
  
  ####### IMPORT ANTERIOR OBJECTS #############
  
  observeEvent(input$spine_approach, ignoreInit = TRUE, ignoreNULL = TRUE, {
    if(input$spine_approach == "Anterior"){
      
      # if(any(all_implants_constructed_df$approach == "anterior") == FALSE){
        anterior_body_constructed_df <- all_object_ids_df %>%
          filter(category == "anterior_body") %>%
          left_join(fread("coordinates/anterior_body.csv") %>%
                      group_by(object_id) %>%
                      nest() %>%
                      mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
                      select(object_id, object_constructed))
        
        anterior_disc_constructed_df <- all_object_ids_df %>%
          filter(category == "anterior_disc") %>%
          filter(object != "anterior_disc_arthroplasty") %>%
          left_join(fread("coordinates/anterior_disc.csv") %>%
                      group_by(object_id) %>%
                      nest() %>%
                      mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
                      select(object_id, object_constructed))
        
        anterior_interbody_fusion_constructed_df <- all_object_ids_df %>%
          filter(category == "anterior_interbody_fusion") %>%
          left_join(fread("coordinates/anterior_interbody_fusion.csv") %>%
                      group_by(object_id) %>%
                      nest() %>%
                      mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
                      select(object_id, object_constructed))
      
        
        arthroplasty_constructed_df <- all_object_ids_df %>%
          filter(str_detect(object, "arthropl")) %>%
          mutate(inferior_endplate_y = c(0.965, 0.925, 0.897, 0.87, 0.846, 0.821, 0.795, 0.77, 0.74, 0.712, 0.682, 0.65, 0.62, 0.591, 0.56, 0.53, 0.495, 0.457, 0.415, 0.367, 0.317, 0.263, 0.215, 0.168), 
                 superior_endplate_y = c(0.955, 0.918, 0.888, 0.865, 0.838, 0.815, 0.79, 0.76, 0.732, 0.705, 0.675, 0.645, 0.613, 0.584, 0.555, 0.525, 0.49, 0.45, 0.406, 0.357, 0.305, 0.255, 0.205, 0.16), 
                 width = c(0.0175, 0.021, 0.02275, 0.02275, 0.0245, 0.0245, 0.0245, 0.02625, 0.02625, 0.028, 0.028, 0.02975, 0.02975, 0.0315, 0.0315, 0.0315, 0.03325, 0.035, 0.035, 0.035, 0.03675, 0.0385, 0.04025, 0.042)
          )  %>%
          mutate(object_constructed = pmap(.l = list(..1 = inferior_endplate_y,
                                                     ..2 = superior_endplate_y, 
                                                     ..3 = width),
                                           .f = ~ arthroplasty_function(y_for_inferior_endplate = ..1, 
                                                                        y_for_superior_endplate = ..2,
                                                                        endplate_width = ..3)))
        
        all_implants_constructed_df <<- all_implants_constructed_df %>%
          bind_rows(anterior_body_constructed_df) %>%
          bind_rows(anterior_disc_constructed_df) %>%
          bind_rows(anterior_interbody_fusion_constructed_df) %>%
          bind_rows(arthroplasty_constructed_df) %>%
          distinct() 
      }
    # }
  })
  
  #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE ANTERIOR REVISION IMPLANTS DF   #############~~~~~~~~~~~~~~~~~~~ ##################### 
  #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE ANTERIOR REVISION IMPLANTS DF   #############~~~~~~~~~~~~~~~~~~~ ##################### 
  
  anterior_plate_revision_implants_df_reactive <- reactive({
    if(req(input$revision_approach) == "anterior"){
      revision_anterior_plate_df <- tibble(level = character(), vertebral_number = double(), object = character(), object_constructed = character()) 
      if(length(input$prior_anterior_plate_levels)>0){
        
        anterior_plate_revision_implants_df <- tibble(prior_plate_present_levels = input$prior_anterior_plate_levels) %>%
          mutate(level = prior_plate_present_levels) %>%
          mutate(prior_plate_status = if_else(level %in% input$prior_anterior_plate_removed_levels, "removed", "retained")) %>%
          left_join(revision_anterior_plate_df)
        
      }else{
        anterior_plate_revision_implants_df <- tibble(prior_plate_present_levels = character(), level = character(), prior_plate_status = character()) 
      } 
    }else{
      anterior_plate_revision_implants_df <- tibble(prior_plate_present_levels = character(), level = character(), prior_plate_status = character()) 
    }
    
    anterior_plate_revision_implants_df
  }) 
  
  ################################   REVISION RODS   ################################  
  observeEvent(input$add_rods,ignoreNULL = TRUE, ignoreInit = TRUE, {
    prior_left_rod_present <- FALSE
    prior_right_rod_present <- FALSE
    prior_rods_present <- FALSE

    if(!is.null(input$left_revision_rod_status) && input$left_revision_rod_status != "removed"){
      prior_left_rod_present <- TRUE
    }else{
      prior_left_rod_present <- FALSE
    }
    if(!is.null(input$right_revision_rod_status) && input$right_revision_rod_status != "removed"){
      prior_right_rod_present <- TRUE
    }else{
      prior_right_rod_present <- FALSE
    }
    prior_rods_present <- case_when(
      prior_left_rod_present == TRUE | prior_right_rod_present == TRUE ~ TRUE,
      TRUE ~ FALSE
    )
    if(prior_rods_present == TRUE && input$fusion_procedure_performed == TRUE && nrow(all_objects_to_add_list$objects_df %>% filter(str_detect(object, "screw|hook|wire")))>0){
      
      if(prior_left_rod_present == TRUE){
        updateSwitchInput(session = session, inputId = "left_revision_rod_present", value = TRUE)
      }
      if(prior_right_rod_present == TRUE){
        updateSwitchInput(session = session, inputId = "right_revision_rod_present", value = TRUE)
      }
    }
  })
  
  observeEvent(input$implants_complete,ignoreNULL = TRUE, ignoreInit = TRUE, {
    # updateTabItems(session = session, inputId = "tabs", selected = "implant_details")
    if(input$fusion_procedure_performed == TRUE && input$add_rods == 0 && nrow(all_objects_to_add_list$objects_df %>% filter(str_detect(object, "screw|hook|wire")))>0){
      sendSweetAlert(
        session = session,
        title = "Please click 'Add Rods' Prior to Proceeding",
        text = NULL,
        type = "warning"
      )
    }else{
      updateTabItems(session = session, inputId = "tabs", selected = "implant_details")
    }
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
  

  
  observeEvent(input$lumbar_vertebrae_count, ignoreInit = TRUE, ignoreNULL = TRUE,{
    if(input$lumbar_vertebrae_count == "6"){
      ## first backup L5
      # source("build_anterior_objects.R", local = TRUE)
      source("load_coordinates_build_objects_6_lumbar.R", local = TRUE)
      
      l5_levels_vector <<- levels_vector
      l5_labels_df <<- labels_df
      l5_vertebral_numbers_vector <<- vertebral_numbers_vector
      l5_levels_numbered_df <<- levels_numbered_df
      l5_jh_get_vertebral_number_function <<- jh_get_vertebral_number_function
      l5_jh_get_vertebral_level_function <<- jh_get_vertebral_level_function
      l5_spine_png <<- spine_png
      l5_anterior_spine_jpg <<- anterior_spine_jpg
      l5_interbody_levels_df <<-interbody_levels_df
      # l5_revision_implants_df <<- revision_implants_df
      # l5_anterior_df <<- anterior_df
      l5_all_implants_constructed_df <<- all_implants_constructed_df
      l5_open_canal_df <<- open_canal_df
      l5_labels_anterior_df <<- labels_anterior_df
      
      # source("load_coordinates_build_objects_6_lumbar.R", local = TRUE)
      
      labels_anterior_df <<- labels_anterior_df  %>%
        add_row(level = "L6", x = 0.5, y = 0.14)
      
      spine_png <<- l6_spine_png
      
      anterior_spine_jpg <<- l6_anterior_spine_png
      
      labels_df <<- l6_labels_df
      levels_numbered_df <<- l6_levels_numbered_df
      interbody_levels_df <<- l6_levels_numbered_df %>%
        filter(str_detect(level, "-"))
      
      all_implants_constructed_df <<- jh_change_object_df_to_l6_function(all_implants_constructed_df)$l6_all_implants_constructed_df
    
      # all_implants_constructed_df <<- all_implants_constructed_df %>%
      #   filter(vertebral_number < 23.9) %>%
      #   bind_rows(l6_all_implants_constructed_df)
      
      # anterior_df <<- l6_anterior_df
      jh_get_vertebral_number_function <<- l6_jh_get_vertebral_number_function
      jh_get_vertebral_level_function <<- l6_jh_get_vertebral_level_function
      # revision_implants_df <<- l6_revision_implants_df
      open_canal_df <<- l6_open_canal_df
      
    }
    
    if(input$lumbar_vertebrae_count == "5" | input$lumbar_vertebrae_count == "4"){
      spine_png <<- l5_spine_png
      anterior_spine_jpg <<-l5_anterior_spine_jpg
      
      labels_df <<- l5_labels_df
      levels_numbered_df <<- l5_levels_numbered_df
      
      labels_anterior_df <<- l5_labels_anterior_df
      
      all_implants_constructed_df <<- l5_all_implants_constructed_df
      
      # anterior_df <<- l5_anterior_df
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

  observe({
    if(jh_determine_if_section_dx_function(diagnosis_vector = input$primary_diagnosis, section_to_determine = "tumor")){
      updateSwitchInput(session = session, inputId = "tumor_diagnosis_true_false", value = TRUE)
    }
  })
  
  ################    ################  UPDATE CHOICES & READ IN NEW COORDINATES   ################  ######################  
  ################    ################  UPDATE CHOICES & READ IN NEW COORDINATES   ################  ######################  
  ################    ################  UPDATE CHOICES & READ IN NEW COORDINATES   ################  ######################  
  
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
                                "Partial Vertebral Excision for lesion" = "partial_vertebral_excision",
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
  
  observeEvent(all_objects_to_add_list$objects_df, ignoreInit = TRUE, ignoreNULL = TRUE, {

    if((any(all_objects_to_add_list$objects_df$object %in% c("grade_3", "grade_4", "grade_5")))){
      updateSwitchInput(session = session, inputId = "intervertebral_cage_true_false", value = TRUE)
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
      
      implant_options <- keep(.x = implants_vector, .p = ~ any(str_detect((all_object_ids_df %>%
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
    
    if(any(all_implants_constructed_df$object == "laminectomy") == FALSE){
      decompression_df <- all_object_ids_df %>%
        filter(category == "decompression") %>%
        left_join(fread("coordinates/decompression.csv") %>%
                    group_by(object_id) %>%
                    nest() %>%
                    mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
                    select(object_id, object_constructed))
      
      all_implants_constructed_df <<- all_implants_constructed_df %>%
        bind_rows(decompression_df)  %>%
        distinct()
    }
    
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
    
    if(any(all_implants_constructed_df$category == "osteotomy") == FALSE){
      osteotomy_df <- all_object_ids_df %>%
        filter(category == "osteotomy") %>%
        left_join(fread("coordinates/osteotomy.csv") %>%
                    group_by(object_id) %>%
                    nest() %>%
                    mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
                    select(object_id, object_constructed))
      
      all_implants_constructed_df <<- all_implants_constructed_df %>%
        bind_rows(osteotomy_df)  %>%
        distinct()
    }
    
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
    if(any(all_implants_constructed_df$object == "tlif") == FALSE){
      interbody_df <- all_object_ids_df %>%
        filter(category == "interbody") %>%
        left_join(fread("coordinates/interbody.csv") %>%
                    group_by(object_id) %>%
                    nest() %>%
                    mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
                    select(object_id, object_constructed))
      
      all_implants_constructed_df <<- all_implants_constructed_df %>%
        bind_rows(interbody_df)  %>%
        distinct()
    }
    
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
    
    if(any(all_implants_constructed_df$object == "transpedicular_approach") == FALSE){
      decompression_df <- all_object_ids_df %>%
        filter(category == "decompression") %>%
        left_join(fread("coordinates/decompression.csv") %>%
                    group_by(object_id) %>%
                    nest() %>%
                    mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
                    select(object_id, object_constructed))
      
      all_implants_constructed_df <<- all_implants_constructed_df %>%
        bind_rows(decompression_df)  %>%
        distinct()
    }
    
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
    if(any(all_implants_constructed_df$object == "incision_drainage") == FALSE){
      incision_drainage_df <- all_object_ids_df %>%
        filter(category == "incision_drainage") %>%
        left_join(fread("coordinates/incision_drainage.csv") %>%
                    group_by(object_id) %>%
                    nest() %>%
                    mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
                    select(object_id, object_constructed))
      
      all_implants_constructed_df <<- all_implants_constructed_df %>%
        bind_rows(incision_drainage_df)  %>%
        distinct()
    }
    
    # nerves_df <- all_implants_constructed_df %>%
    #   # tabyl(object)
    #   filter(object == "lateral_extraforaminal_approach") %>%
    #   # select(level, vertebral_number, side, approach, x, y) %>%
    #   mutate(object = "transection_spinal_nerve") %>%
    #   rename(x1 = x, y1 = y) %>%
    #   mutate(y1 = y1 + 0.003) %>%
    #   mutate(x1 = if_else(side == "left", x1 + 0.003, x1 - 0.003)) %>% 
    #   mutate(x2 = if_else(side == "left", x1 - 0.008, x1 + 0.008), 
    #          y2 = y1 - 0.003) %>%
    #   mutate(object_constructed = pmap(.l = list(..1 = x1, 
    #                                              ..2 = x2, 
    #                                              ..3 = y1, 
    #                                              ..4 = y2), 
    #                                    .f = ~ st_buffer(st_linestring(as.matrix(tibble(x = c(..1, ..2), y = c(..3, ..4)))), dist = 0.005)
    #   ))
    # 
    # nerve_objects <- nerves_df$object_constructed
    # 
    # transection_line <- st_multipolygon(x = list(st_buffer(st_linestring(as.matrix(nerves_df %>%
    #                                                                                  filter(side == "right") %>%
    #                                                                                  group_by(level) %>%
    #                                                                                  mutate(x = mean(x1, x2)) %>%
    #                                                                                  ungroup() %>%
    #                                                                                  select(x, y = y1))), dist = 0.002), 
    #                                              st_buffer(st_linestring(as.matrix(nerves_df %>%
    #                                                                                  filter(side == "left") %>%
    #                                                                                  group_by(level) %>%
    #                                                                                  mutate(x = mean(x1, x2)) %>%
    #                                                                                  ungroup() %>%
    #                                                                                  select(x, y = y1))
    #                                              ), dist = 0.002)))
    # 
    # transected_nerve_objects <- map(.x = nerve_objects, .f =  ~ st_difference(x = .x, y = transection_line))
    # 
    # nerve_geoms <- map(.x = transected_nerve_objects, .f =  ~ geom_sf(data = .x, fill = "red"))
    
    updateRadioGroupButtons(session = session, 
                            inputId = "object_to_add", 
                            choices = c("Vertebroplasty" = "vertebroplasty",
                                        "Vertebral Augmentation (cavity creation, then cement)" = "vertebral_cement_augmentation",
                                        "Structural Allograft Strut" = "structural_allograft",
                                        "Incision & Drainage" = "incision_drainage", 
                                        "Transection of Spinal Nerve" = "nerve_transection"
                            ),
                            checkIcon = list(
                              yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                            ),
                            selected = "vertebroplasty"
    )
  })
  
  
  observeEvent(input$add_tumor_resection,ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(any(all_implants_constructed_df$object == "laminectomy_for_tumor") == FALSE){
      tumor_df <- all_object_ids_df %>%
        filter(category == "tumor") %>%
        left_join(fread("coordinates/tumor.csv") %>%
                    group_by(object_id) %>%
                    nest() %>%
                    mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
                    select(object_id, object_constructed))
      
      all_implants_constructed_df <<- all_implants_constructed_df %>%
        bind_rows(tumor_df)  %>%
        distinct()
    }
    
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
  
  
  

  ################# ------------  #############  ADDING PROCEDURES & BUILDING REACTIVE DATAFRAMES   ################# ------------  ############# 
  ################# ------------  #############  ADDING PROCEDURES & BUILDING REACTIVE DATAFRAMES   ################# ------------  ############# 
  ################# ------------  #############  ADDING PROCEDURES & BUILDING REACTIVE DATAFRAMES   ################# ------------  ############# 
  
  #########------------------------- ADD TO PLOT -------------------------###########
  
  ### RESET ALL AND REMOVE EVERYTHING
  observeEvent(input$reset_all, ignoreNULL = TRUE, ignoreInit = TRUE, {
    all_objects_to_add_list$objects_df <- all_objects_to_add_list$objects_df %>%
      filter(level == "xxx")
  })
  
  
  ########################################### object DETAILS REACTIVE ###########################################
  geoms_list_anterior_diskectomy <- reactiveValues()
  geoms_list_anterior_interbody <- reactiveValues()
  geoms_list_anterior_instrumentation <- reactiveValues()
  geoms_list_revision_anterior <- reactiveValues()
  
  geoms_list_posterior <- reactiveValues()
  geoms_list_revision_posterior <- reactiveValues()
  rods_list <- reactiveValues()


  #### OBSERVE THE PLOT CLICK AND ADD APPROPRIATE object ####
  object_added_reactive_df <- reactive({
    
    if(input$object_to_add == "pelvic_screw"){
      object_currently_selected_to_add <- c("pelvic_screw_1", "pelvic_screw_2")
    }else{
      object_currently_selected_to_add <- input$object_to_add
    }
    
    object_added_reactive_df <- nearPoints(
      df = (all_implants_constructed_df %>%
              filter(object %in% object_currently_selected_to_add)),
      coordinfo = input$plot_click,
      xvar = "x",
      yvar = "y",
      maxpoints = 1,
      threshold = 45
    )
    
    if(input$object_to_add == "decompression_diskectomy_fusion" | input$object_to_add == "diskectomy_fusion"){
      
      # anterior_interbody_df <- all_implants_constructed_df %>%
      #   filter(object == "anterior_interbody_implant", 
      #          level == object_added_reactive_df$level)
      
      anterior_interbody_df <-  object_added_reactive_df %>%
        select(level, approach) %>%
        mutate(object = "anterior_interbody_implant") %>%
        left_join(all_implants_constructed_df) %>%
        select(names(object_added_reactive_df))
      
      object_added_reactive_df <- object_added_reactive_df %>%
        bind_rows(anterior_interbody_df)
    }
    object_added_reactive_df
    
  })
  
  #### OBSERVE THE PLOT CLICK AND ADD APPROPRIATE object ####
  
  # observeEvent(input$plot_click, {
  #   all_objects_to_add_list$objects_df <- all_objects_to_add_list$objects_df  %>%
  #     bind_rows(object_added_reactive_df()) %>%
  #     distinct()
  #   
  #   if(any(str_detect(object_added_reactive_df()$object, "grade_"))){
  #     if(length(unique((all_objects_to_add_list$objects_df %>% filter(str_detect(object, "grade_")))$object)) > 1){
  #       all_objects_to_add_list$objects_df <- jh_filter_osteotomies_function(full_df_to_filter = all_objects_to_add_list$objects_df)
  #     }
  #   }
  #   
  #   # jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left")
  #   
  # })
  
  ######### ~~~~~~~~~~~~~~  ############# MAKE THE GEOMS     ######### ~~~~~~~~~~~~~~  #############
  ######### ~~~~~~~~~~~~~~  ############# MAKE THE GEOMS    ######### ~~~~~~~~~~~~~~  #############
  
  observeEvent(list(input$plot_click,
                    input$plot_double_click,
                    all_objects_to_add_list$objects_df), {
                      
                      all_objects_to_add_list$objects_df <- all_objects_to_add_list$objects_df  %>%
                        bind_rows(object_added_reactive_df()) %>%
                        distinct()
                      
                      if(str_detect(input$object_to_add, "grade_")){
                        if(length(unique((all_objects_to_add_list$objects_df %>% filter(str_detect(object, "grade_")))$object)) > 1){
                          all_objects_to_add_list$objects_df <- jh_filter_osteotomies_function(full_df_to_filter = all_objects_to_add_list$objects_df)
                        }
                      }
                      
                      # if(any(str_detect(object_added_reactive_df()$object, "grade_"))){
                      #   if(length(unique((all_objects_to_add_list$objects_df %>% filter(str_detect(object, "grade_")))$object)) > 1){
                      #     all_objects_to_add_list$objects_df <- jh_filter_osteotomies_function(full_df_to_filter = all_objects_to_add_list$objects_df)
                      #   }
                      # }
                      if(input$left_supplemental_rods_eligible == FALSE){
                        free_screws_vector <- unique(append(jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left")$level, 
                                                            left_revision_implants_reactive_list()$free_revision_screws_vector))
                        if(length(free_screws_vector) > 2){
                          updateSwitchInput(session = session, inputId = "left_supplemental_rods_eligible", value = TRUE)
                        } 
                      }
                      if(input$right_supplemental_rods_eligible == FALSE){
                        free_screws_vector <- unique(append(jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right")$level, 
                                                            right_revision_implants_reactive_list()$free_revision_screws_vector))
                        if(length(free_screws_vector) > 2){
                          updateSwitchInput(session = session, inputId = "right_supplemental_rods_eligible", value = TRUE)
                        } 
                      }
                      
                      
                      if(input$spine_approach == "Anterior"){
                        anterior_df <- all_objects_to_add_list$objects_df %>%
                          filter(approach == "anterior")
                        
                        anterior_geoms_list <- jh_make_anterior_geoms_function(all_anterior_objects_df = anterior_df)
                        
                        geoms_list_anterior_diskectomy$geoms <- anterior_geoms_list$geoms_list_anterior_diskectomy
                        geoms_list_anterior_interbody$geoms <- anterior_geoms_list$geoms_list_anterior_interbody
                        geoms_list_anterior_instrumentation$geoms <- anterior_geoms_list$geoms_list_anterior_instrumentation
                        
                      }else{
                        if(str_detect(input$object_to_add, "screw")){
                          geoms_list_posterior$screws <- jh_make_posterior_screws_geoms_function(all_posterior_objects_df = all_objects_to_add_list$objects_df %>%
                                                                                                   filter(approach == "posterior", str_detect(object, "screw")), 
                                                                                                 plot_with_patterns = input$plot_with_patterns_true)
                          
                        }else{
                          geoms_list_posterior$geoms <- jh_make_posterior_geoms_function(all_posterior_objects_df = all_objects_to_add_list$objects_df %>%
                                                                                           filter(approach == "posterior", str_detect(object, "screw", negate = TRUE)),
                                                                                         plot_with_patterns = input$plot_with_patterns_true)
                        }
                      }
                    })
  
  observeEvent(input$reset_all, ignoreInit = TRUE, {
    geoms_list_posterior$screws <- NULL
    geoms_list_posterior$geoms <- NULL
    geoms_list_posterior <- reactiveValues()
  })
  
  ##### Modal to select whether the C2 nerve root was spared or transected.
  observeEvent(input$plot_click, ignoreInit = TRUE, {
    if(nrow(object_added_reactive_df())>0){
      if(object_added_reactive_df()$object[1] == "lateral_mass_screw" & object_added_reactive_df()$level[1] == "C1"& object_added_reactive_df()$side[1] == "left"){
        updateSwitchInput(session = session, inputId = "left_c1_lateral_mass_screw_true_false", value = TRUE)
      }
      if(object_added_reactive_df()$object[1] == "lateral_mass_screw" & object_added_reactive_df()$level[1] == "C1"& object_added_reactive_df()$side[1] == "right"){
        updateSwitchInput(session = session, inputId = "right_c1_lateral_mass_screw_true_false", value = TRUE)
      }
    }
  })
  
  observeEvent(input$left_c1_lateral_mass_screw_true_false, ignoreInit = TRUE, {
    if(input$left_c1_lateral_mass_screw_true_false == TRUE){
      showModal(
        c2_nerve_transection_modal_function(side = "left")
      )
    }
  })
  observeEvent(input$right_c1_lateral_mass_screw_true_false, ignoreInit = TRUE, {
    if(input$right_c1_lateral_mass_screw_true_false == TRUE){
      showModal(
        c2_nerve_transection_modal_function(side = "right")
      )
    }
  })
  

  c2_nerve_transection_list_reactive <-  reactive({
    c1_screws <- case_when(input$left_c1_lateral_mass_screw_true_false == TRUE & input$right_c1_lateral_mass_screw_true_false == TRUE ~ "bilateral",
                           input$left_c1_lateral_mass_screw_true_false == TRUE & input$right_c1_lateral_mass_screw_true_false == FALSE ~ "left_only",
                           input$left_c1_lateral_mass_screw_true_false == FALSE & input$right_c1_lateral_mass_screw_true_false == TRUE ~ "right_only",
                           input$left_c1_lateral_mass_screw_true_false == FALSE & input$right_c1_lateral_mass_screw_true_false == FALSE ~ "none"
    )
    
    # c2_nerve_transection_list_reactive()$c2_nerve_transection_result = "bilateral_transection", "left", "right", "left_preserved", "right_preserved", "na"
    if(c1_screws == "bilateral"){
      c2_nerve_transection_result <- case_when(
        str_to_lower(input$left_c2_nerve_root_transection) == "yes" & str_to_lower(input$right_c2_nerve_root_transection) == "yes" ~ "bilateral_transection",
        str_to_lower(input$left_c2_nerve_root_transection) == "yes" & str_to_lower(input$right_c2_nerve_root_transection) == "no" ~ "left",
        str_to_lower(input$left_c2_nerve_root_transection) == "no" & str_to_lower(input$right_c2_nerve_root_transection) == "yes" ~ "right",
        TRUE ~ "bilateral_preserved"
      )
    }else if(c1_screws == "left_only"){
      c2_nerve_transection_result <- if_else(str_to_lower(input$left_c2_nerve_root_transection) == "yes", "left", "left_preserved")
    }else if(c1_screws == "right_only"){
      c2_nerve_transection_result <- if_else(str_to_lower(input$right_c2_nerve_root_transection) == "yes", "right", "right_preserved")
    }else{
      c2_nerve_transection_result <- "na"
    }
    
    if(c1_screws == "none" |c1_screws == "na"){
      additional_procedure_statement <- "na"
    }else{
      if(c2_nerve_transection_result %in% c("bilateral_transection", "left", "right")){
        laterality <- if_else(c2_nerve_transection_result == "bilateral_transection", "bilateral", c2_nerve_transection_result)
        additional_procedure_statement <- as.character(glue("Transection of the {laterality} C2 Nerve Root/Greater Occipital Nerve (CPT = 64744)") )
      }else{
        additional_procedure_statement <- "na"
      }
    }
    c2_transection_list <- list()
    c2_transection_list$c1_screws <- c1_screws
    c2_transection_list$c2_nerve_transection_result <- c2_nerve_transection_result
    c2_transection_list$additional_procedure_statement <- additional_procedure_statement
    c2_transection_list
  }) %>%
    bindEvent(input$implants_complete,
              ignoreInit = TRUE,
              ignoreNULL = TRUE)
  
    ##### Modal to report whether the PLL was taken down
  observeEvent(input$plot_click, ignoreInit = TRUE, {
    # object_added <- object_added_reactive_df()$object[1]
    # level_added <- object_added_reactive_df()$level[1]
    if(object_added_reactive_df()$object[1] == "decompression_diskectomy_fusion" & object_added_reactive_df()$level[1] %in% c("C2-C3", "C3-C4", "C4-C5", "C5-C6", "C6-C7", "C7-T1", "T1-T2", "T2-T3")){
      
      # str_replace_all(str_to_lower(object_added_reactive_df()$level[1]), pattern = "-", "_")
      # paste0(str_replace_all(str_to_lower(object_added_reactive_df()$level[1]), pattern = "-", "_"), "_pll")
 
        showModal(
          modalDialog(
            size = "l",
            easyClose = FALSE,
            awesomeRadio(inputId = paste0(str_replace_all(str_to_lower(object_added_reactive_df()$level[1]), pattern = "-", "_"), "_pll"),
                         label = glue("Was the PLL at {object_added_reactive_df()$level[1]} taken down?"), 
                         choices = c("No", "Yes"), 
                         inline = TRUE,
                         width = "100%")
          )
        )
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
  

  
  
  #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE PLOTS    #############~~~~~~~~~~~~~~~~~~~ ##################### 
  #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE PLOTS    #############~~~~~~~~~~~~~~~~~~~ ##################### 
  #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE PLOTS    #############~~~~~~~~~~~~~~~~~~~ ##################### 
  
  #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE PLOTS    #############~~~~~~~~~~~~~~~~~~~ ##################### 
  #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE PLOTS    #############~~~~~~~~~~~~~~~~~~~ ##################### 
  #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE PLOTS    #############~~~~~~~~~~~~~~~~~~~ ##################### 
  
  ######### ~~~~~~~~~~~~~~  ############# POSTERIOR GEOMS     ######### ~~~~~~~~~~~~~~  ############# 
  ######### ~~~~~~~~~~~~~~  ############# POSTERIOR GEOMS     ######### ~~~~~~~~~~~~~~  ############# 
  
  observeEvent(input$prior_instrumentation, ignoreInit = TRUE, {
    if(input$prior_instrumentation == TRUE){

      revision_screws_df <<- all_object_ids_df %>%
        filter(object == "pedicle_screw" | str_detect(object, "pelvic_screw") | object == "occipital_screw" | object == "lateral_mass_screw") %>%
        filter((str_detect(string = level, pattern = "C1|C3|C4|C5|C6") & object == "pedicle_screw") == FALSE) %>%
        arrange(vertebral_number) %>%
        left_join(fread("coordinates/implant.csv") %>%
                    # mutate(x = if_else(x < 0.5, x - 0.025, x + 0.025)) %>%
                    group_by(object_id) %>%
                    nest() %>%
                    mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
                    select(object_id, object_constructed))
    }
  })
  
  ######## ~~~~~~~~~~~ PRIOR DECOMPRESSIONS ---
  observeEvent(input$open_canal, ignoreNULL = TRUE, ignoreInit = TRUE, {
    
    if(length(input$open_canal) > 0){
      
      if(any(all_implants_constructed_df$object == "laminectomy") == FALSE){
        decompression_df <- all_object_ids_df %>%
          filter(category == "decompression") %>%
          left_join(fread("coordinates/decompression.csv") %>%
                      group_by(object_id) %>%
                      nest() %>%
                      mutate(object_constructed = map(.x = data, .f = ~ st_polygon(list(as.matrix(.x))))) %>%
                      select(object_id, object_constructed))
        
        all_implants_constructed_df <<- all_implants_constructed_df %>%
          bind_rows(decompression_df)  %>%
          distinct()
      }
      
      open_df <- all_implants_constructed_df %>%
        filter(object == "laminectomy") %>%
        filter(level %in% input$open_canal) %>%
        mutate(category = "revision")
      
      geoms_list_revision_posterior$open_canal_sf <- geom_sf(data = st_union(st_combine(st_multipolygon(open_df$object_constructed)), by_feature = TRUE, is_coverage = TRUE), fill = "lightblue", alpha = 0.5)
      
    }
  }
  )
  
  # ######## ~~~~~~~~~~~ PRIOR Implants ---
  # 
  # #### ANTERIOR ####
  observeEvent(list(anterior_plate_revision_implants_df_reactive(), input$prior_anterior_plate_removed_levels, input$prior_anterior_plate_levels), ignoreInit = TRUE, ignoreNULL = TRUE, {
    if(nrow(anterior_plate_revision_implants_df_reactive())>0){
      if(any(anterior_plate_revision_implants_df_reactive()$prior_plate_status == "retained")){
        
        geoms_list_revision_anterior$anterior_plate_retained <- geom_sf(data = st_multipolygon((anterior_plate_revision_implants_df_reactive() %>%
                                                                                                  filter(prior_plate_status == "retained"))$object_constructed), color = "black", fill = "grey95", alpha = 0.9)
      }else{
        geoms_list_revision_anterior$anterior_plate_retained <- NULL
      }
      
      if(any(anterior_plate_revision_implants_df_reactive()$prior_plate_status == "removed")){
        geoms_list_revision_anterior$anterior_plate_removed <- geom_sf(data = st_multipolygon((anterior_plate_revision_implants_df_reactive() %>%
                                                                                                 filter(prior_plate_status == "removed"))$object_constructed), color = "grey80", fill = "grey65", alpha = 0.5)
      }else{
        geoms_list_revision_anterior$anterior_plate_removed <-NULL
      }
      
    }else{
      geoms_list_revision_anterior$anterior_plate_retained <- NULL
      geoms_list_revision_anterior$anterior_plate_removed <- NULL
    }
    
  })
  
  # #### POSTERIOR ####
  observeEvent(input$close_startup_modal_2, ignoreInit = TRUE, ignoreNULL = TRUE, {
    if(nrow(left_revision_implants_reactive_list()$removed_df)>0){
      # keep(.x =  left_revision_implants_reactive_list()$removed_df$object_constructed, .p = ~ !is.null(.x))
      
      geoms_list_revision_posterior$left_revision_implants_removed_sf_geom <- geom_sf(data = st_multipolygon(map(.x = keep(.x =  left_revision_implants_reactive_list()$removed_df$object_constructed, .p = ~ !is.null(.x)), 
                                                                                                                 .f = ~ .x - c(0.025, 0))), 
                                                                                      color = "black",
                                                                                      fill = NA)
    }
    
    if(nrow(left_revision_implants_reactive_list()$retained_df)>0){
      # keep(.x =  left_revision_implants_reactive_list()$retained_df$object_constructed, .p = ~ !is.null(.x))
      
      geoms_list_revision_posterior$left_revision_implants_sf_geom <- geom_sf(data = st_multipolygon(keep(.x =  left_revision_implants_reactive_list()$retained_df$object_constructed, .p = ~ !is.null(.x))), fill = "black")
    }
  })
  
  observeEvent(input$close_startup_modal_2, ignoreInit = TRUE, ignoreNULL = TRUE, {
    if(nrow(right_revision_implants_reactive_list()$removed_df)>0){
      # keep(.x =  right_revision_implants_reactive_list()$removed_df$object_constructed, .p = ~ !is.null(.x))
      
      geoms_list_revision_posterior$right_revision_implants_removed_sf_geom <- geom_sf(data = st_multipolygon(map(.x = keep(.x =  right_revision_implants_reactive_list()$removed_df$object_constructed, .p = ~ !is.null(.x)), .f = ~ .x + c(0.025, 0))), 
                                                                                       color = "black",
                                                                                       fill = NA)
    }
    
    if(nrow(right_revision_implants_reactive_list()$retained_df)>0){
      # keep(.x =  right_revision_implants_reactive_list()$retained_df$object_constructed, .p = ~ !is.null(.x))
      
      geoms_list_revision_posterior$right_revision_implants_sf_geom <- geom_sf(data = st_multipolygon(keep(.x =  right_revision_implants_reactive_list()$retained_df$object_constructed, .p = ~ !is.null(.x))), fill = "black")
    }
  })
  
  
  
  ######### ######### REACTIVE OSTEOTOMY ######### #########
  
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
  
  
  #################### LEFT ROD CONSTRUCT ######################## #################### LEFT ROD CONSTRUCT ########################
  #################### LEFT ROD CONSTRUCT ######################## #################### LEFT ROD CONSTRUCT ########################
  #################### LEFT ROD CONSTRUCT ######################## #################### LEFT ROD CONSTRUCT ########################
  #################### LEFT ROD CONSTRUCT ######################## #################### LEFT ROD CONSTRUCT ########################
  #################### LEFT ROD CONSTRUCT ######################## #################### LEFT ROD CONSTRUCT ########################
  #################### LEFT ROD CONSTRUCT ######################## #################### LEFT ROD CONSTRUCT ########################

  # LEFT REVISION IMPLANTS # 
  left_revision_implants_reactive_list <- reactive({
    removed_df <- tibble(level = character(), vertebral_number = double(), x = double(), y = double(), object_constructed = character())
    retained_df <- tibble(level = character(), side = character(), vertebral_number = double(), object = character(), x = double(), y = double(), object_constructed = character())
    revision_implants_status_df <- tibble(level = character(), vertebral_number = double(), side = character(), object = character(), x = double(), y = double(), prior_rod_connected = character(), remove_retain = character(), object_constructed = character())
    free_revision_screws_vector <- c()
    
    if(req(input$revision_approach) == "posterior"){
      
      if(length(input$left_revision_implants)>0){
        if(nrow(existing_patient_data$patient_df)>0){
          revision_implants_status_df <- existing_patient_data$patient_df %>% 
            select(record_id, side, level, object) %>%
            filter(side == "left") %>%
            filter(str_detect(object, "hook|screw")) %>%
            mutate(remove_retain = if_else(level %in% input$left_revision_implants_removed, "remove", "retain")) %>%
            
            group_by(object, level) %>%
            mutate(object = if_else(str_detect(object, "pelvic"), paste0(object, "_", row_number()), object)) %>%
            ungroup() %>%
            
            left_join(all_implants_constructed_df %>%
                        select(level, side, object, object_constructed, vertebral_number, approach, x, y)) %>%
            distinct()%>%
            select(level, vertebral_number, side, object, remove_retain, x, y, object_constructed)
          
        }else{
          revision_implants_status_df <- tibble(level = input$left_revision_implants, side = "left") %>%
            mutate(remove_retain = if_else(level %in% input$left_revision_implants_removed, "remove", "retain")) %>%
            left_join(revision_screws_df) %>% ## this is generated in Load coordinates
            distinct()%>%
            select(level, vertebral_number, side, object, remove_retain, x, y, object_constructed)
        }
        
        if(input$left_revision_rod_status == "removed"){
          revision_implants_status_df <- revision_implants_status_df %>%
            mutate(prior_rod_connected = "no")
        }
        
        if(input$left_revision_rod_status == "retained"){
          revision_implants_status_df <- revision_implants_status_df  %>%
            mutate(prior_rod_connected = "yes")
        }
        
        if(input$left_revision_rod_status == "retained_cut"){
          
          revision_implants_status_df <- revision_implants_status_df %>%
            filter(remove_retain == "retain") %>%
            mutate(prior_rod_connected = if_else(level %in% input$left_revision_implants_connected_to_prior_rod, "yes", "no")) %>%
            union_all(revision_implants_status_df %>%
            filter(remove_retain == "remove") %>%
            mutate(prior_rod_connected = "no")) %>%
            arrange(vertebral_number)

        }
        
        removed_df <- revision_implants_status_df %>%
          filter(remove_retain == "remove")
        
        retained_df <- revision_implants_status_df %>%
          filter(remove_retain == "retain")
        
        free_revision_screws_vector <- (revision_implants_status_df %>%
          filter(remove_retain == "retain") %>%
          filter(prior_rod_connected == "no"))$level
      }
    }
    
    retained_df <- retained_df %>%
      distinct()
    removed_df <- removed_df %>%
      distinct()
    revision_implants_status_df <- revision_implants_status_df %>%
      select(-object_constructed) %>%
      distinct()
    
    list(retained_df = retained_df,
         removed_df = removed_df, 
         revision_implants_status_df = revision_implants_status_df, 
         free_revision_screws_vector = free_revision_screws_vector)
    
  }
  )
  
  observeEvent(left_revision_implants_reactive_list(), ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(nrow(left_revision_implants_reactive_list()$retained_df) < 2){
      updateAwesomeRadio(session = session, inputId = "left_revision_rod_status", selected = "removed")
    }
  })
  
  observeEvent(c(input$left_revision_rod_status, input$left_revision_implants_removed), ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(input$left_revision_rod_status == "retained_cut"){
      level_choices_vector <- (all_screw_coordinates_df %>%
                                 filter(side == "left") %>%
                                 filter(str_detect(level, "-") == TRUE) %>%
                                 filter(str_detect(level, "O-|Sacr") == FALSE)%>%
                                 filter(between(vertebral_number,
                                                min(jh_get_vertebral_number_function(input$left_revision_implants)),
                                                max(jh_get_vertebral_number_function(input$left_revision_implants)))))$level
      
      retained_implants <- setdiff(input$left_revision_implants, input$left_revision_implants_removed)
      removed_implants <- input$left_revision_implants_removed
      
      if(length(removed_implants)>0){
      if(mean(jh_get_vertebral_number_function(retained_implants)) > mean(jh_get_vertebral_number_function(removed_implants))){
        ## this would mean the distal rod is retained:
        cut_level <- jh_get_vertebral_level_function(max(jh_get_vertebral_number_function(removed_implants)) + 0.5)
      }else{
        ## this would mean the proximal rod is retained:
        cut_level <- jh_get_vertebral_level_function(min(jh_get_vertebral_number_function(removed_implants)) - 0.5)
      }
      }else{
        cut_level <- level_choices_vector[1]
      }
      
      updatePickerInput(session = session,
                        inputId = "left_revision_rod_cut_level",
                        choices = level_choices_vector, 
                        selected = cut_level
                        )
    }
  })
  
  observeEvent(list(input$left_revision_rod_cut_level), ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(input$left_revision_rod_status == "retained_cut"){
      if(!is.null(input$left_revision_rod_cut_level)){
        
        level_choices_vector <- (all_screw_coordinates_df %>%
                                   filter(side == "left") %>%
                                   filter(str_detect(level, "-") == TRUE) %>%
                                   filter(str_detect(level, "O-|Sacr") == FALSE)%>%
                                   filter(between(vertebral_number,
                                                  min(jh_get_vertebral_number_function(input$left_revision_implants)),
                                                  max(jh_get_vertebral_number_function(input$left_revision_implants)))))$level
        
        retained_implants <- setdiff(input$left_revision_implants, input$left_revision_implants_removed)
        removed_implants <- input$left_revision_implants_removed
        if(length(retained_implants)>0 && length(removed_implants)>0){
          if(mean(jh_get_vertebral_number_function(retained_implants)) > mean(jh_get_vertebral_number_function(removed_implants))){
            ## this would mean the distal rod is retained:
            left_retained_implants_connected_to_rod <- keep(.x = retained_implants, .p = ~ jh_get_vertebral_number_function(.x) > jh_get_vertebral_number_function(input$left_revision_rod_cut_level))
            
          }else{
            ## this would mean the proximal rod is retained:
            left_retained_implants_connected_to_rod <- keep(.x = retained_implants, .p = ~ jh_get_vertebral_number_function(.x) < jh_get_vertebral_number_function(input$left_revision_rod_cut_level))
          } 
        }else{
          left_retained_implants_connected_to_rod <- retained_implants
        }
        
        updatePickerInput(session = session, 
                          inputId = "left_revision_implants_connected_to_prior_rod", 
                          choices = retained_implants, 
                          selected = left_retained_implants_connected_to_rod
        )
      }else{
        updatePickerInput(session = session, 
                          inputId = "left_revision_implants_connected_to_prior_rod", 
                          choices = setdiff(input$left_revision_implants, input$left_revision_implants_removed), 
                          selected = setdiff(input$left_revision_implants, input$left_revision_implants_removed)
        )  
      }
    }
  })
    

  ######### LEFT REVISION IMPLANTS ENDS ######### 
  ######### LEFT REVISION IMPLANTS ENDS  ######### 
  
  ############### LEFT RODS ################# ############### LEFT RODS ################# ############### LEFT RODS #################
  ############### LEFT RODS ################# ############### LEFT RODS ################# ############### LEFT RODS #################
  
  # left_revision_implants_reactive_list()$retained_df #tibble(level, side, vertebral_number, object, x, y)
  
  ##### UPDATE SUPPLEMENT ROD CHOICES -----
  ##### UPDATE SUPPLEMENT ROD CHOICES -----
  observeEvent(input$reset_all,ignoreNULL = TRUE, ignoreInit = TRUE, {
    updateSwitchInput(session = session, inputId = "left_supplemental_rods_eligible", value = FALSE)
  })
  
  # observeEvent(input$plot_click, ignoreNULL = TRUE, ignoreInit = TRUE, {
  #   free_screws_vector <- unique(append(jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left")$level, left_revision_implants_reactive_list()$free_revision_screws_vector))
  #   # print(paste(free_screws_vector))
  #   if(input$left_supplemental_rods_eligible == FALSE && length(free_screws_vector) > 2){
  #     updateSwitchInput(session = session, inputId = "left_supplemental_rods_eligible", value = TRUE)
  #   }
  # })
  
  # observeEvent(input$plot_click, ignoreNULL = TRUE, ignoreInit = TRUE, {
  #   
  #   if(input$left_supplemental_rods_eligible == FALSE){
  #     free_screws_vector <- unique(append(jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left")$level, 
  #                                         left_revision_implants_reactive_list()$free_revision_screws_vector))
  #     # print(paste(free_screws_vector))
  #     # if(input$left_supplemental_rods_eligible == FALSE && length(free_screws_vector) > 2){
  #     if(length(free_screws_vector) > 2){
  #       updateSwitchInput(session = session, inputId = "left_supplemental_rods_eligible", value = TRUE)
  #     } 
  #   }
  # })
 
  observeEvent(input$add_left_accessory_rod,  ignoreInit = TRUE, {
    if(input$add_left_accessory_rod == TRUE){
      
      
      start_list <- jh_supplementary_rods_choices_function(all_objects_df = jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left"),
                                                           revision_objects_retained_df = left_revision_implants_reactive_list()$revision_implants_status_df,
                                                           rod_type = "accessory_rod")
      updateSliderTextInput(session = session,
                            inputId = "left_accessory_rod",
                            choices =  start_list$full_level_range,
                            selected =  start_list$supplemental_starts)
    }else{
      updateSliderTextInput(session = session,
                            inputId = "left_accessory_rod", 
                            label = "Select Overlap Region:",
                            choices = c("a", "b"),
                            selected = c("a", "b")
      )
    }
  }
  )
  ### UPDATE ROD SIZE AND MATERIAL WHEN SELECTED
  observeEvent(input$add_left_accessory_rod, ignoreInit = TRUE, ignoreNULL = TRUE, {
    jh_update_supplemental_rod_material_size_selected_function(session_input = session, 
                                                               input_add_side_supplemental_rod = input$add_left_accessory_rod, 
                                                               side_supplemental_rod = "left_accessory_rod")
  })
  observeEvent(input$add_left_satellite_rod, ignoreInit = TRUE, ignoreNULL = TRUE, {
    jh_update_supplemental_rod_material_size_selected_function(session_input = session, 
                                                               input_add_side_supplemental_rod = input$add_left_satellite_rod, 
                                                               side_supplemental_rod = "left_satellite_rod")
  })
  observeEvent(input$add_left_intercalary_rod, ignoreInit = TRUE, ignoreNULL = TRUE, {
    jh_update_supplemental_rod_material_size_selected_function(session_input = session, 
                                                               input_add_side_supplemental_rod = input$add_left_intercalary_rod, 
                                                               side_supplemental_rod = "left_intercalary_rod")
  })
  observeEvent(input$add_left_linked_rod, ignoreInit = TRUE, ignoreNULL = TRUE, {
    jh_update_supplemental_rod_material_size_selected_function(session_input = session, 
                                                               input_add_side_supplemental_rod = input$add_left_linked_rod, 
                                                               side_supplemental_rod = "left_linked_rod")
  })
  observeEvent(input$add_left_kickstand_rod, ignoreInit = TRUE, ignoreNULL = TRUE, {
    jh_update_supplemental_rod_material_size_selected_function(session_input = session, 
                                                               input_add_side_supplemental_rod = input$add_left_kickstand_rod, 
                                                               side_supplemental_rod = "left_kickstand_rod")
  })
  
  observeEvent(input$add_left_satellite_rod,  ignoreInit = TRUE, {
    if(input$add_left_satellite_rod == TRUE){
      start_list <- jh_supplementary_rods_choices_function(all_objects_df = jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left"),
                                                           revision_objects_retained_df = left_revision_implants_reactive_list()$revision_implants_status_df,
                                                           osteotomy_site = osteotomy_level_reactive(),
                                                           rod_type = "satellite_rod")
      
      updateSliderTextInput(session = session,
                            inputId = "left_satellite_rod",
                            label = "Select Levels For Satellite Rod:",
                            choices =  start_list$supplemental_choices,
                            selected =  start_list$supplemental_starts)
    }else{
      updateSliderTextInput(session = session,
                            inputId = "left_satellite_rod", 
                            label = "Select Overlap Region:",
                            choices = c("a", "b"),
                            selected = c("a", "b")
      )
    }
  }
  )
  
  observeEvent(list(input$add_left_intercalary_rod), ignoreInit = TRUE, {
    if(input$add_left_intercalary_rod == TRUE){
      if(length(left_revision_implants_reactive_list()$free_revision_screws_vector)>0){
        left_implants_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left") %>%
          bind_rows(left_revision_implants_reactive_list()$revision_implants_status_df %>%
                      filter(level %in% left_revision_implants_reactive_list()$free_revision_screws_vector)) %>%
          arrange(vertebral_number)
        
      }else{
        left_implants_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left") %>%
          arrange(vertebral_number) 
      }
      
      distal_limit <- if_else(tail(left_implants_df$vertebral_number, 2)[1] >= 25, 25, tail(left_implants_df$vertebral_number, 2)[1])
      
      junction_choices_vector <- (implant_levels_numbered_df %>%
                                    filter(between(vertebral_number, left_implants_df$vertebral_number[2], distal_limit)) %>%
                                    filter(level %in% left_implants_df$level  == FALSE ))$level
      
      if(!is.null(osteotomy_level_reactive()) && osteotomy_level_reactive()[1] %in% junction_choices_vector){
        starting_junction <- osteotomy_level_reactive()[1]
        
      }else{
        starting_junction <- junction_choices_vector[round(length(junction_choices_vector)/2, 0)]
      }
      
      if(length(junction_choices_vector) > 0){
        updatePickerInput(session = session, 
                          inputId = "left_intercalary_rod_junction",
                          label = "Junction:",
                          choices = junction_choices_vector,
                          selected = starting_junction
        )
      }
    }else{
      updatePickerInput(session = session,
                        inputId = "left_intercalary_rod_junction",
                        choices = c("a", "b"), 
                        selected = "a")
    }
  }
  )
  
  
  observeEvent(list(input$add_left_intercalary_rod, input$left_intercalary_rod_junction), ignoreInit = TRUE, {

    if(input$add_left_intercalary_rod == TRUE && input$left_intercalary_rod_junction %in% implant_levels_numbered_df$level){
      
      if(length(left_revision_implants_reactive_list()$free_revision_screws_vector)>0){
        left_implant_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left") %>%
          bind_rows(left_revision_implants_reactive_list()$revision_implants_status_df %>%
                      filter(level %in% left_revision_implants_reactive_list()$free_revision_screws_vector)) %>%
          arrange(vertebral_number) %>%
          filter(str_detect(level, "Iliac") == FALSE)
      }else{
        left_implant_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left") %>%
          arrange(vertebral_number) %>%
          filter(str_detect(level, "Iliac") == FALSE)
      }
      
      implants_proximal_to_junction_df <- left_implant_df %>%
        filter(vertebral_number < jh_get_vertebral_number_function(input$left_intercalary_rod_junction))
      
      proximal_options <- (implant_levels_numbered_df %>%
        filter(str_detect(level, "Iliac") == FALSE) %>%
        filter(between(vertebral_number, min(implants_proximal_to_junction_df$vertebral_number), max(implants_proximal_to_junction_df$vertebral_number))))$level
        
        implants_distal_to_junction_df <- left_implant_df %>%
          filter(vertebral_number > jh_get_vertebral_number_function(input$left_intercalary_rod_junction))
        
        distal_options <- (implant_levels_numbered_df %>%
                               filter(str_detect(level, "Iliac") == FALSE) %>%
                               filter(between(vertebral_number, min(implants_distal_to_junction_df$vertebral_number), max(implants_distal_to_junction_df$vertebral_number))))$level
        
        if(length((left_revision_implants_reactive_list()$retained_df)$level)>1){
          overlap_vector_choices_with_revision <- unique(append(proximal_options, distal_options))
          
          overlap_vector_choices <- as.vector(jh_reorder_levels_function(unique(append(overlap_vector_choices_with_revision, (left_revision_implants_reactive_list()$retained_df)$level))))
        }else{
          overlap_vector_choices <- unique(append(proximal_options, distal_options)) 
        }
        
        overlap_start <- c(overlap_vector_choices[round(length(overlap_vector_choices)*0.2, 0)], 
                           overlap_vector_choices[round(length(overlap_vector_choices)*0.8, 0)])
      
      updateSliderTextInput(session = session,
                            inputId = "left_intercalary_rod",
                            label = "Select Overlap Region:",
                            choices =  overlap_vector_choices,
                            selected =  overlap_start)
  
        
    }else{
      updateSliderTextInput(session = session,
                            inputId = "left_intercalary_rod", 
                            label = "Select Overlap Region:",
                            choices = c("a", "b"),
                            selected = c("a", "a")
      )
    }
  }
  )
  
  observeEvent(input$left_intercalary_rod,ignoreInit = TRUE, ignoreNULL = TRUE, {

    if(input$add_left_intercalary_rod == TRUE && input$left_intercalary_rod_junction %in% implant_levels_numbered_df$level && input$left_intercalary_rod[1] %in% implant_levels_numbered_df$level){
      if(between(jh_get_vertebral_number_function(input$left_intercalary_rod_junction),
                 jh_get_vertebral_number_function(input$left_intercalary_rod[1]),
                 jh_get_vertebral_number_function(input$left_intercalary_rod[2 ])) == FALSE){

        if(length(left_revision_implants_reactive_list()$free_revision_screws_vector)>0){
          left_implant_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left") %>%
            bind_rows(left_revision_implants_reactive_list()$revision_implants_status_df %>%
                        filter(level %in% left_revision_implants_reactive_list()$free_revision_screws_vector)) %>%
            arrange(vertebral_number) %>%
            filter(str_detect(level, "Iliac") == FALSE)
        }else{
          left_implant_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left") %>%
            arrange(vertebral_number) %>%
            filter(str_detect(level, "Iliac") == FALSE)
        }

        implants_proximal_to_junction_df <- left_implant_df %>%
          filter(vertebral_number < jh_get_vertebral_number_function(input$left_intercalary_rod_junction))
        
        proximal_options <- (implant_levels_numbered_df %>%
                               filter(str_detect(level, "Iliac") == FALSE) %>%
                               filter(between(vertebral_number, min(implants_proximal_to_junction_df$vertebral_number), max(implants_proximal_to_junction_df$vertebral_number))))$level
        
        implants_distal_to_junction_df <- left_implant_df %>%
          filter(vertebral_number > jh_get_vertebral_number_function(input$left_intercalary_rod_junction))
        
        distal_options <- (implant_levels_numbered_df %>%
                             filter(str_detect(level, "Iliac") == FALSE) %>%
                             filter(between(vertebral_number, min(implants_distal_to_junction_df$vertebral_number), max(implants_distal_to_junction_df$vertebral_number))))$level
        
        if(length((left_revision_implants_reactive_list()$retained_df)$level)>1){
          overlap_vector_choices_with_revision <- unique(append(proximal_options, distal_options))
          
          overlap_vector_choices <- as.vector(jh_reorder_levels_function(unique(append(overlap_vector_choices_with_revision, (left_revision_implants_reactive_list()$retained_df)$level))))
        }else{
          overlap_vector_choices <- unique(append(proximal_options, distal_options))
        }
        
        overlap_start <- c(overlap_vector_choices[round(length(overlap_vector_choices)*0.2, 0)], 
                           overlap_vector_choices[round(length(overlap_vector_choices)*0.8, 0)])

        show_alert(title = "Either choose a different intercalary junction, or choose an overlap range that includes the junction")
        updateSliderTextInput(session = session,
                              inputId = "left_intercalary_rod",
                              label = "Select Overlap Region:",
                              choices =  overlap_vector_choices,
                              selected =  overlap_start)

      }
    }
  })
  
 
  
  observeEvent(input$add_left_linked_rod,
               ignoreNULL = TRUE, 
               ignoreInit = TRUE, {
                 
                 if(input$add_left_linked_rod == TRUE){
                   start_list <- jh_supplementary_rods_choices_function(all_objects_df = jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left"),
                                                                        revision_objects_retained_df = left_revision_implants_reactive_list()$revision_implants_status_df,
                                                                        rod_type = "linked_rod")
                   
                   updateSliderTextInput(session = session,
                                         inputId = "left_linked_rod", 
                                         label = "Select Overlap Region:",
                                         choices = start_list$full_level_range,
                                         # choices = start_list$implant_levels[2:(length(start_list$implant_levels)-1)],
                                         selected = start_list$supplemental_starts
                   )
                 }else{
                   updateSliderTextInput(session = session,
                                         inputId = "left_linked_rod", 
                                         label = "Select Overlap Region:",
                                         choices = c("a", "b"),
                                         selected = c("a", "b")
                   ) 
                 }
               })
  
  observeEvent(input$add_left_kickstand_rod,
               ignoreNULL = TRUE, 
               ignoreInit = TRUE, {
                 if(input$add_left_kickstand_rod == TRUE){
                   
                   if(any(str_detect(str_to_lower(jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left")$level), "iliac"))){
                     start_list <- jh_supplementary_rods_choices_function(all_objects_df = jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left"),
                                                                          revision_objects_retained_df = left_revision_implants_reactive_list()$revision_implants_status_df,
                                                                          rod_type = "kickstand_rod")
                     
                     updateSliderTextInput(session = session,
                                           inputId = "left_kickstand_rod", 
                                           label = "Select Proximal Point of Attachment:",
                                           choices = start_list$full_level_range,
                                           selected = start_list$supplemental_starts, 
                                           to_fixed = start_list$supplemental_starts[2] 
                     ) 
                   }else{
                     show_alert(title = "No Iliac Screw", text = "You must add an iliac screw to add a kickstand rod.")
                     
                     updateAwesomeCheckbox(session = session, 
                                           inputId = "add_left_kickstand_rod", 
                                           value = FALSE)
                   }
                 }
               })
  
  observe({
    if(input$add_left_custom_rods == TRUE){
      left_implants_df <- all_objects_to_add_list$objects_df %>%
        select(level, side, object, x, y) %>%
        filter(side == "left") %>%
        filter(str_detect(string = object, pattern = "screw") | str_detect(string = object, pattern = "hook") | str_detect(string = object, pattern = "wire")) %>%
        mutate(implant_label = glue("{level} {str_to_title(str_replace_all(object, '_', ' '))}"))
      
      left_implant_choices <- as.list(left_implants_df$level)
      
      names(left_implant_choices) <- left_implants_df$implant_label
      
      updatePickerInput(session = session, 
                        inputId = "left_custom_rod_1", 
                        choices = left_implants_df$implant_label)
      
      updatePickerInput(session = session, 
                        inputId = "left_custom_rod_2", 
                        choices = left_implants_df$implant_label)
      
      updatePickerInput(session = session, 
                        inputId = "left_custom_rod_3", 
                        choices = left_implants_df$implant_label)
      
      updatePickerInput(session = session, 
                        inputId = "left_custom_rod_4", 
                        choices = left_implants_df$implant_label)
      
      updatePickerInput(session = session, 
                        inputId = "left_custom_rod_5", 
                        choices = left_implants_df$implant_label)
    }
  }) %>%
    bindEvent(input$add_left_custom_rods, input$left_custom_rods_number, ignoreInit = TRUE)

  ##### UPDATE SUPPLEMENT ROD CHOICES END -----
  ##### UPDATE SUPPLEMENT ROD CHOICES END -----
  
    ######### ######### ROD SIZE AND ROD MATERIAL ######### #########
  # observeEvent(jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left"), ignoreNULL = TRUE, ignoreInit = TRUE, {
  observeEvent(input$add_rods, ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(nrow(jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left")) > 1){
      if(max(jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left")$vertebral_number) < 11){
        rod_size <- "4.0mm"  
      }else{
        rod_size <- "6.0mm"  
      }
      if(input$left_main_rod_size == "None"){
        updatePickerInput(session = session, 
                          inputId = "left_main_rod_size", 
                          selected = rod_size
        )
      }
      if(input$left_main_rod_material == "Non-instrumented"){
        updateAwesomeRadio(session = session, 
                           inputId = "left_main_rod_material",
                           inline = TRUE,
                           choices = c("Titanium", "Cobalt Chrome", "Stainless Steel"),
                           selected = "Titanium"
        )
      }
    }
  })
  
  
  ####################### MAKE LEFT ROD GEOMS ######################
  ####################### MAKE LEFT ROD GEOMS ######################
  ####################### MAKE LEFT ROD GEOMS ######################
  ###### REVISION RODS ---
  ###### REVISION RODS ---
  left_revision_rod_matrix_reactive <- reactive({
    left_revision_rod_matrix <- as.matrix(tibble(x = double(), y = double()))
    
    if(nrow(left_revision_implants_reactive_list()$retained_df)>1){
      if(input$left_revision_rod_status == "retained"){
        
        left_revision_rod_matrix <- left_revision_implants_reactive_list()$retained_df %>%
          select(x, y) %>%
          mutate(y = if_else(y == max(y), y + 0.005, y)) %>%
          mutate(y = if_else(y == min(y), y - 0.005, y)) %>%
          mutate(x = if_else(x < 0.5, x + 0.003, x - 0.003)) %>%
          arrange(y) %>%
          distinct() %>%
          as.matrix()
        
        # rods_list$left_revision_rod_sf <- geom_sf(data = st_buffer(st_linestring(left_revision_rod_matrix), dist = 0.003, endCapStyle = "ROUND"), fill = "black")
      }else if(length(input$left_revision_implants_connected_to_prior_rod)>0 && input$left_revision_rod_status == "retained_cut"){
        left_prior_implants_connected_df <-  left_revision_implants_reactive_list()$revision_implants_status_df %>%
          filter(prior_rod_connected == "yes") 
        
        if(min(jh_get_vertebral_number_function(input$left_revision_implants_connected_to_prior_rod)) < jh_get_vertebral_number_function(input$left_revision_rod_cut_level)){ 
          ##this would indicate the proximal portion was kept
          level_min <- min(jh_get_vertebral_number_function(input$left_revision_implants_connected_to_prior_rod))
          level_max <- jh_get_vertebral_number_function(input$left_revision_rod_cut_level)
        }else{ ## this would indicate the distal rod was retained
          level_min <- jh_get_vertebral_number_function(input$left_revision_rod_cut_level) 
          level_max <- max(jh_get_vertebral_number_function(input$left_revision_implants_connected_to_prior_rod))
        }
        
        # left_revision_rod_matrix <- all_screw_coordinates_df %>%
          left_revision_rod_matrix <- tibble(level = input$left_revision_implants, side = "left") %>%
          # filter(side == "left") %>%
          # select(level, object) %>%
          #   mutate(side == "left") %>%
            left_join(revision_screws_df) %>%
          filter(between(vertebral_number, level_min, level_max))%>%
          mutate(y = if_else(y == max(y), y + 0.005, y)) %>%
          mutate(y = if_else(y == min(y), y - 0.005, y)) %>%
          mutate(x = if_else(x < 0.5, x + 0.004, x - 0.004)) %>%
          select(x, y) %>%
          arrange(y) %>%
          distinct() %>%
          as.matrix()
      } 
    }
    left_revision_rod_matrix
  })
  
  observeEvent(input$close_startup_modal_2, ignoreInit = TRUE, ignoreNULL = TRUE, {
    
    if(nrow(as_tibble(left_revision_rod_matrix_reactive()))>1 && is.numeric(left_revision_rod_matrix_reactive())){
      rods_list$left_revision_rod_sf <- geom_sf(data = st_buffer(st_linestring(left_revision_rod_matrix_reactive()), dist = 0.003, endCapStyle = "ROUND"), fill = "black")
    }else{
      rods_list$left_revision_rod_sf <- geom_sf(data = NULL)
    }
  })
  
  observeEvent(input$close_startup_modal_2, ignoreInit = TRUE, ignoreNULL = TRUE, {
    
    if(nrow(left_revision_implants_reactive_list()$retained_df)>1){
      if(input$left_revision_rod_status == "removed"){
        rods_list$left_revision_rod_sf <- geom_sf(data = NULL)
        
      }else if(input$left_revision_rod_status == "retained"){
        
        left_revision_rod_matrix <- left_revision_implants_reactive_list()$retained_df %>%
          select(x, y) %>%
          mutate(y = if_else(y == max(y), y + 0.005, y)) %>%
          mutate(y = if_else(y == min(y), y - 0.005, y)) %>%
          mutate(x = if_else(x < 0.5, x + 0.003, x - 0.003)) %>%
          arrange(y) %>%
          distinct() %>%
          as.matrix()
        
        rods_list$left_revision_rod_sf <- geom_sf(data = st_buffer(st_linestring(left_revision_rod_matrix), dist = 0.003, endCapStyle = "ROUND"), fill = "black")
      }else if(input$left_revision_rod_status == "retained_cut"){
        left_prior_implants_connected_df <-  left_revision_implants_reactive_list()$revision_implants_status_df %>%
          filter(prior_rod_connected == "yes") 
        
        if(min(jh_get_vertebral_number_function(input$left_revision_implants_connected_to_prior_rod)) < jh_get_vertebral_number_function(input$left_revision_rod_cut_level)){ 
          ##this would indicate the proximal portion was kept
          level_min <- min(jh_get_vertebral_number_function(input$left_revision_implants_connected_to_prior_rod))
          level_max <- jh_get_vertebral_number_function(input$left_revision_rod_cut_level)
          
          left_revision_rod_matrix <- all_screw_coordinates_df %>%
            filter(side == "left") %>%
            filter(between(vertebral_number, level_min, level_max))%>%
            mutate(y = if_else(y == max(y), y + 0.005, y)) %>%
            mutate(y = if_else(y == min(y), y - 0.005, y)) %>%
            mutate(x = if_else(x < 0.5, x + 0.004, x - 0.004)) %>%
            mutate(x = case_when(
              x < 0.5 & vertebral_number > max(jh_get_vertebral_number_function(input$left_revision_implants_connected_to_prior_rod) + 0.25) ~ x + 0.005,
              x > 0.5 & vertebral_number > max(jh_get_vertebral_number_function(input$left_revision_implants_connected_to_prior_rod) + 0.25) ~ x - 0.005,
              TRUE ~ x
            )) %>%
            select(x, y) %>%
            arrange(y) %>%
            distinct() %>%
            as.matrix()
          
        }else{ ## this would indicate the distal rod was retained
          level_min <- jh_get_vertebral_number_function(input$left_revision_rod_cut_level) 
          level_max <- max(jh_get_vertebral_number_function(input$left_revision_implants_connected_to_prior_rod))
          
          # left_revision_rod_matrix <- all_screw_coordinates_df %>%
          # left_revision_implants_reactive_list$revision_implants_status_df
          
          left_revision_rod_matrix <-  tibble(level = input$left_revision_implants, side = "left") %>%
            # filter(side == "left") %>%
            left_join(revision_screws_df) %>%
            filter(between(vertebral_number, level_min, level_max))%>%
            mutate(y = if_else(y == max(y), y + 0.005, y)) %>%
            mutate(y = if_else(y == min(y), y - 0.005, y)) %>%
            mutate(x = if_else(x < 0.5, x + 0.004, x - 0.004)) %>%
            mutate(x = case_when(
              x < 0.5 & vertebral_number < min(jh_get_vertebral_number_function(input$left_revision_implants_connected_to_prior_rod)) - 0.25 ~ x + 0.005,
              x > 0.5 & vertebral_number < min(jh_get_vertebral_number_function(input$left_revision_implants_connected_to_prior_rod)) - 0.25 ~ x - 0.005,
              TRUE ~ x
            )) %>%
            select(x, y) %>%
            arrange(y) %>%
            distinct() %>%
            as.matrix()
        }
         if(is.numeric(left_revision_rod_matrix)){
          rods_list$left_revision_rod_sf <- geom_sf(data = st_buffer(st_linestring(left_revision_rod_matrix), dist = 0.003, endCapStyle = "ROUND"), fill = "black")
        }
        
      }else{
        geoms_list_revision_posterior$left_revision_rod_sf <- geom_sf(data = NULL)
        rods_list$left_revision_rod_sf <- geom_sf(data = NULL)
      }
    }
  })
  

  ###### REVISION RODS END ---
  ###### REVISION RODS END ---
  
  ###### LEFT SUPPLEMENTAL ROD GEOM 
  ###### LEFT SUPPLEMENTAL ROD GEOM 
  observe({
    ##########RODS ############
    ############# left ROD #################
    if(input$add_rods > 0){
      if(input$add_left_accessory_rod == TRUE && input$left_accessory_rod[1] %in% all_screw_coordinates_df$level && length(unique(input$left_accessory_rod)) == 2){
        accessory_vector <- input$left_accessory_rod
        
      }else{
        accessory_vector <- c("a")
      }
      if(input$add_left_satellite_rod == TRUE && input$left_satellite_rod[1] %in% all_screw_coordinates_df$level && length(unique(input$left_satellite_rod)) == 2){
        satellite_vector <- input$left_satellite_rod
      }else{
        satellite_vector <- c("a", "b")
      }
      if(input$add_left_intercalary_rod == TRUE && input$left_intercalary_rod[1] %in% all_screw_coordinates_df$level && length(unique(input$left_intercalary_rod)) == 2){
        intercalary_vector <- input$left_intercalary_rod
        junction <- input$left_intercalary_rod_junction
      }else{
        intercalary_vector <- c("a")
        junction <- NULL
      }
      if(input$add_left_linked_rod == TRUE && input$left_linked_rod[1] %in% all_screw_coordinates_df$level && length(unique(input$left_linked_rod)) == 2){
        linked_vector <- input$left_linked_rod
      }else{
        linked_vector <- c("a")
      }
      
      if(input$add_left_kickstand_rod == TRUE && input$left_kickstand_rod[1] %in% all_screw_coordinates_df$level && length(unique(input$left_kickstand_rod)) == 2){
        left_kickstand_rod_vector <- input$left_kickstand_rod
      }else{
        left_kickstand_rod_vector <- c("a")
      }
      
      custom_rods_vector_list <- list()
      if(input$add_left_custom_rods == TRUE){
        if(input$left_custom_rods_number > 1 & length(input$left_custom_rod_1) > 1){
          custom_rods_vector_list$custom_rod_1 <- input$left_custom_rod_1
        }else{
          custom_rods_vector_list$custom_rod_1 <- c("")
        }
        if(input$left_custom_rods_number > 1 & length(input$left_custom_rod_2) > 1){
          custom_rods_vector_list$custom_rod_2 <- input$left_custom_rod_2
        }else{
          custom_rods_vector_list$custom_rod_2 <- c("")
        }
        if(input$left_custom_rods_number > 2 & length(input$left_custom_rod_3) > 1){
          custom_rods_vector_list$custom_rod_3 <- input$left_custom_rod_3
        }else{
          custom_rods_vector_list$custom_rod_3 <- c("")
        }
        if(input$left_custom_rods_number > 3 & length(input$left_custom_rod_4) > 1){
          custom_rods_vector_list$custom_rod_4 <- input$left_custom_rod_4
        }else{
          custom_rods_vector_list$custom_rod_4 <- c("")
        }
        if(input$left_custom_rods_number > 4 & length(input$left_custom_rod_5) > 1){
          custom_rods_vector_list$custom_rod_5 <- input$left_custom_rod_5
        }else{
          custom_rods_vector_list$custom_rod_5 <- c("")
        }
      }
      
      if(length(left_revision_implants_reactive_list()$free_revision_screws_vector)>0){
        left_retained_free_screws_df <- left_revision_implants_reactive_list()$revision_implants_status_df %>%
          filter(remove_retain == "retain", prior_rod_connected == "no") %>%
          select(level, vertebral_number, x, y, side, object)
        
        left_implants_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left") %>%
          select(level, vertebral_number, x, y, side, object) %>%
          bind_rows(left_retained_free_screws_df) %>%
          arrange(y) %>%
          mutate(implant_label = glue("{level} {str_to_title(str_replace_all(object, '_', ' '))}")) 
        
      }else{
        left_implants_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left") %>%
          select(level, vertebral_number, x, y, side, object) %>%
          arrange(y) %>%
          mutate(implant_label = glue("{level} {str_to_title(str_replace_all(object, '_', ' '))}")) 
      }

      if(length(left_implants_df$level) > 0){
        ############# MAKE THE RODS #############
        left_rods_connectors_list <- build_unilateral_rods_list_function(unilateral_full_implant_df = left_implants_df,
                                                                          rod_side = "left",
                                                                          add_accessory_rod = input$add_left_accessory_rod,
                                                                          accessory_rod_vector = accessory_vector, 
                                                                          add_satellite_rod = input$add_left_satellite_rod,
                                                                          satellite_rods_vector = satellite_vector,
                                                                          add_intercalary_rod = input$add_left_intercalary_rod, 
                                                                         intercalary_rods_vector = intercalary_vector, 
                                                                         intercalary_rod_junction = junction, 
                                                                         add_linked_rod = input$add_left_linked_rod,
                                                                         linked_rod_vector = linked_vector,
                                                                         add_kickstand_rod = input$add_left_kickstand_rod,
                                                                         kickstand_rod_vector = left_kickstand_rod_vector,
                                                                         add_custom_rods = input$add_left_custom_rods,
                                                                         custom_rods_vector_list = custom_rods_vector_list,
                                                                         link_revision_rods_true_false = input$link_left_revision_rods_true_false,
                                                                         revision_rod_matrix = left_revision_rod_matrix_reactive()
        )
        if(length(left_rods_connectors_list$rod_list) > 0){
          rods_list$left_rod_list_sf_geom <- geom_sf(data = st_multipolygon(left_rods_connectors_list$rod_list), alpha = 0.85)
        }else{
          rods_list$left_rod_list_sf_geom <- NULL
        }
        
        if(length(left_rods_connectors_list$connector_list) > 0){
            rods_list$left_connector_list_sf_geom <- geom_sf(data = st_multipolygon(left_rods_connectors_list$connector_list), fill = "lightblue", alpha = 0.95)
        }else{
          rods_list$left_connector_list_sf_geom <- NULL
        }
      }
    }
    
  })
  

  
  
  ###### LEFT SUPPLEMENTAL ROD GEOM END  ---
  ###### LEFT SUPPLEMENTAL ROD GEOM  END --
  
  ############### LEFT ROD ENDS ################# ############### LEFT RODS ENDS ################# ############### LEFT RODS ENDS #################
  ############### LEFT ROD ENDS ################# ############### LEFT RODS ENDS ################# ############### LEFT RODS ENDS #################
  
  
  
  
  
  #################### LEFT ROD CONSTRUCT ENDS ######################## #################### LEFT ROD CONSTRUCT ENDS ########################
  #################### LEFT ROD CONSTRUCT ENDS ######################## #################### LEFT ROD CONSTRUCT ENDS ########################  
  #################### LEFT ROD CONSTRUCT ENDS ######################## #################### LEFT ROD CONSTRUCT ENDS ########################
  #################### LEFT ROD CONSTRUCT ENDS ######################## #################### LEFT ROD CONSTRUCT ENDS ######################## 
  
  
  #################### RIGHT ROD CONSTRUCT ######################## #################### RIGHT ROD CONSTRUCT ########################
  #################### RIGHT ROD CONSTRUCT ######################## #################### RIGHT ROD CONSTRUCT ########################
  #################### RIGHT ROD CONSTRUCT ######################## #################### RIGHT ROD CONSTRUCT ########################
  #################### RIGHT ROD CONSTRUCT ######################## #################### RIGHT ROD CONSTRUCT ########################
  #################### RIGHT ROD CONSTRUCT ######################## #################### RIGHT ROD CONSTRUCT ########################
  #################### RIGHT ROD CONSTRUCT ######################## #################### RIGHT ROD CONSTRUCT ########################
  # right REVISION IMPLANTS # 
  right_revision_implants_reactive_list <- reactive({
    removed_df <- tibble(level = character(), vertebral_number = double(), x = double(), y = double(), object_constructed = character())
    retained_df <- tibble(level = character(), side = character(), vertebral_number = double(), object = character(), x = double(), y = double(), object_constructed = character())
    revision_implants_status_df <- tibble(level = character(), vertebral_number = double(), side = character(), object = character(), x = double(), y = double(), prior_rod_connected = character(), remove_retain = character(), object_constructed = character())
    free_revision_screws_vector <- c()
    
    if(req(input$revision_approach) == "posterior"){
      
      if(length(input$right_revision_implants)>0){
        if(nrow(existing_patient_data$patient_df)>0){
          revision_implants_status_df <- existing_patient_data$patient_df %>% 
            select(record_id, side, level, object) %>%
            filter(side == "right") %>%
            filter(str_detect(object, "hook|screw")) %>%
            mutate(remove_retain = if_else(level %in% input$right_revision_implants_removed, "remove", "retain")) %>%
            
            group_by(object, level) %>%
            mutate(object = if_else(str_detect(object, "pelvic"), paste0(object, "_", row_number()), object)) %>%
            ungroup() %>%
            
            left_join(all_implants_constructed_df %>%
                        select(level, side, object, object_constructed, vertebral_number, approach, x, y)) %>%
            distinct()%>%
            select(level, vertebral_number, side, object, remove_retain, x, y, object_constructed)
          
        }else{
          revision_implants_status_df <- tibble(level = input$right_revision_implants, side = "right") %>%
            mutate(remove_retain = if_else(level %in% input$right_revision_implants_removed, "remove", "retain")) %>%
            left_join(revision_screws_df) %>% ## this is generated in Load coordinates
            distinct()%>%
            select(level, vertebral_number, side, object, remove_retain, x, y, object_constructed)
        }
        
        if(input$right_revision_rod_status == "removed"){
          revision_implants_status_df <- revision_implants_status_df %>%
            mutate(prior_rod_connected = "no")
        }
        
        if(input$right_revision_rod_status == "retained"){
          revision_implants_status_df <- revision_implants_status_df  %>%
            mutate(prior_rod_connected = "yes")
        }
        
        if(input$right_revision_rod_status == "retained_cut"){
          
          revision_implants_status_df <- revision_implants_status_df %>%
            filter(remove_retain == "retain") %>%
            mutate(prior_rod_connected = if_else(level %in% input$right_revision_implants_connected_to_prior_rod, "yes", "no")) %>%
            union_all(revision_implants_status_df %>%
                        filter(remove_retain == "remove") %>%
                        mutate(prior_rod_connected = "no")) %>%
            arrange(vertebral_number)
          
        }
        
        removed_df <- revision_implants_status_df %>%
          filter(remove_retain == "remove")
        
        retained_df <- revision_implants_status_df %>%
          filter(remove_retain == "retain")
        
        free_revision_screws_vector <- (revision_implants_status_df %>%
                                          filter(remove_retain == "retain") %>%
                                          filter(prior_rod_connected == "no"))$level
      }
    }
    
    retained_df <- retained_df %>%
      distinct()
    removed_df <- removed_df %>%
      distinct()
    revision_implants_status_df <- revision_implants_status_df %>%
      select(-object_constructed) %>%
      distinct()
    
    list(retained_df = retained_df,
         removed_df = removed_df, 
         revision_implants_status_df = revision_implants_status_df, 
         free_revision_screws_vector = free_revision_screws_vector)
    
  }
  )
  
  observeEvent(right_revision_implants_reactive_list(), ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(nrow(right_revision_implants_reactive_list()$retained_df) < 2){
      updateAwesomeRadio(session = session, inputId = "right_revision_rod_status", selected = "removed")
    }
  })
  
  observeEvent(c(input$right_revision_rod_status, input$right_revision_implants_removed), ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(input$right_revision_rod_status == "retained_cut"){
      level_choices_vector <- (all_screw_coordinates_df %>%
                                 filter(side == "right") %>%
                                 filter(str_detect(level, "-") == TRUE) %>%
                                 filter(str_detect(level, "O-|Sacr") == FALSE)%>%
                                 filter(between(vertebral_number,
                                                min(jh_get_vertebral_number_function(input$right_revision_implants)),
                                                max(jh_get_vertebral_number_function(input$right_revision_implants)))))$level
      
      retained_implants <- setdiff(input$right_revision_implants, input$right_revision_implants_removed)
      removed_implants <- input$right_revision_implants_removed
      
      if(length(removed_implants)>0){
        if(mean(jh_get_vertebral_number_function(retained_implants)) > mean(jh_get_vertebral_number_function(removed_implants))){
          ## this would mean the distal rod is retained:
          cut_level <- jh_get_vertebral_level_function(max(jh_get_vertebral_number_function(removed_implants)) + 0.5)
        }else{
          ## this would mean the proximal rod is retained:
          cut_level <- jh_get_vertebral_level_function(min(jh_get_vertebral_number_function(removed_implants)) - 0.5)
        }
      }else{
        cut_level <- level_choices_vector[1]
      }
      
      updatePickerInput(session = session,
                        inputId = "right_revision_rod_cut_level",
                        choices = level_choices_vector, 
                        selected = cut_level
      )
    }
  })
  
  observeEvent(list(input$right_revision_rod_cut_level), ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(input$right_revision_rod_status == "retained_cut"){
      if(!is.null(input$right_revision_rod_cut_level)){
        
        level_choices_vector <- (all_screw_coordinates_df %>%
                                   filter(side == "right") %>%
                                   filter(str_detect(level, "-") == TRUE) %>%
                                   filter(str_detect(level, "O-|Sacr") == FALSE)%>%
                                   filter(between(vertebral_number,
                                                  min(jh_get_vertebral_number_function(input$right_revision_implants)),
                                                  max(jh_get_vertebral_number_function(input$right_revision_implants)))))$level
        
        retained_implants <- setdiff(input$right_revision_implants, input$right_revision_implants_removed)
        removed_implants <- input$right_revision_implants_removed
        if(length(retained_implants)>0 && length(removed_implants)>0){
          if(mean(jh_get_vertebral_number_function(retained_implants)) > mean(jh_get_vertebral_number_function(removed_implants))){
            ## this would mean the distal rod is retained:
            right_retained_implants_connected_to_rod <- keep(.x = retained_implants, .p = ~ jh_get_vertebral_number_function(.x) > jh_get_vertebral_number_function(input$right_revision_rod_cut_level))
            
          }else{
            ## this would mean the proximal rod is retained:
            right_retained_implants_connected_to_rod <- keep(.x = retained_implants, .p = ~ jh_get_vertebral_number_function(.x) < jh_get_vertebral_number_function(input$right_revision_rod_cut_level))
          } 
        }else{
          right_retained_implants_connected_to_rod <- retained_implants
        }
        
        updatePickerInput(session = session, 
                          inputId = "right_revision_implants_connected_to_prior_rod", 
                          choices = retained_implants, 
                          selected = right_retained_implants_connected_to_rod
        )
      }else{
        updatePickerInput(session = session, 
                          inputId = "right_revision_implants_connected_to_prior_rod", 
                          choices = setdiff(input$right_revision_implants, input$right_revision_implants_removed), 
                          selected = setdiff(input$right_revision_implants, input$right_revision_implants_removed)
        )  
      }
    }
  })
  
  
  ######### right REVISION IMPLANTS ENDS ######### 
  ######### right REVISION IMPLANTS ENDS  ######### 
  
  ############### right RODS ################# ############### right RODS ################# ############### right RODS #################
  ############### right RODS ################# ############### right RODS ################# ############### right RODS #################
  
  # right_revision_implants_reactive_list()$retained_df #tibble(level, side, vertebral_number, object, x, y)
  
  ##### UPDATE SUPPLEMENT ROD CHOICES -----
  ##### UPDATE SUPPLEMENT ROD CHOICES -----
  observeEvent(input$reset_all,ignoreNULL = TRUE, ignoreInit = TRUE, {
    updateSwitchInput(session = session, inputId = "right_supplemental_rods_eligible", value = FALSE)
  })
  
  # observeEvent(input$plot_click, ignoreNULL = TRUE, ignoreInit = TRUE, {
  #   if(input$right_supplemental_rods_eligible == FALSE){
  #     free_screws_vector <- unique(append(jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right")$level, right_revision_implants_reactive_list()$free_revision_screws_vector))
  #     # print(paste(free_screws_vector))
  #     # if(input$right_supplemental_rods_eligible == FALSE && length(free_screws_vector) > 2){
  #       if(length(free_screws_vector) > 2){
  #       updateSwitchInput(session = session, inputId = "right_supplemental_rods_eligible", value = TRUE)
  #     } 
  #   }
  # })
  # 
  observeEvent(input$add_right_accessory_rod,  ignoreInit = TRUE, {
    if(input$add_right_accessory_rod == TRUE){
      
      
      start_list <- jh_supplementary_rods_choices_function(all_objects_df = jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right"),
                                                           revision_objects_retained_df = right_revision_implants_reactive_list()$revision_implants_status_df,
                                                           rod_type = "accessory_rod")
      updateSliderTextInput(session = session,
                            inputId = "right_accessory_rod",
                            choices =  start_list$full_level_range,
                            selected =  start_list$supplemental_starts)
    }else{
      updateSliderTextInput(session = session,
                            inputId = "right_accessory_rod", 
                            label = "Select Overlap Region:",
                            choices = c("a", "b"),
                            selected = c("a", "b")
      )
    }
  }
  )
  ### UPDATE ROD SIZE AND MATERIAL WHEN SELECTED
  observeEvent(input$add_right_accessory_rod, ignoreInit = TRUE, ignoreNULL = TRUE, {
    jh_update_supplemental_rod_material_size_selected_function(session_input = session, 
                                                               input_add_side_supplemental_rod = input$add_right_accessory_rod, 
                                                               side_supplemental_rod = "right_accessory_rod")
  })
  observeEvent(input$add_right_satellite_rod, ignoreInit = TRUE, ignoreNULL = TRUE, {
    jh_update_supplemental_rod_material_size_selected_function(session_input = session, 
                                                               input_add_side_supplemental_rod = input$add_right_satellite_rod, 
                                                               side_supplemental_rod = "right_satellite_rod")
  })
  observeEvent(input$add_right_intercalary_rod, ignoreInit = TRUE, ignoreNULL = TRUE, {
    jh_update_supplemental_rod_material_size_selected_function(session_input = session, 
                                                               input_add_side_supplemental_rod = input$add_right_intercalary_rod, 
                                                               side_supplemental_rod = "right_intercalary_rod")
  })
  observeEvent(input$add_right_linked_rod, ignoreInit = TRUE, ignoreNULL = TRUE, {
    jh_update_supplemental_rod_material_size_selected_function(session_input = session, 
                                                               input_add_side_supplemental_rod = input$add_right_linked_rod, 
                                                               side_supplemental_rod = "right_linked_rod")
  })
  observeEvent(input$add_right_kickstand_rod, ignoreInit = TRUE, ignoreNULL = TRUE, {
    jh_update_supplemental_rod_material_size_selected_function(session_input = session, 
                                                               input_add_side_supplemental_rod = input$add_right_kickstand_rod, 
                                                               side_supplemental_rod = "right_kickstand_rod")
  })
  
  observeEvent(input$add_right_satellite_rod,  ignoreInit = TRUE, {
    if(input$add_right_satellite_rod == TRUE){
      start_list <- jh_supplementary_rods_choices_function(all_objects_df = jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right"),
                                                           revision_objects_retained_df = right_revision_implants_reactive_list()$revision_implants_status_df,
                                                           osteotomy_site = osteotomy_level_reactive(),
                                                           rod_type = "satellite_rod")
      
      updateSliderTextInput(session = session,
                            inputId = "right_satellite_rod",
                            label = "Select Levels For Satellite Rod:",
                            choices =  start_list$supplemental_choices,
                            selected =  start_list$supplemental_starts)
    }else{
      updateSliderTextInput(session = session,
                            inputId = "right_satellite_rod", 
                            label = "Select Overlap Region:",
                            choices = c("a", "b"),
                            selected = c("a", "b")
      )
    }
  }
  )
  
  observeEvent(list(input$add_right_intercalary_rod), ignoreInit = TRUE, {
    if(input$add_right_intercalary_rod == TRUE){
      if(length(right_revision_implants_reactive_list()$free_revision_screws_vector)>0){
        right_implants_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right") %>%
          bind_rows(right_revision_implants_reactive_list()$revision_implants_status_df %>%
                      filter(level %in% right_revision_implants_reactive_list()$free_revision_screws_vector)) %>%
          arrange(vertebral_number)
        
      }else{
        right_implants_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right") %>%
          arrange(vertebral_number) 
      }
      
      distal_limit <- if_else(tail(right_implants_df$vertebral_number, 2)[1] >= 25, 25, tail(right_implants_df$vertebral_number, 2)[1])
      
      junction_choices_vector <- (implant_levels_numbered_df %>%
                                    filter(between(vertebral_number, right_implants_df$vertebral_number[2], distal_limit)) %>%
                                    filter(level %in% right_implants_df$level  == FALSE ))$level
      
      if(!is.null(osteotomy_level_reactive()) && osteotomy_level_reactive()[1] %in% junction_choices_vector){
        starting_junction <- osteotomy_level_reactive()[1]
        
      }else{
        starting_junction <- junction_choices_vector[round(length(junction_choices_vector)/2, 0)]
      }
      
      if(length(junction_choices_vector) > 0){
        updatePickerInput(session = session, 
                          inputId = "right_intercalary_rod_junction",
                          label = "Junction:",
                          choices = junction_choices_vector,
                          selected = starting_junction
        )
      }
    }else{
      updatePickerInput(session = session,
                        inputId = "right_intercalary_rod_junction",
                        choices = c("a", "b"), 
                        selected = "a")
    }
  }
  )
  
  observeEvent(list(input$add_right_intercalary_rod, input$right_intercalary_rod_junction), ignoreInit = TRUE, {
    
    if(input$add_right_intercalary_rod == TRUE && input$right_intercalary_rod_junction %in% implant_levels_numbered_df$level){
      
      if(length(right_revision_implants_reactive_list()$free_revision_screws_vector)>0){
        right_implant_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right") %>%
          bind_rows(right_revision_implants_reactive_list()$revision_implants_status_df %>%
                      filter(level %in% right_revision_implants_reactive_list()$free_revision_screws_vector)) %>%
          arrange(vertebral_number) %>%
          filter(str_detect(level, "Iliac") == FALSE)
      }else{
        right_implant_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right") %>%
          arrange(vertebral_number) %>%
          filter(str_detect(level, "Iliac") == FALSE)
      }
      
      implants_proximal_to_junction_df <- right_implant_df %>%
        filter(vertebral_number < jh_get_vertebral_number_function(input$right_intercalary_rod_junction))
      
      proximal_options <- (implant_levels_numbered_df %>%
                             filter(str_detect(level, "Iliac") == FALSE) %>%
                             filter(between(vertebral_number, min(implants_proximal_to_junction_df$vertebral_number), max(implants_proximal_to_junction_df$vertebral_number))))$level
      
      implants_distal_to_junction_df <- right_implant_df %>%
        filter(vertebral_number > jh_get_vertebral_number_function(input$right_intercalary_rod_junction))
      
      distal_options <- (implant_levels_numbered_df %>%
                           filter(str_detect(level, "Iliac") == FALSE) %>%
                           filter(between(vertebral_number, min(implants_distal_to_junction_df$vertebral_number), max(implants_distal_to_junction_df$vertebral_number))))$level
      
      if(length((right_revision_implants_reactive_list()$retained_df)$level)>1){
        overlap_vector_choices_with_revision <- unique(append(proximal_options, distal_options))
        
        overlap_vector_choices <- as.vector(jh_reorder_levels_function(unique(append(overlap_vector_choices_with_revision, (right_revision_implants_reactive_list()$retained_df)$level))))
      }else{
        overlap_vector_choices <- unique(append(proximal_options, distal_options))
      }
      
      overlap_start <- c(overlap_vector_choices[round(length(overlap_vector_choices)*0.2, 0)], 
                         overlap_vector_choices[round(length(overlap_vector_choices)*0.8, 0)])
      
      updateSliderTextInput(session = session,
                            inputId = "right_intercalary_rod",
                            label = "Select Overlap Region:",
                            choices =  overlap_vector_choices,
                            selected =  overlap_start)
      
      
    }else{
      updateSliderTextInput(session = session,
                            inputId = "right_intercalary_rod", 
                            label = "Select Overlap Region:",
                            choices = c("a", "b"),
                            selected = c("a", "a")
      )
    }
  }
  )
  
  observeEvent(input$right_intercalary_rod,ignoreInit = TRUE, ignoreNULL = TRUE, {
    
    if(input$add_right_intercalary_rod == TRUE && input$right_intercalary_rod_junction %in% implant_levels_numbered_df$level && input$right_intercalary_rod[1] %in% implant_levels_numbered_df$level){
      if(between(jh_get_vertebral_number_function(input$right_intercalary_rod_junction),
                 jh_get_vertebral_number_function(input$right_intercalary_rod[1]),
                 jh_get_vertebral_number_function(input$right_intercalary_rod[2 ])) == FALSE){
        
        if(length(right_revision_implants_reactive_list()$free_revision_screws_vector)>0){
          right_implant_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right") %>%
            bind_rows(right_revision_implants_reactive_list()$revision_implants_status_df %>%
                        filter(level %in% right_revision_implants_reactive_list()$free_revision_screws_vector)) %>%
            arrange(vertebral_number) %>%
            filter(str_detect(level, "Iliac") == FALSE)
        }else{
          right_implant_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right") %>%
            arrange(vertebral_number) %>%
            filter(str_detect(level, "Iliac") == FALSE)
        }
        
        implants_proximal_to_junction_df <- right_implant_df %>%
          filter(vertebral_number < jh_get_vertebral_number_function(input$right_intercalary_rod_junction))
        
        proximal_options <- (implant_levels_numbered_df %>%
                               filter(str_detect(level, "Iliac") == FALSE) %>%
                               filter(between(vertebral_number, min(implants_proximal_to_junction_df$vertebral_number), max(implants_proximal_to_junction_df$vertebral_number))))$level
        
        implants_distal_to_junction_df <- right_implant_df %>%
          filter(vertebral_number > jh_get_vertebral_number_function(input$right_intercalary_rod_junction))
        
        distal_options <- (implant_levels_numbered_df %>%
                             filter(str_detect(level, "Iliac") == FALSE) %>%
                             filter(between(vertebral_number, min(implants_distal_to_junction_df$vertebral_number), max(implants_distal_to_junction_df$vertebral_number))))$level
        
        
        if(length((right_revision_implants_reactive_list()$retained_df)$level)>1){
          overlap_vector_choices_with_revision <- unique(append(proximal_options, distal_options))
          
          overlap_vector_choices <- as.vector(jh_reorder_levels_function(unique(append(overlap_vector_choices_with_revision, (right_revision_implants_reactive_list()$retained_df)$level))))
        }else{
          overlap_vector_choices <- unique(append(proximal_options, distal_options))
        }
        
        overlap_start <- c(overlap_vector_choices[round(length(overlap_vector_choices)*0.2, 0)], 
                           overlap_vector_choices[round(length(overlap_vector_choices)*0.8, 0)])
        
        show_alert(title = "Either choose a different intercalary junction, or choose an overlap range that includes the junction")
        updateSliderTextInput(session = session,
                              inputId = "right_intercalary_rod",
                              label = "Select Overlap Region:",
                              choices =  overlap_vector_choices,
                              selected =  overlap_start)
        
      }
    }
  })
  
  
  
  observeEvent(input$add_right_linked_rod,
               ignoreNULL = TRUE, 
               ignoreInit = TRUE, {
                 
                 if(input$add_right_linked_rod == TRUE){
                   start_list <- jh_supplementary_rods_choices_function(all_objects_df = jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right"),
                                                                        revision_objects_retained_df = right_revision_implants_reactive_list()$revision_implants_status_df,
                                                                        rod_type = "linked_rod")
                   
                   updateSliderTextInput(session = session,
                                         inputId = "right_linked_rod", 
                                         label = "Select Overlap Region:",
                                         choices = start_list$full_level_range,
                                         # choices = start_list$implant_levels[2:(length(start_list$implant_levels)-1)],
                                         selected = start_list$supplemental_starts
                   )
                 }else{
                   updateSliderTextInput(session = session,
                                         inputId = "right_linked_rod", 
                                         label = "Select Overlap Region:",
                                         choices = c("a", "b"),
                                         selected = c("a", "b")
                   ) 
                 }
               })
  
  observeEvent(input$add_right_kickstand_rod,
               ignoreNULL = TRUE, 
               ignoreInit = TRUE, {
                 if(input$add_right_kickstand_rod == TRUE){
                   
                   if(any(str_detect(str_to_lower(jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right")$level), "iliac"))){
                     start_list <- jh_supplementary_rods_choices_function(all_objects_df = jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right"),
                                                                          revision_objects_retained_df = right_revision_implants_reactive_list()$revision_implants_status_df,
                                                                          rod_type = "kickstand_rod")
                     
                     updateSliderTextInput(session = session,
                                           inputId = "right_kickstand_rod", 
                                           label = "Select Proximal Point of Attachment:",
                                           choices = start_list$full_level_range,
                                           selected = start_list$supplemental_starts, 
                                           to_fixed = start_list$supplemental_starts[2] 
                     ) 
                   }else{
                     show_alert(title = "No Iliac Screw", text = "You must add an iliac screw to add a kickstand rod.")
                     
                     updateAwesomeCheckbox(session = session, 
                                           inputId = "add_right_kickstand_rod", 
                                           value = FALSE)
                   }
                 }
               })
  
  observe({
    if(input$add_right_custom_rods == TRUE){
      right_implants_df <- all_objects_to_add_list$objects_df %>%
        select(level, side, object, x, y) %>%
        filter(side == "right") %>%
        filter(str_detect(string = object, pattern = "screw") | str_detect(string = object, pattern = "hook") | str_detect(string = object, pattern = "wire")) %>%
        mutate(implant_label = glue("{level} {str_to_title(str_replace_all(object, '_', ' '))}"))
      
      right_implant_choices <- as.list(right_implants_df$level)
      
      names(right_implant_choices) <- right_implants_df$implant_label
      
      updatePickerInput(session = session, 
                        inputId = "right_custom_rod_1", 
                        choices = right_implants_df$implant_label)
      
      updatePickerInput(session = session, 
                        inputId = "right_custom_rod_2", 
                        choices = right_implants_df$implant_label)
      
      updatePickerInput(session = session, 
                        inputId = "right_custom_rod_3", 
                        choices = right_implants_df$implant_label)
      
      updatePickerInput(session = session, 
                        inputId = "right_custom_rod_4", 
                        choices = right_implants_df$implant_label)
      
      updatePickerInput(session = session, 
                        inputId = "right_custom_rod_5", 
                        choices = right_implants_df$implant_label)
    }
  }) %>%
    bindEvent(input$add_right_custom_rods, input$right_custom_rods_number, ignoreInit = TRUE)
  
  ##### UPDATE SUPPLEMENT ROD CHOICES END -----
  ##### UPDATE SUPPLEMENT ROD CHOICES END -----
  
  ######### ######### ROD SIZE AND ROD MATERIAL ######### #########
  # observeEvent(jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right"), ignoreNULL = TRUE, ignoreInit = TRUE, {
  observeEvent(input$add_rods, ignoreNULL = TRUE, ignoreInit = TRUE, {
    if(nrow(jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right")) > 1){
      if(max(jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right")$vertebral_number) < 11){
        rod_size <- "4.0mm"  
      }else{
        rod_size <- "6.0mm"  
      }
      if(input$right_main_rod_size == "None"){
        updatePickerInput(session = session, 
                          inputId = "right_main_rod_size", 
                          selected = rod_size
        )
      }
      if(input$right_main_rod_material == "Non-instrumented"){
        updateAwesomeRadio(session = session, 
                           inputId = "right_main_rod_material",
                           inline = TRUE,
                           choices = c("Titanium", "Cobalt Chrome", "Stainless Steel"),
                           selected = "Titanium"
        )
      }
    }
  })
  
  
  ####################### MAKE right ROD GEOMS ######################
  ####################### MAKE right ROD GEOMS ######################
  ####################### MAKE right ROD GEOMS ######################
  ###### REVISION RODS ---
  ###### REVISION RODS ---
  right_revision_rod_matrix_reactive <- reactive({
    right_revision_rod_matrix <- as.matrix(tibble(x = double(), y = double()))
    if(nrow(right_revision_implants_reactive_list()$retained_df)>1){
      if(input$right_revision_rod_status == "retained"){
        
        right_revision_rod_matrix <- right_revision_implants_reactive_list()$retained_df %>%
          select(x, y) %>%
          mutate(y = if_else(y == max(y), y + 0.005, y)) %>%
          mutate(y = if_else(y == min(y), y - 0.005, y)) %>%
          mutate(x = if_else(x < 0.5, x + 0.003, x - 0.003)) %>%
          arrange(y) %>%
          distinct() %>%
          as.matrix()
        
        # rods_list$right_revision_rod_sf <- geom_sf(data = st_buffer(st_linestring(right_revision_rod_matrix), dist = 0.003, endCapStyle = "ROUND"), fill = "black")
      }else if(length(input$right_revision_implants_connected_to_prior_rod)>0 && input$right_revision_rod_status == "retained_cut"){
        right_prior_implants_connected_df <-  right_revision_implants_reactive_list()$revision_implants_status_df %>%
          filter(prior_rod_connected == "yes") 
        
        if(min(jh_get_vertebral_number_function(input$right_revision_implants_connected_to_prior_rod)) < jh_get_vertebral_number_function(input$right_revision_rod_cut_level)){ 
          ##this would indicate the proximal portion was kept
          level_min <- min(jh_get_vertebral_number_function(input$right_revision_implants_connected_to_prior_rod))
          level_max <- jh_get_vertebral_number_function(input$right_revision_rod_cut_level)
        }else{ ## this would indicate the distal rod was retained
          level_min <- jh_get_vertebral_number_function(input$right_revision_rod_cut_level) 
          level_max <- max(jh_get_vertebral_number_function(input$right_revision_implants_connected_to_prior_rod))
        }
        # right_revision_rod_matrix <- all_screw_coordinates_df %>%
        right_revision_rod_matrix <- tibble(level = input$right_revision_implants, side = "right") %>%
          # filter(side == "right") %>%
          left_join(revision_screws_df) %>%
          filter(between(vertebral_number, level_min, level_max))%>%
          mutate(y = if_else(y == max(y), y + 0.005, y)) %>%
          mutate(y = if_else(y == min(y), y - 0.005, y)) %>%
          mutate(x = if_else(x < 0.5, x + 0.004, x - 0.004)) %>%
          select(x, y) %>%
          arrange(y) %>%
          distinct() %>%
          as.matrix()
      } 
    }
    right_revision_rod_matrix
  })
  
  observeEvent(input$close_startup_modal_2, ignoreInit = TRUE, ignoreNULL = TRUE, {
    
    if(nrow(as_tibble(right_revision_rod_matrix_reactive()))>1 && is.numeric(right_revision_rod_matrix_reactive())){
      rods_list$right_revision_rod_sf <- geom_sf(data = st_buffer(st_linestring(right_revision_rod_matrix_reactive()), dist = 0.003, endCapStyle = "ROUND"), fill = "black")
    }else{
      rods_list$right_revision_rod_sf <- geom_sf(data = NULL)
    }
  })
  
  observeEvent(input$close_startup_modal_2, ignoreInit = TRUE, ignoreNULL = TRUE, {
    
    if(nrow(right_revision_implants_reactive_list()$retained_df)>1){
      if(input$right_revision_rod_status == "removed"){
        rods_list$right_revision_rod_sf <- geom_sf(data = NULL)
        
      }else if(input$right_revision_rod_status == "retained"){
        
        right_revision_rod_matrix <- right_revision_implants_reactive_list()$retained_df %>%
          select(x, y) %>%
          mutate(y = if_else(y == max(y), y + 0.005, y)) %>%
          mutate(y = if_else(y == min(y), y - 0.005, y)) %>%
          mutate(x = if_else(x < 0.5, x + 0.003, x - 0.003)) %>%
          arrange(y) %>%
          distinct() %>%
          as.matrix()
        
        rods_list$right_revision_rod_sf <- geom_sf(data = st_buffer(st_linestring(right_revision_rod_matrix), dist = 0.003, endCapStyle = "ROUND"), fill = "black")
      }else if(input$right_revision_rod_status == "retained_cut"){
        right_prior_implants_connected_df <-  right_revision_implants_reactive_list()$revision_implants_status_df %>%
          filter(prior_rod_connected == "yes") 
        
        if(min(jh_get_vertebral_number_function(input$right_revision_implants_connected_to_prior_rod)) < jh_get_vertebral_number_function(input$right_revision_rod_cut_level)){ 
          ##this would indicate the proximal portion was kept
          level_min <- min(jh_get_vertebral_number_function(input$right_revision_implants_connected_to_prior_rod))
          level_max <- jh_get_vertebral_number_function(input$right_revision_rod_cut_level)
          
          right_revision_rod_matrix <- all_screw_coordinates_df %>%
            filter(side == "right") %>%
            filter(between(vertebral_number, level_min, level_max))%>%
            mutate(y = if_else(y == max(y), y + 0.005, y)) %>%
            mutate(y = if_else(y == min(y), y - 0.005, y)) %>%
            mutate(x = if_else(x < 0.5, x + 0.004, x - 0.004)) %>%
            mutate(x = case_when(
              x < 0.5 & vertebral_number > max(jh_get_vertebral_number_function(input$right_revision_implants_connected_to_prior_rod) + 0.25) ~ x + 0.005,
              x > 0.5 & vertebral_number > max(jh_get_vertebral_number_function(input$right_revision_implants_connected_to_prior_rod) + 0.25) ~ x - 0.005,
              TRUE ~ x
            )) %>%
            select(x, y) %>%
            arrange(y) %>%
            distinct() %>%
            as.matrix()
          
        }else{ ## this would indicate the distal rod was retained
          level_min <- jh_get_vertebral_number_function(input$right_revision_rod_cut_level) 
          level_max <- max(jh_get_vertebral_number_function(input$right_revision_implants_connected_to_prior_rod))
          
          # right_revision_rod_matrix <- all_screw_coordinates_df %>%
          right_revision_rod_matrix <-  tibble(level = input$right_revision_implants, side = "right") %>%
            # filter(side == "right") %>%
            left_join(revision_screws_df) %>%
            filter(between(vertebral_number, level_min, level_max))%>%
            mutate(y = if_else(y == max(y), y + 0.005, y)) %>%
            mutate(y = if_else(y == min(y), y - 0.005, y)) %>%
            mutate(x = if_else(x < 0.5, x + 0.004, x - 0.004)) %>%
            mutate(x = case_when(
              x < 0.5 & vertebral_number < min(jh_get_vertebral_number_function(input$right_revision_implants_connected_to_prior_rod)) - 0.25 ~ x + 0.005,
              x > 0.5 & vertebral_number < min(jh_get_vertebral_number_function(input$right_revision_implants_connected_to_prior_rod)) - 0.25 ~ x - 0.005,
              TRUE ~ x
            )) %>%
            select(x, y) %>%
            arrange(y) %>%
            distinct() %>%
            as.matrix()
        }
        if(is.numeric(right_revision_rod_matrix)){
          rods_list$right_revision_rod_sf <- geom_sf(data = st_buffer(st_linestring(right_revision_rod_matrix), dist = 0.003, endCapStyle = "ROUND"), fill = "black")
        }
        
      }else{
        geoms_list_revision_posterior$right_revision_rod_sf <- geom_sf(data = NULL)
        rods_list$right_revision_rod_sf <- geom_sf(data = NULL)
      }
    }
  })
  
  
  ###### REVISION RODS END ---
  ###### REVISION RODS END ---
  
  ###### right SUPPLEMENTAL ROD GEOM 
  ###### right SUPPLEMENTAL ROD GEOM 
  observe({
    ##########RODS ############
    ############# right ROD #################
    if(input$add_rods > 0){
      # if(any(input$add_right_accessory_rod == TRUE, 
      #        input$add_right_satellite_rod == TRUE,
      #        input$add_right_intercalary_rod == TRUE, 
      #        input$add_right_linked_rod == TRUE, 
      #        input$add_right_kickstand_rod == TRUE, 
      #        input$add_right_kickstand_rod == TRUE
      #        )){
      
      if(input$add_right_accessory_rod == TRUE && input$right_accessory_rod[1] %in% all_screw_coordinates_df$level && length(unique(input$right_accessory_rod)) == 2){
        accessory_vector <- input$right_accessory_rod
        
      }else{
        accessory_vector <- c("a")
      }
      if(input$add_right_satellite_rod == TRUE && input$right_satellite_rod[1] %in% all_screw_coordinates_df$level && length(unique(input$right_satellite_rod)) == 2){
        satellite_vector <- input$right_satellite_rod
      }else{
        satellite_vector <- c("a", "b")
      }
      if(input$add_right_intercalary_rod == TRUE && input$right_intercalary_rod[1] %in% all_screw_coordinates_df$level && length(unique(input$right_intercalary_rod)) == 2){
        intercalary_vector <- input$right_intercalary_rod
        junction <- input$right_intercalary_rod_junction
      }else{
        intercalary_vector <- c("a")
        junction <- NULL
      }
      if(input$add_right_linked_rod == TRUE && input$right_linked_rod[1] %in% all_screw_coordinates_df$level && length(unique(input$right_linked_rod)) == 2){
        linked_vector <- input$right_linked_rod
      }else{
        linked_vector <- c("a")
      }
      
      if(input$add_right_kickstand_rod == TRUE && input$right_kickstand_rod[1] %in% all_screw_coordinates_df$level && length(unique(input$right_kickstand_rod)) == 2){
        right_kickstand_rod_vector <- input$right_kickstand_rod
      }else{
        right_kickstand_rod_vector <- c("a")
      }
      
      custom_rods_vector_list <- list()
      if(input$add_right_custom_rods == TRUE){
        if(input$right_custom_rods_number > 1 & length(input$right_custom_rod_1) > 1){
          custom_rods_vector_list$custom_rod_1 <- input$right_custom_rod_1
        }else{
          custom_rods_vector_list$custom_rod_1 <- c("")
        }
        if(input$right_custom_rods_number > 1 & length(input$right_custom_rod_2) > 1){
          custom_rods_vector_list$custom_rod_2 <- input$right_custom_rod_2
        }else{
          custom_rods_vector_list$custom_rod_2 <- c("")
        }
        if(input$right_custom_rods_number > 2 & length(input$right_custom_rod_3) > 1){
          custom_rods_vector_list$custom_rod_3 <- input$right_custom_rod_3
        }else{
          custom_rods_vector_list$custom_rod_3 <- c("")
        }
        if(input$right_custom_rods_number > 3 & length(input$right_custom_rod_4) > 1){
          custom_rods_vector_list$custom_rod_4 <- input$right_custom_rod_4
        }else{
          custom_rods_vector_list$custom_rod_4 <- c("")
        }
        if(input$right_custom_rods_number > 4 & length(input$right_custom_rod_5) > 1){
          custom_rods_vector_list$custom_rod_5 <- input$right_custom_rod_5
        }else{
          custom_rods_vector_list$custom_rod_5 <- c("")
        }
      }
      
      if(length(right_revision_implants_reactive_list()$free_revision_screws_vector)>0){
        right_retained_free_screws_df <- right_revision_implants_reactive_list()$revision_implants_status_df %>%
          filter(remove_retain == "retain", prior_rod_connected == "no") %>%
          select(level, vertebral_number, x, y, side, object)
        
        right_implants_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right") %>%
          select(level, vertebral_number, x, y, side, object) %>%
          bind_rows(right_retained_free_screws_df) %>%
          arrange(y) %>%
          mutate(implant_label = glue("{level} {str_to_title(str_replace_all(object, '_', ' '))}")) 
        
      }else{
        right_implants_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right") %>%
          select(level, vertebral_number, x, y, side, object) %>%
          arrange(y) %>%
          mutate(implant_label = glue("{level} {str_to_title(str_replace_all(object, '_', ' '))}")) 
      }
      
      if(length(right_implants_df$level) > 0){
        ############# MAKE THE RODS #############
        right_rods_connectors_list <- build_unilateral_rods_list_function(unilateral_full_implant_df = right_implants_df,
                                                                         rod_side = "right",
                                                                         add_accessory_rod = input$add_right_accessory_rod,
                                                                         accessory_rod_vector = accessory_vector, 
                                                                         add_satellite_rod = input$add_right_satellite_rod,
                                                                         satellite_rods_vector = satellite_vector,
                                                                         add_intercalary_rod = input$add_right_intercalary_rod, 
                                                                         intercalary_rods_vector = intercalary_vector, 
                                                                         intercalary_rod_junction = junction, 
                                                                         add_linked_rod = input$add_right_linked_rod,
                                                                         linked_rod_vector = linked_vector,
                                                                         add_kickstand_rod = input$add_right_kickstand_rod,
                                                                         kickstand_rod_vector = right_kickstand_rod_vector,
                                                                         add_custom_rods = input$add_right_custom_rods,
                                                                         custom_rods_vector_list = custom_rods_vector_list,
                                                                         link_revision_rods_true_false = input$link_right_revision_rods_true_false,
                                                                         revision_rod_matrix = right_revision_rod_matrix_reactive()
        )
        if(length(right_rods_connectors_list$rod_list) > 0){
          rods_list$right_rod_list_sf_geom <- geom_sf(data = st_multipolygon(right_rods_connectors_list$rod_list), alpha = 0.85)
        }else{
          rods_list$right_rod_list_sf_geom <- NULL
        }
        if(length(right_rods_connectors_list$connector_list) > 0){
          rods_list$right_connector_list_sf_geom <- geom_sf(data = st_multipolygon(right_rods_connectors_list$connector_list), fill = "lightblue", alpha = 0.95)
        }else{
          rods_list$right_connector_list_sf_geom <- NULL
        }
      }
    }
    
  })
  
  
  ###### RIGHT SUPPLEMENTAL ROD GEOM END  ---
  ###### RIGHT SUPPLEMENTAL ROD GEOM  END --
  
  ############### RIGHT ROD ENDS ################# ############### RIGHT RODS ENDS ################# ############### RIGHT RODS ENDS #################
  ############### RIGHT ROD ENDS ################# ############### RIGHT RODS ENDS ################# ############### RIGHT RODS ENDS #################
  
  #################### RIGHT ROD CONSTRUCT ENDS ######################## #################### RIGHT ROD CONSTRUCT ENDS ########################
  #################### RIGHT ROD CONSTRUCT ENDS ######################## #################### RIGHT ROD CONSTRUCT ENDS ########################  
  #################### RIGHT ROD CONSTRUCT ENDS ######################## #################### RIGHT ROD CONSTRUCT ENDS ########################
  #################### RIGHT ROD CONSTRUCT ENDS ######################## #################### RIGHT ROD CONSTRUCT ENDS ########################
  
  
  #####################  ################### ######################### CLEAR SUPPLEMENTAL RODS IF ADDING MORE IMPLANTS  ########
  #####################  ################### ######################### CLEAR SUPPLEMENTAL RODS IF ADDING MORE IMPLANTS  ########
  
  
  observeEvent(list(input$plot_click,input$double_click, input$reset_all), ignoreInit = TRUE, ignoreNULL = TRUE, {
    if(str_detect(input$object_to_add, "screw|hook|wire") && 
       any(input$add_left_accessory_rod, 
           input$add_left_satellite_rod,
           input$add_left_intercalary_rod,
           input$add_left_linked_rod,
           input$add_left_kickstand_rod, 
           input$add_left_custom_rods, 
           input$add_right_accessory_rod, 
           input$add_right_satellite_rod,
           input$add_right_intercalary_rod,
           input$add_right_linked_rod,
           input$add_right_kickstand_rod, 
           input$add_right_custom_rods)){
        updateAwesomeCheckbox(session = session, 
                              inputId = "add_left_accessory_rod", 
                              value = FALSE)
        
        updateAwesomeCheckbox(session = session, 
                              inputId = "add_left_satellite_rod", 
                              value = FALSE)
        
        updateAwesomeCheckbox(session = session, 
                              inputId = "add_left_intercalary_rod", 
                              value = FALSE)
        
        updateAwesomeCheckbox(session = session, 
                              inputId = "add_left_linked_rod", 
                              value = FALSE)
        
        updateAwesomeCheckbox(session = session, 
                              inputId = "add_left_kickstand_rod", 
                              value = FALSE)
        
        updateAwesomeCheckbox(session = session, 
                              inputId = "add_left_custom_rods", 
                              value = FALSE) 
        
        updateAwesomeCheckbox(session = session, 
                              inputId = "add_right_accessory_rod",  
                              value = FALSE)
        
        updateAwesomeCheckbox(session = session, 
                              inputId = "add_right_satellite_rod", 
                              value = FALSE)
        
        updateAwesomeCheckbox(session = session, 
                              inputId = "add_right_intercalary_rod", 
                              value = FALSE)
        
        updateAwesomeCheckbox(session = session, 
                              inputId = "add_right_linked_rod", 
                              value = FALSE)
        
        updateAwesomeCheckbox(session = session, 
                              inputId = "add_right_kickstand_rod", 
                              value = FALSE)
        
        updateAwesomeCheckbox(session = session, 
                              inputId = "add_right_custom_rods", 
                              value = FALSE) 
        
        rods_list$right_connector_list_sf_geom <- NULL
        rods_list$left_connector_list_sf_geom <- NULL
    }
    
  })
  
  observeEvent(list(input$add_left_custom_rods,
                    input$left_custom_rod_1,
                    input$left_custom_rod_2,
                    input$left_custom_rod_3,
                    input$left_custom_rod_4,
                    input$left_custom_rod_5), ignoreInit = TRUE, ignoreNULL = TRUE, {
                      updateAwesomeCheckbox(session = session, 
                                            inputId = "add_left_accessory_rod", 
                                            value = FALSE)
                      
                      updateAwesomeCheckbox(session = session, 
                                            inputId = "add_left_satellite_rod", 
                                            value = FALSE)
                      
                      updateAwesomeCheckbox(session = session, 
                                            inputId = "add_left_intercalary_rod", 
                                            value = FALSE)
                      
                      updateAwesomeCheckbox(session = session, 
                                            inputId = "add_left_linked_rod", 
                                            value = FALSE)
                      
                      updateAwesomeCheckbox(session = session, 
                                            inputId = "add_left_kickstand_rod", 
                                            value = FALSE)
                    })
  
  
  
  observeEvent(list(input$add_right_custom_rods,
                    input$right_custom_rod_1,
                    input$right_custom_rod_2,
                    input$right_custom_rod_3,
                    input$right_custom_rod_4,
                    input$right_custom_rod_5), ignoreInit = TRUE, ignoreNULL = TRUE, {
                      
                      updateAwesomeCheckbox(session = session, 
                                            inputId = "add_right_accessory_rod", 
                                            value = FALSE)
                      
                      updateAwesomeCheckbox(session = session, 
                                            inputId = "add_right_satellite_rod", 
                                            value = FALSE)
                      
                      updateAwesomeCheckbox(session = session, 
                                            inputId = "add_right_intercalary_rod", 
                                            value = FALSE)
                      
                      updateAwesomeCheckbox(session = session, 
                                            inputId = "add_right_linked_rod", 
                                            value = FALSE)
                      
                      updateAwesomeCheckbox(session = session, 
                                            inputId = "add_right_kickstand_rod", 
                                            value = FALSE)
                    })
  
  #####################  ################### ######################### CLEAR SUPPLEMENTAL RODS IF ADDING MORE IMPLANTS END#####################  ################### #########################
  #####################  ################### ######################### CLEAR SUPPLEMENTAL RODS IF ADDING MORE IMPLANTS END#####################  ################### #########################
  
  ######### ######### CROSSLINKS ################# #########
  
  observeEvent(input$add_crosslink_button, ignoreInit = TRUE, {
    implants_df <- all_objects_to_add_list$objects_df %>%
      filter(str_detect(object, "screw|hook|wire"))
    
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
  
  observeEvent(input$remove_all_crosslinks, ignoreInit = TRUE, {
    all_objects_to_add_list$objects_df <- all_objects_to_add_list$objects_df %>%
      filter(object != "crosslink")
    
    rods_list$crosslinks <- NULL 
    
    level_options_vector <- jh_get_level_range_vector_function(object_df = (all_objects_to_add_list$objects_df %>%
                                                                              filter(str_detect(object, "screw") | str_detect(object, "hook") | str_detect(object, "wire"))), 
                                                               interspace_or_body_or_all = "body")
    
    updateCheckboxGroupButtons(session = session, 
                               inputId = "crosslink_connectors", 
                               choices = level_options_vector,
                               label = "Add crosslink at:",
                               selected = c(""),
                               checkIcon = list(
                                 yes = tags$i(class = "fa fa-check-square",
                                              style = "color: steelblue"),
                                 no = tags$i(class = "fa fa-square-o",
                                             style = "color: steelblue"))
    )
  })

  observeEvent(input$crosslink_connectors,  ignoreInit = TRUE, {
    if(length(input$crosslink_connectors) > 0){
      all_objects_to_add_list$objects_df <- all_objects_to_add_list$objects_df  %>%
        # filter(object != "crosslink") %>%
        bind_rows(all_implants_constructed_df %>%
                    filter(object == "crosslink", 
                           level %in% input$crosslink_connectors)
        ) %>%
        distinct()
      
      rods_list$crosslinks <- geom_sf(data = st_multipolygon((all_objects_to_add_list$objects_df %>% filter(object == "crosslink"))$object_constructed), alpha = 0.9, fill = "gold") 
    }else{
      all_objects_to_add_list$objects_df <- all_objects_to_add_list$objects_df %>%
        filter(object != "crosslink")
      
      rods_list$crosslinks <- NULL
    }
  })
  
  
  ######################################################################################################################
  
  
  #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE PLOTS    #############~~~~~~~~~~~~~~~~~~~ ##################### 
  #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE PLOTS    #############~~~~~~~~~~~~~~~~~~~ ##################### 
  #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE PLOTS    #############~~~~~~~~~~~~~~~~~~~ ##################### 
  
  #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE PLOTS    #############~~~~~~~~~~~~~~~~~~~ ##################### 
  #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE PLOTS    #############~~~~~~~~~~~~~~~~~~~ ##################### 
  #############~~~~~~~~~~~~~~~~~~~ ##################### MAKE THE PLOTS    #############~~~~~~~~~~~~~~~~~~~ ##################### 
  
  # ############# ~~~~~~~~~~~~~~ ################## CLEAR ALL WITH RESET ALL    ############# ~~~~~~~~~~~~~~ ################## 

  observeEvent(input$reset_all, ignoreInit = TRUE, {
    
    clear_reactive_values_function <- function(reactive_value_list_to_clear){
      for (name in names(reactive_value_list_to_clear)){
        reactive_value_list_to_clear[[name]] <- NULL
      } 
    }
    clear_reactive_values_function(geoms_list_revision_posterior)
    clear_reactive_values_function(geoms_list_posterior)
    clear_reactive_values_function(rods_list)
    
    clear_reactive_values_function(geoms_list_revision_anterior)
    clear_reactive_values_function(geoms_list_anterior_diskectomy)
    clear_reactive_values_function(geoms_list_anterior_interbody)
    clear_reactive_values_function(geoms_list_anterior_instrumentation)
  })
  
  # ######### ~~~~~~~~~~~~~~  ############# MAKE THE GEOMS     ######### ~~~~~~~~~~~~~~  #############
  # ######### ~~~~~~~~~~~~~~  ############# MAKE THE GEOMS    ######### ~~~~~~~~~~~~~~  #############
  
  
  
  ######### ~~~~~~~~~~~~~~  ############# MAKE REACTIVE PLOT    ######### ~~~~~~~~~~~~~~  ############# 
  ######### ~~~~~~~~~~~~~~  ############# MAKE REACTIVE PLOT    ######### ~~~~~~~~~~~~~~  ############# 
  spine_plan_plot_anterior_reactive <- reactive({
    if(str_to_lower(input$spine_approach) == "anterior"){
      x_left_limit <- 0.3 - input$label_text_offset/100
      x_right_limit <- 1-x_left_limit
      plot_top_y <- input$crop_y[2]
      # y_spacing <- 0.025*input$crop_y[2]

      if(input$lumbar_vertebrae_count == "6"){
        l6_statement <- "Note: 6 Lumbar Vertebrae"
      }else{
        l6_statement <- " "
      }
      
      labels_anterior_cropped_df <- labels_anterior_df %>%
        filter(between(y, input$crop_y[1], plot_top_y)) %>% 
        mutate(x_left = x_left_limit + 0.05) %>%
        mutate(x_right = x_right_limit - 0.05) %>%
        select(level, x_left, x_right, y)
      
      labels_anterior_cropped_df <- labels_anterior_cropped_df %>%
        bind_rows(tibble(level = " ", 
                         x_left = min(labels_anterior_cropped_df$x_left) - 0.03,
                         x_right = max(labels_anterior_cropped_df$x_right) + 0.03, 
                         y = min(labels_anterior_cropped_df$y) - 0.075)) %>%
        bind_rows(tibble(level = " ", 
                         x_left = min(labels_anterior_cropped_df$x_left) - 0.03,
                         x_right = max(labels_anterior_cropped_df$x_right) + 0.03, 
                         y = max(labels_anterior_cropped_df$y) + 0.05))
      
      anterior_spine_ggdraw +
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
        geom_sf(data = NULL) + #this is needed so that plot starts cropped correctly 
        coord_sf(xlim = c(x_left_limit, x_right_limit),
                 ylim = input$crop_y, default = TRUE) 
      # plan_table_geom   
    }
    
  })
  
  spine_plan_plot_posterior_reactive <- reactive({
    x_left_limit <- 0.3 - input$label_text_offset/100
    x_right_limit <- 1-x_left_limit
    plot_top_y <- input$crop_y[2]
    y_spacing <- 0.025*input$crop_y[2]
    
    if(input$lumbar_vertebrae_count == "6"){
      l6_statement <- "Note: 6 Lumbar Vertebrae"
    }else{
      l6_statement <- " "
    }
    
    # ### POSTERIOR
    ### POSTERIOR
    labels_posterior_df <- labels_df %>%
      filter(between(y, input$crop_y[1], plot_top_y)) %>%
      mutate(x_left = x_left_limit + 0.05) %>%
      mutate(x_right = x_right_limit - 0.05) %>%
      select(-vertebral_number)
    
    labels_posterior_df <- labels_posterior_df %>%
      bind_rows(tibble(level = " ", 
                       x_left = min(labels_posterior_df$x_left) - 0.03,
                       x_right = max(labels_posterior_df$x_right) + 0.03, 
                       y = min(labels_posterior_df$y) - 0.03)) %>%
      bind_rows(tibble(level = " ", 
                       x_left = min(labels_posterior_df$x_left) - 0.03,
                       x_right = max(labels_posterior_df$x_right) + 0.03, 
                       y = max(labels_posterior_df$y) + 0.03))

    posterior_spine_ggdraw +
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
      annotate("text", x = 0.5, 
               y = input$crop_y[1] + 0.01, 
               label = l6_statement) +
      coord_sf(xlim = c(x_left_limit, x_right_limit),
               ylim = input$crop_y, default = TRUE) 
  })
  

  ##################### ~~~~~~~~~~~~~~~~ RENDER PLOTS ~~~~~~~~~~~~~~~~~~~ ##################
  ##################### ~~~~~~~~~~~~~~~~ RENDER PLOTS ~~~~~~~~~~~~~~~~~~~ ##################
  ##################### ~~~~~~~~~~~~~~~~ RENDER PLOTS ~~~~~~~~~~~~~~~~~~~ ##################
  
  # input$object_to_add
  # object_added_reactive_df()

  output$spine_plan <-  renderPlot({
    # main_page_reactive_plot()
    if(str_to_lower(input$spine_approach) == "anterior"){
      spine_plan_plot_anterior_reactive() +
        reactiveValuesToList(geoms_list_revision_anterior) +
        reactiveValuesToList(geoms_list_anterior_diskectomy) +
        reactiveValuesToList(geoms_list_anterior_interbody) +
        reactiveValuesToList(geoms_list_anterior_instrumentation)
    }else{
      spine_plan_plot_posterior_reactive() +
        reactiveValuesToList(geoms_list_revision_posterior) +
        reactiveValuesToList(geoms_list_posterior) +
        reactiveValuesToList(rods_list)
    }
  }) 
  
  # summary_spine_plot_reactive<- reactive({
  #   posterior_spine_ggdraw
  #   })%>%
  #     bindEvent(input$implants_complete)
  
  # summary_spine_plot_reactive<- reactive({
  #   approach_vector <- unique(all_objects_to_add_list$objects_df$approach)
  #   
  #   if(length(approach_vector) <2){
  #     if(input$approach_sequence == "posterior"){
  #       panel_2_plot <- spine_plan_plot_posterior_reactive() +
  #         reactiveValuesToList(geoms_list_revision_posterior) +
  #         reactiveValuesToList(geoms_list_posterior) +
  #         reactiveValuesToList(rods_list) 
  #     }else{
  #       panel_2_plot <-spine_plan_plot_anterior_reactive()
  #     }
  #   }else{
  #     if(approach_vector[[1]] == "posterior"){
  #        
  #       panel_2_plot <- plot_grid((spine_plan_plot_posterior_reactive() +
  #                    reactiveValuesToList(geoms_list_revision_posterior) +
  #                    reactiveValuesToList(geoms_list_posterior) +
  #                    reactiveValuesToList(rods_list)), 
  #                 NULL, 
  #                 spine_plan_plot_anterior_reactive(), 
  #                 nrow = 1, 
  #                 rel_widths = c(1, -.1, 1))
  #     }else{
  #       panel_2_plot <- plot_grid(spine_plan_plot_anterior_reactive(), 
  #                                 NULL,
  #                                 (spine_plan_plot_posterior_reactive() +
  #                                    reactiveValuesToList(geoms_list_revision_posterior) +
  #                                    reactiveValuesToList(geoms_list_posterior) +
  #                                    reactiveValuesToList(rods_list)),
  #                                 nrow = 1, 
  #                                 rel_widths = c(1, -.1, 1))
  #     }
  #   } 
  #   
  #   panel_2_plot
  # }) %>%
  #   bindEvent(input$implants_complete)
  
  
  output$spine_plot_for_implants_tab <- renderPlot({
    # summary_spine_plot_reactive()
      approach_vector <- unique(all_objects_to_add_list$objects_df$approach)

      if(length(approach_vector) <2){
        if(input$approach_sequence == "posterior"){
          panel_2_plot <- spine_plan_plot_posterior_reactive() +
            reactiveValuesToList(geoms_list_revision_posterior) +
            reactiveValuesToList(geoms_list_posterior) +
            reactiveValuesToList(rods_list)
        }else{
          panel_2_plot <-spine_plan_plot_anterior_reactive()
        }
      }else{
        if(approach_vector[[1]] == "posterior"){

          panel_2_plot <- plot_grid((spine_plan_plot_posterior_reactive() +
                       reactiveValuesToList(geoms_list_revision_posterior) +
                       reactiveValuesToList(geoms_list_posterior) +
                       reactiveValuesToList(rods_list)),
                    NULL,
                    spine_plan_plot_anterior_reactive(),
                    nrow = 1,
                    rel_widths = c(1, -.1, 1))
        }else{
          panel_2_plot <- plot_grid(spine_plan_plot_anterior_reactive(),
                                    NULL,
                                    (spine_plan_plot_posterior_reactive() +
                                       reactiveValuesToList(geoms_list_revision_posterior) +
                                       reactiveValuesToList(geoms_list_posterior) +
                                       reactiveValuesToList(rods_list)),
                                    nrow = 1,
                                    rel_widths = c(1, -.1, 1))
        }
      }

      panel_2_plot
  })%>%
    bindEvent(input$implants_complete)
  
  
  output$spine_plot_for_tables_tab <- renderPlot({
    summary_spine_plot_reactive()
  })%>%
    bindEvent(input$implants_complete)
  
  
  ########################################### Build REACTIVE DATAFRAMES FOR IMPLANTS CONNECTING TO THE RODS ###########################################

  observeEvent(input$implants_complete, ignoreInit = TRUE, {
    estimated_fusion_levels_posterior <- fusion_levels_df_function(all_objects_to_add_df = all_objects_to_add_list$objects_df %>% filter(approach == "posterior"))
    
    updatePrettyCheckboxGroup(session = session, 
                              inputId = "posterior_fusion_levels_confirmed", 
                              selected = estimated_fusion_levels_posterior$level 
    )
  })
  
  observeEvent(input$implants_complete, ignoreInit = TRUE, {
    estimated_fusion_levels_anterior <- fusion_levels_df_function(all_objects_to_add_df = all_objects_to_add_list$objects_df %>% filter(approach == "anterior"))
    
    updatePrettyCheckboxGroup(session = session, 
                              inputId = "anterior_fusion_levels_confirmed", 
                              selected = estimated_fusion_levels_anterior$level 
    )
  })
  
  ################------------------  Fusion Levels   ----------------------######################  
  
  observeEvent(all_objects_to_add_list$objects_df, ignoreInit = TRUE, ignoreNULL = TRUE, {
    if(input$fusion_procedure_performed == FALSE && (nrow(all_objects_to_add_list$objects_df %>% filter(str_detect(object, "screw|hook|plate"))) > 0 | any(all_objects_to_add_list$objects_df$fusion == "yes"))){
      updateSwitchInput(session = session, 
                        inputId = "fusion_procedure_performed", 
                        value = TRUE)
    }
    
    if(any((all_objects_to_add_list$objects_df %>% filter(approach == "anterior"))$fusion == "yes")  && input$anterior_fusion_performed == FALSE){
      updateSwitchInput(session = session,
                        inputId = "anterior_fusion_performed",
                        value = TRUE)
    }
    
    if(any((all_objects_to_add_list$objects_df %>% filter(approach == "posterior"))$fusion == "yes") && input$posterior_fusion_performed == FALSE){
      updateSwitchInput(session = session, 
                        inputId = "posterior_fusion_performed", 
                        value = TRUE)
    }
  })
  
  #####################   #####################   #########   RUN CONFIRM FUSION LEVELS and TECHNIQUE DETAILS MODAL FUNCTION ##################### #####################
  #####################   #####################   #########   RUN CONFIRM FUSION LEVELS and TECHNIQUE DETAILS MODAL FUNCTION ##################### #####################
  #####################   #####################   #########   RUN CONFIRM FUSION LEVELS and TECHNIQUE DETAILS MODAL FUNCTION ##################### #####################
  #####################   #####################   #########   RUN CONFIRM FUSION LEVELS and TECHNIQUE DETAILS MODAL FUNCTION ##################### #####################
  
  observeEvent(input$implants_complete, ignoreNULL = TRUE, ignoreInit = TRUE, once = TRUE, {
    if(any(str_detect(all_objects_to_add_list$objects_df$object, "screw|anterior_plate"))){
      implants_placed_yes_no <- "yes"  
    }else{
      implants_placed_yes_no <- "no" 
    }
    
    if(any(all_objects_to_add_list$objects_df$approach == "anterior")){
      anterior_approach_yes_no <- "yes"  
    }else{
      anterior_approach_yes_no <- "no" 
    }
    
    if(any(all_objects_to_add_list$objects_df$approach == "posterior")){
      posterior_approach_yes_no <- "yes"  
    }else{
      posterior_approach_yes_no <- "no" 
    }
    
    if(any(input$diagnosis_category == "deformity")){
      deformity_correction_techniques_vector <- c(
        "The rods were secured into place with set screws. ",
        "The Pro-axis bed was bent to achieve the desired sagittal plane alignment and the rods were then secured into place with set screws. ",
        "The working rod was secured into place on the concavity and rotated to corrected the coronal plane. ",
        "The concave rod was secured proximally and distally with set screws and reduction clips were used to sequentially reduce the curve. ",
        "In situ rod benders were then used to correct the coronal and sagittal plane. ",
        "The set screws at the neutral vertebrae were tightened, and the adjacenet vertebrae were sequentially derotated. ",
        "A series of compression along the convexity and distraction along the concavity was performed to further correct the coronal plane and balance the screws. ",
        "Other")
    }else{
      deformity_correction_techniques_vector <- c(
        "The rods were secured into place with set screws. ",
        "In situ rod benders were then used to correct the coronal and sagittal plane. ",
        "Other")
    }
    
    # print(glue("Correct Observe.\n implants_placed_yes_no = {implants_placed_yes_no} \n deformity_correction_techniques_vector = {deformity_correction_techniques_vector}\n procedure_approach_reactive = {input$approach_sequence}\nanterior_approach_yes_no = {anterior_approach_yes_no}"))
    
    
    showModal(
      confirm_fusion_levels_and_technique_details_modal_box_function(implants_placed = implants_placed_yes_no,
                                                                     deformity_correction_choices = deformity_correction_techniques_vector,
                                                                     procedure_approach = input$approach_sequence,
                                                                     # procedure_approach = procedure_approach_reactive(),
                                                                     # fusion_levels_confirmed = fusion_levels_computed_reactive_input,
                                                                     anterior_approach = anterior_approach_yes_no,
                                                                     posterior_approach = posterior_approach_yes_no
      )
    )
  })
  

  
  
  observeEvent(input$implants_complete, ignoreInit = TRUE, {
    if(input$implants_complete > 1){
      # print("wrong observe event observed")
      if(any(str_detect(all_objects_to_add_list$objects_df$object, "screw|anterior_plate"))){
        implants_placed_yes_no <- "yes"  
      }else{
        implants_placed_yes_no <- "no" 
      }
    
    if(any(all_objects_to_add_list$objects_df$approach == "anterior")){
      anterior_approach_yes_no <- "yes"  
    }else{
      anterior_approach_yes_no <- "no" 
    }
    
    if(any(all_objects_to_add_list$objects_df$approach == "posterior")){
      posterior_approach_yes_no <- "yes"  
    }else{
      posterior_approach_yes_no <- "no" 
    }
    if(any(input$diagnosis_category == "deformity")){
      deformity_correction_techniques_vector <- c(
        "The Pro-axis bed was bent to achieve the desired sagittal plane alignment and the rods were then secured into place. ",
        "The rods were secured into place with set screws. ",
        "The working rod was secured into place on the concavity and rotated to corrected the coronal plane. ",
        "The concave rod was secured proximally and distally with set screws and reduction clips were used to sequentially reduce the curve. ",
        "In situ rod benders were then used to correct the coronal and sagittal plane. ",
        "The set screws at the neutral vertebrae were tightened, and the adjacenet vertebrae were sequentially derotated. ",
        "A series of compression along the convexity and distraction along the concavity was performed to further correct the coronal plane and balance the screws. ",
        "Other",
        "NA")
    }else{
      deformity_correction_techniques_vector <- c(
        "The rods were secured into place with set screws. ",
        "In situ rod benders were then used to correct the coronal and sagittal plane. ",
        "Other",
        "NA")
    }
    
      showModal(
        confirm_fusion_levels_and_technique_details_modal_box_function(implants_placed = implants_placed_yes_no, 
                                                                       deformity_correction_choices = deformity_correction_techniques_vector,
                                                                       # procedure_approach = procedure_approach_reactive(),
                                                                       procedure_approach = input$approach_sequence,
                                                                       # screws_selected_df_reactive = screws_selected_df_reactive(), 
                                                                       # fusion_levels_confirmed = input$fusion_levels_confirmed,
                                                                       posterior_fusion_levels_confirmed = input$posterior_fusion_levels_confirmed,
                                                                       anterior_fusion_levels_confirmed = input$anterior_fusion_levels_confirmed,
                                                                       approach_specified_posterior = input$approach_specified_posterior,
                                                                       approach_open_mis = input$approach_open_mis,
                                                                       approach_robot_navigation = input$approach_robot_navigation, 
                                                                       approach_specified_anterior = input$approach_specified_anterior,
                                                                       implant_start_point_method = input$implant_start_point_method,
                                                                       implant_position_confirmation_method = input$implant_position_confirmation_method, 
                                                                       alignment_correction_method = input$alignment_correction_method,
                                                                       alignment_correction_method_other = input$alignment_correction_method_other,
                                                                       instruments_used_for_bony_work = input$instruments_used_for_bony_work,
                                                                       anterior_approach = anterior_approach_yes_no,
                                                                       posterior_approach = posterior_approach_yes_no,
                                                                       anterior_cervical_approach_details_checkbox = input$anterior_cervical_approach_details_checkbox, 
                                                                       posterior_additional_approach_details_checkbox = input$posterior_additional_approach_details_checkbox
        )
        
      )
    }
  }
  )
  observeEvent(input$fusion_levels_technique_details_modal_complete_button, ignoreNULL = TRUE, ignoreInit = TRUE, {
    removeModal()
  }
  )
  
  observeEvent(input$fusion_levels_technique_details_modal_complete_button, ignoreNULL = TRUE, ignoreInit = TRUE, {
    lateral_mass_screws_after_decompression_modal_function(implant_objects_df = all_objects_to_add_list$objects_df)
  }
  )
  
  observeEvent(input$approach_robot_navigation, ignoreInit = TRUE, {
    if(input$approach_robot_navigation == "Fluoroscopy-guided"){
      updateAwesomeRadio(session = session, 
                         inputId = "implant_start_point_method", 
                         selected = "Intraoperative fluoroscopy was used to identify and confirm implant start points." 
      )  
    }
    if(input$approach_robot_navigation == "Navigated" | input$approach_robot_navigation == "Robotic" ){
      updateAwesomeRadio(session = session, 
                         inputId = "implant_start_point_method", 
                         selected = "Intraoperative navigation was used for identifying start points."
      )  
    }
  })
  
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   COMPLETED CONFIRM FUSION LEVELS and TECHNIQUE DETAILS MODAL FUNCTION  #########    !!!!!!!!!!!!!
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   COMPLETED CONFIRM FUSION LEVELS and TECHNIQUE DETAILS MODAL FUNCTION  #########    !!!!!!!!!!!!!
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   COMPLETED CONFIRM FUSION LEVELS and TECHNIQUE DETAILS MODAL FUNCTION  #########    !!!!!!!!!!!!!
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   COMPLETED CONFIRM FUSION LEVELS and TECHNIQUE DETAILS MODAL FUNCTION  #########    !!!!!!!!!!!!!
  
  ##update postop plans based on procedure
  
  age_reactive <- reactive({
    if(length(input$date_of_birth) > 0 & length(input$date_of_surgery)> 0){
      age <- round(interval(start = input$date_of_birth, end = input$date_of_surgery)/years(1))
    }else{
      age <- 999
    }
  })
  
  # observeEvent(list(input$date_of_birth, input$date_of_surgery), ignoreInit = TRUE, ignoreNULL = TRUE, {

  observeEvent(age_reactive(), ignoreInit = TRUE, ignoreNULL = TRUE, {
    # if(length(input$date_of_birth) > 0 & length(input$date_of_surgery)> 0){
    #   age <- round(interval(start = input$date_of_birth, end = input$date_of_surgery)/years(1))
    # }else{
    #   age <- 999
    # }
    
    age <- age_reactive()
    
    if(is.double(age) && age < 15){
      postop_pain_choices_vector <<- c("Ice pack to affected area, PRN",
                                       "Dilaudid PCA [0.1mg bolus, 10min lockout, no basal] - DISCONTINUE on POD 1",
                                       "Oxycodone 0.1mg/kg q4h prn, ok to give additional dose if needed",
                                       "Oxycodone 0.15mg/kg q4h prn, ok to give additional dose if needed",
                                       "Oxycodone *** q4h, ok to give additional dose if needed",
                                       "Tylenol 15mg/kg q6h scheduled.",
                                       "Tylenol ***",
                                       "Gabapentin 100mg q8h scheduled",
                                       "Gabapentin 300mg q8h scheduled",
                                       "Toradol 0.5mg/kg q8h for 48-72hrs",
                                       "Toradol ***",
                                       "Diazepam 0.1mg/kg q6h prn for muscle spasms",
                                       "Diazepam 2.5mg q8h prn for muscle spasms",
                                       "Baclofen 10mg TID prn for muscle spasms",
                                       "Pain team consult",
                                       "Per Primary",
                                       "Continue home pain medications ***",
                                       "***"
      )
    }
  })
  
  observeEvent(age_reactive(), ignoreInit = TRUE, ignoreNULL = TRUE, {

    age <- age_reactive()
    
    if(is.double(age) && age < 15){
      additional_procedure_options_vector[additional_procedure_options_vector == "Application of Halo"] <<- "Application of Halo in a pediatric patient (with >= 6 pins)"
      }
  })
  
  
  
  
  
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
  
  ################################### ANTERIOR ADDITIONAL PROCEDURES PERFORMED #################### 
  ################################### ANTERIOR ADDITIONAL PROCEDURES PERFORMED #################### 
  ################################### ANTERIOR ADDITIONAL PROCEDURES PERFORMED #################### 
  
  additional_procedures_performed_anterior_reactive <- reactive({
    additional_procedures_list <- list()
    if(input$implants_complete > 0){
      
    # if(procedure_approach_reactive() == "anterior" | procedure_approach_reactive() == "combined"){
      if(!is.null(input$approach_sequence) && str_detect(input$approach_sequence, "anterior")){
      additional_procedures_list <- as.list(input$additional_procedures_anterior)
      
      if("Robotic" %in% input$approach_robot_navigation){
        additional_procedures_list$robot <- "Robotic Assisted Spine Surgery"
      }
      if("Navigated" %in% input$approach_robot_navigation){
        additional_procedures_list$navigation <- "Use of stereotactic navigation system for screw placement"
      }
      
      if("Microscopic" %in% input$approach_robot_navigation){
        additional_procedures_list$microscope <- "Intraoperative use of microscope for microdissection"
      }
      
      if(length(input$prior_anterior_plate_removed_levels)>0){
        additional_procedures_list$removal_instrumentation <- "Removal of anterior spinal instrumentation"
      }
      
      if(any(str_detect(string = input$head_positioning_anterior, pattern = "Tongs"))){
        additional_procedures_list$head_positioning_anterior <- "Application of Cranial Tongs"
      }
      if(any(str_detect(string = input$head_positioning_anterior, pattern = "Mayfield"))){
        additional_procedures_list$head_positioning_anterior <- "Application of Cranial Tongs using Mayfield head holder"
      }
      
      # if(any(str_detect(string = input$head_positioning_anterior, pattern = "Halo"))){
      #   age <- as.double(if_else(paste(input$date_of_birth) == "1900-01-01", "0", as.character(round(interval(start = paste(input$date_of_birth), end = paste(input$date_of_surgery))/years(1), 0))))
      #   if(age < 18 & age > 0){
      #     additional_procedures_list$head_positioning_anterior <- "Application of Halo for thin skull osteology (pediatric)"
      #   }else{
      #     additional_procedures_list$head_positioning_anterior <- "Application of Halo"
      #   }
      # }
      
      if(jh_determine_if_section_dx_function(diagnosis_vector = input$primary_diagnosis, section_to_determine = "trauma")){
        additional_procedures_list$fracture <- "Open treatment of vertebral fracture"
      }
      # if(jh_determine_if_section_dx_function(diagnosis_vector = input$primary_diagnosis, section_to_determine = "infection")){
      #   additional_procedures_list$incision_drainage <- "Incision and Drainage"
      # }
      if(jh_determine_if_section_dx_function(diagnosis_vector = input$primary_diagnosis, section_to_determine = "tumor")){
        additional_procedures_list$tumor_biopsy <- "Open Biopsy of extradural spinal lesion"
      }
      
      if(any(input$dressing_details_anterior == "Wound Vac")){
        additional_procedures_list$wound_vac_anterior <- "Application of Wound Vac (negative pressure wound therapy; CPT = 97605)"    
      }
    }
    }
    unname(unlist(additional_procedures_list))
  })
  
  observeEvent(additional_procedures_performed_anterior_reactive(), ignoreInit = TRUE, {
    if(all(additional_procedures_performed_anterior_reactive() %in% additional_procedure_options_vector) == FALSE){
      additional_procedure_options_vector <<- union(additional_procedure_options_vector, additional_procedures_performed_anterior_reactive())
    }
    # updateAwesomeCheckboxGroup(session = session,
    #                            inputId = "additional_procedures_anterior",
    #                            choices = unique(append(additional_procedure_options_vector, additional_procedures_performed_anterior_reactive())),
    #                            selected = additional_procedures_performed_anterior_reactive())
  })
  
  additional_anterior_procedures_vector_for_op_note_reactive <- reactive({
    if(!is.null(input$approach_sequence) && str_detect(input$approach_sequence, "anterior")){
      
      additional_procedures_list <- as.list(input$additional_procedures_anterior)
      
      if("Other" %in% input$additional_procedures_anterior){
        additional_procedures_list$other_anterior <- input$additional_procedures_other_anterior
      }
      
      additional_procedures_list <- discard(additional_procedures_list, .p = ~ (.x == "Other"))
      
      unlist(additional_procedures_list, use.names = FALSE)
    }
  })

  ###################### ANTERIOR ADDITIONAL PROCEDURES PERFORMED #################### END
  ###################### ANTERIOR ADDITIONAL PROCEDURES PERFORMED #################### END
  ###################### ANTERIOR ADDITIONAL PROCEDURES PERFORMED #################### END
  
  ################################### POSTERIOR ADDITIONAL PROCEDURES PERFORMED ####################
  ################################### POSTERIOR ADDITIONAL PROCEDURES PERFORMED ####################
  ################################### POSTERIOR ADDITIONAL PROCEDURES PERFORMED ####################
  additional_procedures_performed_posterior_reactive <- reactive({
    additional_procedures_list <- list()
    
    # if(procedure_approach_reactive() == "posterior" | procedure_approach_reactive() == "combined"){
    if(!is.null(input$approach_sequence) && str_detect(input$approach_sequence, "posterior")){
      additional_procedures_list <- as.list(input$additional_procedures_posterior)
      
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
      
      if(any(str_detect(string = input$head_positioning_posterior, pattern = "Tongs"))){
        additional_procedures_list$head_positioning_posterior <- "Application of Cranial Tongs"
      }
      if(any(str_detect(string = input$head_positioning_posterior, pattern = "Mayfield"))){
        additional_procedures_list$head_positioning_posterior <- "Application of Cranial Tongs using Mayfield head holder"
      }
      
      # if(any(str_detect(string = input$head_positioning_posterior, pattern = "Halo"))){
      #   age <- as.double(if_else(paste(input$date_of_birth) == "1900-01-01", "0", as.character(round(interval(start = paste(input$date_of_birth), end = paste(input$date_of_surgery))/years(1), 0))))
      #   if(age < 18 & age > 0){
      #     additional_procedures_list$head_positioning_posterior <- "Application of Halo for thin skull osteology (pediatric)"
      #   }else{
      #     additional_procedures_list$head_positioning_posterior <- "Application of Halo"
      #   }
      # }
      
      if(length(input$durotomy_repair_method)>0){
        if(str_detect(string = toString(input$durotomy_repair_method), pattern = "No Repair") == FALSE){
          additional_procedures_list$dural_repair <- "Repair of dural/CSF leak"
        }
      }
      
      if(jh_determine_if_section_dx_function(diagnosis_vector = input$primary_diagnosis, section_to_determine = "trauma")){
        additional_procedures_list$fracture <- "Open treatment of vertebral fracture"
      }
      # if(jh_determine_if_section_dx_function(diagnosis_vector = input$primary_diagnosis, section_to_determine = "infection")){
      #   additional_procedures_list$incision_drainage <- "Incision and Drainage"
      # }
      
      if(jh_determine_if_section_dx_function(diagnosis_vector = input$primary_diagnosis, section_to_determine = "tumor")){
        additional_procedures_list$tumor_biopsy <- "Open Biopsy of extradural spinal lesion"
      }
      
      if(any(input$dressing_details_posterior == "Wound Vac")){
        additional_procedures_list$wound_vac_anterior <- "Application of Wound Vac (negative pressure wound therapy; CPT = 97605)"    
      }
      
      if(c2_nerve_transection_list_reactive()$additional_procedure_statement != "na"){
        additional_procedures_list$c2_nerve_transection <- paste(c2_nerve_transection_list_reactive()$additional_procedure_statement)
      }

    }
    
    unname(unlist(additional_procedures_list))
  })
  

  observeEvent(additional_procedures_performed_posterior_reactive(), ignoreInit = TRUE, {
    if(all(additional_procedures_performed_posterior_reactive() %in% additional_procedure_options_vector) == FALSE){
      additional_procedure_options_vector <<- union(additional_procedure_options_vector, additional_procedures_performed_posterior_reactive())
    }

  })
  
  
  additional_posterior_procedures_vector_for_op_note_reactive <- reactive({
    if(!is.null(input$approach_sequence) && str_detect(input$approach_sequence, "posterior")){

      additional_procedures_list <- as.list(input$additional_procedures_posterior)
      
      if("Other" %in% input$additional_procedures_posterior){
        additional_procedures_list$other_posterior <- input$additional_procedures_other_posterior
      }
      additional_procedures_list <- discard(additional_procedures_list, .p = ~ (.x == "Other"))
      unlist(additional_procedures_list, use.names = FALSE)
    }
  })
  
  
  ################################### POSTERIOR ADDITIONAL PROCEDURES PERFORMED #################### END
  ################################### POSTERIOR ADDITIONAL PROCEDURES PERFORMED #################### END
  ################################### POSTERIOR ADDITIONAL PROCEDURES PERFORMED #################### END
  
  
  ######################################## RUN ADDITIONAL SURGICAL DETAILS MODAL AFTER ADVANCING TO NEXT TAB ###############################
  ######################################## RUN ADDITIONAL SURGICAL DETAILS MODAL AFTER ADVANCING TO NEXT TAB ###############################
  ######################################## RUN ADDITIONAL SURGICAL DETAILS MODAL AFTER ADVANCING TO NEXT TAB ###############################
  ######################################## RUN ADDITIONAL SURGICAL DETAILS MODAL AFTER ADVANCING TO NEXT TAB ###############################
  
  observeEvent(input$primary_diagnosis, ignoreInit = TRUE, ignoreNULL = TRUE, {
    if(length(input$primary_diagnosis) >0 && input$implants_complete > 0){
      preop_dx <- glue_collapse(x = jh_add_codes_to_diagnosis_function(input$primary_diagnosis), sep = "; ")
      postop_dx <- glue_collapse(x = jh_add_codes_to_diagnosis_function(input$primary_diagnosis), sep = "; ")
      
      updateTextInput(session = session, 
                      inputId = "preoperative_diagnosis", 
                      value = paste(preop_dx))
      
      updateTextInput(session = session, 
                      inputId = "postoperative_diagnosis", 
                      value = paste(postop_dx))
    }else{
      preop_dx <- " "
      postop_dx <- " "
    }
  })
  
  observeEvent(input$implant_details_complete, ignoreInit = TRUE, once = TRUE, {
    indications_list <- list()
    
    if(!is.null(input$primary_diagnosis) && length(input$primary_diagnosis) >0){
      preop_dx <- glue_collapse(x = jh_add_codes_to_diagnosis_function(input$primary_diagnosis), sep = "; ")
      postop_dx <- glue_collapse(x = jh_add_codes_to_diagnosis_function(input$primary_diagnosis), sep = "; ")
    }else{
      preop_dx <- " "
      postop_dx <- " "
    }
    
    if(length(input$symptoms) >0){
      symptoms <- str_to_lower(glue_collapse(x = map(.x = input$symptoms, 
                                                     .f = ~ if_else(.x == "Other", input$symptoms_other, .x)),
                                             sep = ", ", last = " and "))
    }else{
      symptoms <- "***"
    }
    
    he_she <- if_else(str_to_lower(input$sex) == "female", "She", "He")
    
    age <- trunc((input$date_of_birth %--% input$date_of_surgery) / years(1))
    
    if(existing_patient_data$match_found == TRUE){
      if(str_to_lower(input$primary_revision) == "revision"){
        
        most_recent_surgery_df <- existing_patient_data$prior_surgical_summary %>%
          filter(main_approach == input$revision_approach) %>%
          filter(date_of_surgery == max(date_of_surgery))
        
        most_recent_surgery <- format(most_recent_surgery_df$date_of_surgery, "%m/%d/%y")
        
        prior_surgery_for_revision_statement <- glue(" that underwent surgery most recently on {most_recent_surgery}, from {most_recent_surgery_df$upper_treated_vertebrae} to {most_recent_surgery_df$lower_treated_vertebrae}")
        
      }else{
        prior_surgery_for_revision_statement <- ""
      }
    }else{
      prior_surgery_for_revision_statement <- ""
    }
    
    indications_list$opening <- glue("This is a {age} year-old {str_to_lower(input$sex)}{prior_surgery_for_revision_statement} that presented with {symptoms} and imaging findings consistent with these symptoms.")
    
    if(length(input$diagnosis_category)>0){
      if(any(input$diagnosis_category == "msk") | any(input$diagnosis_category == "deformity")){
        indications_list$conservative_attempted <- glue("{he_she} has exhausted conservative measures.")
      } 
    }
    
    title_name <- if_else(input$sex == "Male", glue("Mr. {input$patient_last_name}"), glue("Ms. {input$patient_last_name}"))
    
    indications_list$risks <- glue("Risks and benefits of operative intervention were considered and discussed in detail and {title_name} has elected to proceed with surgery. I believe adequate informed consent was obtained.")
    
    procedure_indications <- glue_collapse(indications_list, sep = " ")
    
    showModal(
      addition_surgical_details_modal_box_function(
          preoperative_diagnosis = preop_dx, 
          postoperative_diagnosis = postop_dx,
        indications = procedure_indications,
        neuromonitoring = c("SSEP", "tc MEP"), 
        preop_antibiotics = c("Cefazolin (Ancef)"))
    )
  })
  
  
  
  ## NOW make a reactive modal for editing the info if needed ###
  modal_box_surgical_details_reactive <- reactive({
    addition_surgical_details_modal_box_function(editing_the_details = TRUE, ## this shows a different button at the footer
                                                 primary_surgeon_first_name_input = input$primary_surgeon_first_name,
                                                 primary_surgeon_last_name_input = input$primary_surgeon_last_name,
                                                 cosurgeon_yes_no = input$cosurgeon_yes_no, 
                                                 cosurgeon = input$cosurgeon,
                                                 attending_assistant_yes_no = input$attending_assistant_yes_no, 
                                                 attending_assistant = input$attending_assistant,
                                                 surgical_assistants = input$surgical_assistants,
                                                 preoperative_diagnosis = input$preoperative_diagnosis,
                                                 postoperative_diagnosis = input$postoperative_diagnosis, 
                                                 asa_class = input$asa_class,
                                                 anesthesia = input$anesthesia,
                                                 local_anesthesia = input$local_anesthesia,
                                                 indications = input$indications,
                                                 neuromonitoring = input$neuromonitoring,
                                                 triggered_emg = input$triggered_emg,
                                                 pre_positioning_motors = input$pre_positioning_motors,
                                                 neuromonitoring_signal_stability = input$neuromonitoring_signal_stability,
                                                 preop_antibiotics = input$preop_antibiotics,
                                                 preop_antibiotics_other =  input$preop_antibiotics_other, 
                                                 anti_fibrinolytic = input$anti_fibrinolytic,
                                                 txa_loading = input$txa_loading,
                                                 txa_maintenance = input$txa_maintenance)
  })
  
  ## NOW OBSERVE THE COMPLETION OF MODAL BOX 1 AND THEN SHOW MODAL BOX 2
  
  observeEvent(input$additional_surgical_details_1_complete, {
    # add_procedures_list <- list()
    
    showModal(
      addition_surgical_details_modal_box_2_function(required_options_missing = FALSE, 
                                                     # procedure_approach = procedure_approach_reactive(),
                                                     procedure_approach = input$approach_sequence,
                                                     additional_procedures_choices_anterior = additional_procedure_options_vector, 
                                                     additional_procedures_choices_posterior = additional_procedure_options_vector, 
                                                     additional_procedures_anterior = additional_procedures_performed_anterior_reactive(), 
                                                     additional_procedures_posterior = additional_procedures_performed_posterior_reactive()
      )
    )
  })
  
  observeEvent(input$additional_surgical_details_complete, ignoreInit = TRUE, {
    removeModal()
  })
  
  # ### NOW SHOW MODAL 2 IF THERE ARE INCOMPLETE VALUES ###
  # observeEvent(input$additional_surgical_details_complete, ignoreInit = TRUE,
  #              {
  #                if(procedure_approach_reactive() == "anterior"){
  #                  if(length(head_positioning_anterior) == 0 | length(input$closure_details_anterior) == 0 | length(input$dressing_details_anterior) == 0 | length(input$intraoperative_complications_yes_no) == 0){
  #                    show_again <- TRUE
  #                  }else{
  #                    show_again <- FALSE
  #                  }
  #                }
  # 
  #                if(procedure_approach_reactive() == "posterior"){
  #                  if(length(head_positioning_posterior) == 0 | length(input$closure_details_posterior) == 0 | length(input$dressing_details_posterior) == 0 | length(input$intraoperative_complications_yes_no) == 0){
  #                    show_again <- TRUE
  #                  }else{
  #                    show_again <- FALSE
  #                  }
  #                }
  # 
  #                if(procedure_approach_reactive() == "combined"){
  #                  if(length(head_positioning_anterior) == 0 | length(input$closure_details_anterior) == 0 | length(input$dressing_details_anterior) == 0 | length(input$intraoperative_complications_yes_no) == 0 | length(head_positioning_posterior) == 0 | length(input$closure_details_posterior) == 0 | length(input$dressing_details_posterior) == 0 | length(input$intraoperative_complications_yes_no) == 0){
  #                    show_again <- TRUE
  #                  }else{
  #                    show_again <- FALSE
  #                  }
  #                }
  # 
  #                if(show_again == TRUE){
  #                  showModal(
  #                    addition_surgical_details_modal_box_2_function(required_options_missing = TRUE,
  #                                                                   procedure_approach = procedure_approach_reactive(),
  #                                                                   head_positioning_posterior = input$head_positioning_posterior,
  #                                                                   head_positioning_anterior = input$head_positioning_anterior,
  #                                                                   surgical_findings = input$surgical_findings,
  #                                                                   specimens_removed = input$specimens_removed,
  #                                                                   ebl = input$ebl,
  #                                                                   urine_output = input$urine_output,
  #                                                                   crystalloids_administered = input$crystalloids_administered,
  #                                                                   colloids_administered = input$colloids_administered,
  #                                                                   transfusion = input$transfusion,
  #                                                                   cell_saver_transfused = input$cell_saver_transfused,
  #                                                                   prbc_transfused = input$prbc_transfused,
  #                                                                   ffp_transfused = input$ffp_transfused,
  #                                                                   cryoprecipitate_transfused = input$cryoprecipitate_transfused,
  #                                                                   platelets_transfused = input$platelets_transfused,
  #                                                                   intraoperative_complications_yes_no = input$intraoperative_complications_yes_no,
  #                                                                   intraoperative_complications_vector = input$intraoperative_complications_vector,
  #                                                                   other_intraoperative_complications = input$other_intraoperative_complications,
  #                                                                   durotomy_timing_input = input$durotomy_timing,
  #                                                                   durotomy_instrument_input = input$durotomy_instrument,
  #                                                                   durotomy_repair_method_input = input$durotomy_repair_method,
  # 
  #                                                                   additional_procedures_choices_anterior = additional_procedures_options_reactive_vector(),
  #                                                                   additional_procedures_anterior = input$additional_procedures_anterior,
  #                                                                   additional_procedures_other_anterior = input$additional_procedures_other_anterior,
  #                                                                   additional_end_procedure_details_anterior = input$additional_end_procedure_details_anterior,
  #                                                                   closure_details_anterior = input$closure_details_anterior,
  #                                                                   dressing_details_anterior = input$dressing_details_anterior,
  # 
  #                                                                   additional_procedures_choices_posterior = additional_procedures_options_reactive_vector(),
  #                                                                   additional_procedures_posterior = input$additional_procedures_posterior,
  #                                                                   additional_procedures_other_posterior = input$additional_procedures_other_posterior,
  #                                                                   additional_end_procedure_details_posterior = input$additional_end_procedure_details_posterior,
  #                                                                   closure_details_posterior = input$closure_details_posterior,
  #                                                                   dressing_details_posterior = input$dressing_details_posterior,
  # 
  #                                                                   postop_dispo = input$postop_dispo,
  #                                                                   postop_abx = input$postop_abx,
  #                                                                   postop_map_goals = input$postop_map_goals,
  #                                                                   postop_transfusion_threshold = input$postop_transfusion_threshold,
  #                                                                   postop_imaging = input$postop_imaging,
  #                                                                   postop_pain = input$postop_pain,
  #                                                                   postop_activity = input$postop_activity,
  #                                                                   postop_brace = input$postop_brace,
  #                                                                   postop_diet = input$postop_diet,
  #                                                                   postop_dvt_ppx = input$postop_dvt_ppx,
  #                                                                   postop_drains_dressing = input$postop_drains_dressing,
  #                                                                   postop_followup = input$postop_followup
  #                    )
  #                  )
  #                }
  #              })
  
  
  ## NOW make a reactive modal for editing the info if needed ###
  modal_box_surgical_details_2_reactive <- reactive({
    
    addition_surgical_details_modal_box_2_function(
      # procedure_approach = procedure_approach_reactive(),
      procedure_approach = input$approach_sequence,
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
      head_positioning_posterior = input$head_positioning_posterior,
      head_positioning_anterior = input$head_positioning_anterior,
      
      # additional_procedures_choices_anterior = additional_procedures_options_reactive_vector(),
      additional_procedures_anterior = input$additional_procedures_anterior,
      additional_procedures_other_anterior = input$additional_procedures_other_anterior,
      additional_end_procedure_details_anterior = input$additional_end_procedure_details_anterior,
      closure_details_anterior = input$closure_details_anterior,
      dressing_details_anterior = input$dressing_details_anterior,
      
      # additional_procedures_choices_posterior = additional_procedures_options_reactive_vector(),
      additional_procedures_posterior = input$additional_procedures_posterior,
      additional_procedures_other_posterior = input$additional_procedures_other_posterior,
      additional_end_procedure_details_posterior = input$additional_end_procedure_details_posterior,
      closure_details_posterior = input$closure_details_posterior,
      dressing_details_posterior = input$dressing_details_posterior,
      
      postop_dispo = input$postop_dispo,
      postop_abx = input$postop_abx,
      postop_map_goals = input$postop_map_goals,
      postop_transfusion_threshold = input$postop_transfusion_threshold,
      postop_imaging = input$postop_imaging,
      postop_pain = input$postop_pain,
      postop_activity = input$postop_activity,
      postop_brace = input$postop_brace,
      postop_diet = input$postop_diet,
      postop_foley = input$postop_foley,
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
    
    details_list$'Primary Surgeon:' <- paste(input$primary_surgeon_first_name, input$primary_surgeon_last_name)
    details_list$'Assistants:' <- input$surgical_assistants
    
    if(input$cosurgeon_yes_no == TRUE){
      details_list$'Co-Surgeon:' <- input$cosurgeon 
    }
    
    details_list$'Preoperative Diagnosis:' <- input$preoperative_diagnosis
    details_list$'Postoperative Diagnosis:' <- input$postoperative_diagnosis
    details_list$'Surgical Indications:' <- input$indications
    details_list$'--' <- "--"
    details_list$'ASA Class:' <- input$asa_class
    details_list$'Anesthesia Type:' <- input$anesethesia
    details_list$'Neuromonitoring used:' <- toString(input$neuromonitoring)
    details_list$'Preoperative Antibiotics:' <- toString(input$preop_antibiotics)
    details_list$'Antifibrinolytic:' <- if_else(length(input$anti_fibrinolytic)>0, toString(input$anti_fibrinolytic), "none")
    details_list$'Findings:' <- input$surgical_findings
    details_list$'Specimens:' <- input$specimens_removed
    details_list$'--' <- "--"
    details_list$'Estimated Blood Loss:' <- paste0(input$ebl, "cc")     
    details_list$'Urine Output:' <- paste0(input$urine_output, "cc")
    details_list$'Crystalloids:' <- paste0(input$crystalloids_administered, "cc")
    details_list$'Colloids:' <- paste0(input$colloids_administered, "cc")
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
    if(!is.null(input$intraoperative_complications_yes_no) && input$intraoperative_complications_yes_no == "Yes" && length(input$intraoperative_complications_vector) > 0){
      details_list$'Intraoperative Complications:' <- as.character(glue_collapse(x = input$intraoperative_complications_vector, sep = "; "))
    }
    
    details_list$'Posterior Head Position:' <- paste(input$head_positioning_posterior)
    details_list$'Anterior Head Position:' <- paste(input$head_positioning_anterior)
    
    # details_list$'Additional Anterior Procedures:' <- as.character(glue_collapse(x = additional_procedures_vector_anterior_reactive(), sep = "; "))
    # details_list$'Additional Posterior Procedures:' <- as.character(glue_collapse(x = additional_posterior_procedures_vector_for_op_note_reactive(), sep = "; "))
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
    
    details_list$'Used During Anterior Closure:' <- toString(input$additional_end_procedure_details_anterior)
    details_list$'Anterior Skin Closure:' <- toString(input$closure_details_anterior)
    details_list$'Anterior Skin/Dressing:' <- toString(input$dressing_details_anterior)
    
    details_list$'Used During Posterior Closure:' <- toString(input$additional_end_procedure_details_posterior)
    details_list$'Posterior Skin Closure:' <- toString(input$closure_details_posterior)
    details_list$'Posterior Skin/Dressing:' <- toString(input$dressing_details_posterior)
    
    details_list <- map(details_list, .f = ~ as.character(.x)) 
    
    enframe(details_list, name = "Variable", value = "Input") %>%
      unnest("Input") %>%
      mutate(Input = if_else(Input == "NA", "No Value Entered", Input))
  })
  
  output$additional_surgical_details_table <- renderTable({
    additional_surgical_details_reactive_table()
  })
  
  
  
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   ADDITIONAL SURGICAL DETAILS MODAL COMPLETE  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   ADDITIONAL SURGICAL DETAILS MODAL COMPLETE  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  ###~~~~~~~~~~~~~~~ #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   #########   ADDITIONAL SURGICAL DETAILS MODAL COMPLETE  #########    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ######### ~~~~~~~~~~~~~~~###
  

  ###################################### ---------------- BIOLOGICS -----------------------#################################
  ###################################### ---------------- BIOLOGICS -----------------------#################################
  ###################################### ---------------- BIOLOGICS -----------------------#################################
  ###################################### ---------------- BIOLOGICS -----------------------#################################
  ###################################### ---------------- BIOLOGICS -----------------------#################################
  
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
  
  output$posterior_bmp_kits <- renderText({
    bmp_kit_list <- reactiveValuesToList(posterior_bmp_kit_list)
    bmp_kits_df <- enframe(bmp_kit_list) %>%
      filter(value != 0) %>%
      mutate(statement = as.character(glue("{name} {value}")))
    
    if(nrow(bmp_kits_df) > 0){
      kit_statement <- glue_collapse(bmp_kits_df$statement, sep = "<br>")
    }else{
      kit_statement <- "none"
    }
    
    HTML("<div>", kit_statement, "</div>")
  })
  
  posterior_bmp_dose_reactive <- reactive({
    Reduce("+", reactiveValuesToList(posterior_bmp_dose_list))
  })
  

  output$posterior_bmp_dosage <- renderText({
    posterior_dose_statement <- as.character(glue("{posterior_bmp_dose_reactive()}mg"))
    HTML("<div>", posterior_dose_statement, "</div>")
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
  
  output$anterior_bmp_kits <- renderText({
    bmp_kit_list <- reactiveValuesToList(anterior_bmp_kit_list)
    bmp_kits_df <- enframe(bmp_kit_list) %>%
      filter(value != 0) %>%
      mutate(statement = as.character(glue("{name} {value}")))
    if(nrow(bmp_kits_df) > 0){
      kit_statement <- glue_collapse(bmp_kits_df$statement, sep = "<br>")
    }else{
      kit_statement <- "none"
    }
    HTML("<div>", kit_statement, "</div>")
  })
  
  anterior_bmp_dose_reactive <- reactive({
    Reduce("+", reactiveValuesToList(anterior_bmp_dose_list))
  })
  
  output$anterior_bmp_dosage <- renderText({
    anterior_dose_statement <- as.character(glue("{anterior_bmp_dose_reactive()}mg"))
    HTML("<div>", anterior_dose_statement, "</div>")
  })
  
  ###################################### ---------------- BIOLOGICS END -----------------------#################################
  ###################################### ---------------- BIOLOGICS END -----------------------#################################
  ###################################### ---------------- BIOLOGICS END -----------------------#################################
  ###################################### ---------------- BIOLOGICS END -----------------------#################################
  ###################################### ---------------- BIOLOGICS END -----------------------#################################
  
  

  ################------------------  Interbody Details (and generating results)    ----------------------######################  
  ################------------------  Interbody Details (and generating results)    ----------------------######################  
  
  interbody_df_reactive <- reactive({

    if(any(str_detect(all_objects_to_add_list$objects_df$object, "intervertebral_cage"))){
      
      intervertebral_cage_df <- all_objects_to_add_list$objects_df %>%
        filter(object == "intervertebral_cage")  %>%
        filter(vertebral_number == min(vertebral_number)) %>%
        mutate(level = as.character(glue_collapse(x = (all_objects_to_add_list$objects_df %>%  filter(object == "intervertebral_cage") %>%  arrange(vertebral_number))$level, sep = "-"))) %>%
        select(level, side, vertebral_number, object, approach)
      
      interbody_implants_df <-  all_objects_to_add_list$objects_df %>%
        filter(str_detect(object, "interbody_implant|tlif|plif|arthroplasty|corpectomy_cage")) %>%
        select(level, side, vertebral_number, object, approach) %>%
        union_all(intervertebral_cage_df) %>%
        distinct() %>%
        arrange(vertebral_number) %>%
        mutate(cage_id = paste(str_to_lower(str_replace_all(level, "-", "_")), side, object, sep = "_"))
      
    }else{
      interbody_implants_df <- all_objects_to_add_list$objects_df %>%
        filter(str_detect(object, "interbody_implant|tlif|plif|arthroplasty|corpectomy_cage")) %>%
        distinct() %>%
        arrange(vertebral_number) %>%
        mutate(cage_id = paste(str_to_lower(str_replace_all(level, "-", "_")), side, object, sep = "_"))
    }

    interbody_implants_df

  }) 
  # bindEvent(input$implants_complete, ignoreInit = TRUE)
  
  
  
  
  observeEvent(interbody_df_reactive(), ignoreInit = TRUE, ignoreNULL = TRUE, {
    if(nrow(interbody_df_reactive())>0){
      updateSwitchInput(session = session, 
                        inputId = "interbody_implant_true_false", 
                        value = TRUE)
      
      updatePickerInput(session = session, 
                        inputId = "interbody_implant_picker", 
                        label = "Interbodies", 
                        selected = interbody_df_reactive()$cage_id, 
                        choices = interbody_df_reactive()$cage_id)
    }
  }) 
  
  output$interbody_details_ui <- renderUI({
    
    if(length(input$interbody_implant_picker) >0){
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
            map(.x = input$interbody_implant_picker, .f = ~ make_interbody_conditional_panel_function(cage_id_input =  paste(.x)))
          )
      )
    }
    # }
  })
  
  
  interbody_details_df_reactive <- reactive({
    
    if(nrow(interbody_df_reactive()) > 0 && input$implants_complete > 0){
      interbody_details_df <- interbody_df_reactive() %>%
        # mutate(cage_id = str_to_lower(string = str_replace_all(string = level, pattern = "-", replacement = "_"))) %>%
        mutate(composition_label = glue("{cage_id}_interbody_composition")) %>%
        mutate(device_name_label = glue("{cage_id}_interbody_device_name")) %>%
        mutate(height_label = glue("{cage_id}_interbody_height")) %>%
        mutate(integrated_fixation_label = glue("{cage_id}_interbody_integrated_fixation")) %>%
        mutate(integrated_fixation_screw_anchor_label = glue("{cage_id}_interbody_integrated_fixation_screw_anchor")) %>%
        mutate(integrated_cranial_screw_1_label = glue("{cage_id}_interbody_cranial_screw_1_size")) %>%
        mutate(integrated_cranial_screw_2_label = glue("{cage_id}_interbody_cranial_screw_2_size")) %>%
        mutate(integrated_caudal_screw_1_label = glue("{cage_id}_interbody_caudal_screw_1_size")) %>%
        mutate(integrated_caudal_screw_2_label = glue("{cage_id}_interbody_caudal_screw_2_size")) %>%
        mutate(expandable_label = glue("{cage_id}_interbody_expandable")) %>%
        mutate(other_label = glue("{cage_id}_interbody_other")) %>%
        mutate(composition = map(.x = composition_label, .f = ~input[[.x]])) %>%
        unnest(composition) %>%
        mutate(device_name = map(.x = device_name_label, .f = ~input[[.x]])) %>%
        unnest(device_name) %>%
        mutate(height = map(.x = height_label, .f = ~input[[.x]])) %>%
        unnest(height) %>%
        mutate(integrated_fixation = map(.x = integrated_fixation_label, .f = ~if_else(isFALSE(input[[.x]]), "xx", "Integrated Fixation"))) %>%
        unnest(integrated_fixation) %>%
        mutate(integrated_fixation_screw_anchor = map(.x = integrated_fixation_screw_anchor_label, .f = ~ if_else(is.null(input[[.x]]), "xx", paste0(str_to_lower(input[[.x]]))))) %>%
        unnest(integrated_fixation_screw_anchor) %>%
        mutate(integrated_cranial_screw_1 = map(.x = integrated_cranial_screw_1_label, .f = ~if_else(is.null(input[[.x]]), "0", input[[.x]]))) %>%
        unnest(integrated_cranial_screw_1) %>%
        mutate(integrated_cranial_screw_2 = map(.x = integrated_cranial_screw_2_label, .f = ~if_else(is.null(input[[.x]]), "0", input[[.x]]))) %>%
        unnest(integrated_cranial_screw_2) %>%
        mutate(integrated_caudal_screw_1 = map(.x = integrated_caudal_screw_1_label, .f = ~if_else(is.null(input[[.x]]), "0", input[[.x]]))) %>%
        unnest(integrated_caudal_screw_1) %>%
        mutate(integrated_caudal_screw_2 = map(.x = integrated_caudal_screw_2_label, .f = ~if_else(is.null(input[[.x]]), "0", input[[.x]]))) %>%
        unnest(integrated_caudal_screw_2) %>%
        mutate(expandable = map(.x = expandable_label, .f = ~if_else(isFALSE(input[[.x]]), "xx", "Expandable"))) %>%
        unnest(expandable) %>%
        mutate(other = map(.x = other_label, .f = ~if_else(is.null(input[[.x]]), "xx", input[[.x]]))) %>%
        unnest(other) %>%
        mutate(composition = if_else(is.na(composition), " ", paste0(composition))) %>%
        mutate(other = if_else(is.na(other), " ", paste0(other))) %>%
        mutate(device_name = if_else(is.na(device_name), " ", paste0(device_name))) %>%
        mutate(expandable_statement = if_else(expandable == "Expandable", "expandable", " ")) %>%
        mutate(expandable_description = if_else(expandable == "Expandable",
                                                "The implant was then expanded until I felt firm contact with the endplates.",
                                                " "
        )
        ) %>%
        mutate(integrated_fixation_statement = if_else(integrated_fixation == "Integrated Fixation", "with integrated fixation", " ")) %>%
        mutate(integrated_fixation = if_else(integrated_fixation == "xx", "No integrated fixation", "Integrated Fixation")) %>%
        mutate(expandable = if_else(expandable == "xx", "Static", "Expandable")) %>%
        mutate(integrated_cranial_screws_statement = case_when(
          integrated_cranial_screw_1 == "0" & integrated_cranial_screw_2 == "0" ~ glue("No screws were inserted through the implant cranially."),
          integrated_cranial_screw_1 != "0" & integrated_cranial_screw_2 == "0" ~ glue("A {integrated_cranial_screw_1}mm screw was inserted cranially through the implant."),
          integrated_cranial_screw_1 == "0" & integrated_cranial_screw_2 != "0" ~ glue("A {integrated_cranial_screw_2}mm screw was inserted cranially through the implant."),
          integrated_cranial_screw_1 != "0" & integrated_cranial_screw_2 != "0" ~ glue("A {integrated_cranial_screw_1}mm screw and a {integrated_cranial_screw_2}mm screw were inserted cranially through the implant."),
        )) %>%
        mutate(integrated_cranial_screws_statement = as.character(integrated_cranial_screws_statement)) %>%
        mutate(integrated_cranial_screws_statement = if_else(integrated_fixation == "No integrated fixation", "", integrated_cranial_screws_statement)) %>%
        mutate(integrated_cranial_screws_statement = if_else(str_detect(integrated_fixation_screw_anchor, "anchor"), str_replace_all(integrated_cranial_screws_statement, "screw", "anchor"), integrated_cranial_screws_statement)) %>%
        mutate(integrated_caudal_screws_statement = case_when(
          integrated_caudal_screw_1 == "0" & integrated_caudal_screw_2 == "0" ~ glue("No screws were placed through the implant caudally."),
          integrated_caudal_screw_1 != "0" & integrated_caudal_screw_2 == "0" ~ glue("A {integrated_caudal_screw_1}mm screw was inserted aimed caudal."),
          integrated_caudal_screw_1 == "0" & integrated_caudal_screw_2 != "0" ~ glue("A {integrated_caudal_screw_2}mm screw was inserted aimed caudal."),
          integrated_caudal_screw_1 != "0" & integrated_caudal_screw_2 != "0" ~ glue("A {integrated_caudal_screw_1}mm screw and a {integrated_caudal_screw_2}mm screw were placed caudal through the implant."),
        )) %>%
        mutate(integrated_caudal_screws_statement = as.character(integrated_caudal_screws_statement)) %>%
        mutate(integrated_caudal_screws_statement = if_else(integrated_fixation == "No integrated fixation", "", integrated_caudal_screws_statement)) %>%
        mutate(integrated_caudal_screws_statement = if_else(str_detect(integrated_fixation_screw_anchor, "anchor"), str_replace_all(integrated_caudal_screws_statement, "screw", "anchor"), integrated_caudal_screws_statement)) %>%
        mutate(implant_statement = glue("At the {level} interspace, a {height}mm height {composition} {device_name} {other} {expandable_statement} implant {integrated_fixation_statement} was selected and placed into the {level} interspace. {expandable_description} {integrated_cranial_screws_statement} {integrated_caudal_screws_statement}")) %>%
        mutate(implant_statement = str_squish(implant_statement)) %>%
        mutate(implant_statement = str_remove_all(string = implant_statement, pattern = "()")) %>% 
        select(level, vertebral_number, approach, object, composition, device_name, height, integrated_fixation, expandable, other, implant_statement) %>%
        mutate(across(everything(), ~ as.character(.x))) %>%
        mutate(across(everything(), ~ replace_na(.x, " ")))
      
    }else{
      interbody_details_df <- tibble(level = character(), vertebral_number = double(), object = character(), approach = character(),  composition = character(), implant_statement = character())
    }
    interbody_details_df
  })
  
  ######################################### INTERBODY END #########################################
  ######################################### INTERBODY END #########################################
  
  ################################################### SCREWS UI ##########################################################
  ################################################### SCREWS UI ##########################################################
  ################################################### SCREWS UI ##########################################################
  
  observeEvent(input$implants_complete, ignoreInit = TRUE, {
    if(nrow(all_objects_to_add_list$objects_df)>0){
      selected_screws <- jh_make_screw_details_inputs_df_function(all_objects = all_objects_to_add_list$objects_df, return_shiny_inputs_df = FALSE)$side_level_object
      updateCheckboxGroupInput(session = session, 
                               inputId = "screws_implanted_picker_for_ui", 
                               label = "screws:",
                        choices = selected_screws, 
                        selected = selected_screws)
    }
  })
  

  output$screw_details_ui <- renderUI({
    if(length(input$screws_implanted_picker_for_ui) > 0){
          all_screw_size_type_inputs_df <- jh_make_screw_details_inputs_df_function(all_objects = all_objects_to_add_list$objects_df, 
                                                                                    return_shiny_inputs_df = TRUE) ## this adds the needed 'left_object' and 'right_object' so that it matches the  screws_implanted_picker_for_ui format
      box(width = 12,
          title = div(style = "font-size:22px; font-weight:bold; text-align:center", "Screw Details:"), collapsible = TRUE,
          jh_make_shiny_table_row_function(left_column_label = "Screw/Rod Manufacturer:",
                                           left_column_percent_width = 30,
                                           input_type = "checkbox",
                                           font_size = 16,
                                           checkboxes_inline = TRUE,
                                           input_id = "implant_manufacturer",
                                           choices_vector = c("Alphatec", "Depuy Synthes", "Globus Medical", "K2 Stryker", "Medicrea", "Medtronic", "NuVasive", "Orthofix", "Zimmer Bioment", "Other")
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
                fluidRow(
                  column(4,
                         all_screw_size_type_inputs_df$level_object_label[[.x]]
                  ),
                  ## LEFT DIAMETER ##
                  column(2,
                         conditionalPanel(condition = glue("input.screws_implanted_picker_for_ui.indexOf('{all_screw_size_type_inputs_df$left_object[[.x]]}') >-1"),
                                          all_screw_size_type_inputs_df$left_diameter_input[[.x]]
                                          # )
                         )
                  ),
                  ## LEFT LENGTH ##
                  column(2,
                         conditionalPanel(condition = glue("input.screws_implanted_picker_for_ui.indexOf('{all_screw_size_type_inputs_df$left_object[[.x]]}') >-1"),
                                          all_screw_size_type_inputs_df$left_length_input[[.x]]
                         )
                  ),
                  ## RIGHT DIAMETER ##
                  column(2,
                         conditionalPanel(condition = glue("input.screws_implanted_picker_for_ui.indexOf('{all_screw_size_type_inputs_df$right_object[[.x]]}') >-1"),
                                          all_screw_size_type_inputs_df$right_diameter_input[[.x]]
                         )
                  ),
                  ## RIGHT LENGTH ##
                  column(2,
                         conditionalPanel(condition = glue("input.screws_implanted_picker_for_ui.indexOf('{all_screw_size_type_inputs_df$right_object[[.x]]}') >-1"),
                                          all_screw_size_type_inputs_df$right_length_input[[.x]]
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
                # conditionalPanel(condition = glue("input.screws_implanted_picker_for_ui.indexOf('{all_screw_size_type_inputs_df$level_object_label[[.x]]}') >-1"),
                fluidRow(
                  column(4,
                         all_screw_size_type_inputs_df$level_object_label[[.x]]
                  ),
                  column(4,
                         conditionalPanel(condition = glue("input.screws_implanted_picker_for_ui.indexOf('{all_screw_size_type_inputs_df$left_object[[.x]]}') >-1"),
                                          all_screw_size_type_inputs_df$left_type_input[[.x]]
                         )
                  ),
                  column(4,
                         conditionalPanel(condition = glue("input.screws_implanted_picker_for_ui.indexOf('{all_screw_size_type_inputs_df$right_object[[.x]]}') >-1"),
                                          all_screw_size_type_inputs_df$right_type_input[[.x]]
                         )
                  )
                )
              # )
          )
      )
    }
  }) 
  

  ################------------------  Screw Size RESULTS  ----------------------######################  
  ################------------------  Screw Size RESULTS  ----------------------######################  
  

  screw_size_details_df_reactive <- reactive({
    
    if(length(input$screws_implanted_picker_for_ui) > 0 && input$implants_complete > 0){
      
      screw_size_details_df <- jh_make_screw_details_inputs_df_function(all_objects = all_objects_to_add_list$objects_df, 
                                               return_shiny_inputs_df = FALSE)  %>%
        mutate(screw_diameter = map(.x = screw_diameter_input_name, .f = ~ input[[.x]])) %>%
        unnest(screw_diameter) %>%
        mutate(screw_length = map(.x = screw_length_input_name, .f = ~ input[[.x]])) %>%
        unnest(screw_length) %>%
        mutate(screw_type = map(.x = screw_type_input_name, .f = ~ input[[.x]])) %>%
        unnest(screw_type) %>%
        mutate(screw_type = case_when(
          screw_type == "U" ~ "Uniaxial",
          screw_type == "M" ~ "Monoaxial",
          screw_type == "P" ~ "Polyaxial",
          screw_type == "Red" ~ "Reduction",
          screw_type == "Offset" ~ "Offset",
          screw_type == "Anterior" ~ "Anterior",
          screw_type == "Fixed Angle" ~ "fixed angle",
          screw_type == "Variable Angle" ~ "variable angle"
        )) %>%
        mutate(remove_unused_anterior_screws = if_else(screw_type == "Anterior" & (is.na(screw_length) | screw_length == "" | screw_length == screw_diameter), "remove", "keep")) %>%
        filter(remove_unused_anterior_screws == "keep") %>%
        select(-remove_unused_anterior_screws) %>%
        mutate(screw_diameter = as.character(screw_diameter)) %>%
        mutate(screw_length = as.character(screw_length)) %>%
        mutate(screw_diameter = if_else(is.na(screw_diameter), "na", screw_diameter)) %>%
        mutate(screw_length = if_else(is.na(screw_length), "na", screw_length)) %>%
        mutate(screw_size = case_when(
          screw_diameter == "na" & screw_length == "na" ~ "",
          screw_diameter != "na" & screw_length == "na" ~ paste0(screw_diameter, "mm"),
          screw_diameter == "na" & screw_length != "na" ~ paste0(screw_length, "mm"),
          screw_diameter != "na" & screw_length != "na" ~ paste0(screw_diameter, "x", screw_length, "mm"))
        )  %>%
        mutate(screw_size_type = paste(screw_size, screw_type)) 
  
    }else{
      screw_size_details_df <- tibble(level = character(),
                                      side = character(),
                                      approach = character(),
                                      object = character(),
                                      side_level_object = character(),
                                      screw_diameter_input_name = character(),
                                      screw_length_input_name = character(),
                                      screw_type_input_name = character(),
                                      screw_diameter = character(),
                                      screw_length = character(),
                                      screw_type = character(),
                                      screw_size = character(),
                                      screw_size_type = character()
                                      )
    }
    screw_size_details_df
  })
  
  # output$test_screw_size_details_table_output <- renderTable({
  #   screw_size_details_df_reactive()
  # })
  # 
  
  ################################### SCREWS UI END ######################################
  
  ################################### COMPUTE NUMBER OF RODS CROSSING AT EACH LEVEL #####################################
  
  left_rod_crossing_table_reactive <- reactive({
    left_rods_connectors_list <- list()
    if(input$implants_complete > 0 && !is.null(input$approach_sequence) && str_detect(input$approach_sequence, "posterior")){

      custom_rods_vector_list <- list()
      if(input$add_left_custom_rods == TRUE){
        if(input$left_custom_rods_number > 1 & length(input$left_custom_rod_1) > 1){
          custom_rods_vector_list$custom_rod_1 <- input$left_custom_rod_1
        }else{
          custom_rods_vector_list$custom_rod_1 <- c("")
        }
        if(input$left_custom_rods_number > 1 & length(input$left_custom_rod_2) > 1){
          custom_rods_vector_list$custom_rod_2 <- input$left_custom_rod_2
        }else{
          custom_rods_vector_list$custom_rod_2 <- c("")
        }
        if(input$left_custom_rods_number > 2 & length(input$left_custom_rod_3) > 1){
          custom_rods_vector_list$custom_rod_3 <- input$left_custom_rod_3
        }else{
          custom_rods_vector_list$custom_rod_3 <- c("")
        }
        if(input$left_custom_rods_number > 3 & length(input$left_custom_rod_4) > 1){
          custom_rods_vector_list$custom_rod_4 <- input$left_custom_rod_4
        }else{
          custom_rods_vector_list$custom_rod_4 <- c("")
        }
        if(input$left_custom_rods_number > 4 & length(input$left_custom_rod_5) > 1){
          custom_rods_vector_list$custom_rod_5 <- input$left_custom_rod_5
        }else{
          custom_rods_vector_list$custom_rod_5 <- c("")
        }
      }
      
      if(length(left_revision_implants_reactive_list()$free_revision_screws_vector)>0){
        left_retained_free_screws_df <- left_revision_implants_reactive_list()$revision_implants_status_df %>%
          filter(remove_retain == "retain", prior_rod_connected == "no") %>%
          select(level, vertebral_number, x, y, side, object)
        
        left_implants_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left") %>%
          select(level, vertebral_number, x, y, side, object) %>%
          bind_rows(left_retained_free_screws_df) %>%
          arrange(y) %>%
          mutate(implant_label = glue("{level} {str_to_title(str_replace_all(object, '_', ' '))}")) 
        
      }else{
        left_implants_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "left") %>%
          select(level, vertebral_number, x, y, side, object) %>%
          arrange(y) %>%
          mutate(implant_label = glue("{level} {str_to_title(str_replace_all(object, '_', ' '))}")) 
      }
      
      if(length(left_implants_df$level) > 0){
        ############# MAKE THE RODS #############
        left_rods_connectors_list <- build_unilateral_rods_list_function(unilateral_full_implant_df = left_implants_df,
                                                                         rod_side = "left",
                                                                         include_revision_rods_in_list = TRUE,
                                                                         add_accessory_rod = input$add_left_accessory_rod,
                                                                         accessory_rod_vector = input$left_accessory_rod, 
                                                                         add_satellite_rod = input$add_left_satellite_rod,
                                                                         satellite_rods_vector = input$left_satellite_rod,
                                                                         add_intercalary_rod = input$add_left_intercalary_rod, 
                                                                         intercalary_rods_vector = input$left_intercalary_rod, 
                                                                         intercalary_rod_junction = input$left_intercalary_rod_junction, 
                                                                         add_linked_rod = input$add_left_linked_rod,
                                                                         linked_rod_vector = input$left_linked_rod,
                                                                         add_kickstand_rod = input$add_left_kickstand_rod,
                                                                         kickstand_rod_vector = input$left_kickstand_rod,
                                                                         add_custom_rods = input$add_left_custom_rods,
                                                                         custom_rods_vector_list = custom_rods_vector_list,
                                                                         link_revision_rods_true_false = input$link_left_revision_rods_true_false,
                                                                         revision_rod_matrix = left_revision_rod_matrix_reactive()
        )
      }else{
        tibble(level = labels_df$level, left_rod_count = 0, left_rods_crossing = "")
      }
      
      
      if(length(names(left_rods_connectors_list$rod_list))>0){
        for (name in names(left_rods_connectors_list$rod_list)) {
          rods_crossing_by_level_df[[name]] <- name
        } 
        
        rods_crossing_by_level_df %>%
          pivot_longer(cols = -level, names_to = "rod_type", values_to = "value") %>%
          select(level, rod_type) %>%
          mutate(rod_crosses_lower = pmap(.l = list(..1 = level, ..2 = rod_type), .f = ~ st_intersects(x = lower_lines_list[[..1]], y = left_rods_connectors_list$rod_list[[..2]], sparse = FALSE)[1])) %>%
          unnest(rod_crosses_lower)  %>%
          mutate(rod_crosses_upper = pmap(.l = list(..1 = level, ..2 = rod_type), .f = ~ st_intersects(x = upper_lines_list[[..1]], y = left_rods_connectors_list$rod_list[[..2]], sparse = FALSE)[1])) %>%
          unnest(rod_crosses_upper) %>%
          mutate(rod_crosses = if_else(rod_crosses_lower == TRUE & rod_crosses_upper == TRUE, TRUE, FALSE)) %>%
          select(level, rod_type, rod_crosses) %>%
          filter(rod_crosses == TRUE) %>%
          pivot_wider(names_from = rod_type, values_from = rod_crosses) %>%
          mutate(across(-level, ~if_else(is.na(.), FALSE, .))) %>%
          mutate(left_rod_count = rowSums(select(., -level))) %>%
          pivot_longer(cols = c(-level, -left_rod_count), names_to = "rod_type", values_to = "crosses") %>%
          filter(crosses == TRUE) %>%
          mutate(crosses = "rods_crossing") %>%
          pivot_wider(names_from = crosses, values_from = rod_type) %>%
          mutate(rods_crossing = map(.x = rods_crossing, .f = ~ glue_collapse(.x, sep = " + "))) %>%
          unnest(rods_crossing) %>%
          select(level, left_rod_count, left_rods_crossing = rods_crossing)
        
      }else{
        tibble(level = labels_df$level, left_rod_count = 0, left_rods_crossing = "")
      }
    }else{
      tibble(level = labels_df$level, left_rod_count = 0, left_rods_crossing = "")
    }
    
  })
  
  right_rod_crossing_table_reactive <- reactive({
    
    right_rods_connectors_list <- list()
    # if(input$implants_complete > 0){
    if(input$implants_complete > 0 && !is.null(input$approach_sequence) && str_detect(input$approach_sequence, "posterior")){
      custom_rods_vector_list <- list()
      if(input$add_right_custom_rods == TRUE){
        if(input$right_custom_rods_number > 1 & length(input$right_custom_rod_1) > 1){
          custom_rods_vector_list$custom_rod_1 <- input$right_custom_rod_1
        }else{
          custom_rods_vector_list$custom_rod_1 <- c("")
        }
        if(input$right_custom_rods_number > 1 & length(input$right_custom_rod_2) > 1){
          custom_rods_vector_list$custom_rod_2 <- input$right_custom_rod_2
        }else{
          custom_rods_vector_list$custom_rod_2 <- c("")
        }
        if(input$right_custom_rods_number > 2 & length(input$right_custom_rod_3) > 1){
          custom_rods_vector_list$custom_rod_3 <- input$right_custom_rod_3
        }else{
          custom_rods_vector_list$custom_rod_3 <- c("")
        }
        if(input$right_custom_rods_number > 3 & length(input$right_custom_rod_4) > 1){
          custom_rods_vector_list$custom_rod_4 <- input$right_custom_rod_4
        }else{
          custom_rods_vector_list$custom_rod_4 <- c("")
        }
        if(input$right_custom_rods_number > 4 & length(input$right_custom_rod_5) > 1){
          custom_rods_vector_list$custom_rod_5 <- input$right_custom_rod_5
        }else{
          custom_rods_vector_list$custom_rod_5 <- c("")
        }
      }
      
      if(length(right_revision_implants_reactive_list()$free_revision_screws_vector)>0){
        right_retained_free_screws_df <- right_revision_implants_reactive_list()$revision_implants_status_df %>%
          filter(remove_retain == "retain", prior_rod_connected == "no") %>%
          select(level, vertebral_number, x, y, side, object)
        
        right_implants_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right") %>%
          select(level, vertebral_number, x, y, side, object) %>%
          bind_rows(right_retained_free_screws_df) %>%
          arrange(y) %>%
          mutate(implant_label = glue("{level} {str_to_title(str_replace_all(object, '_', ' '))}")) 
        
      }else{
        right_implants_df <- jh_filter_posterior_implants_by_side_function(all_objects_df = all_objects_to_add_list$objects_df, side_to_filter = "right") %>%
          select(level, vertebral_number, x, y, side, object) %>%
          arrange(y) %>%
          mutate(implant_label = glue("{level} {str_to_title(str_replace_all(object, '_', ' '))}")) 
      }
      
      if(length(right_implants_df$level) > 0){
        ############# MAKE THE RODS #############
        right_rods_connectors_list <- build_unilateral_rods_list_function(unilateral_full_implant_df = right_implants_df,
                                                                          rod_side = "right",
                                                                          include_revision_rods_in_list = TRUE,
                                                                          add_accessory_rod = input$add_right_accessory_rod,
                                                                          accessory_rod_vector = input$right_accessory_rod, 
                                                                          add_satellite_rod = input$add_right_satellite_rod,
                                                                          satellite_rods_vector = input$right_satellite_rod,
                                                                          add_intercalary_rod = input$add_right_intercalary_rod, 
                                                                          intercalary_rods_vector = input$right_intercalary_rod, 
                                                                          intercalary_rod_junction = input$right_intercalary_rod_junction, 
                                                                          add_linked_rod = input$add_right_linked_rod,
                                                                          linked_rod_vector = input$right_linked_rod,
                                                                          add_kickstand_rod = input$add_right_kickstand_rod,
                                                                          kickstand_rod_vector = input$right_kickstand_rod,
                                                                          add_custom_rods = input$add_right_custom_rods,
                                                                          custom_rods_vector_list = custom_rods_vector_list,
                                                                          link_revision_rods_true_false = input$link_right_revision_rods_true_false,
                                                                          revision_rod_matrix = right_revision_rod_matrix_reactive()
        )
      }else{
        tibble(level = labels_df$level, left_rod_count = 0, left_rods_crossing = "")
      }
      
      if(length(names(right_rods_connectors_list$rod_list))>0){
        for (name in names(right_rods_connectors_list$rod_list)) {
          rods_crossing_by_level_df[[name]] <- name
        } 
        
        rods_crossing_by_level_df %>%
          pivot_longer(cols = -level, names_to = "rod_type", values_to = "value") %>%
          select(level, rod_type) %>%
          mutate(rod_crosses_lower = pmap(.l = list(..1 = level, ..2 = rod_type), .f = ~ st_intersects(x = lower_lines_list[[..1]], y = right_rods_connectors_list$rod_list[[..2]], sparse = FALSE)[1])) %>%
          unnest(rod_crosses_lower)  %>%
          mutate(rod_crosses_upper = pmap(.l = list(..1 = level, ..2 = rod_type), .f = ~ st_intersects(x = upper_lines_list[[..1]], y = right_rods_connectors_list$rod_list[[..2]], sparse = FALSE)[1])) %>%
          unnest(rod_crosses_upper) %>%
          mutate(rod_crosses = if_else(rod_crosses_lower == TRUE & rod_crosses_upper == TRUE, TRUE, FALSE)) %>%
          select(level, rod_type, rod_crosses) %>%
          filter(rod_crosses == TRUE) %>%
          pivot_wider(names_from = rod_type, values_from = rod_crosses) %>%
          mutate(across(-level, ~if_else(is.na(.), FALSE, .))) %>%
          mutate(right_rod_count = rowSums(select(., -level))) %>%
          pivot_longer(cols = c(-level, -right_rod_count), names_to = "rod_type", values_to = "crosses") %>%
          filter(crosses == TRUE) %>%
          mutate(crosses = "rods_crossing") %>%
          pivot_wider(names_from = crosses, values_from = rod_type) %>%
          mutate(rods_crossing = map(.x = rods_crossing, .f = ~ glue_collapse(.x, sep = " + "))) %>%
          unnest(rods_crossing) %>%
          select(level, right_rod_count, right_rods_crossing = rods_crossing)
        
      }else{
        tibble(level = labels_df$level, right_rod_count = 0, right_rods_crossing = "")
      }
    }else{
      tibble(level = labels_df$level, right_rod_count = 0, right_rods_crossing = "")
    }
    
  })
  
  
  
  
  ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
  ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
  ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
  
  ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
  ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
  ########################################################  OPERATIVE NOTE GENERATOR    ########################################################  
  
  ################------------------  FIRST UPDATE THE OPTIONS USING THE DETAILS ALREADY INPUTTED    ----------------------######################  
  
  
  added_rods_statement_reactive <- reactive({
    additional_rods_list <- list()
    added_rods_statement <- ""
    
    if(input$implants_complete > 0){
    #LEFT#
    if(input$add_left_accessory_rod == TRUE){
      additional_rods_list$left_accessory <-  jh_generate_supplemental_rod_statement_function(rod_type = "accessory",
                                                      rod_side = "left",
                                                      rod_size = input$left_accessory_rod_size,
                                                      rod_material = input$left_accessory_rod_material,
                                                      rod_vector = input$left_accessory_rod)
          }
    if(input$add_left_satellite_rod == TRUE){
      additional_rods_list$left_satellite <-  jh_generate_supplemental_rod_statement_function(rod_type = "satellite",
                                                                                              rod_side = "left",
                                                                                              rod_size = input$left_satellite_rod_size,
                                                                                              rod_material = input$left_satellite_rod_material,
                                                                                              rod_vector = input$left_satellite_rod)
    }
    if(input$add_left_intercalary_rod == TRUE){
      additional_rods_list$left_intercalary <-  jh_generate_supplemental_rod_statement_function(rod_type = "intercalary",
                                                                                              rod_side = "left",
                                                                                              rod_size = input$left_intercalary_rod_size,
                                                                                              rod_material = input$left_intercalary_rod_material,
                                                                                              rod_vector = input$left_intercalary_rod, 
                                                                                              intercalary_rod_junction = input$left_intercalary_rod_junction)
    }
    if(input$add_left_linked_rod == TRUE){
      additional_rods_list$left_linked <-  jh_generate_supplemental_rod_statement_function(rod_type = "linked",
                                                                                              rod_side = "left",
                                                                                              rod_size = input$left_linked_rod_size,
                                                                                              rod_material = input$left_linked_rod_material,
                                                                                              rod_vector = input$left_linked_rod)
    }
    if(input$add_left_kickstand_rod == TRUE){
      additional_rods_list$left_kickstand <-  jh_generate_supplemental_rod_statement_function(rod_type = "kickstand",
                                                                                           rod_side = "left",
                                                                                           rod_size = input$left_kickstand_rod_size,
                                                                                           rod_material = input$left_kickstand_rod_material,
                                                                                           rod_vector = input$left_kickstand_rod)
    }
    
    #RIGHT#
    if(input$add_right_accessory_rod == TRUE){
      additional_rods_list$right_accessory <-  jh_generate_supplemental_rod_statement_function(rod_type = "accessory",
                                                                                              rod_side = "right",
                                                                                              rod_size = input$right_accessory_rod_size,
                                                                                              rod_material = input$right_accessory_rod_material,
                                                                                              rod_vector = input$right_accessory_rod)
    }
    if(input$add_right_satellite_rod == TRUE){
      additional_rods_list$right_satellite <-  jh_generate_supplemental_rod_statement_function(rod_type = "satellite",
                                                                                              rod_side = "right",
                                                                                              rod_size = input$right_satellite_rod_size,
                                                                                              rod_material = input$right_satellite_rod_material,
                                                                                              rod_vector = input$right_satellite_rod)
    }
    if(input$add_right_intercalary_rod == TRUE){
      additional_rods_list$right_intercalary <-  jh_generate_supplemental_rod_statement_function(rod_type = "intercalary",
                                                                                                rod_side = "right",
                                                                                                rod_size = input$right_intercalary_rod_size,
                                                                                                rod_material = input$right_intercalary_rod_material,
                                                                                                rod_vector = input$right_intercalary_rod, 
                                                                                                intercalary_rod_junction = input$right_intercalary_rod_junction)
    }
    if(input$add_right_linked_rod == TRUE){
      additional_rods_list$right_linked <-  jh_generate_supplemental_rod_statement_function(rod_type = "linked",
                                                                                           rod_side = "right",
                                                                                           rod_size = input$right_linked_rod_size,
                                                                                           rod_material = input$right_linked_rod_material,
                                                                                           rod_vector = input$right_linked_rod)
    }
    if(input$add_right_kickstand_rod == TRUE){
      additional_rods_list$right_kickstand <-  jh_generate_supplemental_rod_statement_function(rod_type = "kickstand",
                                                                                              rod_side = "right",
                                                                                              rod_size = input$right_kickstand_rod_size,
                                                                                              rod_material = input$right_kickstand_rod_material,
                                                                                              rod_vector = input$right_kickstand_rod)
    }
    
    if(length(additional_rods_list) > 0){
      added_rods_statement <- glue_collapse(additional_rods_list, sep = " ")
    }
    }
    added_rods_statement
  }) 
    # bindEvent(input$implants_complete, ignoreInit = TRUE) ### THIS MESSES UP THE CONFIRM PROCEDURES AT THE END
  
  
  postop_plan_list_reactive <- reactive({
    postop_plan <- list()
    
    if(input$implants_complete > 0){
 
    ##########   dispo  #############
    if(length(input$postop_dispo) >0){
      postop_plan$postop_dispo_label <- paste("  - Postop Destination: ", glue_collapse(input$postop_dispo, sep = "; ", last = "; and "))
    }
    
    ##########   postop_abx  #############
    if(length(input$postop_abx) >0){
      postop_plan$postop_abx_label <- format_plan_list_function(plan_label = "Postop Abx", input_vector = input$postop_abx)
      
    }
    
    ##########   postop_map_goals  #############
    if(length(input$postop_map_goals) > 0){
      postop_plan$postop_map_goals_label <- paste("  - Postop MAP goals: ", glue_collapse(input$postop_map_goals, sep = "; ", last = "; and "))
    }
    
    ##########   postop transfusion threshold  #############
    if(length(input$postop_transfusion_threshold) > 0){
      postop_plan$postop_transfusion_threshold_label <- paste("  - Postop Anemia: ", glue_collapse(input$postop_transfusion_threshold, sep = "; ", last = "; and "))
    }
    
    ##########   postop_imaging  #############
    if(length(input$postop_imaging) > 0){
      postop_plan$postop_imaging_label <- format_plan_list_function(plan_label = "Postop Imaging", input_vector = input$postop_imaging)
      # postop_plan$postop_imaging_label <- paste("  - Postop Imaging: ", glue_collapse(input$postop_imaging, sep = "; ", last = "; and "))
    }
    ##########   postop_pain  #############
    if(length(input$postop_pain) >0){
      postop_plan$postop_pain_label <- format_plan_list_function(plan_label = "Pain Control", input_vector = input$postop_pain)
      
    }
    ##########   postop_activity  #############
    if(length(input$postop_activity) >0){
      postop_plan$postop_activity_label <- format_plan_list_function(plan_label = "Activity", input_vector = input$postop_activity)
    }
    ##########   postop_brace  #############
    if(length(input$postop_brace) >0){
      postop_plan$postop_brace_label <- paste("  - Bracing: ", glue_collapse(input$postop_brace, sep = "; ", last = "; and "))
    }
    ##########   postop_diet  #############
    if(length(input$postop_diet) >0){
      postop_plan$postop_diet_label <- format_plan_list_function(plan_label = "Diet/GI", input_vector = input$postop_diet)
    }
    ##########   postop_foley  #############
    if(length(input$postop_foley) >0){
      postop_plan$postop_foley_label <- format_plan_list_function(plan_label = "Foley", input_vector = input$postop_foley)
    }
    
    ##########   postop_dvt_ppx  #############
    if(length(input$postop_dvt_ppx) >0){
      postop_plan$postop_dvt_label <- format_plan_list_function(plan_label = "DVT PPX/Anticoag/Antiplatelet", input_vector = input$postop_dvt_ppx)
    }
    ##########   postop_drains_dressing  #############
    if(length(input$postop_drains_dressing) >0){
      postop_plan$postop_drains_dressing_label <- format_plan_list_function(plan_label = "Drains & Dressing", input_vector = input$postop_drains_dressing)
    }
    ##########   postop_followup  #############
    if(length(input$postop_followup) >0){
      if(str_detect(str_to_lower(input$postop_followup), "return to")){
        postop_plan$postop_followup_label <- paste(glue_collapse(input$postop_followup, sep = "; ", last = "; and "))
      }else{
        postop_plan$postop_followup_label <- paste("  - Follow-up: ", glue_collapse(input$postop_followup, sep = "; ", last = "; and "))
      }
    }
    }
    postop_plan
  }) 
    # bindEvent(input$additional_surgical_details_complete, ignoreInit = TRUE)
  
  
  ######################### GENERATE ALL INPUTS FOR POSTERIOR OP NOTE:
  
  posterior_op_note_inputs_list_reactive <- reactive({
    posterior_op_note_inputs_list_reactive <- list()
    
    # if(input$tabs != "patient_details_procedures"){
    ######
    if(input$implants_complete > 0){
    posterior_approach_objects_df <- all_objects_to_add_list$objects_df %>%
      filter(approach == "posterior")  %>%
      select(-object_constructed) 
    
    if(nrow(interbody_details_df_reactive()) > 0){
      posterior_approach_objects_df <- posterior_approach_objects_df %>%
        left_join(interbody_details_df_reactive() %>% select(level, approach, object, implant_statement)) %>%
        replace_na(list(implant_statement = " "))
    }else{
      posterior_approach_objects_df <- posterior_approach_objects_df %>%
        mutate(implant_statement = " ")
    }
    
    if(nrow(screw_size_details_df_reactive() %>% filter(approach == "posterior"))>0){
      
      screw_results_df <- screw_size_details_df_reactive() %>%
        filter(approach == "posterior") %>%
        select(level, side, object, screw_size_type)
      
      posterior_approach_objects_df <- posterior_approach_objects_df %>%
        left_join(screw_results_df) %>%
        replace_na(list(screw_size_type = " "))
    }else{
      posterior_approach_objects_df <- posterior_approach_objects_df %>%
        mutate(screw_size_type = " ")
    }
    
    posterior_approach_objects_df <- posterior_approach_objects_df %>%
      mutate(object = if_else(level == "C1" & object == "lateral_mass_screw", "c1_lateral_mass_screw", object)) 
    
    
    posterior_op_note_inputs_list_reactive$posterior_approach_objects_df <- posterior_approach_objects_df
    # posterior_op_note_inputs_list_reactive()$posterior_approach_objects_df
    ####### fusion levels 
    
    if(length(input$posterior_fusion_levels_confirmed)>0){
      posterior_op_note_inputs_list_reactive$fusions_df <- tibble(level = input$posterior_fusion_levels_confirmed) %>%
        left_join(levels_numbered_df)
    }else{
      posterior_op_note_inputs_list_reactive$fusions_df <- tibble(level = character(), vertebral_number = double(), object = character())
    }
    
    ####### C2 nerve transection
    if(c2_nerve_transection_list_reactive()$c1_screws != "none"){
      posterior_op_note_inputs_list_reactive$c2_nerve_transection <- c2_nerve_transection_list_reactive()$c2_nerve_transection_result
    }else{
      posterior_op_note_inputs_list_reactive$c2_nerve_transection <- "na"
    }
    
    
    ####### Head Positioning
    posterior_op_note_inputs_list_reactive$head_positioning_posterior <- input$head_positioning_posterior
    
    #######
    posterior_op_note_inputs_list_reactive$approach_specified_posterior <- input$approach_specified_posterior
    
    #######
    posterior_op_note_inputs_list_reactive$approach_open_mis <- input$approach_open_mis
    
    #######
    posterior_op_note_inputs_list_reactive$approach_robot_navigation <- input$approach_robot_navigation
    
    #######
    neuromonitoring_input_list <- list()
    neuromonitoring_input_list$modalities <- input$neuromonitoring
    neuromonitoring_input_list$emg <- if_else(input$triggered_emg == "No", " ", paste0(input$triggered_emg))
    
    if(any(input$pre_positioning_motors == "Not obtained")){ 
      neuromonitoring_input_list$pre_positioning_motors <- " "
    }else{
      neuromonitoring_input_list$pre_positioning_motors <-  paste0("Pre-positioning motors were ", str_to_lower(glue_collapse(input$pre_positioning_motors, sep = ", ", last = " and ")), ".")
    }
    
    neuromonitoring_input_list$neuromonitoring_signal_stability <- if_else(str_detect(string = paste(input$neuromonitoring, collapse = ", "), pattern = "SSEP"),
                                                                           as.character(input$neuromonitoring_signal_stability),
                                                                           " ")
    
    posterior_op_note_inputs_list_reactive$neuromonitoring_input_list <- neuromonitoring_input_list
    
    
    ### USE OF MICROSCOPE###
    if(any(str_detect(str_to_lower(input$posterior_additional_approach_details_checkbox), "microscop"))){
      posterior_op_note_inputs_list_reactive$approach_microscope <- "After the deep retractors were placed into position, the microscope was brought into the field for the next steps in the procedure."
    }else{
      posterior_op_note_inputs_list_reactive$approach_microscope <- ""
    }
    
    #######
    if(length(input$implant_start_point_method) == 0){
      implant_start_point_method <- "NA"
    }else{
      implant_start_point_method <- input$implant_start_point_method
    }
    
    posterior_op_note_inputs_list_reactive$implant_start_point_method <- implant_start_point_method
    
    #######
    if(length(input$implant_position_confirmation_method) == 0){
      implant_position_confirmation_method <- "NA"
    }else{
      implant_position_confirmation_method <- input$implant_position_confirmation_method
    }
    
    posterior_op_note_inputs_list_reactive$implant_position_confirmation_method <- implant_position_confirmation_method
    
    
    #########
    posterior_op_note_inputs_list_reactive$instruments_used_for_bony_work <- input$instruments_used_for_bony_work
    
    #######
    posterior_op_note_inputs_list_reactive$local_anesthesia <- input$local_anesthesia
    
    
    # INTRAOP COMPLICATIONS STATEMENT
    #######
    complications_list <- list()
    
    if(length(input$intraoperative_complications_vector)> 0){
      
      complication_text <- paste(glue_collapse(x = str_to_lower(input$intraoperative_complications_vector), sep = ","))
      
      if(str_detect(complication_text, pattern = "durotomy")){
        if(length(input$durotomy_timing)>0){
          if(str_to_lower(input$durotomy_timing) == "other"){
            durotomy_timing_instrument_statement <- glue("An incidental durotomy was created during the procedure, which caused a CSF leak.")
          }else{
            durotomy_timing_instrument_statement <- glue("During the {str_to_lower(input$durotomy_timing)}, an incidental durotomy was created, which caused a CSF leak.")
          }
        }else{
          durotomy_timing_instrument_statement <- glue("An incidental durotomy was created during the procedure, which caused a CSF leak.")
        }
        
        if(length(input$durotomy_repair_method)>0){
          if(any(input$durotomy_repair_method == "No Repair Performed")){
            durotomy_repair_statement <- glue("No dural repair was performed.")
          }else{
            durotomy_repair_statement <- glue("The dura was repaired using {glue_collapse(input$durotomy_repair_method, sep = ', ', last = ' and ')}.")
          }
        }else{
          durotomy_repair_statement <- " "
        }
        complications_list$durotomy <- paste(durotomy_timing_instrument_statement, str_remove_all(string = str_to_sentence(durotomy_repair_statement), pattern = "primarily repaired using ")) 
      }
      
      if(str_detect(complication_text, pattern = "other")){
        complications_list$other <- glue("During the case, the procedure was complicated by {input$other_intraoperative_complications}.")
      }
      
      other_predefined_complications_vector <- discard(.x = input$intraoperative_complications_vector, .p = ~ str_detect(.x, "Other|Durotomy"))
      
      complications_list$other_predifined_complications <- glue("The procedure was complicated by {glue_collapse(other_predefined_complications_vector, sep = ', ', last = ' and ')}")
    }
    
    posterior_op_note_inputs_list_reactive$complications_list <- complications_list
    
    
    #######
    posterior_op_note_inputs_list_reactive$open_canal <- input$open_canal
    
    #######
    revision_implants_df <- left_revision_implants_reactive_list()$revision_implants_status_df %>%
      bind_rows(right_revision_implants_reactive_list()$revision_implants_status_df)
    
    posterior_op_note_inputs_list_reactive$revision_implants_df <- revision_implants_df
    
    # RODS
    ####### Left
    posterior_op_note_inputs_list_reactive$left_main_rod_size <- input$left_main_rod_size
    posterior_op_note_inputs_list_reactive$left_main_rod_material <- input$left_main_rod_material
    
    if(length(input$left_main_rod_contour)>0){
      posterior_op_note_inputs_list_reactive$left_main_rod_contour <- case_when(
        input$left_main_rod_contour == "Pre-contoured" ~ "pre-contoured rod was selected",
        input$left_main_rod_contour == "Cut to length and contoured" ~ "rod was cut to the appropriate length and contoured",
      )
    }else{
      posterior_op_note_inputs_list_reactive$left_main_rod_contour <- ""
    }
    
    ####### Right
    posterior_op_note_inputs_list_reactive$right_main_rod_material <- input$right_main_rod_material
    posterior_op_note_inputs_list_reactive$right_main_rod_size <- input$right_main_rod_size
    
    if(length(input$right_main_rod_contour)>0){
      posterior_op_note_inputs_list_reactive$right_main_rod_contour <- case_when(
        input$right_main_rod_contour == "Pre-contoured" ~ "pre-contoured rod was selected",
        input$right_main_rod_contour == "Cut to length and contoured" ~ "rod was cut to the appropriate length and contoured",
      )
    }else{
      posterior_op_note_inputs_list_reactive$right_main_rod_contour <- ""
    }
    
    
    #######
    posterior_op_note_inputs_list_reactive$added_rods_statement <- added_rods_statement_reactive()
    
    #######
    posterior_op_note_inputs_list_reactive$preop_antibiotics <- jh_replace_checkbox_other_with_text_function(input_vector = input$preop_antibiotics, 
                                                                                                             replacement_text = input$preop_antibiotics_other)
    
    
    ####### Antifibrinolytic
    if(length(input$anti_fibrinolytic)>0 & any(input$anti_fibrinolytic == "Tranexamic Acid (TXA)")){
      if(input$txa_maintenance > 0){
        maintenance_gtt <- glue(" with a {input$txa_maintenance}mg/kg/hr maintenance drip.")
      }else{
        maintenance_gtt <- "."
      }
      posterior_op_note_inputs_list_reactive$antifibrinolytic <- glue("A loading dose of {input$txa_loading}mg/kg of TXA was administered for an antifibrinolytic{maintenance_gtt}")
    }else{
      posterior_op_note_inputs_list_reactive$antifibrinolytic <- " "
    }
    
    #######

    if(length(input$additional_procedures_posterior)>0){
      if("Other" %in% input$additional_procedures_posterior){
        posterior_op_note_inputs_list_reactive$additional_procedures_vector  <- append(discard(input$additional_procedures_posterior, .p = ~ (.x == "Other")), paste(input$additional_procedures_other_posterior))
      }else{
        posterior_op_note_inputs_list_reactive$additional_procedures_vector <- input$additional_procedures_posterior
      }
      
    }else{
      posterior_op_note_inputs_list_reactive$additional_procedures_vector <-  c()
    }
    
    #######
    posterior_op_note_inputs_list_reactive$prior_fusion_levels <- input$prior_fusion_levels
    
    #######
    posterior_op_note_inputs_list_reactive$instrumentation_removed_vector <- unique(c(input$left_revision_implants_removed, input$right_revision_implants_removed))
    
    ####### BONE GRAFT AND BMP ##########
    posterior_op_note_inputs_list_reactive$posterior_bmp_dose_reactive <- posterior_bmp_dose_reactive()
    
    #######
    biologics_generate_list_item_function <- function(biologic_volume, biologic_label = "graft"){
      if(biologic_volume > 0){
        biologic <- paste0(biologic_volume, "cc of ", biologic_label)
      }else{
        biologic<- NULL
      }
      biologic
    } 
    posterior_biologics_list <- list()
    posterior_biologics_list$posterior_allograft <- biologics_generate_list_item_function(biologic_volume = input$posterior_allograft_amount, biologic_label = "morselized allograft")
    posterior_biologics_list$posterior_bone_marrow_aspirate <- biologics_generate_list_item_function(biologic_volume = input$posterior_bone_marrow_aspirate_volume, biologic_label = "bone marrow aspirate")
    posterior_biologics_list$posterior_cell_based_allograft <- biologics_generate_list_item_function(biologic_volume = input$posterior_cell_based_allograft_volume, biologic_label = "cell-based allograft")
    posterior_biologics_list$posterior_dbm <- biologics_generate_list_item_function(biologic_volume = input$posterior_dbm_volume, biologic_label = "demineralized bone matrix")
    posterior_biologics_list$posterior_ifactor <- biologics_generate_list_item_function(biologic_volume = input$posterior_ifactor_volume, biologic_label = "iFactor")
    if(any(input$posterior_biologics == "Other")){
      posterior_biologics_list$other <- paste(input$posterior_biologics_other)
    }
    
    if(any(input$posterior_bone_graft == "Local Autograft")){
      posterior_biologics_list$autograft <- "morselized local autograft"
    }
    
    posterior_op_note_inputs_list_reactive$posterior_biologics_list <- posterior_biologics_list
    
    #######
    if(any(input$posterior_bone_graft == "Morselized Autograft (separate fascial incision)")){
      posterior_op_note_inputs_list_reactive$morselized_autograft_separate <- TRUE
    }else{
      posterior_op_note_inputs_list_reactive$morselized_autograft_separate <- FALSE
    }
    
    ####### DRAINS AND DRESSING AND CLOSURE #########
    posterior_op_note_inputs_list_reactive$deep_drains_posterior <- input$deep_drains_posterior
    
    #######
    posterior_op_note_inputs_list_reactive$superficial_drains_posterior <- input$superficial_drains_posterior
    
    #######
    posterior_op_note_inputs_list_reactive$additional_end_procedure_details <- input$additional_end_procedure_details_posterior
    
    #######
    posterior_op_note_inputs_list_reactive$closure_details <- input$closure_details_posterior
    
    #######
    posterior_op_note_inputs_list_reactive$dressing_details <- input$dressing_details_posterior
    
    ####### MULTIPLE APPROACH
    if(input$approach_sequence == "anterior" | input$approach_sequence == "posterior"){
      posterior_op_note_inputs_list_reactive$multiple_approach <- "NA"
    }
    if(input$approach_sequence == "posterior-anterior" | input$approach_sequence == "posterior-anterior-posterior"){
      posterior_op_note_inputs_list_reactive$multiple_approach <- "posterior_first"
    }
    if(input$approach_sequence == "anterior-posterior"){
      posterior_op_note_inputs_list_reactive$multiple_approach <- "anterior_first"
    }

    
    #######
    if(any(input$alignment_correction_method == "Other")){
      posterior_op_note_inputs_list_reactive$alignment_correction_method <- gsub("Other", input$alignment_correction_method_other, input$alignment_correction_method, ignore.case = TRUE)
    }else{
      if(length(input$alignment_correction_method)>0){
        posterior_op_note_inputs_list_reactive$alignment_correction_method <- input$alignment_correction_method
      }else{
        posterior_op_note_inputs_list_reactive$alignment_correction_method <- "The rods were set into place and secured with set screws. "
      }
      
    }
    
    #######
    posterior_op_note_inputs_list_reactive$sex <- input$sex
    
    #######
    if(length(input$lateral_mass_screws_after_decompression) == 0){
      lateral_mass_screws_after_decompression <- "No"
    }else{
      lateral_mass_screws_after_decompression <- paste(input$lateral_mass_screws_after_decompression)
    }
    
    posterior_op_note_inputs_list_reactive$lateral_mass_screws_after_decompression <- lateral_mass_screws_after_decompression
    
    #### attending assistant
    if(input$attending_assistant_yes_no == TRUE){
      posterior_op_note_inputs_list_reactive$attending_assistant <- input$attending_assistant
    }else{
      posterior_op_note_inputs_list_reactive$attending_assistant <- " "
    }
    
    # posterior_op_note_inputs_list_reactive$additional_procedures_vector
    ### PROCEDURES PERFORMED 
    posterior_op_note_inputs_list_reactive$procedures_performed_sentence <- glue_collapse(str_to_lower(gsub("^\\d+\\.\\s+", "", str_split(input$procedures_numbered_confirm_edit_posterior, pattern = "\n")[[1]])), sep = ", ", last = ", and ")
    
  }

    posterior_op_note_inputs_list_reactive
  }) 
  
  ######################################### NOW ALL THE NECESSARY INPUTS FOR THE POSTERIOR OP NOTE ARE GENERATED
  
  ###################### NOW GENERATE ALL THE NECESSARY INPUTS FOR THE THE ANTERIOR OP NOTE
  anterior_op_note_inputs_list_reactive <- reactive({
    anterior_op_note_inputs_list_reactive <- list()
    
    ######
    if(input$implants_complete > 0){
    
    anterior_approach_objects_df <- all_objects_to_add_list$objects_df %>%
      filter(approach == "anterior")  %>%
      select(-object_constructed)
    
    
    # screw_size_details_df_reactive()
    if(any(str_detect(screw_size_details_df_reactive()$object, "anterior"))){
      
      # names include: level, screw_implant, screw_side, screw_diameter, screw_length, screw_type, screw_size_type
      
      anterior_plate_screws_objects_df <- screw_size_details_df_reactive() %>%
        filter(approach == "anterior") %>%
        mutate(approach = "anterior", 
               category = "anterior_disc", 
               implant = "yes"
               # object = "anterior_plate_screw"
               ) %>%
        mutate(vertebral_number = jh_get_vertebral_number_function(level_to_get_number = level))  %>%
        select(level, approach, category, vertebral_number, implant, object, side, screw_size_type)
      
      anterior_approach_objects_df <- anterior_approach_objects_df %>%
        bind_rows(anterior_plate_screws_objects_df) %>%
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
    
    anterior_op_note_inputs_list_reactive$anterior_approach_objects_df <- anterior_approach_objects_df
    
    ##revision implants ##
    
    if(nrow(anterior_plate_revision_implants_df_reactive())>0){
      anterior_op_note_inputs_list_reactive$anterior_plate_revision_df <- anterior_plate_revision_implants_df_reactive() %>% select(level, prior_plate_status) 
    }else{
      anterior_op_note_inputs_list_reactive$anterior_plate_revision_df <- tibble(level = character(), prior_plate_status = character())
    }
    
    #######
    if(!is.null(input$approach_specified_anterior)){
      anterior_op_note_inputs_list_reactive$head_positioning_anterior <- input$head_positioning_anterior
    }else{
      anterior_op_note_inputs_list_reactive$head_positioning_anterior <- " "
    }
    
    #######
    if(!is.null(input$approach_specified_anterior)){
      anterior_op_note_inputs_list_reactive$anterior_approach_laterality <- input$approach_specified_anterior
    }else{
      anterior_op_note_inputs_list_reactive$anterior_approach_laterality <- " "
    }
    
    
    
    #######
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
    
    anterior_op_note_inputs_list_reactive$approach_statement <- anterior_cervical_approach_details
    
    #######
    anterior_op_note_inputs_list_reactive$preop_antibiotics <- jh_replace_checkbox_other_with_text_function(input_vector = input$preop_antibiotics, 
                                                                                                             replacement_text = input$preop_antibiotics_other)

    ####### Antifibrinolytic
    if(length(input$anti_fibrinolytic)>0 & any(input$anti_fibrinolytic == "Tranexamic Acid (TXA)")){ 
      if(input$txa_maintenance > 0){
        maintenance_gtt <- glue(" with a {input$txa_maintenance}mg/kg/hr maintenance drip.")
      }else{
        maintenance_gtt <- "."
      }
      anterior_op_note_inputs_list_reactive$antifibrinolytic <- glue("A loading dose of {input$txa_loading}mg/kg of TXA was administered for an antifibrinolytic{maintenance_gtt}")
    }else{
      anterior_op_note_inputs_list_reactive$antifibrinolytic <- " "
    }
    
    ####### ADDITIONAL PROCEDURES

    anterior_op_note_inputs_list_reactive$additional_procedures_vector <- additional_anterior_procedures_vector_for_op_note_reactive() #input$additional_procedures_anterior
    
    #######
    neuromonitoring_input_list <- list()
    neuromonitoring_input_list$modalities <- input$neuromonitoring
    neuromonitoring_input_list$emg <- if_else(input$triggered_emg == "No", "", paste0(input$triggered_emg))
    
    # neuromonitoring_input_list$pre_positioning_motors <- if_else(input$pre_positioning_motors == "Pre-positioning motors not obtained", "", input$pre_positioning_motors)
    if(any(input$pre_positioning_motors == "Not obtained")){
      neuromonitoring_input_list$pre_positioning_motors <- ""
    }else{
      neuromonitoring_input_list$pre_positioning_motors <-  paste0("Pre-positioning motors were ", str_to_lower(glue_collapse(input$pre_positioning_motors, sep = ", ", last = " and ")), ".")
    }
    
    neuromonitoring_input_list$neuromonitoring_signal_stability <- if_else(str_detect(string = paste(input$neuromonitoring, collapse = ", "), pattern = "SSEP"),
                                                                           as.character(input$neuromonitoring_signal_stability),
                                                                           " ")    
    
    anterior_op_note_inputs_list_reactive$neuromonitoring_list <- neuromonitoring_input_list
    
    #######
    anterior_op_note_inputs_list_reactive$local_anesthesia <- input$local_anesthesia
    
    #######
    anterior_op_note_inputs_list_reactive$bmp <- anterior_bmp_dose_reactive()
    
    #######
    anterior_biologics_list <- list()
    
    
    number_of_fusion_levels <- nrow(anterior_approach_objects_df %>% filter(fusion == "yes") %>% select(level) %>% distinct())
    
    if(any(input$anterior_bone_graft == "Morselized Allograft")){
      anterior_biologics_list$'Morselized Allograft' <- round(input$anterior_allograft_amount/number_of_fusion_levels, 1)
    }else{
      anterior_biologics_list$'Morselized Allograft' <- 0
    }
    if(any(input$anterior_biologics == "Bone Marrow Aspirate")){
      anterior_biologics_list$'Bone Marrow Aspirate' <- round(input$anterior_bone_marrow_aspirate_volume/number_of_fusion_levels, 1) 
    }else{
      anterior_biologics_list$'Bone Marrow Aspirate' <- 0
    }
    if(any(input$anterior_biologics == "Cell Based Allograft")){
      anterior_biologics_list$'Cell Based Allograft' <- round(input$anterior_cell_based_allograft_volume/number_of_fusion_levels, 1)
    }else{
      anterior_biologics_list$'Cell Based Allograft' <- 0
    }
    if(any(input$anterior_biologics == "DBM")){
      anterior_biologics_list$'DBM' <- round(input$anterior_dbm_volume/number_of_fusion_levels, 1) 
    }else{
      anterior_biologics_list$'DBM' <- 0
    }
    if(any(input$anterior_biologics == "iFactor")){
      anterior_biologics_list$'iFactor' <-  round(input$anterior_ifactor_volume/number_of_fusion_levels, 1) 
    }else{
      anterior_biologics_list$'iFactor' <- 0
    }
    if(any(input$anterior_biologics == "Other")){
      anterior_biologics_list$'Other' <-  999
    }else{
      anterior_biologics_list$'Other' <- 0
    }
    
    anterior_biologics_df_formatted <- enframe(anterior_biologics_list) %>%
      unnest(value) %>%
      filter(value != 0) %>%
      mutate(name = str_replace_all(name, "Other", input$anterior_biologics_other))
    
    anterior_op_note_inputs_list_reactive$anterior_biologics_df <- anterior_biologics_df_formatted
    
    #######
    anterior_op_note_inputs_list_reactive$bone_graft_vector <- input$anterior_bone_graft
    
    #######
    anterior_op_note_inputs_list_reactive$morselized_allograft <- input$anterior_allograft_amount
    
    #######
    anterior_op_note_inputs_list_reactive$deep_drains_anterior <- input$deep_drains_anterior
    
    #######
    anterior_op_note_inputs_list_reactive$superficial_drains_anterior <- input$superficial_drains_anterior
    
    #######
    anterior_op_note_inputs_list_reactive$additional_end_procedure_details <- input$additional_end_procedure_details_anterior
    
    #######
    anterior_op_note_inputs_list_reactive$closure_details <- input$closure_details_anterior
    
    #######
    anterior_op_note_inputs_list_reactive$dressing_details <- input$dressing_details_anterior
    
    #######
    ####### MULTIPLE APPROACH
    if(input$approach_sequence == "posterior-anterior" | input$approach_sequence == "posterior-anterior-posterior"){
      anterior_op_note_inputs_list_reactive$multiple_approach <- "posterior_first"
    }
    if(input$approach_sequence == "anterior-posterior"){
      anterior_op_note_inputs_list_reactive$multiple_approach <- "anterior_first"
    }
    if(input$approach_sequence == "anterior" | input$approach_sequence == "posterior"){
      anterior_op_note_inputs_list_reactive$multiple_approach <- "Not Applicable"
    }
    
    ### PROCEDURES PERFORMED 
    anterior_op_note_inputs_list_reactive$procedures_performed_sentence <- glue_collapse(str_to_lower(gsub("^\\d+\\.\\s+", "", str_split(input$procedures_numbered_confirm_edit_anterior, pattern = "\n")[[1]])), sep = ", ", last = ", and ")
    
    #######
    anterior_op_note_inputs_list_reactive$sex <- input$sex
    
    }
    
    anterior_op_note_inputs_list_reactive
  })
  

  ########### NOW ASSEMBLE THE REACTIVE TEXT OF THE ENTIRE OP NOTE ###############
  
  approach_sequence_reactive <- reactive({
    if(nrow(all_objects_to_add_list$objects_df)>0){
      approach_sequence <- glue_collapse(head(unique(all_objects_to_add_list$objects_df$approach), 3), sep = "-")
    }else{
      approach_sequence <- "posterior"
    }
    approach_sequence
  }) %>%
    bindEvent(input$spine_approach,
              input$add_rods,
              input$implants_complete,
              all_objects_to_add_list$objects_df,
              # ignoreInit = TRUE,
              ignoreNULL = TRUE
    )

  observe(
    updatePickerInput(session = session, 
                            inputId = "approach_sequence", 
                            selected = paste0(approach_sequence_reactive()))
  )%>%
    bindEvent(approach_sequence_reactive(),
              ignoreInit = TRUE,
              ignoreNULL = TRUE
    )
  
  procedure_approach_reactive <- reactive({
    case_when(input$approach_sequence == "anterior" ~ "anterior", 
              input$approach_sequence == "posterior" ~ "posterior",
              TRUE ~ "combined")
  }) %>%
    bindEvent(input$approach_sequence,
              ignoreInit = TRUE,
              ignoreNULL = TRUE
    )

  ##### Assemble and update the procedures performed section for POSTERIOR #########
  
  op_note_procedures_numbered_posterior_reactive <- reactive({
    if(str_detect(input$approach_sequence, "posterior") && input$implants_complete > 0){
      primary_procedures_numbered_paragraph <- op_note_procedures_performed_numbered_function(objects_added_df = posterior_op_note_inputs_list_reactive()$posterior_approach_objects_df, 
                                                                                              revision_implants_df = posterior_op_note_inputs_list_reactive()$revision_implants_df, 
                                                                                              revision_decompression_vector = posterior_op_note_inputs_list_reactive()$open_canal, 
                                                                                              fusion_levels_vector = posterior_op_note_inputs_list_reactive()$fusions_df$level, 
                                                                                              additional_procedures_performed_vector = posterior_op_note_inputs_list_reactive()$additional_procedures_vector
      )
    }else{
      primary_procedures_numbered_paragraph <- c()
    }
  })
  

  
  observe(
    updateTextAreaInput(session = session, 
                        inputId = "procedures_numbered_confirm_edit_posterior", 
                        value = paste(op_note_procedures_numbered_posterior_reactive()
                                      )
    )
  ) %>%
    bindEvent(input$editing_additional_surgical_details_1_complete,
              input$additional_surgical_details_complete, 
              # input$operative_note,
              input$additional_procedures_other_posterior,
              ignoreInit = TRUE,
              ignoreNULL = TRUE
    )


  ##### Assemble and update the procedures performed section for ANTERIOR #########
  
  op_note_procedures_numbered_anterior_reactive <- reactive({
    if(nrow(anterior_op_note_inputs_list_reactive()$anterior_approach_objects_df) >0 && input$implants_complete > 0){
      primary_procedures_numbered_paragraph_anterior <- anterior_op_note_procedures_performed_numbered_function(objects_added_df = anterior_op_note_inputs_list_reactive()$anterior_approach_objects_df, 
                                                                                                                additional_procedures_performed_vector = anterior_op_note_inputs_list_reactive()$additional_procedures_vector)
    }else{
      primary_procedures_numbered_paragraph_anterior <- c()
    }
    
    primary_procedures_numbered_paragraph_anterior
  }) 
  
  observe(
    updateTextAreaInput(session = session, 
                        inputId = "procedures_numbered_confirm_edit_anterior", 
                        value = paste(op_note_procedures_numbered_anterior_reactive())
    )
  ) %>%
    bindEvent(input$editing_additional_surgical_details_1_complete, ### this is only the editing box
              input$additional_surgical_details_complete, 
              # input$operative_note,
              input$additional_procedures_other_anterior,
              ignoreInit = TRUE,
              ignoreNULL = TRUE
    )
  
  
  ### ASSEMBLE THE REACTIVE OP NOTE TEXT
  # op_note_text_reactive <
  
  ### ASSEMBLE THE REACTIVE OP NOTE TEXT
  op_note_text_reactive <- reactive({
    
    revision_implants_df <- left_revision_implants_reactive_list()$revision_implants_status_df %>%
      bind_rows(right_revision_implants_reactive_list()$revision_implants_status_df)
    
    ################# COMPLICATIONS ################3
    complication_df <- tibble(complication = append(input$intraoperative_complications_vector, input$other_intraoperative_complications)) %>%
      filter(complication != "") %>%
      filter(complication != " ") %>%
      filter(complication != "Other") %>%
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
    
    op_note_list <- list()
    op_note_list$"OPERATIVE REPORT" <- paste(" ")
    op_note_list$"*Patient:" <- paste(input$patient_first_name, input$patient_last_name)
    op_note_list$"\n*Date of Surgery:" <- as.character(input$date_of_surgery)
    op_note_list$"\n*Primary Surgeon:" <- paste(input$primary_surgeon_first_name, input$primary_surgeon_last_name)
    if(input$cosurgeon_yes_no == TRUE){
      op_note_list$'\n*Co-Surgeon:' <- input$cosurgeon 
    }
    if(input$attending_assistant_yes_no == TRUE){
      op_note_list$'\nSurgical Assistants:' <- paste(as.character(input$attending_assistant), 
                                                     if_else(!is.null(input$surgical_assistants), 
                                                             as.character(input$surgical_assistants),
                                                             " "),
                                                     sep = "; ")
    }else{
      op_note_list$"\nSurgical Assistants:" <- if_else(!is.null(input$surgical_assistants), as.character(input$surgical_assistants), " ") 
    }
    
    op_note_list$"\n*Preprocedure ASA Class:" <- input$asa_class
    op_note_list$"\n*Anesthesia:" <- input$anesthesia
    op_note_list$"\n*Intraoperative Complications:" <- complication_statement
    op_note_list$"\n*Pre-operative Diagnosis:" <- glue_collapse(unlist(str_split(paste0("  -", input$preoperative_diagnosis), pattern = "; ")), sep = "\n  -")
    op_note_list$"\n*Post-operative Diagnosis:" <- glue_collapse(unlist(str_split(paste0("  -", input$postoperative_diagnosis), pattern = "; ")), sep = "\n  -")
    op_note_list$"\n*Indications:" <- input$indications
    op_note_list$"\n*Estimated Blood Loss:" <- if_else(is.na(input$ebl), "See anesthesia records", paste0(input$ebl, "cc"))
    op_note_list$"\n*Fluids/Transfusions:" <- fluids_transfusions_statement
    op_note_list$"\n*Surgical Findings:" <- if_else(input$surgical_findings == "", "none", input$surgical_findings)
    op_note_list$"\n*Specimens Taken:" <- if_else(input$specimens_removed == "", "none", input$specimens_removed)

    procedure_results_list <- list()
    procedure_results_list_posterior <- list()
    procedure_results_list_posterior$procedure_details_paragraph <- ""
    procedure_results_list_posterior$procedures_numbered_paragraph <- ""
    procedure_results_list_anterior <- list()
    procedure_results_list_anterior$procedure_details_paragraph <- ""
    procedure_results_list_anterior$procedures_numbered_paragraph <- ""
    
    posterior_approach_objects_df <- all_objects_to_add_list$objects_df %>%
      filter(approach == "posterior")  %>%
      select(-object_constructed) 
    
    posterior_procedures_count <-(nrow(all_objects_to_add_list$objects_df %>% filter(approach == "posterior")) + nrow(revision_implants_df))
    
    if(posterior_procedures_count>0){
      procedure_results_list_posterior <- op_note_posterior_function(all_objects_to_add_df = posterior_op_note_inputs_list_reactive()$posterior_approach_objects_df,
                                                                         fusion_levels_df = posterior_op_note_inputs_list_reactive()$fusions_df,
                                                                         c2_nerve_transection = posterior_op_note_inputs_list_reactive()$c2_nerve_transection,
                                                                         head_position = posterior_op_note_inputs_list_reactive()$head_positioning_posterior,
                                                                         surgical_approach = posterior_op_note_inputs_list_reactive()$approach_specified_posterior,
                                                                         approach_mis_open = posterior_op_note_inputs_list_reactive()$approach_open_mis,
                                                                         approach_microscope = posterior_op_note_inputs_list_reactive()$approach_microscope,
                                                                         approach_robot_nav_xray = posterior_op_note_inputs_list_reactive()$approach_robot_navigation,
                                                                         neuromonitoring_list = posterior_op_note_inputs_list_reactive()$neuromonitoring_input_list, ## this is a named list with names: modalities, emg, and pre_positioning_motors
                                                                         implant_start_point_method_input = posterior_op_note_inputs_list_reactive()$implant_start_point_method,
                                                                         implant_confirmation_method = posterior_op_note_inputs_list_reactive()$implant_position_confirmation_method,
                                                                         local_anesthesia = posterior_op_note_inputs_list_reactive()$local_anesthesia,
                                                                         complications_list = posterior_op_note_inputs_list_reactive()$complications_list,
                                                                         revision_decompression_vector = posterior_op_note_inputs_list_reactive()$open_canal,
                                                                         revision_implants_df = posterior_op_note_inputs_list_reactive()$revision_implants_df,
                                                                         left_main_rod_size = posterior_op_note_inputs_list_reactive()$left_main_rod_size,
                                                                         left_main_rod_material = posterior_op_note_inputs_list_reactive()$left_main_rod_material,
                                                                         left_main_rod_contour = posterior_op_note_inputs_list_reactive()$left_main_rod_contour,
                                                                         right_main_rod_size = posterior_op_note_inputs_list_reactive()$right_main_rod_size,
                                                                         right_main_rod_material = posterior_op_note_inputs_list_reactive()$right_main_rod_material,
                                                                         right_main_rod_contour = posterior_op_note_inputs_list_reactive()$right_main_rod_contour,
                                                                         additional_rods_statement = posterior_op_note_inputs_list_reactive()$added_rods_statement,
                                                                         antibiotics = posterior_op_note_inputs_list_reactive()$preop_antibiotics,
                                                                         antifibrinolytic = posterior_op_note_inputs_list_reactive()$antifibrinolytic,
                                                                         additional_procedures_vector = posterior_op_note_inputs_list_reactive()$additional_procedures_vector,
                                                                         prior_fusion_levels_vector = posterior_op_note_inputs_list_reactive()$prior_fusion_levels,
                                                                         instrumentation_removal_vector = posterior_op_note_inputs_list_reactive()$instrumentation_removed_vector,
                                                                         bmp = posterior_op_note_inputs_list_reactive()$posterior_bmp_dose_reactive,
                                                                         biologics_list = posterior_op_note_inputs_list_reactive()$posterior_biologics_list,
                                                                         morselized_autograft_separate = posterior_op_note_inputs_list_reactive()$morselized_autograft_separate,
                                                                         deep_drains = posterior_op_note_inputs_list_reactive()$deep_drains_posterior,
                                                                         superficial_drains = posterior_op_note_inputs_list_reactive()$superficial_drains_posterior,
                                                                         end_procedure_details = posterior_op_note_inputs_list_reactive()$additional_end_procedure_details,
                                                                         closure = posterior_op_note_inputs_list_reactive()$closure_details,
                                                                         dressing = posterior_op_note_inputs_list_reactive()$dressing_details,
                                                                         multiple_position_procedure = posterior_op_note_inputs_list_reactive()$multiple_approach,
                                                                         alignment_correction_technique = posterior_op_note_inputs_list_reactive()$alignment_correction_method,
                                                                         sex = posterior_op_note_inputs_list_reactive()$sex,
                                                                         lateral_mass_screws_after_decompression = posterior_op_note_inputs_list_reactive()$lateral_mass_screws_after_decompression,
                                                                         instruments_used_for_bony_work = posterior_op_note_inputs_list_reactive()$instruments_used_for_bony_work,
                                                                         attending_assistant = posterior_op_note_inputs_list_reactive()$attending_assistant,
                                                                         procedures_performed_sentence =  posterior_op_note_inputs_list_reactive()$procedures_performed_sentence
      )

    }
    
    if(nrow(anterior_op_note_inputs_list_reactive()$anterior_approach_objects_df) > 0){
      
      procedure_results_list_anterior <- op_note_anterior_function(all_objects_to_add_df = anterior_op_note_inputs_list_reactive()$anterior_approach_objects_df, 
                                                                   anterior_plate_revision_df = anterior_op_note_inputs_list_reactive()$anterior_plate_revision_df,
                                                                   anterior_approach_laterality = anterior_op_note_inputs_list_reactive()$anterior_approach_laterality,
                                                                   head_position = anterior_op_note_inputs_list_reactive()$head_positioning_anterior,
                                                                   approach_statement = anterior_op_note_inputs_list_reactive()$approach_statement,
                                                                   antibiotics = anterior_op_note_inputs_list_reactive()$preop_antibiotics,
                                                                   antifibrinolytic = anterior_op_note_inputs_list_reactive()$antifibrinolytic,
                                                                   additional_procedures_vector = anterior_op_note_inputs_list_reactive()$additional_procedures_vector,
                                                                   neuromonitoring_list = anterior_op_note_inputs_list_reactive()$neuromonitoring_list,
                                                                   local_anesthesia = anterior_op_note_inputs_list_reactive()$local_anesthesia,
                                                                   bmp = anterior_op_note_inputs_list_reactive()$bmp,
                                                                   anterior_biologics_df = anterior_op_note_inputs_list_reactive()$anterior_biologics_df,
                                                                   bone_graft_vector = anterior_op_note_inputs_list_reactive()$bone_graft_vector,
                                                                   morselized_allograft = anterior_op_note_inputs_list_reactive()$morselized_allograft,
                                                                   deep_drains = anterior_op_note_inputs_list_reactive()$deep_drains_anterior,
                                                                   superficial_drains = anterior_op_note_inputs_list_reactive()$superficial_drains_anterior,
                                                                   end_procedure_details = anterior_op_note_inputs_list_reactive()$additional_end_procedure_details,
                                                                   closure = anterior_op_note_inputs_list_reactive()$closure_details,
                                                                   dressing = anterior_op_note_inputs_list_reactive()$dressing_details,
                                                                   multiple_position_procedure = anterior_op_note_inputs_list_reactive()$multiple_approach,
                                                                   sex = anterior_op_note_inputs_list_reactive()$sex,
                                                                   procedures_performed_sentence = anterior_op_note_inputs_list_reactive()$procedures_performed_sentence
      )
    }
    
    

    
    procedure_results_list$procedure_details_paragraph <- case_when(
      input$approach_sequence == "posterior" ~ glue("{procedure_results_list_posterior$procedure_details_paragraph}"),
      input$approach_sequence == "anterior" ~ glue("{procedure_results_list_anterior$procedure_details_paragraph}"),
      input$approach_sequence == "posterior-anterior" ~  glue("Posterior:\n{procedure_results_list_posterior$procedure_details_paragraph} \n\nWe then turned to the anterior portion of the case.\n\n{procedure_results_list_anterior$procedure_details_paragraph}"),
      input$approach_sequence == "anterior-posterior" ~ glue("Anterior:\n{procedure_results_list_anterior$procedure_details_paragraph} \n\nWe then turned to the posterior portion of the case.\n\n{procedure_results_list_posterior$procedure_details_paragraph}"),
      input$approach_sequence == "posterior-anterior-posterior" ~ glue("Posterior:\n{procedure_results_list_posterior$procedure_details_paragraph} \n\nWe then turned to the anterior portion of the case.\n\n{procedure_results_list_anterior$procedure_details_paragraph}")
    )
    
    procedure_results_list$procedures_numbered_paragraph <- case_when(
      input$approach_sequence == "posterior" ~ glue("{input$procedures_numbered_confirm_edit_posterior}"),
      input$approach_sequence == "anterior" ~ glue("{input$procedures_numbered_confirm_edit_anterior}"),
      input$approach_sequence == "posterior-anterior" ~ glue("Posterior:\n{procedure_results_list_posterior$procedures_numbered_paragraph} \n\nAnterior:\n{procedure_results_list_anterior$procedures_numbered_paragraph}"),
      input$approach_sequence == "anterior-posterior" ~ glue("Anterior:\n{procedure_results_list_anterior$procedures_numbered_paragraph} \n\nPosterior:\n{procedure_results_list_posterior$procedures_numbered_paragraph}"),
      input$approach_sequence == "posterior-anterior-posterior" ~ glue("Posterior:\n{procedure_results_list_posterior$procedures_numbered_paragraph} \n\nAnterior:\n{procedure_results_list_anterior$procedures_numbered_paragraph}")
    )
    
    op_note_list$"\n*Procedures Performed:" <- procedure_results_list$procedures_numbered_paragraph
    
    if(length(input$implant_manufacturer)>0){
      op_note_list$"\n*Implant Manufacturer:" <- paste(glue_collapse(x = input$implant_manufacturer, sep = ", ", last = " and "))
    }
    
    if(nrow(all_objects_to_add_list$objects_df) == 0 & nrow(revision_implants_df) == 0){
      
      if(any(any(str_detect(posterior_op_note_inputs_list_reactive()$additional_procedures_vector, "Halo")), 
             any(str_detect(anterior_op_note_inputs_list_reactive()$additional_procedures_vector, "Halo")))
      ){
        procedure_results_list <- op_note_posterior_function(local_anesthesia = input$local_anesthesia,
                                                                       antibiotics = jh_replace_checkbox_other_with_text_function(input_vector = input$preop_antibiotics, 
                                                                                                                                  replacement_text = input$preop_antibiotics_other),
                                                                       additional_procedures_vector = posterior_op_note_inputs_list_reactive()$additional_procedures_vector,
        )
        
        op_note_list$"\n*Procedure  Description:" <-  procedure_results_list$procedure_details_paragraph
        
      }else{
        op_note_list$"\n*Procedure  Description:" <- "***"
      }
      
      }else{
      
      op_note_list$"\n*Procedure  Description:" <- procedure_results_list$procedure_details_paragraph
      
    }
    op_note_list$"\n*Postop Plan:" <- if_else(length(postop_plan_list_reactive()) >0, glue_collapse(postop_plan_list_reactive(), sep = "\n"), " ")
    

    secion_headers_df <- enframe(op_note_list, name = "section", value = "result") %>%
      unnest(result) %>%
      mutate(row = row_number()) %>%
      pivot_longer(cols = c(section, result), names_to = "text", values_to = "full_text_vector") %>%
      select(row, full_text_vector)
    
    op_note_text <- glue_collapse(secion_headers_df$full_text_vector, sep = "\n")
 
    op_note_text
  })
  
  

  
  observeEvent(input$generate_operative_note, {

    updateTextAreaInput(session = session,
                        inputId = "operative_note_text",
                        value = HTML(op_note_text_reactive())
                        # value = HTML(op_note_text_reactive_testing())
                        )

  }
  )
  
  observeEvent(input$operative_note_text, ignoreNULL = TRUE, {

    output$operative_note_formatted <-  renderText({

      op_note <- input$operative_note_text

      op_note <- str_replace_all(string = op_note, pattern = "\\n\\n", replacement = "<br><br>")
      op_note <- str_replace_all(string = op_note, pattern = "\\n", replacement = "<br>")

      op_note <- str_replace_all(string = op_note, pattern = "OPERATIVE REPORT", replacement = "<u><B>OPERATIVE REPORT</B></u>")
      op_note <- str_replace_all(string = op_note, pattern = "Patient:", replacement = "<B>Patient:</B>")
      op_note <- str_replace_all(string = op_note, pattern = "Date of Surgery:", replacement = "<B>Date of Surgery:</B>")
      op_note <- str_replace_all(string = op_note, pattern = "Primary Surgeon:", replacement = "<B>Primary Surgeon:</B>")
      op_note <- str_replace_all(string = op_note, pattern = "Co-Surgeon:", replacement = "<B>Co-Surgeon:</B>")
      op_note <- str_replace_all(string = op_note, pattern = "Surgical Assistants:", replacement = "<B>Surgical Assistants:</B>")
      op_note <- str_replace_all(string = op_note, pattern = "Preprocedure ASA Class:", replacement = "<B>Preprocedure ASA Class:</B>")
      op_note <- str_replace_all(string = op_note, pattern = "Anesthesia:", replacement = "<B>Anesthesia:</B>")
      op_note <- str_replace_all(string = op_note, pattern = "Intraoperative Complications:", replacement = "<B>Intraoperative Complications:</B>")
      op_note <- str_replace_all(string = op_note, pattern = "Pre-operative Diagnosis:", replacement = "<B>Pre-operative Diagnosis:</B>")
      op_note <- str_replace_all(string = op_note, pattern = "Post-operative Diagnosis:", replacement = "<B>Post-operative Diagnosis:</B>")
      op_note <- str_replace_all(string = op_note, pattern = "Indications:", replacement = "<B>Indications:</B>")
      op_note <- str_replace_all(string = op_note, pattern = "Estimated Blood Loss:", replacement = "<B>Estimated Blood Loss:</B>")
      op_note <- str_replace_all(string = op_note, pattern = "Fluids/Transfusions:", replacement = "<B>Fluids/Transfusions:</B>")
      op_note <- str_replace_all(string = op_note, pattern = "Surgical Findings:", replacement = "<B>Surgical Findings:</B>")
      op_note <- str_replace_all(string = op_note, pattern = "Specimens Taken:", replacement = "<B>Specimens Taken:</B>")
      op_note <- str_replace_all(string = op_note, pattern = "Implant Manufacturer:", replacement = "<B>Implant Manufacturer:</B>")
      op_note <- str_replace_all(string = op_note, pattern = "Procedures Performed:", replacement = "<B>Procedures Performed:</B>")
      op_note <- str_replace_all(string = op_note, pattern = "Procedure  Description:", replacement = "<B>Procedure  Description:</B>")
      op_note <- str_replace_all(string = op_note, pattern = "Postop Plan:", replacement = "<br><u><B>POSTOP PLAN:</B></u>")
      # op_note <- str_replace_all(string = op_note, pattern = "xxxx:", replacement = "<B>xxxxx:</B>")

      # postop_plan_sections_list
      for(word in list("Postop Destination",
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
                       "Follow-up")){
        op_note <- str_replace_all(string = op_note, pattern = word, replacement = glue("<em>{word}</em>"))
      }

      op_note <- str_replace_all(string = op_note, pattern = "     > ", replacement = " &emsp; > ")

      # HTML(glue_collapse(op_note, sep = "<br>"))
    })

  })
  
  
  observeEvent(input$generate_operative_note, {
    output$clipboard_ui <- renderUI({
      rclipboard::rclipButton(inputId = "clip_button",
                              label = "Copy To Clipboard",
                              clipText = input$operative_note_text,
                              icon = icon("clipboard"))
    })
  }
  )
  
  # tryCatch({
  #   # Code that might cause the error
  #   observeEvent(input$generate_operative_note, {
  #     output$clipboard_ui <- renderUI({
  #       rclipboard::rclipButton(inputId = "clip_button", 
  #                               label = "Copy To Clipboard", 
  #                               clipText = input$operative_note_text, 
  #                               icon = icon("clipboard"))
  #     })
  #   }
  #   )
  # }, error = function(e) {
  #   # Log the error message along with additional information
  #   log_error_message <- paste("Error occurred:", e$message)
  #   cat(log_error_message, file = "logfile.txt", append = TRUE)
  # })
  
  # observeEvent(input$generate_operative_note, {
  #   output$operative_note_header_details <- renderUI({
  #     HTML(op_note_text_reactive())
  #   })
  # }
  # )
  
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
  
  

  

  
  ###### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Patient Details TABLE ('patient_details' instrument in redcap)  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ######## 
  
  patient_details_redcap_table_reactive <- reactive({
    patient_details_df <- tibble(last_name = input$patient_last_name,
                                 first_name = input$patient_first_name,
                                 date_of_birth = as.character(paste(input$date_of_birth)),
                                 # date_of_birth = if_else(paste(input$date_of_birth) == "1900-01-01", "--", paste(input$date_of_birth)),
                                 sex = input$sex)
    patient_details_df
  })
  
  
  
  ###### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PROCEDURE SUMMARY TABLE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ########
  
  surgical_details_redcap_table_reactive <- reactive({
    if(input$implants_complete>0){
      surgery_details_df <-  redcap_table_surgical_details_df_function(all_objects_df_input = all_objects_to_add_list$objects_df,
                                                                       hospital_input = input$hospital,
                                                                       hospital_mrn_input = input$hospital_mrn,
                                                                       date_of_birth_input = input$date_of_birth,
                                                                       date_of_surgery_input = input$date_of_surgery,
                                                                       primary_surgeon_first_name_input = input$primary_surgeon_first_name,
                                                                       primary_surgeon_last_name_input = input$primary_surgeon_last_name,
                                                                       surgical_assistants_input = input$surgical_assistants,
                                                                       symptoms_input = input$symptoms,
                                                                       primary_diagnosis_input = input$primary_diagnosis,
                                                                       indications_input = input$indications,
                                                                       asa_class_input = input$asa_class,
                                                                       anesthesia_input = input$anesthesia,
                                                                       primary_revision_input = input$primary_revision,
                                                                       revision_indication_input = input$revision_indication,
                                                                       prior_fusion_levels_input = input$prior_fusion_levels,
                                                                       left_revision_implants_removed_input = input$left_revision_implants_removed,
                                                                       right_revision_implants_removed_input = input$right_revision_implants_removed,
                                                                       staged_procedure_input = input$staged_procedure,
                                                                       stage_number = input$stage_number,
                                                                       approach_sequence_input = input$approach_sequence,
                                                                       spine_approach_input = input$spine_approach,
                                                                       approach_specified_anterior_input = input$approach_specified_anterior,
                                                                       approach_specified_posterior_input = input$approach_specified_posterior,
                                                                       posterior_fusion_levels_confirmed_input = input$posterior_fusion_levels_confirmed,
                                                                       anterior_fusion_levels_confirmed_input = input$anterior_fusion_levels_confirmed,
                                                                       left_main_rod_size_input = input$left_main_rod_size,
                                                                       right_main_rod_size_input = input$right_main_rod_size,
                                                                       left_main_rod_material_input = input$left_main_rod_material,
                                                                       right_main_rod_material_input = input$right_main_rod_material,
                                                                       add_left_accessory_rod_input = input$add_left_accessory_rod,
                                                                       add_left_satellite_rod_input = input$add_left_satellite_rod,
                                                                       add_left_intercalary_rod_input = input$add_left_intercalary_rod,
                                                                       add_left_linked_rod_input = input$add_left_linked_rod,
                                                                       add_left_kickstand_rod_input = input$add_left_kickstand_rod,
                                                                       
                                                                       left_accessory_rod_material_input = input$left_accessory_rod_material,
                                                                       left_accessory_rod_size_input = input$left_accessory_rod_size,
                                                                       left_satellite_rod_material_input = input$left_satellite_rod_material,
                                                                       left_satellite_rod_size_input = input$left_satellite_rod_size,
                                                                       left_intercalary_rod_material_input = input$left_intercalary_rod_material,
                                                                       left_intercalary_rod_size_input = input$left_intercalary_rod_size,
                                                                       left_linked_rod_material_input = input$left_linked_rod_material,
                                                                       left_linked_rod_size_input = input$left_linked_rod_size,
                                                                       left_kickstand_rod_material_input = input$left_kickstand_rod_material,
                                                                       left_kickstand_rod_size_input = input$left_kickstand_rod_size,
                                                                       
                                                                       add_right_accessory_rod_input = input$add_right_accessory_rod,
                                                                       add_right_satellite_rod_input = input$add_right_satellite_rod,
                                                                       add_right_intercalary_rod_input = input$add_right_intercalary_rod,
                                                                       add_right_linked_rod_input = input$add_right_linked_rod,
                                                                       add_right_kickstand_rod_input = input$add_right_kickstand_rod,
                                                                       
                                                                       right_accessory_rod_material_input = input$right_accessory_rod_material,
                                                                       right_accessory_rod_size_input = input$right_accessory_rod_size,
                                                                       right_satellite_rod_material_input = input$right_satellite_rod_material,
                                                                       right_satellite_rod_size_input = input$right_satellite_rod_size,
                                                                       right_intercalary_rod_material_input = input$right_intercalary_rod_material,
                                                                       right_intercalary_rod_size_input = input$right_intercalary_rod_size,
                                                                       right_linked_rod_material_input = input$right_linked_rod_material,
                                                                       right_linked_rod_size_input = input$right_linked_rod_size,
                                                                       right_kickstand_rod_material_input = input$right_kickstand_rod_material,
                                                                       right_kickstand_rod_size_input = input$right_kickstand_rod_size,

                                                                       crosslink_connectors_input = input$crosslink_connectors,
                                                                       anterior_biologics_input = input$anterior_biologics,
                                                                       anterior_bmp_dose_reactive_input = anterior_bmp_dose_reactive(),
                                                                       anterior_bone_graft_input = input$anterior_bone_graft,
                                                                       anterior_allograft_amount_input = input$anterior_allograft_amount,
                                                                       anterior_bone_marrow_aspirate_volume_input = input$anterior_bone_marrow_aspirate_volume,
                                                                       anterior_cell_based_allograft_volume_input = input$anterior_cell_based_allograft_volume,
                                                                       anterior_dbm_volume_input = input$anterior_dbm_volume,
                                                                       anterior_ifactor_volume_input = input$anterior_ifactor_volume,
                                                                       anterior_biologics_other_input = input$anterior_biologics_other,
                                                                       
                                                                       posterior_biologics_input = input$posterior_biologics,
                                                                       posterior_bmp_dose_reactive_input = posterior_bmp_dose_reactive(),
                                                                       posterior_bone_graft_input = input$posterior_bone_graft,
                                                                       posterior_allograft_amount_input = input$posterior_allograft_amount,
                                                                       posterior_bone_marrow_aspirate_volume_input = input$posterior_bone_marrow_aspirate_volume,
                                                                       posterior_cell_based_allograft_volume_input = input$posterior_cell_based_allograft_volume,
                                                                       posterior_dbm_volume_input = input$posterior_dbm_volume,
                                                                       posterior_ifactor_volume_input = input$posterior_ifactor_volume,
                                                                       posterior_biologics_other_input = input$posterior_biologics_other,
                                                                       
                                                                       intraoperative_complications_vector_input = input$intraoperative_complications_vector,
                                                                       other_intraoperative_complications_input = input$other_intraoperative_complications,
                                                                       implant_manufacturer_input = input$implant_manufacturer,
                                                                       operative_note_text_input = input$operative_note_text
                                                                       # operative_note_text_input = "test if this is the error"
      )
      
      surgery_details_df$surgery_details_df
    }else{
      tibble(name = character(), value = character())
    }
  })
    
  ################## GENERATE INTRAOPERATIVE DETAILS TABLE #############
  intraoperative_details_redcap_table_reactive <- reactive({
    if(input$implants_complete > 0){
      intraoperative_details_redcap_df <- redcap_table_intraop_details_df_function(all_objects_df_input = all_objects_to_add_list$objects_df,
                                                                                           date_of_surgery_input = input$date_of_surgery,
                                                                                           preop_antibiotics_input = input$preop_antibiotics,
                                                                                           neuromonitoring_input = input$neuromonitoring,
                                                                                           anti_fibrinolytic_input = input$anti_fibrinolytic,
                                                                                           txa_loading_input = input$txa_loading,
                                                                                           txa_maintenance_input = input$txa_maintenance,
                                                                                           surgical_findings_input = input$surgical_findings,
                                                                                           specimens_removed_input = input$specimens_removed,
                                                                                           ebl_input = input$ebl,
                                                                                           urine_output_input = input$urine_output,
                                                                                           crystalloids_administered_input = input$crystalloids_administered,
                                                                                           colloids_administered_input = input$colloids_administered,
                                                                                           transfusion_input = input$transfusion,
                                                                                           cell_saver_transfused_input = input$cell_saver_transfused,
                                                                                           prbc_transfused_input = input$prbc_transfused,
                                                                                           ffp_transfused_input = input$ffp_transfused,
                                                                                           cryoprecipitate_transfused_input = input$cryoprecipitate_transfused,
                                                                                           platelets_transfused_input = input$platelets_transfused,
                                                                                           intraoperative_complications_vector_input = input$intraoperative_complications_vector,
                                                                                           other_intraoperative_complications_input = input$other_intraoperative_complications,
                                                                                           deep_drains_anterior_input = input$deep_drains_anterior,
                                                                                           superficial_drains_anterior_input = input$superficial_drains_anterior,
                                                                                           deep_drains_posterior_input = input$deep_drains_posterior,
                                                                                           superficial_drains_posterior_input = input$superficial_drains_posterior,
                                                                                           additional_end_procedure_details_anterior_input = input$additional_end_procedure_details_anterior,
                                                                                           additional_end_procedure_details_posterior_input = input$additional_end_procedure_details_posterior,
                                                                                           closure_details_anterior_input = input$closure_details_anterior,
                                                                                           dressing_details_anterior_input = input$dressing_details_anterior,
                                                                                           closure_details_posterior_input = input$closure_details_posterior,
                                                                                           dressing_details_posterior_input = input$dressing_details_posterior
      )
      
      intraoperative_details_redcap_df$intraop_details_df
    }else{
      tibble(name = character(), value = character())
    }
    
  })
  
  
  ################# MAKE THE procedures by level DATAFRAME ##################
  procedures_by_level_redcap_table_reactive <- reactive({
    data_wide <- tibble()
    if(nrow(all_objects_to_add_list$objects_df)>0 && input$implants_complete > 0){
      fusion_df <- jh_fusion_category_function(fusion_vector = union(input$posterior_fusion_levels_confirmed, input$anterior_fusion_levels_confirmed), 
                                               all_objects_df = all_objects_to_add_list$objects_df)%>%
        mutate(side = "central")
      
      data_wide <- all_objects_to_add_list$objects_df %>%
        select(level, vertebral_number, body_interspace, approach, category, implant, object, side, x, y, fusion, interbody_fusion, fixation_uiv_liv) %>%
        mutate(proc_category = map(.x = object, .f = ~ op_note_procedure_performed_summary_classifier_function(object = .x))) %>%
        unnest(proc_category) %>%
        bind_rows(fusion_df) %>%
        select(level, approach, category, object, side) %>%
        group_by(level, category, side) %>%
        mutate(repeat_count = row_number()) %>%
        ungroup() %>%
        pivot_wider(names_from = level, values_from = object) %>%
        select(-repeat_count) %>%
        clean_names() 

      
    }else{
      data_wide <- tibble(redcap_repeat_instrument = character(), 
                          redcap_repeat_instance = integer(),
                          dos_surg_repeating = character(),
                          approach = character(),
                          side = character()
      ) 
      
    }
    
    ### ADD THE REVISION DETAILS
    
    if(input$revision_approach == "anterior" && input$implants_complete > 0){
      if(length(input$prior_anterior_plate_removed_levels)>0){
        anterior_plate_removed_df <- tibble(level = input$prior_anterior_plate_removed_levels,
                                            object = "anterior_plate_removal", 
                                            category = "anterior_implant_removal",
                                            approach = "anterior",
                                            side = "central") %>%
          group_by(level, category, side) %>%
          mutate(repeat_count = row_number()) %>%
          ungroup() %>%
          pivot_wider(names_from = level, values_from = object) %>%
          select(-repeat_count) %>%
          clean_names()
        
        data_wide <- data_wide %>%
          bind_rows(anterior_plate_removed_df)
        
      }
    }
    
    if(input$revision_approach == "posterior" && input$implants_complete > 0){
      if(length(input$left_revision_implants_removed)>0){
        left_implants_removed_df <- tibble(level = input$left_revision_implants_removed,
                                           object = "implant_removal", 
                                           category = "implant_removal",
                                           approach = "posterior",
                                           side = "left") %>%
          group_by(level, category, side) %>%
          mutate(repeat_count = row_number()) %>%
          ungroup() %>%
          pivot_wider(names_from = level, values_from = object) %>%
          select(-repeat_count) %>%
          clean_names() 
        
        data_wide <- data_wide %>%
          bind_rows(left_implants_removed_df)
  
      }
      
      if(length(input$right_revision_implants_removed)>0 && input$implants_complete > 0){
        right_implants_removed_df <- tibble(level = input$right_revision_implants_removed,
                                            object = "implant_removal", 
                                            category = "implant_removal",
                                            approach = "posterior",
                                            side = "right")%>%
          group_by(level, category, side) %>%
          mutate(repeat_count = row_number()) %>%
          ungroup() %>%
          pivot_wider(names_from = level, values_from = object) %>%
          select(-repeat_count) %>%
          clean_names() 
        
        data_wide <- data_wide %>%
          bind_rows(right_implants_removed_df)
      }
    }
    data_wide %>%
      ungroup() %>%
      mutate(across(everything(), ~ replace_na(.x, " "))) %>%
      mutate(redcap_repeat_instance = row_number()) %>%
      mutate(redcap_repeat_instrument = "procedures_by_level_repeating") %>%
      mutate(dos_surg_repeating = as.character(input$date_of_surgery)) %>%
      select(redcap_repeat_instrument, redcap_repeat_instance, dos_surg_repeating, approach_repeating = approach, everything())
  })
  
  
  
  ################------------------  Screw Size RESULTS  ----------------------######################  
  ################------------------  Screw Size RESULTS  ----------------------######################  
    ## NEW
  screw_details_redcap_table_reactive <- reactive({
    if(input$implants_complete > 0){
    if(length(input$screws_implanted_picker_for_ui) > 0){
      screw_details_full_df <- screw_size_details_df_reactive() %>%
        mutate(redcap_repeat_instrument = "screw_details_repeating") %>%
        mutate(redcap_repeat_instance = row_number()) %>%
        mutate(dos_screws_repeating = as.character(input$date_of_surgery)) %>%
        select(redcap_repeat_instrument,
               redcap_repeat_instance,
               dos_screws_repeating,
               screw_level = level,
               screw_implant = object,
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
    }
    
  })
  
  
  ################# INTERBODY  DETAILS TABLE ##################
  
  interbody_details_redcap_table_reactive <- reactive({
    if(input$implants_complete > 0){
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
    }
  })
  
  
  ##### rods crossing_by_level ####
  ##### rods crossing_by_level ####
  rods_crossing_by_level_redcap_table_reactive <- reactive({
    
    # if(input$implants_complete > 0){
    if(input$implants_complete > 0 && !is.null(input$approach_sequence) && str_detect(input$approach_sequence, "posterior")){
      
      rods_crossing_no_sizes_df <- labels_df %>%
        mutate(vertebral_number = vertebral_number - 0.5) %>%
        select(-level) %>%
        left_join(levels_numbered_df) %>%
        filter(!is.na(level)) %>%
        select(level) %>%
        left_join(left_rod_crossing_table_reactive()) %>%
        left_join(right_rod_crossing_table_reactive()) %>%
        replace_na(list(left_rod_count = 0, right_rod_count = 0, left_rods_crossing = "", right_rods_crossing = "")) %>%
        mutate(total_rods_crossing = left_rod_count + right_rod_count) %>%
        filter(total_rods_crossing >0) %>%
        mutate(left_rods_crossing = str_remove_all(left_rods_crossing, "_sf"),
               right_rods_crossing = str_remove_all(right_rods_crossing, "_sf")) %>%
        mutate(dos_rods_crossing_repeating = as.character(input$date_of_surgery)) %>%
        select(dos_rods_crossing_repeating, rods_crossing_level = level, left_rod_count, left_rods_crossing, right_rod_count, right_rods_crossing, total_rods_crossing) 
      
      if(nrow(rods_crossing_no_sizes_df)>0){
        
      left_accessory_rod_material <- if(input$add_left_accessory_rod){paste(input$left_accessory_rod_material)}else{"na"}
      left_accessory_rod_size <- if(input$add_left_accessory_rod){paste(input$left_accessory_rod_size)}else{"na"}
      left_satellite_rod_material <- if(input$add_left_satellite_rod){paste(input$left_satellite_rod_material)}else{"na"}
      left_satellite_rod_size <- if(input$add_left_satellite_rod){paste(input$left_satellite_rod_size)}else{"na"}
      left_intercalary_rod_material <- if(input$add_left_intercalary_rod){paste(input$left_intercalary_rod_material)}else{"na"}
      left_intercalary_rod_size <- if(input$add_left_intercalary_rod){paste(input$left_intercalary_rod_size)}else{"na"}
      left_linked_rod_material <- if(input$add_left_linked_rod){paste(input$left_linked_rod_material)}else{"na"}
      left_linked_rod_size <- if(input$add_left_linked_rod){paste(input$left_linked_rod_size)}else{"na"}
      left_kickstand_rod_material <- if(input$add_left_kickstand_rod){paste(input$left_kickstand_rod_material)}else{"na"}
      left_kickstand_rod_size <- if(input$add_left_kickstand_rod){paste(input$left_kickstand_rod_size)}else{"na"}
      
      rods_crossing_left_df <- jh_rods_table_add_supplemental_rod_size_material_function(rods_crossing_df_input = rods_crossing_no_sizes_df, 
                                                                                         side = "left", 
                                                                                         main_rod_size =input$left_main_rod_size,
                                                                                         main_rod_material = input$left_main_rod_material,
                                                                                         accessory_rod_material = left_accessory_rod_material, 
                                                                                         accessory_rod_size = left_accessory_rod_size, 
                                                                                         satellite_rod_material = left_satellite_rod_material, 
                                                                                         satellite_rod_size = left_satellite_rod_size, 
                                                                                         intercalary_rod_material = left_intercalary_rod_material, 
                                                                                         intercalary_rod_size = left_intercalary_rod_size, 
                                                                                         linked_rod_material = left_linked_rod_material, 
                                                                                         linked_rod_size = left_linked_rod_size, 
                                                                                         kickstand_rod_material = left_kickstand_rod_material, 
                                                                                         kickstand_rod_size = left_kickstand_rod_size
      )
      
      right_accessory_rod_material <- if(input$add_right_accessory_rod){paste(input$right_accessory_rod_material)}else{"na"}
      right_accessory_rod_size <- if(input$add_right_accessory_rod){paste(input$right_accessory_rod_size)}else{"na"}
      right_satellite_rod_material <- if(input$add_right_satellite_rod){paste(input$right_satellite_rod_material)}else{"na"}
      right_satellite_rod_size <- if(input$add_right_satellite_rod){paste(input$right_satellite_rod_size)}else{"na"}
      right_intercalary_rod_material <- if(input$add_right_intercalary_rod){paste(input$right_intercalary_rod_material)}else{"na"}
      right_intercalary_rod_size <- if(input$add_right_intercalary_rod){paste(input$right_intercalary_rod_size)}else{"na"}
      right_linked_rod_material <- if(input$add_right_linked_rod){paste(input$right_linked_rod_material)}else{"na"}
      right_linked_rod_size <- if(input$add_right_linked_rod){paste(input$right_linked_rod_size)}else{"na"}
      right_kickstand_rod_material <- if(input$add_right_kickstand_rod){paste(input$right_kickstand_rod_material)}else{"na"}
      right_kickstand_rod_size <- if(input$add_right_kickstand_rod){paste(input$right_kickstand_rod_size)}else{"na"}
      
      rods_crossing_full_df <- jh_rods_table_add_supplemental_rod_size_material_function(rods_crossing_df_input = rods_crossing_left_df,
                                                                                         side = "right", 
                                                                                         main_rod_size =input$right_main_rod_size,
                                                                                         main_rod_material = input$right_main_rod_material,
                                                                                         accessory_rod_material = right_accessory_rod_material, 
                                                                                         accessory_rod_size = right_accessory_rod_size, 
                                                                                         satellite_rod_material = right_satellite_rod_material, 
                                                                                         satellite_rod_size = right_satellite_rod_size, 
                                                                                         intercalary_rod_material = right_intercalary_rod_material, 
                                                                                         intercalary_rod_size = right_intercalary_rod_size, 
                                                                                         linked_rod_material = right_linked_rod_material, 
                                                                                         linked_rod_size = right_linked_rod_size, 
                                                                                         kickstand_rod_material = right_kickstand_rod_material, 
                                                                                         kickstand_rod_size = right_kickstand_rod_size
      )
      }else{
        rods_crossing_full_df <- tibble(dos_rods_crossing_repeating = character(),
                                        rods_crossing_level = character(),
                                        left_rod_count = double(),
                                        left_rods_crossing = character(),
                                        right_rod_count = double(),
                                        right_rods_crossing = character(),
                                        total_rods_crossing = double(),
                                        left_main_rod_size = character(),
                                        left_main_rod_material = character(),
                                        left_accessory_rod_size = character(),
                                        left_accessory_rod_material = character(),
                                        left_satellite_rod_size = character(),
                                        left_satellite_rod_material = character(),
                                        left_intercalary_rod_size = character(),
                                        left_intercalary_rod_material = character(),
                                        left_linked_rod_size = character(),
                                        left_linked_rod_material = character(),
                                        left_kickstand_rod_size = character(),
                                        left_kickstand_rod_material = character(),
                                        right_main_rod_size = character(),
                                        right_main_rod_material = character(),
                                        right_accessory_rod_size = character(),
                                        right_accessory_rod_material = character(),
                                        right_satellite_rod_size = character(),
                                        right_satellite_rod_material = character(),
                                        right_intercalary_rod_size = character(),
                                        right_intercalary_rod_material = character(),
                                        right_linked_rod_size = character(),
                                        right_linked_rod_material = character(),
                                        right_kickstand_rod_size = character(),
                                        right_kickstand_rod_material = character()
        )
      }
      
    }else{
      rods_crossing_full_df <- tibble(dos_rods_crossing_repeating = character(),
                                      rods_crossing_level = character(),
                                      left_rod_count = double(),
                                      left_rods_crossing = character(),
                                      right_rod_count = double(),
                                      right_rods_crossing = character(),
                                      total_rods_crossing = double(),
                                      left_main_rod_size = character(),
                                      left_main_rod_material = character(),
                                      left_accessory_rod_size = character(),
                                      left_accessory_rod_material = character(),
                                      left_satellite_rod_size = character(),
                                      left_satellite_rod_material = character(),
                                      left_intercalary_rod_size = character(),
                                      left_intercalary_rod_material = character(),
                                      left_linked_rod_size = character(),
                                      left_linked_rod_material = character(),
                                      left_kickstand_rod_size = character(),
                                      left_kickstand_rod_material = character(),
                                      right_main_rod_size = character(),
                                      right_main_rod_material = character(),
                                      right_accessory_rod_size = character(),
                                      right_accessory_rod_material = character(),
                                      right_satellite_rod_size = character(),
                                      right_satellite_rod_material = character(),
                                      right_intercalary_rod_size = character(),
                                      right_intercalary_rod_material = character(),
                                      right_linked_rod_size = character(),
                                      right_linked_rod_material = character(),
                                      right_kickstand_rod_size = character(),
                                      right_kickstand_rod_material = character()
      )
    }
    rods_crossing_full_df
  })
  

  ###### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  ALL INPUTS  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ######## 
  
  # all_inputs_reactive_list <- reactive({
  #   
  #   all_inputs_list <- reactiveValuesToList(input, all.names = FALSE)
  #   
  #   all_inputs_list <- keep(.x = all_inputs_list, .p = ~ !is.null(.x))
  #   
  # })
  
  
  all_inputs_trimmed_table_reactive <- reactive({
    if(input$implants_complete > 0){
      
      strings_to_detect_and_remove_vector <- c("object_to_add",
                                               "txa",
                                               "plot_with_patterns_true", 
                                               "tab",
                                               "add",
                                               "label_text_offset", 
                                               "search_for_prior_patient", 
                                               "plot_summary_table",
                                               "add_implants", 
                                               "add_other", 
                                               "add_special_approach", 
                                               "add_interbody",
                                               "add_osteotomies",
                                               "add_decompressions",
                                               "button",
                                               "screws_implanted_picker_for_ui",
                                               "central_corpectomy_cage",
                                               "central_anterior_disc_arthroplasty",
                                               "central_anterior_interbody_implant",
                                               "left_tlif", 
                                               "right_tlif",
                                               "left_plif", 
                                               "right_plif",
                                               "operative_note",
                                               "crop_y", 
                                               "screw_length",
                                               "screw_diameter", 
                                               "screw_type",
                                               "rods_eligible", 
                                               "reset", 
                                               "button",
                                               "level_object_for",
                                               "pelvic_screw",
                                               "modal", 
                                               "_complete",
                                               "text_size", 
                                               "idpostop", 
                                               "drop", 
                                               "prior_patient_match_located", 
                                               "redcap_token", 
                                               "redcap", 
                                               "edit",
                                               "true_false")
      
      all_inputs_list <- reactiveValuesToList(input, all.names = FALSE)
      
      all_inputs_list <- keep(.x = all_inputs_list, .p = ~ !is.null(.x))
      
      all_inputs_to_log_df <- enframe(all_inputs_list) %>%
        mutate(result = map(.x = value, .f = ~ as.character(glue_collapse(.x, sep = "-AND-")))) %>%
        select(-value) %>%
        unnest(cols = result) %>%
        filter(str_detect(name, pattern = glue_collapse(x = strings_to_detect_and_remove_vector, sep = "|"))==FALSE) %>%
        filter(!is.na(result)) %>%
        filter(result != "") %>%
        filter(result != " ") %>%
        filter(str_detect(string = name, pattern = "[:upper:]", negate = TRUE)) %>%
        filter(str_detect(string = name, pattern = "\\s", negate = TRUE) ) %>%
        filter(str_detect(string = name, pattern = "\\W", negate = TRUE)) %>%
        filter(str_detect(string = name, pattern = "\\t", negate = TRUE))
      
      rods_to_keep <- all_inputs_to_log_df %>%
        filter(str_detect(name, "rod")) %>%
        filter(str_detect(name, "add")) %>%
        filter(result == "TRUE") %>%
        mutate(rod_type = str_remove_all(string = name, pattern = "add_"))
      
      if(nrow(rods_to_keep)>0){
        rod_types_to_keep_string <- paste0(rods_to_keep$rod_type, collapse = "|")
        
        all_rod_info_to_keep_df <- all_inputs_to_log_df %>%
          filter(str_detect(string = name, pattern = rod_types_to_keep_string))
        main_rod_info_to_keep_df <- all_inputs_to_log_df %>%
          filter(str_detect(string = name, pattern = "main_rod"))
        
        all_inputs_to_log_df <- all_inputs_to_log_df %>%
          filter(str_detect(string = name, pattern = "rod") == FALSE) %>%
          bind_rows(main_rod_info_to_keep_df) %>%
          bind_rows(all_rod_info_to_keep_df)
      }else{
        
        main_rod_info_to_keep_df <- all_inputs_to_log_df %>%
          filter(str_detect(string = name, pattern = "main_rod"))
        
        all_inputs_to_log_df <- all_inputs_to_log_df %>%
          filter(str_detect(string = name, pattern = "rod") == FALSE) %>%
          bind_rows(main_rod_info_to_keep_df) 
      }
      
      
      # all_inputs_to_log_df %>%
      # filter(str_detect(measure, pattern = paste0(glue_collapse(rods_not_used$rod_type_not_used, sep = "|"))) == FALSE) 
      
      all_inputs_to_log_df
    }else{
      tibble()
    }
    
  })
  
  
  
  
  ############### ############### NOW RENDER EACH OF THE TABLES FOR THE SIDE TABULAR VIEW:    ###############     ############### 
  ############### ############### NOW RENDER EACH OF THE TABLES FOR THE SIDE TABULAR VIEW:    ###############     ############### 
  ############### ############### NOW RENDER EACH OF THE TABLES FOR THE SIDE TABULAR VIEW:    ###############     ############### 
  
  ########################
   #################  PATIENT DETAILS TABLE ##################
  output$patient_details_redcap_table <- renderTable({
    patient_details_redcap_table_reactive()
  })
  
  #################  SURGICAL DETAILS TABLE ##################
  output$surgical_details_redcap_table <- renderTable({
    surgical_details_redcap_table_reactive()
  })
  
  #################  INTRAOP DETAILS TABLE ##################
  output$intraoperative_details_redcap_table <- renderTable({
    intraoperative_details_redcap_table_reactive()
  })
  
  #################  RODS CROSSING BY LEVEL TABLE ##################
  output$rods_crossing_by_level_table <- renderTable({
    # rods_crossing_by_level_redcap_table_reactive()
    
    rods_crossing_by_level_redcap_table_reactive() %>%
      select(-dos_rods_crossing_repeating) %>%
      select(rods_crossing_level, total_rods_crossing, everything())
    
    # rods_crossing_by_level_repeating <- rods_crossing_by_level_redcap_table_reactive() %>%
    #   mutate(record_id = "record number will go here") %>%
    #   select(record_id, everything()) %>%
    #   mutate(redcap_event_name = "surgery_arm_1") %>%
    #   mutate(redcap_repeat_instance = paste(row_number(), "repeat instance will go here")) %>%
    #   mutate(redcap_repeat_instrument = "rods_crossing_by_level_repeating") %>%
    #   mutate(rods_crossing_by_level_repeating_complete = "Complete") %>%
    #   mutate(across(everything(), ~ paste0(as.character(.x)))) %>%
    #   select(record_id, redcap_event_name, redcap_repeat_instrument, redcap_repeat_instance, dos_rods_crossing_repeating, rods_crossing_level, left_rod_count, left_rods_crossing, right_rod_count, right_rods_crossing, total_rods_crossing, rods_crossing_by_level_repeating_complete)
  })
  
  #################  PROCEDURES BY LEVEL TABLE ##################
  output$procedures_by_level_redcap_table <- renderTable({
    procedures_by_level_redcap_table_reactive()
  })
  
  #################  SCREW DETAILS TABLE ##################
  output$screw_details_redcap_table <- renderTable({
    screw_details_redcap_table_reactive()
  })
  
  
  #################  INTERBODY DETAILS TABLE ##################
  output$interbody_details_redcap_table <- renderTable({
    interbody_details_redcap_table_reactive()
  })
  
  #################  ALL INPUTS TABLE ##################
  output$all_inputs_trimmed_table <- renderTable({
    all_inputs_trimmed_table_reactive()
  })
  
  
  
  #### TABLES THAT WILL NOT BE UPLOADED:
  #################  ALL OBJECTS TABLE ##################
  output$all_objects_table <- renderTable({
    if(input$implants_complete > 0){
      
      all_objects_to_add_list$objects_df %>%
        select(level, vertebral_number, body_interspace, approach, category, implant, object, side, x, y, fusion, interbody_fusion, fixation_uiv_liv) %>%
        mutate(proc_category = map(.x = object, .f = ~ op_note_procedure_performed_summary_classifier_function(object = .x))) %>%
        unnest() %>%
        select(proc_category, everything())
    }
  })
  
  ######## Render "Revision Implants" for side tab:"    ######## 
  output$revision_implants_table <- renderTable({
    revision_implants_df <- left_revision_implants_reactive_list()$revision_implants_status_df %>%
      bind_rows(right_revision_implants_reactive_list()$revision_implants_status_df)
    
    revision_implants_df
  })
  
  output$anterior_revision_implants_table <- renderTable({
    anterior_plate_revision_implants_df_reactive() %>%
      select(level, prior_plate_present_levels, prior_plate_status)
    
  })
  
  ################## GENERATE TABLES AND PRINTOUTS FOR SIDE TAB TO SHOW WHAT IS UPLOADED TO THE OP NOTE GENERATOR
  output$posterior_approach_objects_for_op_note_df <- renderTable({
    if(input$implants_complete > 0){
      posterior_op_note_inputs_list_reactive()$posterior_approach_objects_df
    }
  }
  )
  
  
  output$full_objects_passed_to_posterior_op_note <- renderPrint({
    posterior_op_note_list <- posterior_op_note_inputs_list_reactive()
    posterior_op_note_list$posterior_approach_objects_df <- "See table"
    
    paste(map2(.x = posterior_op_note_list, .y = names(posterior_op_note_list), .f = ~ paste0(.y, ": ", paste(.x, sep = ", ", collapse = ", ")))
    )
  })
  
  output$full_objects_passed_to_posterior_op_note_df <- renderTable({
    posterior_op_note_list <- posterior_op_note_inputs_list_reactive()
    posterior_list_names <- names(posterior_op_note_list)
    
    tibble(contents = paste(map(posterior_op_note_list, .f = ~ paste(dput(.x))))) %>%
      mutate(list_input_name = posterior_list_names) %>%
      select(list_input_name, contents)
    
  })
  
  output$anterior_approach_objects_for_op_note_df <- renderTable({
    anterior_op_note_inputs_list_reactive()$anterior_approach_objects_df
  }
  )
  
  output$full_objects_passed_to_anterior_op_note <- renderPrint({
    anterior_op_note_list <- anterior_op_note_inputs_list_reactive()
    anterior_op_note_list$anterior_approach_objects_df <- "See table"
    paste(map2(.x = anterior_op_note_list, .y = names(anterior_op_note_list), .f = ~ paste0(.y, ": ", paste(.x, sep = ", ", collapse = ", "))))
  })
  
  output$full_objects_passed_to_anterior_op_note_df <- renderTable({
    # list_no_df <- keep(.x = anterior_op_note_inputs_list_reactive(), .p = ~ is.data.frame(.x) == FALSE)
    anterior_op_note_list <- anterior_op_note_inputs_list_reactive()
    anterior_list_names <- names(anterior_op_note_list)
    
    tibble(contents = paste(map(anterior_op_note_list, .f = ~ paste(dput(.x))))) %>%
      mutate(list_input_name = anterior_list_names) %>%
      select(list_input_name, contents)
    
  })
  
  
  output$all_inputs_removed <- renderTable({
    if(input$implants_complete > 0){
      all_inputs_list <- reactiveValuesToList(input, all.names = FALSE)
      
      all_inputs_list <- keep(.x = all_inputs_list, .p = ~ !is.null(.x))
      
      all_inputs_to_log_df <- enframe(all_inputs_list) %>%
        mutate(result = map(.x = value, .f = ~ as.character(glue_collapse(.x, sep = "-AND-")))) %>%
        select(-value) %>%
        unnest(cols = result)
      all_inputs_to_log_df
    }
  })

  

  ############### ############### NOW RENDER EACH OF THE TABLES FOR THE FINAL REVIEW IN THE MODAL:    ###############     ############### 
  ############### ############### NOW RENDER EACH OF THE TABLES FOR THE FINAL REVIEW IN THE MODAL:    ###############     ############### 
  ############### ############### NOW RENDER EACH OF THE TABLES FOR THE FINAL REVIEW IN THE MODAL:    ###############     ############### 
  #################  PATIENT DETAILS TABLE ##################
  output$patient_details_redcap_table_modal <- renderTable({
    patient_details_redcap_table_reactive()
  })
  
  #################  SURGICAL DETAILS TABLE ##################
  output$surgical_details_redcap_table_modal <- renderTable({
    surgical_details_redcap_table_reactive()
  })
  
  #################  INTRAOP DETAILS TABLE ##################
  output$intraoperative_details_redcap_table_modal <- renderTable({
    intraoperative_details_redcap_table_reactive()
  })
  
  #################  RODS CROSSING BY LEVEL TABLE ##################
  output$rods_crossing_by_level_table_modal <- renderTable({
    rods_crossing_by_level_redcap_table_reactive()
  })
  
  #################  PROCEDURES BY LEVEL TABLE ##################
  output$procedures_by_level_redcap_table_modal <- renderTable({
    procedures_by_level_redcap_table_reactive()
  })
  
  #################  SCREW DETAILS TABLE ##################
  output$screw_details_redcap_table_modal <- renderTable({
    screw_details_redcap_table_reactive()
  })
  
  
  #################  INTERBODY DETAILS TABLE ##################
  output$interbody_details_redcap_table_modal <- renderTable({
    interbody_details_redcap_table_reactive()
  })
  
  #################  ALL INPUTS TABLE ##################
  output$all_inputs_trimmed_table_modal <- renderTable({
    all_inputs_trimmed_table_reactive()
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
                                        tableOutput(outputId = "patient_details_redcap_table_modal")
                               ),
                               tabPanel(title = "Surgical Summary",
                                        tableOutput(outputId = "surgical_details_redcap_table_modal")
                               ),
                               tabPanel(title = "Intraoperative Details", 
                                        tableOutput(outputId = "intraoperative_details_redcap_table_modal")
                               ),
                               tabPanel(title = "Rods Crossing by Level",
                                        tableOutput(outputId = "rods_crossing_by_level_table_modal")
                               ),
                               tabPanel(title = "Procedures by Level",
                                        tableOutput(outputId = "procedures_by_level_redcap_table_modal")
                               ),
                               tabPanel(title = "Screw Details",
                                        tableOutput(outputId = "screw_details_redcap_table_modal")
                               ),
                               tabPanel(title = "Interbody Details",
                                        tableOutput(outputId = "interbody_details_redcap_table_modal")
                               ),
                               tabPanel(title = "All Inputs",
                                        tableOutput(outputId = "all_inputs_trimmed_table_modal")
                               )
                        )
                      )
                  )
      )
      
    )
  })
  
  
  
  final_upload_reactive_count <- reactiveValues()
  final_upload_reactive_count$count <- 0
  observeEvent(input$confirm_upload_final, {
    final_upload_reactive_count$count <- final_upload_reactive_count$count + 1
  }
  )
  
  observeEvent(input$confirm_upload_final, {
    
    # if(rcon_reactive$rcon$has_metadata() == FALSE){
      
      if(str_length(input$redcap_token) < 5){
      final_upload_reactive_count$count <- 0
      showModal(modalDialog(title = "Please enter a valid Redcap Token and click the button below and then attempt to upload again", 
                            easyClose = TRUE, 
                            box(width = 12,
                                h3("PLEASE GO BACK AND ENTER A VALID REDCAP TOKEN ON FIRST PAGE, Edit Patient") 
                            )
      )
      )
      
        
    }else if(final_upload_reactive_count$count == 1){
      
      
      if(redcapAPI::exportNextRecordName(rcon = rcon_reactive$rcon)>1){
        all_patient_ids_df <- exportRecords(rcon = rcon_reactive$rcon, fields = c("record_id", "last_name", "first_name", "date_of_birth"), events = "enrollment_arm_1") %>%
          type.convert(as.is = TRUE) %>%
          select(record_id, last_name, first_name, date_of_birth) %>%
          mutate(last_name = str_to_lower(last_name),
                 first_name = str_to_lower(first_name))   
      }else{
        all_patient_ids_df <- tibble()
      }
      
      if(nrow(all_patient_ids_df)>0){
        joined_df <- patient_details_redcap_table_reactive() %>%
          select(last_name, first_name, date_of_birth) %>%
          mutate(last_name = str_to_lower(last_name),
                 first_name = str_to_lower(first_name)) %>%
          left_join(all_patient_ids_df)
        
        match_found <- if_else(!is.na(joined_df$record_id[[1]]), TRUE, FALSE)
        
        if(match_found == TRUE){
          record_number <- joined_df$record_id[[1]]
          
          max_repeat_instances_df <- exportRecords(rcon = rcon_reactive$rcon, records = record_number) %>%
            type.convert(as.is = TRUE) %>%
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
          if("rods_crossing_by_level_repeating" %in% max_repeat_instances_df$redcap_repeat_instrument){
            rods_crossing_by_level_repeating_instance_add <- repeat_list$rods_crossing_by_level_repeating
          }else{
            rods_crossing_by_level_repeating_instance_add <- 0
          }
          
          if("all_inputs_repeating" %in% max_repeat_instances_df$redcap_repeat_instrument){
            all_inputs_repeating_instance_add <- repeat_list$all_inputs_repeating
          }else{
            all_inputs_repeating_instance_add <- 0
          }
          
          if("implant_removal_repeating" %in% max_repeat_instances_df$redcap_repeat_instrument){
            implant_removal_repeating_instance_add <- repeat_list$implant_removal_repeating
          }else{
            implant_removal_repeating_instance_add <- 0
          }
          if("logging_data_time_repeating" %in% max_repeat_instances_df$redcap_repeat_instrument){
            logging_data_time_repeating_instance_add <- repeat_list$logging_data_time_repeating
          }else{
            logging_data_time_repeating_instance_add <- 0
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
          rods_crossing_by_level_repeating_instance_add <- 0
          all_inputs_repeating_instance_add <- 0
          # posterior_implant_removal_instance_add <- 0
          implant_removal_repeating_instance_add <- 0
          logging_data_time_repeating_instance_add <- 0
        }
      }else{
        record_number <- exportNextRecordName(rcon = rcon_reactive$rcon)
        surgical_details_instance_add <- 0
        procedures_by_level_repeating_instance_add <- 0
        screw_details_repeating_instance_add <- 0
        interbody_implant_repeating_instance_add <- 0
        rods_crossing_by_level_repeating_instance_add <- 0
        all_inputs_repeating_instance_add <- 0
        # posterior_implant_removal_instance_add <- 0
        implant_removal_repeating_instance_add <- 0
        logging_data_time_repeating_instance_add <- 0
      }
      
      ##### uploaded patient details #######
      
      redcap_names_df <- exportFieldNames(rcon = rcon_reactive$rcon) %>%
        as_tibble() %>%
        select(redcap_field_names = original_field_name)
      
      tables_not_uploaded_list <- list()
    
      withProgress(message = 'Uploading Data', value = 0, {
        number_of_steps <- 10
        
        incProgress(1/number_of_steps, detail = paste("Uploading Patient Details"))
        
        ##### uploaded patient details #######
        patient_df_for_upload <- patient_details_redcap_table_reactive() %>%
          mutate(record_id = record_number) %>%
          mutate(patient_details_complete = "Complete") %>%
          select(record_id, everything())
        
        if(all(names(patient_df_for_upload) %in% redcap_names_df$redcap_field_names)){
          importRecords(rcon = rcon_reactive$rcon, data = patient_df_for_upload, returnContent = "count")
        }else{
          tables_not_uploaded_list$patient_df <- "PatientDf"
        }
        
        incProgress(1/number_of_steps, detail = paste("Uploading Surgical Details"))
        
        ##### uploaded surgical details #######
        surgical_details_instrument <- surgical_details_redcap_table_reactive() %>%
          pivot_wider(names_from = name, values_from = value) %>%
          mutate(record_id = record_number) %>%
          mutate(redcap_event_name = "surgery_arm_1") %>%
          mutate(redcap_repeat_instance = row_number() + surgical_details_instance_add) %>%
          mutate(redcap_repeat_instrument = "surgical_details") %>%
          mutate(surgical_details_complete = "Complete") %>%
          select(record_id, redcap_event_name, everything())
        
        if(all(names(surgical_details_instrument %>% select(-contains("redcap"))) %in% redcap_names_df$redcap_field_names)){
          importRecords(rcon = rcon_reactive$rcon, data = surgical_details_instrument, returnContent = "count")
          }else{
            tables_not_uploaded_list$surgical_details_instrument <- "surgical_details_instrument"
        }
        
        incProgress(1/number_of_steps, detail = paste("Uploading Intraoperative Details"))
        
        ###### Upload Intraoperative Details ####
        intraoperative_details_redcap_upload_df <- intraoperative_details_redcap_table_reactive() %>%
          pivot_wider(names_from = name, values_from = value) %>%
          mutate(record_id = record_number) %>%
          mutate(redcap_event_name = "surgery_arm_1") %>%
          mutate(redcap_repeat_instance = row_number() + surgical_details_instance_add) %>%
          mutate(redcap_repeat_instrument = "intraoperative_details") %>%
          mutate(intraoperative_details_complete = "Complete") %>%
          select(record_id, redcap_event_name, everything())
        
        if(all(names(intraoperative_details_redcap_upload_df %>% select(-contains("redcap"))) %in% redcap_names_df$redcap_field_names)){
          importRecords(rcon = rcon_reactive$rcon, data = intraoperative_details_redcap_upload_df, returnContent = "count")
        }else{
          tables_not_uploaded_list$intraoperative_details_redcap_upload_df <- "intraoperative_details_redcap_upload_df"
        }
      
        incProgress(1/number_of_steps, detail = paste("Uploading Data per Level"))
        
        ###### Upload repeating objects for all levels ####
        if(nrow(procedures_by_level_redcap_table_reactive())>0){
          procedures_by_level_repeating_instrument <- procedures_by_level_redcap_table_reactive() %>%
            mutate(record_id = record_number) %>% 
            mutate(redcap_event_name = "surgery_arm_1") %>%
            arrange(category) %>%
            mutate(redcap_repeat_instance = row_number() + procedures_by_level_repeating_instance_add) %>%
            mutate(redcap_repeat_instrument = "procedures_by_level_repeating") %>%
            mutate(procedures_by_level_repeating_complete = "Complete") %>%
            select(record_id, redcap_event_name, everything())
          
          if(all(names(procedures_by_level_repeating_instrument %>% select(-contains("redcap"))) %in% redcap_names_df$redcap_field_names)){
            importRecords(rcon = rcon_reactive$rcon, data = procedures_by_level_repeating_instrument, returnContent = "count")
          }else{
            tables_not_uploaded_list$procedures_by_level_repeating_instrument <- "procedures_by_level_repeating_instrument"
          }
          
        }
        
        
        incProgress(1/number_of_steps, detail = paste("Uploading Implant Data"))
        
        ##### uploaded screw details #######
        if(nrow(screw_details_redcap_table_reactive())>0){
          screw_details_repeating <- screw_details_redcap_table_reactive() %>%
            mutate(record_id = record_number) %>%
            mutate(redcap_event_name = "surgery_arm_1") %>%
            mutate(redcap_repeat_instance = row_number() + screw_details_repeating_instance_add) %>%
            mutate(redcap_repeat_instrument = "screw_details_repeating") %>%
            mutate(screw_details_repeating_complete = "Complete") %>%
            select(record_id, redcap_event_name, everything())
          
          if(all(names(screw_details_repeating %>% select(-contains("redcap"))) %in% redcap_names_df$redcap_field_names)){
            importRecords(rcon = rcon_reactive$rcon, data = screw_details_repeating, returnContent = "count")
          }else{
            tables_not_uploaded_list$screw_details_repeating <- "screw_details_repeating"
          }
          
        }
        
        incProgress(1/number_of_steps, detail = paste("Uploading Interbody Implant Data"))
        
        ##### uploaded interbody details #######
        if(nrow(interbody_details_redcap_table_reactive())>0){
          interbody_implant_repeating <- interbody_details_redcap_table_reactive() %>%
            mutate(record_id = record_number) %>%
            select(record_id, everything()) %>%
            mutate(redcap_event_name = "surgery_arm_1") %>%
            mutate(redcap_repeat_instance = row_number() + interbody_implant_repeating_instance_add) %>%
            mutate(redcap_repeat_instrument = "interbody_implant_repeating") %>%
            mutate(interbody_implant_repeating_complete = "Complete") %>%
            mutate(across(everything(), ~ paste0(as.character(.x)))) %>%
            select(record_id, redcap_event_name, everything())
          
          if(all(names(interbody_implant_repeating %>% select(-contains("redcap"))) %in% redcap_names_df$redcap_field_names)){
            importRecords(rcon = rcon_reactive$rcon, data = interbody_implant_repeating, returnContent = "count")
          }else{
            tables_not_uploaded_list$interbody_implant_repeating <- "interbody_implant_repeating"
          }
        }
        
        incProgress(1/number_of_steps, detail = paste("Uploading Rods crossing by Level Data"))
        
        ##### uploaded RODS CROSSING BY LEVEL details #######
        if(nrow(rods_crossing_by_level_redcap_table_reactive())>0){
          rods_crossing_by_level_repeating <- rods_crossing_by_level_redcap_table_reactive() %>%
            mutate(record_id = record_number) %>%
            select(record_id, everything()) %>%
            mutate(redcap_event_name = "surgery_arm_1") %>%
            mutate(redcap_repeat_instance = row_number() + rods_crossing_by_level_repeating_instance_add) %>%
            mutate(redcap_repeat_instrument = "rods_crossing_by_level_repeating") %>%
            mutate(rods_crossing_by_level_repeating_complete = "Complete") %>%
            mutate(across(everything(), ~ paste0(as.character(.x)))) %>%
            select(record_id, redcap_event_name, redcap_repeat_instrument,redcap_repeat_instance, dos_rods_crossing_repeating, rods_crossing_level, left_rod_count, left_rods_crossing, right_rod_count, right_rods_crossing, total_rods_crossing, contains("left"), contains("right"), rods_crossing_by_level_repeating_complete)
          
          
          # rods_crossing_by_level_repeating <- rods_crossing_by_level_redcap_table_reactive() %>%
          #   mutate(record_id = record_number) %>%
          #   select(record_id, everything()) %>%
          #   mutate(redcap_event_name = "surgery_arm_1") %>%
          #   mutate(redcap_repeat_instance = row_number() + rods_crossing_by_level_repeating_instance_add) %>%
          #   mutate(redcap_repeat_instrument = "rods_crossing_by_level_repeating") %>%
          #   mutate(rods_crossing_by_level_repeating_complete = "Complete") %>%
          #   mutate(across(everything(), ~ paste0(as.character(.x)))) %>%
          #   select(record_id, redcap_event_name, redcap_repeat_instrument,redcap_repeat_instance, dos_rods_crossing_repeating, rods_crossing_level, left_rod_count, left_rods_crossing, right_rod_count, right_rods_crossing, total_rods_crossing, rods_crossing_by_level_repeating_complete)

          if(all(names(rods_crossing_by_level_repeating %>% select(-contains("redcap"))) %in% redcap_names_df$redcap_field_names)){
            importRecords(rcon = rcon_reactive$rcon, data = rods_crossing_by_level_repeating, returnContent = "count")
            }else{
              tables_not_uploaded_list$rods_crossing_by_level_repeating <- "rods_crossing_by_level_repeating"
          }
        }
        
        # rods_crossing_by_level_redcap_table_reactive
        
        incProgress(1/number_of_steps, detail = paste("Uploading All Data Inputs"))
        
        ##### upload ALL INPUTS details #######
        if(nrow(all_inputs_trimmed_table_reactive())>0){
          
          all_inputs_repeating_df <- all_inputs_trimmed_table_reactive() %>%
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
          
          if(all(names(all_inputs_repeating_df %>% select(-contains("redcap"))) %in% redcap_names_df$redcap_field_names)){
            importRecords(rcon = rcon_reactive$rcon, data = all_inputs_repeating_df, returnContent = "count")
          }else{
            tables_not_uploaded_list$all_inputs_repeating_df <- "all_inputs_repeating_df"
          }
          
        }
        
        
        if(nrow(left_revision_implants_reactive_list()$revision_implants_status_df %>%
                bind_rows(right_revision_implants_reactive_list()$revision_implants_status_df))>0){
          
          incProgress(1/number_of_steps, detail = paste("Complete"))
          

          implant_removal_repeating_df <-  left_revision_implants_reactive_list()$revision_implants_status_df %>%
            bind_rows(right_revision_implants_reactive_list()$revision_implants_status_df) %>%
            select(level, side, object, remove_retain) %>%
            filter(remove_retain == "remove") %>%
            select(level, side, remove_retain, object) %>%
            clean_names() %>%
            mutate(dos_implant_removal_repeating = as.character(input$date_of_surgery)) %>%
            mutate(record_id = record_number) %>%
            mutate(redcap_repeat_instance = row_number() + implant_removal_repeating_instance_add) %>%
            mutate(redcap_event_name = "surgery_arm_1") %>%
            mutate(redcap_repeat_instrument = "implant_removal_repeating")%>%
            mutate(implant_removal_repeating_complete = "Complete") %>% 
            rename(level_implant_removal_repeating = level, 
                   side_implant_removal_repeating = side,
                   removed_implant_removal_repeating = object) %>%
            select(
              record_id,
              redcap_repeat_instance,
              redcap_event_name,
              redcap_repeat_instrument,
              dos_implant_removal_repeating,
              level_implant_removal_repeating,
              side_implant_removal_repeating,
              removed_implant_removal_repeating,
              implant_removal_repeating_complete)
          
          if(all(names(implant_removal_repeating_df %>% select(-contains("redcap"))) %in% redcap_names_df$redcap_field_names)){
            importRecords(rcon = rcon_reactive$rcon, data = implant_removal_repeating_df, returnContent = "count") 
          }else{
            tables_not_uploaded_list$implant_removal_repeating_df <- "implant_removal_repeating_df"
          }
        
          incProgress(1/number_of_steps, detail = paste("Complete"))
          
        }else{
          
          incProgress(1/number_of_steps, detail = paste("Complete"))
        }
        
        if(!is.na(logging_timer_reactive_list$elapsed_time))
          
          time_spent_logging_df <- tibble(record_id = record_number) %>%
          mutate(redcap_event_name = "surgery_arm_1") %>%
          mutate(redcap_repeat_instance = row_number() + logging_data_time_repeating_instance_add) %>%
          mutate(redcap_repeat_instrument = "logging_data_time_repeating") %>%
          mutate(logging_data_time_repeating_complete = "Complete") %>%
          mutate(across(everything(), ~ paste0(as.character(.x)))) %>%
          mutate(dos_logging_data_time_repeating = as.character(input$date_of_surgery), 
                 data_logging_time_seconds = as.character(logging_timer_reactive_list$elapsed_time)) %>%
          select(record_id, redcap_event_name, everything())
        
        if(all(names(time_spent_logging_df %>% select(-contains("redcap"))) %in% redcap_names_df$redcap_field_names)){
          importRecords(rcon = rcon_reactive$rcon, data = time_spent_logging_df, returnContent = "count") 
        }else{
          tables_not_uploaded_list$time_spent_logging_df <- "time_spent_logging_df"
        }
        
  
        
      })
      
      if(length(tables_not_uploaded_list)>0){
        completion_text <- glue("Data was successfully Uploaded. But, the following tables were not uploaded because the field names did not match the redcap database: {glue_collapse(tables_not_uploaded_list, sep = ', ', last = ', and ')}.")
      }else{
        completion_text <- "All tables were successfully uploaded."
      }

      sendSweetAlert(
        session = session,
        title = "Success !!",
        text = completion_text,
        type = "success"
      )
    }else{
      sendSweetAlert(
        session = session,
        title = "Upload Already Attempted",
        text = "You have already attempted to upload. Please check your redcap to confirm the data was uploaded. You will not be able to upload again during this session.",
        type = "info"
      )
    }
  })
  
  output$upload_completed_text <- renderText({
    
    if(final_upload_reactive_count$count < 1){
      # HTML(style = "font-size:20px; font-weight:bold; color:darkred; font-family:sans-serif; font-style:italic",
      #      "<div>", "Upload not yet completed.", "</div>")
      HTML(paste("<span style='font-size:20px; font-weight:bold; color:darkred; font-family:sans-serif; font-style:italic;'>Upload not yet completed</span>"))
    }else{
      # HTML(style = "font-size:20px; font-weight:bold; color:green; font-family:sans-serif; font-style:italic",
      #      "<div>", "Upload Completed", "</div>")
      HTML(paste("<span style='font-size:20px; font-weight:bold; color:green; font-family:sans-serif; font-style:italic;'>Upload Completed</span>"))
      
    }

  })
  
  
  
  
  ############################################    ##    ############################################    ############################################    ############################################    ############################################
  
  # task_items_reactive_list <- reactiveValues()
  # 
  # task_items_reactive_list$upload_to_redcap <- 0
  # 
  # observeEvent(input$confirm_upload_final, {
  #   task_items_reactive_list$upload_to_redcap <- 100
  # })
  
  
  #   output$upload_to_redcap_task <- renderMenu({
  #     dropdownMenu(type = "tasks", 
  #                  badgeStatus = if_else(task_items_reactive_list$upload_to_redcap == 100, "success", "warning"), 
  #                  taskItem(text = "Upload Data to Redcap", 
  #                           value = task_items_reactive_list$upload_to_redcap))
  #   })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
