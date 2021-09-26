sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Patient Details & Surgical Procedures", 
             tabName = "patient_details_procedures", 
             icon = icon("screwdriver")
    ), 
    menuItem(text = "Implant & Additional Procedural Details", 
             tabName = "implant_details", 
             icon = icon("screwdriver"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "patient_details_procedures",
            column(width = 4, 
                   box(width = 12,
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
                         column(width = 5, 
                                h5(strong("Diagnosis Subgroup:")) 
                         ),
                         column(width = 7,
                                checkboxGroupButtons(
                                  inputId = "spine_dx_subgroup",
                                  label = NULL,
                                  size = "xs",
                                  justified = TRUE,
                                  width = "100%",
                                  # individual = TRUE,
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
                       fluidRow(
                         column(width = 5, 
                                h5(strong("Symptoms:")) 
                         ),
                         column(width = 7,
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
                   box(width = 12, 
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
                                        fluidRow(column(width = 6,
                                                        h5(strong("Select Levels with Open Canal:"))
                                        ),
                                        column(width = 6,
                                               pickerInput(
                                                 inputId = "open_canal",
                                                 label = NULL, 
                                                 choices = open_canal_df$level,
                                                 multiple = TRUE
                                               )
                                        )
                                        ),
                                        fluidRow(
                                          column(width = 6,
                                                 h5(strong("Select Prior Fusion Levels:"))
                                          ),
                                          column(width = 6,
                                                 pickerInput(
                                                   inputId = "prior_fusion_levels",
                                                   label = NULL, 
                                                   choices = unique(interbody_levels_df$level),
                                                   multiple = TRUE
                                                 )
                                          )
                                        ),
                                        fluidRow(
                                          column(width = 6,
                                                 h5(strong("Select Levels to remove instrumentation:"))
                                          ),
                                          column(width = 6,
                                                 pickerInput(
                                                   inputId = "removal_instrumentation_levels",
                                                   label = NULL, 
                                                   choices = unique(labels_df$level),
                                                   multiple = TRUE
                                                 )
                                          )
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
                   )
            ),
            column(width = 8, 
                   box(width = 12,
                       fluidRow(
                         h4(strong("Select Approach:")),
                       ),
                       fluidRow(column(12, 
                                       radioGroupButtons(
                                         inputId = "spine_approach",
                                         label = NULL,
                                         choices = c("Anterior",
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
                       )
                   ),
                   box(width = 12,
                       fluidRow(
                         column(width = 9, 
                                h3(strong("Surgical Procedure")),
                                h4("Add procedures & implants in the order they were performed."),
                         ),
                         column(width = 3, 
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
                                )
                         )
                       ),
                       fluidRow(
                         column(width = 1, 
                                br(),
                                br(),
                                br(),
                                noUiSliderInput(inputId = "crop_y",
                                                label = "Spine Region",
                                                min = 0,
                                                max = 1,
                                                value = c(0.05,0.45), 
                                                direction = "rtl",
                                                behaviour = "drag",
                                                color = "#0036FD",
                                                orientation = "vertical",
                                                height = "600px", width = "3px",
                                                inline = TRUE)
                         ),
                         column(width = 8,
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
            )
    ),
    tabItem(tabName = "implant_details",
            h2("Add Implant Details")
    )
  )
)