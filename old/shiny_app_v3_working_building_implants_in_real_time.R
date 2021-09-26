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

# install.packages("htmlwidgets")
# library(htmlwidgets)

# source("functions.R", local = TRUE)
# source("labels_screws_functions.R", local = TRUE)

source("load_coordinates.R", local = TRUE)
source("hook_function.R", local = TRUE)
source("screw_function.R", local = TRUE)
source("osteotomies_decompressions_functions.R", local = TRUE)
# 
# source("hook_function.R", local = TRUE)

# source("modularize_implant_details.R", local = TRUE)

# source("hook_functions.R", local = TRUE)
#
# source("pcos_function.R", local = TRUE)
#
# source("decompression_functions.R", local = TRUE)
#
# source("implant_sizes_functions.R", local = TRUE)
# remotes::install_github("coolbutuseless/ggpattern")

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Spine Templating"),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      textInput(inputId = "patient_name", label = "Name:"),
      radioGroupButtons(
        inputId = "primary_revision",
        label = "Primary or Revision/Open Canal:",
        choices = c("Primary", "Revision or Open Canal"),
        justified = TRUE,
        selected = "Primary",
        size = "sm"
      ),
      pickerInput(
        inputId = "symptoms",
        label = "Primary Symptoms (check all that apply)",
        choices = list('Axial Pain' = c("Neck Pain", "Thoracic Pain", "Low Back Pain"), 
                       'Arm Pain' = c("Left Arm Pain", "Right Arm Pain"),
                       'Leg Pain' = c("Left Leg Pain", "Right Leg Pain"),
                       'Myelopathy' = c("Nurick Grade 1",
                                        "Nurick Grade 2",
                                        "Nurick Grade 3",
                                        "Nurick Grade 4",
                                        "Nurick Grade 5"),
                       'Other' = c("Other Symptoms")),
        multiple = TRUE
      ),
      textInput(inputId = "bone_density", label = "Bone Density (lowest T score or HU):"),
      textInput(inputId = "relevant_history", label = "Other Comments/History:"),
      h4("Spinal Anatomy:"),
      sliderInput(
        inputId = "number_of_lumbar_vertebrae",
        label = "Number of Lumbar Vertebrae:",
        min = 4,
        max = 6,
        value = 5,
        step = 1
      ),
      sliderInput(
        inputId = "number_of_thoracic_vertebrae",
        label = "Number of Thoracic (Ribbed) Vertebrae:",
        min = 11,
        max = 13,
        value = 12,
        step = 1
      )
    ),
    mainPanel(width = 10,
              tabsetPanel(type = "tabs",
                          tabPanel(title = "Spine Plan",
                                   fixedRow(
                                     column(
                                       width = 3,
                                       dropdown(icon = icon("grip-lines-vertical"), width = "150%",
                                                size = "sm",
                                                label = "Add or Customize Rod Construct",
                                                style = "material-flat",    
                                                animate = animateOptions(
                                                  enter = animations$fading_entrances$fadeInLeftBig,
                                                  exit = animations$fading_exits$fadeOutRightBig
                                                ),
                                                fixedRow(
                                                  radioGroupButtons(
                                                    inputId = "main_rods_only_vs_custom",
                                                    label = NULL,
                                                    choices = c("Main Rods Only", 
                                                                "Customize Construct"),
                                                    justified = TRUE
                                                  )
                                                ),
                                                fixedRow(
                                                  h4("Left Primary Rod:"),
                                                  column(width = 6,
                                                         sliderTextInput(
                                                           inputId = "left_main_rod",
                                                           label = "Left Main Rod Spans:",
                                                           choices = c("C1", "L5"),
                                                           selected = c("C1", "L5")),
                                                         radioGroupButtons(
                                                           inputId = "left_main_rod_size",size = "xs", justified = TRUE, direction = "vertical",
                                                           label = "Size:",
                                                           choices = c("Transition", "4.5mm", "4.75mm", "5.5mm", "6.0mm", "6.34mm/quarter in")
                                                         ),
                                                         radioGroupButtons(
                                                           inputId = "left_main_rod_material", size = "xs", justified = TRUE,direction = "vertical",
                                                           label = "Material:",
                                                           choices = c("Cobalt Chrome", "Stainless Steel", "Titanium")
                                                         )
                                                  ),
                                                  column(width = 6,
                                                         h4("Right Primary Rod:"),
                                                         sliderTextInput(
                                                           inputId = "right_main_rod",
                                                           label = "Right Main Rod Spans:",
                                                           choices = c("C1", "L5"),
                                                           selected = c("C1", "L5")),
                                                         radioGroupButtons(
                                                           inputId = "right_main_rod_size", size = "xs", justified = TRUE,direction = "vertical",
                                                           label = "Size:",
                                                           choices = c("Transition", "4.5mm", "4.75mm", "5.5mm", "6.0mm", "6.34mm/quarter in")
                                                         ),
                                                         radioGroupButtons(
                                                           inputId = "right_main_rod_material",size = "xs", justified = TRUE,direction = "vertical",
                                                           label = "Material:",
                                                           choices = c("Cobalt Chrome", "Stainless Steel", "Titanium")
                                                         )
                                                  )
                                                ),
                                                hr(),
                                                conditionalPanel(condition = "input.main_rods_only_vs_custom == 'Customize Construct'",
                                                                 fixedRow(
                                                                   column(width = 6, 
                                                                          materialSwitch(
                                                                            inputId = "left_accessory_rod_switch",
                                                                            label = "Add Left Accessory rod:",
                                                                            value = FALSE,
                                                                            status = "success"
                                                                          )
                                                                   ),
                                                                   column(width = 6,
                                                                          materialSwitch(
                                                                            inputId = "right_accessory_rod_switch",
                                                                            label = "Add Right Accessory Rod:",
                                                                            value = FALSE,
                                                                            status = "success"
                                                                          )
                                                                   )
                                                                 ),
                                                                 fixedRow(column(width = 6,
                                                                                 uiOutput(outputId = "left_accessory_rod_range_ui")
                                                                 ),
                                                                 column(width = 6,
                                                                        uiOutput(outputId = "right_accessory_rod_range_ui")
                                                                 )
                                                                 ),
                                                                 tags$hr(),
                                                                 fixedRow(
                                                                   column(width = 6, 
                                                                          materialSwitch(
                                                                            inputId = "left_satellite_rod_switch",
                                                                            label = "Add Left Satellite rod:",
                                                                            value = FALSE,
                                                                            status = "success"
                                                                          )
                                                                   ),
                                                                   column(width = 6,
                                                                          materialSwitch(
                                                                            inputId = "right_satellite_rod_switch",
                                                                            label = "Add Right Satellite Rod:",
                                                                            value = FALSE,
                                                                            status = "success"
                                                                          )
                                                                   )
                                                                 ),
                                                                 fixedRow(column(width = 6,
                                                                                 uiOutput(outputId = "left_satellite_rod_range_ui")
                                                                 ),
                                                                 column(width = 6,
                                                                        uiOutput(outputId = "right_satellite_rod_range_ui")
                                                                 )),
                                                                 tags$hr(),
                                                                 fixedRow(
                                                                   column(width = 6, 
                                                                          materialSwitch(
                                                                            inputId = "left_intercalary_rod_switch",
                                                                            label = "Add Left Intercalary rod:",
                                                                            value = FALSE,
                                                                            status = "success"
                                                                          )
                                                                   ),
                                                                   column(width = 6,
                                                                          materialSwitch(
                                                                            inputId = "right_intercalary_rod_switch",
                                                                            label = "Add Right Intercalary Rod:",
                                                                            value = FALSE,
                                                                            status = "success"
                                                                          )
                                                                   )
                                                                 ),
                                                                 fixedRow(column(width = 6,
                                                                                 uiOutput(outputId = "left_intercalary_rod_range_ui")
                                                                 ),
                                                                 column(width = 6,
                                                                        uiOutput(outputId = "right_intercalary_rod_range_ui")
                                                                 ))
                                                )
                                       ), 
                                       hr(),
                                       dropdown(icon = icon("screwdriver"),
                                                size = "sm", 
                                                width = "200%",
                                                label = "Add Pedicle Screw Details",
                                                style = "material-flat", 
                                                uiOutput(outputId = "pedicle_screw_details_ui")
                                       ),
                                       hr(),
                                       tableOutput(outputId = "coordinates_table_2"),
                                       textOutput(outputId = "no_screw"),
                                       tableOutput(outputId = "osteotomies_df")
                                     ),
                                     # column(width = 2,
                                     #        align = "center",
                                     #        # uiOutput(outputId = "special_implants_ui"),
                                     #        br(),
                                     #        br(),
                                     #        h4(strong("Implants & Other Details:")),
                                     #        pickerInput(
                                     #          inputId = "rod_size",
                                     #          label = "Rod Size:",
                                     #          choices = c("-", "5.5mm", "0.25in")),
                                     #        pickerInput(
                                     #          inputId = "rod_material",
                                     #          label = "Rod Material:",
                                     #          choices = c("-", "Titanium", "Cobalt Chrome")),
                                     #        textInput(inputId = "bmp_text", label = "BMP:"),
                                     #        textInput(inputId = "allograft_chips", label = "Cancellous Chips:"),
                                     #        textInput(inputId = "plan_free_text", label = "Other Plan Details:")),
                                     column(
                                       width = 9,
                                       h3("Surgical Plan:"),
                                       # column(width = 12, 
                                       #        column(width = 4, 
                                       #               htmlOutput(outputId = "surgical_summary")
                                       #               )
                                       #        ),
                                       # renderPrint("coordinates"),
                                       # uiOutput("open_canal_ui"),
                                       align = "center",
                                       column(width = 12,
                                              column(10,
                                                     fixedRow(
                                                       column(width = 2, 
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
                                                       column(width = 10,
                                                              plotOutput("spine_plan",
                                                                         height = 750,
                                                                         click = "plot_click", 
                                                                         dblclick = "plot_double_click"),
                                                              textInput(inputId = "coordinate_type", label = "Coordinate Type"),
                                                              actionButton(inputId = "save_coordinates_table_button", label = "Click to save coordinates table"),
                                                              textInput(inputId = "file_name", label = "Name of File (add .csv)")
                                                       )
                                                     )
                                              ),
                                              column(width = 2, 
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
                                                     radioGroupButtons(
                                                       inputId = "add_implants",
                                                       # status = "success", 
                                                       # size = "sm", 
                                                       direction = "vertical", 
                                                       justified = FALSE,
                                                       individual = FALSE, 
                                                       width = "120%",
                                                       checkIcon = list(
                                                         yes = tags$i(class = "fas fa-screwdriver", style = "color: steelblue")
                                                       ),
                                                       label = "Select Implant to add & then Click Spine to Plot Plan",
                                                       choices = c(
                                                         "Pedicle Screws",
                                                         "Pelvic Screws",
                                                         "Lateral Mass Screws",
                                                         "Pars Screws",
                                                         "Translaminar Screws",
                                                         "TP Hook",
                                                         "Laminar Hook (Downgoing)",
                                                         "Laminar Hook (Upgoing)",
                                                         "Pedicle Hook",
                                                         "Tether",
                                                         "Sublaminar Wire"
                                                       ),
                                                       selected = "Pedicle Screws"
                                                     ),
                                                     pickerInput(
                                                       inputId = "add_schwab_osteotomies",
                                                       label = "Select & Add Schwab Osteotomies:", 
                                                       choices = c(" ", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5", "Grade 6"),
                                                       options = list(
                                                         style = "btn-primary", title = "Select Grade Osteotomy"),
                                                       selected = NULL
                                                     )
                                              )
                                       ),
                                       # verbatimTextOutput("click_info")
                                       # dataTableOutput("left_screw_sizes_df")
                                       # fileInput("myFile", "Choose a file", accept = c('image/png', 'image/jpeg')),
                                       # div(id = "image-container", style = "display:flexbox")
                                     )
                                   )
                          ),
                          tabPanel(title = "Data Sheet",
                                   tableOutput(outputId = "coordinates_table")
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
  
  observeEvent(implants_list$left_implants_df, ignoreNULL = TRUE, ignoreInit = TRUE, {
    updateSliderTextInput(session = session, inputId = "left_main_rod", 
                          choices = unique(implants_list$left_implants_df$level),
                          selected = c(unique(implants_list$left_implants_df$level)[1], tail(unique(implants_list$left_implants_df$level), n = 1))
    )
    
  })
  
  observeEvent(implants_list$right_implants_df, ignoreNULL = TRUE, ignoreInit = TRUE, {
    updateSliderTextInput(session = session, inputId = "right_main_rod", 
                          choices = unique(implants_list$right_implants_df$level),
                          selected = c(unique(implants_list$right_implants_df$level)[1], tail(unique(implants_list$right_implants_df$level), n = 1))
    )
    
  })
  
  output$left_accessory_rod_range_ui <- renderUI({
    if(req(input$left_accessory_rod_switch) == FALSE){
      NULL
    }else{
      sliderTextInput(
        inputId = "left_accessory_rod", 
        label = "Left Accessory Rod Spans:",
        choices = unique(implants_list$left_implants_df$level),
        selected = c(unique(implants_list$left_implants_df$level)[1], tail(unique(implants_list$left_implants_df$level), n = 1))
      )
    }
  })
  
  observeEvent(implants_list$left_implants_df, ignoreNULL = TRUE, ignoreInit = TRUE, {
    updateSliderTextInput(session = session, inputId = "left_accessory_rod", 
                          choices = unique(implants_list$left_implants_df$level),
                          selected = isolate(input$left_accessory_rod)
    )
    
  })
  
  observeEvent(input$add_implants, ignoreInit = TRUE, {
    updatePickerInput(session = session, inputId = "add_schwab_osteotomies",
                      selected = " "
    )
    
  })
  
  output$left_satellite_rod_range_ui <- renderUI({
    if(req(input$left_satellite_rod_switch) == FALSE){
      NULL
    }else{
      
      implant_levels_df <- implants_list$left_implants_df %>%
        mutate(implant_level = paste(level, implant, sep = " ")) %>%
        mutate(implant_level = str_to_title(str_replace_all(implant_level, "_", " "))) %>%
        select(level, implant_level) %>%
        distinct()
      
      checkboxGroupButtons(
        inputId = "left_satellite_rod",
        label = "Left Satellite Rod Implants:",
        choices = implant_levels_df$implant_level,
        selected = isolate(input$left_satellite_rod)
      )
    }
  })
  output$left_intercalary_rod_range_ui <- renderUI({
    if(req(input$left_intercalary_rod_switch) == FALSE){
      NULL
    }else{
      sliderTextInput(
        inputId = "left_intercalary_rod",
        label = "Left Intercalary Rod Spans:",
        choices = unique(implants_list$left_implants_df$level),
        selected = isolate(input$left_intercalary_rod)
      )
    }
  })
  # 
  output$right_accessory_rod_range_ui <- renderUI({
    if(req(input$right_accessory_rod_switch) == FALSE){
      NULL
    }else{
      sliderTextInput(
        inputId = "right_accessory_rod",
        label = "Right Accessory Rod Spans:",
        choices = unique(implants_list$right_implants_df$level),
        selected = c(unique(implants_list$right_implants_df$level)[1], tail(unique(implants_list$right_implants_df$level), n = 1))
      )
    }
  })
  
  observeEvent(implants_list$right_implants_df, ignoreNULL = TRUE, ignoreInit = TRUE, {
    updateSliderTextInput(session = session, inputId = "right_accessory_rod", 
                          choices = unique(implants_list$right_implants_df$level),
                          selected = isolate(input$right_accessory_rod)
    )
    
  })
  
  
  output$right_satellite_rod_range_ui <- renderUI({
    if(req(input$right_satellite_rod_switch) == FALSE){
      NULL
    }else{
      implant_levels_df <- implants_list$right_implants_df %>%
        mutate(implant_level = paste(level, implant, sep = " ")) %>%
        mutate(implant_level = str_to_title(str_replace_all(implant_level, "_", " "))) %>%
        select(level, implant_level) %>%
        distinct()
      
      checkboxGroupButtons(
        inputId = "right_satellite_rod",
        label = "Right Satellite Rod Implants:",
        choices = implant_levels_df$implant_level,
        selected = isolate(input$right_satellite_rod)
      )
    }
  })
  
  output$right_intercalary_rod_range_ui <- renderUI({
    if(req(input$right_intercalary_rod_switch) == FALSE){
      NULL
    }else{
      right_slider <- sliderTextInput(
        inputId = "right_intercalary_rod",
        label = "Right Intercalary Rod Spans:",
        choices = unique(implants_list$right_implants_df$level),
        selected = isolate(input$right_intercalary_rod))
    }
  })
  
  #################################################################### CONSTRUCT IMPLANTS, DECOMPRESSIONS AND PLOT     ####################################################################
  
  output$click_info <- renderPrint({
    cat("input$plot_click:\n")
    str(input$plot_click)
  })
  
  
  implants_list <- reactiveValues()
  
  implants_list$implants_df <- tibble(implant = character(),
                                      level = character(),
                                      side = character(),
                                      vertebral_number = numeric(),
                                      x = numeric(),
                                      y = numeric())
  
  implants_list$left_implants_df <- tibble(implant = character(), 
                                           level = character(),
                                           side = character(),
                                           vertebral_number = numeric(),
                                           x = numeric(),
                                           y = numeric())
  
  implants_list$left_implants_constructed_df <- NULL
  
  implants_list$right_implants_df <- tibble(implant = character(), 
                                            level = character(),
                                            side = character(),
                                            vertebral_number = numeric(),
                                            x = numeric(),
                                            y = numeric())
  
  implants_list$right_implants_constructed_df <- NULL
  
  implants_list$tethers_wires_df <- tibble(implant = character(), 
                                           level = character(),
                                           vertebral_number = numeric(),
                                           x = numeric(),
                                           y = numeric(),
                                           length = numeric())
  
  decompressions_osteotomies_list <- reactiveValues()
  
  decompressions_osteotomies_list$osteotomies_df <- tibble(level = character(),
                                                           grade = character(), 
                                                           side = character(),
                                                           lateral_x = numeric(),
                                                           superior_y = numeric(),
                                                           medial_x = numeric(),
                                                           inferior_y = numeric())
  
  # implants_list$tethers_wires_df <- NULL
  
  
  ########################################### IMPLANT DETAILS REACTIVE ###########################################
  # implant_details_to_select_function(levels_vector = implants_list$implants_df$level)
  
  implant_type_selected <- reactive({
    implant_type <- case_when(
      input$add_implants == "Pedicle Screws" ~ "pedicle_screw",
      input$add_implants == "Pelvic Screws" ~ "pelvic_screw",
      input$add_implants == "Lateral Mass Screws" ~ "lateral_mass_screw",
      input$add_implants == "Pars Screws" ~ "pars_screw",
      input$add_implants == "Translaminar Screws" ~ "translaminar_screw",
      input$add_implants == "TP Hook" ~ "tp_hook",
      input$add_implants == "Laminar Hook (Downgoing)" ~ "laminar_downgoing_hook",
      input$add_implants == "Laminar Hook (Upgoing)" ~ "laminar_upgoing_hook",
      input$add_implants == "Tether" ~ "tether",
      input$add_implants == "Sublaminar Wire" ~ "sublaminar_wire",
      input$add_implants == "Pedicle Hook" ~ "pedicle_hook"
    )
    implant_type
  })
  
  implant_added_reactive_df <- reactive({
    
    implant_type_filtered_df <- implant_starts_df %>%
      filter(implant == implant_type_selected()) %>%
      remove_empty()
    
    implant_df <- nearPoints(
      df = implant_type_filtered_df,
      coordinfo = input$plot_click,
      xvar = "x",
      yvar = "y",
      maxpoints = 1,
      threshold = 20
    ) %>%
      mutate(rod = "main_rod")
  })
  
  
  #### OBSERVE THE PLOT CLICK AND ADD APPROPRIATE IMPLANT ####
  
  observeEvent(input$plot_click, {
    
    if(input$add_schwab_osteotomies == "Grade 1" | 
       input$add_schwab_osteotomies == "Grade 2" | 
       input$add_schwab_osteotomies == "Grade 3" |
       input$add_schwab_osteotomies == "Grade 4" | 
       input$add_schwab_osteotomies == "Grade 5" | 
       input$add_schwab_osteotomies == "Grade 6"){
      
      decompressions_osteotomies_list$osteotomies_df <- implant_df <- nearPoints(
        df = (schwab_points_df %>% filter(grade == input$add_schwab_osteotomies)),
        coordinfo = input$plot_click,
        xvar = "medial_x",
        yvar = "inferior_y",
        maxpoints = 1,
        threshold = 15
      ) %>%
        union_all(decompressions_osteotomies_list$osteotomies_df) %>%
        distinct() %>%
        mutate(grade_number = as.numeric(str_remove_all(string = grade,pattern = "Grade "))) %>%
        group_by(level) %>%
        filter(grade_number == max(grade_number)) %>%
        ungroup()
      
    }else{
      if(implant_type_selected() == "tether"){
        
        implants_list$tethers_wires_df <- implant_added_reactive_df() %>%
          # select(level, implant, x, y, length) %>%
          union_all(implants_list$tethers_wires_df) %>%
          distinct()
        
      }else{
        implants_list$left_implants_df <- implant_added_reactive_df() %>%
          union_all(implants_list$left_implants_df) %>%
          arrange(vertebral_number) %>%
          filter(side == "left") %>%
          distinct()
        
        implants_list$right_implants_df <- implant_added_reactive_df() %>%
          union_all(implants_list$right_implants_df) %>%
          arrange(vertebral_number) %>%
          filter(side == "right") %>%
          distinct()
      }
    }
    
    
    
    
  })
  
  observeEvent(input$plot_double_click, {
    implant_type_filtered_df <- implant_starts_df %>%
      filter(implant == implant_type_selected()) %>%
      remove_empty() 
    
    implant_to_remove_df <- nearPoints(
      df = implant_type_filtered_df,
      coordinfo = input$plot_double_click,
      xvar = "x",
      yvar = "y",
      maxpoints = 1,
      threshold = 15
    ) 
    
    implants_list$left_implants_df <- implants_list$left_implants_df %>%
      anti_join(implant_to_remove_df)
    
    implants_list$right_implants_df <- implants_list$right_implants_df %>%
      anti_join(implant_to_remove_df)
    
    implants_list$tethers_wires_df <- implants_list$tethers_wires_df %>%
      anti_join(implant_to_remove_df)
  })
  
  
  
  ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
  ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
  ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
  ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
  
  
  ######################### LEFT #########################
  observe({
    left_implants_active_df <- implants_list$left_implants_df
    
    if(nrow(implants_list$left_implants_df) >0){
      if(length(input$left_satellite_rod)>1){
        left_satellite_rod_levels_df <- tibble(levels_implant = input$left_satellite_rod, side = "left") %>%
          separate(col = levels_implant, into = c("level", "implant"), sep = " ", extra = "merge") %>%
          mutate(implant = str_replace_all(string = str_to_lower(implant), pattern = " ", replacement = "_")) 
        
        left_satellite_rod_levels_with_sat_labels_df <- left_satellite_rod_levels_df %>%
          left_join(implant_starts_df) %>%
          mutate(rod = "satellite_rod")
        
        left_final_implants_df <- implants_list$left_implants_df %>%
          anti_join(left_satellite_rod_levels_df) %>%
          mutate(rod = "main_rod") %>%
          union_all(left_satellite_rod_levels_with_sat_labels_df)
        
        left_final_implants_df
      }else{
        left_final_implants_df <- implants_list$left_implants_df %>%
          mutate(rod = "main_rod")
        
        left_final_implants_df
      }
      left_implants_constructed_df <- left_final_implants_df %>%
        mutate(sublaminar_band_length = length) %>%
        mutate(implant_constructed = pmap(.l =  list(..1 = implant, 
                                                     ..2 = x, 
                                                     ..3 = y, 
                                                     ..4 = angle,
                                                     ..5 = length, 
                                                     ..6 = width,
                                                     ..7 = rod, 
                                                     ..8 = sublaminar_band_length), .f = ~ screw_hook_implant_function(implant_type = ..1, main_or_sattelite_rod = ..7,
                                                                                                                       start_x = ..2,
                                                                                                                       y = ..3,
                                                                                                                       angle = ..4,
                                                                                                                       screw_length_mod = ..5,
                                                                                                                       screw_width_mod = ..6, sublaminar_band_length = ..8)))
      
      implants_list$left_implants_constructed_df <- left_implants_constructed_df
      
    }else{
      implants_list$left_implants_constructed_df <- NULL
    }
  })
  
  left_rods <- reactiveValues()
  right_rods <- reactiveValues()
  
  left_rods$left_main_rod_reactive_sf <- reactive({
    if(nrow(implants_list$left_implants_df) > 1){
      
      rod_matrix <- implants_list$left_implants_constructed_df %>%
        filter(rod == "main_rod") %>%
        select(x, y) %>%
        arrange(y) %>%
        distinct() %>%
        as.matrix()  
      rod_sf <- st_buffer(st_linestring(rod_matrix), dist = 0.003, endCapStyle = "ROUND")
      rod_sf
    }else{
      NULL
    }
  })
  
  left_rods$left_accessory_rod_reactive_sf <- reactive({
    if(nrow(implants_list$left_implants_df) > 1 & length(input$left_accessory_rod) > 1){
      rod_matrix <- tibble(level = input$left_accessory_rod, side = "left") %>%
        left_join(implants_list$left_implants_df) %>%
        select(x, y) %>%
        mutate(x = x + 0.015) %>%
        arrange(y) %>%
        distinct() %>%
        as.matrix()  
      
      rod_sf <- st_buffer(st_linestring(rod_matrix), dist = 0.003, endCapStyle = "ROUND")
      rod_sf
    }else{
      NULL
    }
  })
  
  
  left_rods$left_satellite_rod_reactive_sf <- reactive({
    if(nrow(implants_list$left_implants_df) > 1){
      if(length(input$left_satellite_rod) > 1){
        rod_matrix <- implants_list$left_implants_constructed_df %>%
          filter(rod == "satellite_rod") %>%
          select(x, y) %>%
          mutate(x = x - 0.02) %>%
          arrange(y) %>%
          distinct() %>%
          as.matrix()  
        rod_sf <- st_buffer(st_linestring(rod_matrix), dist = 0.003, endCapStyle = "ROUND")
        rod_sf
      }else{
        NULL
      }
    }
    
  })
  
  
  ############### RIGHT IMPLANTS AND RODS #################
  
  observe({
    right_implants_active_df <- implants_list$right_implants_df
    
    if(nrow(implants_list$right_implants_df) >0){
      if(length(input$right_satellite_rod)>1){
        right_satellite_rod_levels_df <- tibble(levels_implant = input$right_satellite_rod, side = "right") %>%
          separate(col = levels_implant, into = c("level", "implant"), sep = " ", extra = "merge") %>%
          mutate(implant = str_replace_all(string = str_to_lower(implant), pattern = " ", replacement = "_")) 
        
        right_satellite_rod_levels_with_sat_labels_df <- right_satellite_rod_levels_df %>%
          left_join(implant_starts_df) %>%
          mutate(rod = "satellite_rod")
        
        right_final_implants_df <- implants_list$right_implants_df %>%
          anti_join(right_satellite_rod_levels_df) %>%
          mutate(rod = "main_rod") %>%
          union_all(right_satellite_rod_levels_with_sat_labels_df)
        
        right_final_implants_df
      }else{
        right_final_implants_df <- implants_list$right_implants_df %>%
          mutate(rod = "main_rod")
        
        right_final_implants_df
      }
      right_implants_constructed_df <- right_final_implants_df %>%
        mutate(sublaminar_band_length = length) %>%
        mutate(implant_constructed = pmap(.l =  list(..1 = implant, 
                                                     ..2 = x, 
                                                     ..3 = y, 
                                                     ..4 = angle,
                                                     ..5 = length, 
                                                     ..6 = width,
                                                     ..7 = rod, 
                                                     ..8 = sublaminar_band_length), .f = ~ screw_hook_implant_function(implant_type = ..1, main_or_sattelite_rod = ..7,
                                                                                                                       start_x = ..2,
                                                                                                                       y = ..3,
                                                                                                                       angle = ..4,
                                                                                                                       screw_length_mod = ..5,
                                                                                                                       screw_width_mod = ..6,
                                                                                                                       sublaminar_band_length = ..8)))
      
      implants_list$right_implants_constructed_df <- right_implants_constructed_df
      
    }else{
      implants_list$right_implants_constructed_df <- NULL
    }
  })
  
  
  right_rods$right_main_rod_reactive_sf <- reactive({
    if(nrow(implants_list$right_implants_df) > 1){
      
      rod_matrix <- implants_list$right_implants_constructed_df %>%
        filter(rod == "main_rod") %>%
        select(x, y) %>%
        arrange(y) %>%
        distinct() %>%
        as.matrix()  
      rod_sf <- st_buffer(st_linestring(rod_matrix), dist = 0.003, endCapStyle = "ROUND")
      rod_sf
    }else{
      NULL
    }
  })
  
  right_rods$right_accessory_rod_reactive_sf <- reactive({
    if(nrow(implants_list$right_implants_df) > 1 & length(input$right_accessory_rod) > 1){
      rod_matrix <- tibble(level = input$right_accessory_rod, side = "right") %>%
        left_join(implants_list$right_implants_df) %>%
        select(x, y) %>%
        mutate(x = x - 0.015) %>%
        arrange(y) %>%
        distinct() %>%
        as.matrix()  
      
      rod_sf <- st_buffer(st_linestring(rod_matrix), dist = 0.003, endCapStyle = "ROUND")
      rod_sf
    }else{
      NULL
    }
  })
  
  right_rods$right_satellite_rod_reactive_sf <- reactive({
    if(nrow(implants_list$right_implants_df) > 1){
      if(length(input$right_satellite_rod) > 1){
        rod_matrix <- implants_list$right_implants_constructed_df %>%
          filter(rod == "satellite_rod") %>%
          select(x, y) %>%
          mutate(x = x + 0.02) %>%
          arrange(y) %>%
          distinct() %>%
          as.matrix()  
        rod_sf <- st_buffer(st_linestring(rod_matrix), dist = 0.003, endCapStyle = "ROUND")
        rod_sf
      }else{
        NULL
      }
    }
    
  })
  
  
  tethers_reactive_sf <- reactive({
    if(nrow(implants_list$tethers_wires_df %>% filter(implant == "tether")) > 0){      
      
      tethers_df <- implants_list$tethers_wires_df %>%
        filter(implant == "tether") %>%
        distinct() %>%
        mutate(tether_implants_constructed = map2(.x = y, .y = length, .f = ~tether_function(tether_start_y = .x, tether_length = .y)))
      
      st_multipolygon(tethers_df$tether_implants_constructed)
    }else{
      NULL
    }
    
  })
  
  
  
  ########### OSTEOTOMIES
  
  osteotomy_reactive_df <- reactive({
    if(nrow(decompressions_osteotomies_list$osteotomies_df) > 0){
      osteotomy_df <- decompressions_osteotomies_list$osteotomies_df %>%
        mutate(osteotomy_constructed = pmap(list(..1 = level, 
                                                 ..2 = grade,
                                                 ..3 = lateral_x,
                                                 ..4 = superior_y,
                                                 ..5 = medial_x,
                                                 ..6 = inferior_y), .f = ~build_osteotomy_function(level = ..1,
                                                                                                   osteotomy_grade = ..2,
                                                                                                   lateral_x = ..3,
                                                                                                   superior_y = ..4,
                                                                                                   medial_x = ..5, 
                                                                                                   inferior_y = ..6)))
      
    } else {
      osteotomy_df <- decompressions_osteotomies_list$osteotomies_df
    }
    osteotomy_df
  })
  
  posterior_spine_image <- reactiveFileReader(
    intervalMillis = 100,
    readFunc = image_read,
    session = NULL,
    filePath = "posterior_spine_figure.png"
  )
  
  #################### MAKE THE PLOT #######################
  #################### MAKE THE PLOT #######################
  #################### MAKE THE PLOT #######################
  
  output$spine_plan <- renderPlot({
    
    if(length(implants_list$left_implants_constructed_df$implant_constructed) > 0){
      left_implants_sf <- st_multipolygon(req(implants_list$left_implants_constructed_df$implant_constructed))
    }else{
      left_implants_sf <- NULL
    }
    
    if(length(implants_list$right_implants_constructed_df$implant_constructed) > 0){
      right_implants_sf <- st_multipolygon(req(implants_list$right_implants_constructed_df$implant_constructed))
    }else{
      right_implants_sf <- NULL
    }
    
    if(!is.null(tethers_reactive_sf())){
      tethers_sf <- tethers_reactive_sf()
    }else{
      tethers_sf <- NULL
    }
    
    #### Assemble OSTEOTOMIES ####
    osteotomy_df <- osteotomy_reactive_df()
    
    if(nrow(osteotomy_df %>%
            filter(grade == "Grade 1")) > 0){
      grade_1_df <- osteotomy_df %>%
        filter(grade == "Grade 1")
      grade_1_sf <- st_multilinestring(grade_1_df$osteotomy_constructed)
    }else{
      grade_1_sf <- NULL
    }
    
    if(nrow(osteotomy_df %>%
            filter(grade == "Grade 2")) > 0){
      grade_2_df <- osteotomy_df %>%
        filter(grade == "Grade 2")
      grade_2_sf <- st_multipolygon(grade_2_df$osteotomy_constructed)
    }else{
      grade_2_sf <- NULL
    }
    
    if(nrow(osteotomy_df %>%
            filter(grade == "Grade 3")) > 0){
      grade_3_df <- osteotomy_df %>%
        filter(grade == "Grade 3")
      grade_3_sf <- st_multipolygon(grade_3_df$osteotomy_constructed)
    }else{
      grade_3_sf <- NULL
    }
    
    if(nrow(osteotomy_df %>%
            filter(grade == "Grade 4")) > 0){
      grade_4_df <- osteotomy_df %>%
        filter(grade == "Grade 4")
      grade_4_sf <- st_multipolygon(grade_4_df$osteotomy_constructed)
    }else{
      grade_4_sf <- NULL
    }
    
    if(nrow(osteotomy_df %>%
            filter(grade == "Grade 5")) > 0){
      grade_5_df <- osteotomy_df %>%
        filter(grade == "Grade 5")
      grade_5_sf <- st_multipolygon(grade_5_df$osteotomy_constructed)
    }else{
      grade_5_sf <- NULL
    }
    if(nrow(osteotomy_df %>%
            filter(grade == "Grade 6")) > 0){
      grade_6_df <- osteotomy_df %>%
        filter(grade == "Grade 6")
      grade_6_sf <- st_multipolygon(grade_6_df$osteotomy_constructed)
    }else{
      grade_6_sf <- NULL
    }
    # 
    
    #################### MAKE THE PLOT #######################
    
    spine_plot <- ggdraw() +
      draw_image(
        posterior_spine_image(),
        scale = 1,
        y = 0,
        valign = 0,
        x = 0,
        width = 1
      ) +
      draw_text(
        text = labels_df$labels,
        x = 0.3 - input$label_text_offset/100,
        y = labels_df$y,
        size = input$label_text_size,
        fontface = "bold"
      ) +
      draw_text(
        text = labels_df$labels,
        x = 0.7 + input$label_text_offset/100,
        y = labels_df$y,
        size = input$label_text_size,
        fontface = "bold"
      ) +
      geom_sf(data = grade_1_sf, color = "red") +
      ggpattern::geom_sf_pattern(
        data = grade_2_sf, 
        pattern = "crosshatch",
        alpha = 0.8,
        pattern_colour = "red",
        pattern_fill = "black",
        pattern_density = 0.02,
        pattern_spacing = 0.01,
        pattern_angle = 70
      ) +
      ggpattern::geom_sf_pattern(
        data = grade_3_sf, 
        pattern = "crosshatch",
        alpha = 0.8,
        pattern_colour = "red",
        pattern_fill = "black",
        pattern_density = 0.02,
        pattern_spacing = 0.01,
        pattern_angle = 70
      ) +
      # ggpattern::geom_sf_pattern(
      #     data = grade_4_sf, 
      #     pattern = "crosshatch",
      #     alpha = 0.8,
      #     pattern_colour = "red",
      #     pattern_fill = "black",
      #     pattern_density = 0.02,
      #     pattern_spacing = 0.01,
      #     pattern_angle = 70
      # ) +
      ggpattern::geom_sf_pattern(
        data = left_implants_sf, 
        pattern = "stripe",
        pattern_fill = "blue",
        alpha = 0.8,
        pattern_colour = "blue",
        pattern_density = 0.02,
        pattern_spacing = 0.01,
        pattern_angle = 70
      ) +
      ggpattern::geom_sf_pattern(
        data = right_implants_sf,
        pattern = "stripe",
        pattern_fill = "blue",
        alpha = 0.8,
        pattern_colour = "blue",
        pattern_density = 0.02,
        pattern_spacing = 0.01,
        pattern_angle = 20
      ) +
      # theme_minimal_grid() +
      geom_sf(data = left_rods$left_main_rod_reactive_sf(), alpha = 0.75) +
      geom_sf(data = left_rods$left_accessory_rod_reactive_sf(), alpha = 0.5, fill = "green") +
      geom_sf(data = left_rods$left_satellite_rod_reactive_sf(), alpha = 0.5, fill = "blue") +
      # geom_sf(data = left_rods$left_intercalary_rod_reactive_sf(), alpha = 0.5, fill = "red") +
      geom_sf(data = right_rods$right_main_rod_reactive_sf(), alpha = 0.5) +
      geom_sf(data = right_rods$right_accessory_rod_reactive_sf(), alpha = 0.5, fill = "green") +
      geom_sf(data = right_rods$right_satellite_rod_reactive_sf(), alpha = 0.5, fill = "blue") +
      # geom_sf(data = right_rods$right_intercalary_rod_reactive_sf(), alpha = 0.5, fill = "red") +
      geom_sf(data = tethers_reactive_sf()) +
      # geom_sf(data = right_rods$right_rod_reactive_sf(), alpha = 0.5) +
      ylim(input$crop_y[1], input$crop_y[2]) +
      xlim(0.25- input$label_text_offset/100, 0.75 + input$label_text_offset/100)
    
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
    
    all_implants <- implants_list$left_implants_df %>%
      union_all(implants_list$right_implants_df) %>%
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
  
  
  
  output$coordinates_table <- renderTable({
    all_implants <- implants_list$left_implants_df %>%
      union_all(implants_list$right_implants_df) %>%
      select(level, vertebral_number, implant, side) %>%
      distinct() %>%
      arrange(vertebral_number)
    
    all_implants
    
  })
  
  output$osteotomies_df <- renderTable({
    decompressions_osteotomies_list$osteotomies_df
    
  })
  
  output$surgical_summary <- renderUI({
    all_implants <- implants_list$left_implants_df %>%
      union_all(implants_list$right_implants_df) %>%
      select(level, vertebral_number, implant, side) %>%
      distinct() %>%
      arrange(vertebral_number)
    
    all_implants
    
    uiv_df <- all_implants %>%
      filter(implant != "tether") %>%
      filter(vertebral_number == min(vertebral_number)) 
    
    liv_df <- all_implants %>%
      filter(implant != "tether") %>%
      filter(vertebral_number == max(vertebral_number))
    
    HTML(paste(paste("UIV = ", uiv_df$level[[1]]), paste("LIV = ", liv_df$level[[1]]), sep = "<br/>"))
  })
  
  
  
  
  
  ########################################  RECORD A LIST OF POINTS ## ############################
  
  
  point_clicks <- reactiveValues()
  
  point_clicks$click_coordinates_df <- tibble(coordinate_type = character(), 
                                              side = character(),
                                              x = numeric(),
                                              y = numeric())
  
  #### OBSERVE THE PLOT CLICK AND ADD APPROPRIATE IMPLANT ####
  
  observeEvent(input$plot_double_click, {
    
    x <- seq(from = 0.4, to = 0.6, by = 0.001)
    
    y <- seq(from = 0.1, to = 0.9, by = 0.001)
    
    xy_grid <- expand.grid(x, y) %>%
      as_tibble() %>%
      rename(x = Var1, y = Var2) %>%
      mutate(side = if_else(x < 0.5, "left", "right"))
    
    add_nearest_row <-
      nearPoints(
        df = xy_grid,
        coordinfo = input$plot_double_click,
        xvar = "x",
        yvar = "y",
        maxpoints = 1,
        threshold = 15
      ) %>%
      mutate(coordinate_type = input$coordinate_type) %>%
      mutate(side = if_else(x < 0.5, "left", "right"))
    
    point_clicks$click_coordinates_df <- union_all(x = point_clicks$click_coordinates_df, y = add_nearest_row)
    
    point_clicks$click_coordinates_final_df <- point_clicks$click_coordinates_df %>%
      remove_missing()
  })
  
  output$coordinates_table_2 <- renderTable({
    # clicks_list
    point_clicks$click_coordinates_final_df
    
    input$save_coordinates_table_button
    isolate(write_csv(x = point_clicks$click_coordinates_final_df, file = input$file_name))
  })
  
  
  ################################# REDCAP API #########################
  
  # rcon <- redcapConnection(url = 'https://redcap.wustl.edu/redcap/api/', token = "4B2AA28A4D6EFBC6CA1F49EC0FB385F7")
  
  # patient_df <- isolate(exportRecords(rcon, records = url_text()))
  
  # rcon <- redcapConnection(url = 'https://redcap.wustl.edu/redcap/api/', token = "4B2AA28A4D6EFBC6CA1F49EC0FB385F7")
  # 
  # 
  # redcap_exported_df <- exportRecords(rcon = rcon)
  # 
  # 
  # 
  # number_of_records <- redcap_exported_df %>%
  #     select(record_id) %>%
  #     mutate(record_id = as.double(record_id)) %>%
  #     filter(record_id == max(record_id)) %>%
  #     distinct()
  # 
  # new_record_number <- number_of_records[[1]] +1
  # 
  # upload_test <- implant_starts_df %>%
  #     select(level, side, implant) %>%
  #     filter(implant == "pedicle_screw", level == "l1") %>%
  #     mutate(level_side = paste(level, side, sep = "_")) %>%
  #     select(implant, level_side) %>%
  #     pivot_wider(names_from = level_side, values_from = implant) %>%
  #     mutate(last_name = "test_name") %>%
  #     mutate(record_id = new_record_number) %>% 
  #     select(record_id, last_name, everything())
  # 
  # 
  # importRecords(rcon = rcon, data = upload_test, returnContent = "count")
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
