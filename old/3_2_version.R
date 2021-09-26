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

source("screw_function.R", local = TRUE)

source("hook_function.R", local = TRUE)

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
      textInput(inputId = "symptoms", label = "Primary Symptoms:"),
      textInput(inputId = "bone_density", label = "Bone Density (lowest T score):"),
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
      ),
      radioGroupButtons(
        inputId = "primary_revision",
        label = "Primary or Revision/Open Canal:",
        choices = c("Primary", "Revision or Open Canal"),
        justified = TRUE,
        selected = "Primary",
        size = "sm"
      )
    ),
    mainPanel(width = 10,
              fixedRow(
                column(
                  width = 4,
                  dropdown(icon = icon("grip-lines-vertical"),
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
                           uiOutput(outputId = "main_rod_ui"),
                           hr(),
                           uiOutput(outputId = "accessory_rods_ui"),
                           fixedRow(column(width = 6,
                                           uiOutput(outputId = "left_accessory_rod_range_ui")
                           ),
                           column(width = 6,
                                  uiOutput(outputId = "right_accessory_rod_range_ui")
                           )
                           ),
                           tags$hr(),
                           uiOutput(outputId = "satellite_rods_ui"), 
                           fixedRow(column(width = 6,
                                           uiOutput(outputId = "left_satellite_rod_range_ui")
                           ),
                           column(width = 6,
                                  uiOutput(outputId = "right_satellite_rod_range_ui")
                           )),
                           tags$hr(),
                           uiOutput(outputId = "intercalary_rods_ui"),
                           fixedRow(column(width = 6,
                                           uiOutput(outputId = "left_intercalary_rod_range_ui")
                           ),
                           column(width = 6,
                                  uiOutput(outputId = "right_intercalary_rod_range_ui")
                           ))
                  ), 
                  hr(),
                  dropdown(icon = icon("screwdriver"),
                           size = "sm", 
                           label = "Add Pedicle Screw Details",
                           style = "material-flat", 
                           uiOutput(outputId = "pedicle_screw_details_ui")
                  ),
                  hr(),
                  tableOutput(outputId = "coordinates_table"),
                  textOutput(outputId = "no_screw")
                ),
                # column(width = 5,
                #        h3("Screws & Decompression Levels"),
                #        # uiOutput(outputId = "screws_decompression_configuration"),
                #        # column(width = 6,
                #               # uiOutput(outputId = "left_c2_implant")),
                #        # column(width = 6,
                #               # uiOutput(outputId = "right_c2_implant")),
                #        # uiOutput(outputId = "screw_sizes_ui")
                # ),
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
                  width = 8,
                  h3("Surgical Plan:"),
                  column(width = 12, 
                         column(width = 8, 
                                pickerInput(
                                  inputId = "add_implants",
                                  label = "Select Implant to add & then Click Spine to Plot Plan",
                                  choices = list(
                                    Implant = c(
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
                                    Decompression = c("A",
                                                      "B", "C", "D")
                                  )
                                ), 
                                # textInput(inputId = "coordinate_type", label = "Coordinate Type"),
                                # actionButton(inputId = "save_coordinates_table_button", label = "Click to save coordinates table"),
                                # textInput(inputId = "file_name", label = "Name of File (add .csv)")
                         ),
                         column(width = 4, 
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
                                htmlOutput(outputId = "surgical_summary")
                         )
                  ),
                  # renderPrint("coordinates"),
                  # uiOutput("open_canal_ui"),
                  align = "center",
                  column(width = 12,
                         column(width = 2, 
                                noUiSliderInput(inputId = "crop_y", 
                                                label = "Crop Y", 
                                                min = 0, 
                                                max = 1, 
                                                value = c(0,1), direction = "rtl", behaviour = "drag",
                                                orientation = "vertical", height = "500px", width = "5px")),
                         column(width = 10, 
                                plotOutput("spine_plan",
                                           height = 750,
                                           click = "plot_click", 
                                           dblclick = "plot_double_click"),
                                actionButton(inputId = "implants_complete", label = "Click when Implants Complete"),)
                  ),
                  verbatimTextOutput("click_info")
                  # dataTableOutput("left_screw_sizes_df")
                  # fileInput("myFile", "Choose a file", accept = c('image/png', 'image/jpeg')),
                  # div(id = "image-container", style = "display:flexbox")
                )
              ))
  ))

###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ########## SERVER STARTS  ###### ###### ###### ###### ###### ######  ###### ###### ###### ###### ###### ######
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ########## SERVER STARTS  ###### ###### ###### ###### ###### ######  ###### ###### ###### ###### ###### ######
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ########## SERVER STARTS  ###### ###### ###### ###### ###### ######  ###### ###### ###### ###### ###### ######
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ########## SERVER STARTS  ###### ###### ###### ###### ###### ######  ###### ###### ###### ###### ###### ######
###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ########## SERVER STARTS  ###### ###### ###### ###### ###### ######  ###### ###### ###### ###### ###### ######


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ################     ################     ################  RENDER UI'S ######################################
  
  output$main_rod_ui <- renderUI({
    
    fixedRow(
      column(width = 6,
             if(nrow(implants_list$left_implants_df) > 1){
               sliderTextInput(
                 inputId = "left_main_rod",
                 label = "Left Main Rod Spans:",
                 choices = unique(implants_list$left_implants_df$level),
                 selected = c(unique(implants_list$left_implants_df$level)[1], tail(unique(implants_list$left_implants_df$level), n = 1))
               )  
             }
      ),
      column(width = 6,
             if(nrow(implants_list$right_implants_df) > 1){
               sliderTextInput(
                 inputId = "right_main_rod",
                 label = "Right Main Rod Spans:",
                 choices = unique(implants_list$right_implants_df$level),
                 selected = c(unique(implants_list$right_implants_df$level)[1], tail(unique(implants_list$right_implants_df$level), n = 1))
               )
             }
      )
    )
  })
  
  output$accessory_rods_ui <- renderUI({
    fixedRow(
      column(width = 6, 
             if(req(input$main_rods_only_vs_custom)== "Customize Construct"){
               materialSwitch(
                 inputId = "left_accessory_rod_switch",
                 label = "Add Left Accessory rod:",
                 value = FALSE,
                 status = "success"
               )
             }
      ),
      column(width = 6,
             if(req(input$main_rods_only_vs_custom)== "Customize Construct"){
               materialSwitch(
                 inputId = "right_accessory_rod_switch",
                 label = "Add Right Accessory Rod:",
                 value = FALSE,
                 status = "success"
               )
             }
      )
    )
  })
  
  output$satellite_rods_ui <- renderUI({
    if(req(input$main_rods_only_vs_custom)== "Customize Construct"){
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
      )
    }
    
  })
  
  output$intercalary_rods_ui <- renderUI({
    if(req(input$main_rods_only_vs_custom)== "Customize Construct"){
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
      )
    }
  })
  
  output$left_accessory_rod_range_ui <- renderUI({
    if(req(input$left_accessory_rod_switch) == FALSE){
      NULL
    }else{
      checkboxGroupButtons(
        inputId = "left_accessory_rod",
        label = "Left Accessory Rod Spans:",
        choices = unique(implants_list$left_implants_df$level),
        selected = c(implant_levels_df$implant_level[1], tail(implant_levels_df$implant_level , n = 1)) #c(unique(implants_list$left_implants_df$level)[1], tail(unique(implants_list$left_implants_df$level), n = 1))
      )
    }
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
        selected = c(unique(implants_list$left_implants_df$level)[1], tail(unique(implants_list$left_implants_df$level), n = 1))
      )
    }
  })
  
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
  
  output$right_satellite_rod_range_ui <- renderUI({
    if(req(input$right_satellite_rod_switch) == FALSE){
      NULL
    }else{
      implant_levels_df <- implants_list$left_implants_df %>%
        mutate(implant_level = paste(level, implant, sep = " ")) %>%
        mutate(implant_level = str_to_title(str_replace_all(implant_level, "_", " "))) %>%
        select(level, implant_level) %>%
        distinct()
      
      checkboxGroupButtons(
        inputId = "right_satellite_rod",
        label = "Right Satellite Rod Implants:",
        choices = implant_levels_df$implant_level,
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
        selected = c(unique(implants_list$right_implants_df$level)[1], tail(unique(implants_list$right_implants_df$level), n = 1))
      )
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
  
  implants_list$right_implants_df <- tibble(implant = character(), 
                                            level = character(),
                                            side = character(),
                                            vertebral_number = numeric(),
                                            x = numeric(),
                                            y = numeric())
  
  
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
      input$add_implants == "Sublaminar Wire" ~ "sublaminar_wire"
    )
    implant_type
  })
  
  implant_added_reactive <- reactive({
    implant_type_filtered_df <- implant_starts_df %>%
      filter(implant == implant_type_selected()) %>%
      remove_empty()
    
    nearPoints(
      df = implant_type_filtered_df,
      coordinfo = input$plot_click,
      xvar = "x",
      yvar = "y",
      maxpoints = 1,
      threshold = 20, 
    ) 
    
    
  })
  
  
  #### OBSERVE THE PLOT CLICK AND ADD APPROPRIATE IMPLANT ####
  
  point_to_test <- reactive({
    st_point(c(input$plot_double_click$x, input$plot_double_click$y))
  })
  
  observeEvent(input$plot_double_click, {
    req(nrow(implants_list$implants_df) > 0)
    implants_list$implants_df <- implants_list$implants_df %>%
      mutate(point_touches = map_lgl(.x = implant_constructed, .f = ~ st_contains(x = .x, y = point_to_test(), sparse = FALSE))) %>%
      filter(point_touches == FALSE) 
    
    implants_list$left_implants_df <- implants_list$implants_df %>%
      filter(side == "left")
    
    implants_list$right_implants_df <- implants_list$implants_df %>%
      filter(side == "right")
  })
  
  observeEvent(input$plot_click, {
    implant_type <- implant_type_selected()
    
    implant_type_filtered_df <- implant_starts_df %>%
      filter(implant == implant_type) %>%
      remove_empty() 
    
    if(implant_type == "pedicle_screw" | implant_type == "lateral_mass_screw" |implant_type == "pars_screw" |implant_type == "pelvic_screw" | implant_type == "translaminar_screw"){
      implant_to_add_df <- implant_added_reactive() %>% #add_nearest_row_implant %>%
        mutate(implant_constructed = pmap(list(..1 = x, 
                                               ..2 = y,
                                               ..3 = angle, 
                                               ..4 = length,
                                               ..5 = width), .f = ~ screw_function(screw_start_x = ..1, screw_start_y = ..2, angle = ..3, screw_length_modifier = ..4, screw_width_modifier = ..5)))
    }
    
    if(implant_type == "tp_hook"){
      implant_to_add_df <- implant_added_reactive() %>%
        mutate(implant_constructed = pmap(list(..1 = x, 
                                               ..2 = y), .f = ~ hook_tp_function(hook_start_x = ..1, hook_edge_y = ..2)))
    }
    
    if(implant_type == "laminar_downgoing_hook"){
      implant_to_add_df <- implant_added_reactive() %>%
        mutate(implant_constructed = pmap(list(..1 = x, 
                                               ..2 = y), .f = ~ hook_laminar_function(hook_direction = "downgoing", hook_start_x = ..1, hook_edge_y = ..2)))
    }
    
    if(implant_type == "laminar_upgoing_hook"){
      implant_to_add_df <- implant_added_reactive() %>%
        mutate(implant_constructed = pmap(list(..1 = x, 
                                               ..2 = y), .f = ~ hook_laminar_function(hook_direction = "upgoing", hook_start_x = ..1, hook_edge_y = ..2)))
    }
    
    implants_list$implants_df <-  implant_to_add_df %>%
      union_all(implants_list$implants_df) %>%
      arrange(vertebral_number)
    
    implants_list$left_implants_df <- implants_list$implants_df %>%
      filter(side == "left")
    
    implants_list$right_implants_df <- implants_list$implants_df %>%
      filter(side == "right")
  })
  
  
  
  ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
  ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
  ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
  ################# ############# CONSTRUCT REACTIVE IMPLANTS  #######################  #######################
  ##################### REACTIVE RODS ##################
  # build_rod_function <- function(range_to_filter = NULL, range_to_filter_out = NULL, satellite_or_accessory_rod = "accessory", satellite_levels_implants = NULL, side, add_medial_or_lateral = "medial", full_range_possible_df, x_offset = 0.003, y_upper_modifier = 0, y_lower_modifier = 0){
  #     if(side == "left"){
  #         x_modifier <- if_else(add_medial_or_lateral == "medial", x_offset, -x_offset)
  #     }else{
  #         x_modifier <- if_else(add_medial_or_lateral == "medial", -x_offset, x_offset)
  #     }
  #     
  #     if(!is.null(range_to_filter)){
  #         
  #         if(satellite_or_accessory_rod == "satellite"){ ### this code creates a rod that is anchored with screws at multiple levels
  #             satellite_rod_levels_df <- tibble(satellite_levels_implants = satellite_levels_implants) %>%
  #                 separate(col = satellite_levels_implants, into = c("level", "implant"), sep = " ", extra = "merge") %>%
  #                 mutate(implant = str_replace_all(string = implant, pattern = " ", replacement = "_")) %>%
  #                 left_join(full_range_possible_df) %>%
  #                 distinct()
  # 
  #             rod_matrix <- satellite_rod_levels_df %>%
  #                 select(x, y) %>%
  #                 arrange(y) %>%
  #                 distinct() %>%
  #                 mutate(x = x + x_modifier) %>%
  #                 mutate(y = if_else(y == min(y), y - y_lower_modifier, y)) %>%
  #                 mutate(y = if_else(y == max(y), y - y_upper_modifier, y)) %>%
  #                 as.matrix()  
  #             rod_sf <- st_buffer(st_linestring(rod_matrix), dist = 0.003, endCapStyle = "ROUND")
  # 
  #         } else{  ### this code creates a rod that is filtered by an upper and lower level
  #             upper_level <- range_to_filter[1]
  #             lower_level <- range_to_filter[2] 
  #             
  #             number_range_for_rod_df <- full_range_possible_df %>%
  #                 select(level, vertebral_number) %>%
  #                 mutate(upper_level = upper_level, lower_level = lower_level) %>%
  #                 filter(level == upper_level | level == lower_level) %>%
  #                 distinct() %>%
  #                 select(vertebral_number)
  #             
  #             rod_matrix <- full_range_possible_df %>%
  #                 filter(between(vertebral_number, number_range_for_rod_df$vertebral_number[1], number_range_for_rod_df$vertebral_number[2])) %>%
  #                 select(x, y) %>%
  #                 arrange(y) %>%
  #                 distinct() %>%
  #                 mutate(x = x + x_modifier) %>%
  #                 mutate(y = if_else(y == min(y), y - y_lower_modifier, y)) %>%
  #                 mutate(y = if_else(y == max(y), y - y_upper_modifier, y)) %>%
  #                 as.matrix()   
  #             rod_sf <- st_buffer(st_linestring(rod_matrix), dist = 0.003, endCapStyle = "ROUND")
  #         }
  #         
  #     }else{
  #         if(length(range_to_filter_out) > 0){
  #             range_to_filter_out_df <- tibble(levels_implants = range_to_filter_out) %>%
  #                 separate(col = levels_implants, into = c("level", "implant"), sep = " ", extra = "merge") %>%
  #                 mutate(implant = str_to_lower(string = str_replace_all(string = implant, pattern = " ", replacement = "_")))
  #             
  #             rod_matrix <- full_range_possible_df %>%
  #                 anti_join(range_to_filter_out_df) %>%
  #                 select(x, y) %>%
  #                 arrange(y) %>%
  #                 distinct() %>%
  #                 mutate(x = x + x_modifier) %>%
  #                 as.matrix()  
  #             rod_sf <- st_buffer(st_linestring(rod_matrix), dist = 0.003, endCapStyle = "ROUND")
  #         }else{
  #             rod_matrix <- full_range_possible_df %>%
  #                 select(x, y) %>%
  #                 arrange(y) %>%
  #                 distinct() %>%
  #                 mutate(x = x + x_modifier) %>%
  #                 as.matrix()
  #             rod_sf <- st_buffer(st_linestring(rod_matrix), dist = 0.003, endCapStyle = "ROUND")
  #         }
  #     }
  # 
  #     rod_sf
  #     
  # }
  ##################### REACTIVE RODS ##################
  build_rod_function <- function(range_to_filter = NULL, range_to_filter_out = NULL, satellite_or_accessory_rod = "accessory", satellite_levels_implants = NULL, side, add_medial_or_lateral = "medial", full_range_possible_df, x_offset = 0.003, y_upper_modifier = 0, y_lower_modifier = 0){
    if(side == "left"){
      x_modifier <- if_else(add_medial_or_lateral == "medial", x_offset, -x_offset)
    }else{
      x_modifier <- if_else(add_medial_or_lateral == "medial", -x_offset, x_offset)
    }
    
    if(!is.null(range_to_filter)){
      
      if(satellite_or_accessory_rod == "satellite"){ ### this code creates a rod that is anchored with screws at multiple levels
        satellite_rod_levels_df <- tibble(satellite_levels_implants = satellite_levels_implants) %>%
          separate(col = satellite_levels_implants, into = c("level", "implant"), sep = " ", extra = "merge") %>%
          mutate(implant = str_replace_all(string = implant, pattern = " ", replacement = "_")) %>%
          left_join(full_range_possible_df) %>%
          distinct()
        
        rod_matrix <- satellite_rod_levels_df %>%
          select(x, y) %>%
          arrange(y) %>%
          distinct() %>%
          mutate(x = x + x_modifier) %>%
          mutate(y = if_else(y == min(y), y - y_lower_modifier, y)) %>%
          mutate(y = if_else(y == max(y), y - y_upper_modifier, y)) %>%
          as.matrix()  
        rod_sf <- st_buffer(st_linestring(rod_matrix), dist = 0.003, endCapStyle = "ROUND")
        
      } else{  ### this code creates a rod that is filtered by an upper and lower level
        upper_level <- range_to_filter[1]
        lower_level <- range_to_filter[2] 
        
        number_range_for_rod_df <- full_range_possible_df %>%
          select(level, vertebral_number) %>%
          mutate(upper_level = upper_level, lower_level = lower_level) %>%
          filter(level == upper_level | level == lower_level) %>%
          distinct() %>%
          select(vertebral_number)
        
        rod_matrix <- full_range_possible_df %>%
          filter(between(vertebral_number, number_range_for_rod_df$vertebral_number[1], number_range_for_rod_df$vertebral_number[2])) %>%
          select(x, y) %>%
          arrange(y) %>%
          distinct() %>%
          mutate(x = x + x_modifier) %>%
          mutate(y = if_else(y == min(y), y - y_lower_modifier, y)) %>%
          mutate(y = if_else(y == max(y), y - y_upper_modifier, y)) %>%
          as.matrix()   
        rod_sf <- st_buffer(st_linestring(rod_matrix), dist = 0.003, endCapStyle = "ROUND")
      }
      
    }else{
      if(length(range_to_filter_out) > 0){
        range_to_filter_out_df <- tibble(levels_implants = range_to_filter_out) %>%
          separate(col = levels_implants, into = c("level", "implant"), sep = " ", extra = "merge") %>%
          mutate(implant = str_to_lower(string = str_replace_all(string = implant, pattern = " ", replacement = "_")))
        
        rod_matrix <- full_range_possible_df %>%
          anti_join(range_to_filter_out_df) %>%
          select(x, y) %>%
          arrange(y) %>%
          distinct() %>%
          mutate(x = x + x_modifier) %>%
          as.matrix()  
        rod_sf <- st_buffer(st_linestring(rod_matrix), dist = 0.003, endCapStyle = "ROUND")
      }else{
        rod_matrix <- full_range_possible_df %>%
          select(x, y) %>%
          arrange(y) %>%
          distinct() %>%
          mutate(x = x + x_modifier) %>%
          as.matrix()
        rod_sf <- st_buffer(st_linestring(rod_matrix), dist = 0.003, endCapStyle = "ROUND")
      }
    }
    
    rod_sf
    
  }
  
  left_rods <- reactiveValues()
  right_rods <- reactiveValues()
  
  left_rods$left_rod_reactive_sf <- reactive({
    
    if(nrow(implants_list$left_implants_df) > 1){
      if(req(input$main_rods_only_vs_custom)== "Customize Construct"){
        if(input$left_satellite_rod_switch == TRUE){
          if(length(input$left_satellite_rod) > 1){
            left_rod_sf <- build_rod_function(range_to_filter = NULL, range_to_filter_out = input$left_satellite_rod,  side = "left", add_medial_or_lateral = "medial", full_range_possible_df = implants_list$left_implants_df, x_offset = 0)
          }else{
            left_rod_sf <- build_rod_function(range_to_filter = req(input$left_main_rod), side = "left", add_medial_or_lateral = "medial", full_range_possible_df = implants_list$left_implants_df, x_offset = 0)
          }
        }else{
          if(length(input$left_main_rod) > 1){
            left_rod_sf <- build_rod_function(range_to_filter = req(input$left_main_rod), side = "left", add_medial_or_lateral = "medial", full_range_possible_df = implants_list$left_implants_df, x_offset = 0)
          }else{
            left_rod_sf <- build_rod_function(side = "left", add_medial_or_lateral = "medial", full_range_possible_df = implants_list$left_implants_df, x_offset = 0)
          } 
        }
      }else
        if(length(input$left_main_rod) > 1){
          left_rod_sf <- build_rod_function(range_to_filter = req(input$left_main_rod), side = "left", add_medial_or_lateral = "medial", full_range_possible_df = implants_list$left_implants_df, x_offset = 0)
        }else{
          left_rod_sf <- build_rod_function(side = "left", add_medial_or_lateral = "medial", full_range_possible_df = implants_list$left_implants_df, x_offset = 0)
        } 
    }else{
      left_rod_sf <- NULL
    }
    
    left_rod_sf
    
    
  })
  
  left_rods$left_accessory_rod_reactive_sf <- reactive({
    if(req(input$main_rods_only_vs_custom)== "Customize Construct"){
      if(input$left_accessory_rod_switch == FALSE){
        NULL
      }else{
        build_rod_function(range_to_filter = input$left_accessory_rod,
                           side = "left", 
                           add_medial_or_lateral = "medial", 
                           full_range_possible_df = implants_list$left_implants_df, 
                           x_offset = .015)
      }
    }else{
      NULL
    }
  })
  
  left_rods$left_satellite_rod_reactive_sf <- reactive({
    if(req(input$main_rods_only_vs_custom)== "Customize Construct"){
      if(input$left_satellite_rod_switch == FALSE){
        NULL
      }else{
        if(length(input$left_satellite_rod) > 1){
          build_rod_function(side = "left",
                             satellite_or_accessory_rod = "satellite",
                             satellite_levels_implants = input$left_satellite_rod,
                             add_medial_or_lateral = "lateral", 
                             full_range_possible_df = implants_list$left_implants_df, 
                             x_offset = .015)
        }else{
          NULL
        }
        
      }
    }else{
      NULL
    }
  })
  
  left_rods$left_intercalary_rod_reactive_sf <- reactive({
    if(req(input$main_rods_only_vs_custom)== "Customize Construct"){
      if(input$left_intercalary_rod_switch == FALSE){
        NULL
      }else{
        build_rod_function(range_to_filter = input$left_intercalary_rod,
                           side = "left", 
                           add_medial_or_lateral = "lateral", 
                           full_range_possible_df = implants_list$left_implants_df, 
                           x_offset = .015)
      }
    }else{
      NULL
    }
  })
  
  
  ############################ RIGHT RODS ########################
  
  right_rods$right_rod_reactive_sf <- reactive({
    if(length(input$right_main_rod) > 1){
      right_rod_sf <- build_rod_function(range_to_filter = req(input$right_main_rod), side = "right", add_medial_or_lateral = "medial", full_range_possible_df = implants_list$right_implants_df, x_offset = 0)
    }else{
      if(nrow(implants_list$right_implants_df) > 1){
        right_rod_sf <- build_rod_function(side = "right", add_medial_or_lateral = "medial", full_range_possible_df = implants_list$right_implants_df, x_offset = 0)
      }else{
        right_rod_sf <- NULL
      }
    }
    
  })
  
  right_rods$right_accessory_rod_reactive_sf <- reactive({
    if(req(input$main_rods_only_vs_custom)== "Customize Construct"){
      if(input$right_accessory_rod_switch == FALSE){
        NULL
      }else{
        build_rod_function(range_to_filter = input$right_accessory_rod,
                           side = "right", 
                           add_medial_or_lateral = "medial", 
                           full_range_possible_df = implants_list$right_implants_df, 
                           x_offset = .015)
      }
    }else{
      NULL
    }
  })
  
  right_rods$right_satellite_rod_reactive_sf <- reactive({
    if(req(input$main_rods_only_vs_custom)== "Customize Construct"){
      if(input$right_satellite_rod_switch == FALSE){
        NULL
      }else{
        build_rod_function(range_to_filter = input$right_satellite_rod,
                           side = "right", 
                           add_medial_or_lateral = "lateral", 
                           full_range_possible_df = implants_list$right_implants_df, 
                           x_offset = .015)
      }
    }else{
      NULL
    }
  })
  
  right_rods$right_intercalary_rod_reactive_sf <- reactive({
    if(req(input$main_rods_only_vs_custom)== "Customize Construct"){
      if(input$right_intercalary_rod_switch == FALSE){
        NULL
      }else{
        build_rod_function(range_to_filter = input$right_intercalary_rod,
                           side = "right", 
                           add_medial_or_lateral = "lateral", 
                           full_range_possible_df = implants_list$right_implants_df, 
                           x_offset = .015)
      }
    }else{
      NULL
    }
  })
  
  implants_reactive_sf <- reactive({
    if (nrow(implants_list$implants_df) == 0) {
      implants_sf <- NULL
    } else {
      st_multipolygon(implants_list$implants_df$implant_constructed)
    }
  })
  
  
  
  #### CREATE FINAL LEFT SCREWS/IMPLANTS SF ELEMENT
  left_implants_reactive_sf <- reactive({   ### ALL THIS CODE IS FOR IF THERE IS A SATELLITE ROD, THEN YOU NEED TO CHANGE THE LENGTH OF THOSE SCREWS
    if (nrow(implants_list$left_implants_df) == 0) {
      implants_sf <- NULL
    } else{
      if(is.null(left_rods$left_satellite_rod_reactive_sf)){
        implants_sf <-st_multipolygon(implants_list$implants_df$implant_constructed)
      }else{
        satellite_rod_levels_df <- tibble(satellite_levels_implants = input$left_satellite_rod) %>%
          separate(col = satellite_levels_implants, into = c("level", "implant"), sep = " ", extra = "merge") %>%
          mutate(implant = str_to_lower(string = str_replace_all(string = implant, pattern = " ", replacement = "_"))) %>%
          left_join(implants_list$left_implants_df) %>%
          distinct()
        
        left_implant_list_filtered_df <- implants_list$left_implants_df %>%
          anti_join(satellite_rod_levels_df)
        
        sat_rods_levels_constructed_df <- satellite_rod_levels_df %>%
          mutate(x = x-0.015, length = length*2) %>%
          mutate(implant_constructed = pmap(list(..1 = x,
                                                 ..2 = y,
                                                 ..3 = angle,
                                                 ..4 = length,
                                                 ..5 = width), 
                                            .f = ~ screw_function(screw_start_x = ..1, screw_start_y = ..2, angle = ..3, screw_length_modifier = ..4, screw_width_modifier = ..5)))
        
        left_implants <- left_implant_list_filtered_df %>%
          union_all(sat_rods_levels_constructed_df)
        
        implants_sf <-st_multipolygon(left_implants$implant_constructed)
      }
    }
    implants_sf
  })
  
  
  right_implants_reactive_sf <- reactive({ ### ALL THIS CODE IS FOR IF THERE IS A SATELLITE ROD, THEN YOU NEED TO CHANGE THE LENGTH OF THOSE SCREWS
    if (nrow(implants_list$right_implants_df) == 0) {
      implants_sf <- NULL
    } else{
      if(req(input$main_rods_only_vs_custom) == "Customize Construct"){
        if(input$right_satellite_rod_switch == FALSE){
          st_multipolygon(implants_list$implants_df$implant_constructed)
        }else{
          sat_rods_level_range_df <- tibble(level = input$right_satellite_rod) %>%
            left_join(implants_list$right_implants_df) %>%
            # filter(implant == "pedicle_screw", side == "right") %>%
            select(vertebral_number)
          
          sat_rods_levels_df <- implants_list$right_implants_df %>%
            filter(between(vertebral_number, min(sat_rods_level_range_df$vertebral_number), max(sat_rods_level_range_df$vertebral_number)))
          # filter(implant == "pedicle_screw", side == "right")
          
          main_rod_levels <- implants_list$right_implants_df %>%
            # filter(implant == "pedicle_screw", side == "right") %>%
            anti_join(sat_rods_levels_df)
          
          sat_rods_levels_constructed_df <- sat_rods_levels_df %>%
            mutate(x = x+0.015, length = length*2) %>%
            mutate(implant_constructed = pmap(list(..1 = x,
                                                   ..2 = y,
                                                   ..3 = angle,
                                                   ..4 = length,
                                                   ..5 = width), .f = ~ screw_function(screw_start_x = ..1, screw_start_y = ..2, angle = ..3, screw_length_modifier = ..4, screw_width_modifier = ..5)))
          right_implants <- main_rod_levels %>%
            union_all(sat_rods_levels_constructed_df)
          
          st_multipolygon(right_implants$implant_constructed)
        }
      }else{
        st_multipolygon(implants_list$implants_df$implant_constructed)
      }
    }
  })
  
  
  output$surgical_summary <- renderUI({
    req(nrow(implants_list$implants_df) > 0)
    uiv_df <- implants_list$implants_df %>%
      filter(vertebral_number == min(vertebral_number)) %>%
      mutate(level = str_to_title(level))
    
    liv_df <- implants_list$implants_df %>%
      filter(vertebral_number == max(vertebral_number)) %>%
      mutate(level = str_to_title(level))
    
    HTML(paste(paste("UIV = ", uiv_df$level[[1]]), paste("LIV = ", liv_df$level[[1]]), sep = "<br/>"))
  })
  
  
  posterior_spine_image <- reactiveFileReader(
    intervalMillis = 100,
    readFunc = image_read,
    session = NULL,
    filePath = "posterior_spine_figure.png"
  )
  
  
  output$spine_plan <- renderPlot({
    
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
      # geom_point(data = implants_list$implants_df,
      #            aes(x = x, y = y),
      #            size = 3) +
      # geom_sf(data = pedicle_screws_sf) +
      ggpattern::geom_sf_pattern(
        data = left_implants_reactive_sf(),
        pattern = "stripe",
        pattern_fill = "blue",
        alpha = 0.8,
        pattern_colour = "blue",
        pattern_density = 0.02,
        pattern_spacing = 0.01,
        angle = 70
      ) +
      ggpattern::geom_sf_pattern(
        data = right_implants_reactive_sf(),
        pattern = "stripe",
        pattern_fill = "blue",
        alpha = 0.8,
        pattern_colour = "blue",
        pattern_density = 0.02,
        pattern_spacing = 0.01,
        angle = 20
      ) +
      theme_minimal_grid() +
      geom_sf(data = left_rods$left_rod_reactive_sf(), alpha = 0.5) +
      # geom_sf(data = left_rods$left_accessory_rod_reactive_sf(), alpha = 0.5, fill = "green") +
      geom_sf(data = left_rods$left_satellite_rod_reactive_sf(), alpha = 0.5, fill = "blue") +
      # geom_sf(data = left_rods$left_intercalary_rod_reactive_sf(), alpha = 0.5, fill = "red") +
      # geom_sf(data = right_rods$right_rod_reactive_sf(), alpha = 0.5) +
      # geom_sf(data = right_rods$right_accessory_rod_reactive_sf(), alpha = 0.5, fill = "green") +
      # geom_sf(data = right_rods$right_satellite_rod_reactive_sf(), alpha = 0.5, fill = "blue") +
      # geom_sf(data = right_rods$right_intercalary_rod_reactive_sf(), alpha = 0.5, fill = "red") +
      # geom_sf(data = right_rods$right_rod_reactive_sf(), alpha = 0.5) +
      ylim(input$crop_y[1], input$crop_y[2]) +
      xlim(0.25- input$label_text_offset/100, 0.75 + input$label_text_offset/100)
    
    spine_plot
    
  })
  
  
  ##################### REACTIVE UI AND FUNCTION TO GENERATE INPUT NAMES AND RETRIEVE VALUES ##################
  ##################### REACTIVE UI AND FUNCTION TO GENERATE INPUT NAMES AND RETRIEVE VALUES ##################
  ##################### REACTIVE UI AND FUNCTION TO GENERATE INPUT NAMES AND RETRIEVE VALUES ##################
  
  make_ui_rows <-  function(level = NULL, left_screw_level = "no_screw", right_screw_level = "no_screw", left_selected = NULL, right_selected = NULL){
    
    level <- str_to_title(level)
    
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
    if(nrow(implants_list$implants_df) > 1){
      full_ui_df <- pedicle_screw_level_reactive_df() %>%
        select(level, vertebral_number, level_side, side) %>%
        pivot_wider(names_from = side, values_from = level_side) %>%
        mutate(no_screw_vector = "no_screw") 
      
      df_names <- glue_collapse(implants_list$implants_df$side, sep = " ")
      
      level_vector <- full_ui_df$level
      
      ## the data frame can't have any missing values, otherwise the vectors will be of different lengths when you run map()
      if(str_detect(string = df_names, pattern = "left")){
        full_ui_df <- full_ui_df %>%
          mutate(left = if_else(is.na(left), "no_screw", left))
        
        left_vector <- full_ui_df$left
      }else{
        left_vector <- full_ui_df$no_screw_vector
      }
      
      if(str_detect(string = df_names, pattern = "right")){
        full_ui_df <- full_ui_df %>%
          mutate(right = if_else(is.na(right), "no_screw", right))
        
        right_vector <- full_ui_df$right
      }else{
        right_vector <- full_ui_df$no_screw_vector
      }
      
      max_levels <- nrow(full_ui_df)     
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
      fixedRow()
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
    # if(nrow(implants_list$implants_df) > 1){
    #     implants_list$implants_df %>%
    #         mutate(level_side = paste(level, side, implant, sep = "_")) %>%
    #         select(vertebral_number, level, side, level_side, implant) %>%
    #         filter(implant == "pedicle_screw") %>%
    #         distinct() %>%
    #         arrange(vertebral_number)
    # }else{
    #     implants_list$implants_df
    # }
    implants_list$implants_df %>% ### cant just do the dataframe because of the implant_constructed column
      select(level, vertebral_number, implant, side, x, y)
    
    # pedicle_screw_details_selected_reactive()
  })
  
  
  
  
  
  ########################################  RECORD A LIST OF POINTS ## ############################
  
  
  point_clicks <- reactiveValues()
  
  point_clicks$click_coordinates_df <- tibble(coordinate_type = character(), 
                                              side = character(),
                                              x = numeric(),
                                              y = numeric())
  
  #### OBSERVE THE PLOT CLICK AND ADD APPROPRIATE IMPLANT ####
  
  # observeEvent(input$plot_double_click, {
  # 
  #     add_nearest_row <-
  #         nearPoints(
  #             df = xy_grid,
  #             coordinfo = input$plot_double_click,
  #             xvar = "x",
  #             yvar = "y",
  #             maxpoints = 1,
  #             threshold = 15
  #         ) %>%
  #         mutate(coordinate_type = input$coordinate_type) %>%
  #         mutate(side = if_else(x < 0.5, "left", "right"))
  #     
  #     point_clicks$click_coordinates_df <- union_all(x = point_clicks$click_coordinates_df, y = add_nearest_row)
  #     
  #     point_clicks$click_coordinates_final_df <- point_clicks$click_coordinates_df %>%
  #         remove_missing()
  # })
  # 
  # output$coordinates_table_2 <- renderTable({
  #     # clicks_list
  #     point_clicks$click_coordinates_final_df
  #     
  #     input$save_coordinates_table_button
  #     isolate(write_csv(x = point_clicks$click_coordinates_final_df, file = input$file_name))
  # })
  # 
  
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
