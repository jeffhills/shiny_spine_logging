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
source("labels_screws_functions.R", local = TRUE)
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
            uiOutput(outputId = "crop_range_ui"),
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
                          width = 3,
                          # pickerInput(
                          #     inputId = "add_implants",
                          #     label = "Select Implant to add",
                          #     choices = list(
                          #         Implant = c(
                          #             "Pedicle Screws",
                          #             "Lateral Mass Screws",
                          #             "Pars Screws",
                          #             "TP Hook (Downgoing)",
                          #             "TP Hook (Upgoing)",
                          #             "Laminar Hook (Downgoing)",
                          #             "Laminar Hook (Upgoing)",
                          #             "Pedicle Hook",
                          #             "Tether",
                          #             "Sublaminar Wire"
                          #         ),
                          #         Decompression = c("A",
                          #                           "B", "C", "D")
                          #     )
                          # ),
                          # radioGroupButtons(
                          #   inputId = "add_implants",
                          #   label = "Select an Implant to add",
                          #   choices = c("Pedicle Screws",
                          #               "Lateral Mass Screws",
                          #               "Pars Screws",
                          #               "TP Hook (Downgoing)",
                          #               "TP Hook (Upgoing)",
                          #               "Laminar Hook (Downgoing)",
                          #               "Laminar Hook (Upgoing)",
                          #               "Pedicle Hook",
                          #               "Tether",
                          #               "Sublaminar Wire"),
                          #   direction = "vertical",
                          #   justified = TRUE,
                          #   checkIcon = list(
                          #     yes = icon("ok",
                          #                lib = "glyphicon"))
                          # ),
                          column(
                              width = 6,
                              materialSwitch(
                                  inputId = "left_accessory_rod_switch",
                                  label = "Add left accessory rod:",
                                  value = FALSE,
                                  status = "success"
                              ),
                              uiOutput(outputId = "left_accessory_rod_ui"),
                              materialSwitch(
                                  inputId = "left_satellite_rod_switch",
                                  label = "Add left satellite rod:",
                                  value = FALSE,
                                  status = "success"
                              ),
                              uiOutput(outputId = "left_satellite_rod_ui")
                          ),
                          column(
                              width = 6,
                              materialSwitch(
                                  inputId = "right_accessory_rod_switch",
                                  label = "Add right accessory rod:",
                                  value = FALSE,
                                  status = "success"
                              ),
                              uiOutput(outputId = "right_accessory_rod_ui"),
                              materialSwitch(
                                  inputId = "right_satellite_rod_switch",
                                  label = "Add right satellite rod:",
                                  value = FALSE,
                                  status = "success"
                              ),
                              uiOutput(outputId = "right_satellite_rod_ui")
                          ),
                          # column(width = 4, 
                          #        noUiSliderInput(inputId = "crop_y", 
                          #                        label = "Crop Y", 
                          #                        min = 0, 
                          #                        max = 1, 
                          #                        value = c(0,1), 
                          #                        orientation = "vertical", height = "300px", width = "10px")),
                          verbatimTextOutput(outputId = "test_print"),
                          tableOutput(outputId = "coordinates_table")
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
                                                    "Lateral Mass Screws",
                                                    "Pars Screws",
                                                    "TP Hook (Downgoing)",
                                                    "TP Hook (Upgoing)",
                                                    "Laminar Hook (Downgoing)",
                                                    "Laminar Hook (Upgoing)",
                                                    "Pedicle Hook",
                                                    "Tether",
                                                    "Sublaminar Wire"
                                                ),
                                                Decompression = c("A",
                                                                  "B", "C", "D")
                                            )
                                        )),
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
                                        ))),
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
                                                   click = "plot_click"))
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

    ################ RENDER UI'S ######################################
    
    output$left_accessory_rod_ui <- renderUI({
        if (input$left_accessory_rod_switch == TRUE) {
            column(
                width = 12,
                sliderTextInput(
                    inputId = "left_accessory_rod",
                    label = "Add additional left rod from:",
                    choices = levels_vector,
                    #levels_filtered_vector(),
                    selected = c(levels_vector[10], levels_vector[15])  #levels_filtered_vector()[c(2, length(levels_filtered_vector()) - 2)]
                )
            )
        } else{
            NULL
        }
    })
    
    output$left_satellite_rod_ui <- renderUI({
        if (input$left_satellite_rod_switch == TRUE) {
            column(
                width = 12,
                sliderTextInput(
                    inputId = "left_satellite_rod",
                    label = "Add satellite left rod from:",
                    choices = levels_vector,
                    #levels_filtered_vector(),
                    selected = c(levels_vector[10], levels_vector[15])
                )
            )
        } else{
            NULL
        }
        
    })
    
    output$right_accessory_rod_ui <- renderUI({
        if (input$right_accessory_rod_switch == TRUE) {
            column(
                width = 12,
                sliderTextInput(
                    inputId = "right_accessory_rod",
                    label = "Add additional right rod from:",
                    choices = levels_vector,
                    #levels_filtered_vector(),
                    selected = c(levels_vector[10], levels_vector[15])  #levels_filtered_vector()[c(2, length(levels_filtered_vector()) - 2)]
                )
            )
        } else{
            NULL
        }
    })
    
    output$right_satellite_rod_ui <- renderUI({
        if (input$right_satellite_rod_switch == TRUE) {
            column(
                width = 12,
                sliderTextInput(
                    inputId = "right_satellite_rod",
                    label = "Add satellite right rod from:",
                    choices = levels_vector,
                    #levels_filtered_vector(),
                    selected = c(levels_vector[10], levels_vector[15])
                )
            )
        } else{
            NULL
        }
        
    })
    
    ####################################################################
    
    output$click_info <- renderPrint({
        cat("input$plot_click:\n")
        str(input$plot_click)
    })
    
    implants_list <- reactiveValues()
    
    implants_list$implants_df <- tibble(implant = character(), side = character(),
               x = numeric(),
               y = numeric())
    
    #### OBSERVE THE PLOT CLICK AND ADD APPROPRIATE IMPLANT ####
    
    observeEvent(input$plot_click, {
        implant_type <- case_when(
            input$add_implants == "Pedicle Screws" ~ "pedicle_screw",
            input$add_implants == "Lateral Mass Screws" ~ "lm_screw",
            input$add_implants == "Pars Screws" ~ "pars_screw",
            input$add_implants == "Translaminar Screws" ~ "translaminar_screw",
            input$add_implants == "TP Hook" ~ "tp_hook",
            input$add_implants == "Laminar Hook (Down going)" ~ "laminar_down_hook",
            input$add_implants == "Laminar Hook (Up going)" ~ "laminar_up_hook",
            input$add_implants == "Tether" ~ "tether",
            input$add_implants == "Sublaminar Wire" ~ "sublaminar_wire"
        )

        add_nearest_row <-
            nearPoints(
                df = screw_start_df,
                coordinfo = input$plot_click,
                xvar = "x",
                yvar = "y",
                maxpoints = 1,
                threshold = 15
            ) %>%
            mutate(
                implant_type = if_else(
                    labels == "Iliac" |
                        labels == "S2AI" | labels == "Occiput",
                    "screw",
                    implant_type
                )
            ) %>%
            mutate(implant = if_else(
                x < 0.5,
                paste("left_", labels, "_", implant_type),
                paste("right_", labels, "_", implant_type)
            )) %>%
            mutate(side = if_else(x < 0.5, "left", "right")) %>%
            mutate(implant = str_to_lower(implant)) %>%
            mutate(implant = str_remove_all(string = implant, pattern = " ")) %>%
            mutate(implant = if_else(
                str_detect(string = implant, pattern = "left_c1"),
                "left_c1_lm_screw",
                implant
            )) %>%
            mutate(implant = if_else(
                str_detect(string = implant, pattern = "right_c1"),
                "right_c1_lm_screw",
                implant
            ))
        
        implants_list$implants_df <-
            union_all(x = implants_list$implants_df, y = add_nearest_row)
        
    })
    
    
    posterior_spine_image <- reactiveFileReader(
            intervalMillis = 100,
            readFunc = image_read,
            session = NULL,
            filePath = "posterior_spine_figure.png"
        )
    
    
    ################# IMPLANTS REACTIVE LIST #######################
    
    implants_reactive_sf <- reactive({
        ### IMPLANTS ####
        if (length(implants_list$implants_df$implant) == 0) {
            implants_sf <- NULL
        } else{
            implants_to_plot_df <- implants_list$implants_df %>%
                group_by(implant) %>%
                add_tally(name = "number_of_repeats") %>%
                ungroup() %>%
                mutate(discard_repeat = if_else(as.integer(number_of_repeats %% 2) == 0, "discard", "keep")) %>%
                filter(discard_repeat == "keep") %>%
                left_join(screw_screwhead_long_df) %>%
                remove_missing()
            
            if (length(implants_to_plot_df$screws) == 0) {
                implants_sf <- NULL
            } else{
                implants_sf <- st_multipolygon(implants_to_plot_df$screws)
            }
        }
        
        implants_sf
        
    })
    
    ##################### REACTIVE RODS ##################
    

    
    left_rod_reactive_sf <- reactive({
        
        if(sum(str_count(implants_list$implants_df$side, pattern = "left")) > 1){

            left_rods_wide_df <- implants_list$implants_df %>%
                left_join(screw_screwhead_long_df) %>%
                select(implant, side, screw_heads) %>%
                pivot_wider(names_from = side, values_from = "screw_heads")

            left_rod_df <- do.call(rbind, left_rods_wide_df$left) %>%
                as_tibble() %>%
                arrange(V2) %>%
                as.matrix()

            left_rod_sf <- st_buffer(st_linestring(left_rod_df), dist = 0.003)
        }else{
            left_rod_sf <- NULL
        }
        left_rod_sf

    })
    
    right_rod_reactive_sf <- reactive({

        if(sum(str_count(implants_list$implants_df$side, pattern = "right")) > 1){
            right_rods_wide_df <- implants_list$implants_df %>%
                left_join(screw_screwhead_long_df) %>%
                select(implant, side, screw_heads) %>%
                pivot_wider(names_from = side, values_from = "screw_heads")
            
            right_rod_df <- do.call(rbind, right_rods_wide_df$right) %>%
                as_tibble() %>%
                arrange(V2) %>%
                as.matrix()
            
            right_rod_sf <- st_buffer(st_linestring(right_rod_df), dist = 0.003)
        }else{
            right_rod_sf <- NULL
        }
        right_rod_sf

    })
    
    # right_rod_reactive_sf <- reactive({
    #     if (length(implants_list$implants_df$implant) < 2){
    #         right_rod_sf <- NULL
    #     }else{
    #         right_rods_long_df <- implants_list$implants_df %>%
    #             left_join(screw_screwhead_long_df) %>%
    #             select(implant, side, screw_heads)
    #         if(str_detect(string = right_rods_long_df$side, pattern = "right")){
    #             right_rods_wide_df <- right_rods_long_df %>%
    #                 pivot_wider(names_from = side, values_from = "screw_heads")
    #             
    #             right_rod_df <- do.call(rbind, right_rods_wide_df$right)
    #             
    #             right_rod_sf <- st_buffer(st_linestring(right_rod_df), dist = 0.001)
    #         }else{
    #             right_rod_sf <- NULL
    #         }
    #     }
    #     right_rod_sf
    # })
        

    
    output$spine_plan <- renderPlot({


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
                    x = labels_df$x_left,
                    y = labels_df$y,
                    size = 12,
                    fontface = "bold"
                ) +
                draw_text(
                    text = labels_df$labels,
                    x = labels_df$x_right,
                    y = labels_df$y,
                    size = 12,
                    fontface = "bold"
                ) +
                geom_point(data = implants_list$implants_df,
                           aes(x = x, y = y),
                           size = 3) +
                # geom_sf(data = pedicle_screws_sf) +
                ggpattern::geom_sf_pattern(
                    data = implants_reactive_sf(),
                    pattern = "stripe",
                    pattern_fill = "blue",
                    alpha = 0.8,
                    pattern_colour = "blue",
                    pattern_density = 0.02,
                    pattern_spacing = 0.01,
                    angle = 70
                ) +
                theme_minimal_grid() +
                geom_sf(data = left_rod_reactive_sf()) +
                geom_sf(data = right_rod_reactive_sf()) +
                ylim(input$crop_y[1], input$crop_y[2])
            
            
            spine_plot
            
        })
    
    output$coordinates_table <- renderTable({
        # clicks_list
        
        implants_list$implants_df
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
