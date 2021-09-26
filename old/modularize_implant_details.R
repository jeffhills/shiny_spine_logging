# row_ui <- function(id) {
#   ns <- NS(id)
#   fluidRow(
#     column(3, 
#            selectInput(ns("type_chooser"), 
#                        label = "Choose Type:", 
#                        choices = c("text", "numeric"))
#     ),
#     column(9,
#            uiOutput(ns("ui_placeholder"))
#     )
#   )
# } 
# 

# row_server <- function(input, output, session) {
#   return_value <- reactive({input$inner_element})
#   ns <- session$ns
#   output$ui_placeholder <- renderUI({
#     type <- req(input$type_chooser)
#     if(type == "text") {
#       textInput(ns("inner_element"), "Text:")
#     } else if (type == "numeric") {
#       numericInput(ns("inner_element"), "Value:", 0)
#     }
#   })
#   
#   ## if we later want to do some more sophisticated logic
#   ## we can add reactives to this list
#   list(return_value = return_value) 
# }
# 
# ui <- fluidPage(  
#   div(id="placeholder"),
#   actionButton("addLine", "Add Line"),
#   verbatimTextOutput("out")
# )
# 
# server <- function(input, output, session) {
#   handler <- reactiveVal(list())
#   observeEvent(input$addLine, {
#     new_id <- paste("row", input$addLine, sep = "_")
#     insertUI(
#       selector = "#placeholder",
#       where = "beforeBegin",
#       ui = row_ui(new_id)
#     )
#     handler_list <- isolate(handler())
#     new_handler <- callModule(row_server, new_id)
#     handler_list <- c(handler_list, new_handler)
#     names(handler_list)[length(handler_list)] <- new_id
#     handler(handler_list)
#   })
#   
#   output$out <- renderPrint({
#     lapply(handler(), function(handle) {
#       handle()
#     })
#   })
# }

implant_details_UI <- function(id, label = "Implant Details"){
  ns <- NS(id)
  
  uiOutput(NS(id, ""))
}

make_ui_function <- function(variable_value, id, variable){
  if(str_detect(string = variable_value, pattern = "screw")){
    radioGroupButtons(
      inputId = id,
      label = variable,
      choices = c("Poly", "Uni", "Mono", "Reduction"),
      selected = "Poly"
    ) else {
      NULL
    }
  }
  
}

implant_details_Server <- function(id, reactive_df){
  stopifnot(is.reactive(reactive_df))
  
  moduleServer(id, function(input, output, session) {
    vars <- reactive(names(reactive_df()))
    
    output$controls <- renderUI({
      map(.x = vars(), .f = function(var) make_ui(x = df()[[var]], id =  NS(id, var),var = var))
      map(vars(), function(var) make_ui(x = df()[[var]], id =  NS(id, var), var =  var))
    })
    
    reactive({
      each_var <- map(vars(), function(var) filter_var(df()[[var]], input[[var]]))
      reduce(each_var, `&`)
    })
    
  })
  
}



# implant_Server <- function(input, output, session, implant_wide_df){
#   ns <- session$ns
#   
#   req(nrow(implants_list$implants_df) >0)
#   implants_wide_df <- implants_list$implants_df %>%
#     select(level, implant, side) %<%
#     pivot_wider(names_from = side, values_from = implant) %>%
#     rename(implants_left = left, implants_right = right)
#   
#   if(TRUE %in% str_detect(string = implants_wide_df$left, pattern = "l1")){
#     output$l1_left_ui <- renderUI({
#       radioGroupButtons(
#         inputId = ns("l1_left_screw_details"),
#         label = "Select Left L1 Implant Details",
#         choices = c("Poly", "Uni", "Mono", "Reduction"),
#         selected = "Poly"
#       )
#     })
#   }
#   
#   if(TRUE %in% str_detect(string = implants_wide_df$right, pattern = "l1")){
#     output$l1_right_ui <- renderUI({
#       radioGroupButtons(
#         inputId = ns("l1_right_screw_details"),
#         label = "Select Right L1 Implant Details",
#         choices = c("Poly", "Uni", "Mono", "Reduction"),
#         selected = "Poly"
#       )
#     })
#   }
#   
#          return(list(l1_left_ui = reactive({input$l1_left_ui}),
#                      l1_right_ui   = reactive({input$l1_right_ui})))
#   
# 
#   
# }
# 


# 
# ui <- fluidPage(  
#   div(id="placeholder"),
#   actionButton("addLine", "Add Line"),
#   verbatimTextOutput("out")
# )
# 
# server <- function(input, output, session) {
#   handler <- reactiveVal(list())
#   observeEvent(input$addLine, {
#     new_id <- paste("row", input$addLine, sep = "_")
#     insertUI(
#       selector = "#placeholder",
#       where = "beforeBegin",
#       ui = row_ui(new_id)
#     )
#     handler_list <- isolate(handler())
#     new_handler <- callModule(row_server, new_id)
#     handler_list <- c(handler_list, new_handler)
#     names(handler_list)[length(handler_list)] <- new_id
#     handler(handler_list)
#   })
#   
#   output$out <- renderPrint({
#     lapply(handler(), function(handle) {
#       handle()
#     })
#   })
# }


# 
# 
# implant_detailsServer <- function(id, levels_vector){
#   
#   moduleServer(
#     id, 
#     function(input, output, session){
#       
#       if(levels_vector %in% "l1"){
#         ouput$l1_ui  <- renderUI({
#           fluidRow(column(width = 12, 
#                           radioGroupButtons(
#                             inputId = "l1_left_implant_details",
#                             label = "Select Left L1 Implant Details",
#                             choices = c("Poly", "Uni", "Mono", "Reduction"),
#                             selected = "Poly"
#                           )),
#                    column(width = 12, 
#                           radioGroupButtons(
#                             inputId = "l1_right_implant_details",
#                             label = "Select Right L1 Implant Details",
#                             choices = c("Poly", "Uni", "Mono", "Reduction"),
#                             selected = "Poly"
#                           )))
#           
#         })
#         # ui_list <- append(ui_list, output_l1_ui = output$l1_ui)
#         
#       }
#       
#       
#       if(levels_vector %in% "l2"){
#         ouput$l2_ui  <- renderUI({
#           fluidRow(column(width = 12, 
#                           radioGroupButtons(
#                             inputId = "l2_left_implant_details",
#                             label = "Select Left L2 Implant Details",
#                             choices = c("Poly", "Uni", "Mono", "Reduction"),
#                             selected = "Poly"
#                           )),
#                    column(width = 12, 
#                           radioGroupButtons(
#                             inputId = "l2_right_implant_details",
#                             label = "Select Right L2 Implant Details",
#                             choices = c("Poly", "Uni", "Mono", "Reduction"),
#                             selected = "Poly"
#                           )))
#           
#         })
#         
#         # ui_list <- append(ui_list, output_l2_ui = output$l2_ui)
#       }
#       
#       
#       if(levels_vector %in% "l3"){
#         ouput$l3_ui  <- renderUI({
#           fluidRow(column(width = 12, 
#                           radioGroupButtons(
#                             inputId = "l3_left_implant_details",
#                             label = "Select Left L3 Implant Details",
#                             choices = c("Poly", "Uni", "Mono", "Reduction"),
#                             selected = "Poly"
#                           )),
#                    column(width = 12, 
#                           radioGroupButtons(
#                             inputId = "l3_right_implant_details",
#                             label = "Select Right L3 Implant Details",
#                             choices = c("Poly", "Uni", "Mono", "Reduction"),
#                             selected = "Poly"
#                           )))
#           
#         })
#         
#         # ui_list <- append(ui_list, output_l3_ui = output$l3_ui)
#       }
#       
#       if(levels_vector %in% "l4"){
#         ouput$l4_ui  <- renderUI({
#           fluidRow(column(width = 12, 
#                           radioGroupButtons(
#                             inputId = "l4_left_implant_details",
#                             label = "Select Left L4 Implant Details",
#                             choices = c("Poly", "Uni", "Mono", "Reduction"),
#                             selected = "Poly"
#                           )),
#                    column(width = 12, 
#                           radioGroupButtons(
#                             inputId = "l4_right_implant_details",
#                             label = "Select Right L4 Implant Details",
#                             choices = c("Poly", "Uni", "Mono", "Reduction"),
#                             selected = "Poly"
#                           )))
#           
#         })
#         
#         # ui_list <- append(ui_list, output_l4_ui = output$l4_ui)
#       }
#       
#       
#       if(levels_vector %in% "l5"){
#         ouput$l5_ui  <- renderUI({
#           fluidRow(column(width = 12, 
#                           radioGroupButtons(
#                             inputId = "l5_left_implant_details",
#                             label = "Select Left L5 Implant Details",
#                             choices = c("Poly", "Uni", "Mono", "Reduction"),
#                             selected = "Poly"
#                           )),
#                    column(width = 12, 
#                           radioGroupButtons(
#                             inputId = "l5_right_implant_details",
#                             label = "Select Right L5 Implant Details",
#                             choices = c("Poly", "Uni", "Mono", "Reduction"),
#                             selected = "Poly"
#                           )))
#           
#         })
#         
#         # ui_list <- append(ui_list, output_l5_ui = output$l5_ui)
#       }
#       
#       if(levels_vector %in% "s1"){
#         ouput$s1_ui  <- renderUI({
#           fluidRow(column(width = 12, 
#                           radioGroupButtons(
#                             inputId = "s1_left_implant_details",
#                             label = "Select Left S1 Implant Details",
#                             choices = c("Poly", "Uni", "Mono", "Reduction"),
#                             selected = "Poly"
#                           )),
#                    column(width = 12, 
#                           radioGroupButtons(
#                             inputId = "s1_right_implant_details",
#                             label = "Select Right S1 Implant Details",
#                             choices = c("Poly", "Uni", "Mono", "Reduction"),
#                             selected = "Poly"
#                           )))
#           
#         })
#         
#         # ui_list <- append(ui_list, output_s1_ui = output$s1_ui)
#       }
#       
#       # return(ui_list)
#       
#     }
#   )
# }
#   



# implant_details_to_select_function <- function(input, output, session, levels_vector){
#   
#   if(levels_vector %in% "l1"){
#     
#   }
#   
#   ouput$l1_left <- renderUI({
#     ns <- session$ns
#     radioGroupButtons(
#       inputId = "l1_implant_details",
#       label = "Select L1 Implant Details",
#       choices = c("Poly", "Uni", "Mono", "Reduction"),
#       selected = "Poly"
#     )
#   })
#   
#   ouput$l2_left <- renderUI({
#     ns <- session$ns
#     radioGroupButtons(
#       inputId = "l2_implant_details",
#       label = "Select L2 Implant Details",
#       choices = c("Poly", "Uni", "Mono", "Reduction"),
#       selected = "Poly"
#     )
#   })
#   
# }

# 
# selectQuarters <- function(input, output, session, vec_dates) {
#     
#     vec_dates <- sort(unique(vec_dates))
#     
#     output$startQuarter <-  renderUI({
#         ns <- session$ns
#         selectInput(ns("startQuarter"), "Start:", vec_dates[1:(length(vec_dates)-1)],
#                     multiple = FALSE,
#                     selected =  max(vec_dates[1:(length(vec_dates)-1)])) 
#     })
#     
#     output$endQuarter  <- renderUI({
#         ns <- session$ns #See "Using renderUI within modules" part in tutorial
#         selectInput(ns("endQuarter"), "End:", vec_dates,
#                     multiple = FALSE,
#                     selected =  max(vec_dates)) #3nd latest quarter
#     })
#     
#     #See tutorial: "If a module wants to return reactive expressions to the calling app, 
#     #               then return a list of reactive expressions from the function
#     # Using c() instead causes the same issues
#        return(list(start = reactive({input$startQuarter}), 
#                    end   = reactive({input$endQuarter})))
#     
## USE A NAMED LIST
# }
# ui <- fixedPage(
#   selectQuartersUI("test"),
#   textOutput("summary")
# )
# 
# server <- function(input, output, session) {
#   testValues <- callModule(selectQuarters, "test", 1:10) 
#   output$summary <- renderText({
#     sprintf(paste0("Start: ", testValues$start(), " end: ", testValues$end()))
#   })
# }
