#' variant_annoter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @importFrom shinyWidgets closeSweetAlert progressSweetAlert updateProgressBar ask_confirmation pickerInput sendSweetAlert
#' @importFrom shiny NS tagList 
mod_parameters_management_ui <- function(id, modal  = NULL, reactiveValues = NULL, conn = NULL){ #inputs = NULL){
  print("entering mod_parameters_management_ui")
  ns <- NS(id)
  fluidPage(
    tabsetPanel(id = "tabsParams",
                tabPanel("My Filters",
                  fluidRow(uiOutput(ns("ui"))),
                  fluidRow(uiOutput(ns("setupfilters")))
                  ),
                tabPanel("My transcripts",
                         ),
                tabPanel("My genomic regions",
                )
                ),
  )
}
    
#' variant_annoter Server Functions
#'
#' @noRd 
mod_parameters_management_server <- function(id, conn = NULL, modal = FALSE, reactiveValuesInputs = NULL,reload =  reactiveValues(value = 0)){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    print("entering mod_parameters_management_server")
    print(reactiveValuesInputs)
    
    user_presets <- reactive({
      user_presets <- presets %>% filter(user == Sys.getenv("SHINYPROXY_USERNAME"))
      return(user_presets)
    })
    
    observeEvent(input$selectset,{
      req(input$selectset)
      if(input$selectset == 'In used filter values'){
        reactiveValuesInputsInside <- reactiveValuesInputs
      } else {
        current_preset <- user_presets() %>% filter(name  = input$selectset)
        if(nrow(current_preset > 1)){
          readRDS(textConnection(current_preset$values))
        }
      }
    })

    output$ui <- renderUI({
      req(user_presets())
      tagList(
        fluidRow(
          br(),br(),
          column(width = 8, selectInput(width = '100%', inputId = ns('selectset'), label = "Select a preset to edit", choices = c(user_presets(),"In used filter values"))),
          column(width = 2, br(), actionButton(width = '100%', inputId = ns('addpreset'), icon = icon("plus"), label = NULL)),
          column(width = 2, br(), actionButton(width = '100%', inputId = ns('removepreset'), icon = icon("minus"), label = NULL)))
      )
   })
  
  observeEvent(input$addpreset,{
    req(input$addpreset)
    showModal(modalDialog(size = 'l',
      textInput(inputId = ns("newpresetname"), label = "Name your preset here"),
      easyClose = TRUE,
      footer = tagList(modalButton("Cancel"),
                       actionButton(ns("confirmadd"),"Add this preset")))
    )
  })
  observeEvent(input$confirmadd,{
    req(input$confirmadd)
    print(input$confirmadd)
  })
  observeEvent(input$removepreset,{
    req(input$removepreset)
    req(input$selectset)
    showModal(modalDialog(size = 'l',
                          # textInput(inputId = ns("newpresetname"), label = "Name your preset here"),
                          title = "Are you sure you want to delete the folowing preset ?",
                          HTML(paste(input$selectset)),
                          easyClose = TRUE,
                          footer = tagList(modalButton("Cancel"),
                                           actionButton(ns("confirmremove"),"Yes")))
    )
  })
  observeEvent(input$confirmremove,{
    req(input$confirmremove)
    print(input$confirmremove)
  })
    
    output$setupfilters <- renderUI({  
      req(reactiveValuesInputs)
      print("update current parameters in setup view")
      tagList(
        fluidRow(
        column(width = 12, h1("Selected preset values : "))),
        br(),
        fluidRow(
        column(width = 4,
               h2("Allele frequency :"),  
               numericInput(inputId = ns("allelefrequencynumsetup"), label = NULL ,width = '100%',step = 0.01,
                            value = reactiveValuesInputs$allelefrequencynum)),
        column(width = 4,
               h2("Coverage :"),  
               numericInput(inputId = ns("allelefrequencynumsetup"), label = NULL ,width = '100%',step = 0.01,
                            value = reactiveValuesInputs$allelefrequencynum)),
        column(width = 4,
               h2("Quality :"),  
               numericInput(inputId = ns("allelefrequencynumsetup"), label = NULL ,width = '100%',step = 0.01,
                            value = reactiveValuesInputs$allelefrequencynum))),
        fluidRow(
        column(width = 4,
               h2("Impact :"),  
               numericInput(inputId = ns("allelefrequencynumsetup"), label = NULL ,width = '100%',step = 0.01,
                            value = reactiveValuesInputs$allelefrequencynum))),
        fluidRow(actionButton(inputId = ns('save_params'), "Save current parameters"))
        )
    })

    observeEvent(input$save_params,{
      req(input$save_params)
      req(user_presets())
      print("Saving current parameters")  
      if(exists("zz")){rm(zz)}
      to_keep <-c("allelefrequencynum","allelefrequency",
                  "coveragenum","coverage",                                                            
                  "quality", "impact")
      inputs  <- reactiveValuesToList(reactiveValuesInputs)[to_keep]
      zz <- textConnection('tempConnection','wb')
      saveRDS(inputs,zz, ascii = TRUE)
      inputs_char <- paste(textConnectionValue(zz), collapse= '\n')
      print(inputs_char)
      print(head(user_presets()))
      current_preset <- user_presets() %>% filter(name  == input$presetname)
      print(current_preset)
      if(nrow(current_preset) >=1){
        print(paste0("already a preset named : ",input$newpresetname, " for the user : ", Sys.getenv("SHINYPROXY_USERNAME")))
      } else {
        current_preset <- data.frame(user = Sys.getenv("SHINYPROXY_USERNAME"), name = input$newpresetname, values = inputs_char)
        print(current_preset)
        DBI::dbWriteTable(conn = con, name="presets", value = current_preset, append = TRUE)
      }
    })
  })
  
  
  
}
