#' variant_annoter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @importFrom shinyWidgets closeSweetAlert progressSweetAlert updateProgressBar ask_confirmation pickerInput sendSweetAlert
#' @importFrom shiny NS tagList
#' @importFrom readr read_delim cols col_character
#' @importFrom shinydashboardPlus dashboardPage
mod_parameters_management_ui <- function(id, modal  = NULL, reactiveValues = NULL, conn = NULL){ #inputs = NULL){
  print("entering mod_parameters_management_ui")
  ns <- NS(id)
  fluidPage(
                dashboardHeader(
                  titleWidth = '25%',
                  title = span(img(src = 'www/CHUlogo.png', width = 40, height = 39), get_golem_options("app_title"))#,
                ),
                  br(),
                  fluidRow(column(width = 12 ,actionButton(label = "go back to analysis", inputId = ns("goroot"),icon = icon("arrow-left")))),
                  br(),
                  tabsetPanel(id = ns("tabsParams"),
                    tabPanel("My Filters",
                             br(),
                             fluidRow(
                             column(width = 8, align = "left",
                                      selectizeInput(width = '100%', inputId = ns('selectset'), label = "Select a preset to edit",
                                                    choices = NULL, selected = NULL)),
                             column(width = 2, align = "left",br(),
                                      actionButton(width = '100%', inputId = ns('addpreset'), icon = icon("plus"), label = NULL)),
                             column(width = 2, align = "left",br(),
                                      actionButton(width = '100%', inputId = ns('removepreset'), icon = icon("minus"), label = NULL))
                             ),
                             fluidRow(
                               column(width = 12, h1("Selected preset values : "))),br(),
                             fluidRow(
                               column(width = 4,
                                      h2("Allele frequency :"),  
                                      numericInput(inputId = ns("allelefrequencynumsetup"), label = NULL ,width = '100%',step = 0.01,value = 0)),
                               column(width = 4,
                                      h2("Coverage :"),  
                                      numericInput(inputId = ns("coveragenumsetup"), label = NULL ,width = '100%',step = 0.01,value = 0)),
                               column(width = 4,
                                      h2("Quality :"),  
                                      numericInput(inputId = ns("qualitynumsetup"), label = NULL ,width = '100%',step = 0.01,value = 0))),
                             fluidRow(
                               column(width = 4,
                                      h2("Impact :"),  
                                      selectInput(inputId = ns("impactsetup"), label = NULL ,width = '100%',
                                                  choices = c("Low", "Moderate","High"))),
                               column(width = 4,
                                      h2("Prefered transcripts list :"),  
                                      selectInput(inputId = ns("trlistsetup"), label = NULL ,width = '100%',
                                                  choices = "None", selected = "None"))
                              ),
                             fluidRow(column(width = 12,actionButton(inputId = ns('save_params'), "Save current parameters")))
                    ),
                  tabPanel("My transcripts",
                           br(),
                           fluidRow(column(width = 9, align = "left",
                                  selectizeInput(width = '100%', inputId = ns('selectlist'), label = "Select a transcript list to visualize",
                                                 choices = NULL, selected = NULL)),
                           column(width = 3, align = "left",br(),
                                  actionButton(width = '100%', inputId = ns('removelist'), icon = icon("minus"), label = NULL))),
                           fluidRow(shinydashboardPlus::box(
                             title = "Add a new list", closable = FALSE ,solidHeader = TRUE,
                             width = 12, status = "primary", collapsible = TRUE,collapsed = FALSE,
                             dropdownMenu = boxDropdown(
                               boxDropdownItem("Show an input file example", id = ns("dropdownExample")),icon = icon("info")),
                              column(width = 8,
                                  fileInput(ns("file"), "Upload a Text File"),
                                  helpText("The file should contain two columns named: Transcripts (RefSeq format) and Genes (Hugo symbols format).")),
                              column(width = 4,
                                  selectInput(ns("separator"),"Choose a field separator" ,
                                              choices = c("Tabulation" = "\t", 
                                                          "comma" = ",", 
                                                          "space" = " ",
                                                          "semicolon" = ";")),
                                  helpText("How are your columns separated in your input file")
                              ),
                              uiOutput(ns("transcriptlistui")),
                              br()
                           ))
                         ),
                tabPanel("My genomic regions",
                )
                )
  )
}
    
#' variant_annoter Server Functions
#'
#' @noRd 
mod_parameters_management_server <- function(id, conn = NULL, modal = FALSE, reactiveValuesInputs = NULL,reload =  reactiveValues(value = 0)){
  moduleServer( id, function(input, output, session){
    
    observeEvent(input$goroot,{
      req(input$goroot)
      change_page('/')
    })
    ns <- session$ns
    print("entering mod_parameters_management_server")
    req(reactiveValuesInputs)
    print(reactiveValuesInputs)
    
    ######################################## MY FILTERS  ##################################
    if(DBI::dbExistsTable(con,"presets")){
      print('load initial presets')
      presets <- DBI::dbReadTable(con,"presets")

      transcript_lists <- DBI::dbGetQuery(conn = con, paste0("SELECT name FROM sqlite_master WHERE type='table' AND name LIKE '%", 
                                         Sys.getenv("SHINYPROXY_USERNAME"),"_transcriptlist",
                                         "%';"))

      transcript_lists <- gsub(paste0("_",Sys.getenv("SHINYPROXY_USERNAME")),"",gsub("_transcriptlist","",transcript_lists$name))
  
      user_presets <- reactiveValues(filters = presets %>% filter(user == Sys.getenv("SHINYPROXY_USERNAME")), init = FALSE,
                                     transcript_lists = transcript_lists)
    }
    
    observe({
      if(user_presets$init == FALSE){
        print("init user preset"); req(reactiveValuesInputs$allelefrequencynum); req(reactiveValuesInputs$coveragenum); req(reactiveValuesInputs$qualitynum); req(reactiveValuesInputs$impact)
        user_presets$init <- TRUE
      }
    })
    
    observeEvent(user_presets$init,ignoreInit = FALSE,ignoreNULL = FALSE,{
      req(user_presets$init)
      updateSelectInput(session = session, inputId = 'selectset', choices = c(user_presets$filters$name,"In use filter values"), selected = "In use filter values")
      updateSelectInput(session = session, inputId = 'selectlist', choices = user_presets$transcript_lists)
      updateSelectInput(session = session, inputId = 'trlistsetup', choices = c(user_presets$transcript_lists,"None"), selected = c("None"))
    })
    
    observeEvent(input$confirmadd,ignoreNULL = TRUE,{  
      req(user_presets$filters)
      if(DBI::dbExistsTable(con,"presets")){
        presets <- DBI::dbReadTable(con,"presets")
        user_presets$filters <- presets %>% filter(user == Sys.getenv("SHINYPROXY_USERNAME"))
        updateSelectInput(session = session, inputId = 'selectset', choices = c(user_presets$filters$name,"In use filter values"), selected = input$newpresetname)
      }
    })
    observeEvent(input$confirmremove,ignoreNULL = TRUE,{  
      req(user_presets$filters)
      if(DBI::dbExistsTable(con,"presets")){
        presets <- DBI::dbReadTable(con,"presets")
        user_presets$filters <- presets %>% filter(user == Sys.getenv("SHINYPROXY_USERNAME"))
        updateSelectInput(session = session, inputId = 'selectset', choices = c(user_presets$filters$name,"In use filter values"))
      }
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
    observeEvent(input$confirmadd,priority = 100, {
      req(input$newpresetname)
      req(input$confirmadd)
      current_preset <- data.frame(user = Sys.getenv("SHINYPROXY_USERNAME"), name = input$newpresetname, 
                                   allelefrequencynum = 'Emptypreset',
                                   coveragenum = 'Emptypreset', 
                                   qualitynum = 'Emptypreset', 
                                   impact = 'Emptypreset')
      DBI::dbWriteTable(conn = con, name ="presets", value = current_preset, append = TRUE)
      removeModal()
    })
  
    observeEvent(input$removepreset,{
      req(input$removepreset)
      req(input$selectset)
      showModal(modalDialog(size = 'l',
                            title = "Are you sure you want to delete the folowing preset ?",
                            HTML(paste(input$selectset)),
                            easyClose = TRUE,
                            footer = tagList(modalButton("Cancel"),
                                             actionButton(ns("confirmremove"),"Yes")))
      )
    })
    observeEvent(input$confirmremove, priority = 100,{
      req(input$confirmremove)
      req(input$selectset)
      print(paste0("DELETE FROM presets WHERE user = '", Sys.getenv("SHINYPROXY_USERNAME"), "' AND name = '",input$selectset,"' ;"))
      DBI::dbSendQuery(conn = con,paste0("DELETE FROM presets WHERE user = '", Sys.getenv("SHINYPROXY_USERNAME"), "' AND name = '",input$selectset,"' ;"))
      removeModal()
    })
    reactiveValuesInputsInside <- reactiveValues("allelefrequencynum" = 0,
                                                 "coveragenum" = 0 ,
                                                 "qualitynum" = 0 ,
                                                 "impact" = 0,
                                                 "trlist" = "None")

    observeEvent(c(input$selectset,
                 reactiveValuesInputs$allelefrequencynum,
                 reactiveValuesInputs$coveragenum,
                 reactiveValuesInputs$qualitynum,
                 reactiveValuesInputs$impact,
                 reactiveValuesInputs$trlist),ignoreNULL = TRUE, {
      req(input$selectset)
      req(user_presets$filters)
      req(input$selectset)
        if(input$selectset == 'In use filter values'){
        print('Load In use filter values')
        reactiveValuesInputsInside$allelefrequencynum <- reactiveValuesInputs$allelefrequencynum 
        reactiveValuesInputsInside$coveragenum <- reactiveValuesInputs$coveragenum
        reactiveValuesInputsInside$qualitynum <- reactiveValuesInputs$qualitynum
        reactiveValuesInputsInside$impact <- reactiveValuesInputs$impact
        reactiveValuesInputsInside$trlist <- reactiveValuesInputs$trlist
      } else {
        print(paste(' load ',input$selectset,' filter values'))
        presets <- DBI::dbReadTable(con,"presets")
        current_preset <- user_presets$filters %>% filter(name  == input$selectset)
        if(current_preset$allelefrequencynum != "Emptypreset"){
          print(paste0("reading", current_preset$name, " preset values..." ))
          values <- DBI::dbGetQuery(conn = con, paste0("SELECT  allelefrequencynum, coveragenum , qualitynum , impact, trlist FROM presets ",
                                                "WHERE user = '", Sys.getenv("SHINYPROXY_USERNAME"), "' AND name = '",input$selectset,"' ;"))
          reactiveValuesInputsInside$allelefrequencynum <- values$allelefrequencynum 
          reactiveValuesInputsInside$coveragenum <- values$coveragenum
          reactiveValuesInputsInside$qualitynum <- values$qualitynum
          reactiveValuesInputsInside$impact <- values$impact
          reactiveValuesInputsInside$trlist <- values$trlist
        }
      }
    })

    observeEvent(reactiveValuesInputsInside$allelefrequencynum,{
      req(reactiveValuesInputsInside$allelefrequencynum)
      updateNumericInput(session = session, inputId = 'allelefrequencynumsetup', value = reactiveValuesInputsInside$allelefrequencynum)
    })
    observeEvent(reactiveValuesInputsInside$coveragenum,{
      req(reactiveValuesInputsInside$coveragenum)
      updateNumericInput(session = session, inputId = 'coveragenumsetup', value = reactiveValuesInputsInside$coveragenum)
    })
    observeEvent(reactiveValuesInputsInside$qualitynum,{
      req(reactiveValuesInputsInside$qualitynum)
      updateNumericInput(session = session, inputId = 'qualitynumsetup', value = reactiveValuesInputsInside$qualitynum)
    })
    observeEvent(reactiveValuesInputsInside$impact,{
      req(reactiveValuesInputsInside$impact)
      updateSelectInput(session = session, inputId = 'impactsetup', selected = reactiveValuesInputsInside$impact)
    })
    observeEvent(reactiveValuesInputsInside$trlist,{
      req(reactiveValuesInputsInside$trlist)
      updateSelectInput(session = session, inputId = 'trlistsetup', selected = reactiveValuesInputsInside$trlist)
    })      

    reactiveValuesInputstoSave <-  reactiveValues("allelefrequencynum" = 0, "coveragenum" = 0 , "qualitynum" = 0, "impact" = "Low","trlist" = "None")
  
    observeEvent(input$allelefrequencynumsetup,{reactiveValuesInputstoSave$allelefrequencynum <-  input$allelefrequencynumsetup})
    observeEvent(input$coveragenumsetup,{reactiveValuesInputstoSave$coveragenum <- input$coveragenumsetup})
    observeEvent( input$qualitynumsetup,{reactiveValuesInputstoSave$qualitynum <- input$qualitynumsetup })
    observeEvent( input$impactsetup ,{reactiveValuesInputstoSave$impact <- input$impactsetup })
    observeEvent( input$trlistsetup,{reactiveValuesInputstoSave$trlist <- input$trlistsetup })
  
    observeEvent(input$save_params,{
      req(reactiveValuesInputstoSave); req(input$save_params); req(user_presets$filters); req(input$selectset);req(input$trlistsetup)
      print("Saving current parameters")
      current_preset <- user_presets$filters %>% filter(name  == input$selectset)
      if(nrow(current_preset) >=1){
        DBI::dbSendQuery(conn = con, paste0("UPDATE presets SET ",
                                            "allelefrequencynum = '",reactiveValuesInputstoSave$allelefrequencynum , "', ",
                                            "coveragenum = '",reactiveValuesInputstoSave$coveragenum , "', ",
                                            "qualitynum = '", reactiveValuesInputstoSave$qualitynum , "', ",
                                            "impact = '", reactiveValuesInputstoSave$impact , "', ",
                                            "trlist = '", input$trlistsetup, "' ",
                                            "WHERE user = '", Sys.getenv("SHINYPROXY_USERNAME"), "' AND name = '",input$selectset,"' ;"))
        presets <- DBI::dbReadTable(con,"presets")
        user_presets$filters <- presets %>% filter(user == Sys.getenv("SHINYPROXY_USERNAME"))
        sendSweetAlert(session = session,title = "Preset parameters saved !", text = paste(input$selectset, "preset has been updated"),type = "success")
        reactiveValuesInputsInside$allelefrequencynum <- reactiveValuesInputstoSave$allelefrequencynum 
        reactiveValuesInputsInside$coveragenum <- reactiveValuesInputstoSave$coveragenum
        reactiveValuesInputsInside$qualitynum <- reactiveValuesInputstoSave$qualitynum
        reactiveValuesInputsInside$impact <- reactiveValuesInputstoSave$impact
        reactiveValuesInputsInside$trlist <- reactiveValuesInputstoSave$trlist
      } else {
        showModal(modalDialog(size = 'l',
                              textInput(inputId = ns("newpresetnamecurrent"), label = "Name your preset here to save current parameters"),
                              easyClose = TRUE,
                              footer = tagList(modalButton("Cancel"),
                                               actionButton(ns("confirmaddcurrent"),"Add this preset")))
        )
      }
    })
    observeEvent(input$confirmaddcurrent, {
                  req(input$confirmaddcurrent); req(input$newpresetnamecurrent); req(reactiveValuesInputstoSave);req(input$trlistsetup)
                  removeModal()
                  current_preset <- data.frame(
                    "allelefrequencynum" = reactiveValuesInputstoSave$allelefrequencynum ,
                    "coveragenum" = reactiveValuesInputstoSave$coveragenum,
                    "qualitynum" = reactiveValuesInputstoSave$qualitynum, 
                    "impact" = reactiveValuesInputstoSave$impact,
                    "trlist" = input$trlistsetup,
                    "user" =  Sys.getenv("SHINYPROXY_USERNAME"),
                    "name" = input$newpresetnamecurrent)
                  DBI::dbWriteTable(conn = con, name = "presets", value = current_preset, append =TRUE)
                  presets <- DBI::dbReadTable(con,"presets")
                  user_presets$filters <- presets %>% filter(user == Sys.getenv("SHINYPROXY_USERNAME"))
                  updateSelectInput(session = session, inputId = 'selectset', choices = c(user_presets$filters$name,"In use filter values"), selected = input$newpresetname)
                  sendSweetAlert(session = session,title = "Preset parameters saved !", 
                                 text = paste(input$newpresetnamecurrent, 
                                              "preset has been updated"),type = "success")
                  reactiveValuesInputsInside$allelefrequencynum <- reactiveValuesInputstoSave$allelefrequencynum 
                  reactiveValuesInputsInside$coveragenum <- reactiveValuesInputstoSave$coveragenum
                  reactiveValuesInputsInside$qualitynum <- reactiveValuesInputstoSave$qualitynum
                  reactiveValuesInputsInside$impact <- reactiveValuesInputstoSave$impact
                  reactiveValuesInputsInside$trlist <- reactiveValuesInputstoSave$trlist
    })
  
    ######################################## MY TRANSCRIPTS  ##################################
    
    # Read the uploaded file and check its format
    file_data <- reactive({
        req(input$file)
        req(input$separator)
        file <- input$file$datapath
        tryCatch({
          data <- read_delim(file, col_types = cols(Transcripts = col_character(), Genes = col_character()),delim = input$separator)
          return(data)
        }, error = function(e) {
          print(e)
        })
      })
    # Check if the file format is correct
    check_format <- function(data) {
      if (all(c("Transcripts", "Genes") %in% colnames(data))) {
        return(TRUE) } else { return(FALSE)
      }
    }
    # Display the data if the format is correct
    output$data_table <- renderTable({
      req(file_data())
      if (check_format(file_data())) {
        file_data()
      }
    })
    output$transcriptlistui <- renderUI({
      if (isTruthy(check_format(file_data()))) {
        fluidRow(
          column(width = 8 ,
            br(), "File format is correct. Here is your currently uploaded transcripts list :",br(),
            tableOutput(ns("data_table"))),
          column(width = 4, 
            br(), actionButton(width = '100%', inputId = ns('addtranscriptlist'), icon = NULL, label = 'Add this list')
            )
        )
      } else {
        "File format is not correct. Make sure the file contains 'Transcripts' and 'Genes' columns."
      }
    })
    # Display the example correctly formatted data
    output$example_data_table <- renderTable({
      data.frame(
        Transcripts = c("NM_006015.6", "NM_002168.4", "NM_006206.6"),
        Genes = c("ARID1A", "NADP", "PDGFRA")
      )
    })
    
    observeEvent(input$addtranscriptlist,{
      req(input$addtranscriptlist)
      showModal(modalDialog(size = 'l',
                            textInput(inputId = ns("newtranscriptlistname"), label = "Name your preset here"),
                            easyClose = TRUE,
                            footer = tagList(modalButton("Cancel"),
                                             actionButton(ns("confirmtranscriptlist"),"Add this transcript list")))
      )
    })
    
    observeEvent(input$confirmtranscriptlist, {
      req(input$confirmtranscriptlist)
      req(input$newtranscriptlistname)
      req(file_data())
      removeModal()
      DBI::dbWriteTable(conn = con, name = paste0(input$newtranscriptlistname, "_" , Sys.getenv("SHINYPROXY_USERNAME"),"_transcriptlist"), value = file_data())
      user_presets$transcript_lists <- c(input$newtranscriptlistname,user_presets$transcript_lists)
      updateSelectInput(session = session, inputId = 'selectlist', choices = user_presets$transcript_lists, selected = input$newtranscriptlistname)
      updateSelectInput(session = session, inputId = 'trlistsetup', choices = c("None", user_presets$transcript_lists))
      sendSweetAlert(session = session,
                     title = "Transcript List added", 
                     text = paste(input$newtranscriptlistname, "preset has been updated"),
                     type = "success")
      
    })
    observeEvent(input$removelist,{
      req(input$removelist)
      req(input$selectlist)
      print(paste0("DROP TABLE IF EXISTS ",input$selectlist, "_" , Sys.getenv("SHINYPROXY_USERNAME"),"_transcriptlist;"))
      DBI::dbSendQuery(conn = con,paste0("DROP TABLE IF EXISTS ",input$selectlist, "_" , Sys.getenv("SHINYPROXY_USERNAME"),"_transcriptlist;"))
      user_presets$transcript_lists <- user_presets$transcript_lists[user_presets$transcript_lists != input$selectlist]
      updateSelectInput(session = session, inputId = 'selectlist', choices = c(user_presets$transcript_lists ,"None"))
    })
    
    observeEvent(input$dropdownExample,{
       req(input$dropdownExample)
       showModal(modalDialog(size = 'l',
                             column(width = 12, 
                                    h3("Example of Correctly Formatted Data:"),br(),
                                    tableOutput(ns("example_data_table")),br(),
                                    h3("File should be a text file with column separator either a comma, a tabulation, a semicolon, or a space")),
                            easyClose = TRUE,
                            footer = tagList(modalButton("Cancel"))
      ))
    })
    
  #### END OF SERVER PART ####
  })
}
