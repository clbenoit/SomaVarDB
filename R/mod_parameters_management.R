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
#' @importFrom DBI dbExecute
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
                                      h2("Allele frequency max:"),
                                      numericInput(inputId = ns("allelefrequencynummaxsetup"), label = NULL ,width = '100%',step = 0.01,value = 1)),
                               column(width = 4,
                                      h2("Allele frequency min:"),
                                      numericInput(inputId = ns("allelefrequencynumminsetup"), label = NULL ,width = '100%',step = 0.01,value = 0)),                               
                               column(width = 4,
                                      h2("Coverage :"),
                                      numericInput(inputId = ns("coveragenumsetup"), label = NULL ,width = '100%',step = 0.01,value = 0))),
                             fluidRow(
                               column(width = 4,
                                      h2("Quality :"),
                                      numericInput(inputId = ns("qualitynumsetup"), label = NULL ,width = '100%',step = 0.01,value = 0)),
                               column(width = 4,
                                      h2("gnomAd frequency :"),  
                                      numericInput(inputId = ns("gnomadnumsetup"), label = NULL ,width = '100%',step = 0.01,value = 0, max = 1, min = 0)),
                               column(width = 4,
                                      h2("Prefered manifest :"),
                                      selectInput(inputId = ns("manifestlistsetup"), label = NULL ,width = '100%',                                      
                                                  choices = "None", selected = "None"))                               
                              ),
                             fluidRow(
                               column(width = 4,
                                      h2("Prefered transcripts list :"),
                                      selectInput(inputId = ns("trlistsetup"), label = NULL ,width = '100%',
                                                  choices = "None", selected = "None")),
                               column(width = 4,
                                      h2("Impact :"),  
                                      selectInput(inputId = ns("impactsetup"), label = NULL ,width = '100%',
                                                  choices = c("Low", "Moderate","High"), selected = "Low"))
                             ),                             
                             fluidRow(column(width = 12,actionButton(inputId = ns('save_params'), "Save current parameters")))
                    ),
                  tabPanel("My transcripts",
                           br(),
                           fluidRow(column(width = 9, align = "left",
                                  selectizeInput(width = '100%', inputId = ns('selectlist'), label = "Select a transcript list to remove",
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
                         br(),br(),
                         fluidRow(column(width = 9, align = "left",
                                         selectizeInput(width = '100%', inputId = ns('selectManifest'), label = "Select a manifest to remove",
                                                        choices = NULL, selected = NULL)),
                                  column(width = 3, align = "left",br(),
                                         actionButton(width = '100%', inputId = ns('removeManifest'), icon = icon("minus"), label = NULL))),
                         fluidRow(shinydashboardPlus::box(
                           title = "Add a new manifest", closable = FALSE ,solidHeader = TRUE,
                           width = 12, status = "primary", collapsible = TRUE,collapsed = FALSE,
                           dropdownMenu = boxDropdown(
                             boxDropdownItem("Show an input file example", id = ns("dropdownExampleManifest")),icon = icon("info")),
                           column(width = 8,
                                  fileInput(ns("newmanifestfile"), "Upload a Text File"),
                                  helpText("The file should contain three columns named: chromosome, start and end (Columns names are case sensitive).")),
                           column(width = 4,
                                  selectInput(ns("separatorManifest"),"Choose a field separator" ,
                                              choices = c("Tabulation" = "\t", 
                                                          "comma" = ",", 
                                                          "space" = " ",
                                                          "semicolon" = ";")),
                                  helpText("How are your columns separated in your input file")
                           ),
                           uiOutput(ns("Manifestsui")),
                           br()
                         ))                     
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
  
      manifests_list <- DBI::dbReadTable(conn = con, name = "manifests_list") %>% filter(user_id == Sys.getenv("SHINYPROXY_USERNAME"))
      manifests_list <- gsub(paste0("_",Sys.getenv("SHINYPROXY_USERNAME")),"",manifests_list$manifests)
      
      user_presets <- reactiveValues(filters = presets %>% filter(user == Sys.getenv("SHINYPROXY_USERNAME")), 
                                     #init = FALSE,
                                     init = 0,
                                     transcript_lists = transcript_lists, manifests_list = manifests_list)
    }
    
    observeEvent(c(reactiveValuesInputs$allelefrequencynummax,reactiveValuesInputs$allelefrequencynummin,reactiveValuesInputs$coveragenum,reactiveValuesInputs$qualitynum,reactiveValuesInputs$impact,reactiveValuesInputs$gnomadnum),{  
      print("init user preset");req(reactiveValuesInputs$allelefrequencynummax); req(reactiveValuesInputs$allelefrequencynummin); req(reactiveValuesInputs$coveragenum); req(reactiveValuesInputs$qualitynum); req(reactiveValuesInputs$impact);req(reactiveValuesInputs$gnomadnum)
      user_presets$init <- user_presets$init + 1
    })    
    
    observeEvent(user_presets$init,ignoreInit = FALSE,ignoreNULL = FALSE,{
      req(user_presets$init)
      print("update user metadata (trlist,presetslist,manifestslist...) in mod_parameters_management module")
      print(user_presets$manifests_list)
      updateSelectInput(session = session, inputId = 'selectset', choices = c(user_presets$filters$name,"In use filter values"), selected = "In use filter values")
      updateSelectInput(session = session, inputId = 'selectlist', choices = user_presets$transcript_lists)
      updateSelectInput(session = session, inputId = 'trlistsetup', choices = c(user_presets$transcript_lists,"None"), selected = c("None"))
      updateSelectInput(session = session, inputId = 'selectManifest', choices = user_presets$manifests_list)
      updateSelectInput(session = session, inputId = 'manifestlistsetup', choices = c(user_presets$manifests_list,"None"), selected = c("None"))
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
                                   allelefrequencynumin = 'Emptypreset',
                                   allelefrequencynummax = 'Emptypreset',
                                   coveragenum = 'Emptypreset', 
                                   qualitynum = 'Emptypreset',
                                   gnomadnum = 'Emptypreset', 
                                   impact = 'Emptypreset', 
                                   trlist = 'Emptypreset',
                                   manifest = 'Emptypreset')
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
    reactiveValuesInputsInside <- reactiveValues("allelefrequencynummin" = 0,
                                                 "allelefrequencynummax" = 1,
                                                 "coveragenum" = 0 ,
                                                 "qualitynum" = 0 ,
                                                 "gnomadnum" = 0,                                                 
                                                 "impact" = 0,
                                                 "trlist" = "None", 
                                                 "manifest" = "None")

    observeEvent(c(input$selectset,
                 reactiveValuesInputs$allelefrequency,
                 reactiveValuesInputs$coveragenum,
                 reactiveValuesInputs$qualitynum,
                 reactiveValuesInputs$gnomadfrequency,                 
                 reactiveValuesInputs$impact,
                 reactiveValuesInputs$trlist,
                 reactiveValuesInputs$manifest),ignoreNULL = TRUE, {
      req(input$selectset)
      req(user_presets$filters)
      if(input$selectset == 'In use filter values'){
        print('Load In use filter values')
        reactiveValuesInputsInside$allelefrequencynummax <- reactiveValuesInputs$allelefrequency[2]
        reactiveValuesInputsInside$allelefrequencynummin <- reactiveValuesInputs$allelefrequency[1]
        reactiveValuesInputsInside$coveragenum <- reactiveValuesInputs$coveragenum
        reactiveValuesInputsInside$qualitynum <- reactiveValuesInputs$qualitynum
        reactiveValuesInputsInside$gnomadnum <- reactiveValuesInputs$gnomadfrequency        
        reactiveValuesInputsInside$impact <- reactiveValuesInputs$impact
        reactiveValuesInputsInside$trlist <- reactiveValuesInputs$trlist
        reactiveValuesInputsInside$manifest <- reactiveValuesInputs$manifest        
      } else {
        print(paste('Load ',input$selectset,' preset filters values (inside module)'))
        presets <- DBI::dbReadTable(con,"presets")
        current_preset <- user_presets$filters %>% filter(name  == input$selectset)
        if(current_preset$allelefrequencynummax != "Emptypreset"){
          values <- DBI::dbGetQuery(conn = con, paste0("SELECT  allelefrequencynummax, allelefrequencynummin, coveragenum , qualitynum , gnomadnum , impact, trlist, manifest FROM presets ",
                                                "WHERE user = '", Sys.getenv("SHINYPROXY_USERNAME"), "' AND name = '",input$selectset,"' ;"))
          reactiveValuesInputsInside$allelefrequencynummax <- values$allelefrequencynummax
          reactiveValuesInputsInside$allelefrequencynummin <- values$allelefrequencynummin
          reactiveValuesInputsInside$coveragenum <- values$coveragenum
          reactiveValuesInputsInside$qualitynum <- values$qualitynum
          reactiveValuesInputsInside$gnomadnum <- values$gnomadnum          
          reactiveValuesInputsInside$impact <- values$impact
          reactiveValuesInputsInside$trlist <- values$trlist
          reactiveValuesInputsInside$manifest <- values$manifest  
        }
      }
    })

    observeEvent(reactiveValuesInputsInside$allelefrequencynummax,{
      req(reactiveValuesInputsInside$allelefrequencynummax)
      updateNumericInput(session = session, inputId = 'allelefrequencynummaxsetup', value = reactiveValuesInputsInside$allelefrequencynummax)
    })
    observeEvent(reactiveValuesInputsInside$allelefrequencynummin,{
      req(reactiveValuesInputsInside$allelefrequencynummin)
      updateNumericInput(session = session, inputId = 'allelefrequencynumminsetup', value = reactiveValuesInputsInside$allelefrequencynummin)
    })
    observeEvent(reactiveValuesInputsInside$coveragenum,{
      req(reactiveValuesInputsInside$coveragenum)
      updateNumericInput(session = session, inputId = 'coveragenumsetup', value = reactiveValuesInputsInside$coveragenum)
    })
    observeEvent(reactiveValuesInputsInside$qualitynum,{
      req(reactiveValuesInputsInside$qualitynum)
      updateNumericInput(session = session, inputId = 'qualitynumsetup', value = reactiveValuesInputsInside$qualitynum)
    })
    observeEvent(reactiveValuesInputsInside$gnomadnum,{
      req(reactiveValuesInputsInside$gnomadnum)
      updateNumericInput(session = session, inputId = 'gnomadnumsetup', value = reactiveValuesInputsInside$gnomadnum)
    })
    observeEvent(reactiveValuesInputsInside$impact,{
      req(reactiveValuesInputsInside$impact)
      updateSelectInput(session = session, inputId = 'impactsetup', selected = reactiveValuesInputsInside$impact)
    })
    observeEvent(reactiveValuesInputsInside$trlist,{
      req(reactiveValuesInputsInside$trlist)
      updateSelectInput(session = session, inputId = 'trlistsetup', selected = reactiveValuesInputsInside$trlist)
    })
    observeEvent(reactiveValuesInputsInside$manifest,{
      req(reactiveValuesInputsInside$manifest)
      updateSelectInput(session = session, inputId = 'manifestlistsetup', selected = reactiveValuesInputsInside$manifest)
    })    

    reactiveValuesInputstoSave <-  reactiveValues("allelefrequencynummax" = 0,"allelefrequencynummin" = 1, "coveragenum" = 0 , "qualitynum" = 0, "gnomadnum" = 0,
                                                  "impact" = "Low","trlist" = "None", "manifest" = "None")
    
    observeEvent(input$allelefrequencynumminsetup,{reactiveValuesInputstoSave$allelefrequencynummin <-  input$allelefrequencynumminsetup})
    observeEvent(input$allelefrequencynummaxsetup,{reactiveValuesInputstoSave$allelefrequencynummax <-  input$allelefrequencynummaxsetup})    
    observeEvent(input$coveragenumsetup,{reactiveValuesInputstoSave$coveragenum <- input$coveragenumsetup})
    observeEvent( input$qualitynumsetup,{reactiveValuesInputstoSave$qualitynum <- input$qualitynumsetup })
    observeEvent( input$gnomadnumsetup,{reactiveValuesInputstoSave$gnomadnum <- input$gnomadnumsetup })    
    observeEvent( input$impactsetup ,{reactiveValuesInputstoSave$impact <- input$impactsetup })
    observeEvent( input$trlistsetup,{reactiveValuesInputstoSave$trlist <- input$trlistsetup })
    observeEvent( input$manifestlistsetup,{reactiveValuesInputstoSave$manifest <- input$manifestlistsetup })  
  
    observeEvent(input$save_params,{
      req(reactiveValuesInputstoSave); req(input$save_params); req(user_presets$filters); req(input$selectset);req(input$trlistsetup);req(input$manifestlistsetup)
      print("Saving current parameters")
      current_preset <- user_presets$filters %>% filter(name  == input$selectset)
      if(nrow(current_preset) >=1){
        DBI::dbSendQuery(conn = con, paste0("UPDATE presets SET ",
                                            "allelefrequencynummin = '",reactiveValuesInputstoSave$allelefrequencynummin , "', ",
                                            "allelefrequencynummax = '",reactiveValuesInputstoSave$allelefrequencynummax , "', ",                                            
                                            "coveragenum = '",reactiveValuesInputstoSave$coveragenum , "', ",
                                            "qualitynum = '", reactiveValuesInputstoSave$qualitynum , "', ",
                                            "gnomadnum = '", reactiveValuesInputstoSave$gnomadnum , "', ",                                            
                                            "impact = '", reactiveValuesInputstoSave$impact , "', ",
                                            "trlist = '", input$trlistsetup, "', ",
                                            "manifest = '", input$manifestlistsetup, "' ",
                                            "WHERE user = '", Sys.getenv("SHINYPROXY_USERNAME"), "' AND name = '",input$selectset,"' ;"))
        presets <- DBI::dbReadTable(con,"presets")
        user_presets$filters <- presets %>% filter(user == Sys.getenv("SHINYPROXY_USERNAME"))
        sendSweetAlert(session = session,title = "Preset parameters saved !", 
                       text = HTML(paste("<p style='color:#086A87;'>", input$selectset, "</p>","preset has been updated")),
                       html = TRUE,
                       type = "success")
        reactiveValuesInputsInside$allelefrequencynummax <- reactiveValuesInputstoSave$allelefrequencynummax
        reactiveValuesInputsInside$allelefrequencynummin <- reactiveValuesInputstoSave$allelefrequencynunmin        
        reactiveValuesInputsInside$coveragenum <- reactiveValuesInputstoSave$coveragenum
        reactiveValuesInputsInside$qualitynum <- reactiveValuesInputstoSave$qualitynum
        reactiveValuesInputsInside$gnomadnum <- reactiveValuesInputstoSave$gnomadnum        
        reactiveValuesInputsInside$impact <- reactiveValuesInputstoSave$impact
        reactiveValuesInputsInside$trlist <- reactiveValuesInputstoSave$trlist
        reactiveValuesInputsInside$manifest <- reactiveValuesInputstoSave$manifest        
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
                  req(input$confirmaddcurrent); req(input$newpresetnamecurrent); req(reactiveValuesInputstoSave);req(input$trlistsetup);req(input$manifestlistsetup)
                  removeModal()
                  current_preset <- data.frame(
                    "allelefrequencynummin" = reactiveValuesInputstoSave$allelefrequencynummin ,
                    "allelefrequencynummax" = reactiveValuesInputstoSave$allelefrequencynummax ,                    
                    "coveragenum" = reactiveValuesInputstoSave$coveragenum,
                    "qualitynum" = reactiveValuesInputstoSave$qualitynum, 
                    "gnomadnum" = reactiveValuesInputstoSave$gnomadnum,                    
                    "impact" = reactiveValuesInputstoSave$impact,
                    "trlist" = input$trlistsetup,
                    "manifest" = input$manifestlistsetup,                    
                    "user" =  Sys.getenv("SHINYPROXY_USERNAME"),
                    "name" = input$newpresetnamecurrent)
                  DBI::dbWriteTable(conn = con, name = "presets", value = current_preset, append =TRUE)
                  presets <- DBI::dbReadTable(con,"presets")
                  user_presets$filters <- presets %>% filter(user == Sys.getenv("SHINYPROXY_USERNAME"))
                  updateSelectInput(session = session, inputId = 'selectset', choices = c(user_presets$filters$name,"In use filter values"), selected = input$newpresetname)
                  sendSweetAlert(session = session,title = HTML(paste0("<p style='color:#086A87;'>",input$newpresetnamecurrent,"</p>", " Parameters preset added !")), 
                                 text = "You might have to restart the app to see it available in data analysis window",
                                 type = "success")
                  reactiveValuesInputsInside$allelefrequencynummin <- reactiveValuesInputstoSave$allelefrequencynummin
                  reactiveValuesInputsInside$allelefrequencynummax <- reactiveValuesInputstoSave$allelefrequencynummax                   
                  reactiveValuesInputsInside$coveragenum <- reactiveValuesInputstoSave$coveragenum
                  reactiveValuesInputsInside$qualitynum <- reactiveValuesInputstoSave$qualitynum
                  reactiveValuesInputsInside$gnomadnum <- reactiveValuesInputstoSave$gnomadnum                  
                  reactiveValuesInputsInside$impact <- reactiveValuesInputstoSave$impact
                  reactiveValuesInputsInside$trlist <- reactiveValuesInputstoSave$trlist
                  reactiveValuesInputsInside$manifest <- reactiveValuesInputstoSave$manifest                  
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
        }, error = function(e) {print(e)})        
      })
    # Check if the file format is correct
    check_format <- function(data) {if (all(c("Transcripts", "Genes") %in% colnames(data))) {return(TRUE) } else { return(FALSE)}}
    # Display the data if the format is correct
    output$data_table <- renderTable({
      req(file_data())
      if (check_format(file_data())) {file_data()}
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
      } else { "File format is not correct. Make sure the file contains 'Transcripts' and 'Genes' columns."}
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
                     title = HTML(paste0("<p style='color:#086A87;'>", input$newtranscriptlistname,"</p>",
                                         " Transcript List added !")), 
                     text = "You might have to restart the app to see it available in data analysis window",
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

    ######################################## MY GENOMIC REGIONS  ##################################
    
    observeEvent(input$addnewmanifest,{
      req(input$addnewmanifest)
      showModal(modalDialog(size = 'l',
                            column(width =6,
                                   textInput(inputId = ns("newmanifestname"), label = "Name your manifest here", width = "100%")
                            ),
                            easyClose = TRUE,
                            footer = tagList(modalButton("Cancel"),
                                             actionButton(ns("confirmManifest"),"Add this manifest")))
      )
    })
    
    # Read the uploaded file and check its format
    file_dataManifest <- reactive({
      req(input$newmanifestfile$datapath)
      req(input$separatorManifest)
      file <- input$newmanifestfile$datapath
      tryCatch({
        data <- read_delim(file, col_types = cols(chromosome = col_character(), start = col_character(), end = col_character()),delim = input$separatorManifest)
        return(data)
      }, error = function(e) {print(e)})
    })
    # Check if the file format is correct
    check_formatManifest <- function(data) {
      if (all(c("chromosome", "start","end") %in% colnames(data))) {return(TRUE) } else {return(FALSE)}
    }
    # Display the data if the format is correct
    output$data_tableManifest <- renderTable({
      req(file_dataManifest())
      if (check_formatManifest(file_dataManifest())) {file_dataManifest()}
    })
    output$Manifestsui <- renderUI({
      if (isTruthy(check_formatManifest(file_dataManifest()))) {
        fluidRow(
          column(width = 8 ,
                 br(), "File format is correct. Here is your currently uploaded manifest :",br(),
                 tableOutput(ns("data_tableManifest"))),
          column(width = 4, 
                 br(), actionButton(width = '100%', inputId = ns('addnewmanifest'), icon = NULL, label = 'Add this manifest')
          )
        )
      } else { "File format is not correct. Make sure the file contains 'chromosome' and 'start' and 'end' columns." }
    })
    output$example_data_tableManifest <- renderTable({
      data.frame(
        chromosome = c("1", "1", "1"),
        start = c("11169311", "11174344", "11184507"),
        end = c("11169447", "11174556", "11184709")
      )
    })
    observeEvent(input$confirmManifest, {
      req(input$confirmManifest)
      req(input$newmanifestname)
      req(file_dataManifest())
      removeModal()
      dbExecute(conn = con, "CREATE TABLE IF NOT EXISTS manifests_list (user_id TEXT, manifests TEXT);")
      manifest_name <- paste(input$newmanifestname, Sys.getenv("SHINYPROXY_USERNAME"), sep = "_")
      dbWriteTable(conn = con, name = manifest_name, value = file_dataManifest(), row.names = FALSE, overwrite = TRUE)
      add_to_list <- data.frame(user_id = Sys.getenv("SHINYPROXY_USERNAME") , manifests = manifest_name)
      dbWriteTable(conn = con, name = "manifests_list", value = add_to_list, row.names = FALSE, append = TRUE)
      user_presets$manifests_list <- c(user_presets$manifests_list, input$newmanifestname)
      updateSelectInput(session = session, inputId = 'manifestlistsetup', choices = c("None", user_presets$manifests_list))
      updateSelectInput(session = session, inputId = 'selectManifest', choices = user_presets$manifests_list, selected = input$newmanifestname)
      sendSweetAlert(session = session,
                     title = "Manifest added to database !", 
                     text = HTML(paste0("<p style='color:#086A87;'><b>", input$newmanifestname,"</b> : </p>", 
                                        " has been added to the manifests list of the user : ",
                                        "<p style='color:#086A87;'><b>", Sys.getenv("SHINYPROXY_USERNAME"),"</b></p>",
                                        "You might have to restart the app to see it available in the data analysis window")), 
                     html = TRUE,
                     type = "success")
    })
    observeEvent(input$removeManifest,{
      req(input$removeManifest)
      req(input$selectManifest)
      print(paste0("DROP TABLE IF EXISTS ",input$selectManifest, "_" , Sys.getenv("SHINYPROXY_USERNAME"),"_manifest;"))
      DBI::dbSendQuery(conn = con,paste0("DROP TABLE IF EXISTS ",input$selectManifest, "_" , Sys.getenv("SHINYPROXY_USERNAME"),"_manifest;"))
      user_presets$manifests_list <- user_presets$manifests_list[user_presets$manifests_list != input$selectManifest]
      updateSelectInput(session = session, inputId = 'selectManifest', choices = c(user_presets$manifests_list ,"None"))
    })
    
    observeEvent(input$dropdownExampleManifest,{
      req(input$dropdownExampleManifest)
      showModal(modalDialog(size = 'l',
                            column(width = 12, 
                                   h3("Example of Correctly Formatted Data:"),br(),
                                   tableOutput(ns("example_data_tableManifest")),br(),
                                   h3("File should be a text file with column separator either a comma, a tabulation, a semicolon, or a space")),
                            easyClose = TRUE,
                            footer = tagList(modalButton("Cancel"))
      ))
    })
    
  #### END OF SERVER PART ####
  })
}
