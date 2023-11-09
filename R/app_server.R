#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'     
#' @noRd
#' 
#' @import shiny shinydashboard shinydashboardPlus
#' @importFrom RSQLite SQLite
#' @importFrom DBI dbGetQuery dbExistsTable dbWriteTable dbDisconnect dbConnect
#' @importFrom DT datatable renderDataTable dataTableOutput
#' @importFrom shinyWidgets closeSweetAlert progressSweetAlert updateProgressBar
#' @importFrom JBrowseR serve_data renderJBrowseR JBrowseR
#' @importFrom dplyr %>% select filter mutate inner_join group_by row_number arrange
#' @importFrom stringr str_split str_extract
#' @importFrom data.table rbindlist
#' @importFrom plotly renderPlotly
#' @importFrom golem get_golem_options
#' @importFrom shinyBS bsTooltip
#' @importFrom shiny.router router_server change_page
app_server <- function(input, output, session) {

  #sidebarVisible <- reactiveVal(TRUE)
  # shinyjs::runjs(HTML(paste0('<script src="www/modifyHeaders.js"></script>')))
  # 
  # # Envoyez un message pour ajouter les en-têtes personnalisés
  # observe({
  #   shinyjs::runjs("window.postMessage('modifyHeaders', '*');")
  # })
  
  # Multipage set up   
  router_server() # mandatory of shiny.route package use
  observeEvent(input$goparams,{
    req(input$goparams)
    #shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    #shinyjs::removeClass(selector = "body", class = "sidebar-disable")
    change_page('parameters')
    #shinyjs::hide("sidebar")
  })
  # observeEvent(input$goroot,{
  #   req(input$goroot)
  #   change_page('/')
  # })
  mod_parameters_management_server("save_parameters_module", reactiveValuesInputs = input, conn = con)
  #mod_parameters_management_server("save_parameters_module", reactiveValuesInputs = input, conn = con)
  #callModule()

  tempdir <- tempdir()
  print(get_golem_options("config_file"))
  print(get_golem_options("config"))
  print(config::get("cache_directory", 
                    file = get_golem_options("config_file"), 
                    config = get_golem_options("config")) )
  if (config::get("cache_directory", 
                  file = get_golem_options("config_file"), 
                  config = get_golem_options("config")) ==  "default"){
    dir.create(file.path(tempdir,"cache"))
    print(paste0("using following cache directory : ", file.path(tempdir,"cache")))
    shinyOptions(cache = cachem::cache_disk(file.path(tempdir,"cache")))
  } else {
    print(paste0("using following cache directory : ", 
                 config::get("cache_directory", 
                             file = get_golem_options("config_file"), 
                             config = get_golem_options("config"))))
    shinyOptions(cache = cachem::cache_disk(config::get("cache_directory", 
                                                        file = get_golem_options("config_file"), 
                                                        config = get_golem_options("config"))))
  }
  # Set up default user 
  if(Sys.getenv("SHINYPROXY_USERNAME") == ""){Sys.setenv(SHINYPROXY_USERNAME = "Me")}
  # db raw infos
  observeEvent(input$godbinfo,{ 
    req(input$godbinfo)
    showModal(modalDialog(size = "l",
        fluidRow(
          shinydashboardPlus::box(
            title = "Overview of the database", closable = FALSE ,
            width = 12, status = "primary", solidHeader = TRUE, collapsible = FALSE,
            dropdownMenu = boxDropdown(
              boxDropdownItem("Supplementary info", id = "dropdownItem", icon = icon("info")),icon = icon("plus")),
            tabsetPanel(id = "tabsBox",
                        tabPanel("Tab1",
                                 fluidRow(
                                   column(width = 6,
                                          infoBox("Total number of variants ",
                                                  value = as.character(db_metadata$nb_variants),
                                                  icon = icon("dna"),color = "olive",width = "100%")
                                   ),
                                   column(width = 6,
                                          infoBox("Total number of samples : ",
                                                  value = as.character(db_metadata$nb_samples),
                                                  icon = icon("users"), color = "light-blue",width = "100%")
                                   ))),
                        tabPanel("Tab2","tab2")),
            conditionalPanel(condition = "input.dropdownItem == true","supp figures")
            )
      ),
      easyClose = TRUE,
      footer = tagList(modalButton("OK"))
    ))
  })

  # Different sidebars according to selected tab
  output$sidebars <- renderUI({
    tagList(
      conditionalPanel(condition = 'input.tabsBody=="PatientView"',
                       tabsetPanel(id = "tabsPatient",
                                   tabPanel("Sample",
                                            br(),
                                            span(h4("Coverage",
                                                 bsplus::shiny_iconlink(name = "info-circle") %>%
                                                 bsplus::bs_embed_tooltip("Some information about this filter"),style = "text-align: center;")),
                                            fluidRow(column(width = 12 ,
                                                     column(width = 8 ,
                                                        sliderInput(inputId = "coverage", label = "Coverage",width = '100%',step = 10,
                                                          value = db_metadata$dp_min,
                                                          min = db_metadata$dp_min, max = db_metadata$dp_max)),
                                                     column(width = 4 ,br(),
                                                            numericInput(inputId = "coveragenum", label = NULL ,width = '100%',step = 10,
                                                                         value = db_metadata$af_min)))),br(),         
                                            span(h4("Quality",
                                               bsplus::shiny_iconlink(name = "info-circle") %>%
                                                 bsplus::bs_embed_tooltip("Some information about this filter"),style = "text-align: center;")),
                                            fluidRow(column(width = 12 ,column(width = 8 ,
                                               sliderInput(inputId = "quality", label = "Quality",width = '100%',
                                                          value = db_metadata$qual_min, min = db_metadata$qual_min, max = db_metadata$qual_max)),
                                               column(width = 4 ,br(),
                                                      numericInput(inputId = "qualitynum", label = NULL ,width = '100%',step = 1,
                                                                   value = db_metadata$qual_min)))),
                                            span(htmltools::h4("Allele Frequency",
                                              bsplus::shiny_iconlink(name = "info-circle") %>%
                                              bsplus::bs_embed_tooltip("Some information about this filter"),style = "text-align: center;")),
                                            fluidRow(column(width = 12 ,column(width = 8 ,
                                                   sliderInput(inputId = "allelefrequency",step = 0.01,
                                                               label = NULL,
                                                               width = '100%', value = db_metadata$af_min, 
                                                               min = db_metadata$af_min, max = db_metadata$af_max)),
                                            column(width = 4 ,br(),
                                                    numericInput(inputId = "allelefrequencynum", label = NULL ,width = '100%',step = 0.01,
                                                        value = db_metadata$af_min))))
                                   ),
                                   tabPanel("Annotation",
                                            selectInput(inputId = "impact", width = '100%', label = "Impact", choices  = c("Low","Moderate","High"),selected = "Low")
                                   ),
                                   tabPanel("Phenotype",br(),
                                            "my phenotype selectors"
                                   )
                       )
      ),
      conditionalPanel(condition = 'input.tabsBody=="RunView"',
                       selectInput(inputId = "runviewfilter", width = '100%', label = "MyRunViewParameter", choices  = c("Low","Moderate","High"),selected = "Low"))
    )
  })
  
  ## Link sliders and numeric inputs ##
  quality_value <- reactiveVal(db_metadata$qual_min)
  observeEvent(quality_value(), {
    req(quality_value)
    updateSliderInput("quality", value = quality_value(), session = session)
    updateSliderInput("qualitynum", value = quality_value(), session = session)
  })
  
  observeEvent(input$qualitynum, {
    req(input$qualitynum)
    print("update qualitynum")
    coverage_value(input$qualitynum)
  })
  observeEvent(input$quality, {
    req(input$quality)
    print("update quality")
    quality_value(input$quality)
  })
  
  coverage_value <- reactiveVal(db_metadata$dp_min)
  observeEvent(coverage_value(), {
    req(coverage_value)
    updateSliderInput("coverage", value = coverage_value(), session = session)
    updateSliderInput("coveragenum", value = coverage_value(), session = session)
  })
  
  observeEvent(input$coveragenum, {
    req(input$coveragenum)
    print("update coverage")
    coverage_value(input$coveragenum)
  })
  observeEvent(input$coverage, {
    req(input$coverage)
    print("update coveragenum")
    coverage_value(input$coverage)
  })
  
  allelefrequency_value <- reactiveVal(db_metadata$af_min)
  observeEvent(allelefrequency_value(), {
    updateSliderInput("allelefrequency", value = allelefrequency_value(), session = session)
    updateSliderInput("allelefrequencynum", value = allelefrequency_value(), session = session)
  })
  observeEvent(input$allelefrequencynum, {
    req(input$allelefrequencynum)
    print("update allelefrequency")
    allelefrequency_value(input$allelefrequencynum)
  })
  observeEvent(input$allelefrequency, {
    req(input$allelefrequency)
    print("update allelefrequencynum")
    allelefrequency_value(input$allelefrequencynum)
  })

  ######## SAMPLE VIEW #####
  current_sample_variants_genos <- reactive({
      req(input$selectedsample) ; req(input$coverage); req(input$quality); req(input$allelefrequency)
      print("getting current_sample_variants_genos")
      return(dbGetQuery(con,
                paste0("SELECT * from variant_geno WHERE sample = '",input$selectedsample,"' AND  gt_raw NOT IN ('0/0','./0','0/.')")
                ) %>%
                filter((dp >= input$coverage) &
                (qa >= input$quality) &
                (af >= input$allelefrequency))
      )
  }) %>% bindCache({list(input$selectedsample,input$coverage,input$quality,input$allelefrequency,globalRvalues())})
  
  current_sample_variants_ids <- reactive({
    print("getting current_sample_variants_ids")
    req(current_sample_variants_genos())
    return(unique(current_sample_variants_genos()$variant_id))
  }) %>% bindCache({list(input$selectedsample,input$coverage,input$quality,input$allelefrequency,globalRvalues())}) %>% 
         bindEvent(current_sample_variants_genos())

  current_sample_variants_infos <- reactive({
      print("running current_sample_variants_infos")
      req(current_sample_variants_ids())
      current_sample_variants_infos <- dbGetQuery(con,
                                                 paste0("SELECT * from variant_info WHERE variant_id IN ('",
                                                       paste0(current_sample_variants_ids(),collapse="' , '"),
                                                       "');")
      ) %>% select(-c("af"))
      return(current_sample_variants_infos)
  }) %>% bindCache({paste(current_sample_variants_ids())})

  current_sample_variants_impact <- reactive({
    req(current_sample_variants_ids())
    req(current_sample_variants_genos())
    req(input$impact)
    print(paste("reload value : ",reload$value))
    print("running current_sample_variants_impact")
    current_sample_variants_impact <- dbGetQuery(con,
                                                 paste0("SELECT * from variant_impact WHERE variant_id IN ('",
                                                        paste0(current_sample_variants_ids(),collapse="' , '"),
                                                        "');")
    ) %>% filter(case_when(input$impact  == "Low" ~ impact %in% c("LOW","MODERATE","HIGH","MODIFIER"),
                           input$impact  == "Moderate" ~ impact %in% c("MODERATE","HIGH","MODIFIER"),
                           input$impact  == "High" ~ impact %in% c("HIGH","MODIFIER")))
    return(current_sample_variants_impact)
  }) %>% bindCache({list(input$selectedsample,input$impact,globalRvalues())}) %>%
         bindEvent(c(current_sample_variants_ids(),input$impact,reload$value))
  
  current_sample_variants_MD <- reactive({
    req(current_sample_variants_ids())
      print("running current_sample_variants_MD")
      current_sample_variants_MD <- dbGetQuery(con,
                                                 paste0("SELECT * from variant_MD WHERE variant_id IN ('",
                                                        paste0(current_sample_variants_ids(),collapse="' , '"),
                                                        "');"))
      return(current_sample_variants_MD)
    }) %>% bindCache({paste(current_sample_variants_ids())})
  
  current_sample_variants_frequencies <- reactive({
    req(current_sample_variants_ids())
    print("running current_sample_frequencies")
    current_sample_variants_frequencies <- dbGetQuery(con,
                                             paste0("SELECT * from frequencies WHERE variant_id IN ('",
                                                    paste0(current_sample_variants_ids(),collapse="' , '"),
                                                    "');"))
    return(current_sample_variants_frequencies)
    
  }) %>% bindCache({paste(current_sample_variants_ids())})

  current_sample_variants_table <- reactive({
                  req(current_sample_variants_impact())
                  req(current_sample_variants_infos())
                  req(current_sample_variants_genos())
                  req(current_sample_variants_MD())
                  req(current_sample_variants_frequencies())
                   
                  if(nrow(current_sample_variants_genos()) >=1 && nrow(current_sample_variants_impact()) >=1){
                    if(nrow(current_sample_variants_MD()) >=1){
                       
                      print("running current_sample_variants_table")
                      progressSweetAlert(session = session, id = "renderingvarianttable",title = "Rendering variant table",display_pct = TRUE, value = 75)
                      #`VKB2_freq(%)` <- colnames(current_sample_variants_frequencies())[2]
                      `VKB2_freq(%)` <- colnames(current_sample_variants_frequencies())[grepl("ALL_DB", colnames(current_sample_variants_frequencies()))]
                   
                      current_sample_variants_table <- dplyr::inner_join(current_sample_variants_impact(),isolate({current_sample_variants_infos()}),by = "variant_id") %>%
                      dplyr::inner_join(current_sample_variants_genos(),by = "variant_id") %>%
                      dplyr::inner_join(current_sample_variants_MD(),by = "variant_id") %>%
                      dplyr::inner_join(current_sample_variants_frequencies(),by = "variant_id") %>%
                      select(c("symbol","VKB",
                                "variant_id","hgvsp",
                                `VKB2_freq(%)`,
                                #"hgvsc", "canonical",
                                "af",
                                "gt_raw","chr",
                                "dbSNP",
                                "siftPred",
                                "siftScore" ,
                                "polyphen2HdivPred",
                                "polyphen2HdivScore",
                                "polyphen2HvarPred",
                                "polyphen2HvarScore",
                                "clinvarClinsig",
                                "clinvarClinsigConf",
                                "feature","consequence","impact","biotype","exon","intron", # normal
                                "cosmic",
                                "mdurl", "TumorSuppressor","Oncogene","gnomADv3",
                                "polyphen", "sift",
                                colnames(current_sample_variants_frequencies())))
                      collapsed <- data.frame()
                      ids <- unique(current_sample_variants_table$variant_id)
                      for (id in ids){
                        subset <- current_sample_variants_table %>% filter(variant_id ==  id)
                        row <- subset %>% filter(hgvsp != "")
                        if(nrow(row) > 0){
                          row$consequence <- paste(unique(unlist(str_split(subset$consequence,pattern  = "&"))),collapse = " ")
                          row$feature <- paste(subset$feature,collapse = " ")
                          row$biotype <- paste(unique(subset$biotype),collapse = " ")
                          row <- row[1,]
                        } else {
                          row <- subset[1,]
                          row$consequence <- paste(unique(unlist(str_split(subset$consequence,pattern  = "&"))),collapse = " ")
                          row$feature <- paste(subset$feature,collapse = " ")
                          row$biotype <- paste(unique(subset$biotype),collapse = " ")
                        }
                        collapsed <- rbind(collapsed,row)
                        #collapsed <- data.table::rbindlist(list(collapsed,row)) # faster check if everything is good in table
                        #collapsed <- bind_rows(list(collapsed,row)) # even faster check if everything is good in table
                      }
                      # print(`VKB2_freq(%)`)
                      # print(head(current_sample_variants_impact()))
                      # print(head(current_sample_variants_infos()))
                      # print(head(current_sample_variants_genos()))
                      # print(head(current_sample_variants_MD()))
                      # print(head(current_sample_variants_frequencies()))
                      #print(VKB2_freq(%))
                      #print(head(collapsed))
                      collapsed <- collapsed %>%
                      mutate(`VKB2_freq(%)` =  signif(!!as.name(`VKB2_freq(%)`), digits = 2) * 100) %>%
                      #arrange(`VKB2_freq(%)`, desc(hgvsp)) %>% ##### ARRANGE LIKE THIS IN GERMLINE DATA
                      arrange(desc(af), desc(cosmic)) %>% ##### ARRANGE LIKE THIS IN SOMATIC DATA
                      mutate(hgvsp = case_when(
                        hgvsp != "" ~ paste0('<button id="variant_view_button_',variant_id,"_",symbol,'" type="button" class="btn btn-default action-button" onclick="Shiny.setInputValue(&quot;goVariantView&quot;,  this.id, {priority: &quot;event&quot;})">',hgvsp,'</button>'),
                        TRUE ~ paste0('<button id="variant_view_button_',variant_id,"_",symbol,'" type="button" class="btn btn-default action-button" onclick="Shiny.setInputValue(&quot;goVariantView&quot;,  this.id, {priority: &quot;event&quot;})"> GoToVariantView </button>'))) %>% 
                      mutate(dbSNP = paste0(sprintf('<a href="https://www.ncbi.nlm.nih.gov/snp/?term=%s" target="_blank" class="btn btn-primary"',dbSNP),">",dbSNP,"</a>")) %>%
                      mutate(cosmic = paste0(sprintf('<a href="https://cancer.sanger.ac.uk/cosmic/search?q=%s" target="_blank" class="btn btn-primary"',cosmic),">",cosmic,"</a>")) %>%
                      mutate(VKB2 =  paste0('<button id="button_',variant_id,"_",symbol,'" type="button" class="btn btn-default action-button" onclick="Shiny.setInputValue(&quot;goannotateVKB&quot;,  this.id, {priority: &quot;event&quot;})">',VKB,'</button>')) %>%
                      mutate(symbol =  paste0('<a href="https://www.omim.org/search?index=entry&start=1&limit=10&sort=score+desc%2C+prefix_sort+desc&search=',symbol,'"','target="_blank"><b>',symbol,'</b></a>')) %>%
                      select(c("VKB","variant_id",# hidden
                               "mdurl","VKB2", # fixed
                                `VKB2_freq(%)`,
                                "af",
                                "hgvsp","symbol",
                                "chr","gt_raw", 
                                "cosmic", "dbSNP", "siftPred", "siftScore" , "polyphen2HdivPred",
                                "polyphen2HdivScore","polyphen2HvarPred",
                                "polyphen2HvarScore","clinvarClinsig","clinvarClinsigConf",
                                "feature","consequence","impact","biotype","exon","intron", # normal
                                "TumorSuppressor","Oncogene","gnomADv3"))# %>% #%>% arrange(desc(VKB2_freq(%))) #%>%
                                #mutate(VKB2_freq = paste0(VKB2_freq, "%"))
                    
                     closeSweetAlert(session = session)
                     return(collapsed)
                       } else { print("no mobidetails information for variants contains in this sample. Have you run addMDtodb function after importing the vcf in base ?") }
                    } else { print("novariantsmatching filtercriteria") }
  }) %>% bindCache({list(input$selectedsample, input$impact, input$coverage,input$quality,input$allelefrequency,globalRvalues())}) %>%
         bindEvent(c(current_sample_variants_impact(), 
                      current_sample_variants_infos(), 
                      current_sample_variants_genos(), 
                      current_sample_variants_MD()))

 observeEvent(c(current_sample_variants_table(),input$tabsBody),{
    output$current_sample_variants_table <- DT::renderDataTable({
      print("Rendering current sample variants table")
      req(current_sample_variants_table())
      if(nrow(current_sample_variants_genos()) >=1 && nrow(current_sample_variants_impact()) >=1){
        DT::datatable(current_sample_variants_table(),
                      extensions = c("FixedColumns","FixedHeader","Buttons"),
                      options = list(scrollX = TRUE,
                                     autoWidth = FALSE,
                                     scrollY = "500px",lengthMenu = c(50, 100, 200, 300),
                                     columnDefs = list(list(className = "dt-center", targets=c(0,20)),list(visible=FALSE, targets=c(0,1))),
                                     fixedColumns = list(leftColumns = 2),fixedHeader = TRUE,
                                     dom = 'l<"sep">Bfrtip', # place where buttons are placed
                                     buttons = c('colvis','copy','excel')),
                     rownames = FALSE,
                     escape = FALSE
        )  %>% DT::formatStyle(
          'VKB2',"VKB",
          backgroundColor = DT::styleEqual(c("PossibleArtifact","Benign","LikelyBenign","UncertainSignificance","LikelyPathogenic","Pathogenic"),
                                         c('gray','green','blue','black','orange','red')))
      } else {DT::datatable(data.frame("No results" = "0 variants passing the filters"), rownames = FALSE)}
    }) })
  
  #######################  VKB PART #################################
  variant_annoter_1_reactives <- reactiveValues(launchmodal = 0, my_variant_id = NULL)
  observeEvent(input$goannotateVKB, {
     variant_annoter_1_reactives$my_variant_id <- paste0(stringr::str_split(input$goannotateVKB, pattern = "_")[[1]][c(2,3)],collapse = "_")
     variant_annoter_1_reactives$launchmodal <- variant_annoter_1_reactives$launchmodal +  1
  })
  reload <- reactiveValues(value = 0)
  mod_variant_annoter_server("variant_annoter_1", modal = TRUE, conn = con, reactiveValues = variant_annoter_1_reactives, reload = reload )
  globalRvalues <- reactive({
    req(reload$value)
    print(reload$value)
    if(reload$value == 0){
      print("INITIAL RELOAD :: ")
      return(db_metadata$hash)
    } else {
      print("RELOAD AGAIN :: ")
      print(db_metadata$hash)
      apn_sql <- paste0("UPDATE db_metadata SET hash = '",paste0(db_metadata$hash," | UPDATED"),"';")
      dbSendQuery(con, apn_sql)
      db_metadata <<- DBI::dbReadTable(con,"db_metadata")
      return(db_metadata$hash)
    }
  })
  
  sample_table_reactive_values <- reactiveValues(reload_comments = 0,nrow_comments = 0, selected_tab = "Infos", qc = NULL,nrow_qc = 0)
  ## QC ##
  if(DBI::dbExistsTable(conn = con, name="QC")){sample_table_reactive_values$qc <- DBI::dbReadTable(conn = con,name = "QC",check.names = FALSE)}
  output$noqc <- renderText({"No qc information for this sample. Please ask your administrator to add it on base."})
  observeEvent(input$selectedsample,{
    req(input$selectedsample)
    req(sample_table_reactive_values$qc)
    qc_table <- sample_table_reactive_values$qc %>% filter(sample == input$selectedsample)
    output$qc_table <- DT::renderDataTable(DT::datatable(sample_table_reactive_values$qc %>% filter(sample == input$selectedsample),
                                                         rownames = FALSE, escape = FALSE,
                                                         options = list(scrollX = TRUE, dom = 't')))
    sample_table_reactive_values$nrow_qc <- nrow(qc_table)
  })
  
  ## comments ##
  output$nocommenttextsample <- renderText({"No comment for this sample yet"})
  observe(priority = 1,{
    req(input$selectedsample)
    req(sample_table_reactive_values$reload_comments)
    print("selected tab = comment")
    comments_table_sample <- if(DBI::dbExistsTable(con,"sample_comments")){DBI::dbReadTable(conn = con,name="sample_comments") %>% dplyr::filter(sample == input$selectedsample) %>%
        dplyr::select(-c("com_id"))} else {data.frame()}
    output$comments_sample <- DT::renderDataTable(DT::datatable(comments_table_sample,rownames = FALSE, escape = FALSE))
    sample_table_reactive_values$nrow_comments <- nrow(comments_table_sample) 
    sample_table_reactive_values$selected_tab <- "Comments"
  })
  observe({
    req(input$selectedsample)
    print("selected tab = info")
    sample_table_reactive_values$selected_tab <- "Infos"
  })

  observe({
    req(input$selectedsample)
    print("renderUI comment")
    sample_attributes <- dbGetQuery(con,paste0("SELECT * from samples WHERE sample = '",input$selectedsample,"'"))
    output$db_boxPatient <- renderUI({
      fluidPage(
        fluidRow(
          shinydashboardPlus::box(
            title = "Some information about the sample", closable = FALSE,
            width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
            dropdownMenu = boxDropdown(
              boxDropdownItem("Supplementary info", id = "dropdownItem", icon = icon("info")),
              icon = icon("plus")
            ),
            tabsetPanel(id = "tabsBoxPatient",selected = sample_table_reactive_values$selected_tab,
                        tabPanel("Infos",
                                 br(),
                                 fluidRow(
                                   column(width = 6,
                                          infoBox(title = "RUN",
                                                  br(),
                                                  value = HTML(paste0('<button id="button_',as.character(sample_attributes$run),'" type="button" class="btn btn-default action-button" onclick="Shiny.setInputValue(&quot;gorun&quot;,  this.id, {priority: &quot;event&quot;})">',as.character(sample_attributes$run),'</button>')),
                                                  icon = icon("running"),color = "olive",width = "100%")),
                                 )),
                        tabPanel("Comments",br(),
                                 if(sample_table_reactive_values$nrow_comments >= 1) {DT::dataTableOutput("comments_sample")} else {verbatimTextOutput("nocommenttextsample")},br(),
                                 textAreaInput(inputId = "annotatecomSample",label = "Write a commentary about the sample",placeholder = "my commentary",width  = "100%",
                                               value = "my commentary"),
                                 actionButton("okCommentSample", "Add comment on sample")),
                        tabPanel("QC",br(),
                                 if(sample_table_reactive_values$nrow_qc >= 1) {
                                   DT::dataTableOutput("qc_table")}
                                 else {verbatimTextOutput("noqc")}#,
                        )),
            conditionalPanel(condition = "input.dropdownItem == true", "supp figures")
          )
        )
      )
    })
  })
  
  observeEvent(input$gorun,{
    req(input$gorun)
    updateTabsetPanel(session, "tabsBody", "RunView")
    updateSelectizeInput(session, inputId = "selectedrun", selected = gsub("button_","",input$gorun))
  })
  ## Plusieurs appels successifs à closeSweetAlert sans creation d'une nouvelle alerte 
  ## va prévenir la prochaine confirmation de s'afficher a l'ecran
  observeEvent(input$okCommentSample,{
    print(input$okCommentSample)
    progressSweetAlert(session = session, id = "Just_to_restore_reactivity_[Tricky]",title = "",display_pct = TRUE, value = 9)
    closeSweetAlert(session = session)
    ask_confirmation(inputId = "annotationconfCommentSample",
                     type = "warning",
                     session = session,
                     title = "Are you sure ??", "Previous annotation state will be overriden",
                     btn_labels = c("No", "Yes"),
                     btn_colors = c("#FE642E", "#04B404"))
  })
  
  observeEvent(input$annotationconfCommentSample,{
    req(input$selectedsample)
    req(input$annotatecomSample)
    req(input$annotationconfCommentSample)
    if(input$annotationconfCommentSample == TRUE){
      new_comment <- data.frame(sample = input$selectedsample, comment = HTML(gsub("\n","<br/>",input$annotatecomSample)), user = Sys.getenv("SHINYPROXY_USERNAME") ,date = format(Sys.Date()," %d/%m/%Y"))
      DBI::dbWriteTable(conn = con, name="sample_comments", new_comment, append = TRUE)
      sample_table_reactive_values$reload_comments <- sample_table_reactive_values$reload_comments + 1
    }
  })
  
  ### SAMPLE VIEW CNV ###
  current_sample_variants_table_cnv <- reactive({
    req(input$selectedsample)
    print("getting current_sample_cnv_genos")
    progressSweetAlert(session = session, id = "progresscnvtable",title = "Querying cnv results",display_pct = TRUE, value = 9)
    current_sample_variants_table_cnv <- data.frame(dbGetQuery(con,paste0("SELECT * from cnv_geno WHERE sample_name = '",input$selectedsample,"'"))) %>%
      mutate(mean_ratio = signif(mean_ratio, digits = 2)) %>%
      select(c("variant_id","concat","sample_name","gene","exon_rank","CNV_status","mean_ratio")) #,"profiles"))
    closeSweetAlert(session = session)
    return(current_sample_variants_table_cnv)
    }) %>% bindCache(list(input$selectedsample,globalRvalues())) %>% bindEvent(input$selectedsample)
  
  observe({updateSelectInput(session = session, inputId = "cnvgene", choices  = unique(current_sample_variants_table_cnv()$gene), selected = NULL)})
  
  observe({
    req(current_sample_variants_table_cnv())
    if(nrow(current_sample_variants_table_cnv()) == 0) {
      current_sample_variants_table_cnv <- data.frame("No data available" = "No cnv information avaialble for this sample. Please ask your administrator to add it in base.")
      output$current_sample_variants_table_cnv <- DT::renderDataTable(DT::datatable(current_sample_variants_table_cnv(), rownames = FALSE))
    } else {
    output$current_sample_variants_table_cnv <- DT::renderDataTable(DT::datatable(
        current_sample_variants_table_cnv(),
        extensions = c("FixedColumns","FixedHeader"),
        rownames = FALSE, escape = FALSE,
        options = list(scrollX = TRUE, dom = 't', columnDefs = list(list(visible=FALSE, targets=c(0,1,2))))))
    }
  })

  cnvplot <- reactive({   
    req(input$cnvgene)
    req(input$selectedsample)
    req(current_sample_variants_table_cnv())
    if(input$tabsBoxVariation == "CNV"){
      if(nrow(current_sample_variants_table_cnv()) > 0) {  
      progressSweetAlert(session = session, id = "progresscnvtable",title = "Ploting cnv results",display_pct = TRUE, value = 9)
      run <- dbGetQuery(con, paste0("SELECT run from samples WHERE sample = '",input$selectedsample,"'"))
      background_samples <- dbGetQuery(con, paste0("SELECT sample from samples WHERE run = '",as.character(run$run),"'"))
      cnv_plot <- SomaVarDB::drawCNVplot_seqone(
        session = session,
        cnvfile_path = cnvfile_path,
        prefix = get_golem_options("prefix"), db_path = get_golem_options("db_path"), 
        selected_sample = input$selectedsample,
        background_samples = background_samples,
        selected_gene = input$cnvgene)
      closeSweetAlert(session = session)
      return(cnv_plot)
    }}
  }) %>% bindCache(list(input$cnvgene,globalRvalues(), input$selectedsample)) %>%
         bindEvent(c(input$cnvgene,input$selectedsample))
    
  output$cnvui <- renderUI({
    req(current_sample_variants_table_cnv())
    if(nrow(current_sample_variants_table_cnv()) > 0) {  
      p(input$cnvgene,style = "text-align: center" )
      renderPlotly(cnvplot())
      } else {
        print(HTML("<p style=\"color:red\">No CNV data available for this sample. Please ask your administrator to add it in base"))}  
  })
  
  #### JBROWSE ######
  # Start server container data
  print(config::get("use_browser", file = get_golem_options("config_file"), config = get_golem_options("config")))
  if(config::get("use_browser", file = get_golem_options("config_file"), config = get_golem_options("config"))) {
    if (!(config::get("browser_server_path", file = get_golem_options("config_file"), config = get_golem_options("config")) %in%  c("remote","","FALSE","None"))){
              browser_server_path = config::get("browser_server_path", file = get_golem_options("config_file"), config = get_golem_options("config"))
              if(file.exists(
                            file.path(browser_server_path,
                                      config::get("genome_ref", 
                                                  file = get_golem_options("config_file"), 
                                                  config = get_golem_options("config")))
                              ) == FALSE){
                print("no reference genome file found in your local JBrowseR server, is it correctly set up ?")
              } else {
              print(paste0("JBrowseR server path location", browser_server_path))
              data_server <- JBrowseR::serve_data(browser_server_path)
              }
    }

  genome_ref <- assembly(file.path(config::get("browser_client_url", file = get_golem_options("config_file"), config = get_golem_options("config")),
                                   config::get("genome_ref", file = get_golem_options("config_file"), config = get_golem_options("config"))),
                         bgzip = TRUE)
  
  JBrowseR_reactive_values <- reactiveValues(tracks = NULL)
  observeEvent(input$selectedsample, {
    req(input$selectedsample)
    if(length(list.files(browser_server_path, pattern = (paste0(input$selectedsample,"_min.bam$")),full.names = TRUE)) > 0){
      bam_track <- track_alignments(file.path(config::get("browser_client_url", file = get_golem_options("config_file"), config = get_golem_options("config")),paste0(input$selectedsample,"_min.bam")), assembly = genome_ref)
      print(bam_track)
      print("tracks :")
      print(tracks(bam_track))
      JBrowseR_reactive_values$tracks <- tracks(bam_track)
    } else { JBrowseR_reactive_values$tracks <- "notrack" }
  })
  
  # link the UI with the browser widget
  observeEvent(JBrowseR_reactive_values$tracks,{
    req(JBrowseR_reactive_values$tracks)
    if(JBrowseR_reactive_values$tracks != "notrack"){
      print("bam exists :")
      output$browserOutput <- renderJBrowseR(JBrowseR("View",assembly = genome_ref,tracks = JBrowseR_reactive_values$tracks))
    }
  })
  
  output$nobrowsertext <- renderText({"No bam file available for this sample. Please ask your administrator to add it in base"})
  observeEvent(JBrowseR_reactive_values$tracks,ignoreInit = FALSE,{
    req(JBrowseR_reactive_values$tracks)
    output$browserui <- renderUI({
      if(JBrowseR_reactive_values$tracks != "notrack"){
        tags$div(JBrowseROutput("browserOutput", height = 10000), style = "height:100%")
      } else {  print(HTML("<p style=\"color:red\">No bam file available for this sample. Please ask your administrator to add it in base")) }
    })
  })
  
  } # end of if browser
  
  ######## VARIANTS VIEW #####
  variant_annoter_2_reactives <- reactiveValues(my_variant_id = NULL, launchmodal = NULL)
  observeEvent(input$selectedvariant,{
    req(input$selectedvariant)
    variant_annoter_2_reactives$my_variant_id <- input$selectedvariant
  })
  mod_variant_annoter_server("variant_annoter_2", modal = FALSE, reactiveValues = variant_annoter_2_reactives, conn = con, reload = reload)
  variant_view_reactive_values <- reactiveValues(total_freq= NULL, samples_list = NULL)
  
  observe({
    req(variant_infos())
    updateSelectizeInput(session, inputId = "selectedvariant",
                        choices =  variant_infos(), server = TRUE)
  })
   
  ### MobiDetails
  observeEvent(input$goVariantView,{
    req(input$goVariantView)
    variant <- gsub("_$","",str_extract(gsub("variant_view_button_","",input$goVariantView),"^.*_"))
    updateTabsetPanel(session, "tabsBody", "VariantView")
    updateSelectizeInput(session, inputId = "selectedvariant", selected = variant,
                         choices = variant_infos(), server = TRUE)
  
  })
  
  variant_view_total_freq <- reactive({
    req(input$selectedvariant)
    return(dbGetQuery(con, paste0("SELECT * from frequencies WHERE variant_id = '",input$selectedvariant,"'")) %>%
      select(matches("^ALL_DB_freq_total")))
    }) %>% bindCache(list(input$selectedvariant,globalRvalues())) %>%
           bindEvent(c(input$selectedvariant))
  
  current_var_table <- reactive({
    print("rendering selected variant table")
    req(input$selectedvariant)
    current_impact <- dbGetQuery(con, paste0("SELECT * from variant_impact WHERE variant_id = '",input$selectedvariant,"'")) %>%
                      inner_join(dbGetQuery(con, paste0("SELECT * from variant_MD WHERE variant_id = '",input$selectedvariant,"'")),by = "variant_id") %>%
                      inner_join(dbGetQuery(con, paste0("SELECT * from variant_info WHERE variant_id = '",input$selectedvariant,"'")),by = "variant_id")
    collapsed <- data.frame()
    ids <- unique(current_impact$variant_id)
    for (id in ids){
      subset <- current_impact %>% filter(variant_id ==  id)
      row <- subset %>% filter(hgvsp != "")
      if(nrow(row) > 0){
        row$consequence <- paste(unique(unlist(str_split(subset$consequence,pattern  = "&"))),collapse = " ")
        row$feature <- paste(subset$feature,collapse = " ")
        row$biotype <- paste(unique(subset$biotype),collapse = " ")
        row <- row[1,]
      } else {
        row <- subset[1,]
        row$consequence <- paste(unique(unlist(str_split(subset$consequence,pattern  = "&"))),collapse = " ")
        row$feature <- paste(subset$feature,collapse = " ")
        row$biotype <- paste(unique(subset$biotype),collapse = " ")
      }
    }
    collapsed <- row %>%
      mutate(cosmic = paste0(sprintf('<a href="https://cancer.sanger.ac.uk/cosmic/search?q=%s" target="_blank" class="btn btn-primary"',cosmic),">",cosmic,"</a>")) %>%
      mutate(dbSNP =   paste0(sprintf('<a href="https://www.ncbi.nlm.nih.gov/snp/?term=%s" target="_blank" class="btn btn-primary"',dbSNP),">",dbSNP,"</a>")) %>%
      mutate(hgvsp = paste0('<button id="variant_view_button_',variant_id,"_",symbol,'" type="button" class="btn btn-default action-button" onclick="Shiny.setInputValue(&quot;goVariantView&quot;,  this.id, {priority: &quot;event&quot;})">',hgvsp,'</button>')) %>% 
      mutate(symbol =  paste0('<a href="https://www.omim.org/search?index=entry&start=1&limit=10&sort=score+desc%2C+prefix_sort+desc&search=',symbol,'"','target="_blank"><b>',symbol,'</b></a>')) %>%
      select(c("variant_id",#"VKB", # fixed
               "mdurl","VKB", # hidden
               "symbol","hgvsp", "feature","consequence","impact",
               "biotype","exon","intron", # normal
               "dbSNP","clinvar_clnsig",
               "cosmic",
              "TumorSuppressor","Oncogene",
              #"canonical",
              "gnomADv3",
              "polyphen", "sift")) 
     return(collapsed)
   }) %>% bindCache(list(input$selectedvariant,globalRvalues())) %>%
          bindEvent(c(input$selectedvariant, globalRvalues()))
  
   observe({
    output$current_var_table  <- DT::renderDataTable(DT::datatable(current_var_table(),
                                                                  extensions = c("FixedColumns","FixedHeader"),
                                                                  rownames = FALSE, escape = FALSE,
                                                                  options = list(scrollX = TRUE,
                                                                                 #columnDefs = list(list(visible=FALSE, targets=c(0,1))),
                                                                                 columnDefs = list(list(visible=FALSE, targets=c(0))),
                                                                                 fixedColumns = list(leftColumns = 2),fixedHeader = TRUE,
                                                                                 autoWidth = FALSE,
                                                                                 dom = 't')
                                                                  ) %>% DT::formatStyle(
                                                                    "VKB",
                                                                    color = DT::styleEqual(c("PossibleArtifact","Benign","LikelyBenign","UncertainSignificance","LikelyPathogenic","Pathogenic"),
                                                                                                     c('gray','green','blue','black','orange','red')))
                                                    )
   })

  observeEvent(input$gosample,{
    req(input$gosample)
    updateTabsetPanel(session, "tabsBody", "PatientView")
    updateSelectizeInput(session, inputId = "selectedsample",selected = gsub("button_","",input$gosample))
  })

  variant_view_samples_list <- reactive({
    req(input$selectedvariant)
    samples_list <- dbGetQuery(con, paste0("SELECT * from variant_geno WHERE variant_id = '",input$selectedvariant,"' AND  gt_raw NOT IN ('0/0','./0','0/.')")) %>%
      mutate(samples = paste0('<button id="button_',sample,'" type="button" class="btn btn-default action-button" onclick="Shiny.setInputValue(&quot;gosample&quot;,  this.id, {priority: &quot;event&quot;})">',sample,'</button>')) %>%
      select(c("samples","gt_raw"))
      return(samples_list)
    }) %>% bindCache(list(input$selectedvariant,globalRvalues())) %>%
           bindEvent(c(input$selectedvariant))
  
  output$samples_list_table <- DT::renderDataTable(
      DT::datatable(variant_view_samples_list(),
                    caption = htmltools::tags$caption("The variant is present in the following samples",style = "caption-side: top; text-align: center;color:black"),
                    extensions = c("FixedColumns","FixedHeader"),
                    options = list(scrollX = FALSE),
                    rownames = FALSE,escape = FALSE )) 
        
  observe({
    req(variant_annoter_2_reactives$my_variant_id)
    req(variant_view_samples_list())
    req(variant_view_total_freq())
    req(current_var_table())
    req(input$selectedvariant)
    print("rendering db box ui")
    output$db_boxVariant <- renderUI({
    fluidPage(
      fluidRow(
        shinydashboardPlus::box(
          title = "Variant info in VKB", closable = TRUE,
          width = 12, status = "primary",solidHeader = TRUE, collapsible = TRUE,
          dropdownMenu = boxDropdown(boxDropdownItem("Supplementary info", id = "dropdownItem", icon = icon("info")),icon = icon("plus")),
          tabsetPanel(id = "tabsBoxVariant",
                      tabPanel("Infos",
                               fluidRow(
                                 column(width = 6,
                                        infoBox("Variant frequence over the whole database",
                                                value = as.character(format(variant_view_total_freq(),scientific = TRUE, digits = 2)),
                                                icon = icon("dna"),color = "olive",width = "100%")),
                                 column(width = 6,
                                        infoBox("Number of samples with the variation in DB ",
                                                value = as.character(nrow(variant_view_samples_list())),
                                                icon = icon("users"), color = "light-blue",width = "100%")),
                                 column(width = 12,
                                        DT::dataTableOutput("samples_list_table")),
                      )),
                      tabPanel("Comments",
                               br(),
                               mod_variant_annoter_ui("variant_annoter_2", modal = FALSE,  conn = con, reactiveValues = variant_annoter_2_reactives)
                               )
                      ),
          conditionalPanel(condition = "input.dropdownItem == true", "supp figures")
          )
      )
    )
  })
  })
  
  ######## RUN VIEW #####
 
  ## QC ##
  observeEvent(c(input$selectedrun,sample_table_reactive_values$qc),{
    req(input$selectedrun)
    req(sample_table_reactive_values$qc)
    same_run_samples <- dbGetQuery(con,paste0("SELECT sample from samples WHERE run = '",input$selectedrun,"'"))
    same_run_samples$sample <- gsub("_.*","",same_run_samples$sample)
    #qc_table_run <- sample_table_reactive_values$qc[sample_table_reactive_values$qc$sample %in% same_run_samples$sample,]
    qc_table_run <- sample_table_reactive_values$qc[gsub("_.*","",sample_table_reactive_values$qc$sample) %in% same_run_samples$sample,]
    if(nrow(qc_table_run) == 0){
    qc_table_run <- data.frame("No data available"  = "No qc information for this run. Please ask your administrator to add it on base.")
    }
    output$qc_table_run <- DT::renderDataTable(DT::datatable(qc_table_run,
                                                             rownames = FALSE, escape = FALSE,
                                                             options = list(scrollX = TRUE, dom = 't')))
  })
}

