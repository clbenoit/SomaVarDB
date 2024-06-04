#app/view/patient_view.R

box::use(
  shiny[h3, moduleServer, tagList, conditionalPanel, tabsetPanel, tabPanel, 
        span, br, column, fluidRow, h4, uiOutput, renderUI, NS, tags, updateTabsetPanel, 
        sliderInput, req, numericInput, selectInput, selectizeInput, observeEvent, 
        updateSelectizeInput, fluidPage, bindCache, reactive, observe, reactiveValues, bindEvent, isolate],
  dplyr[filter, `%>%`, select, case_when, mutate, arrange],
  DBI[dbGetQuery, dbSendQuery],
  shinyWidgets[progressSweetAlert, closeSweetAlert],
  stringr[str_split, str_extract], 
  shinydashboardPlus[box],
  DT[dataTableOutput, datatable]
  
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(), fluidRow(uiOutput(ns("db_boxPatient"))),
    fluidRow(box(
      title = "Genomic variations", closable = FALSE ,solidHeader = TRUE,
      width = 12, status = "primary", collapsible = TRUE,
      selectizeInput(
        inputId = ns('selectedsample'), label = "Select a sample to explore",
        width = '100%', multiple = FALSE,
        choices = NULL,
        selected = NULL,
        size = 1),
      tabsetPanel(id = "tabsBoxVariation",
                  tabPanel("NP", br(),
                           dataTableOutput(ns("current_sample_variants_table"))),
                  tabPanel("CNV",
                           fluidPage(
                             fluidRow(dataTableOutput("current_sample_variants_table_cnv")),
                             br(), br(),
                             # fluidRow(selectInput(ns("cnvgene"),label = "gene copy number profile :" ,
                             #                      choices =  NULL, width = '100%')),
                             # fluidRow(column(width = 12, uiOutput(ns("cnvui"))))
                           )))))#,
    # fluidRow(box(title = "Genome Browser",
    #              width =  12,
    #              uiOutput("browserui"),
    #              solidHeader = TRUE,
    #              status = "primary",collapsed = TRUE,
    #              collapsible = TRUE, closable = FALSE))
  )
}

#' @export
server <- function(id, con, appData, genomicData, main_session) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    req(appData$db_metadata)
    
    updateSelectizeInput(session = session, inputId = "selectedsample", choices = genomicData$samples_db$sample)

    current_sample_variants_genos <- reactive({
      req(input$selectedsample) ; req(appData$filters$coverage_value); req(appData$filters$quality_value); req(appData$filters$allelefrequency_value)
      print("getting current_sample_variants_genos")
      return(dbGetQuery(appData$con,
                        paste0("SELECT * from variant_geno WHERE sample = '", input$selectedsample, 
                               "' AND  gt_raw NOT IN ('0/0','./0','0/.')")
      ) %>%
        filter((dp >= appData$filters$coverage_value) &
                (qa >= appData$filters$quality_value) &
                (af >= appData$filters$allelefrequency_value))
      )
    }) %>% bindCache({list(input$selectedsample,
                           appData$filters$allelefrequency_value,
                           appData$filters$coverage_value,
                           appData$filters$quality_value,
                           appData$db_metadata$hash
                           )
      })
    
    current_sample_variants_ids <- reactive({
      print("getting current_sample_variants_ids")
      req(current_sample_variants_genos())
      return(unique(current_sample_variants_genos()$variant_id))
    }) %>% bindCache({list(input$selectedsample,
                           appData$filters$allelefrequency_value,
                           appData$filters$coverage_value,
                           appData$filters$quality_value,
                           appData$db_metadata$hash)
                      }) %>% 
                      bindEvent(current_sample_variants_genos())

    current_sample_variants_infos <- reactive({
      print("running current_sample_variants_infos")
      req(current_sample_variants_ids())
      current_sample_variants_infos <- dbGetQuery(appData$con,
                                                  paste0("SELECT * from variant_info WHERE variant_id IN ('",
                                                         paste0(current_sample_variants_ids(),collapse="' , '"),
                                                         "');")
      ) %>% select(-c("af"))
      return(current_sample_variants_infos)
    }) %>% bindCache({paste(current_sample_variants_ids())})      

    current_sample_variants_impact <- reactive({
      req(current_sample_variants_ids())
      req(current_sample_variants_genos())
      req(appData$filters$impact)
      print(paste("reload value : ",appData$annoter_reactives$reload))
      print("running current_sample_variants_impact")
      current_sample_variants_impact <- dbGetQuery(appData$con,
                                                   paste0("SELECT * from variant_impact WHERE variant_id IN ('",
                                                          paste0(current_sample_variants_ids(),collapse="' , '"),
                                                          "');")
      ) %>% filter(case_when(appData$filters$impact  == "Low" ~ impact %in% c("LOW","MODERATE","HIGH","MODIFIER"),
                             appData$filters$impact  == "Moderate" ~ impact %in% c("MODERATE","HIGH","MODIFIER"),
                             appData$filters$impact  == "High" ~ impact %in% c("HIGH","MODIFIER")))
      return(current_sample_variants_impact)
    }) %>% bindCache({list(input$selectedsample, appData$filters$impact, appData$db_metadata$hash)}) %>%
      bindEvent(c(current_sample_variants_ids(), appData$filters$impact, appData$annoter_reactives$reload))    
    
    current_sample_variants_MD <- reactive({
      req(current_sample_variants_ids())
      req(appData$filters$gnomadfrequency_value)
      print("running current_sample_variants_MD")
      current_sample_variants_MD <- dbGetQuery(appData$con,
                                               paste0("SELECT * from variant_MD WHERE variant_id IN ('",
                                                      paste0(current_sample_variants_ids(),collapse="' , '"),
                                                      "');"))
      
    current_sample_variants_MD_filtered <- current_sample_variants_MD %>% 
        filter(!(gnomADv3 %in% c("No match in gnomADv3","Error on MobiDetails"))) %>%
        mutate(gnomADv3 = as.numeric(gnomADv3)) %>%
        filter(gnomADv3 <= appData$filters$gnomadfrequency_value)
      current_sample_variants_MD_nomatch <- current_sample_variants_MD %>% filter(gnomADv3 %in% c("No match in gnomADv3","Error on MobiDetails"))
      current_sample_variants_MD <- rbind(current_sample_variants_MD_nomatch, current_sample_variants_MD_filtered)
      return(current_sample_variants_MD)
    }) %>% bindCache({paste(current_sample_variants_ids(), appData$filters$gnomadfrequency_value)}) %>% 
      bindEvent(c(current_sample_variants_ids(), appData$filters$gnomadfrequency_value))

    current_sample_variants_frequencies <- reactive({
      req(current_sample_variants_ids())
      print("running current_sample_frequencies")
      current_sample_variants_frequencies <- dbGetQuery(appData$con,
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
                     "feature", "consequence", "impact", "biotype", "exon", "intron",
                     #"cosmic",
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
          collapsed <- collapsed %>%
            mutate(`VKB2_freq(%)` =  signif(!!as.name(`VKB2_freq(%)`), digits = 2) * 100) %>%
            arrange(`VKB2_freq(%)`, desc(hgvsp)) %>% ##### ARRANGE LIKE THIS IN GERMLINE DATA
            #arrange(desc(af), desc(cosmic)) %>% ##### ARRANGE LIKE THIS IN SOMATIC DATA
            mutate(hgvsp = case_when(
              hgvsp != "" ~ paste0('<button id="variant_view_button_', variant_id, "_", symbol, '" type="button" class="btn btn-default action-button" onclick="Shiny.setInputValue(&quot;', ns('goVariantView'), '&quot;,  this.id, {priority: &quot;event&quot;})">',hgvsp,'</button>'),
              TRUE ~ paste0('<button id="variant_view_button_', variant_id, "_", symbol, '" type="button" class="btn btn-default action-button" onclick="Shiny.setInputValue(&quot;' , ns('goVariantView'),'&quot;,  this.id, {priority: &quot;event&quot;})"> GoToVariantView </button>'))) %>% 
            mutate(dbSNP = paste0(sprintf('<a href="https://www.ncbi.nlm.nih.gov/snp/?term=%s" target="_blank" class="btn btn-primary"', dbSNP),">", dbSNP, "</a>")) %>%
            #mutate(cosmic = paste0(sprintf('<a href="https://cancer.sanger.ac.uk/cosmic/search?q=%s" target="_blank" class="btn btn-primary"',cosmic),">",cosmic,"</a>")) %>%
            mutate(VKB2 =  paste0('<button id="button_', variant_id, "_", symbol, '" type="button" class="btn btn-default action-button" onclick="Shiny.setInputValue(&quot;', ns("goannotateVKB"), '&quot;,  this.id, {priority: &quot;event&quot;})">',VKB,'</button>')) %>%
            mutate(symbol =  paste0('<a href="https://www.omim.org/search?index=entry&start=1&limit=10&sort=score+desc%2C+prefix_sort+desc&search=', symbol, '"', 'target="_blank"><b>', symbol, '</b></a>')) %>%
            select(c("VKB", "variant_id",# hidden
                     "mdurl","VKB2", # fixed
                     `VKB2_freq(%)`,
                     "af",
                     "hgvsp","symbol",
                     "chr","gt_raw", 
                     #"cosmic", 
                     "dbSNP", "siftPred", "siftScore" , "polyphen2HdivPred",
                     "polyphen2HdivScore","polyphen2HvarPred",
                     "polyphen2HvarScore","clinvarClinsig","clinvarClinsigConf",
                     "feature","consequence","impact","biotype","exon","intron", # normal
                     "TumorSuppressor","Oncogene","gnomADv3"))# %>% #%>% arrange(desc(VKB2_freq(%))) #%>%
          #mutate(VKB2_freq = paste0(VKB2_freq, "%"))
          
          closeSweetAlert(session = session)
          return(collapsed)
        }  else { print("no mobidetails information for variants contains in this sample. Have you run addMDtodb function after importing the vcf in base ?") }
      } else { print("novariantsmatching filtercriteria") }
    }) %>% bindCache({list(input$selectedsample,
                           appData$filters$gnomadfrequency_value,
                           appData$filters$impact, 
                           appData$filters$allelefrequency_value,
                           appData$filters$coverage_value,
                           appData$filters$quality_value,
                           appData$db_metadata$hash)}) %>%
      bindEvent(c(current_sample_variants_impact(), 
                  current_sample_variants_infos(), 
                  current_sample_variants_genos(), 
                  current_sample_variants_MD()))    
    
    #observeEvent(c(current_sample_variants_table(), input$tabsBody),{
    observeEvent(current_sample_variants_table(), {
        
      output$current_sample_variants_table <- DT::renderDataTable({
        print("Rendering current sample variants table")
        req(current_sample_variants_table())
        if(nrow(current_sample_variants_genos()) >=1 && nrow(current_sample_variants_impact()) >=1){
          datatable(current_sample_variants_table(),
                        extensions = c("FixedColumns", "FixedHeader", "Buttons"),
                        options = list(scrollX = TRUE,
                                       autoWidth = FALSE,
                                       scrollY = "500px",lengthMenu = c(50, 100, 200, 300),
                                       columnDefs = list(list(className = "dt-center", targets=c(0,20)), list(visible = FALSE, targets=c(0,1))),
                                       fixedColumns = list(leftColumns = 2), fixedHeader = TRUE,
                                       dom = 'l<"sep">Bfrtip',
                                       buttons = c('colvis','copy','excel')),
                        rownames = FALSE,
                        escape = FALSE
          )  %>% DT::formatStyle(
            'VKB2', "VKB",
            backgroundColor = DT::styleEqual(c("PossibleArtifact", "Benign", "LikelyBenign", "UncertainSignificance", "LikelyPathogenic", "Pathogenic"),
                                             c('gray','green','blue','black','orange','red')))
        } else {DT::datatable(data.frame("No results" = "0 variants passing the filters"), rownames = FALSE)}
      }) })
    
    observeEvent(input$goannotateVKB, {
      print("goannotateVKB")
      appData$annoter_reactives$my_variant_id <- paste0(str_split(input$goannotateVKB, pattern = "_")[[1]][c(2,3)], collapse = "_")
      appData$annoter_reactives$launchmodal <- appData$annoter_reactives$launchmodal +  1
    })
    
    observeEvent(input$goVariantView, {      
      req(input$goVariantView)
      variant <- gsub("_$", "", str_extract(gsub("variant_view_button_", "", input$goVariantView), "^.*_"))
      updateTabsetPanel(session = main_session, inputId = "tabsBody",  selected = "VariantView")
      appData$selectors$variant <- variant
    })
 
  })
}
