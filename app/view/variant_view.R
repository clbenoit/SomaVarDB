#app/view/variant_view.R

box::use(
  shiny[h3, moduleServer, tagList, conditionalPanel, tabsetPanel, tabPanel, 
        span, br, column, fluidRow, h4, uiOutput, renderUI, NS, tags, icon,
        sliderInput, req, numericInput, selectInput, selectizeInput, observeEvent, updateTabsetPanel, 
        updateSelectizeInput, fluidPage, bindCache, reactive, observe, reactiveValues, bindEvent, isolate],
  dplyr[filter, `%>%`, select, case_when, mutate, arrange, inner_join],
  DBI[dbGetQuery, dbSendQuery],
  shinyWidgets[progressSweetAlert, closeSweetAlert],
  stringr[str_split],
  DT[dataTableOutput, datatable, renderDataTable, formatStyle, styleEqual],
  shinydashboardPlus[box, boxDropdown, boxDropdownItem], 
  shinydashboard[infoBox], 
  htmltools[tags]
  
)

box::use(
  app/view/variant_annoter,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    selectizeInput(
      inputId = ns('selectedvariant'), label = "Select a variant to explore",
      choices = NULL , selected = NULL,
      width = '100%', multiple = FALSE, size = 1),
    fluidRow(box(
      title = "Variant annotations", closable = TRUE, solidHeader = TRUE,
      width = 12, status = "primary", collapsible = TRUE, dataTableOutput(ns("current_var_table")))),
    fluidRow(uiOutput(ns("db_boxVariant"))),
  )
}

#' @export
server <- function(id, con, appData, genomicData, main_session) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    observeEvent(appData$selectors$variant, {
      req(genomicData$variant_infos)
      req(appData$selectors$variant)
      updateSelectizeInput(session, inputId = "selectedvariant", selected = appData$selectors$variant,
                         choices = genomicData$variant_infos, server = TRUE)
    })
 
    ######## VARIANTS VIEW #####
    variant_annoter_2_reactives <- reactiveValues(my_variant_id = NULL, launchmodal = NULL)
    observeEvent(input$selectedvariant,{
      req(input$selectedvariant)
      variant_annoter_2_reactives$my_variant_id <- input$selectedvariant
    })
    variant_annoter$server("variant_annoter_variant_view", appData = appData, modal = FALSE)
    #mod_variant_annoter_server("variant_annoter_2", modal = FALSE, reactiveValues = variant_annoter_2_reactives, conn = con, reload = reload)  
    variant_view_reactive_values <- reactiveValues(total_freq = NULL, samples_list = NULL)
    
    observe({
      req(genomicData$variant_infos)
      updateSelectizeInput(session, inputId = "selectedvariant",
                           choices =  genomicData$variant_infos, server = TRUE)
    })
    
    variant_view_total_freq <- reactive({
      req(input$selectedvariant)
      return(dbGetQuery(appData$con, paste0("SELECT * from frequencies WHERE variant_id = '", input$selectedvariant, "'")) %>%
               select(matches("^ALL_DB_freq_total")))
    }) %>% bindCache(list(input$selectedvariant, appData$db_metadata$hash)) %>%
      bindEvent(c(input$selectedvariant))
    
    current_var_table <- reactive({
      print("rendering selected variant table")
      req(input$selectedvariant)
      current_impact <- dbGetQuery(appData$con, paste0("SELECT * from variant_impact WHERE variant_id = '",input$selectedvariant,"'")) %>%
        inner_join(dbGetQuery(appData$con, paste0("SELECT * from variant_MD WHERE variant_id = '",input$selectedvariant,"'")),by = "variant_id") %>%
        inner_join(dbGetQuery(appData$con, paste0("SELECT * from variant_info WHERE variant_id = '",input$selectedvariant,"'")),by = "variant_id")
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
        #mutate(cosmic = paste0(sprintf('<a href="https://cancer.sanger.ac.uk/cosmic/search?q=%s" target="_blank" class="btn btn-primary"',cosmic),">",cosmic,"</a>")) %>%
        mutate(dbSNP =   paste0(sprintf('<a href="https://www.ncbi.nlm.nih.gov/snp/?term=%s" target="_blank" class="btn btn-primary"', dbSNP), ">", dbSNP,"</a>")) %>%
        mutate(hgvsp = paste0('<button id="variant_view_button_', variant_id, "_", symbol,'" type="button" class="btn btn-default action-button" onclick="Shiny.setInputValue(&quot;goVariantView&quot;,  this.id, {priority: &quot;event&quot;})">',hgvsp,'</button>')) %>% 
        mutate(symbol =  paste0('<a href="https://www.omim.org/search?index=entry&start=1&limit=10&sort=score+desc%2C+prefix_sort+desc&search=', symbol, '"','target="_blank"><b>',symbol,'</b></a>')) %>%
        select(c("variant_id",#"VKB", # fixed
                 "mdurl","VKB", # hidden
                 "symbol","hgvsp", "feature","consequence","impact",
                 "biotype","exon","intron", # normal
                 "dbSNP","clinvar_clnsig",
                 #"cosmic",
                 "TumorSuppressor","Oncogene",
                 #"canonical",
                 "gnomADv3",
                 "polyphen", "sift")) 
      return(collapsed)
    }) %>% bindCache(list(input$selectedvariant,appData$db_metadata$hash)) %>%
      bindEvent(c(input$selectedvariant, appData$db_metadata$hash))
    
    observe({
      output$current_var_table  <- renderDataTable(datatable(current_var_table(),
                                                                     extensions = c("FixedColumns", "FixedHeader"),
                                                                     rownames = FALSE, escape = FALSE,
                                                                     options = list(scrollX = TRUE,
                                                                                    #columnDefs = list(list(visible=FALSE, targets=c(0,1))),
                                                                                    columnDefs = list(list(visible=FALSE, targets=c(0))),
                                                                                    fixedColumns = list(leftColumns = 2), fixedHeader = TRUE,
                                                                                    autoWidth = FALSE,
                                                                                    dom = 't')
      ) %>% formatStyle(
        "VKB",
        color = styleEqual(c("PossibleArtifact", "Benign", "LikelyBenign", "UncertainSignificance", "LikelyPathogenic", "Pathogenic"),
                               c('gray','green','blue','black','orange','red')))
      )
    })
    
    observeEvent(input$gosample, {
      req(input$gosample)
      updateTabsetPanel(session = main_session, "tabsBody", "PatientView") ### Ajax error, maybe wait for the patient table to be completely computed before to switch to patient tab
      appData$selectors$sample <- gsub("button_", "", input$gosample)
    })
    
    variant_view_samples_list <- reactive({
      req(input$selectedvariant)
      samples_list <- dbGetQuery(con = appData$con, paste0("SELECT * from variant_geno WHERE variant_id = '", input$selectedvariant,"' AND  gt_raw NOT IN ('0/0','./0','0/.')")) %>%
        mutate(samples = paste0('<button id="button_',sample,'" type="button" class="btn btn-default action-button" onclick="Shiny.setInputValue(&quot;', ns('gosample'), '&quot;,  this.id, {priority: &quot;event&quot;})">',sample,'</button>')) %>%
        select(c("samples","gt_raw"))
      return(samples_list)
    }) %>% bindCache(list(input$selectedvariant, appData$db_metadata$hash)) %>%
      bindEvent(c(input$selectedvariant))
    
    output$samples_list_table <- renderDataTable(
      datatable(variant_view_samples_list(),
                    caption = tags$caption("The variant is present in the following samples", 
                                           style = "caption-side: top; text-align: center;color:black"),
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
                                            dataTableOutput(ns("samples_list_table"))),
                                   )),
                          tabPanel("Comments",
                                   br(),
                                   variant_annoter$ui(ns("variant_annoter_variant_view"))
                          )
              ),
              conditionalPanel(condition = "input.dropdownItem == true", "supp figures")
            )
          )
        )
      })
    })    
 
  })
}
