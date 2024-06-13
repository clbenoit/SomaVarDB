#app/view/run_view.R

box::use(
  shiny[h3, moduleServer, tagList, conditionalPanel, tabsetPanel, tabPanel, 
        span, br, column, fluidRow, h4, uiOutput, renderUI, NS, tags, updateSelectInput,
        sliderInput, req, numericInput, selectInput, selectizeInput, observeEvent, 
        updateSelectizeInput, fluidPage, bindCache, reactive, observe, reactiveValues, bindEvent, isolate],
  dplyr[filter, `%>%`, select, case_when, mutate, arrange],
  DBI[dbGetQuery, dbSendQuery],
  shinyWidgets[progressSweetAlert, closeSweetAlert],
  stringr[str_split]
  
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(label = "select run",
                   inputId = ns("selectedrun"),
                   choices = NULL, width = '100%'),
    br(),
    #DT::dataTableOutput("qc_table_run"),
      fluidRow(
        column(width = 12,
               tags$iframe(id = 'b', 
                           src = "https://multiqc.info/examples/rna-seq/multiqc_report",
                           style='width:100%;height:1200px;'))
    )
  )
}

#' @export
server <- function(id, appData, genomicData, main_session) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    observe({
      req(genomicData$samples_db)
      updateSelectInput(session = session, 'selectedrun', 
                        choices = unique(genomicData$samples_db$run))
    })
 
 
  })
}
