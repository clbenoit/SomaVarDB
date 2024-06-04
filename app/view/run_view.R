#app/view/run_view.R

box::use(
  shiny[h3, moduleServer, tagList, conditionalPanel, tabsetPanel, tabPanel, 
        span, br, column, fluidRow, h4, uiOutput, renderUI, NS, tags,
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
 
  )
}

#' @export
server <- function(id, con, data, variables) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
 
 
  })
}
