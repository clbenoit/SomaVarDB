#app/view/sidebar.R

box::use(
  reactable,
  shiny[h3, moduleServer, tagList, conditionalPanel, tabsetPanel, tabPanel, 
        span, br, column, fluidRow, h4, uiOutput, renderUI, NS, tags, sliderInput, req, numericInput, selectInput, reactiveVal,
        observeEvent, updateSliderInput, updateNumericInput],
  bsplus[bs_embed_tooltip, shiny_iconlink ],
  dplyr[`%>%`, filter]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("sidebarUI"))
  )
}

#' @export
server <- function(id, con, data, variables) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    #req(data$db_metadata)

    output$sidebarUI <- renderUI({
      tagList(
        #conditionalPanel(condition = 'input.tabsBody=="PatientView"',
          tabsetPanel(id = ns("tabsPatient"),
            tabPanel("Sample",
              br(),
              span(h4("Coverage",
                shiny_iconlink(name = "info-circle") %>%
                  bs_embed_tooltip("Some information about this filter"), style = "text-align: center;")),
              fluidRow(column(width = 12 ,
                column(width = 8 ,
                  tags$div(sliderInput(inputId = ns("coverage"),
                                       label = "Coverage", 
                            width = '100%', step = 10,
                            value = data$db_metadata$dp_min,
                            min = data$db_metadata$dp_min, max = data$db_metadata$dp_max), 
                    class = "reverseSlider")),
                      column(width = 4 , br(),
                        numericInput(inputId = ns("coveragenum"), label = NULL , 
                          width = '100%', step = 10,
                          value = data$db_metadata$af_min))
                )), 
                br(),         
                span(h4("Quality",
                  shiny_iconlink(name = "info-circle") %>%
                    bs_embed_tooltip("Some information about this filter"), style = "text-align: center;")),
                  fluidRow(column(width = 12 ,
                    column(width = 8 , 
                      tags$div(
                        sliderInput(inputId = ns("quality"), label = "Quality", width = '100%',
                          value = data$db_metadata$qual_min,
                          min = data$db_metadata$qual_min, max = data$db_metadata$qual_max),
                        class = "reverseSlider")
                     ),
                    column(width = 4 , br(),
                       numericInput(inputId = ns("qualitynum"), label = NULL , width = '100%', step = 1,
                        value = data$db_metadata$qual_min))
                    )),                                                        
                    span(h4("Allele Frequency",
                      shiny_iconlink(name = "info-circle") %>%
                        bs_embed_tooltip("Some information about this filter"), style = "text-align: center;")),
                    fluidRow(column(width = 12 , 
                              column(width = 8 ,
                                tags$div(
                                  sliderInput(inputId = ns("allelefrequency"),
                                    label = NULL, step = 0.01 ,
                                    width = '100%', value = data$db_metadata$af_min, 
                                    min = data$db_metadata$af_min, max = data$db_metadata$af_max),
                                  class = "reverseSlider")),
                               column(width = 4 , br(),
                                  numericInput(inputId = ns("allelefrequencynum"), label = NULL ,
                                    width = '100%',step = 0.01 ,
                                    value = data$db_metadata$af_min))
                              ))
                            ),
            tabPanel("Annotation", br(),
              span(h4("gnomAD Frequency",
                shiny_iconlink(name = "info-circle") %>%
                  bs_embed_tooltip("Some information about this filter"), style = "text-align: center;")),
              fluidRow(column(width = 12 ,
                          column(width = 8 ,
                            sliderInput(inputId = ns("gnomadfrequency"), step = 0.01,
                                label = NULL, width = '100%', 
                                value = 1, min = 0 , max = 1)),
                           column(width = 4 , br(),
                              numericInput(inputId = ns("gnomadfrequencynum"), label = NULL  ,
                                width = '100%',  step = 0.01,
                                value = 1, min = 0, max = 1))
                          )),
                          selectInput(inputId = ns("impact"), width = '100%', label = "Impact",
                            choices  = c("Low","Moderate","High"),selected = "Low"),
                           selectInput(inputId = ns("manifest"), width = '100%', label = "Manifest",
                             choices  = data$manifests_list ,selected = "None"),
                           selectInput(inputId = ns("trlist"), width = '100%', label = "Select a prefered transcripts list",
                             choices  = data$transcript_lists ,selected = "None")
              ),
              tabPanel("Phenotype", br(),
                "my phenotype selectors"
              ),
              tabPanel("Preset", br(),
                selectInput(inputId = ns("selectedpreset"), width = '100%', label = "Select a filters preset",
                   choices  = data$presets ,selected = "None")
              )
            )
          #),
          ,
          #conditionalPanel(condition = 'input.tabsBody=="RunView"',
            selectInput(inputId = ns("runviewfilter"), width = '100%', label = "MyRunViewParameter",
              choices  = c("Low","Moderate","High"),selected = "Low"))      
      #)
    })
    
    ## Link sliders and numeric inputs ##
    gnomad_value <- reactiveVal(1)
    observeEvent(gnomad_value(), {
      req(gnomad_value)
      updateSliderInput("gnomadfrequency", value = gnomad_value(), session = session)
      updateNumericInput("gnomadfrequencynum", value = gnomad_value(), session = session)
    })
    
    observeEvent(input$gnomadfrequencynum, {
      req(input$gnomadfrequencynum)
      print("update gnomadfrequencynum")
      gnomad_value(input$gnomadfrequencynum)
    })
    observeEvent(input$gnomadfrequency, {
      req(input$gnomadfrequency)
      print("update gnomadfrequency")
      gnomad_value(input$gnomadfrequency)
    })
    
    quality_value <- reactiveVal(data$db_metadata$qual_min)
    observeEvent(quality_value(), {
      req(quality_value)
      updateSliderInput("quality", value = quality_value(), session = session)
      updateNumericInput("qualitynum", value = quality_value(), session = session)
    })
    
    observeEvent(input$qualitynum, {
      req(input$qualitynum)
      print("update qualitynum")
      quality_value(input$qualitynum)
    })
    observeEvent(input$quality, {
      req(input$quality)
      print("update quality")
      quality_value(input$quality)
    })
    
    coverage_value <- reactiveVal(data$db_metadata$dp_min)
    observeEvent(coverage_value(), {
      req(coverage_value)
      updateSliderInput("coverage", value = coverage_value(), session = session)
      updateNumericInput("coveragenum", value = coverage_value(), session = session)
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
    
    allelefrequency_value <- reactiveVal(data$db_metadata$af_min)
    observeEvent(allelefrequency_value(), {
      updateSliderInput("allelefrequency", value = allelefrequency_value(), session = session)
      updateNumericInput("allelefrequencynum", value = allelefrequency_value(), session = session)
    })
    observeEvent(input$allelefrequencynum, {
      req(input$allelefrequencynum)
      print("update allelefrequency")
      allelefrequency_value(input$allelefrequencynum)
    })
    observeEvent(input$allelefrequency, {
      req(input$allelefrequency)
      print("update allelefrequencynum")
      allelefrequency_value(input$allelefrequency)
    })    
 
  })
}
