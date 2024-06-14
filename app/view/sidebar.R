#app/view/sidebar.R

box::use(
  reactable,
  shiny[h3, moduleServer, tagList, conditionalPanel, tabsetPanel, tabPanel, 
        span, br, column, fluidRow, h4, uiOutput, renderUI, NS, tags, sliderInput, req, numericInput, selectInput, reactiveVal,
        observeEvent, updateSliderInput, updateNumericInput, outputOptions, reactive, 
        renderText, textOutput, updateSelectInput],
  bsplus[bs_embed_tooltip, shiny_iconlink ],
  dplyr[`%>%`, filter]
)

box::use(
  app/view/react[sliderNumeric],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("sidebarUI"))#,
  )
}

#' @export
server <- function(id, con, appData, main_session) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns

    output$tabValue <- reactive({
       req(appData$selectors$tab)
       return(appData$selectors$tab)
    })
    outputOptions(output, "tabValue", suspendWhenHidden = FALSE)
    
    output$sidebarUI <- renderUI({
      tagList(
        conditionalPanel(condition = sprintf("output['%s'] == 'PatientView'", ns("tabValue")),
          tabsetPanel(id = ns("tabsPatient"),
            tabPanel("Sample",
              br(),
              span(h4("Coverage",
                shiny_iconlink(name = "info-circle") %>%
                  bs_embed_tooltip("Some information about this filter"), style = "text-align: center;")),br(),
                sliderNumeric(inputId = ns("coverage"),
                  initialMin = appData$db_metadata$dp_min,
                  initialValue = appData$db_metadata$dp_min,
                  initialMax = appData$db_metadata$dp_max,
                  initialStep = 1,
                  fillDirection = 'right'),
                span(h4("Quality",
                  shiny_iconlink(name = "info-circle") %>%
                    bs_embed_tooltip("Some information about this filter"), style = "text-align: center;")),br(),
                  sliderNumeric(inputId = ns("quality"),
                    initialMin = appData$db_metadata$qual_min,
                    initialMax = appData$db_metadata$qual_max,
                    initialValue = appData$db_metadata$qual_min,
                    initialStep = 1,
                    fillDirection = 'right'),
                  span(h4("Allele Frequency",
                      shiny_iconlink(name = "info-circle") %>%
                        bs_embed_tooltip("Some information about this filter"), style = "text-align: center;")),br(),
                      sliderNumeric(inputId = ns("allelefrequency"),
                        initialMin = appData$db_metadata$af_min,
                        initialMax = appData$db_metadata$af_max,
                        initialValue = appData$db_metadata$af_max,
                        initialStep = 0.01,
                        fillDirection = 'left'),
            ),
            tabPanel("Annotation", br(),
              span(h4("gnomAD Frequency",
                shiny_iconlink(name = "info-circle") %>%
                  bs_embed_tooltip("Some information about this filter"), style = "text-align: center;")),br(),
              sliderNumeric(inputId = ns("gnomadfrequency"),
                            initialMin = 0,
                            initialMax = 1,
                            initialStep = 0.001,
                            initialValue = 1,
                            fillDirection = 'left'),
              selectInput(inputId = ns("impact"), width = '100%', label = "Impact",
                choices  = c("Low","Moderate","High"), selected = "Low"),
              selectInput(inputId = ns("manifest"), width = '100%', label = "Manifest",
                choices  = appData$user_parameters$manifests_list , selected = "None"),
              selectInput(inputId = ns("trlist"), width = '100%', label = "Select a prefered transcripts list",
                choices  = c(appData$user_parameters$transcript_lists, "None") , selected = "None")
              ),
              tabPanel("Phenotype", br(),
                "my phenotype selectors"
              ),
              tabPanel("Preset", br(),
                selectInput(inputId = ns("selectedpreset"), width = '100%', label = "Select a filters preset",
                   choices  = c(appData$user_parameters$presets$name, "None") ,selected = "None")
              )
            )
          ),
          conditionalPanel(condition = sprintf("output['%s'] == 'RunView'", ns("tabValue")),
            selectInput(inputId = ns("runviewfilter"), width = '100%', label = "MyRunViewParameter",
              choices  = c("Low","Moderate","High"),selected = "Low"))      
      )
    })
    
    observeEvent(input$gnomadfrequency, {
      req(input$gnomadfrequency)
      appData$filters$gnomadfrequency_value <- input$gnomadfrequency
    })
    
    observeEvent(input$quality, {
      req(input$quality)
      appData$filters$quality_value <- input$quality
    })
    
    observeEvent(input$coverage, {
      req(input$coverage)
      appData$filters$coverage_value <- input$coverage
    })
    
    observeEvent(input$allelefrequency, {
      req(input$allelefrequency)
      appData$filters$allelefrequency_value <- input$allelefrequency
    })
    
    observeEvent(input$impact, {
      req(input$impact)
      appData$filters$impact <- input$impact
    })
    
    observeEvent(input$manifest, {
      req(input$manifest)
      appData$filters$manifest <- input$manifest
    })
 
    observeEvent(input$trlist, {
      req(input$trlist)
      appData$filters$trlist <- input$trlist
    })
    
    observeEvent(input$selectedpreset ,{
      req(input$selectedpreset)
      if(input$selectedpreset != "None"){
        print("loading preset")
        loaded_preset <- appData$user_parameters$presets %>% filter(name == input$selectedpreset)
        updateSelectInput(session = session, inputId = "coverage", selected = loaded_preset$coveragenum)
        updateSelectInput(session = session, inputId = "quality", selected = loaded_preset$qualitynum)
        updateSelectInput(session = session, inputId = "allelefrequency", selected = loaded_preset$allelefrequencynum)
        updateSelectInput(session = session, inputId = "gnomadfrequency", selected = loaded_preset$gnomadfrequencynum)
        updateSelectInput(session = session, inputId = "impact", selected = loaded_preset$impact)
        updateSelectInput(session = session, inputId = "manifest", selected = loaded_preset$manifest)
        updateSelectInput(session = session, inputId = "trlist", selected = loaded_preset$trlist)
      }
    })
    
  })
}
