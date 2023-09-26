#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard shinydashboardPlus shinyjs
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(skin = "blue",#"blue-light",
                  options = list(sidebarExpandOnHover = TRUE),
                  dashboardHeader(
                    titleWidth = '25%',
                    title = span(img(src = 'www/CHUlogo.png', width = 40, height = 39), get_golem_options("app_title")),
                    tags$li(class = "dropdown", actionButton(label = NULL, inputId = "godbinfo",icon = icon("database"),
                                                             style = "position: relative; margin: 10px 10px 10px 10px; display:center-align;"))
                    ),
                  dashboardSidebar(width = '25vw',
                                   br(),
                                   fluidPage(column(width = 12,
                                          uiOutput("patientsidebar"),
                                          uiOutput("variantsidebar"))),
                                   minified = FALSE,collapsed = FALSE),
                  dashboardBody(
                    #tags$head(tags$link(rel  = "stylesheet,", type = "text/css", href = 'custom.css')),
                    tabsetPanel(id = "tabsBody",
                                tabPanel("PatientView",
                                         br(),fluidRow(uiOutput("db_boxPatient")), # decaler logo run a dte
                                         fluidRow(shinydashboardPlus::box(
                                           title = "Genomic variations", closable = FALSE ,solidHeader = TRUE,
                                           width = 12, status = "primary", collapsible = TRUE,
                                         selectizeInput(
                                           inputId = 'selectedsample', label = "Select a sample to explore",
                                           choices = samples_db$sample, width = '100%',multiple = FALSE,
                                           selected = samples_db$sample[1],
                                           size = 1),
                                         tabsetPanel(id = "tabsBoxVariation",
                                                     tabPanel("NP",br(),
                                                     DT::dataTableOutput("current_sample_variants_table")),
                                                     tabPanel("CNV",
                                                              fluidPage(
                                                              fluidRow(DT::dataTableOutput("current_sample_variants_table_cnv")),
                                                              br(),br(),
                                                              fluidRow(selectInput("cnvgene",label = "gene copy number profile :" ,
                                                                                   choices =  NULL, width = '100%')),
                                                              fluidRow(column(width = 12,uiOutput("cnvui")))
                                                              ))))),
                                         fluidRow(box(title = "Genome Browser",
                                             width =  12,
                                             uiOutput("browserui"),
                                             solidHeader = TRUE,
                                             status = "primary",collapsed = TRUE,
                                             collapsible = TRUE, closable = FALSE))
                                         ),
                                tabPanel("VariantView",
                                         br(),
                                         selectizeInput(
                                           inputId = 'selectedvariant',label = "Select a variant to explore",
                                           choices = NULL ,selected = NULL,
                                           width = '100%',multiple = FALSE,size = 1),
                                        fluidRow(shinydashboardPlus::box(
                                           title = "Variant annotations", closable = TRUE,solidHeader = TRUE,
                                           width = 12, status = "primary", collapsible = TRUE, DT::dataTableOutput("current_var_table"))),#),br(),
                                         fluidRow(uiOutput("db_boxVariant"))),
                                tabPanel("RunView",br(),
                                         selectizeInput(label = "select run",inputId = "selectedrun", choices = unique(samples_db$run) ,width = '100%'),br(),
                                         DT::dataTableOutput("qc_table_run"))
                    )),
                    footer = dashboardFooter(
                      left = HTML('Support: <b>cbenoit3@chu-grenoble.fr</b>'),
                      right = HTML('<a href="https://biologie.chu-grenoble.fr/biologie-moleculaire" target="_blank"><b>Plateforme de biologie moléculaire du CHU Grenoble Alpes</b></a>')
                    )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www",app_sys("app/www"))
  useShinyjs()
  tags$head(favicon(),bundle_resources(path = app_sys("app/www"),app_title = "SomaVarDB"))
  tags$head(tags$style(HTML(".sep {width: 20px;height: 1px;float: left;}")))
  # Add here other external resources
  # for example, you can add shinyalert::useShinyalert()
}
