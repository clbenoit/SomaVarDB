#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard shinydashboardPlus shinyjs
#' @importFrom shiny.router router_ui route
#' @noRd
#' 
app_ui <- function(request) {
  
parameters <- div(
  tagList(
    mod_parameters_management_ui("save_parameters_module")
  )
)

tempdir <- tempdir()
print("config multiqc")
print(config::get("multiqc", 
                  file = get_golem_options("config_file"), 
                  config = get_golem_options("config")))
if (config::get("multiqc", 
                file = get_golem_options("config_file"), 
                config = get_golem_options("config")) ==  "default"){
                dir.create(file.path(tempdir,"multiqc"))
                multiqc_path <- file.path(tempdir,"multiqc")
} else {
  multiqc_path <- config::get("multiqc", 
                             file = get_golem_options("config_file"), 
                             config = get_golem_options("config"))
}
print(multiqc_path)
#addResourcePath("multiqc_path",multiqc_path)

# addCustomHeaders <- function() {
#   res <- shiny::addResourcePath("multiqc_path", multiqc_path)
#   shiny::addResourcePath(
#     "multiqc_path",
#     res$prefix,
#     headers = list("Access-Control-Allow-Origin" = "*")
#   )
# }
#shiny::onLoad(addCustomHeaders)

home_page <- div(
    tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(skin = "blue",#"blue-light",
                  options = list(sidebarExpandOnHover = TRUE),
                  dashboardHeader(
                    titleWidth = '25%',
                    title = span(img(src = 'www/CHUlogo.png', width = 40, height = 39), get_golem_options("app_title")),
                    tags$li(class = "dropdown", 
                            actionButton(label = NULL, inputId = "goparams",icon = icon("gear"),
                                         style = "position: relative; margin: 10px 10px 10px 10px; display:center-align;"),
                            actionButton(label = NULL, inputId = "godbinfo",icon = icon("database"),
                                                             style = "position: relative; margin: 10px 10px 10px 10px; display:center-align;"))
                    ),
                  dashboardSidebar(width = '25vw', id = "sidebars",
                                   br(),
                                   fluidPage(column(width = 12,
                                          uiOutput("sidebars"))),
                                          #uiOutput("variantsidebar"))),
                                   minified = FALSE,collapsed = FALSE),
                  dashboardBody(
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
                                         #DT::dataTableOutput("qc_table_run"),
                                         fluidPage(
                                           fluidRow(
                                             column(width = 12,
                                                           tags$iframe(id = 'b', 
                                                                       src = "https://multiqc.info/examples/rna-seq/multiqc_report",
                                                                       style='width:100%;height:1200px;'))),
                                           # fluidRow(
                                           #   column(width = 12,
                                           #          tags$iframe(id = 'multiqc',
                                           #                      src = "multiqc_path/multiqc_report.html",
                                           #                      loading="eager",
                                           #                      style='width:100%;height:1200px;')))
                                           )
                                        #  tags$script(HTML("
                                        #   document.getElementById(\"multiqc\").onload = function() {
                                        #     var iframeDocument = document.getElementById('multiqc').contentDocument;
                                        #     var scriptElement = iframeDocument.createElement(\"script\");
                                        #     scriptElement.innerHTML = \"console.log('Script execute dans l\'iframe');\";
                                        #     iframeDocument.body.appendChild(scriptElement);
                                        #   };
                                        # "))
                                        # tags$script(
                                        # HTML("
                                        #   document.getElementById(\"multiqc\").onload = function() {
                                        #     var iframeDocument = document.getElementById(\"multiqc\").contentDocument;
                                        #     var iframeScripts = iframeDocument.getElementsByTagName(\"script\");
                                        #     for (var i = 0; i < iframeScripts.length; i++) {
                                        #       eval(iframeScripts[i].text);
                                        #     }
                                        #   };
                                        # "))
                                ))
                  ),
                  footer = tags$footer(class = "main-footer", 
                                       HTML("<div class=\"pull-right hidden-xs\">
                                             <a href=\"https://clbenoit.github.io/portfolio/projects/somavardb\" target=\"_blank\"><b>About the app</b></a>
                                             </div>
                                             Support: <b>benoitclement.data@gmail.com</b>"
                                       )
                                     )
          )
    )
) # end of home_page
 

fluidPage(
  router_ui(
    route("/", home_page),
    route("parameters", parameters)
  ))
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
  add_resource_path("www",app_sys("app/www"), warn_empty = TRUE)
  useShinyjs()
  tags$head(favicon(),bundle_resources(path = app_sys("app/www"),app_title = "SomaVarDB"))
  tags$head(tags$style(HTML(".sep {width: 20px;height: 1px;float: left;}")))
  bsplus::use_bs_tooltip()
  # Add here other external resources
  # for example, you can add shinyalert::useShinyalert()
}
