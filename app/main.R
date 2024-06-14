box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI,
        tags, uiOutput, shinyOptions, column,
        observeEvent, HTML, tagList, req, conditionalPanel,
        actionButton, icon, span, img, br, modalButton,
        tabsetPanel, tabPanel, showModal, modalDialog, fluidRow],
  cachem[cache_disk],
  config[get],
  shiny.router[router_server, change_page, router_ui, route],
  DBI[dbConnect],
  RSQLite[SQLite],
  shinydashboard[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, infoBox],
  shinydashboardPlus[boxDropdown, boxDropdownItem]
)

box::use(
  app/logic/appDataManager[appDataManager],
  app/logic/genomicDataManager[genomicDataManager],
  app/view/sidebar,
  app/view/patient_view,
  app/view/variant_view,
  app/view/run_view,
  app/view/variant_annoter,
  app/view/presets_manager,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    router_ui(
      route("/",
        tagList(
          dashboardPage(skin = "blue",
        #options = list(sidebarExpandOnHover = TRUE),
            dashboardHeader(
              titleWidth = '25%',
              title = span(img(src = 'static/CHUlogo.png', width = 40, height = 39), "My app"),
              tags$li(class = "dropdown", 
                  actionButton(label = NULL, inputId = ns("goparams"), icon = icon("gear"),
                    class = "actionButtonHeader"),
                  actionButton(label = NULL, inputId = ns("godbinfo"), icon = icon("database"),
                    class = "actionButtonHeader"))
             ),
             dashboardSidebar(width = '25vw', 
                br(),
                sidebar$ui(ns("sidebar")),
                collapsed = FALSE),
             dashboardBody(
               tabsetPanel(id = ns("tabsBody"),
                           tabPanel("PatientView",
                              patient_view$ui(ns("patient_view"))
                            ),
                           tabPanel("VariantView",
                              variant_view$ui(ns("variant_view"))
                           ),
                           tabPanel("RunView",
                                    run_view$ui(ns("run_view"))
                           )
                )
             )
          )#,
          # footer = tags$footer(class = "main-footer",
          #   HTML("<div class=\"pull-right hidden-xs\">
          #   <a href=\"https://clbenoit.github.io/portfolio/projects/germlinevardb\" target=\"_blank\"><b>About the app</b></a>
          #   </div>
          #   Support: <b>benoitclement.data@gmail.com</b>"
          #   )
          # )
      )),
      route("presets_manager_page",
            tagList(
              presets_manager$ui(ns("presets_manager"))
            )
      )
    )
  )
}

box::use(
  app/view/react[sliderNumeric],
)

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    router_server("/")
    
    ## shiny options ##
    options(future.globals.maxSize = 10000*1024^2)
    
    # set up cache directory ##
    Sys.setenv(R_CONFIG_ACTIVE = "default")
    config <- get()
    tempdir <- tempdir()
    print(get("cache_directory"))
    if (get("cache_directory") ==  "default") {
      dir.create(file.path(tempdir, "cache"))
      print(paste0("using following cache directory : ", file.path(tempdir, "cache")))
      shinyOptions(cache = cache_disk(file.path(tempdir,"cache")))
    } else {
      print(paste0("using following cache directory : ", 
                   get("cache_directory")))
      shinyOptions(cache = cache_disk(get("cache_directory")))
    }
    # Set up default user 
    if(Sys.getenv("SHINYPROXY_USERNAME") == ""){
      Sys.setenv(SHINYPROXY_USERNAME = "Me")
    }
    
    ## load database ##
    db_name <- file.path(get("db_path"), paste0(get("prefix"), ".db"))
    con <- dbConnect(SQLite(), db_name)
    
    appDataManager <- appDataManager$new()
    appDataManager$loadAppData(con)
    observeEvent(appDataManager$annoter_reactives$reload, {
      req(appDataManager$annoter_reactives$reload)
      if(appDataManager$annoter_reactives$reload == 0) {
        print("INITIAL RELOAD :: ")
      } else {
        appDataManager$updateDBhash()
      }
    })

    genomicDataManager <- genomicDataManager$new()
    genomicDataManager$loadGenomicData(appDataManager$con)

    observeEvent(input$goparams, {
      req(input$goparams)
      print("go parameters view")
      if(is.null( appDataManager$user_parameters$init_presets_manager)){
        appDataManager$user_parameters$init_presets_manager <- 0
      } else {
        appDataManager$user_parameters$init_presets_manager <- appDataManager$user_parameters$init_presets_manager + 1
      }
      change_page('presets_manager_page')
    })
    
    sidebar$server("sidebar", appData = appDataManager, main_session = session)
    patient_view$server("patient_view", appData = appDataManager, genomicData = genomicDataManager, main_session = session)
    variant_view$server("variant_view", appData = appDataManager, genomicData = genomicDataManager, main_session = session)
    variant_annoter$server("variant_annoter", appData = appDataManager, modal = TRUE)
    run_view$server("run_view", appData = appDataManager, genomicData = genomicDataManager, main_session = session)
    
    
    observeEvent(input$tabsBody, {
      req(input$tabsBody)
      appDataManager$selectors$tab <- input$tabsBody
    })
    
    presets_manager$server("presets_manager", appData = appDataManager)

    
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
                                                                      value = as.character(appDataManager$db_metadata$nb_variants),
                                                                      icon = icon("dna"),color = "olive",width = "100%")
                                                       ),
                                                       column(width = 6,
                                                              infoBox("Total number of samples : ",
                                                                      value = as.character(appDataManager$db_metadata$nb_samples),
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
        
  })
}
