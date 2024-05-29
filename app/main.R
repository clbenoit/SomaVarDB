box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI,
        tags, uiOutput, shinyOptions, 
        observeEvent, HTML, tagList, 
        actionButton, icon, span, img, br],
  cachem[cache_disk],
  config[get],
  shiny.router[router_server, change_page, router_ui, route],
  DBI[dbConnect],
  RSQLite[SQLite],
  shinydashboard[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody]
  
)

box::use(
  app/logic/dataManager[DataManager],
  app/view/sidebar,
  app/view/patient_view,
  app/view/variant_annoter,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  bootstrapPage(
    router_ui(
      route("/",
        tagList(
          dashboardPage(skin = "blue", #"blue-light",
            #options = list(sidebarExpandOnHover = TRUE),
            dashboardHeader(
              titleWidth = '25%',
              #title = span(img(src = 'www/CHUlogo.png', width = 40, height = 39), get_golem_options("app_title")),
              title = span(img(src = 'static/CHUlogo.png', width = 40, height = 39), "My app"),
              tags$li(class = "dropdown", 
                  actionButton(label = NULL, inputId = "goparams",icon = icon("gear"),
                    style = "position: relative; margin: 10px 10px 10px 10px; display:center-align;"),
                  actionButton(label = NULL, inputId = "godbinfo",icon = icon("database"),
                    style = "position: relative; margin: 10px 10px 10px 10px; display:center-align;"))
             ),
             dashboardSidebar(width = '25vw', 
                #id = "sidebars", minified = FALSE,
                br(),
                sidebar$ui(ns("sidebar")),
                collapsed = FALSE),
             dashboardBody(
               patient_view$ui(ns("patient_view"))#,
               #variant_annoter$ui(ns("variant_annoter")),
               
             )
          )#,
          # footer = tags$footer(class = "main-footer",
          #   HTML("<div class=\"pull-right hidden-xs\">
          #   <a href=\"https://clbenoit.github.io/portfolio/projects/germlinevardb\" target=\"_blank\"><b>About the app</b></a>
          #   </div>
          #   Support: <b>benoitclement.data@gmail.com</b>"
          #   )
          # )
      ),
      route("parameters",
            tagList(
              HTML("parameters page content")
            )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
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
    DataManager <- DataManager$new()
    db_name <- file.path(get("db_path"), paste0(get("prefix"), ".db"))
    con <- dbConnect(SQLite(), db_name)
    DataManager$loadDb(con)
    
    router_server() # mandatory of shiny.route package use
    observeEvent(input$goparams, {
      req(input$goparams)
      change_page('parameters')
    })
    
    sidebar$server("sidebar", data = DataManager)
    patient_view$server("patient_view", data = DataManager)
    variant_annoter$server("variant_annoter", data = DataManager, modal = TRUE)
    
  })
}
