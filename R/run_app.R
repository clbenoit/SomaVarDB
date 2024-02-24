#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options get_golem_options
#' @importFrom purrr partial
run_app <- function(
  onStart = NULL,
  options = list(launch.browser = TRUE, host  = "0.0.0.0"),
  enableBookmarking = NULL,
  uiPattern = "/",
  prefix = NULL,
  db_path = NULL,
  config_file = NULL,
  config = "default" , 
  app_title = "My Demo Database",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = purrr::partial(eval, expr = global, envir = globalenv()),
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(app_title = app_title ,
                      db_path = db_path, 
                      prefix = prefix, 
                      config_file = config_file, 
                      config = config)
  )
}


#' Run the Demo Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options get_golem_options
run_demo_app <- function(
    onStart = NULL,
    options = list(launch.browser = TRUE, host  = "0.0.0.0"),
    #options = list(launch.browser = FALSE, host  = "0.0.0.0"),
    enableBookmarking = NULL,
    uiPattern = "/",
    prefix = "test",
    config = "demo",
    browser = FALSE,
    db_path = system.file("extdata","testdata", package = "SomaVarDB"),
    config_file = system.file("golem-config.yml", package = "SomaVarDB"),
    app_title = "My Demo Database",
    ...
) {
  if(browser == TRUE){
    config <- paste0(config,"_browser")
    download_genome_reference(browser_server_path = system.file("extdata/testdata/JBrowseR/", package = "SomaVarDB"),
                              "hg19")
  }
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = purrr::partial(eval, expr = global, envir = globalenv()),
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(app_title = app_title ,
                      db_path = db_path, 
                      prefix = prefix, 
                      config_file = config_file,
                      config = config)
  )
}