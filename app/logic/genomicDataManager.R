box::use(
  R6[R6Class],
  shiny[reactiveValues, observeEvent, shinyOptions, req, reactive],
  RSQLite[SQLite],
  dplyr[`%>%`, filter],
  stats[setNames],
  config[get],
  shinybusy[remove_modal_spinner, show_modal_spinner],
  DBI[dbReadTable, dbGetQuery, dbExistsTable, dbExecute],
  
)

#' @export
genomicDataManager <- R6::R6Class(
  classname = "DataManager",
  public = list(
    variant_infos = NULL,
    samples_db = NULL,
    loadGenomicData = function(con) {
      self$variant_infos <- unique(dbReadTable(con,"variant_info")$variant_id) #%>% # Besoin de tout load ici ? C'est juste pour les value de box et input
      self$samples_db <- dbReadTable(con, "samples")
    }
    )
)
