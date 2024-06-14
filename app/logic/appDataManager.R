box::use(
  R6[R6Class],
  shiny[reactiveValues, observeEvent, shinyOptions, req, reactive],
  RSQLite[SQLite],
  dplyr[`%>%`, filter],
  stats[setNames],
  config[get],
  shinybusy[remove_modal_spinner, show_modal_spinner],
  DBI[dbReadTable, dbGetQuery, dbExistsTable, dbExecute, dbSendQuery],
  utils[read.table]
)

#' @export
appDataManager <- R6::R6Class(
  classname = "DataManager",
  public = list(
    con = NULL,
    selectors = reactiveValues(tab = "VariantView", variant = NULL),
    db_metadata = NULL,
    canonical_transcripts = NULL,
    user_parameters = reactiveValues(presets = NULL, manifests_list = NULL, transcript_lists = NULL, init_presets_manager =  NULL), 
    filters = reactiveValues(allelefrequency_value = NULL, gnomadfrequency_value= NULL, 
                             quality_value = NULL, coverage_value = NULL,
                             impact = NULL, trlist = NULL,
                             manifest = NULL),
    annoter_reactives = reactiveValues(launchmodal = 0, my_variant_id = NULL, reload = 0),
    loadAppData = function(con) {
      print("inside load DB")
      self$con <- con
      shinybusy::show_modal_spinner(
        spin = "double-bounce", color = "#112446",
        text = "Loading database metadata")
  
        self$db_metadata <- dbReadTable(con, "db_metadata")
        if(self$db_metadata$genome_version == "hg19"){
          print(getwd())
          self$canonical_transcripts <- read.table(file = "app/data/annotations/hg19_canonical_transcripts_and_genes.txt",
                                                  header = TRUE, sep = "\t")
          
        } else if (self$db_metadata$genome_version == "hg38"){
          self$canonical_transcripts <- read.table(file = "app/data/annotations/hg38_canonical_transcripts_and_genes.txt",
                                                   header = TRUE, sep = "\t")
        }

        # Different sidebars according to selected tab
        
        if (!dbExistsTable(conn = con, "manifests_list")) {
          manifests_list <- data.frame(user_id = "Me", 
                                       manifests = "mymanifest_Me")
          dbWriteTable(con, name = "manifests_list", value = manifests_list, overwrite = TRUE)
        } else {
          manifests_list <- DBI::dbReadTable(conn = con, name = "manifests_list") %>% filter(user_id == Sys.getenv("SHINYPROXY_USERNAME"))
          self$user_parameters$manifests_list <- gsub(paste0("_", Sys.getenv("SHINYPROXY_USERNAME")), "", manifests_list$manifests)
        }
        
        if (!(dbExistsTable(con,"presets"))) {
          presets <- data.frame(user = "mysetup", name = "mysetup", allelefrequencynum = "mysetup", coveragenum = "mysetup", 
                                qualitynum = "mysetup", gnomadnum = "mysetup",
                                impact = "mysetup", trlist = "mysetup", manifest = "mysetup")
          dbWriteTable(con, name = "presets", value = presets, overwrite = TRUE)
        } else {
          self$user_parameters$presets <- DBI::dbReadTable(con, "presets") %>% filter(user == Sys.getenv("SHINYPROXY_USERNAME"))
        }
        
        transcript_lists <- DBI::dbGetQuery(conn = con, 
                                            paste0("SELECT name FROM sqlite_master WHERE type='table' AND name LIKE '%",
                                                   Sys.getenv("SHINYPROXY_USERNAME"), "_transcriptlist",
                                                   "%';"))
        
        self$user_parameters$transcript_lists <- gsub(paste0("_",Sys.getenv("SHINYPROXY_USERNAME")),
                                      "", gsub("_transcriptlist","",transcript_lists$name))
        
        dbExecute(conn = con, "CREATE TABLE IF NOT EXISTS manifests_list (user_id TEXT, manifests TEXT);")
        remove_modal_spinner()
    }, 
    updateDBhash = function() {
        print("RELOAD AGAIN :: ")
        print(self$db_metadata$hash)
        apn_sql <- paste0("UPDATE db_metadata SET hash = '", paste0(self$db_metadata$hash," | UPDATED"), "';")
        dbSendQuery(self$con, apn_sql)
        db_metadata <- DBI::dbReadTable(self$con, "db_metadata")
        self$db_metadata$hash <- db_metadata$hash
    }    
  )
)
