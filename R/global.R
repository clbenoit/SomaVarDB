global <- quote({
  db_name <- file.path(golem::get_golem_options("db_path"),paste0(golem::get_golem_options("prefix"), ".db"))
  tictoc::tic(paste0("loging in ",db_name," database"))
  
  options(future.globals.maxSize = 10000*1024^2)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  samples_db <- DBI::dbReadTable(con,"samples")
  db_metadata <- DBI::dbReadTable(con,"db_metadata")
  variant_infos <- shiny::reactive({unique(DBI::dbReadTable(con,"variant_info")$variant_id)}) #%>% # Besoin de tout load ici ? C'est juste pour les value de box et input
  if(!(DBI::dbExistsTable(con,"presets"))){
    presets <- data.frame(user = "mysetup", 
                          name = "mysetup",
                          allelefrequencynum = "mysetup",
                          coveragenum = "mysetup", 
                          qualitynum = "mysetup", 
                          impact = "mysetup",
                          trlist = "mysetup",
                          manifest = "mysetup")
    DBI::dbWriteTable(con, name = "presets", value = presets, overwrite = TRUE)}
  
  DBI::dbExecute(conn = con, "CREATE TABLE IF NOT EXISTS manifests_list (user_id TEXT, manifests TEXT);")  
  
  tictoc::toc()
})

