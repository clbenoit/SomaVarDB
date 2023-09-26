testthat::test_that("draw CNV plot works", {
  
  #cnvfile_path <- "/home/ptngs/ClinicalResultsBrowser_vcfs/SEQONE_DATABASE_EXPORTS/CNVs/cnv.tsv"
  #prefix = "splitted_myogre_full"
  #db_path = "/home/ptngs/testdb/"
  cnvfile_path <- NULL
  prefix = NULL
  db_path = NULL
  plota <- SomaVarDB::drawCNVplot_seqone(
                   cnvfile_path = cnvfile_path,
                   prefix = prefix, db_path = db_path, 
                   selected_sample = "22A0173",
                   background_samples = "22A1881",
                   selected_gene = "PLEC")
  
  plota

})
