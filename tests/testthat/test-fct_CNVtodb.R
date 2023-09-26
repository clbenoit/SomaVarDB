testthat::test_that("CNVtodb works", {
  
  #cnvfile_path <- "/home/ptngs/ClinicalResultsBrowser_vcfs/SEQONE_DATABASE_EXPORTS/CNVs/cnv.tsv"
  #prefix = "splitted_myogre_full"
  #db_path = "/home/ptngs/testdb/"
  cnvfile_path <- NULL
  prefix = NULL
  db_path = NULL
  CNVtodb_seqone(cnvfile_path = cnvfile_path,db_path =db_path,
                   prefix = prefix)

})

#### CHECKER SI LES DATA SONT DUPLIQUUES PAR SAMPLE/GENE ET PK ?
