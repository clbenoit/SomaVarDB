testthat::test_that("frequency computation works", {
  db_path = system.file("extdata","testdata", package = "SomaVarDB")
  prefix = "test"
  SomaVarDB::compute_frequency(db_path = db_path, prefix = prefix,attribute = "constant_group")
})
  
