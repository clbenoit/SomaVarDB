testthat::test_that("adding Mobidetails to database works", {
  addMDtodb(API_key = "l6ln_tsCbdZk8ICVpy6z86vhhb2rFUkFFTfV3wq7L-4",
            db_path = system.file("extdata","testdata", package = "SomaVarDB"),
            prefix = "test")
})
