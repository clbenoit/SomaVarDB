test_that("app ui", {
  # Disabling test app ui because ui contains a call to 
  # config::get("multiqc", 
  #             file = get_golem_options("config_file"), 
  #             config = get_golem_options("config"))
  #which needs to be run in the run app context
  #ui <- app_ui()
  #golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  #fmls <- formals(app_ui)
  #for (i in c("request")) {
  #  expect_true(i %in% names(fmls))
  #}
})

test_that("app server", {
  server <- app_server
  expect_type(server, "closure")
  # Check that formals have not been removed
  fmls <- formals(app_server)
  for (i in c("input", "output", "session")) {
    expect_true(i %in% names(fmls))
  }
})

test_that(
  "app_sys works",
  {
    expect_true(
      app_sys("golem-config.yml") != ""
    )
  }
)

test_that(
  "golem-config works",
  {
    config_file <- app_sys("golem-config.yml")
    skip_if(config_file == "")
    
    expect_true(
      get_golem_config(
        "app_prod",
        config = "production",
        file = config_file
      )
    )
    expect_false(
      get_golem_config(
        "app_prod",
        config = "dev",
        file = config_file
      )
    )
  }
)

# Configure this test to fit your need.
# testServer() function makes it possible to test code in server functions and modules, without needing to run the full Shiny application
#testServer(app_server, {

# Set and test an input
#session$setInputs(x = 2)
#expect_equal(input$x, 2)

# Example of tests you can do on the server:
# - Checking reactiveValues
# expect_equal(r$lg, 'EN')
# - Checking output
# expect_equal(output$txt, "Text")
#})

# Configure this test to fit your need
test_that(
  "app launches",
  {
    rlang::check_installed(
      "testthat",
      "to run the tests.",
      version = "3.0.0"
    )
    testthat::skip_if_not_installed("pkgload")
    testthat::skip_if_not_installed("processx")
    testthat::skip_on_cran()
    
    # Ok for now we'll get back to this
    testthat::skip_if_not(interactive())
    if (Sys.getenv("CALLR_CHILD_R_LIBS_USER") == "") {
      pkg_name <- pkgload::pkg_name()
      # We are not in RCMDCHECK
      go_for_pkgload <- TRUE
    } else {
      pkg_name <- Sys.getenv("TESTTHAT_PKG")
      go_for_pkgload <- FALSE
    }
    
    if (is.null(R_path)) {
      if (tolower(.Platform$OS.type) == "windows") {
        r_ <- normalizePath(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"))
      } else {
        r_ <- normalizePath(file.path(Sys.getenv("R_HOME"), "bin", "R"))
      }
    } else {
      r_ <- R_path
    }
    
    if (go_for_pkgload) {
      shinyproc <- processx::process$new(
        command = r_,
        c(
          "-e",
          "pkgload::load_all(here::here());run_demo_app()"
        )
      )
    } else {
      shinyproc <- processx::process$new(
        echo_cmd = TRUE,
        command = r_,
        c(
          "-e",
          sprintf("library(%s, lib = '%s');run_demo_app()", pkg_name, .libPaths())
        ),
        stdout = "|",
        stderr = "|"
      )
    }
    Sys.sleep(16)
    testthat::expect_true(shinyproc$is_alive())
    shinyproc$kill()
  }
)