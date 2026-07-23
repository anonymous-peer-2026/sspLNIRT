# Tests for mod_ssp_custom module

testServer(
  mod_ssp_custom_server,
  args = list(),
  {
    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    session$setInputs(
      mu_theta       = 0,
      mu_zeta        = 0,
      corr_person    = 0.2,
      K              = 30,
      mu_alpha       = 1,
      mu_beta        = 0,
      mu_phi         = 0.5,
      mu_lambda      = 1,
      meanlog_sigma2 = log(0.2),
      sdlog_sigma2   = 0,
      thresh_alpha   = 0.1,
      thresh_beta    = NA,
      thresh_phi     = NA,
      thresh_lambda  = NA,
      iter           = 200,
      XG             = 5000,
      lb             = 50,
      ub             = 2000,
      burnin         = 20,
      seed           = 123456,
      rt_level       = "person",
      rt_logRT       = "FALSE",
      ra_level       = "person",
      ra_by_theta    = "TRUE"
    )

    try(output$out_text, silent = TRUE)

    try(output$plot1, silent = TRUE)
    try(output$plot2, silent = TRUE)

    session$setInputs(draw_rt = 1)
    session$setInputs(draw_ra = 1)
    try(output$plot1, silent = TRUE)
    try(output$plot2, silent = TRUE)

    session$setInputs(rt_level = "item", rt_logRT = "TRUE")
    session$setInputs(ra_level = "item", ra_by_theta = "FALSE")
    try(output$plot1, silent = TRUE)
    try(output$plot2, silent = TRUE)

    try(output$download_script, silent = TRUE)
    try(output$download_design, silent = TRUE)
  }
)

test_that("module ui works", {
  ui <- mod_ssp_custom_ui(id = "test")
  golem::expect_shinytaglist(ui)
  fmls <- formals(mod_ssp_custom_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
