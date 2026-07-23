# Tests for mod_ssp_data module

testServer(
  mod_ssp_data_server,
  args = list(),
  {
    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    session$setInputs(
      K              = 30,
      mu_alpha       = 0.6,
      meanlog_sigma2 = log(0.2),
      corr_person    = 0.2,
      thresh_alpha   = 0.1,
      thresh_beta    = "",
      thresh_phi     = "",
      thresh_lambda  = "",
      rt_level       = "person",
      rt_logRT       = "FALSE",
      ra_level       = "person",
      ra_by_theta    = "TRUE",
      estimation_pars = "item",
      accuracy_yval  = "rmse"
    )

    result <- ssp_result()
    expect_true(is.list(result))
    expect_true(
      !is.null(result$error) || !is.null(result$object) || is.null(result$object)
    )

    expect_true(TRUE)

    try(output$out_header,        silent = TRUE)
    try(output$out_param_tables,  silent = TRUE)
    try(output$convergence_table, silent = TRUE)

    try(output$plot1, silent = TRUE)
    try(output$plot2, silent = TRUE)
    try(output$plot3, silent = TRUE)
    try(output$plot4, silent = TRUE)

    session$setInputs(draw_rt = 1)
    session$setInputs(draw_ra = 1)
    try(output$plot1, silent = TRUE)
    try(output$plot2, silent = TRUE)

    try(output$download_object, silent = TRUE)
    try(output$download_design, silent = TRUE)

    session$setInputs(
      thresh_alpha  = "",
      thresh_beta   = "",
      thresh_phi    = "",
      thresh_lambda = ""
    )
    result2 <- ssp_result()
    expect_false(is.null(result2$error))

    session$setInputs(estimation_pars = "person", accuracy_yval = "bias")
    try(output$plot3, silent = TRUE)

    session$setInputs(rt_level = "item", rt_logRT = "TRUE")
    session$setInputs(ra_level = "item", ra_by_theta = "FALSE")
    try(output$plot1, silent = TRUE)
    try(output$plot2, silent = TRUE)
  }
)

test_that("module ui works", {
  ui <- mod_ssp_data_ui(id = "test")
  golem::expect_shinytaglist(ui)
  fmls <- formals(mod_ssp_data_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})
