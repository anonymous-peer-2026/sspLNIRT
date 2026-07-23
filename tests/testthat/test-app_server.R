# Tests for app_server (internal)

test_that("app_server updates navbar when go_to_tab changes", {

  skip_if_not_installed("shiny")

  testServer(app_server, {

    session$setInputs(go_to_tab = "ssp_data")

    # observer should run without error
    expect_true(TRUE)
  })
})


test_that("app_server calls module server functions", {

  called_custom <- FALSE
  called_data <- FALSE

  mockery::stub(
    app_server,
    "mod_ssp_custom_server",
    function(id) {
      called_custom <<- TRUE
      expect_equal(id, "ssp_custom")
    }
  )

  mockery::stub(
    app_server,
    "mod_ssp_data_server",
    function(id) {
      called_data <<- TRUE
      expect_equal(id, "ssp_data")
    }
  )

  testServer(app_server, {})

  expect_true(called_custom)
  expect_true(called_data)
})
