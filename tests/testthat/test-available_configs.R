# tests/testthat/test-available_configs.R

test_that("available_configs returns a data frame with expected columns", {
  skip_if_not(exists("sspLNIRT.data", where = asNamespace(utils::packageName())),
              message = "sspLNIRT.data not available")

  configs <- available_configs()

  expect_s3_class(configs, "data.frame")
  expect_named(configs, c("thresh", "out.par", "K", "mu.alpha",
                          "meanlog.sigma2", "rho"))
  expect_true(nrow(configs) > 0)
})

test_that("available_configs columns have valid values", {
  skip_if_not(exists("sspLNIRT.data", where = asNamespace(utils::packageName())),
              message = "sspLNIRT.data not available")

  configs <- available_configs()

  expect_true(all(configs$thresh > 0))
  expect_true(all(configs$out.par %in% c("alpha", "beta", "phi", "lambda")))
  expect_true(all(configs$K > 0))
  expect_true(all(abs(configs$rho) <= 1))
})
