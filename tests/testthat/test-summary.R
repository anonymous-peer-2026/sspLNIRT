# tests/testthat/test-summary_sspLNIRT.R

test_that("summary.sspLNIRT.object works on optim_sample-style objects", {

  obj <- structure(
    list(
      N.min     = 200L,
      res.best  = c(alpha = 0.09),
      comp.rmse = list(
        person = list(rmse = c(theta = 0.3, zeta = 0.25)),
        item   = list(rmse = c(alpha = 0.09, beta = 0.10,
                               phi = 0.03, lambda = 0.04, sigma2 = 0.04))
      ),
      trace = list(steps = 10,
                   track.res = data.frame(x = 1),
                   track.N   = data.frame(N.lb = 50, N.ub = 200, N.temp = 200),
                   time.taken = as.difftime(1, units = "hours"))
    ),
    class = "sspLNIRT"
  )

  smry <- summary(obj)

  expect_s3_class(smry, "summary.sspLNIRT")
  expect_named(smry, c("N.min", "out.par", "res.best", "comp.rmse", "trace"))
  expect_equal(smry$N.min, 200L)
  expect_equal(smry$res.best, c(alpha = 0.09))
})

test_that("summary.sspLNIRT.object works on comp_rmse-style objects", {

  obj <- structure(
    list(
      person   = list(rmse = c(theta = 0.3, zeta = 0.25),
                      mc.sd.rmse = c(theta = 0.01, zeta = 0.01),
                      bias = c(theta = 0.001, zeta = -0.002)),
      item     = list(rmse = c(alpha = 0.09, beta = 0.10),
                      mc.sd.rmse = c(alpha = 0.01, beta = 0.02),
                      bias = c(alpha = 0.001, beta = -0.003)),
      rhat.dat = NULL,
      err.dat  = list(person = data.frame(), item = data.frame())
    ),
    class = "sspLNIRT"
  )

  smry <- summary(obj)

  expect_s3_class(smry, "summary.sspLNIRT")
  expect_named(smry, c("person", "item", "rhat.dat"))
  expect_equal(smry$person$rmse, c(theta = 0.3, zeta = 0.25))
})

test_that("summary.sspLNIRT.object rejects wrong class", {
  expect_error(summary.sspLNIRT(list(a = 1)),
               "Input must be an sspLNIRT object")
})

test_that("summary.sspLNIRT.object handles character N.min (boundary case)", {

  obj <- structure(
    list(
      N.min     = "res.ub > thresh",
      res.best  = c(alpha = 0.15),
      comp.rmse = list(),
      trace     = list(steps = 2, track.res = data.frame(),
                       track.N = data.frame(),
                       time.taken = as.difftime(0.5, units = "hours"))
    ),
    class = "sspLNIRT"
  )

  smry <- summary(obj)

  expect_s3_class(smry, "summary.sspLNIRT")
  expect_equal(smry$N.min, "res.ub > thresh")
})
