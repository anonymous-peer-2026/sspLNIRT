# tests/testthat/test-print_summary_sspLNIRT.R

test_that("print.summary.sspLNIRT.object prints optim_sample output without error", {

  obj <- structure(
    list(
      N.min     = 200L,
      res.best  = c(alpha = 0.09),
      comp.rmse = list(
        person = list(
          rmse       = c(theta = 0.30, zeta = 0.25),
          mc.sd.rmse = c(theta = 0.01, zeta = 0.02),
          bias       = c(theta = 0.001, zeta = -0.002)
        ),
        item = list(
          rmse       = c(alpha = 0.09, beta = 0.10, phi = 0.03, lambda = 0.04, sigma2 = 0.04),
          mc.sd.rmse = c(alpha = 0.01, beta = 0.02, phi = 0.005, lambda = 0.01, sigma2 = 0.005),
          bias       = c(alpha = 0.001, beta = -0.003, phi = 0.0, lambda = -0.002, sigma2 = 0.02)
        )
      ),
      trace = list(
        steps      = 10,
        track.res  = data.frame(x = 1),
        track.N    = data.frame(N.lb = 50, N.ub = 200, N.temp = 200),
        time.taken = as.difftime(2.5, units = "hours")
      )
    ),
    class = "summary.sspLNIRT"
  )

  output <- capture.output(print(obj))

  expect_true(any(grepl("optim_sample", output)))
  expect_true(any(grepl("Min Sample Size", output)))
  expect_true(any(grepl("200", output)))
  expect_true(any(grepl("RMSE at Min N", output)))
  expect_true(any(grepl("Time Taken", output)))
  expect_true(any(grepl("alpha", output)))
  expect_true(any(grepl("theta", output)))
})

test_that("print.summary.sspLNIRT.object prints comp_rmse output without error", {

  obj <- structure(
    list(
      person = list(
        rmse       = c(theta = 0.30, zeta = 0.25),
        mc.sd.rmse = c(theta = 0.01, zeta = 0.02),
        bias       = c(theta = 0.001, zeta = -0.002)
      ),
      item = list(
        rmse       = c(alpha = 0.09, beta = 0.10),
        mc.sd.rmse = c(alpha = 0.01, beta = 0.02),
        bias       = c(alpha = 0.001, beta = -0.003)
      ),
      rhat.dat = NULL
    ),
    class = "summary.sspLNIRT"
  )

  output <- capture.output(print(obj))

  expect_true(any(grepl("comp_rmse", output)))
  expect_true(any(grepl("alpha", output)))
  expect_true(any(grepl("Bias", output)))
  expect_true(any(grepl("theta", output)))
})

test_that("print.summary.sspLNIRT.object returns x invisibly", {

  obj <- structure(
    list(
      person   = list(rmse = c(theta = 0.3), mc.sd.rmse = c(theta = 0.01), bias = c(theta = 0.0)),
      item     = list(rmse = c(alpha = 0.1), mc.sd.rmse = c(alpha = 0.01), bias = c(alpha = 0.0)),
      rhat.dat = NULL
    ),
    class = "summary.sspLNIRT"
  )

  result <- invisible(capture.output(ret <- print(obj)))
  expect_identical(ret, obj)
})

test_that("print.summary.sspLNIRT.object handles unnamed res.best", {

  # res.best with no names triggers the else branch (lines 37-39)
  obj <- structure(
    list(
      N.min     = 200L,
      res.best  = unname(c(0.09)),
      comp.rmse = list(
        person = list(
          rmse       = c(theta = 0.30, zeta = 0.25),
          mc.sd.rmse = c(theta = 0.01, zeta = 0.02),
          bias       = c(theta = 0.001, zeta = -0.002)
        ),
        item = list(
          rmse       = c(alpha = 0.09, beta = 0.10),
          mc.sd.rmse = c(alpha = 0.01, beta = 0.02),
          bias       = c(alpha = 0.001, beta = -0.003)
        )
      ),
      out.par = "alpha",
      trace = list(
        steps      = 5,
        time.taken = as.difftime(1, units = "hours")
      )
    ),
    class = "summary.sspLNIRT"
  )

  output <- capture.output(print(obj))
  expect_true(any(grepl("RMSE at Min N", output)))
  expect_true(any(grepl("0.0900", output)))
})

test_that("print.summary.sspLNIRT.object prints fallback when no detailed RMSE", {

  # comp.rmse with empty item$rmse triggers the else branch (lines 61-63)
  obj <- structure(
    list(
      N.min     = 200L,
      res.best  = c(alpha = 0.09),
      comp.rmse = list(
        item = list(rmse = numeric(0))
      ),
      trace = list(
        steps      = 5,
        time.taken = as.difftime(1, units = "hours")
      )
    ),
    class = "summary.sspLNIRT"
  )

  output <- capture.output(print(obj))
  expect_true(any(grepl("Detailed RMSE breakdown not available", output)))

  # also covers NULL comp.rmse case
  obj2 <- structure(
    list(
      N.min     = 200L,
      res.best  = c(alpha = 0.09),
      comp.rmse = NULL,
      trace = list(
        steps      = 5,
        time.taken = as.difftime(1, units = "hours")
      )
    ),
    class = "summary.sspLNIRT"
  )
  output2 <- capture.output(print(obj2))
  expect_true(any(grepl("Detailed RMSE breakdown not available", output2)))
})

test_that("print_rmse_block handles empty and mismatched-length blocks", {

  # Empty rmse block triggers (no values) branch
  obj_empty <- structure(
    list(
      person = list(
        rmse       = numeric(0),
        mc.sd.rmse = numeric(0),
        bias       = numeric(0)
      ),
      item = list(
        rmse       = numeric(0),
        mc.sd.rmse = numeric(0),
        bias       = numeric(0)
      ),
      rhat.dat = NULL
    ),
    class = "summary.sspLNIRT"
  )

  output_empty <- capture.output(print(obj_empty))
  expect_true(any(grepl("\\(no values\\)", output_empty)))

  # Mismatched lengths trigger the pad() fallback (line 99): mc.sd.rmse missing
  obj_mismatch <- structure(
    list(
      person = list(
        rmse       = c(theta = 0.30, zeta = 0.25),
        mc.sd.rmse = numeric(0),
        bias       = numeric(0)
      ),
      item = list(
        rmse       = c(alpha = 0.09, beta = 0.10),
        mc.sd.rmse = numeric(0),
        bias       = numeric(0)
      ),
      rhat.dat = NULL
    ),
    class = "summary.sspLNIRT"
  )

  output_mismatch <- capture.output(print(obj_mismatch))
  expect_true(any(grepl("NA", output_mismatch)))
})
