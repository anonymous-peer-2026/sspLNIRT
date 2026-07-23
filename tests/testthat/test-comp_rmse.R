# tests/testthat/test-comp_rmse.R

test_that("comp_rmse returns correct structure with default flags", {

  skip_if_not_installed("LNIRT")
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  skip_if_not_installed("progressr")

  future::plan(future::sequential)
  on.exit(future::plan(future::sequential), add = TRUE)

  result <- comp_rmse(
    N              = 30,
    iter           = 1,
    K              = 5,
    mu.person      = c(0, 0),
    mu.item        = c(1, 0, 0.5, 1),
    meanlog.sigma2 = log(0.6),
    cov.m.person   = matrix(c(1, 0.2, 0.2, 1), ncol = 2),
    cov.m.item     = diag(4),
    sd.item        = c(0.2, 1, 0.2, 0.5),
    cor2cov.item   = TRUE,
    sdlog.sigma2   = 0,
    XG             = 50,
    burnin         = 20,
    seed           = 123,
    keep.err.dat   = FALSE,
    keep.rhat.dat  = FALSE
  )

  expect_s3_class(result, "sspLNIRT")

  expect_named(result, c("person", "item", "rhat.dat", "err.dat"))

  expect_named(result$person, c("rmse", "mc.sd.rmse", "bias"))
  expect_named(result$person$rmse, c("theta", "zeta"))
  expect_length(result$person$rmse, 2)
  expect_true(all(result$person$rmse >= 0))

  expect_named(result$item, c("rmse", "mc.sd.rmse", "bias"))
  expect_named(result$item$rmse, c("alpha", "beta", "phi", "lambda", "sigma2"))
  expect_length(result$item$rmse, 5)
  expect_true(all(result$item$rmse >= 0))

  expect_null(result$rhat.dat)

  expect_true(is.data.frame(result$err.dat$item))
  expect_true(is.data.frame(result$err.dat$person))
  expect_true(all(c("par", "bin", "mean_sim", "mean_err", "mean_rmse") %in%
                    colnames(result$err.dat$item)))
})


test_that("comp_rmse returns rhat.dat when keep.rhat.dat = TRUE", {

  skip_if_not_installed("LNIRT")
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  skip_if_not_installed("progressr")

  future::plan(future::sequential)
  on.exit(future::plan(future::sequential), add = TRUE)

  result <- comp_rmse(
    N              = 30,
    iter           = 1,
    K              = 5,
    mu.person      = c(0, 0),
    mu.item        = c(1, 0, 0.5, 1),
    meanlog.sigma2 = log(0.6),
    cov.m.person   = matrix(c(1, 0.2, 0.2, 1), ncol = 2),
    cov.m.item     = diag(4),
    sd.item        = c(0.2, 1, 0.2, 0.5),
    cor2cov.item   = TRUE,
    sdlog.sigma2   = 0,
    XG             = 50,
    burnin         = 20,
    seed           = 456,
    keep.err.dat   = FALSE,
    keep.rhat.dat  = TRUE
  )

  expect_false(is.null(result$rhat.dat))
  expect_true(is.matrix(result$rhat.dat) || is.numeric(result$rhat.dat))
})


test_that("comp_rmse input validation works", {

  expect_error(comp_rmse(N = -1), "'N' must be a single integer >= 2")
  expect_error(comp_rmse(N = c(10, 20)), "'N' must be a single integer >= 2")
  expect_error(comp_rmse(N = "a"), "'N' must be a single integer >= 2")
  expect_error(comp_rmse(N = 100, iter = -1), "'iter' must be a positive integer")
  expect_error(comp_rmse(N = 100, K = 0), "'K' must be a positive integer")
  expect_error(comp_rmse(N = 100, mu.person = c(0, 0, 0)), "'mu.person' must be length 2")
  expect_error(comp_rmse(N = 100, mu.item = c(1, 2)), "'mu.item' must be length 4")
  expect_error(comp_rmse(N = 100, cov.m.person = diag(3)), "'cov.m.person' must be a 2x2 matrix")
  expect_error(comp_rmse(N = 100, cov.m.item = diag(3)), "'cov.m.item' must be a 4x4 matrix")
  expect_error(comp_rmse(N = 100, cor2cov.item = TRUE, sd.item = NULL),
               "'sd.item' must be provided")
  expect_error(comp_rmse(N = 100, sd.item = c(1, 2)), "'sd.item' must be length 4")
  expect_error(comp_rmse(N = 100, XG = 0), "'XG' must be a positive integer")
  expect_error(comp_rmse(N = 100, burnin = 100), "'burnin' must be in")
})


test_that("comp_rmse validates symmetry and item.pars.m shape", {

  expect_error(comp_rmse(N = 100, cov.m.person = c(1, 0.4, 0.4, 1)),
               "'cov.m.person' must be a 2x2 matrix")

  expect_error(comp_rmse(N = 100, cov.m.item = c(1, 0)),
               "'cov.m.item' must be a 4x4 matrix")

  expect_error(
    comp_rmse(N = 100,
              cov.m.person = matrix(c(1, 0.4, 0.5, 1), ncol = 2, byrow = TRUE)),
    "'cov.m.person' must be symmetric"
  )

  asym_cov_item <- matrix(c(1, 0,   0,   0,
                            0, 1,   0,   0.4,
                            0, 0,   1,   0,
                            0, 0.5, 0,   1), ncol = 4, byrow = TRUE)
  expect_error(
    comp_rmse(N = 100, cov.m.item = asym_cov_item),
    "'cov.m.item' must be symmetric"
  )

  expect_error(comp_rmse(N = 100, item.pars.m = c(1, 2, 3, 4)),
               "'item.pars.m' must be a matrix with 4 columns")

  expect_error(comp_rmse(N = 100, K = 5,
                         item.pars.m = matrix(0, nrow = 5, ncol = 3)),
               "'item.pars.m' must be a matrix with 4 columns")

  expect_error(comp_rmse(N = 100, K = 5,
                         item.pars.m = matrix(0, nrow = 3, ncol = 4)),
               "'item.pars.m' must have K rows")
})
