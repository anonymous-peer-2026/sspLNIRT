# Tests for optim_sample

# Valid baseline arguments
valid_args <- function(...) {
  defaults <- list(
    out.par        = "alpha",
    thresh         = 0.1,
    range          = c(50, 2000),
    iter           = 200,
    K              = 30,
    mu.person      = c(0, 0),
    mu.item        = c(1, 0, 0.5, 1),
    meanlog.sigma2 = log(0.6),
    cov.m.person   = matrix(c(1,   0.4,
                              0.4, 1), ncol = 2, byrow = TRUE),
    cov.m.item     = matrix(c(1, 0,   0,   0,
                              0, 1,   0,   0.4,
                              0, 0,   1,   0,
                              0, 0.4, 0,   1), ncol = 4, byrow = TRUE),
    sdlog.sigma2   = 0,
    item.pars.m    = NULL,
    cor2cov.item   = FALSE,
    sd.item        = NULL,
    seed           = 1L,
    XG             = 5000,
    burnin         = 20,
    keep.err.dat   = FALSE,
    keep.rhat.dat  = FALSE,
    verbose        = FALSE
  )
  utils::modifyList(defaults, list(...))
}

test_that("optim_sample errors when range is not length 2", {
  expect_error(
    do.call(optim_sample, valid_args(range = 100)),
    "range"
  )
  expect_error(
    do.call(optim_sample, valid_args(range = c(50, 100, 200))),
    "range"
  )
})

test_that("optim_sample errors when range[1] >= range[2]", {
  expect_error(
    do.call(optim_sample, valid_args(range = c(500, 500))),
    "range"
  )
  expect_error(
    do.call(optim_sample, valid_args(range = c(500, 100))),
    "range"
  )
})

test_that("optim_sample errors when range[1] < 2", {
  expect_error(
    do.call(optim_sample, valid_args(range = c(1, 100))),
    "range\\[1\\]"
  )
})

test_that("optim_sample errors when thresh and out.par lengths differ", {
  expect_error(
    do.call(optim_sample,
            valid_args(thresh = c(0.1, 0.2), out.par = "alpha")),
    "same length"
  )
})

test_that("optim_sample errors when thresh contains non-positive values", {
  expect_error(
    do.call(optim_sample, valid_args(thresh = 0)),
    "positive"
  )
  expect_error(
    do.call(optim_sample, valid_args(thresh = -0.1)),
    "positive"
  )
  expect_error(
    do.call(optim_sample,
            valid_args(thresh  = c(0.1, -0.05),
                       out.par = c("alpha", "beta"))),
    "positive"
  )
})

test_that("optim_sample errors when out.par contains invalid names", {
  expect_error(
    do.call(optim_sample, valid_args(out.par = "discrim")),
    "subset of"
  )
  expect_error(
    do.call(optim_sample,
            valid_args(thresh = c(0.1, 0.1),
                       out.par = c("alpha", "gamma"))),
    "subset of"
  )
})

test_that("optim_sample errors on non-positive iter", {
  expect_error(do.call(optim_sample, valid_args(iter = 0)),  "iter")
  expect_error(do.call(optim_sample, valid_args(iter = -1)), "iter")
  expect_error(do.call(optim_sample, valid_args(iter = "many")), "iter")
})

test_that("optim_sample errors on non-positive K", {
  expect_error(do.call(optim_sample, valid_args(K = 0)),  "K")
  expect_error(do.call(optim_sample, valid_args(K = -5)), "K")
  expect_error(do.call(optim_sample, valid_args(K = "thirty")), "K")
})

test_that("optim_sample errors when mu.person is not length 2", {
  expect_error(
    do.call(optim_sample, valid_args(mu.person = c(0, 0, 0))),
    "mu.person"
  )
  expect_error(
    do.call(optim_sample, valid_args(mu.person = 0)),
    "mu.person"
  )
})

test_that("optim_sample errors when mu.item is not length 4", {
  expect_error(
    do.call(optim_sample, valid_args(mu.item = c(1, 0, 0.5))),
    "mu.item"
  )
  expect_error(
    do.call(optim_sample, valid_args(mu.item = c(1, 0, 0.5, 1, 1))),
    "mu.item"
  )
})

test_that("optim_sample errors when cov.m.person is not a 2x2 matrix", {
  expect_error(
    do.call(optim_sample, valid_args(cov.m.person = c(1, 0.4, 0.4, 1))),
    "2x2"
  )
  expect_error(
    do.call(optim_sample,
            valid_args(cov.m.person = matrix(1, 3, 3))),
    "2x2"
  )
})

test_that("optim_sample errors when cov.m.person is not symmetric", {
  asymm <- matrix(c(1, 0.4,
                    0.5, 1), ncol = 2, byrow = TRUE)
  expect_error(
    do.call(optim_sample, valid_args(cov.m.person = asymm)),
    "symmetric"
  )
})

test_that("optim_sample errors when cov.m.item is not a 4x4 matrix", {
  expect_error(
    do.call(optim_sample, valid_args(cov.m.item = matrix(1, 3, 3))),
    "4x4"
  )
  expect_error(
    do.call(optim_sample, valid_args(cov.m.item = "not a matrix")),
    "4x4"
  )
})

test_that("optim_sample errors when cov.m.item is not symmetric", {
  asymm <- matrix(c(1, 0,   0,   0,
                    0, 1,   0,   0.4,
                    0, 0,   1,   0,
                    0, 0.5, 0,   1),  # 0.4 vs 0.5 breaks symmetry
                  ncol = 4, byrow = TRUE)
  expect_error(
    do.call(optim_sample, valid_args(cov.m.item = asymm)),
    "symmetric"
  )
})

test_that("optim_sample errors when cor2cov.item = TRUE but sd.item is NULL", {
  expect_error(
    do.call(optim_sample,
            valid_args(cor2cov.item = TRUE, sd.item = NULL)),
    "sd.item"
  )
})

test_that("optim_sample errors when sd.item is not length 4", {
  expect_error(
    do.call(optim_sample, valid_args(sd.item = c(0.2, 1, 0.2))),
    "sd.item"
  )
  expect_error(
    do.call(optim_sample, valid_args(sd.item = c(0.2, 1, 0.2, 0.5, 0.1))),
    "sd.item"
  )
})

test_that("optim_sample errors when item.pars.m has wrong column count", {
  expect_error(
    do.call(optim_sample,
            valid_args(item.pars.m = matrix(0, nrow = 30, ncol = 3))),
    "4 columns"
  )
})

test_that("optim_sample errors when item.pars.m row count != K", {
  expect_error(
    do.call(optim_sample,
            valid_args(K = 30,
                       item.pars.m = matrix(0, nrow = 25, ncol = 4))),
    "K rows"
  )
})

test_that("optim_sample errors when item.pars.m is not a matrix", {
  expect_error(
    do.call(optim_sample,
            valid_args(item.pars.m = as.data.frame(matrix(0, 30, 4)))),
    "4 columns"
  )
})

test_that("optim_sample errors on non-positive XG", {
  expect_error(do.call(optim_sample, valid_args(XG = 0)),    "XG")
  expect_error(do.call(optim_sample, valid_args(XG = -100)), "XG")
})

test_that("optim_sample errors on out-of-range burnin", {
  expect_error(do.call(optim_sample, valid_args(burnin = -1)),  "burnin")
  expect_error(do.call(optim_sample, valid_args(burnin = 100)), "burnin")
  expect_error(do.call(optim_sample, valid_args(burnin = 150)), "burnin")
})

test_that("optim_sample errors when verbose is not a single logical", {
  expect_error(do.call(optim_sample, valid_args(verbose = "yes")),
               "verbose")
  expect_error(do.call(optim_sample, valid_args(verbose = c(TRUE, FALSE))),
               "verbose")
  expect_error(do.call(optim_sample, valid_args(verbose = NA_integer_)),
               "verbose")
})

test_that("optim_sample returns S3 'sspLNIRT' object with expected fields", {

  testthat::skip_if_not_installed("LNIRT")

  result <- do.call(optim_sample, valid_args(
    thresh  = 100,            # impossibly loose, lb will satisfy it
    range   = c(50, 100),
    iter    = 2,
    K       = 5,
    XG      = 100,
    burnin  = 10,
    seed    = 1L,
    verbose = FALSE
  ))

  expect_s3_class(result, "sspLNIRT")
  expect_named(result, c("N.min", "res.best", "comp.rmse", "trace"))
  expect_named(result$trace,
               c("steps", "track.res", "track.N", "time.taken"))
  expect_equal(result$N.min, "res.lb < thresh")
})

test_that("optim_sample triggers res.ub > thresh branch", {

  testthat::skip_if_not_installed("LNIRT")

  result <- do.call(optim_sample, valid_args(
    thresh  = 1e-10,          # impossibly tight, ub will not satisfy it
    range   = c(50, 60),
    iter    = 2,
    K       = 5,
    XG      = 100,
    burnin  = 10,
    seed    = 1L,
    verbose = FALSE
  ))

  expect_s3_class(result, "sspLNIRT")
  expect_equal(result$N.min, "res.ub > thresh")
  expect_equal(result$trace$steps, 2)
})

test_that("optim_sample verbose = TRUE emits messages", {
  testthat::skip_if_not_installed("LNIRT")

  expect_message(
    do.call(optim_sample, valid_args(
      thresh  = 100,
      range   = c(50, 100),
      iter    = 2,
      K       = 5,
      XG      = 100,
      burnin  = 10,
      seed    = 1L,
      verbose = TRUE
    )),
    "LB result"
  )
})

# Helper: build a fake comp_rmse
make_fake_comp_rmse <- function(rmse_value, out_par = "alpha") {
  res <- stats::setNames(rmse_value, out_par)
  list(
    item   = list(
      rmse        = res,
      mc.sd.rmse  = stats::setNames(0.01, out_par),
      bias        = stats::setNames(0,    out_par)
    ),
    person = list(
      rmse        = c(theta = 0.1, zeta = 0.1),
      mc.sd.rmse  = c(theta = 0.01, zeta = 0.01),
      bias        = c(theta = 0,    zeta = 0)
    ),
    rhat.dat = NULL,
    err.dat  = NULL
  )
}

test_that("optim_sample drives both bisection branches and terminates on inc < 1", {
  testthat::skip_if_not_installed("testthat")

  # Scripted RMSE sequence
  scripted_rmse <- c(0.20, 0.05, 0.08, 0.15, 0.09, 0.12, 0.08, 0.09)
  call_counter  <- 0L

  fake_comp_rmse <- function(...) {
    call_counter <<- call_counter + 1L
    if (call_counter > length(scripted_rmse)) {
      stop("Mock exhausted — bisection ran longer than expected.")
    }
    make_fake_comp_rmse(scripted_rmse[call_counter])
  }

  testthat::local_mocked_bindings(
    comp_rmse = fake_comp_rmse,
    .package  = "sspLNIRT"
  )

  result <- do.call(optim_sample, valid_args(
    thresh  = 0.1,
    out.par = "alpha",
    range   = c(50, 100),
    iter    = 2,
    K       = 5,
    verbose = FALSE
  ))

  expect_s3_class(result, "sspLNIRT")
  expect_equal(result$N.min, 67)
  expect_equal(call_counter, 8L)
  expect_true(result$trace$steps >= 8)
  expect_equal(result$res.best,
               stats::setNames(0.09, "alpha"))
})

test_that("optim_sample mocked: res.lb < thresh early exit", {
  fake_comp_rmse <- function(...) make_fake_comp_rmse(0.05)

  testthat::local_mocked_bindings(
    comp_rmse = fake_comp_rmse,
    .package  = "sspLNIRT"
  )

  result <- do.call(optim_sample, valid_args(
    thresh  = 0.1,
    out.par = "alpha",
    range   = c(50, 100),
    verbose = FALSE
  ))

  expect_equal(result$N.min, "res.lb < thresh")
  expect_equal(result$trace$steps, 1)
})

test_that("optim_sample mocked: res.ub > thresh early exit", {
  scripted <- c(0.30, 0.20)  # both > thresh = 0.1
  call_counter <- 0L
  fake_comp_rmse <- function(...) {
    call_counter <<- call_counter + 1L
    make_fake_comp_rmse(scripted[call_counter])
  }

  testthat::local_mocked_bindings(
    comp_rmse = fake_comp_rmse,
    .package  = "sspLNIRT"
  )

  result <- do.call(optim_sample, valid_args(
    thresh  = 0.1,
    out.par = "alpha",
    range   = c(50, 100),
    verbose = FALSE
  ))

  expect_equal(result$N.min, "res.ub > thresh")
  expect_equal(result$trace$steps, 2)
  expect_equal(call_counter, 2L)
})

test_that("optim_sample mocked: verbose = TRUE emits messages during bisection", {
  scripted_rmse <- c(0.20, 0.05, 0.08, 0.15, 0.09, 0.12, 0.08, 0.09)
  call_counter  <- 0L
  fake_comp_rmse <- function(...) {
    call_counter <<- call_counter + 1L
    make_fake_comp_rmse(scripted_rmse[call_counter])
  }

  testthat::local_mocked_bindings(
    comp_rmse = fake_comp_rmse,
    .package  = "sspLNIRT"
  )

  expect_message(
    do.call(optim_sample, valid_args(
      thresh  = 0.1,
      out.par = "alpha",
      range   = c(50, 100),
      verbose = TRUE
    )),
    "Best result"
  )
})

test_that("optim_sample mocked: multi-parameter target works", {

    fake_comp_rmse <- function(...) {
    list(
      item = list(
        rmse       = c(alpha = 0.05, beta = 0.05),  # both < thresh
        mc.sd.rmse = c(alpha = 0.01, beta = 0.01),
        bias       = c(alpha = 0,    beta = 0)
      ),
      person = list(
        rmse       = c(theta = 0.1, zeta = 0.1),
        mc.sd.rmse = c(theta = 0.01, zeta = 0.01),
        bias       = c(theta = 0,    zeta = 0)
      ),
      rhat.dat = NULL, err.dat = NULL
    )
  }

  testthat::local_mocked_bindings(
    comp_rmse = fake_comp_rmse,
    .package  = "sspLNIRT"
  )

  result <- do.call(optim_sample, valid_args(
    thresh  = c(0.1, 0.1),
    out.par = c("alpha", "beta"),
    range   = c(50, 100),
    verbose = FALSE
  ))

  # lb (50) already passes for both params → res.lb < thresh early exit
  expect_equal(result$N.min, "res.lb < thresh")
  expect_named(result$res.best, c("alpha", "beta"))
})
