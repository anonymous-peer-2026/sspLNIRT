# tests/testthat/test-get_sspLNIRT.R

## Input validation
test_that("get_sspLNIRT validates thresh", {
  expect_error(get_sspLNIRT(thresh = -1, out.par = "alpha",
                            K = 30, mu.alpha = 1,
                            meanlog.sigma2 = log(0.6), rho = 0.2),
               "'thresh' must be positive")
  expect_error(get_sspLNIRT(thresh = "0.1", out.par = "alpha",
                            K = 30, mu.alpha = 1,
                            meanlog.sigma2 = log(0.6), rho = 0.2),
               "'thresh' must be positive")
})

test_that("get_sspLNIRT validates out.par", {
  expect_error(get_sspLNIRT(thresh = 0.1, out.par = 1,
                            K = 30, mu.alpha = 1,
                            meanlog.sigma2 = log(0.6), rho = 0.2),
               "'out.par' must be a character vector")
  expect_error(get_sspLNIRT(thresh = 0.1, out.par = "gamma",
                            K = 30, mu.alpha = 1,
                            meanlog.sigma2 = log(0.6), rho = 0.2),
               "'out.par' must be a subset of")
})

test_that("get_sspLNIRT validates thresh / out.par alignment", {
  expect_error(get_sspLNIRT(thresh = c(0.1, 0.2), out.par = "alpha",
                            K = 30, mu.alpha = 1,
                            meanlog.sigma2 = log(0.6), rho = 0.2),
               "'thresh' and 'out.par' must have the same length")
})

test_that("get_sspLNIRT validates K", {
  expect_error(get_sspLNIRT(thresh = 0.1, out.par = "alpha",
                            K = -1, mu.alpha = 1,
                            meanlog.sigma2 = log(0.6), rho = 0.2),
               "'K' must be a single positive integer")
  expect_error(get_sspLNIRT(thresh = 0.1, out.par = "alpha",
                            K = c(10, 30), mu.alpha = 1,
                            meanlog.sigma2 = log(0.6), rho = 0.2),
               "'K' must be a single positive integer")
})

test_that("get_sspLNIRT validates mu.alpha", {
  expect_error(get_sspLNIRT(thresh = 0.1, out.par = "alpha",
                            K = 30, mu.alpha = c(1, 2),
                            meanlog.sigma2 = log(0.6), rho = 0.2),
               "'mu.alpha' must be a single number")
})

test_that("get_sspLNIRT validates meanlog.sigma2", {
  expect_error(get_sspLNIRT(thresh = 0.1, out.par = "alpha",
                            K = 30, mu.alpha = 1,
                            meanlog.sigma2 = "a", rho = 0.2),
               "'meanlog.sigma2' must be a single number")
})

test_that("get_sspLNIRT validates rho", {
  expect_error(get_sspLNIRT(thresh = 0.1, out.par = "alpha",
                            K = 30, mu.alpha = 1,
                            meanlog.sigma2 = log(0.6), rho = 1.5),
               "'rho' must be a single number in \\[-1, 1\\]")
  expect_error(get_sspLNIRT(thresh = 0.1, out.par = "alpha",
                            K = 30, mu.alpha = 1,
                            meanlog.sigma2 = log(0.6), rho = c(0.2, 0.4)),
               "'rho' must be a single number in \\[-1, 1\\]")
})

## Structural tests with package data
test_that("get_sspLNIRT returns correct structure (single parameter)", {
  skip_if_not(exists("sspLNIRT.data", where = asNamespace(utils::packageName())),
              message = "sspLNIRT.data not available")

  result <- get_sspLNIRT(
    thresh         = 0.10,
    out.par        = "alpha",
    K              = 30,
    mu.alpha       = 0.6,
    meanlog.sigma2 = log(0.2),
    rho            = 0.2
  )

  expect_type(result, "list")
  expect_named(result, c("object", "design"))
  expect_s3_class(result$object, "sspLNIRT")
  expect_true("N.min" %in% names(result$object))
  expect_true("res.best" %in% names(result$object))
  expect_true("comp.rmse" %in% names(result$object))
  expect_true("trace" %in% names(result$object))
})

test_that("get_sspLNIRT returns bottleneck for multiple parameters", {
  skip_if_not(exists("sspLNIRT.data", where = asNamespace(utils::packageName())),
              message = "sspLNIRT.data not available")

  result <- get_sspLNIRT(
    thresh         = c(0.10, 0.10),
    out.par        = c("alpha", "beta"),
    K              = 30,
    mu.alpha       = 0.6,
    meanlog.sigma2 = log(0.2),
    rho            = 0.2
  )

  expect_type(result, "list")
  expect_named(result, c("object", "design"))

  expect_length(result$design$out.par, 1)
  expect_true(result$design$out.par %in% c("alpha", "beta"))
})

test_that("get_sspLNIRT errors informatively for unavailable config", {
  skip_if_not(exists("sspLNIRT.data", where = asNamespace(utils::packageName())),
              message = "sspLNIRT.data not available")

  expect_error(
    get_sspLNIRT(
      thresh         = 0.001,
      out.par        = "alpha",
      K              = 30,
      mu.alpha       = 0.6,
      meanlog.sigma2 = log(0.2),
      rho            = 0.2
    ),
    "No matching configuration found"
  )
})

## Bottleneck branch tests
# Helper
make_mock_data <- function(N_min_values, out_pars) {
  stopifnot(length(N_min_values) == length(out_pars))

  cfgs <- lapply(seq_along(out_pars), function(i) {
    list(
      thresh         = 0.10,
      out.par        = out_pars[i],
      K              = 30,
      mu.item        = c(0.6, 0, 0.5, 1),
      meanlog.sigma2 = log(0.2),
      cov.m.person   = matrix(c(1, 0.2, 0.2, 1), ncol = 2)
    )
  })

  ress <- lapply(N_min_values, function(nm) {
    list(
      N.min     = nm,
      res.best  = c(setNames(0.09, "alpha")),
      comp.rmse = list(),
      trace     = list(steps = 1)
    )
  })

  data.frame(
    cfg = I(cfgs),
    res = I(ress),
    stringsAsFactors = FALSE
  )
}

# Helper
call_get_sspLNIRT_with_mock <- function(mock_data, ...) {
  pkg_name <- utils::packageName()
  pkg_ns   <- asNamespace(pkg_name)

  fn_local <- get("get_sspLNIRT", envir = pkg_ns)

  mask_env <- new.env(parent = environment(fn_local))
  assign("sspLNIRT.data", mock_data, envir = mask_env)

  environment(fn_local) <- mask_env
  fn_local(...)
}

test_that("get_sspLNIRT returns 'res.ub > thresh' bottleneck when present", {
  mock_data <- make_mock_data(
    N_min_values = list(150L, "res.ub > thresh"),
    out_pars     = c("alpha", "beta")
  )

  result <- call_get_sspLNIRT_with_mock(
    mock_data,
    thresh         = c(0.10, 0.10),
    out.par        = c("alpha", "beta"),
    K              = 30,
    mu.alpha       = 0.6,
    meanlog.sigma2 = log(0.2),
    rho            = 0.2
  )

  expect_identical(result$object$N.min, "res.ub > thresh")
})

test_that("get_sspLNIRT returns first 'res.lb < thresh' when no numeric/ub", {
  mock_data <- make_mock_data(
    N_min_values = list("res.lb < thresh", "res.lb < thresh"),
    out_pars     = c("alpha", "beta")
  )

  result <- call_get_sspLNIRT_with_mock(
    mock_data,
    thresh         = c(0.10, 0.10),
    out.par        = c("alpha", "beta"),
    K              = 30,
    mu.alpha       = 0.6,
    meanlog.sigma2 = log(0.2),
    rho            = 0.2
  )

  expect_identical(result$object$N.min, "res.lb < thresh")
})

test_that("values_match handles NULL config fields (early-return branch)", {
  cfg_with_null <- list(
    thresh         = 0.10,
    out.par        = "alpha",
    K              = 30,
    mu.item        = c(0.6, 0, 0.5, 1),
    meanlog.sigma2 = NULL,                  # triggers is.null(a) in values_match
    cov.m.person   = matrix(c(1, 0.2, 0.2, 1), ncol = 2)
  )
  res <- list(
    N.min     = 100L,
    res.best  = c(setNames(0.09, "alpha")),
    comp.rmse = list(),
    trace     = list(steps = 1)
  )
  mock_data <- data.frame(
    cfg = I(list(cfg_with_null)),
    res = I(list(res)),
    stringsAsFactors = FALSE
  )

  expect_error(
    call_get_sspLNIRT_with_mock(
      mock_data,
      thresh         = 0.10,
      out.par        = "alpha",
      K              = 30,
      mu.alpha       = 0.6,
      meanlog.sigma2 = log(0.2),
      rho            = 0.2
    ),
    "No matching configuration found"
  )
})
