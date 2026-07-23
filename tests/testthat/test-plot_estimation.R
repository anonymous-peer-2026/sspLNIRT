# tests/testthat/test-plot_estimation.R

## Helper: minimal binned error data

make_binned_err <- function() {
  item <- data.frame(
    par       = rep(c("alpha", "beta", "phi", "lambda", "sigma2"), each = 5),
    bin       = rep(1:5, times = 5),
    mean_sim  = rep(seq(0.5, 1.5, length.out = 5), times = 5),
    mean_err  = rnorm(25, 0, 0.01),
    mean_rmse = runif(25, 0.02, 0.10)
  )
  person <- data.frame(
    par       = rep(c("theta", "zeta"), each = 5),
    bin       = rep(1:5, times = 2),
    mean_sim  = rep(seq(-1, 1, length.out = 5), times = 2),
    mean_err  = rnorm(10, 0, 0.01),
    mean_rmse = runif(10, 0.20, 0.35)
  )
  list(person = person, item = item)
}

## Tests

test_that("plot_estimation works with comp_rmse-style object (binned)", {
  skip_if_not_installed("ggplot2")

  obj <- structure(
    list(
      person   = list(rmse = c(theta = 0.3)),
      item     = list(rmse = c(alpha = 0.1)),
      rhat.dat = NULL,
      err.dat  = make_binned_err()
    ),
    class = "sspLNIRT"
  )

  p <- plot_estimation(obj, pars = "item", y.val = "rmse")
  expect_s3_class(p, "gg")

  p2 <- plot_estimation(obj, pars = "person", y.val = "bias")
  expect_s3_class(p2, "gg")
})

test_that("plot_estimation works with optim_sample-style object", {
  skip_if_not_installed("ggplot2")

  obj <- structure(
    list(
      N.min     = 200L,
      res.best  = c(alpha = 0.09),
      comp.rmse = list(
        person = list(rmse = c(theta = 0.3)),
        item   = list(rmse = c(alpha = 0.1)),
        err.dat = make_binned_err()
      ),
      trace = list(steps = 5)
    ),
    class = "sspLNIRT"
  )

  p <- plot_estimation(obj, pars = "item", y.val = "rmse")
  expect_s3_class(p, "gg")
})

test_that("plot_estimation excludes sigma2 for item plots", {
  skip_if_not_installed("ggplot2")

  obj <- structure(
    list(err.dat = make_binned_err()),
    class = "sspLNIRT"
  )

  p <- plot_estimation(obj, pars = "item", y.val = "rmse")
  plot_data <- ggplot2::ggplot_build(p)$data[[2]]  # points layer

  facet_pars <- unique(p$data$par)
  expect_false("sigma2" %in% facet_pars)
})

test_that("plot_estimation validates inputs", {
  skip_if_not_installed("ggplot2")

  expect_error(plot_estimation(list(a = 1)),
               "'object' must be an sspLNIRT object")

  obj <- structure(
    list(err.dat = make_binned_err()),
    class = "sspLNIRT"
  )

  expect_error(plot_estimation(obj, n.bins = 1),
               "'n.bins' must be a single integer >= 2")
  expect_error(plot_estimation(obj, pars = "wrong"),
               "'arg' should be one of")
  expect_error(plot_estimation(obj, y.val = "wrong"),
               "'arg' should be one of")
})

test_that("plot_estimation handles full (unbinned) error data", {
  skip_if_not_installed("ggplot2")

  set.seed(1)
  full_item <- data.frame(
    rep     = rep(1:2, each = 20),
    par     = rep(c("alpha", "beta"), each = 10, times = 2),
    sim.val = rnorm(40),
    err     = rnorm(40, 0, 0.1)
  )
  full_person <- data.frame(
    rep     = rep(1:2, each = 20),
    par     = rep(c("theta", "zeta"), each = 10, times = 2),
    sim.val = rnorm(40),
    err     = rnorm(40, 0, 0.2)
  )

  obj <- structure(
    list(err.dat = list(person = full_person, item = full_item)),
    class = "sspLNIRT"
  )

  p <- plot_estimation(obj, pars = "item", y.val = "rmse", n.bins = 5)
  expect_s3_class(p, "gg")
})

test_that("plot_estimation errors when no comp.rmse and no err.dat", {
  obj <- structure(
    list(person = list(rmse = c(theta = 0.3))),
    class = "sspLNIRT"
  )

  expect_error(plot_estimation(obj, pars = "item", y.val = "rmse"),
               "'object' must contain either \\$comp.rmse or \\$err.dat")
})

test_that("plot_estimation errors when err.dat lacks requested pars", {
  obj <- structure(
    list(err.dat = list(person = make_binned_err()$person)),
    class = "sspLNIRT"
  )

  expect_error(plot_estimation(obj, pars = "item", y.val = "rmse"),
               "No error data found for pars = 'item'")
})

test_that("plot_estimation errors on unrecognized error data format", {
  bad_dat <- data.frame(foo = 1:5, bar = 6:10)

  obj <- structure(
    list(err.dat = list(person = bad_dat, item = bad_dat)),
    class = "sspLNIRT"
  )

  expect_error(plot_estimation(obj, pars = "item", y.val = "rmse"),
               "Unrecognized error data format")
})
