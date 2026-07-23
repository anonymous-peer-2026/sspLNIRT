# tests/testthat/test-plot_sspLNIRT.R

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

make_trace_obj <- function(n_steps = 8, N.min = 200L) {
  N_vals   <- sort(sample(50:500, n_steps))
  res_vals <- exp(2 - 0.5 * log(N_vals)) + rnorm(n_steps, 0, 0.005)

  structure(
    list(
      N.min     = N.min,
      res.best  = c(alpha = tail(res_vals, 1)),
      comp.rmse = list(),
      trace     = list(
        steps     = n_steps,
        track.res = data.frame(
          res.lb.alpha   = res_vals * 1.1,
          res.ub.alpha   = res_vals * 0.9,
          res.temp.alpha = res_vals,
          mc.sd.alpha    = rep(0.01, n_steps)
        ),
        track.N = data.frame(
          N.lb   = N_vals - 10,
          N.ub   = N_vals + 10,
          N.temp = N_vals
        ),
        time.taken = as.difftime(1, units = "hours")
      )
    ),
    class = "sspLNIRT"
  )
}

test_that("plot() dispatches to plot.sspLNIRT for sspLNIRT objects", {
  skip_if_not_installed("ggplot2")

  obj <- structure(
    list(err.dat = make_binned_err()),
    class = "sspLNIRT"
  )

  p <- plot(obj)              # default type = "estimation"
  expect_s3_class(p, "gg")

  p2 <- plot(obj, type = "estimation", pars = "person", y.val = "bias")
  expect_s3_class(p2, "gg")
})

test_that("plot(type = 'power_curve') works on optim_sample-style objects", {
  skip_if_not_installed("ggplot2")

  obj <- make_trace_obj()
  p <- plot(obj, type = "power_curve", out.par = "alpha", thresh = 0.10)
  expect_s3_class(p, "gg")
})

test_that("plot(type = 'power_curve') auto-defaults out.par from res.best", {
  skip_if_not_installed("ggplot2")

  obj <- make_trace_obj()
  p <- plot(obj, type = "power_curve", thresh = 0.10)  # no out.par
  expect_s3_class(p, "gg")
})

test_that("plot.sspLNIRT rejects unknown type", {
  obj <- structure(list(err.dat = make_binned_err()), class = "sspLNIRT")
  expect_error(plot(obj, type = "wrong"), "should be one of")
})

test_that("plot.sspLNIRT warns when y is supplied", {
  skip_if_not_installed("ggplot2")
  obj <- structure(list(err.dat = make_binned_err()), class = "sspLNIRT")
  expect_warning(plot(obj, y = 1), "'y' is ignored")
})
