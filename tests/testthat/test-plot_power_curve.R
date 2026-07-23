# tests/testthat/test-plot_power_curve.R

## Helper: minimal optim_sample-style object with trace

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

## Tests

test_that("plot_power_curve returns a ggplot for valid input", {
  skip_if_not_installed("ggplot2")

  obj <- make_trace_obj()
  p <- plot_power_curve(obj, out.par = "alpha", thresh = 0.10)

  expect_s3_class(p, "gg")
})

test_that("plot_power_curve handles character N.min (boundary case)", {
  skip_if_not_installed("ggplot2")

  obj <- make_trace_obj(n_steps = 5, N.min = "res.ub > thresh")
  p <- plot_power_curve(obj, out.par = "alpha", thresh = 0.10)

  expect_s3_class(p, "gg")
  # caption should contain the character N.min
  expect_true(grepl("res.ub > thresh", p$labels$caption))
})

test_that("plot_power_curve validates out.par", {
  obj <- make_trace_obj()

  expect_error(plot_power_curve(obj, out.par = "gamma", thresh = 0.1),
               "'out.par' must be one of")
  expect_error(plot_power_curve(obj, out.par = c("alpha", "beta"), thresh = 0.1),
               "'out.par' must be a single character string")
})

test_that("plot_power_curve validates thresh", {
  obj <- make_trace_obj()

  expect_error(plot_power_curve(obj, out.par = "alpha", thresh = -1),
               "'thresh' must be a single positive number")
  expect_error(plot_power_curve(obj, out.par = "alpha", thresh = "0.1"),
               "'thresh' must be a single positive number")
})

test_that("plot_power_curve requires trace element", {
  obj <- structure(list(N.min = 200L), class = "sspLNIRT")

  expect_error(plot_power_curve(obj, out.par = "alpha", thresh = 0.1),
               "must be from optim_sample\\(\\) containing a \\$trace element")
})

test_that("plot_power_curve errors on too few trace points", {
  obj <- make_trace_obj(n_steps = 2)

  expect_error(plot_power_curve(obj, out.par = "alpha", thresh = 0.1),
               "Cannot fit power curve")
})

test_that("plot_power_curve errors for missing out.par column", {
  obj <- make_trace_obj()

  expect_error(plot_power_curve(obj, out.par = "beta", thresh = 0.1),
               "No results found for out.par = 'beta'")
})

test_that("plot_power_curve rejects non-sspLNIRT object", {
  expect_error(plot_power_curve(list(a = 1), out.par = "alpha", thresh = 0.1),
               "'object' must be an sspLNIRT object from optim_sample")
})

test_that("plot_power_curve infers out.par from res.best when NULL", {
  skip_if_not_installed("ggplot2")

  obj <- make_trace_obj()
  p <- plot_power_curve(obj, out.par = NULL, thresh = 0.10)
  expect_s3_class(p, "gg")
})

test_that("plot_power_curve errors when out.par cannot be inferred", {
  obj <- make_trace_obj()
  # remove names so inference fails
  obj$res.best <- unname(obj$res.best)

  expect_error(plot_power_curve(obj, out.par = NULL, thresh = 0.10),
               "'out.par' is missing and could not be inferred")

  obj2 <- make_trace_obj()
  obj2$res.best <- numeric(0)

  expect_error(plot_power_curve(obj2, out.par = NULL, thresh = 0.10),
               "'out.par' is missing and could not be inferred")
})

test_that("plot_power_curve errors when track.N or track.res is missing", {
  obj <- make_trace_obj()
  obj$trace$track.N <- NULL

  expect_error(plot_power_curve(obj, out.par = "alpha", thresh = 0.10),
               "must contain \\$track.N and \\$track.res")

  obj2 <- make_trace_obj()
  obj2$trace$track.res <- NULL

  expect_error(plot_power_curve(obj2, out.par = "alpha", thresh = 0.10),
               "must contain \\$track.N and \\$track.res")
})

test_that("plot_power_curve errors when N and res lengths mismatch", {
  obj <- make_trace_obj(n_steps = 8)
  # Truncate N to create a length mismatch
  obj$trace$track.N <- obj$trace$track.N[seq_len(5), ]

  expect_error(plot_power_curve(obj, out.par = "alpha", thresh = 0.10),
               "Lengths of tracked N and result vectors do not match")
})

test_that("plot_power_curve errors when fewer than 3 positive trace points", {
  obj <- make_trace_obj(n_steps = 5)
  # Set most res values to 0 so they get filtered out
  obj$trace$track.res$res.temp.alpha[1:4] <- 0

  expect_error(plot_power_curve(obj, out.par = "alpha", thresh = 0.10),
               "Fewer than 3 valid \\(positive\\) trace points")
})
