# Tests for scale_M (internal)

# Helper
make_inputs <- function(I = 10, N = 20, seed = 42) {
  set.seed(seed)
  item.pars <- data.frame(
    alpha  = stats::rlnorm(I, meanlog = 0,    sdlog = 0.2),
    beta   = stats::rnorm(I,  mean    = 0,    sd    = 1),
    phi    = stats::rlnorm(I, meanlog = -0.7, sdlog = 0.2),
    lambda = stats::rnorm(I,  mean    = 1,    sd    = 0.5),
    sigma2 = stats::rlnorm(I, meanlog = -0.5, sdlog = 0.1)
  )
  person.pars <- data.frame(
    theta = stats::rnorm(N, 0, 1),
    zeta  = stats::rnorm(N, 0, 1)
  )
  list(item.pars = item.pars, person.pars = person.pars)
}

test_that("scale_M (forward) returns the expected list structure", {
  inp <- make_inputs()
  out <- scale_M(inp$item.pars, inp$person.pars, re.scale = FALSE)

  expect_named(out, c("items.pars.scaled", "person.pars.scaled",
                      "c.alpha", "c.phi"))
  expect_s3_class(out$items.pars.scaled,  "data.frame")
  expect_s3_class(out$person.pars.scaled, "data.frame")
  expect_equal(dim(out$items.pars.scaled),  dim(inp$item.pars))
  expect_equal(dim(out$person.pars.scaled), dim(inp$person.pars))
  expect_true(is.numeric(out$c.alpha) && out$c.alpha > 0)
  expect_true(is.numeric(out$c.phi)   && out$c.phi   > 0)
})

test_that("scale_M (forward) constrains geometric means of alpha and phi to 1", {
  inp <- make_inputs()
  out <- scale_M(inp$item.pars, inp$person.pars, re.scale = FALSE)

  I <- nrow(inp$item.pars)
  expect_equal(prod(out$items.pars.scaled$alpha)^(1 / I), 1, tolerance = 1e-10)
  expect_equal(prod(out$items.pars.scaled$phi)^(1 / I),   1, tolerance = 1e-10)
})

test_that("scale_M (forward) returns scaling constants matching inputs", {
  inp <- make_inputs()
  out <- scale_M(inp$item.pars, inp$person.pars, re.scale = FALSE)
  I <- nrow(inp$item.pars)

  expect_equal(out$c.alpha, prod(inp$item.pars$alpha)^(1 / I))
  expect_equal(out$c.phi,   prod(inp$item.pars$phi)^(1 / I))
})

test_that("scale_M round-trip (forward then inverse) recovers the original", {
  inp <- make_inputs()
  fwd <- scale_M(inp$item.pars, inp$person.pars, re.scale = FALSE)
  inv <- scale_M(fwd$items.pars.scaled, fwd$person.pars.scaled,
                 re.scale = TRUE,
                 c.alpha  = fwd$c.alpha,
                 c.phi    = fwd$c.phi)

  expect_equal(inv$items.pars.scaled,  inp$item.pars,
               tolerance = 1e-10, ignore_attr = TRUE)
  expect_equal(inv$person.pars.scaled, inp$person.pars,
               tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("scale_M (forward) leaves lambda and sigma2 columns unchanged", {
  inp <- make_inputs()
  out <- scale_M(inp$item.pars, inp$person.pars, re.scale = FALSE)

  expect_equal(out$items.pars.scaled$lambda, inp$item.pars$lambda)
  expect_equal(out$items.pars.scaled$sigma2, inp$item.pars$sigma2)
})
