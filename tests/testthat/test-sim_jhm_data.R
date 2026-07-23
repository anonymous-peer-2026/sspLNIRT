# Tests for sim_jhm_data (internal)

test_that("sim.jhm.data returns correctly structured output", {
  set.seed(123)

  iter <- 2
  N <- 50
  K <- 10

  out <- sim.jhm.data(
    iter = iter,
    N = N,
    K = K,
    mu.person = c(0, 0),
    mu.item = c(1, 0, 1, 4),
    meanlog.sigma2 = -0.5,
    cov.m.person = matrix(c(1, 0.3,
                            0.3, 1), 2, 2),
    cov.m.item = diag(4),
    sdlog.sigma2 = 0.2,
    item.pars.m = NULL,
    cor2cov.item = FALSE,
    sd.item = NULL,
    scale = TRUE
  )

  # top-level structure
  expect_type(out, "list")
  expect_named(out,
               c("time.data",
                 "response.data",
                 "person.par",
                 "item.par",
                 "scale.factor"))

  # correct list lengths
  expect_length(out$time.data, iter)
  expect_length(out$response.data, iter)
  expect_length(out$person.par, iter)
  expect_length(out$item.par, iter)
  expect_length(out$scale.factor, iter)

  # inspect first replication
  expect_equal(dim(out$time.data[[1]]), c(N, K))
  expect_equal(dim(out$response.data[[1]]), c(N, K))

  # response matrix should be binary
  expect_true(all(out$response.data[[1]] %in% c(0, 1)))

  # item parameter structure
  expect_s3_class(out$item.par[[1]], "data.frame")
  expect_named(out$item.par[[1]],
               c("alpha", "beta", "phi", "lambda", "sigma2"))

  expect_equal(nrow(out$item.par[[1]]), K)

  # person parameter structure
  expect_s3_class(out$person.par[[1]], "data.frame")
  expect_named(out$person.par[[1]],
               c("theta", "zeta"))

  expect_equal(nrow(out$person.par[[1]]), N)

  # scale factor structure
  expect_s3_class(out$scale.factor[[1]], "data.frame")
  expect_named(out$scale.factor[[1]],
               c("c.alpha", "c.phi"))

  # no missing values
  expect_false(anyNA(out$time.data[[1]]))
  expect_false(anyNA(out$response.data[[1]]))
  expect_false(anyNA(out$item.par[[1]]))
  expect_false(anyNA(out$person.par[[1]]))
})

test_that("sim.jhm.data respects fixed item parameters", {
  set.seed(123)

  K <- 5

  fixed_items <- data.frame(
    alpha = rep(1, K),
    beta = seq(-1, 1, length.out = K),
    phi = rep(0.5, K),
    lambda = rep(4, K),
    sigma2 = rep(0.2, K)
  )

  out <- sim.jhm.data(
    iter = 2,
    N = 20,
    K = K,
    mu.person = c(0, 0),
    mu.item = c(1, 0, 1, 4),
    meanlog.sigma2 = -0.5,
    cov.m.person = diag(2),
    cov.m.item = diag(4),
    sdlog.sigma2 = 0.2,
    item.pars.m = fixed_items,
    cor2cov.item = FALSE,
    sd.item = NULL,
    scale = FALSE
  )

  expect_equal(out$item.par[[1]], fixed_items)
  expect_equal(out$item.par[[2]], fixed_items)

  # scale factors should be NA when scale = FALSE
  expect_true(all(is.na(out$scale.factor[[1]])))
  expect_true(all(is.na(out$scale.factor[[2]])))
})

test_that("sim.jhm.data is reproducible with fixed seed", {
  set.seed(999)

  args <- list(
    iter = 1,
    N = 25,
    K = 6,
    mu.person = c(0, 0),
    mu.item = c(1, 0, 1, 4),
    meanlog.sigma2 = -0.5,
    cov.m.person = diag(2),
    cov.m.item = diag(4),
    sdlog.sigma2 = 0.2,
    item.pars.m = NULL,
    cor2cov.item = FALSE,
    sd.item = NULL,
    scale = TRUE
  )

  out1 <- do.call(sim.jhm.data, args)

  set.seed(999)

  out2 <- do.call(sim.jhm.data, args)

  expect_equal(out1, out2)
})
