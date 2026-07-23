# Tests for rhat_LNIRT (internal)

# Helper
make_fake_lnirt <- function(XG = 200, burnin = 10,
                            n_items = 5, n_persons = 20, seed) {
  set.seed(seed)

  # D1: scalar hyper-parameters
  D1_names <- c(
    "Mu.Person.Ability", "Mu.Person.Speed",
    "Var.Person.Ability", "Var.Person.Speed", "Cov.Person.Ability.Speed",
    "Mu.Item.Discrimination", "Mu.Item.Difficulty",
    "Mu.Time.Discrimination", "Mu.Time.Intensity"
  )
  D1 <- stats::setNames(
    lapply(D1_names, function(.) stats::rnorm(XG)),
    D1_names
  )

  # D2.item: per-item parameters
  D2_item_names <- c("Item.Discrimination", "Item.Difficulty",
                     "Time.Discrimination", "Time.Intensity", "Sigma2")
  D2_item <- stats::setNames(
    lapply(D2_item_names, function(.) {
      matrix(stats::rnorm(XG * n_items), nrow = XG, ncol = n_items)
    }),
    D2_item_names
  )

  # D2.person: per-person parameters
  D2_person_names <- c("Person.Ability", "Person.Speed")
  D2_person <- stats::setNames(
    lapply(D2_person_names, function(.) {
      matrix(stats::rnorm(XG * n_persons), nrow = XG, ncol = n_persons)
    }),
    D2_person_names
  )

  # D3: CovMat.Item
  CovMat_Item <- array(stats::rnorm(XG * 4 * 4), dim = c(XG, 4, 4))

  MCMC.Samples <- c(D1, D2_item, D2_person,
                    list(CovMat.Item = CovMat_Item))

  list(
    XG           = XG,
    burnin       = burnin,
    MCMC.Samples = MCMC.Samples
  )
}

test_that("rhat_LNIRT returns expected structure with cutoff", {
  testthat::skip_if_not_installed("posterior")
  testthat::skip_if_not_installed("purrr")

  chains <- 4
  fits <- lapply(seq_len(chains), function(i) {
    make_fake_lnirt(seed = i)
  })

  out <- rhat_LNIRT(fits, chains = chains, cutoff = 1.05)

  expect_named(out, c("value", "convergence", "rate"))
  expect_named(out$value,       c("D1", "D2.item", "D2.person", "D3"))
  expect_named(out$convergence, c("D1", "D2.item", "D2.person", "D3"))
  expect_named(out$rate,        c("D1", "D2.item", "D2.person", "D3"))
})

test_that("rhat_LNIRT convergence indicators are 0/1 and rates are in [0, 1]", {
  testthat::skip_if_not_installed("posterior")
  testthat::skip_if_not_installed("purrr")

  chains <- 4
  fits <- lapply(seq_len(chains), function(i) make_fake_lnirt(seed = i))

  out <- rhat_LNIRT(fits, chains = chains, cutoff = 1.05)

  for (block in c("D1", "D2.item", "D2.person", "D3")) {
    expect_true(all(out$convergence[[block]] %in% c(0, 1)))
    expect_true(out$rate[[block]] >= 0 && out$rate[[block]] <= 1)
  }
})

test_that("rhat_LNIRT returns NULL convergence and rate when cutoff is NULL", {
  testthat::skip_if_not_installed("posterior")
  testthat::skip_if_not_installed("purrr")

  chains <- 4
  fits <- lapply(seq_len(chains), function(i) make_fake_lnirt(seed = i))

  out <- rhat_LNIRT(fits, chains = chains, cutoff = NULL)

  expect_null(out$convergence)
  expect_null(out$rate)
  expect_named(out$value, c("D1", "D2.item", "D2.person", "D3"))
})

test_that("rhat_LNIRT errors when fewer than 2 fits are supplied", {
  fits <- list(make_fake_lnirt(seed = 1))
  expect_error(rhat_LNIRT(fits, chains = 1, cutoff = 1.05))
})
