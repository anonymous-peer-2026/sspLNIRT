# tests/testthat/test-plot_RT.R

test_that("plot_RT returns ggplot for person level, seconds", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tmvtnorm")
  skip_if_not_installed("MASS")

  p <- plot_RT(level = "person", logRT = FALSE, N = 50, K = 5)
  expect_s3_class(p, "gg")
})

test_that("plot_RT returns ggplot for person level, log", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tmvtnorm")
  skip_if_not_installed("MASS")

  p <- plot_RT(level = "person", logRT = TRUE, N = 50, K = 5)
  expect_s3_class(p, "gg")
})

test_that("plot_RT returns ggplot for item level, seconds", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tmvtnorm")
  skip_if_not_installed("MASS")

  p <- plot_RT(level = "item", logRT = FALSE, N = 50, K = 3)
  expect_s3_class(p, "gg")
})

test_that("plot_RT returns ggplot for item level, log", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tmvtnorm")
  skip_if_not_installed("MASS")

  p <- plot_RT(level = "item", logRT = TRUE, N = 50, K = 3)
  expect_s3_class(p, "gg")
})

test_that("plot_RT returns ggplot for item level overlay, seconds", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tmvtnorm")
  skip_if_not_installed("MASS")

  p <- plot_RT(level = "item", logRT = FALSE, overlay = TRUE, N = 50, K = 3)
  expect_s3_class(p, "gg")
})

test_that("plot_RT returns ggplot for item level overlay, log", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tmvtnorm")
  skip_if_not_installed("MASS")

  p <- plot_RT(level = "item", logRT = TRUE, overlay = TRUE, N = 50, K = 3)
  expect_s3_class(p, "gg")
})

test_that("plot_RT overlay maps colour to item and drops the facets", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tmvtnorm")
  skip_if_not_installed("MASS")

  p <- plot_RT(level = "item", logRT = TRUE, overlay = TRUE, N = 50, K = 3)

  expect_true("colour" %in% names(p$mapping))
  expect_s3_class(p$facet, "FacetNull")

  p_facet <- plot_RT(level = "item", logRT = TRUE, N = 50, K = 3)
  expect_s3_class(p_facet$facet, "FacetWrap")
})

test_that("plot_RT overlay drops the rug and the median lines", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tmvtnorm")
  skip_if_not_installed("MASS")

  p <- plot_RT(level = "item", logRT = TRUE, overlay = TRUE, N = 50, K = 3)
  expect_length(p$layers, 1L)

  p_facet <- plot_RT(level = "item", logRT = TRUE, N = 50, K = 3)
  expect_gt(length(p_facet$layers), 1L)
})

test_that("plot_RT overlay handles more items than palette anchors", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tmvtnorm")
  skip_if_not_installed("MASS")

  p <- plot_RT(level = "item", logRT = TRUE, overlay = TRUE, N = 50, K = 8)
  expect_s3_class(p, "gg")

  p <- plot_RT(level = "item", logRT = FALSE, overlay = TRUE, N = 50, K = 8)
  expect_s3_class(p, "gg")
})

test_that("plot_RT overlay suppresses the legend beyond 12 items", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tmvtnorm")
  skip_if_not_installed("MASS")

  p_small <- plot_RT(level = "item", logRT = TRUE, overlay = TRUE,
                     N = 50, K = 5)
  expect_identical(p_small$theme$legend.position, "right")

  p_large <- plot_RT(level = "item", logRT = TRUE, overlay = TRUE,
                     N = 50, K = 15)
  expect_identical(p_large$theme$legend.position, "none")
})

test_that("plot_RT ignores overlay at person level", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tmvtnorm")
  skip_if_not_installed("MASS")

  p_on  <- plot_RT(level = "person", logRT = TRUE, overlay = TRUE,
                   N = 50, K = 5)
  p_off <- plot_RT(level = "person", logRT = TRUE, overlay = FALSE,
                   N = 50, K = 5)

  expect_s3_class(p_on, "gg")
  expect_identical(length(p_on$layers), length(p_off$layers))
})

test_that("plot_RT validates inputs", {
  expect_error(plot_RT(level = "wrong"), "'arg' should be one of")
  expect_error(plot_RT(level = "person", logRT = "yes"),
               "'logRT' must be logical")
  expect_error(plot_RT(level = "person", K = 0),
               "'K' must be a positive integer")
  expect_error(plot_RT(level = "person", mu.person = c(0, 0, 0)),
               "'mu.person' must be length 2")
  expect_error(plot_RT(level = "person", cor2cov.item = TRUE, sd.item = NULL),
               "'sd.item' must be provided")
})

test_that("plot_RT validates overlay", {
  expect_error(plot_RT(level = "person", overlay = "yes"),
               "'overlay' must be a single logical value")
  expect_error(plot_RT(level = "person", overlay = c(TRUE, FALSE)),
               "'overlay' must be a single logical value")
  expect_error(plot_RT(level = "person", overlay = NA),
               "'overlay' must be a single logical value")
  expect_error(plot_RT(level = "person", overlay = NULL),
               "'overlay' must be a single logical value")
})

test_that("plot_RT validates remaining input arguments", {
  expect_error(plot_RT(level = "person", mu.item = c(1, 0, 0.5)),
               "'mu.item' must be length 4")

  expect_error(plot_RT(level = "person", cov.m.person = c(1, 0.4, 0.4, 1)),
               "'cov.m.person' must be a 2x2 matrix")
  expect_error(plot_RT(level = "person", cov.m.person = diag(3)),
               "'cov.m.person' must be a 2x2 matrix")

  expect_error(plot_RT(level = "person", cov.m.item = c(1, 0)),
               "'cov.m.item' must be a 4x4 matrix")
  expect_error(plot_RT(level = "person", cov.m.item = diag(3)),
               "'cov.m.item' must be a 4x4 matrix")

  expect_error(
    plot_RT(level = "person",
            cov.m.person = matrix(c(1, 0.4, 0.5, 1), ncol = 2, byrow = TRUE)),
    "'cov.m.person' must be symmetric"
  )

  asym_cov_item <- matrix(c(1, 0,   0,   0,
                            0, 1,   0,   0.4,
                            0, 0,   1,   0,
                            0, 0.5, 0,   1), ncol = 4, byrow = TRUE)
  expect_error(
    plot_RT(level = "person", cov.m.item = asym_cov_item),
    "'cov.m.item' must be symmetric"
  )

  expect_error(plot_RT(level = "person", sd.item = c(0.2, 1, 0.2)),
               "'sd.item' must be length 4")

  expect_error(plot_RT(level = "person", item.pars.m = c(1, 2, 3, 4)),
               "'item.pars.m' must be a matrix with 4 columns")
  expect_error(plot_RT(level = "person", K = 5,
                       item.pars.m = matrix(0, nrow = 5, ncol = 3)),
               "'item.pars.m' must be a matrix with 4 columns")

  expect_error(plot_RT(level = "person", K = 5,
                       item.pars.m = matrix(0, nrow = 3, ncol = 4)),
               "'item.pars.m' must have K rows")
})

test_that("plot_RT accepts a design list and uses its fields", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tmvtnorm")
  skip_if_not_installed("MASS")

  design <- list(
    K              = 5,
    mu.person      = c(0, 0),
    mu.item        = c(1, 0, 0.5, 1),
    meanlog.sigma2 = log(0.6),
    cov.m.person   = matrix(c(1, 0.4, 0.4, 1), ncol = 2),
    cov.m.item     = matrix(c(1, 0,   0,   0,
                              0, 1,   0,   0.4,
                              0, 0,   1,   0,
                              0, 0.4, 0,   1), ncol = 4, byrow = TRUE),
    sd.item        = c(0.2, 1, 0.2, 0.5),
    sdlog.sigma2   = 0,
    cor2cov.item   = TRUE,
    thresh         = 0.1,
    out.par        = "alpha"
  )

  p <- plot_RT(design = design, level = "person", logRT = FALSE, N = 50)
  expect_s3_class(p, "gg")
})

test_that("plot_RT: design is not consulted for overlay", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tmvtnorm")
  skip_if_not_installed("MASS")

  design <- list(K = 3, overlay = TRUE)
  p <- plot_RT(design = design, level = "item", logRT = TRUE, N = 50)

  expect_s3_class(p$facet, "FacetWrap")
})

test_that("plot_RT: explicit argument overrides design field", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tmvtnorm")
  skip_if_not_installed("MASS")

  design <- list(mu.person = c(0, 0, 0))
  p <- plot_RT(design = design,
               level = "person", logRT = FALSE,
               N = 50, K = 5, mu.person = c(0, 0))
  expect_s3_class(p, "gg")
})

test_that("plot_RT: design field is used when caller omits it", {
  design <- list(mu.person = c(0, 0, 0))
  expect_error(
    plot_RT(design = design, level = "person", logRT = FALSE,
            N = 50, K = 5),
    "'mu.person' must be length 2"
  )
})

test_that("plot_RT: errors if design is not a list", {
  expect_error(plot_RT(design = "not a list", level = "person"),
               "'design' must be a list")
})
