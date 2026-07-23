# tests/testthat/test-theme_sspLNIRT.R

test_that("theme_sspLNIRT returns a ggplot theme object", {
  skip_if_not_installed("ggplot2")
  th <- theme_sspLNIRT()
  expect_s3_class(th, "theme")
  expect_true(attr(th, "complete") || !is.null(th$panel.grid.minor))
})

test_that("theme_sspLNIRT respects base_size argument", {
  skip_if_not_installed("ggplot2")
  th <- theme_sspLNIRT(base_size = 14)
  expect_s3_class(th, "theme")
})

test_that("scale_colour_sspLNIRT and scale_fill_sspLNIRT are usable in plots", {
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(iris,
                       ggplot2::aes(Sepal.Length, Sepal.Width,
                                    colour = Species, fill = Species)) +
    ggplot2::geom_point() +
    scale_colour_sspLNIRT() +
    scale_fill_sspLNIRT() +
    theme_sspLNIRT()

  expect_s3_class(p, "gg")
})

test_that("sspLNIRT_palette interpolates when n exceeds anchor count", {
  pal_small <- sspLNIRT_palette(3)
  expect_length(pal_small, 3)
  expect_true(all(grepl("^#", pal_small)))

  pal_large <- sspLNIRT_palette(10)
  expect_length(pal_large, 10)
  expect_true(all(grepl("^#", pal_large)))
})
