####  Figure generation for the manuscript

# install from GitHub
# devtools::install_github("anonymous-peer-2026/sspLNIRT", force = TRUE)
library(sspLNIRT)
library(ggplot2)
library(dplyr)
library(patchwork)
library(jtools)
library(tidyr)

#### input parameters
sim.mod.args <- list(
  K              = 30,
  mu.person      = c(0, 0),
  mu.item        = c(1, 0, .5, 1),
  meanlog.sigma2 = log(0.6),
  sdlog.sigma2   = 0,
  cov.m.person   = matrix(c(1,   0.4,
                            0.4, 1), ncol = 2, byrow = TRUE),
  cov.m.item     = matrix(c(1, 0,   0,   0,
                            0, 1,   0,   0.4,
                            0, 0,   1,   0,
                            0, 0.4, 0,   1), ncol = 4, byrow = TRUE),
  sd.item        = c(.2, 1, .2, .5),
  cor2cov.item   = TRUE
)

#### RA plot
set.seed(123)
RA.plot <- do.call(plot_RA, c(list(level = "item", by.theta = TRUE, N = 1e3),
                              sim.mod.args)) +
  jtools::theme_apa()
ggsave(
  filename = "data-raw/plots/RA.design.plot.pdf",
  plot     = RA.plot,
  width    = 180, height = 140, units = "mm",
  bg       = "white", dpi = 300
)

#### RT plot
set.seed(456)
RT.plot <- do.call(plot_RT, c(list(level = "item", logRT = FALSE, N = 1e3),
                              sim.mod.args)) +
  jtools::theme_apa()
ggsave(
  filename = "data-raw/plots/RT.design.plot.pdf",
  plot     = RT.plot,
  width    = 180, height = 140, units = "mm",
  bg       = "white", dpi = 300
)

#### SSP output
res_alpha <- get_sspLNIRT(
    thresh = c(0.10, 0.15, 0.05, 0.05),
    out.par = c("alpha", "beta", "phi", "lambda"),
    K = sim.mod.args$K,
    mu.alpha = sim.mod.args$mu.item[1],
    meanlog.sigma2 = sim.mod.args$meanlog.sigma2,
    rho = sim.mod.args$cov.m.person[1, 2]
)
summary(res_alpha$object)

res_final <- res_alpha

#### Power curve
power.plot <- plot(res_final$object, type = "power", thresh = 0.1, out.par = "alpha") +
  jtools::theme_apa()
ggsave(
  filename = "data-raw/plots/power.plot.pdf",
  plot     = power.plot,
  width    = 180, height = 80, units = "mm",
  bg       = "white", dpi = 300
)

#### Estimation plots (item)
est.item <- plot(res_final$object, pars = "item", y.val = "rmse") +
  jtools::theme_apa()
ggsave(
  filename = "data-raw/plots/est.item.plot.pdf",
  plot     = est.item,
  width    = 180, height = 100, units = "mm",
  bg       = "white", dpi = 300
)

#### Estimation plots (person)
est.person <- plot_estimation(res_final$object,
                              pars  = "person",
                              y.val = "rmse") +
  jtools::theme_apa()
ggsave(
  filename = "data-raw/plots/est.person.plot.pdf",
  plot     = est.person,
  width    = 180, height = 100, units = "mm",
  bg       = "white", dpi = 300
)


#### Sample-size grid plot across all conditions

N.dat <- as.data.frame(
  do.call(rbind, lapply(seq_len(2400), function(i) {
    cfg <- sspLNIRT.data$cfg[[i]]
    res <- sspLNIRT.data$res[[i]]
    cbind(
      par    = cfg$out.par,
      N      = res$N.min,
      thresh = cfg$thresh,
      K      = cfg$K,
      rho    = cfg$cov.m.person[1, 2],
      alpha  = cfg$mu.item[1],
      sigma2 = round(cfg$meanlog.sigma2, 2)
    )
  }))
)

N.dat <- N.dat %>%
  mutate(
    bound = case_when(
      N == "res.lb < thresh" ~ "floor",
      N == "res.ub > thresh" ~ "ceiling",
      TRUE                   ~ "interior"
    ),
    N = case_when(
      bound == "floor"   ~ 50,
      bound == "ceiling" ~ 2000,
      TRUE               ~ as.numeric(N)
    )
  )
num_cols <- c("N", "thresh", "K", "rho", "alpha", "sigma2")
N.dat[num_cols] <- lapply(N.dat[num_cols], as.numeric)

plot_N_grid <- function(data, thresh_filter = NULL) {
  plot_dat <- data %>%
    { if (!is.null(thresh_filter)) dplyr::filter(., thresh %in% thresh_filter) else . } %>%
    tidyr::pivot_longer(
      cols      = c(rho, alpha, sigma2, K),
      names_to  = "factor",
      values_to = "value"
    ) %>%
    dplyr::mutate(thresh = factor(thresh))

  pars    <- c("alpha", "beta", "phi", "lambda")
  factors <- c("rho", "alpha", "sigma2", "K")
  ylims <- list(
    alpha  = c(30, 2300), beta = c(30, 2300),
    phi    = c(30,  550), lambda = c(30,  550)
  )
  factor_labels <- c(
    rho = expression(rho), alpha = expression(alpha),
    sigma2 = expression(mu[sigma^2]), K = "K"
  )
  par_labels <- c(
    alpha = expression(alpha), beta = expression(beta),
    phi = expression(phi), lambda = expression(lambda)
  )

  make_plot <- function(p, f, show_legend = FALSE) {
    dat <- plot_dat %>% dplyr::filter(par == p, factor == f)
    row <- which(pars == p); col <- which(factors == f)
    is_left <- col == 1; is_bottom <- row == length(pars)
    pos <- if (show_legend) "bottom" else "none"

    summ <- dat %>%
      dplyr::group_by(thresh, value) %>%
      dplyr::summarise(
        p_ceil  = mean(bound == "ceiling"),
        p_floor = mean(bound == "floor"),
        med     = stats::median(N),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        value = factor(value, levels = sort(unique(value))),
        med_status = dplyr::case_when(
          p_ceil  >= 0.5 ~ "ceiling",
          p_floor >= 0.5 ~ "floor",
          TRUE           ~ "interior"
        ),
        med_line = dplyr::if_else(med_status == "interior", med, NA_real_)
      )

    interior  <- dplyr::filter(summ, med_status == "interior")
    ceil_med  <- dplyr::filter(summ, med_status == "ceiling") %>% dplyr::mutate(y = 2000)
    floor_med <- dplyr::filter(summ, med_status == "floor")   %>% dplyr::mutate(y = 50)

    ggplot(summ, aes(x = value, colour = thresh, group = thresh)) +
      geom_hline(yintercept = 50, linetype = "dotted", colour = "grey70", linewidth = 0.3) +
      { if (2000 <= ylims[[p]][2])
        geom_hline(yintercept = 2000, linetype = "dotted", colour = "grey70", linewidth = 0.3) } +
      geom_line(aes(y = med_line), linewidth = 1, na.rm = TRUE) +
      geom_point(data = interior, aes(y = med), size = 2, na.rm = TRUE) +
      geom_point(data = ceil_med,  aes(y = y, shape = "ceiling"),
                 colour = "grey30", fill = NA, size = 2.6, stroke = 0.7, na.rm = TRUE) +
      geom_point(data = floor_med, aes(y = y, shape = "floor"),
                 colour = "grey30", fill = NA, size = 2.6, stroke = 0.7, na.rm = TRUE) +
      scale_shape_manual(
        name   = "Censored median",
        values = c(ceiling = 24, floor = 25),
        labels = c(ceiling = expression(N^"*" > 2000),
                   floor   = expression(N^"*" < 50)),
        limits = c("ceiling", "floor"),
        breaks = c("ceiling", "floor"),
        drop   = FALSE
      ) +
      scale_colour_grey(start = 0.2, end = 0.8) +
      scale_y_continuous(breaks = c(50, 100, 200, 500, 1000, 2000)) +
      coord_trans(y = "log10", ylim = ylims[[p]]) +
      labs(
        x      = if (is_bottom) factor_labels[f] else NULL,
        y      = if (is_left) bquote("N*" ~ (.(par_labels[[p]]))) else NULL,
        colour = "RMSE threshold"
      ) +
      guides(
        colour = guide_legend(order = 1, nrow = 1, override.aes = list(shape = NA)),
        shape  = guide_legend(order = 2, override.aes = list(colour = "grey30"))
      ) +      jtools::theme_apa() +
      theme(
        legend.position = pos,
        axis.text.x  = if (is_bottom) element_text() else element_blank(),
        axis.ticks.x = if (is_bottom) element_line() else element_blank(),
        axis.text.y  = if (is_left)   element_text() else element_blank(),
        axis.ticks.y = if (is_left)   element_line() else element_blank()
      )
  }

  plots <- lapply(pars, function(p) lapply(factors, function(f) make_plot(p, f)))
  plots[[length(pars)]][[2]] <- make_plot(pars[length(pars)], factors[2], show_legend = TRUE)
  patchwork::wrap_plots(unlist(plots, recursive = FALSE),
                        ncol = length(factors), byrow = TRUE)
}

sample.size.plot <- plot_N_grid(N.dat, thresh_filter = c(0.05, 0.1, 0.15, 0.20))

nrow(N.dat)
N.dat %>% dplyr::count(bound)

ggsave(
  filename = "data-raw/plots/sample.size.plot.pdf",
  plot     = sample.size.plot,
  width    = 180, height = 180, units = "mm",
  bg       = "white", dpi = 300
)

####  Monte Carlo variability of RMSE
iter <- 200
out  <- readRDS("data-raw/results/mse.variance.no.seed.")

rmse.data <-
  as.data.frame(t(sapply(out, FUN = function(y) {
    cbind(y$item$rmse)
  })))
colnames(rmse.data) <- c("alpha", "beta", "phi", "lambda", "sigma2")

rmse.data <- tidyr::pivot_longer(rmse.data, cols = 1:4,
                                 names_to  = "parameter",
                                 values_to = "rmse")

sum.stats <- rmse.data %>%
  summarise(
    mu     = mean(rmse),
    sd     = sd(rmse),
    min    = min(rmse),
    max    = max(rmse),
    rel.sd = sd(rmse) / mean(rmse),
    .by    = c(parameter)
  )

saveRDS(sum.stats, "data-raw/results/rmse.variance.sum.stats")

sd.rmse.plot <- ggplot(rmse.data, aes(x = rmse, fill = parameter)) +
  geom_density(alpha = .5) +
  scale_fill_grey(start = 0.05, end = 0.95, name = "Parameter", labels = c(
    expression(alpha),
    expression(beta),
    expression(lambda),
    expression(phi)
  )) +
  xlab("RMSE") +
  ylab("Density") +
  xlim(c(0.025, .125)) +
  theme_apa() +
  theme(legend.position = c(.9, 0.5))

ggsave(
  filename = "data-raw/plots/sd.rmse.plot.pdf",
  plot     = sd.rmse.plot,
  width    = 180, height = 100, units = "mm",
  bg       = "white", dpi = 300
)
