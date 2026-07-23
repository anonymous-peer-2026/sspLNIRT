# Script to create the tibble `sspLNIRT.data` from the precomputed results

# libraries
library(tibble)
library(dplyr)
library(sspLNIRT)

# load the batches
save.dir <- "./data-raw/results/"

files <- list.files(
  path = save.dir,
  pattern = "^batch_[0-9]{3}\\.rds$",
  full.names = TRUE
)

# numeric batch id
batch_id <- as.integer(sub("^batch_([0-9]{3})\\.rds$", "\\1", basename(files)))

# read all batches into a named list
batch.list <- setNames(lapply(files, readRDS), sprintf("batch_%03d", batch_id))

# combine the batches into one data frame
rows <- lapply(names(batch.list), function(bn) {
  batch <- batch.list[[bn]]
  lapply(seq_along(batch), function(i) {
    res <- batch[[i]]$res
    cfg <- batch[[i]]$args
    class(cfg) <- "sspLNIRT.design"

    if (grepl("thresh", res$N.min)) {
      res[["N.curve"]] <- NULL
    }
    alpha.idx <- grepl("Item.Discrimination", rownames(res$comp.rmse$rhat.dat))
    beta.idx <- grepl("Item.Difficulty", rownames(res$comp.rmse$rhat.dat))
    phi.idx <- grepl("Time.Discrimination", rownames(res$comp.rmse$rhat.dat))
    lambda.idx <- grepl("Time.Intensity", rownames(res$comp.rmse$rhat.dat))
    sigma2.idx <- grepl("Sigma", rownames(res$comp.rmse$rhat.dat))
    res$comp.rmse$rhat.dat <- data.frame(
      alpha = quantile(c(subset(res$comp.rmse$rhat.dat, alpha.idx)), c(.5, .8, .9, .95)),
      beta = quantile(c(subset(res$comp.rmse$rhat.dat, beta.idx)), c(.5, .8, .9, .95)),
      phi = quantile(c(subset(res$comp.rmse$rhat.dat, phi.idx)), c(.5, .8, .9, .95)),
      lambda = quantile(c(subset(res$comp.rmse$rhat.dat, phi.idx)), c(.5, .8, .9, .95)),
      sigma2 = quantile(c(subset(res$comp.rmse$rhat.dat, lambda.idx)), c(.5, .8, .9, .95)))

    colnames(res$trace$track.res)[1] <- paste0("res.lb.", cfg$out.par)
    colnames(res$trace$track.res)[2] <- paste0("res.ub.", cfg$out.par)
    colnames(res$trace$track.res)[3] <- paste0("res.temp.", cfg$out.par)
    colnames(res$trace$track.res)[4] <- paste0("mc.sd.", cfg$out.par)

    tibble(
      batch   = bn,
      element = i,
      res = list(res),
      cfg = list(cfg)
    )
  })
})

# flatten into a single tibble
sspLNIRT.data <- do.call(rbind, unlist(rows, recursive = FALSE))
sspLNIRT.data <- sspLNIRT.data[, c("cfg", "res")]

#### write data in file
usethis::use_data(sspLNIRT.data, overwrite = TRUE)


####### Inspect the data #########
summary(sspLNIRT.data)

lb_idx <- sapply(seq_len(nrow(sspLNIRT.data)), function(x) {
  grepl("<", sspLNIRT.data$res[[x]]$N.min)
})
sum(lb_idx)  # total number of LB hits

lb_rows <- which(lb_idx)

lb_summary <- data.frame(
  row     = lb_rows,
  out.par = sapply(lb_rows, function(x) sspLNIRT.data$cfg[[x]]$out.par),
  thresh  = sapply(lb_rows, function(x) sspLNIRT.data$cfg[[x]]$thresh)
)

# Cross-tabulation: how many LB hits per (out.par, thresh) combination
table(lb_summary$out.par, lb_summary$thresh)

lb_idx2 <- sapply(seq_len(nrow(sspLNIRT.data)), function(x) {
  grepl(">", sspLNIRT.data$res[[x]]$N.min)
})
sum(lb_idx2)  # total number of UB hits

# Extract out.par and thresh for the LB-hit rows
lb_rows2 <- which(lb_idx2)

lb_summary2 <- data.frame(
  row     = lb_rows2,
  out.par = sapply(lb_rows2, function(x) sspLNIRT.data$cfg[[x]]$out.par),
  thresh  = sapply(lb_rows2, function(x) sspLNIRT.data$cfg[[x]]$thresh)
)

# Cross-tabulation: how many LB hits per (out.par, thresh) combination
table(lb_summary2$out.par, lb_summary2$thresh)


# Identify conditions that did NOT hit either bound (numeric N.min only)
no_bound_idx <- sapply(seq_len(nrow(sspLNIRT.data)), function(x) {
  nm <- sspLNIRT.data$res[[x]]$N.min
  is.numeric(nm) || (is.character(nm) && !grepl("<|>", nm))
})
sum(no_bound_idx)  # number of conditions with a true interior N*

# Extract elapsed time for these conditions and convert to a common unit (minutes)
no_bound_rows <- which(no_bound_idx)

time_min <- sapply(no_bound_rows, function(x) {
  tt <- sspLNIRT.data$res[[x]]$trace$time.taken
  as.numeric(tt, units = "mins")
})

# Summary
summary(time_min)
range(time_min)
median(time_min)

# Optional: also report in hours for the manuscript footnote
range(time_min) / 60   # range in hours
median(time_min) / 60  # median in hours


