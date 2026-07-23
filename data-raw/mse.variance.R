# script to determine the variance in MSE given the number of
# iterations for calculating the mse and the number of posterior samples.


# Setup Config ------------------------------------------------------------


# sys settings
Sys.setenv(
  OMP_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  OPENBLAS_NUM_THREADS = "1",
  BLAS_NUM_THREADS = "1"
)

# cran repo
options(repos=c(CRAN="https://ftp.belnet.be/mirror/CRAN/"))

# setup for HPC or local
HPC = TRUE

if (HPC) {
  # set root path
  root.dir <- "/home4/p310779/sspLNIRT/"

  # set save path
  save.dir <- "/home4/p310779/sspLNIRT/data-raw/results/"
  dir.create(save.dir, recursive = TRUE, showWarnings = FALSE)

} else {
  # set root path
  root.dir <- "./"

  # set save path
  save.dir <- "./data-raw/results/"
  #dir.create(save.dir, recursive = TRUE, showWarnings = FALSE)


}

# required functions
fct.names <- list(
  "R/fct_comp_rmse.R",
  "R/fct_item_par.R",
  "R/fct_optim_sample.R",
  "R/fct_person_par.R",
  "R/fct_rhat_LNIRT.R",
  "R/fct_scale_M.R",
  "R/fct_sim_jhm_data.R"
)

# load to environment
invisible (
  lapply(fct.names, FUN = function(x) {
    source(paste0(root.dir, x))
  }))

# cores
if (HPC ) {
  # set cores
  n.cores <- future::availableCores() - 5
  cat("running with ", n.cores, "cores! \n\n")
  future::plan(future::multisession, workers = n.cores)
} else {
  n.cores <- 6
  future::plan(future::multisession, workers = n.cores)
}

parallelly::supportsMulticore()

# Run the Job -------------------------------------------------------------

# storage
result.list <- list()

# compute RMSE
start.time = Sys.time()

  result <- list()

  for (k in 1:100) {
    res <- comp_rmse(
      N = 500,
      iter = 200,
      K = 30,
      mu.person = c(0,0),
      mu.item = c(1,0,.5,1),
      meanlog.sigma2 = log(.6),
      cov.m.person = matrix(c(1,0.4,
                              0.4,1), ncol = 2, byrow = TRUE),
      cov.m.item = matrix(c(1, 0, 0, 0,
                            0, 1, 0, 0.4,
                            0, 0, 1, 0,
                            0, 0.4, 0, 1), ncol =  4, byrow = TRUE),
      sd.item         = c(.2, 1, .2, .5),
      cor2cov.item    = TRUE,
      sdlog.sigma2 = 0.2,
      XG = 5000,
      seed = NULL)
    result[[k]] <- res
    saveRDS(res, paste0(save.dir, "mse.variance.no.seed.", k))
    cat("iteration", k, "of", 100, "done!!!! \n\n")
    rm(res)
  }
  saveRDS(result, paste0(save.dir, "mse.variance.no.seed."))

end.time = Sys.time()
time.taken = end.time-start.time
print(time.taken)


# Results -----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(jtools)

iter = 200

# load results
out <- readRDS(paste0(save.dir, "mse.variance.no.seed."))

# get rmse data
rmse.data <-
  as.data.frame(t(sapply(out, FUN = function(y) {
    cbind(y$item$rmse)
  })))
colnames(rmse.data) <- c("alpha", "beta", "phi", "lambda", "sigma2")

rmse.data <- tidyr::pivot_longer(rmse.data, cols = 1:4, names_to = "parameter", values_to = "rmse")

sum.stats <- rmse.data %>%
  summarise(
    mu  = mean(rmse),
    sd  = sd(rmse),
    min = min(rmse),
    max = max(rmse),
    rel.sd = sd(rmse)/mean(rmse),
    .by = c(parameter)
  )

saveRDS(sum.stats, paste0(save.dir, "rmse.variance.sum.stats"))

### Paper Figure and numbers

# density plot
sd.rmse.plot <- ggplot(rmse.data, aes(x = rmse, fill = parameter)) +
  geom_density(alpha = .5) +
  scale_fill_grey(start = 0.05, end = 0.95, name = "Parameter") +
  xlab("RMSE") +
  xlim(c(0.025, .125)) +
  theme_apa()


ggsave(
  filename = "./data-raw/plots/sd.rmse.plot.pdf",
  plot     = sd.rmse.plot,
  width    = 180,
  height   = 100,
  units    = "mm",
  bg       = "white",
  dpi = 300
)



