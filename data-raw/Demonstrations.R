
library(sspLNIRT) # package name

available_configs() # no arguments to print the grid

result <- get_sspLNIRT(
  thresh         = c(0.10, 0.15, 0.05, 0.05), # target RMSE thresholds
  out.par        = c("alpha", "beta", "phi", "lambda"), # target item parameters
  K              = 30, # test length
  mu.alpha       = 1, # mean item discrimination
  meanlog.sigma2 = log(0.6), # mean log residual variance
  rho            = 0.4 # ability-speed correlation
)


summary(
  result$object # extracted result object
)

plot(
result$object, # extracted result object
type = "estimation", # "estimation" or "power"
pars = "item", # "item" or "person" level
y.val = "rmse" # "rmse" or "bias"
)

plot(
  result$object, # extracted result object
  type = "power", # "estimation" or "power"
  out.par = "alpha", # critical parameter
  thresh = 0.1 # target RMSE threshold
)

plot_RA(
result$design, # the design object
level = "item", # "item" or "person" level
by.theta = TRUE # whether to condition on ability
)

plot_RT(
result$design, # the design object
level = "item", # "item" or "person" level
log = FALSE  # whether to use log response times
)


design = list(
K              = 30,  # test length
mu.person      = c(0, 0),  # person parameter means
mu.item        = c(1, 0, 0.5, 1),  # item parameter means
meanlog.sigma2 = log(0.6),  # mean log residual variance
sdlog.sigma2 = 0, # sd log residual variance
cov.m.person   = matrix(c(1, 0.4, 0.4, 1), ncol = 2),  # person (co)variances
cov.m.item     = matrix(c(1, 0,   0,   0,
                          0, 1,   0,   0.4,
                          0, 0,   1,   0,
                          0, 0.4, 0,   1),
                        ncol = 4, byrow = TRUE),  # item correlations
sd.item        = c(0.2, 0.2, 0.2, 0.5),  # item SDs
cor2cov.item   = TRUE  # whether correlations or covariances
)

do.call(
plot_RA, # plot_RA or plot_RT
design # the specified design setting
)

library(future)
plan(multisession, workers = 4)

inputs = c(list(
thresh         = c(0.10, 0.15, 0.05, 0.05), # target RMSE thresholds
out.par        = c("alpha", "beta", "phi", "lambda")), # target item parameters
design
)


custom_result = do.call(
optim_sample, # sample-size optimization
inputs # specified custom design
)

