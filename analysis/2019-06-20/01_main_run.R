############################## PGAS for KZ model ###############################
rm(list = ls())
source("./R/helper/00_helper_lib_load.R")
source("./R/helper/00_helper_model_fcts.R")
source("./R/helper/00_helper_simulation_data.R")
source("./R/helper/99_helper_diagnostics.R")

source("./R/helper/01_helper_cBPF_as.R")
source("./R/helper/02_helper_pgas.R")
source("./R/01_cBPF_as.R")
source("./R/02_pgas.R")
# PGAS run ----------------------------------------------------------------
set.seed(139423) # set.seed(3) #
source("./analysis/2019-06-19/00_settings_simulation_data.R")
data_us_all <- read_excel("./data/tidy/us_energy.xls")
TT <- dim(data_us_all)[1]
y_shares <- data_us_all[, c("coal", "oil", "gas", "nuclear", "renewable")]
ID_reg <- 2:7
regs <- data_us_all[, ID_reg]
num_reg <- ncol(regs)
scale_reg <- c(0, 1, 1000, 10000, 1, 10, 1)[ID_reg]
regs_scaled <- t(t(as.matrix(regs))/scale_reg)
colnames(regs_scaled) <- names(data_us_all)[2:7]

za1_t <- matrix(1, nrow = TT, ncol = 1)
za1_t <- cbind(za1_t, regs_scaled[, 1:3])
# always include colums 1 to 3 which is temp, rain, gdp
za5_t <- za4_t <- za3_t <- za2_t <- za1_t
# for coal-share: use lagged coal and oil prices
za1_t <- cbind(za1_t, regs_scaled[, c("pricecoal(t-1)", "priceoil(t-1)")])
# for oil-share: use lagged oil prices
za2_t <- cbind(za2_t, regs_scaled[, "priceoil(t-1)"])
# for gas-share: use all lagged prices
za3_t <- cbind(za3_t, regs_scaled[, c("pricecoal(t-1)", "priceoil(t-1)", "pricegas(t-1)")])
# for nuclear-share: use nuclear price which we do not have
za4_t <- za4_t # cbind(za4_t, regs_scaled[, ?])
# for renewable-share: use nuclear and oil prices, but we do not have nuclear prices
za5_t <- cbind(za5_t, regs_scaled[, "priceoil(t-1)"])

# TT <- TT - 1
# y_shares <- as.matrix(y_shares)
# states_init <- colMeans(yshares)
states_init <- colMeans(y_shares)*10
states_init <- log(states_init)
# Hyperparameters for the inverse gamma priors (uninformative)
prior_a <- 0.01
prior_b <- 0.01
# I. xa1_t process parameters:
init_sig_sq_xa1 <- 1
init_phi_xa1    <- 0.5
init_bet_xa1    <- rep(1, times = ncol(za1_t))
# II. xa2_t process parameters:
init_sig_sq_xa2 <- 1
init_phi_xa2    <- 0.5
init_bet_xa2    <- rep(1, times = ncol(za2_t))
# III. xa3_t process parameters:
init_sig_sq_xa3 <- 1
init_phi_xa3    <- 0.5
init_bet_xa3    <- rep(1, times = ncol(za3_t))
# IV. xa4_t process parameters:
init_sig_sq_xa4 <- 1
init_phi_xa4    <- 0.5
init_bet_xa4    <- rep(1, times = ncol(za4_t))
# IV. xa5_t process parameters:
init_sig_sq_xa5 <- 1
init_phi_xa5    <- 0.5
init_bet_xa5    <- rep(1, times = ncol(za5_t))

par_init <- list(list(init_sig_sq_xa1, init_phi_xa1, init_bet_xa1),
                 list(init_sig_sq_xa2, init_phi_xa2, init_bet_xa2),
                 list(init_sig_sq_xa3, init_phi_xa3, init_bet_xa3),
                 list(init_sig_sq_xa4, init_phi_xa4, init_bet_xa4),
                 list(init_sig_sq_xa5, init_phi_xa5, init_bet_xa5))

# 1. Set up MCMC settings -------------------------------------------------
num_particles <- 10000  # Number of particles used in the conditional BPF
num_mcmc <- 10000       # Number of iterations in the MCMC samplers
burnin   <- 5000        # Number of interations to burn

out_pgas <- pgas(N = num_particles, MM = num_mcmc, TT = TT,
                 y = y_shares,
                 Za1 = za1_t, Za2 = za2_t,
                 Za3 = za3_t, Za4 = za4_t, Za5 = za5_t,
                 priors = c(prior_a, prior_b),
                 par_init = par_init,
                 par_true = true_vals,
                 traj_init = states_init,
                 filtering = TRUE,
                 num_plots_states = 1)

source("./analysis/2019-06-20/99_analyse_convergence_run.R")









