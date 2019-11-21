# 0. Load data and dimension settings -------------------------------------
TT <- nrow(data_current)
y_t_temp   <- data_current %>% select(CLEIB:renewables)
y_t        <- as.matrix(y_t_temp)
y_t        <- y_t[1:TT, ]
# y_t        <- y_t[2:TT, ]
num_counts <- rowSums(y_t)
D  <- ncol(y_t)
reg_scale <- 1
state_scale <- rep(1, times = D)
# za1_t <- cbind(rep(1, times = TT - 1),
#                data_current$cleid[2:(TT)]/reg_scale,
#                data_current$dfeid[1:(TT - 1)]/reg_scale,
#                data_current$ngeid[1:(TT - 1)]/reg_scale)
#
# za2_t <- cbind(rep(1, times = TT - 1),
#                data_current$cleid[1:(TT - 1)]/reg_scale,
#                data_current$dfeid[1:(TT - 1)]/reg_scale,
#                data_current$ngeid[2:(TT)]/reg_scale)
# za3_t <- cbind(rep(1, times = TT - 1),
#                data_current$cleid[1:(TT - 1)]/reg_scale,
#                data_current$dfeid[2:(TT)]/reg_scale,
#                data_current$ngeid[1:(TT - 1)]/reg_scale)
# za4_t <- cbind(rep(1, times = TT - 1),
#                data_current$cleid[1:(TT - 1)]/reg_scale,
#                data_current$dfeid[1:(TT - 1)]/reg_scale,
#                data_current$ngeid[1:(TT - 1)]/reg_scale)
# TT <- TT - 1
za1_t <- cbind(rep(1, times = TT),
               data_current$cleid[1:(TT)]/reg_scale,
               data_current$dfeid[1:(TT)]/reg_scale,
               data_current$ngeid[1:(TT)]/reg_scale)
za2_t <- za1_t
za3_t <- za1_t
za4_t <- za1_t
za5_t <- za4_t
za6_t <- za4_t
reg_dim <- ncol(za1_t)
# 1. Set up MCMC settings -------------------------------------------------
num_particles <- 100000 # Number of particles used in the conditional BPF
num_mcmc <- 5000      # Number of iterations in the MCMC samplers
burnin   <- 1000       # Number of interations to burn
# Initialize states at particular deviated values from true state values
# states_init <- log(c(0.01, 0.4, 0.2, 0.3, 0.2, 0.05)*reg_scale)
# browser()
zero_lower_bound <- 0.001
states_init_1 <- y_t[, 1]
states_init_1[states_init_1 == 0] <- zero_lower_bound
states_init_1 <- log(states_init_1/state_scale[1])
states_init_2 <- log(y_t[, 2]/state_scale[2])
states_init_3 <- log(y_t[, 3]/state_scale[3])
states_init_4 <- log(y_t[, 4]/state_scale[4])
states_init_5 <- y_t[, 5]
states_init_5[states_init_5 == 0] <- zero_lower_bound
states_init_5 <- log(states_init_5/state_scale[5])
states_init_6 <- y_t[, 6]
states_init_6[states_init_6 == 0] <- zero_lower_bound
states_init_6 <- log(states_init_6/state_scale[6])
states_init <- list(states_init_1, states_init_2, states_init_3,
                    states_init_4, states_init_5, states_init_6)
# 2. Initialization for the parameters ------------------------------------
bet_init <- 1 # 0.1
# I. xa1_t process parameters:
init_sig_sq_xa1 <- 0.5
init_phi_xa1    <- 0.5
init_bet_xa1    <- rep(bet_init, times = reg_dim)
# II. xa2_t process parameters:
init_sig_sq_xa2 <- 0.5
init_phi_xa2    <- 0.5
init_bet_xa2    <- rep(bet_init, times = reg_dim)
# III. xa3_t process parameters:
init_sig_sq_xa3 <- 0.5
init_phi_xa3    <- 0.5
init_bet_xa3    <- rep(bet_init, times = reg_dim)
# IV. xa4_t process parameters:
init_sig_sq_xa4 <- 0.5
init_phi_xa4    <- 0.5
init_bet_xa4    <- rep(bet_init, times = reg_dim)
# IV. xa5_t process parameters:
init_sig_sq_xa5 <- 0.5
init_phi_xa5    <- 0.5
init_bet_xa5    <- rep(bet_init, times = reg_dim)
# IV. xa6_t process parameters:
init_sig_sq_xa6 <- 0.5
init_phi_xa6    <- 0.5
init_bet_xa6    <- rep(bet_init, times = reg_dim)
# V. Merging initialization parameters:
par_init <- list(list(init_sig_sq_xa1, init_phi_xa1, init_bet_xa1),
                 list(init_sig_sq_xa2, init_phi_xa2, init_bet_xa2),
                 list(init_sig_sq_xa3, init_phi_xa3, init_bet_xa3),
                 list(init_sig_sq_xa4, init_phi_xa4, init_bet_xa4),
                 list(init_sig_sq_xa5, init_phi_xa5, init_bet_xa5),
                 list(init_sig_sq_xa6, init_phi_xa6, init_bet_xa6))
# Hyperparameters for the inverse gamma priors (uninformative)
prior_a <- 0.01
prior_b <- 0.01
