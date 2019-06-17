# 1. Set up MCMC settings -------------------------------------------------
num_particles <- 5             # Number of particles used in the conditional BPF
num_mcmc <- 10                 # Number of iterations in the MCMC samplers
burnin   <- 0                  # Number of interations to burn
# Initialize states at particular deviated values from true state values
deviate_states_init <- c(log(par_levels[1:4]))
# Initialize pars at percentage deviation from true par values
# HERE: no deviation: test are initialized always at true par values values
# deviate_par_init    <- 400 # in %
# 2. Initialization for the parameters ------------------------------------
# I. xa1_t process parameters:
init_sig_sq_xa1 <- true_sig_sq_xa1
init_phi_xa1    <- true_phi_xa1
init_bet_xa1    <- true_bet_xa1
# II. xa2_t process parameters:
init_sig_sq_xa2 <- true_sig_sq_xa2
init_phi_xa2    <- true_phi_xa2
init_bet_xa2    <- true_bet_xa2
# III. xa3_t process parameters:
init_sig_sq_xa3 <- true_sig_sq_xa3
init_phi_xa3    <- true_phi_xa3
init_bet_xa3    <- true_bet_xa3
# IV. xa4_t process parameters:
init_sig_sq_xa4 <- true_sig_sq_xa4
init_phi_xa4    <- true_phi_xa4
init_bet_xa4    <- true_bet_xa4
# V. Merging initialization parameters:
par_init <- list(list(init_sig_sq_xa1, init_phi_xa1, init_bet_xa1),
                 list(init_sig_sq_xa2, init_phi_xa2, init_bet_xa2),
                 list(init_sig_sq_xa3, init_phi_xa3, init_bet_xa3),
                 list(init_sig_sq_xa4, init_phi_xa4, init_bet_xa4))
true_vals <- c(true_sig_sq_xa1, true_phi_xa1, true_bet_xa1,
               true_sig_sq_xa2, true_phi_xa2, true_bet_xa2,
               true_sig_sq_xa3, true_phi_xa3, true_bet_xa3,
               true_sig_sq_xa4, true_phi_xa4, true_bet_xa4)
# Hyperparameters for the inverse gamma priors (uninformative)
prior_a <- 0.01
prior_b <- 0.01
