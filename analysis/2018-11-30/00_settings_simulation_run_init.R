# 1. Set up MCMC settings -------------------------------------------------
num_particles <- 1000  # Number of particles used in the conditional BPF
num_mcmc <- 1380       # Number of iterations in the MCMC samplers
burnin   <- 100        # Number of interations to burn
# Initialize states at particular deviated values from true state values
deviate_par_rate    <- 10  # in %
deviate_states_init <- c(log(par_levels[2:5])) + c(1, 2, 1, 2) #
# Initialize pars at percentage deviation from true par values
# deviate_par_rate    <- 400
# 2. Initialization for the parameters ------------------------------------
if (init_at_true) {
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
} else {
  # I. xa1_t process parameters:
  init_sig_sq_xa1 <- 1
  init_phi_xa1    <- -0.9
  init_bet_xa1    <- true_bet_xa1 + true_bet_xa1 * (deviate_par_rate/100)
  # II. xa2_t process parameters:
  init_sig_sq_xa2 <- 1
  init_phi_xa2    <- 0.1
  init_bet_xa2    <- true_bet_xa2 + true_bet_xa2 * (deviate_par_rate/100)
  # III. xa3_t process parameters:
  init_sig_sq_xa3 <- 1
  init_phi_xa3    <- 0.1
  init_bet_xa3    <- true_bet_xa3 + true_bet_xa3 * (deviate_par_rate/100)
  # IV. xa4_t process parameters:
  init_sig_sq_xa4 <- 1   # true_sig_sq_xa4
  init_phi_xa4    <- 0.1 # true_phi_xa4
  init_bet_xa4    <- true_bet_xa4 + true_bet_xa4 * (deviate_par_rate/100)
  # init_bet_xa4    <- true_bet_xa4
}
# V. Merging initialization parameters:
par_init <- list(list(init_sig_sq_xa1, init_phi_xa1, init_bet_xa1),
                 list(init_sig_sq_xa2, init_phi_xa2, init_bet_xa2),
                 list(init_sig_sq_xa3, init_phi_xa3, init_bet_xa3),
                 list(init_sig_sq_xa4, init_phi_xa4, init_bet_xa4))
true_vals <- c(true_sig_sq_xa1, true_phi_xa1, true_bet_xa1,
               true_sig_sq_xa2, true_phi_xa2, true_bet_xa2,
               true_sig_sq_xa3, true_phi_xa3, true_bet_xa3,
               true_sig_sq_xa4, true_phi_xa4, true_bet_xa4)
