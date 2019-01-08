# 1. Set up MCMC settings -------------------------------------------------
num_particles <- 1000  # Number of particles used in the conditional BPF
num_mcmc <- 1380       # Number of iterations in the MCMC samplers
burnin   <- 100        # Number of interations to burn
# Initialize states at particular deviated values from true state values
deviate_par_rate    <- 10  # in %
deviate_states_init <- log(par_levels) # + c(1, 2, 1, 2)  # c(2, 3, 2, 3)
# Initialize pars at percentage deviation from true par values
deviate_par_rate    <- 400
# 2. Initialization for the parameters ------------------------------------
if (init_at_true) {
  # I. xa_t process parameters:
  init_sig_sq_xa <- true_sig_sq_xa
  init_phi_xa    <- true_phi_xa
  init_bet_xa    <- true_bet_xa
  # II. xb_t process parameters:
  init_sig_sq_xb <- true_sig_sq_xb
  init_phi_xb    <- true_phi_xb
  init_bet_xb    <- true_bet_xb
  # III. xp_t process parameters:
  init_sig_sq_xp <- true_sig_sq_xp
  init_phi_xp    <- true_phi_xp
  init_bet_xp    <- true_bet_xp
  # IV. xq_t process parameters:
  init_sig_sq_xq <- true_sig_sq_xq
  init_phi_xq    <- true_phi_xq
  init_bet_xq    <- true_bet_xq
} else {
  # I. xa_t process parameters:
  init_sig_sq_xa <- 1
  init_phi_xa    <- -0.9
  init_bet_xa    <- true_bet_xa + true_bet_xa * (deviate_par_rate/100)
  # II. xb_t process parameters:
  init_sig_sq_xb <- 1
  init_phi_xb    <- 0.1
  init_bet_xb    <- true_bet_xb + true_bet_xb * (deviate_par_rate/100)
  # III. xp_t process parameters:
  init_sig_sq_xp <- 1
  init_phi_xp    <- 0.1
  init_bet_xp    <- true_bet_xp + true_bet_xp * (deviate_par_rate/100)
  # IV. xq_t process parameters:
  init_sig_sq_xq <- true_sig_sq_xq
  init_phi_xq    <- true_phi_xq
  init_bet_xq    <- true_bet_xq
  # init_sig_sq_xq <- 1
  # init_phi_xq    <- -0.9
  # init_bet_xq    <- -sign(true_bet_xq)*0.5
}
# V. Merging initialization parameters:
par_init <- list(list(init_sig_sq_xa, init_phi_xa, init_bet_xa),
                 list(init_sig_sq_xb, init_phi_xb, init_bet_xb),
                 list(init_sig_sq_xp, init_phi_xp, init_bet_xp),
                 list(init_sig_sq_xq, init_phi_xq, init_bet_xq))
true_vals <- c(true_sig_sq_xa, true_phi_xa, true_bet_xa,
               true_sig_sq_xb, true_phi_xb, true_bet_xb,
               true_sig_sq_xp, true_phi_xp, true_bet_xp,
               true_sig_sq_xq, true_phi_xq, true_bet_xq)
