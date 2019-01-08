# 1. Set up MCMC settings -------------------------------------------------
num_particles <- 5             # Number of particles used in the conditional BPF
num_mcmc <- 10                 # Number of iterations in the MCMC samplers
burnin   <- 0                  # Number of interations to burn
# Initialize states at particular deviated values from true state values
deviate_states_init <- c(log(1.5), 150, log(2.5), 3.5)
# Initialize pars at percentage deviation from true par values
# HERE: no deviation: test are initialized always at true par values values
# deviate_par_init    <- 400 # in %
# 2. Initialization for the parameters ------------------------------------
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
# V. Merging initialization parameters:
par_init <- list(list(init_sig_sq_xa, init_phi_xa, init_bet_xa),
                 list(init_sig_sq_xb, init_phi_xb, init_bet_xb),
                 list(init_sig_sq_xp, init_phi_xp, init_bet_xp),
                 list(init_sig_sq_xq, init_phi_xq, init_bet_xq))
true_vals <- c(true_sig_sq_xa, true_phi_xa, true_bet_xa,
               true_sig_sq_xb, true_phi_xb, true_bet_xb,
               true_sig_sq_xp, true_phi_xp, true_bet_xp,
               true_sig_sq_xq, true_phi_xq, true_bet_xq)

