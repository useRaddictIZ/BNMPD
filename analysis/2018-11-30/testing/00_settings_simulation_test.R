# 1. Set up parameter values ----------------------------------------------
# I. xa_t process parameters:
true_sig_sq_xa <- 0.1      # True latent state process noise variance
true_phi_xa    <- 0.5      # True autoregressive parameter for states
true_bet_xa    <- c(-1, 2) # True regressor coefficients for states
# II. xb_t process parameters:
true_sig_sq_xb <- 5
true_phi_xb    <- 0.5
true_bet_xb    <- c(2, -1)
# III. xp_t process parameters:
true_sig_sq_xp <- 0.2
true_phi_xp    <- 0.5
true_bet_xp    <- c(-3, 4)
# IV. xp_t process parameters:
true_sig_sq_xq <- 0.2
true_phi_xq    <- 0.5
true_bet_xq    <- c(4, -5)
# V. Merging true parameters
par_true <- list(list(true_sig_sq_xa, true_phi_xa, true_bet_xa),
                 list(true_sig_sq_xb, true_phi_xb, true_bet_xb),
                 list(true_sig_sq_xp, true_phi_xp, true_bet_xp),
                 list(true_sig_sq_xq, true_phi_xq, true_bet_xq))
# 2. Data settings --------------------------------------------------------
TT       <- 10     # Length of data record
KK       <- 10     # Number of income classes - 1
num_obs  <- 10e4   # Number of total individual incomes
par_levels <- c(1.5, 150, 2.5, 3.5)
# 3. Generate data --------------------------------------------------------
dataSim <- generate_data(par_true = par_true,
                         T = TT,
                         K = KK,
                         num_incs = num_obs,
                         x_levels = par_levels,
                         seq_logs = c(T, F, T, F),
                         seq_cept = c(F, F, F, F),
                         old_regs = FALSE,
                         plot_states = FALSE)
y_raw <- dataSim[[1]]
yz_t  <- dataSim[[2]]
y_t   <- matrix(0, nrow = TT, ncol = KK)
for (t in 1:TT) {
  ncut <- cut(y_raw[t, ], breaks = yz_t[t, ])
  y_t[t, ] <- as.vector(table(ncut))
}
yz_t <- yz_t[, -(KK + 1)]
xa_t <- dataSim[[3]][[1]]
xb_t <- dataSim[[3]][[2]]
xp_t <- dataSim[[3]][[3]]
xq_t <- dataSim[[3]][[4]]
za_t <- dataSim[[4]][[1]]
zb_t <- dataSim[[4]][[2]]
zp_t <- dataSim[[4]][[3]]
zq_t <- dataSim[[4]][[4]]
# Hyperparameters for the inverse gamma priors (uninformative)
prior_a <- 0.01
prior_b <- 0.01
