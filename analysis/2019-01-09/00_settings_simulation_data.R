# 1. Set up parameter values ----------------------------------------------
# I. xa1_t process parameters:
true_sig_sq_xa1 <- 0.01        # True latent state process noise variance
true_phi_xa1    <- 0.8        # True autoregressive parameter for states
true_bet_xa1    <- c(-2.5, 3) # c(-2.5, 3, -1, 0.5) # c(2) #   True regressor coefficients for states
# II. xa2_t process parameters:
true_sig_sq_xa2 <- 0.01
true_phi_xa2    <- 0.5
true_bet_xa2    <- c(2, -1)
# III. xa3_t process parameters:
true_sig_sq_xa3 <- 0.01
true_phi_xa3    <- 0.5
true_bet_xa3    <- c(-3, 4)
# IV. xa4_t process parameters:
true_sig_sq_xa4 <- 0.01
true_phi_xa4    <- 0.5
true_bet_xa4    <- c(4, -5)
# V. Merging true parameters
par_true <- list(list(true_sig_sq_xa1, true_phi_xa1, true_bet_xa1),
                 list(true_sig_sq_xa2, true_phi_xa2, true_bet_xa2),
                 list(true_sig_sq_xa3, true_phi_xa3, true_bet_xa3),
                 list(true_sig_sq_xa4, true_phi_xa4, true_bet_xa4))
# 2. Data settings --------------------------------------------------------
TT         <- 50     # Length of data record
D          <- 4      # Number of fractions (dimension of Dirichelet distr.)
par_levels <- (10*1:4)*c(log(2:5))
# 3. Generate data --------------------------------------------------------
dataSim <- generate_data(par_true = par_true,
                         T = TT,
                         D = D,
                         x_levels = par_levels,
                         seq_logs = c(T, T, T, T),
                         # seq_logs = c(F, F, F, F),
                         seq_cept = c(F, F, F, F),
                         old_regs = FALSE,
                         plot_states = TRUE,
                         plot_measurements = TRUE)
y_t   <- dataSim[[1]]
xa1_t <- dataSim[[2]][[1]]
xa2_t <- dataSim[[2]][[2]]
xa3_t <- dataSim[[2]][[3]]
xa4_t <- dataSim[[2]][[4]]
za1_t <- dataSim[[3]][[1]]
za2_t <- dataSim[[3]][[2]]
za3_t <- dataSim[[3]][[3]]
za4_t <- dataSim[[3]][[4]]
