############################## Various tests for KZ model ###############################
# library(colorout)
rm(list = ls())
sapply(paste0(getwd(),"/R/helper/", list.files(paste0(getwd(),"/R/helper/"), recursive = TRUE)), source)
set.seed(139423) # set.seed(3) #
init_at_true <- TRUE
source("./tests/00_settings_simulation_data.R")
source("./tests/00_settings_simulation_init.R")
source("./R/01_cBPF_as.R")
source("./R/01_cBPF_as_test.R")
num_particles <- 200
seed_nr <- 234
set.seed(seed_nr)
test_old <- cBPF_as(N = num_particles, TT = TT, num_counts = num_counts,
                    y = y_t,
                    Za1 = za1_t,
                    Za2 = za2_t,
                    Za3 = za3_t,
                    Za4 = za4_t,
                    Za5 = za5_t,
                    Za6 = za6_t,
                    sig_sq_xa1 = true_sig_sq_xa1,
                    sig_sq_xa2 = true_sig_sq_xa2,
                    sig_sq_xa3 = true_sig_sq_xa3,
                    sig_sq_xa4 = true_sig_sq_xa4,
                    sig_sq_xa5 = true_sig_sq_xa5,
                    sig_sq_xa6 = true_sig_sq_xa6,
                    phi_xa1 = true_phi_xa1,
                    phi_xa2 = true_phi_xa2,
                    phi_xa3 = true_phi_xa3,
                    phi_xa4 = true_phi_xa4,
                    phi_xa5 = true_phi_xa5,
                    phi_xa6 = true_phi_xa6,
                    bet_xa1 = true_bet_xa1,
                    bet_xa2 = true_bet_xa2,
                    bet_xa3 = true_bet_xa3,
                    bet_xa4 = true_bet_xa4,
                    bet_xa5 = true_bet_xa5,
                    bet_xa6 = true_bet_xa6,
                    xa1_r = xa1_t,
                    xa2_r = xa2_t,
                    xa3_r = xa3_t,
                    xa4_r = xa4_t,
                    xa5_r = xa5_t,
                    xa6_r = xa6_t)
dim_bet <- sapply(lapply(par_init, unlist), length, simplify = TRUE) - 2
dim_zet <- sapply(list(za1_t, za2_t, za3_t, za4_t, za5_t, za6_t), ncol)
id_bet  <- c(0, cumsum(dim_bet))
id_zet  <- c(0, cumsum(dim_zet))
sig_sq_xa <- matrix(0, nrow = D, ncol = num_mcmc)
phi_xa    <- matrix(0, nrow = D, ncol = num_mcmc)
bet_xa    <- matrix(0, nrow = sum(dim_bet) + D, ncol = num_mcmc)
Za   <- cbind(za1_t, za2_t, za3_t, za4_t, za5_t, za6_t)
Z_beta <- matrix(0, nrow = TT, ncol = D)
for (d in 1:D) {
  sig_sq_xa[d, 1]                          <- par_init[[d]][[1]]
  phi_xa[d, 1]                             <- par_init[[d]][[2]]
  bet_xa[(id_bet[d] + 1):id_bet[d + 1], 1] <- par_init[[d]][[3]]
  Z_beta[, d] <- Za[, (id_zet[d] + 1):id_zet[d + 1]] %*% bet_xa[(id_bet[d] + 1):id_bet[d + 1], 1]
}
set.seed(seed_nr)
test_new <- cBPF_as_R_short(N = num_particles, TT = TT, DD = D,
                            y = y_t, num_counts = num_counts,
                            Z_beta = Z_beta,
                            sig_sq_x = c(sig_sq_xa[1, 1], sig_sq_xa[2, 1], sig_sq_xa[3, 1],
                                         sig_sq_xa[4, 1], sig_sq_xa[5, 1], sig_sq_xa[6, 1]),
                            phi_x = c(phi_xa[1, 1], phi_xa[2, 1], phi_xa[3, 1],
                                      phi_xa[4, 1], phi_xa[5, 1], phi_xa[6, 1]),
                            bet_x = c(bet_xa[1, 1], bet_xa[2, 1], bet_xa[3, 1],
                                      bet_xa[4, 1], bet_xa[5, 1], bet_xa[6, 1]),
                            x_r = c(xa1_t, xa2_t, xa3_t,
                                    xa4_t, xa5_t, xa6_t))
print(all.equal(test_new, test_old))
# test_cpp[[1]][, 1] == test_r[[1]][, 1]
# n_particles_testing <- 1e6
# microbenchmark::microbenchmark(
# rcpp =
# cbpf_as_c2(N = n_particles_testing, TT = TT, num_counts,
#                 y = y_t,
#                 Za1 = za1_t,
#                 Za2 = za2_t,
#                 Za3 = za3_t,
#                 Za4 = za4_t,
#                 Za5 = za5_t,
#                 Za6 = za6_t,
#                 sig_sq_xa1 = true_sig_sq_xa1,
#                 sig_sq_xa2 = true_sig_sq_xa2,
#                 sig_sq_xa3 = true_sig_sq_xa3,
#                 sig_sq_xa4 = true_sig_sq_xa4,
#                 sig_sq_xa5 = true_sig_sq_xa5,
#                 sig_sq_xa6 = true_sig_sq_xa6,
#                 phi_xa1 = true_phi_xa1,
#                 phi_xa2 = true_phi_xa2,
#                 phi_xa3 = true_phi_xa3,
#                 phi_xa4 = true_phi_xa4,
#                 phi_xa5 = true_phi_xa5,
#                 phi_xa6 = true_phi_xa6,
#                 bet_xa1 = true_bet_xa1,
#                 bet_xa2 = true_bet_xa2,
#                 bet_xa3 = true_bet_xa3,
#                 bet_xa4 = true_bet_xa4,
#                 bet_xa5 = true_bet_xa5,
#                 bet_xa6 = true_bet_xa6,
#                 xa1_r = 1:TT,
#                 xa2_r = 1:TT,
#                 xa3_r = 1:TT,
#                 xa4_r = 1:TT,
#                 xa5_r = 1:TT,
#                 xa6_r = 1:TT),
# arma =
# cbpf_as_c3(N = n_particles_testing, TT = TT, num_counts,
#                 y = y_t,
#                 Za1 = za1_t,
#                 Za2 = za2_t,
#                 Za3 = za3_t,
#                 Za4 = za4_t,
#                 Za5 = za5_t,
#                 Za6 = za6_t,
#                 sig_sq_xa1 = true_sig_sq_xa1,
#                 sig_sq_xa2 = true_sig_sq_xa2,
#                 sig_sq_xa3 = true_sig_sq_xa3,
#                 sig_sq_xa4 = true_sig_sq_xa4,
#                 sig_sq_xa5 = true_sig_sq_xa5,
#                 sig_sq_xa6 = true_sig_sq_xa6,
#                 phi_xa1 = true_phi_xa1,
#                 phi_xa2 = true_phi_xa2,
#                 phi_xa3 = true_phi_xa3,
#                 phi_xa4 = true_phi_xa4,
#                 phi_xa5 = true_phi_xa5,
#                 phi_xa6 = true_phi_xa6,
#                 bet_xa1 = true_bet_xa1,
#                 bet_xa2 = true_bet_xa2,
#                 bet_xa3 = true_bet_xa3,
#                 bet_xa4 = true_bet_xa4,
#                 bet_xa5 = true_bet_xa5,
#                 bet_xa6 = true_bet_xa6,
#                 xa1_r = 1:TT,
#                 xa2_r = 1:TT,
#                 xa3_r = 1:TT,
#                 xa4_r = 1:TT,
#                 xa5_r = 1:TT,
#                 xa6_r = 1:TT),
# r =
# cBPF_as_test(N = n_particles_testing, TT = TT, num_counts = num_counts,
#                y = y_t,
#                 Za1 = za1_t,
#                 Za2 = za2_t,
#                 Za3 = za3_t,
#                 Za4 = za4_t,
#                 Za5 = za5_t,
#                 Za6 = za6_t,
#                 sig_sq_xa1 = true_sig_sq_xa1,
#                 sig_sq_xa2 = true_sig_sq_xa2,
#                 sig_sq_xa3 = true_sig_sq_xa3,
#                 sig_sq_xa4 = true_sig_sq_xa4,
#                 sig_sq_xa5 = true_sig_sq_xa5,
#                 sig_sq_xa6 = true_sig_sq_xa6,
#                 phi_xa1 = true_phi_xa1,
#                 phi_xa2 = true_phi_xa2,
#                 phi_xa3 = true_phi_xa3,
#                 phi_xa4 = true_phi_xa4,
#                 phi_xa5 = true_phi_xa5,
#                 phi_xa6 = true_phi_xa6,
#                 bet_xa1 = true_bet_xa1,
#                 bet_xa2 = true_bet_xa2,
#                 bet_xa3 = true_bet_xa3,
#                 bet_xa4 = true_bet_xa4,
#                 bet_xa5 = true_bet_xa5,
#                 bet_xa6 = true_bet_xa6,
#                 xa1_r = 1:TT,
#                 xa2_r = 1:TT,
#                 xa3_r = 1:TT,
#                 xa4_r = 1:TT,
#                 xa5_r = 1:TT,
#                 xa6_r = 1:TT)
# )
