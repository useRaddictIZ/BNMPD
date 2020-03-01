pgas1_short <- function(N, MM, TT, DD,
                        y, num_counts,
                        Za1, Za2, Za3, Za4, Za5, Za6,
                        priors,
                        par_init,
                        par_true = NULL,
                        traj_init,
                        filtering = TRUE) {# num_plots_states
  # Initialize data containers
  Za_list <- list(Za1, Za2, Za3, Za4, Za5, Za6)
  dim_bet <- sapply(lapply(par_init, unlist), length, simplify = TRUE) - 2
  dim_zet <- sapply(Za_list, ncol)
  id_bet  <- c(0, cumsum(dim_bet))
  id_zet  <- c(0, cumsum(dim_zet))
  id_reg  <- c(0, cumsum(dim_zet + 1))

  w  <- numeric(N)
  Xa <- array(0, dim = c(MM, TT, DD))
  sig_sq_xa <- matrix(0, nrow = DD, ncol = MM)
  phi_xa    <- matrix(0, nrow = DD, ncol = MM)
  bet_xa    <- matrix(0, nrow = sum(dim_bet) + DD, ncol = MM)

  prior_V_xa  <- list()

  Za   <- cbind(Za1, Za2, Za3, Za4, Za5, Za6)
  regs <- matrix(0, nrow = TT - 1, ncol = sum(dim_zet) + DD)
  # Initialize priors:
  prior_a     <- priors[1] + (TT - 1)/2
  prior_b     <- priors[2]
  ## I. Set states to deterministic starting values, initialize parameters, regressor values, and priors
  for (d in 1:DD) {
    Xa[1, , d] <- traj_init[[d]]
    sig_sq_xa[d, 1]                          <- par_init[[d]][[1]]
    phi_xa[d, 1]                             <- par_init[[d]][[2]]
    bet_xa[(id_bet[d] + 1):id_bet[d + 1], 1] <- par_init[[d]][[3]]
    regs[, (id_zet[d] + 1 + 1*d):(id_zet[d + 1] + 1*d)] <- Za[2:TT, (id_zet[d] + 1):id_zet[d + 1]]
    prior_V_xa[[d]] <- diag(dim_bet[d] + 1)/1000
  }
  ## II. run cBPF and use output as first conditioning trajectory
  # monitor_pgas_states(states_drawn = cbind(exp(Xa1[1, ]), exp(Xa2[1, ]),
  #                                          exp(Xa3[1, ]), exp(Xa4[1, ]),
  #                                          exp(Xa5[1, ]), exp(Xa6[1, ])),
  #                     states_comp = cbind(exp(states_init_1), exp(states_init_2),
  #                                          exp(states_init_3), exp(states_init_4),
  #                                          exp(states_init_5), exp(states_init_6)),
  #                       # NULL,
  #                       # cbind(xa1_t, xa2_t,
  #                       #                    xa3_t, xa4_t,
  #                       #                    xa5_5, xa6_t),
  #                     current = 1, total = 1, num_prints = 1)
  # cbpf_as_c2 # cBPF_as
  out_cPF <- cbpf_as_c2(y = y, num_counts = num_counts,
                        Za1 = Za[, (id_zet[1] + 1):id_zet[2]], Za2 = Za[, (id_zet[2] + 1):id_zet[3]],
                        Za3 = Za[, (id_zet[3] + 1):id_zet[4]], Za4 = Za[, (id_zet[4] + 1):id_zet[5]],
                        Za5 = Za[, (id_zet[5] + 1):id_zet[6]], Za6 = Za[, (id_zet[6] + 1):id_zet[7]],
                        N = N, TT = TT,
                        sig_sq_xa1 = sig_sq_xa[1, 1],
                        phi_xa1 = phi_xa[1, 1],
                        bet_xa1 = bet_xa[(id_bet[1] + 1):id_bet[2], 1, drop = F],
                        xa1_r = Xa[1, , 1],
                        sig_sq_xa2 = sig_sq_xa[2, 1],
                        phi_xa2 = phi_xa[2, 1],
                        bet_xa2 = bet_xa[(id_bet[2] + 1):id_bet[3], 1, drop = F],
                        xa2_r = Xa[1, , 2],
                        sig_sq_xa3 = sig_sq_xa[3, 1],
                        phi_xa3 = phi_xa[3, 1],
                        bet_xa3 = bet_xa[(id_bet[3] + 1):id_bet[4], 1, drop = F],
                        xa3_r = Xa[1, , 3],
                        sig_sq_xa4 = sig_sq_xa[4, 1],
                        phi_xa4 = phi_xa[4, 1],
                        bet_xa4 = bet_xa[(id_bet[4] + 1):id_bet[5], 1, drop = F],
                        xa4_r = Xa[1, , 4],
                        sig_sq_xa5 = sig_sq_xa[5, 1],
                        phi_xa5 = phi_xa[5, 1],
                        bet_xa5 = bet_xa[(id_bet[5] + 1):id_bet[6], 1, drop = F],
                        xa5_r = Xa[1, , 5],
                        sig_sq_xa6 = sig_sq_xa[6, 1],
                        phi_xa6 = phi_xa[6, 1],
                        bet_xa6 = bet_xa[(id_bet[6] + 1):id_bet[7], 1, drop = F],
                        xa6_r = Xa[1, , 6])
  w        <- out_cPF[[1]][, TT]
  b        <- sample.int(n = N, size = 1, replace = TRUE, prob = w)
  for (d in 1:DD) {
    Xa[1, , d] <- out_cPF[[d + 1]][b, ]
  }
  # monitor_pgas_states(states_drawn = cbind(exp(Xa1[1, ]), exp(Xa2[1, ]),
  #                                          exp(Xa3[1, ]), exp(Xa4[1, ]),
  #                                          exp(Xa5[1, ]), exp(Xa6[1, ])),
  #                     states_comp  = cbind(xa1_t, xa2_t,
  #                                          xa3_t, xa4_t,
  #                                          xa5_t, xa6_t),
  #                     #            cbind(exp(states_init_1), exp(states_init_2),
  #                     #                  exp(states_init_3), exp(states_init_4),
  #                     #                  exp(states_init_5), exp(states_init_6)),
  #                     current = 1, total = 1, num_prints = 1)
  # Run MCMC loop
  for (m in 2:MM) {
    # I. Run GIBBS part
    # 1. pars for xa processes -------------------------------------------
    for (d in 1:DD) {
      err_sig_sq_x <- Xa[m - 1, 2:TT, d] - f(x_tt = Xa[m - 1, 1:(TT - 1), d],
                                             z = Za[2:TT, (id_zet[d] + 1):id_zet[d + 1], drop = F],
                                             phi_x = phi_xa[d, m - 1],
                                             bet_x = bet_xa[(id_bet[d] + 1):id_bet[d + 1], m - 1])
      sig_sq_xa[d, m]  <- 1/rgamma(n = 1, prior_a,
                                   prior_b + crossprod(err_sig_sq_x)/2)
      regs[, (id_reg[d] + 1 + 1*d) - d]  <- Xa[m - 1, 1:(TT - 1), d]
      x_lhs        <- Xa[m - 1, 2:TT, d]
      Omega_xad    <- solve(crossprod(regs[, (id_reg[d] + 1):id_reg[d + 1]], regs[, (id_reg[d] + 1):id_reg[d + 1]])/sig_sq_xa[d, m] + prior_V_xa[[d]])
      mu_xad       <- Omega_xad %*% (crossprod(regs[, (id_reg[d] + 1):id_reg[d + 1]], x_lhs)/sig_sq_xa[d, m])
      beta_xad     <- mvrnorm(n = 1, mu = mu_xad, Sigma = Omega_xad)
      phi_xa[d, m] <- beta_xad[1]
      bet_xa[(id_bet[d] + 1):id_bet[d + 1], m] <- beta_xad[-1]
      while (near(abs(phi_xa[d, m]), 1, tol = 0.01) | abs(phi_xa[d, m]) > 1) {
        beta_xad       <- mvrnorm(n = 1, mu = mu_xad, Sigma = Omega_xad)
        phi_xa[d, m]   <- beta_xad[1]
        bet_xa[(id_bet[d] + 1):id_bet[d + 1], m] <- beta_xad[-1]
      }
      bet_xa[(id_bet[d] + 1):id_bet[d + 1], m] <- beta_xad[-1]
    }
    # II. Run cBPF-AS part
    # cbpf_as_c2 # cBPF_as
    out_cPF <- cbpf_as_c2(y = y, num_counts = num_counts,
                          Za1 = Za[, (id_zet[1] + 1):id_zet[2]], Za2 = Za[, (id_zet[2] + 1):id_zet[3]],
                          Za3 = Za[, (id_zet[3] + 1):id_zet[4]], Za4 = Za[, (id_zet[4] + 1):id_zet[5]],
                          Za5 = Za[, (id_zet[5] + 1):id_zet[6]], Za6 = Za[, (id_zet[6] + 1):id_zet[7]],
                          N = N, TT = TT,
                          sig_sq_xa1 = sig_sq_xa[1, m],
                          phi_xa1 = phi_xa[1, m],
                          bet_xa1 = bet_xa[(id_bet[1] + 1):id_bet[2], m, drop = F],
                          xa1_r = Xa[m - 1, , 1],
                          sig_sq_xa2 = sig_sq_xa[2, m],
                          phi_xa2 = phi_xa[2, m],
                          bet_xa2 = bet_xa[(id_bet[2] + 1):id_bet[3], m, drop = F],
                          xa2_r = Xa[m - 1, , 2],
                          sig_sq_xa3 = sig_sq_xa[3, m],
                          phi_xa3 = phi_xa[3, m],
                          bet_xa3 = bet_xa[(id_bet[3] + 1):id_bet[4], m, drop = F],
                          xa3_r = Xa[m - 1, , 3],
                          sig_sq_xa4 = sig_sq_xa[4, m],
                          phi_xa4 = phi_xa[4, m],
                          bet_xa4 = bet_xa[(id_bet[4] + 1):id_bet[5], m, drop = F],
                          xa4_r = Xa[m - 1,  , 4],
                          sig_sq_xa5 = sig_sq_xa[5, m],
                          phi_xa5 = phi_xa[5, m],
                          bet_xa5 = bet_xa[(id_bet[5] + 1):id_bet[6], m, drop = F],
                          xa5_r = Xa[m - 1,  , 5],
                          sig_sq_xa6 = sig_sq_xa[6, m],
                          phi_xa6 = phi_xa[6, m],
                          bet_xa6 = bet_xa[(id_bet[6] + 1):id_bet[7], m, drop = F],
                          xa6_r = Xa[m - 1,  , 6])
    w        <- out_cPF[[1]][, TT]
    b        <- sample.int(n = N, size = 1, replace = TRUE, prob = w)
    for (d in 1:DD) {
      Xa[m, , d] <- out_cPF[[d + 1]][b, ]
    }
    cat("Iteration number:", m, "\n")
    # monitor_pgas_states(states_drawn = cbind(exp(Xa1[m, ]), exp(Xa2[m, ]),
    #                                          exp(Xa3[m, ]), exp(Xa4[m, ]),
    #                                          exp(Xa5[m, ]), exp(Xa6[m, ])),
    #                     # states_comp = cbind(exp(states_init_1), exp(states_init_2),
    #                     #                      exp(states_init_3), exp(states_init_4),
    #                     #                      exp(states_init_5), exp(states_init_6)),
    #                     states_comp = cbind(exp(Xa1[m - 1, ]), exp(Xa2[m - 1, ]),
    #                                         exp(Xa3[m - 1, ]), exp(Xa4[m - 1, ]),
    #                                         exp(Xa5[m - 1, ]), exp(Xa6[m - 1, ])),
    #                     # NULL,
    #                     # cbind(xa1_t, xa2_t, xa3_t,
    #                     #                    xa4_t, xa5_t, xa6_t),
    #                     current = m, total = MM,
    #                     num_prints = num_plots_states)
    # monitor_pgas_time(m, MM, len = MM)
    # monitor_pgas_mcmc2(m, MM, len = MM,
    #                    val_init = par_init,
    #                    current_pars = cbind(sig_sq_xa1[1:m], phi_xa1[1:m],
    #                                         t(bet_xa1)[1:m,],
    #                                         sig_sq_xa2[1:m], phi_xa2[1:m],
    #                                         t(bet_xa2)[1:m,],
    #                                         sig_sq_xa3[1:m], phi_xa3[1:m],
    #                                         t(bet_xa3)[1:m,],
    #                                         sig_sq_xa4[1:m], phi_xa4[1:m],
    #                                         t(bet_xa4)[1:m,],
    #                                         sig_sq_xa5[1:m], phi_xa5[1:m],
    #                                         t(bet_xa5)[1:m,],
    #                                         sig_sq_xa6[1:m], phi_xa6[1:m],
    #                                         t(bet_xa6)[1:m,]),
    #                    dim_all = dim_all)
    # monitor_pgas_mcmc2(m, MM, len = MM,
    #                    val_true = par_true,
    #                    val_init = par_init,
    #                    current_pars = cbind(sig_sq_xa1[1:m], phi_xa1[1:m],
    #                                         t(bet_xa1)[1:m,],
    #                                         sig_sq_xa2[1:m], phi_xa2[1:m],
    #                                         t(bet_xa2)[1:m,],
    #                                         sig_sq_xa3[1:m], phi_xa3[1:m],
    #                                         t(bet_xa3)[1:m,],
    #                                         sig_sq_xa4[1:m], phi_xa4[1:m],
    #                                         t(bet_xa4)[1:m,],
    #                                         sig_sq_xa5[1:m], phi_xa5[1:m],
    #                                         t(bet_xa5)[1:m,],
    #                                         sig_sq_xa6[1:m], phi_xa6[1:m],
    #                                         t(bet_xa6)[1:m,]),
    #                    dim_all = dim_all)
  }
  return(list(sigma_sq_xa1 = sig_sq_xa[1, ],
              phi_xa1 = phi_xa[1, ],
              bet_xa1 = bet_xa[(id_bet[1] + 1):id_bet[2], ],
              sigma_sq_xa2 = sig_sq_xa[2, ],
              phi_xa2 = phi_xa[2, ],
              bet_xa2 = bet_xa[(id_bet[2] + 1):id_bet[3], ],
              sigma_sq_xa3 = sig_sq_xa[3, ],
              phi_xa3 = phi_xa[3, ],
              bet_xa3 = bet_xa[(id_bet[3] + 1):id_bet[4], ],
              sigma_sq_xa4 = sig_sq_xa[4, ],
              phi_xa4 = phi_xa[4, ],
              bet_xa4 = bet_xa[(id_bet[4] + 1):id_bet[5], ],
              sigma_sq_xa5 = sig_sq_xa[5, ],
              phi_xa5 = phi_xa[5, ],
              bet_xa5 = bet_xa[(id_bet[5] + 1):id_bet[6], ],
              sigma_sq_xa6 = sig_sq_xa[6, ],
              phi_xa6 = phi_xa[6, ],
              bet_xa6 = bet_xa[(id_bet[6] + 1):id_bet[7], ],
              xtraj  = list(Xa[, , 1], Xa[, , 2], Xa[, , 3], Xa[, , 4], Xa[, , 5], Xa[, , 6])))
}
############################################################################
# # 1. pars for xa1_t process --------------------------------------------
# err_sig_sq_x <- Xa[m - 1, 2:TT, 1] - f(x_tt = Xa[m - 1, 1:(TT - 1), 1],
#                                        z = Za[2:TT, (id_zet[1] + 1):id_zet[2], drop = F],
#                                        phi_x = phi_xa[1, m - 1],
#                                        bet_x = bet_xa[(id_bet[1] + 1):id_bet[2], m - 1])
# sig_sq_xa[1, m]  <- 1/rgamma(n = 1, prior_a,
#                              prior_b + crossprod(err_sig_sq_x)/2)
# # regs_a1[, 1] <- Xa[m - 1, 1:(TT - 1), 1]
# regs[, (id_reg[1] + 1 + 1*1) - 1]  <- Xa[m - 1, 1:(TT - 1), 1]
# x_lhs     <- Xa[m - 1, 2:TT, 1]
# Omega_xa1 <- solve(crossprod(regs[, (id_reg[1] + 1):id_reg[2]], regs[, (id_reg[1] + 1):id_reg[2]])/sig_sq_xa[1, m] + prior_V_xa1)
# mu_xa1       <- Omega_xa1 %*% (crossprod(regs[, (id_reg[1] + 1):id_reg[2]], x_lhs)/sig_sq_xa[1, m])
# beta_xa1     <- mvrnorm(n = 1, mu = mu_xa1, Sigma = Omega_xa1)
# phi_xa[1, m] <- beta_xa1[1]
# bet_xa[(id_bet[1] + 1):id_bet[2], m] <- beta_xa1[-1]
# while (near(abs(phi_xa[1, m]), 1, tol = 0.01) | abs(phi_xa[1, m]) > 1) {
#   beta_xa1     <- mvrnorm(n = 1, mu = mu_xa1, Sigma = Omega_xa1)
#   phi_xa[1, m]   <- beta_xa1[1]
#   bet_xa[(id_bet[1] + 1):id_bet[2], m] <- beta_xa1[-1]
# }
# # 2. pars for xa2_t process --------------------------------------------
# err_sig_sq_x <- Xa[m - 1, 2:TT, 2] - f(x_tt =  Xa[m - 1, 1:(TT - 1), 2],
#                                      z = Za[2:TT, (id_zet[2] + 1):id_zet[3], drop = F],
#                                      phi_x = phi_xa[2, m - 1],
#                                      bet_x = bet_xa[(id_bet[2] + 1):id_bet[3], m - 1])
# sig_sq_xa[2, m]  <- 1/rgamma(n = 1, prior_a,
#                            prior_b + crossprod(err_sig_sq_x)/2)
# # regs_a2[, 1] <- Xa[m - 1, 1:(TT - 1), 2]
# regs[, (id_reg[2] + 1 + 1*2) - 2]  <- Xa[m - 1, 1:(TT - 1), 2]
# x_lhs        <- Xa[m - 1, 2:TT, 2]
# Omega_xa2    <- solve(crossprod(regs[, (id_reg[2] + 1):id_reg[3]], regs[, (id_reg[2] + 1):id_reg[3]])/sig_sq_xa[2, m] + prior_V_xa2)
# mu_xa2       <- Omega_xa2 %*% (crossprod(regs[, (id_reg[2] + 1):id_reg[3]], x_lhs)/sig_sq_xa[2, m])
# beta_xa2     <- mvrnorm(n = 1, mu = mu_xa2, Sigma = Omega_xa2)
# phi_xa[2, m]   <- beta_xa2[1]
# bet_xa[(id_bet[2] + 1):id_bet[3], m] <- beta_xa2[-1]
# while (near(abs(phi_xa[2, m]), 1, tol = 0.01) | abs(phi_xa[2, m]) > 1) {
#   beta_xa2     <- mvrnorm(n = 1, mu = mu_xa2, Sigma = Omega_xa2)
#   phi_xa[2, m]   <- beta_xa2[1]
#   bet_xa[(id_bet[2] + 1):id_bet[3], m] <- beta_xa2[-1]
# }
# bet_xa[(id_bet[2] + 1):id_bet[3], m] <- beta_xa2[-1]
# # 3. pars for xa3_t process --------------------------------------------
# err_sig_sq_x <- Xa[m - 1, 2:TT, 3] - f(x_tt =  Xa[m - 1, 1:(TT - 1), 3],
#                                      z = Za[2:TT, (id_zet[3] + 1):id_zet[4], drop = F],
#                                      phi_x = phi_xa[3, m - 1],
#                                      bet_x = bet_xa[(id_bet[3] + 1):id_bet[4], m - 1])
# sig_sq_xa[3, m]  <- 1/rgamma(n = 1, prior_a,
#                            prior_b + crossprod(err_sig_sq_x)/2)
# # regs_a3[, 1] <- Xa[m - 1, 1:(TT - 1), 3]
# regs[, (id_reg[3] + 1 + 1*3) - 3]  <- Xa[m - 1, 1:(TT - 1), 3]
# x_lhs        <- Xa[m - 1, 2:TT, 3]
# Omega_xa3    <- solve(crossprod(regs[, (id_reg[3] + 1):id_reg[4]], regs[, (id_reg[3] + 1):id_reg[4]])/sig_sq_xa[3, m] + prior_V_xa3)
# mu_xa3       <- Omega_xa3 %*% (crossprod(regs[, (id_reg[3] + 1):id_reg[4]], x_lhs)/sig_sq_xa[3, m])
# beta_xa3     <- mvrnorm(n = 1, mu = mu_xa3, Sigma = Omega_xa3)
# phi_xa[3, m]   <- beta_xa3[1]
# bet_xa[(id_bet[3] + 1):id_bet[4], m] <- beta_xa3[-1]
# while (near(abs(phi_xa[3, m]), 1, tol = 0.01) | abs(phi_xa[3, m]) > 1) {
#   beta_xa3     <- mvrnorm(n = 1, mu = mu_xa3, Sigma = Omega_xa3)
#   phi_xa[3, m]   <- beta_xa3[1]
#   bet_xa[(id_bet[3] + 1):id_bet[4], m] <- beta_xa3[-1]
# }
# bet_xa[(id_bet[3] + 1):id_bet[4], m] <- beta_xa3[-1]
# # 4. pars for xa4_t process --------------------------------------------
# err_sig_sq_x <- Xa[m - 1, 2:TT, 4] - f(x_tt = Xa[m - 1, 1:(TT - 1), 4],
#                                      z = Za[2:TT, (id_zet[4] + 1):id_zet[5], drop = F],
#                                      phi_x = phi_xa[4, m - 1],
#                                      bet_x = bet_xa[(id_bet[4] + 1):id_bet[5], m - 1])
# sig_sq_xa[4, m]  <- 1/rgamma(n = 1, prior_a,
#                            prior_b + crossprod(err_sig_sq_x)/2)
# # regs_a4[, 1] <- Xa[m - 1, 1:(TT - 1), 4]
# regs[, (id_reg[4] + 1 + 1*4) - 4]  <- Xa[m - 1, 1:(TT - 1), 4]
# x_lhs        <- Xa[m - 1, 2:TT, 4]
# Omega_xa4    <- solve(crossprod(regs[, (id_reg[4] + 1):id_reg[5]], regs[, (id_reg[4] + 1):id_reg[5]])/sig_sq_xa[4, m] + prior_V_xa4)
# mu_xa4       <- Omega_xa4 %*% (crossprod(regs[, (id_reg[4] + 1):id_reg[5]], x_lhs)/sig_sq_xa[4, m])
# beta_xa4     <- mvrnorm(n = 1, mu = mu_xa4, Sigma = Omega_xa4)
# phi_xa[4, m] <- beta_xa4[1]
# bet_xa[(id_bet[4] + 1):id_bet[5], m] <- beta_xa4[-1]
# while (near(abs(phi_xa[4, m]), 1, tol = 0.01) | abs(phi_xa[4, m]) > 1) {
#   beta_xa4     <- mvrnorm(n = 1, mu = mu_xa4, Sigma = Omega_xa4)
#   phi_xa[4, m]   <- beta_xa4[1]
#   bet_xa[(id_bet[4] + 1):id_bet[5], m] <- beta_xa4[-1]
# }
# bet_xa[(id_bet[4] + 1):id_bet[5], m] <- beta_xa4[-1]
# # 5. pars for xa5_t process --------------------------------------------
# err_sig_sq_x <- Xa[m - 1, 2:TT, 5] - f(x_tt = Xa[m - 1, 1:(TT - 1), 5],
#                                      z = Za[2:TT, (id_zet[5] + 1):id_zet[6], drop = F],
#                                      phi_x = phi_xa[5, m - 1],
#                                      bet_x = bet_xa[(id_bet[5] + 1):id_bet[6], m - 1])
# sig_sq_xa[5, m]  <- 1/rgamma(n = 1, prior_a,
#                            prior_b + crossprod(err_sig_sq_x)/2)
# # regs_a5[, 1] <- Xa[m - 1, 1:(TT - 1), 5]
# regs[, (id_reg[5] + 1 + 1*5) - 5]  <- Xa[m - 1, 1:(TT - 1), 5]
# x_lhs        <- Xa[m - 1, 2:TT, 5]
# Omega_xa5    <- solve(crossprod(regs[, (id_reg[5] + 1):id_reg[6]], regs[, (id_reg[5] + 1):id_reg[6]])/sig_sq_xa[5, m] + prior_V_xa5)
# mu_xa5       <- Omega_xa5 %*% (crossprod(regs[, (id_reg[5] + 1):id_reg[6]], x_lhs)/sig_sq_xa[5, m])
# beta_xa5     <- mvrnorm(n = 1, mu = mu_xa5, Sigma = Omega_xa5)
# phi_xa[5, m] <- beta_xa5[1]
# bet_xa[(id_bet[5] + 1):id_bet[6], m] <- beta_xa5[-1]
# while (near(abs(phi_xa[5, m]), 1, tol = 0.01) | abs(phi_xa[5, m]) > 1) {
#   beta_xa5     <- mvrnorm(n = 1, mu = mu_xa5, Sigma = Omega_xa5)
#   phi_xa[5, m] <- beta_xa5[1]
#   bet_xa[(id_bet[5] + 1):id_bet[6], m] <- beta_xa5[-1]
# }
# bet_xa[(id_bet[5] + 1):id_bet[6], m] <- beta_xa5[-1]
# ############################################################################
# # 6. pars for xa6_t process --------------------------------------------
# err_sig_sq_x <- Xa[m - 1, 2:TT, 6] - f(x_tt = Xa[m - 1, 1:(TT - 1), 6],
#                                      z = Za[2:TT, (id_zet[6] + 1):id_zet[7], drop = F],
#                                      phi_x = phi_xa[6, m - 1],
#                                      bet_x = bet_xa[(id_bet[6] + 1):id_bet[7], m - 1])
# sig_sq_xa[6, m]  <- 1/rgamma(n = 1, prior_a,
#                            prior_b + crossprod(err_sig_sq_x)/2)
# regs_a6[, 1] <- Xa[m - 1, 1:(TT - 1), 6]
# regs[, (id_reg[6] + 1 + 1*6) - 6]  <- Xa[m - 1, 1:(TT - 1), 6]
# x_lhs        <- Xa[m - 1, 2:TT, 6]
# # Omega_xa6    <- solve(crossprod(regs_a6, regs_a6)/sig_sq_xa[6, m] + prior_V_xa6)
# Omega_xa6    <- solve(crossprod(regs[, (id_reg[6] + 1):id_reg[7]], regs[, (id_reg[6] + 1):id_reg[7]])/sig_sq_xa[6, m] + prior_V_xa6)
# mu_xa6       <- Omega_xa6 %*% (crossprod(regs[, (id_reg[6] + 1):id_reg[7]], x_lhs)/sig_sq_xa[6, m])
# beta_xa6     <- mvrnorm(n = 1, mu = mu_xa6, Sigma = Omega_xa6)
# phi_xa[6, m]   <- beta_xa6[1]
# bet_xa[(id_bet[6] + 1):id_bet[7], m] <- beta_xa6[-1]
# while (near(abs(phi_xa[6, m]), 1, tol = 0.01) | abs(phi_xa[6, m]) > 1) {
#   beta_xa6     <- mvrnorm(n = 1, mu = mu_xa6, Sigma = Omega_xa6)
#   phi_xa[6, m]   <- beta_xa6[1]
#   bet_xa[(id_bet[6] + 1):id_bet[7], m] <- beta_xa6[-1]
# }
# bet_xa[(id_bet[6] + 1):id_bet[7], m] <- beta_xa6[-1]
############################################################################
