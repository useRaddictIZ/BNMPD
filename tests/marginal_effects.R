analyze_marginal_effect <- function(k, dd, MM,
                                    X, betas,
                                    num_counts,
                                    y_counts = NULL,
                                    regs = NULL,
                                    burnin,
                                    out_pgas,
                                    xa_t_all = NULL,
                                    true_bet_all = NULL,
                                    elasticity = TRUE,
                                    simulation = FALSE,
                                    hist_plots = FALSE,
                                    traj_plots = FALSE
                                    ) {
  MM <- nrow(X[[1]])
  TT <- ncol(X[[1]])
  # browser()
  D <- length(X)
  for (d in 1:D) {
    X[[d]] <- X[[d]][burnin:MM, ]
    betas[[d]] <- betas[[d]][, burnin:MM]
  }
  if (simulation) {
    true_marginal <- rep(0, times = TT)
    for (t in 1:TT) {
      # browser()
      lhs <- xa_t_all[t, dd]*true_bet_all[[dd]][[k]] * sum(xa_t_all[t, , drop = TRUE])
      rhs <- xa_t_all[t, dd]
      temp <- 0
      for (d in 1:D) {
        # browser()
        temp <- temp + sum(xa_t_all[t, d, drop = TRUE]*true_bet_all[[d]][[k]])
      }
      rhs <- temp*rhs
      bottom <-  (sum(xa_t_all[t, , drop = TRUE]))^2

      true_marginal[t] <- num_counts[t]*(lhs - rhs)/bottom
      if (elasticity) {
        # browser()
        true_marginal[t] <- true_marginal[t] * (regs/y_counts)[t]
      }
    }
  }

  # browser()
  f <- exp(X[[dd]])
  f_prime <- exp(X[[dd]]) * betas[[dd]][k, , drop = TRUE]
  g <- matrix(0, nrow = (MM - burnin + 1), ncol = TT)
  for (d in 1:D) {
    g <- g + exp(X[[d]])
  }
  g_squared <- g^2
  g_prime <- matrix(0, nrow = (MM - burnin + 1), ncol = TT)
  for (d in 1:D) {
    g_prime <- g_prime + exp(X[[d]]) * betas[[d]][k, , drop = TRUE]
  }
  out <- (f_prime*g - g_prime*f)/g_squared
  out <- t(out)*num_counts
  means_out <- rowMeans(out)
  if (elasticity) {
    # browser()
    means_out <- means_out * (regs/y_counts)
  }
  # browser()
  if (hist_plots) {
    if (simulation) {
      for (t in 1:TT) {
        hist(out[t, ], main = paste0("state no: ", dd, "for k = ", k, "regressor"))
        abline(v = means_out[t], col = "red")
        abline(v = true_marginal[t], col = "green")
      }
    } else {
      for (t in 1:TT) {
        hist(out[t, ], main = paste0("state no: ", dd, "for k = ", k, "regressor"))
        abline(v = means_out[t], col = "red")
      }
    }
  }
  if (traj_plots) {
    if (simulation) {
      plot(means_out, type = "l", col = "red", main = paste0("state no: ", dd, "for k = ", k, "regressor"))
      lines(true_marginal, type = "l", col = "green")
    } else {
      plot(means_out, type = "l", col = "red", main = paste0("state no: ", dd, "for k = ", k, "regressor"))
    }
  }


  # return(out)
  # out <- (f_prime*g - g_prime*f)/g_squared
  # out <- t(out)*num_counts
  # out <- rowMeans(out)
  # return(out)
}
sim_res <- TRUE
if (sim_res) {
  bet_all <- list(out_pgas_sim$bet_xa1,
                  out_pgas_sim$bet_xa2,
                  out_pgas_sim$bet_xa3,
                  out_pgas_sim$bet_xa4,
                  out_pgas_sim$bet_xa5,
                  out_pgas_sim$bet_xa6)
  y_counts <- y_t
  regs <- list(za1_t, za2_t, za3_t, za4_t, za5_t, za6_t)
  xa_t_all <- cbind(xa1_t, xa2_t, xa3_t, xa4_t, xa5_t, xa6_t)
  true_bet_all <- list(true_bet_xa1, true_bet_xa2, true_bet_xa3,
                       true_bet_xa4, true_bet_xa5, true_bet_xa6)
  reg_length <- unlist(lapply(true_bet_all, length))
  for (dd in 1:D) {
    for (kk in 2:reg_length[dd]) {
      analyze_marginal_effect(k = kk , dd = dd, MM = num_mcmc,
                              X = out_pgas_sim$xtraj,
                              betas = bet_all,
                              num_counts = num_counts,
                              y_counts = y_counts[, dd],
                              regs = regs[[dd]][, kk, drop = TRUE],
                              burnin = burnin,
                              out_pgas = out_pgas_sim,
                              xa_t_all = xa_t_all,
                              true_bet_all = true_bet_all,
                              simulation = TRUE,
                              traj_plots = TRUE)

    }
  }
} else if (!sim_res) {
  bet_all <- list(out_pgas_US$bet_xa1,
                  out_pgas_US$bet_xa2,
                  out_pgas_US$bet_xa3,
                  out_pgas_US$bet_xa4,
                  out_pgas_US$bet_xa5,
                  out_pgas_US$bet_xa6)
  reg_length <- unlist(lapply(bet_all, nrow))
  y_counts <- y_t
  regs <- list(za1_t, za2_t, za3_t, za4_t, za5_t, za6_t)
  for (dd in 1:D) {
    for (kk in 2:reg_length[dd]) {
      analyze_marginal_effect(k = kk , dd = dd, MM = num_mcmc,
                              X = out_pgas_US$xtraj,
                              betas = bet_all,
                              num_counts = num_counts,
                              y_counts = y_counts[, dd],
                              regs = regs[[dd]][, kk, drop = TRUE],
                              burnin = burnin,
                              out_pgas = out_pgas_US,
                              # xa_t_all = xa_t_all,
                              # true_bet_all = true_bet_all,
                              simulation = FALSE,
                              hist_plots = FALSE,
                              traj_plots = TRUE)
    }
  }
}
