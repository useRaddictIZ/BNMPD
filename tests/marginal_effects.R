if (simulate_data) {
  browser()
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
} else if (!simulate_data) {
  browser()
  bet_all <- list(res$bet_xa1,
                  res$bet_xa2,
                  res$bet_xa3,
                  res$bet_xa4,
                  res$bet_xa5,
                  res$bet_xa6)
  reg_length <- unlist(lapply(bet_all, nrow))
  y_counts <- y_t
  regs <- list(za1_t, za2_t, za3_t, za4_t, za5_t, za6_t)
  for (dd in 1:D) {
    for (kk in 2:reg_length[dd]) {
      analyze_marginal_effect(k = kk , dd = dd, MM = num_mcmc,
                              X = res$xtraj,
                              betas = bet_all,
                              num_counts = num_counts,
                              y_counts = y_counts[, dd],
                              regs = regs[[dd]][, kk, drop = TRUE],
                              burnin = burnin,
                              out_pgas = res,
                              eval_at_means = TRUE,
                              # xa_t_all = xa_t_all,
                              # true_bet_all = true_bet_all,
                              simulation = FALSE,
                              hist_plots = FALSE,
                              traj_plots = TRUE)
    }
  }
}
