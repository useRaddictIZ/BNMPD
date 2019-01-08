# pgas_run <- F
if (pgas_run) {
  res <- out_pgas
  sub_folder_name <- "pgas"
  sub_name   <- "pgas"
} else {
  res <- out_gibbs
  sub_folder_name <- "gibbs"
  sub_name   <- "gibbs"
}
par_mcmc  <- rbind(res$sigma_sq_xa, res$phi_xa, res$bet_xa,
                   res$sigma_sq_xb, res$phi_xb, res$bet_xb,
                   res$sigma_sq_xp, res$phi_xp, res$bet_xp,
                   res$sigma_sq_xq, res$phi_xq, res$bet_xq)
par_names <- c("sigma_sq_xa", "phi_xa",
               paste("bet_xa", 1:length(true_bet_xa), sep = "_"),
               "sigma_sq_xb", "phi_xb",
               paste("bet_xb", 1:length(true_bet_xb), sep = "_"),
               "sigma_sq_xp", "phi_xp",
               paste("bet_xp", 1:length(true_bet_xp), sep = "_"),
               "sigma_sq_xq", "phi_xq",
               paste("bet_xq", 1:length(true_bet_xq), sep = "_"))

analyse_mcmc_convergence(mcmc_sims  = par_mcmc,
                         true_vals  = unlist(par_true[1:4]),
                         start_vals = unlist(par_init[1:4]),
                         par_names  = par_names,
                         states = res$xtraj,
                         burn = burnin,
                         plot_view = FALSE,
                         plot_ggp2 = TRUE,
                         plot_save = TRUE,
                         plot_path = file.path(getwd(),
                                               "analysis",
                                               "2018-11-30",
                                               "doc",
                                               "fig",
                                               sub_folder_name),
                         plot_name = sub_folder_name,
                         table_view = TRUE,
                         table_name = sub_name,
                         ur_view    = TRUE)
