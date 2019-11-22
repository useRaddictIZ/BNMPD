if (simulate_data) {
  path_sim_res <- "~/Dropbox/research/KZ/results/simulations/2019-11-18/"
  res <- out_pgas_sim
  sub_name_plots <- "sim"
  sub_name_table <- "sim"
  sub_name_ur    <- "00_UR_sim"
  par_mcmc  <- rbind(res$sigma_sq_xa1, res$phi_xa1, res$bet_xa1,
                     res$sigma_sq_xa2, res$phi_xa2, res$bet_xa2,
                     res$sigma_sq_xa3, res$phi_xa3, res$bet_xa3,
                     res$sigma_sq_xa4, res$phi_xa4, res$bet_xa4,
                     res$sigma_sq_xa5, res$phi_xa5, res$bet_xa5,
                     res$sigma_sq_xa6, res$phi_xa6, res$bet_xa6)

  par_names <- c("sigma_sq_xa1", "phi_xa1", "const_xa1",
                 paste("bet_xa1", 1:(length(true_bet_xa1) - 1), sep = "_"),
                 "sigma_sq_xa2", "phi_xa2", "const_xa2",
                 paste("bet_xa2", 1:(length(true_bet_xa2) - 1), sep = "_"),
                 "sigma_sq_xa3", "phi_xa3", "const_xa3",
                 paste("bet_xa3", 1:(length(true_bet_xa3) - 1), sep = "_"),
                 "sigma_sq_xa4", "phi_xa4", "const_xa4",
                 paste("bet_xa4", 1:(length(true_bet_xa4) - 1), sep = "_"),
                 "sigma_sq_xa5", "phi_xa5", "const_xa5",
                 paste("bet_xa5", 1:(length(true_bet_xa5) - 1), sep = "_"),
                 "sigma_sq_xa6", "phi_xa6", "const_xa6",
                 paste("bet_xa6", 1:(length(true_bet_xa6) - 1), sep = "_"))

  analyse_mcmc_convergence(mcmc_sims  = par_mcmc,
                           start_vals = unlist(par_init[1:D]),
                           par_names  = par_names,
                           true_vals = true_vals,
                           states = res$xtraj,
                           burn = burnin,
                           plot_view = FALSE,
                           plot_ggp2 = TRUE,
                           plot_save = TRUE,
                           plot_path = file.path(path_sim_res, "convergence"),
                           plot_name = sub_name_plots,
                           table_view = TRUE,
                           table_save = TRUE,
                           table_path = file.path(path_sim_res, "inference"),
                           table_name = sub_name_table,
                           ur_view    = TRUE)
} else if (!simulate_data) {
  path_us_energy
  if (state_run == "TX") {
    res <- out_pgas_TX
    sub_name_plots <- "TX"
    # sub_name_table <- ".csv"
    sub_name_ur    <- "00_UR_TX"
  } else if (state_run == "CA") {
    res <- out_pgas_CA
    sub_name_plots <- "CA"
    # sub_name_table <- ".csv"
    sub_name_ur    <- "00_UR_CA"
  }  else if (state_run == "NE") {
    res <- out_pgas_NE
    sub_name_plots <- "NE"
    sub_name_table <- "NE"
    sub_name_ur    <- "00_UR_NE"
  }
  par_mcmc  <- rbind(res$sigma_sq_xa1, res$phi_xa1, res$bet_xa1,
                     res$sigma_sq_xa2, res$phi_xa2, res$bet_xa2,
                     res$sigma_sq_xa3, res$phi_xa3, res$bet_xa3,
                     res$sigma_sq_xa4, res$phi_xa4, res$bet_xa4,
                     res$sigma_sq_xa5, res$phi_xa5, res$bet_xa5,
                     res$sigma_sq_xa6, res$phi_xa6, res$bet_xa6)
  reg_names <- c("const", "cleid", "dfeid", "ngeid")
  par_names <- c("CLEIB_sigma_sq", "CLEIB_phi",
                 paste("CLEIB_bet", reg_names, sep = "_"),
                 "NGEIB_sigma_sq", "NGEIB_phi",
                 paste("NGEIB_bet", reg_names, sep = "_"),
                 "PAEIB_sigma_sq", "PAEIB_phi",
                 paste("PAEIB_bet", reg_names, sep = "_"),
                 "HYEGB_sigma_sq", "HYEGB_phi",
                 paste("HYEGB_bet", reg_names, sep = "_"),
                 "NUEGB_sigma_sq", "NUEGB_phi",
                 paste("NUEGB_bet", reg_names, sep = "_"),
                 "renwes_sigma_sq", "renews_phi",
                 paste("renwes_bet", reg_names, sep = "_"))

  analyse_mcmc_convergence2(mcmc_sims  = par_mcmc,
                            start_vals = unlist(par_init[1:D]),
                            par_names  = par_names,
                            states = res$xtraj,
                            burn = burnin,
                            plot_view = TRUE,
                            plot_ggp2 = FALSE,
                            plot_save = FALSE,
                            plot_path = file.path(path_us_energy, "convergence"),
                            plot_name = sub_name_plots,
                            table_view = TRUE,
                            table_save = FALSE,
                            table_path = file.path(path_us_energy, "inference"),
                            table_name = sub_name_table,
                            ur_view    = TRUE)
}
