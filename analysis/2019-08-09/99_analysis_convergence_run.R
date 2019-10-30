if (state_run == "TX") {
  res <- out_pgas_TX
  sub_name_plots <- "TX"
  sub_name_table <- "TX.csv"

} else if (state_run == "CA") {
  res <- out_pgas_CA
  sub_name_plots <- "CA"
  sub_name_table <- "CA.csv"
}

par_mcmc  <- rbind(res$sigma_sq_xa1, res$phi_xa1, res$bet_xa1,
                   res$sigma_sq_xa2, res$phi_xa2, res$bet_xa2,
                   res$sigma_sq_xa3, res$phi_xa3, res$bet_xa3,
                   res$sigma_sq_xa4, res$phi_xa4, res$bet_xa4,
                   res$sigma_sq_xa5, res$phi_xa5, res$bet_xa5,
                   res$sigma_sq_xa6, res$phi_xa6, res$bet_xa6)
reg_names <- c("const", "cleid", "dfeid", "estcd", "ngeid")
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

path_us_energy <- file.path("~/Dropbox/research/usa_energy/01-analysis-results/empirical-analysis/small-models-test-estimations/04-results/")

analyse_mcmc_convergence2(mcmc_sims  = par_mcmc,
                          start_vals = unlist(par_init[1:D]),
                          par_names  = par_names,
                          states = res$xtraj,
                          burn = burnin,
                          plot_view = FALSE,
                          plot_ggp2 = FALSE,
                          plot_save = FALSE,
                          plot_path = file.path(path_us_energy, "convergence"),
                          plot_name = sub_name_plots,
                          table_view = TRUE,
                          table_save = FALSE,
                          table_path = file.path(path_us_energy, "inference"),
                          table_name = sub_name_table,
                          ur_view    = FALSE)
