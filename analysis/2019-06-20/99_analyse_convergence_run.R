res <- out_pgas
sub_folder_name <- "pgas"
sub_name   <- "pgas"

par_mcmc  <- rbind(res$sigma_sq_xa1, res$phi_xa1, res$bet_xa1,
                   res$sigma_sq_xa2, res$phi_xa2, res$bet_xa2,
                   res$sigma_sq_xa3, res$phi_xa3, res$bet_xa3,
                   res$sigma_sq_xa4, res$phi_xa4, res$bet_xa4,
                   res$sigma_sq_xa5, res$phi_xa5, res$bet_xa5)
par_names <- c("sigma_sq_xa1", "phi_xa1",
               paste("bet_xa1", 1:length(init_bet_xa1), sep = "_"),
               "sigma_sq_xa2", "phi_xa2",
               paste("bet_xa2", 1:length(init_bet_xa2), sep = "_"),
               "sigma_sq_xa3", "phi_xa3",
               paste("bet_xa3", 1:length(init_bet_xa3), sep = "_"),
               "sigma_sq_xa4", "phi_xa4",
               paste("bet_xa4", 1:length(init_bet_xa4), sep = "_"),
               "sigma_sq_xa5", "phi_xa5",
               paste("bet_xa5", 1:length(init_bet_xa5), sep = "_"))

analyse_mcmc_convergence2(mcmc_sims  = par_mcmc,
                          start_vals = unlist(par_init[1:5]),
                          par_names  = par_names,
                          states = res$xtraj,
                          burn = burnin,
                          plot_view = FALSE,
                          plot_ggp2 = TRUE,
                          plot_save = TRUE,
                          plot_path = file.path(getwd(),
                                                "analysis",
                                                "2019-06-20",
                                                "fig",
                                                sub_folder_name),
                          plot_name = sub_folder_name,
                          table_view = TRUE,
                          table_name = sub_name,
                          ur_view    = TRUE)
