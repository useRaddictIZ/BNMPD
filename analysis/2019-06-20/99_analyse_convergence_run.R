res <- out_pgas
sub_folder_name <- "pgas"
sub_name   <- "pgas"

par_mcmc  <- rbind(res$sigma_sq_xa1, res$phi_xa1, res$bet_xa1,
                   res$sigma_sq_xa2, res$phi_xa2, res$bet_xa2,
                   res$sigma_sq_xa3, res$phi_xa3, res$bet_xa3,
                   res$sigma_sq_xa4, res$phi_xa4, res$bet_xa4,
                   res$sigma_sq_xa5, res$phi_xa5, res$bet_xa5)
par_names <- c("sigma_sq_coal", "phi_coal",
               paste("bet_coal", 1:length(init_bet_xa1), sep = "_"),
               "sigma_sq_oil", "phi_oil",
               paste("bet_oil", 1:length(init_bet_xa2), sep = "_"),
               "sigma_sq_gas", "phi_gas",
               paste("bet_gas", 1:length(init_bet_xa3), sep = "_"),
               "sigma_sq_nuclear", "phi_nuclear",
               paste("bet_nuclear", 1:length(init_bet_xa4), sep = "_"),
               "sigma_sq_renewable", "phi_renewable",
               paste("bet_renewable", 1:length(init_bet_xa5), sep = "_"))

analyse_mcmc_convergence2(mcmc_sims  = par_mcmc,
                          start_vals = unlist(par_init[1:5]),
                          par_names  = par_names,
                          states = res$xtraj,
                          burn = burnin,
                          plot_view = FALSE,
                          plot_ggp2 = TRUE,
                          plot_save = TRUE,
                          plot_path = file.path(getwd(),
                                                "results",
                                                "real_data",
                                                "2019-06-20",
                                                "fig",
                                                sub_folder_name),
                          plot_name = sub_folder_name,
                          table_view = TRUE,
                          table_name = sub_name,
                          ur_view    = TRUE)
