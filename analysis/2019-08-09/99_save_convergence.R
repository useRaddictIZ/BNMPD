library(tidyverse)
library(gridExtra)
path_us_energy      <- file.path("~/Dropbox/research/usa_energy")
path_us_energy      <- file.path(path_us_energy, "03-analysis-results")
path_us_energy      <- file.path(path_us_energy, "empirical-analysis/CA-TX-estimations")

type_of_analysis    <- c("01_CA", "01_TX", "02_CA", "02_TX", "03_CA", "03_TX")
result_names <- c("01_CA_reg_unscaled.RData",
                  "01_TX_reg_unscaled.RData",
                  "02_CA_reg_unscaled_lagged_all.RData",
                  "02_TX_reg_unscaled_lagged_all.RData",
                  "03_CA_reg_unscaled_lagged_cross_prices_contemp_own_price.RData",
                  "03_TX_reg_unscaled_lagged_cross_prices_contemp_own_price.RData")
pth_name <- "~/Dropbox/research/KZ/results/real_data/2019-11-15/99_settings_convergence_save.RData"
save(path_us_energy, type_of_analysis, result_names, file = pth_name)
for (i in 1:6) {
  load(file.path("~/Dropbox/research/KZ/results/real_data/2019-11-15", result_names[i]))

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
  lab_plain <- c("sd", "ar", "const", "prc_coal", "prc_oil", "prc_gas")
  lab_names <- c(paste0(lab_plain, "_", "COAL"),
                 paste0(lab_plain, "_", "GAS"),
                 paste0(lab_plain, "_", "OIL"),
                 paste0(lab_plain, "_", "HYDRO"),
                 paste0(lab_plain, "_", "NUCLEAR"),
                 paste0(lab_plain, "_", "RENEWS"))

  sub_name_table      <- paste0(type_of_analysis[i], ".csv")
  path_us_energy      <- file.path("~/Dropbox/research/usa_energy")
  path_us_energy      <- file.path(path_us_energy, "03-analysis-results")
  path_us_energy      <- file.path(path_us_energy, "empirical-analysis/CA-TX-estimations")
  source('~/Dropbox/research/KZ/R/helper/99_helper_diagnostics_real_data.R')
  analyse_mcmc_convergence2(mcmc_sims  = par_mcmc,
                            start_vals = unlist(par_init[1:D]),
                            par_names  = par_names,
                            lab_names  = lab_names,
                            states = res$xtraj,
                            burn = burnin,
                            plot_view = FALSE,
                            plot_ggp2 = FALSE,
                            plot_save = FALSE,
                            plot_path = file.path(path_us_energy, "convergence", type_of_analysis[i]),
                            plot_name = sub_name_plots,
                            table_view = FALSE,
                            table_save = TRUE,
                            table_path = file.path(path_us_energy, "inference"),
                            table_name = sub_name_table,
                            ur_view    = FALSE,
                            ur_save    = FALSE,
                            ur_path    = file.path(path_us_energy, "convergence", type_of_analysis[i]),
                            ur_name    = sub_name_ur)
  rm(list = ls())
  load( "~/Dropbox/research/KZ/results/real_data/2019-11-15/99_settings_convergence_save.RData")
}
