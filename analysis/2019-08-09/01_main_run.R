############################## PGAS for KZ model ###############################
rm(list = ls())
# R.utils::sourceDirectory(path = paste0(getwd(),"/R/"))
sapply(paste0(getwd(),"/R/", list.files(paste0(getwd(),"/R/"), recursive = TRUE)), source)
# Analyse California data -------------------------------------------------


# PGAS run ----------------------------------------------------------------
simulate_data <- T
init_at_true  <- F
pgas_run      <- T
if (simulate_data) {
  set.seed(139423) # set.seed(3) #
  source("./analysis/2019-06-22/00_settings_simulation_data.R")
  source("./analysis/2019-06-22/00_settings_simulation_init.R")
} else {
  source("./analysis/2019-06-22/00_settings_simulation_init.R")
}
if (pgas_run) {
  out_pgas <- pgas(N = num_particles, MM = num_mcmc, TT = TT,
                   y = y_t,
                   Za1 = za1_t, Za2 = za2_t,
                   Za4 = za4_t, Za3 = za3_t, Za5 = za5_t,
                   priors = c(prior_a, prior_b),
                   par_init = par_init,
                   par_true = true_vals,
                   traj_init = deviate_states_init,
                   filtering = TRUE,
                   num_plots_states = 1)
} else {
  out_gibbs <- pgas(N = num_particles, MM = num_mcmc, TT = TT,
                    y = y_t,
                    Za1 = za1_t, Za2 = za2_t,
                    Za4 = za4_t, Za3 = za3_t, Za5 = za5_t,
                    priors = c(prior_a, prior_b),
                    par_init = par_init,
                    par_true = true_vals,
                    traj_init = deviate_states_init,
                    filtering = FALSE,
                    num_plots_states = 1)
}
source("./analysis/2019-06-22/99_analyse_convergence_run.R")
