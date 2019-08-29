############################## PGAS for KZ model ###############################
rm(list = ls())
source_all <- function() {
  dir <- paste0(getwd(),"/R/")
  file_names <- paste0(dir, list.files(dir, recursive = TRUE))
  invisible(sapply(file_names, source))
}
source_all()
# PGAS run ----------------------------------------------------------------
simulate_data <- T
init_at_true  <- F
pgas_run      <- F
if (simulate_data) {
  set.seed(2016)# set.seed(42) # set.seed(3) # set.seed(139423) # T=100,50,200 don't "really" work
  source("./analysis/2019-06-19/00_settings_simulation_data.R")
  source("./analysis/2019-06-19/00_settings_simulation_init.R")
} else {
  source("./analysis/2019-06-19/00_settings_simulation_init.R")
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
source("./analysis/2019-06-19/99_analyse_convergence_run.R")
