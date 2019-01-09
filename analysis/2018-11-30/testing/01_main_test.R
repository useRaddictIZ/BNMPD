############################## PGAS for KZ model ###############################
rm(list = ls())
source("./R/helper/00_helper_lib_load.R")
source("./R/helper/00_helper_model_fcts.R")
source("./R/helper/00_helper_simulation_data.R")
source("./R/helper/99_helper_diagnostics.R")

source("./R/helper/01_helper_cBPF_as.R")
source("./R/helper/02_helper_pgas.R")
source("./R/01_cBPF_as.R")
source("./R/02_pgas.R")
# PGAS run ----------------------------------------------------------------
init_at_true  <- TRUE
pgas_run      <- TRUE
set.seed(123)
source("./analysis/2018-11-30/testing/00_settings_simulation_test.R")
source("./analysis/2018-11-30/testing/00_settings_simulation_test_init.R")
out_pgas <- pgas(N = num_particles, MM = num_mcmc, TT = TT,
                 y = y_t,
                 Za1 = za1_t, Za2 = za2_t,
                 Za3 = za3_t, Za4 = za4_t,
                 priors = c(prior_a, prior_b),
                 par_init = par_init,
                 par_true = true_vals,
                 traj_init = deviate_states_init,
                 filtering = pgas_run,
                 num_plots_states = 1)
source("./analysis/2018-11-30/testing/99_analyse_convergence_test.R")
