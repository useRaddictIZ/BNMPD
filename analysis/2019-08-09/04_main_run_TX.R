############################## PGAS for KZ model ###############################
rm(list = ls())
source_all <- function() {
  dir <- paste0(getwd(),"/R/")
  file_names <- paste0(dir, list.files(dir, recursive = TRUE))
  invisible(sapply(file_names, source))
}
source_all()
# set.seed(123)
# PGAS run ----------------------------------------------------------------
simulate_data <- T
init_at_true  <- F
pgas_run      <- T

if (simulate_data) {
  set.seed(2016)# set.seed(42) # set.seed(3) # set.seed(139423) # T=100,50,200 don't "really" work
  source("./analysis/2019-08-09/02_simulation_data.R")
  source("./analysis/2019-08-09/02_simulation_init.R")
  state_run <- NULL
} else {
  state_run     <- "NE"
  # dep_scale <- 1e2
  # source("./analysis/2019-08-09/00_preparation_real_data.R")
  # data_CA <- read_xlsx("data/tidy/shares_mwatts_prices/data_test_01_CA.xlsx")
  # data_TX <- read_xlsx("data/tidy/shares_mwatts_prices/data_test_01_TX.xlsx")
  data_current <- data_current <- haven::read_dta("./data/uspp_aggregated_final.dta")
  data_current <- data_current %>% filter(state == state_run)
  source("./analysis/2019-08-09/03_real_data_init.R")
}
# out_pgas_NE <- pgas(N = num_particles, MM = num_mcmc, TT = TT,
#                     y = y_t,
#                     Za1 = za1_t, Za2 = za2_t,
#                     Za4 = za4_t, Za3 = za3_t,
#                     Za5 = za5_t, Za6 = za6_t,
#                     priors = c(prior_a, prior_b),
#                     par_init = par_init,
#                     traj_init = states_init,
#                     filtering = TRUE,
#                     num_plots_states = 20)
out_pgas_sim <- pgas2(N = num_particles, MM = num_mcmc, TT = TT,
                      y = y_t, num_counts = num_counts,
                      Za1 = za1_t, Za2 = za2_t,
                      Za4 = za4_t, Za3 = za3_t,
                      Za5 = za5_t, Za6 = za6_t,
                      priors = c(prior_a, prior_b),
                      par_init = par_init,
                      traj_init = deviate_states_init,
                      filtering = TRUE,
                      num_plots_states = 20)
source("./analysis/2019-08-09/99_analyze_convergence.R")
