############################## PGAS for KZ model ###############################
rm(list = ls())
source_all <- function() {
  dir_r <- paste0(getwd(),"/R/")
  file_names_r <- paste0(dir_r, list.files(dir_r, recursive = TRUE))
  dir_cpp <- paste0(getwd(),"/src/")
  file_names_cpp <- paste0(dir_cpp, list.files(dir_cpp, recursive = TRUE))
  # invisible(sapply(file_names_r, source))
  invisible(list(sapply(file_names_r, source), sapply(file_names_cpp, Rcpp::sourceCpp)))
}
# sapply(paste0(getwd(),"/src/", list.files(paste0(getwd(),"/src/"), recursive = TRUE)), Rcpp::sourceCpp)
# # R.utils::sourceDirectory(path = paste0(getwd(),"/R/"))
source_all()
# set.seed(123)
# PGAS run ----------------------------------------------------------------
pgas_run      <- T
state_run     <- "TX"
data_current <- data_current <- haven::read_dta("./data/uspp_aggregated_final.dta")
data_current <- data_current %>% filter(state == state_run)
source("./analysis/2019-08-09/03_real_data_init.R")
out_pgas_TX <- pgas1(N = num_particles, MM = num_mcmc, TT = TT,
                     y = y_t,
                     Za1 = za1_t, Za2 = za2_t,
                     Za4 = za4_t, Za3 = za3_t,
                     Za5 = za5_t, Za6 = za6_t,
                     priors = c(prior_a, prior_b),
                     par_init = par_init,
                     traj_init = states_init,
                     filtering = TRUE,
                     num_plots_states = 20)
source("./analysis/2019-08-09/99_analyze_convergence.R")
# source("./analysis/2019-08-09/99_save_convergence.R")
