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
simulate_data <- T
init_at_true  <- F
if (simulate_data) {
  set.seed(2016)# set.seed(42) # set.seed(3) # set.seed(139423) # T=100,50,200 don't "really" work
  source("./analysis/2019-08-09/02_simulation_data.R")
  source("./analysis/2019-08-09/02_simulation_init.R")
}
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

microbenchmark::microbenchmark(
  pgas(N = num_particles, MM = num_mcmc, TT = TT,
       y = y_t,
       Za1 = za1_t, Za2 = za2_t,
       Za4 = za4_t, Za3 = za3_t,
       Za5 = za5_t, Za6 = za6_t,
       priors = c(prior_a, prior_b),
       par_init = par_init,
       traj_init = deviate_states_init,
       filtering = TRUE,
       num_plots_states = 1),
  pgas2(N = num_particles, MM = num_mcmc, TT = TT,
        y = y_t, num_counts = num_counts,
        Za1 = za1_t, Za2 = za2_t,
        Za4 = za4_t, Za3 = za3_t,
        Za5 = za5_t, Za6 = za6_t,
        priors = c(prior_a, prior_b),
        par_init = par_init,
        traj_init = deviate_states_init,
        filtering = TRUE,
        num_plots_states = 1),
  pgas3(N = num_particles, MM = num_mcmc, TT = TT,
        y = y_t, num_counts = num_counts,
        Za1 = za1_t, Za2 = za2_t,
        Za4 = za4_t, Za3 = za3_t,
        Za5 = za5_t, Za6 = za6_t,
        priors = c(prior_a, prior_b),
        par_init = par_init,
        traj_init = deviate_states_init,
        filtering = TRUE,
        num_plots_states = 1)
)
num_particles <- 10000
num_mcmc <- 5000
out_pgas_sim1 <- pgas(N = num_particles, MM = num_mcmc, TT = TT,
                      y = y_t,
                      Za1 = za1_t, Za2 = za2_t,
                      Za4 = za4_t, Za3 = za3_t,
                      Za5 = za5_t, Za6 = za6_t,
                      priors = c(prior_a, prior_b),
                      par_init = par_init,
                      traj_init = deviate_states_init,
                      filtering = TRUE,
                      num_plots_states = 20)
out_pgas_sim2 <- pgas2(N = num_particles, MM = num_mcmc, TT = TT,
                       y = y_t, num_counts = num_counts,
                       Za1 = za1_t, Za2 = za2_t,
                       Za4 = za4_t, Za3 = za3_t,
                       Za5 = za5_t, Za6 = za6_t,
                       priors = c(prior_a, prior_b),
                       par_init = par_init,
                       traj_init = deviate_states_init,
                       filtering = TRUE,
                       num_plots_states = 20)
out_pgas_sim3 <- pgas3(N = num_particles, MM = num_mcmc, TT = TT,
                       y = y_t, num_counts = num_counts,
                       Za1 = za1_t, Za2 = za2_t,
                       Za4 = za4_t, Za3 = za3_t,
                       Za5 = za5_t, Za6 = za6_t,
                       priors = c(prior_a, prior_b),
                       par_init = par_init,
                       traj_init = deviate_states_init,
                       filtering = TRUE,
                       num_plots_states = 20)
