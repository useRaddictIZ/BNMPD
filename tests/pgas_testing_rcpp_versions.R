rm(list = ls())
sapply(paste0(getwd(),"/R/", list.files(paste0(getwd(),"/R/"), recursive = TRUE)), source)
sapply(paste0(getwd(),"/src/", list.files(paste0(getwd(),"/src/"), recursive = TRUE)), Rcpp::sourceCpp)
set.seed(139423) # set.seed(3) #
init_at_true <- TRUE
source("./tests/02_settings_simulation_data.R")
source("./tests/02_settings_simulation_init.R")
# Rcpp::sourceCpp("tests/pgas_testing_rcpp_versions_rng_R.cpp")
# Rcpp::sourceCpp("tests/pgas_testing_rcpp_versions2_rng_R.cpp")
Rcpp::sourceCpp("tests/pgas_testing_rcpp_versions_rng_arma.cpp")
Rcpp::sourceCpp("tests/pgas_testing_rcpp_versions2_rng_arma.cpp")
# source("tests/pgas_testing_R_versions.R")
par_init_cpp_version <- lapply(par_init, unlist)
deviate_states_init2 <- as.vector(sapply(as.list(deviate_states_init), rep, times = TT))
seed_nr <- 234
test_list_Z <- cbind(za1_t, za2_t, za3_t, za4_t, za5_t, za6_t)
# set.seed(seed_nr)
# set.seed(seed_nr)
# out1 <- pgas2_full_rng_R(10000, TT, 5, y_t, num_counts,
#                      za1_t, za2_t, za3_t, za4_t, za5_t, za6_t,
#                      c(prior_a, prior_b),
#                      par_init_cpp_version,
#                      deviate_states_init2)
# set.seed(seed_nr)
# out2 <- pgas2_full_short_rng_R(10000, TT, 5, y_t, num_counts,
#                             test_list_Z,
#                             c(prior_a, prior_b),
#                             par_init_cpp_version,
#                             deviate_states_init)
# all.equal(out1, out2)
#
#
#
#
#
set.seed(seed_nr)
out3 <- pgas2_full_rng_arma(10000, TT, 5, y_t, num_counts,
                            za1_t, za2_t, za3_t, za4_t, za5_t, za6_t,
                            c(prior_a, prior_b),
                            par_init_cpp_version,
                            deviate_states_init2)
set.seed(seed_nr)
out4 <- pgas2_full_short_rng_arma(10000, TT, 5, y_t, num_counts,
                                  test_list_Z,
                                  c(prior_a, prior_b),
                                  par_init_cpp_version,
                                  deviate_states_init)
print(all.equal(out3, out4))
# all.equal(pgas2c2, pgas2c[1:6])
# print(pgas2c)
# set.seed(seed_nr)
# pgas2R <- pgas2(N = 10000, MM = num_mcmc, TT = TT,
#                 y = y_t, num_counts = num_counts,
#                 Za1 = za1_t, Za2 = za2_t,
#                 Za4 = za4_t, Za3 = za3_t,
#                 Za5 = za5_t, Za6 = za6_t,
#                 priors = c(prior_a, prior_b),
#                 par_init = par_init,
#                 traj_init = deviate_states_init,
#                 filtering = TRUE,
#                 num_plots_states = 20)
# # print(pgas2R)
# all.equal(unname(unlist(pgas2c)), unlist(pgas2R))
# # all.equal(unlist(pgas2c[1:18]), unlist(pgas2R[1:18]))
# # max_print <- length(pgas2c)
# for (i in 7:8) {
#   print(pgas2c[[i]])
#   print(pgas2R[[i]])
# }
#
#
# out_pgas_sim1 <- pgas1(N = 10, MM = num_mcmc, TT = TT,
#                       y = y_t,
#                       Za1 = za1_t, Za2 = za2_t,
#                       Za4 = za4_t, Za3 = za3_t,
#                       Za5 = za5_t, Za6 = za6_t,
#                       priors = c(prior_a, prior_b),
#                       par_init = par_init,
#                       traj_init = deviate_states_init,
#                       filtering = TRUE,
#                       num_plots_states = 20)
# set.seed(seed_nr)
# out_pgas_sim2 <- pgas2(N = 10, MM = num_mcmc, TT = TT,
#                       y = y_t, num_counts = num_counts,
#                       Za1 = za1_t, Za2 = za2_t,
#                       Za4 = za4_t, Za3 = za3_t,
#                       Za5 = za5_t, Za6 = za6_t,
#                       priors = c(prior_a, prior_b),
#                       par_init = par_init,
#                       traj_init = deviate_states_init,
#                       filtering = TRUE,
#                       num_plots_states = 20)
# all.equal(out_pgas_sim1, out_pgas_sim2)
# set.seed(seed_nr)
# microbenchmark::microbenchmark(pgas1(N = 5, MM = 10, TT = TT,
#                                      y = y_t,
#                                      Za1 = za1_t, Za2 = za2_t,
#                                      Za4 = za4_t, Za3 = za3_t,
#                                      Za5 = za5_t, Za6 = za6_t,
#                                      priors = c(prior_a, prior_b),
#                                      par_init = par_init,
#                                      traj_init = deviate_states_init,
#                                      filtering = TRUE),
#                                pgas2_full(N = 5, MM = num_mcmc, TT = TT,
#                                           y = y_t, num_counts = num_counts,
#                                           Za1 = za1_t, Za2 = za2_t,
#                                           Za4 = za4_t, Za3 = za3_t,
#                                           Za5 = za5_t, Za6 = za6_t,
#                                           priors = c(prior_a, prior_b),
#                                           par_init = par_init,
#                                           traj_init = deviate_states_init,
#                                           num_plots_states = 20))
# # pgas1(N = num_particles,
#       MM = num_mcmc,
#       TT = TT,
#       y = y_t,
#       Za1 = Za_list[[1]],
#       Za2 = Za_list[[2]],
#       Za4 = Za_list[[3]],
#       Za3 = Za_list[[4]],
#       Za5 = Za_list[[5]],
#       Za6 = Za_list[[6]],
#       priors = c(prior_a, prior_b),
#       par_init = par_init,
#       traj_init = states_init,
#       filtering = TRUE,
#       num_plots_states = 20)
