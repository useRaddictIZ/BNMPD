rm(list = ls())
sapply(paste0(getwd(),"/R/helper/", list.files(paste0(getwd(),"/R/helper/"), recursive = TRUE)), source)
# set.seed(139423) #
set.seed(3) #
init_at_true <- TRUE
Rcpp::sourceCpp("tests/02_pgas_rcpp_final.cpp")
Rcpp::sourceCpp("tests/02_pgas_testing_rcpp_full_rng_R.cpp")
Rcpp::sourceCpp("tests/02_pgas_testing_rcpp_short_rng_R.cpp")
Rcpp::sourceCpp("tests/02_pgas_testing_rcpp_full_rng_arma.cpp")
Rcpp::sourceCpp("tests/02_pgas_testing_rcpp_short_rng_arma.cpp")
source("./tests/00_settings_simulation_data.R")
source("./tests/00_settings_simulation_init.R")
par_init_cpp_version <- lapply(par_init, unlist)
deviate_states_init2 <- as.vector(sapply(as.list(deviate_states_init), rep, times = TT))
test_list_Z <- cbind(za1_t, za2_t, za3_t, za4_t, za5_t, za6_t)
seed_nr <- 2345
set.seed(seed_nr)
#
#
#
#
#
NN <- 10000
DD <- 6
MM <- 10
#
#
#
#
#
set.seed(seed_nr)
out1 <- pgas2_full_rng_R(NN, TT, MM, y_t, num_counts,
                         za1_t, za2_t, za3_t, za4_t, za5_t, za6_t,
                         c(prior_a, prior_b),
                         par_init_cpp_version,
                         deviate_states_init2)
set.seed(seed_nr)
out2 <- pgas2_short_rng_R(NN, TT, DD, MM, y_t, num_counts,
                          test_list_Z,
                          c(prior_a, prior_b),
                          par_init_cpp_version,
                          deviate_states_init)
set.seed(seed_nr)
out_final_rng_R <- pgas_cpp(NN, TT, DD, MM, y_t, num_counts,
                            test_list_Z,
                            c(prior_a, prior_b),
                            par_init_cpp_version,
                            deviate_states_init)
print(all.equal(out1, out2))
print(all.equal(out1, out_final_rng_R))
print(all.equal(out2, out_final_rng_R))
#
#
#
#
#
set.seed(seed_nr)
out3 <- pgas2_full_rng_arma(NN, TT, MM, y_t, num_counts,
                            za1_t, za2_t, za3_t, za4_t, za5_t, za6_t,
                            c(prior_a, prior_b),
                            par_init_cpp_version,
                            deviate_states_init2)
set.seed(seed_nr)
out4 <- pgas2_short_rng_arma(NN, TT, DD, MM, y_t, num_counts,
                             test_list_Z,
                             c(prior_a, prior_b),
                             par_init_cpp_version,
                             deviate_states_init)
set.seed(seed_nr)
out_final_rng_arma <- pgas_cpp(NN, TT, DD, MM, y_t, num_counts,
                               test_list_Z,
                               c(prior_a, prior_b),
                               par_init_cpp_version,
                               deviate_states_init)
print(all.equal(out3, out4))
print(all.equal(out3, out_final_rng_arma))
print(all.equal(out4, out_final_rng_arma))
#
#
#
#
#
# f1 <- function(a1 = NN, a2 = TT, a3 = MM, a4 = y_t, a5 = num_counts,
#                a6 = za1_t, a7 =  za2_t, a8 =  za3_t, a9 =  za4_t, a10 =  za5_t, a11 =  za6_t,
#                a12 = c(prior_a, prior_b), a13 = par_init_cpp_version, a14 = deviate_states_init2) {
#   set.seed(seed_nr)
#   return(pgas2_full_rng_R(a1, a2, a3, a4, a5,
#                              a6, a7, a8, a9, a10, a11,
#                              a12, a13, a14))
# }
# f2 <- function(a1 = NN, a2 = TT, a3 = DD, a4 = MM, a5 = y_t, a6 = num_counts, a7 = test_list_Z,
#                a8 = c(prior_a, prior_b), a9 = par_init_cpp_version, a10 = deviate_states_init) {
#   set.seed(seed_nr)
#   return(pgas2_short_rng_R(a1, a2, a3, a4, a5, a6,
#                               a7, a8, a9, a10))
# }
# all.equal(
#   f1(a1 = NN, a2 = TT, a3 = MM, a4 = y_t, a5 = num_counts,
#      a6 = za1_t, a7 =  za2_t, a8 =  za3_t, a9 =  za4_t, a10 =  za5_t, a11 =  za6_t,
#      a12 = c(prior_a, prior_b), a13 = par_init_cpp_version, a14 = deviate_states_init2),
#   f2(a1 = NN, a2 = TT, a3 = DD,
#      a4 = MM, a5 = y_t, a6 = num_counts, a7 = test_list_Z,
#      a8 = c(prior_a, prior_b), a9 = par_init_cpp_version, a10 = deviate_states_init))
# f3 <- function(a1 = NN, a2 = TT, a3 = MM, a4 = y_t, a5 = num_counts,
#                a6 = za1_t, a7 =  za2_t, a8 =  za3_t, a9 =  za4_t, a10 =  za5_t, a11 =  za6_t,
#                a12 = c(prior_a, prior_b), a13 = par_init_cpp_version, a14 = deviate_states_init2) {
#   set.seed(seed_nr)
#   return(pgas2_full_rng_arma(a1, a2, a3, a4, a5,
#                              a6, a7, a8, a9, a10, a11,
#                              a12, a13, a14))
# }
# f4 <- function(a1 = NN, a2 = TT, a3 = DD, a4 = MM, a5 = y_t, a6 = num_counts, a7 = test_list_Z,
#                a8 = c(prior_a, prior_b), a9 = par_init_cpp_version, a10 = deviate_states_init) {
#   set.seed(seed_nr)
#   return(pgas2_short_rng_arma(a1, a2, a3, a4, a5, a6,
#                               a7, a8, a9, a10))
# }
# all.equal(
#   f3(a1 = NN, a2 = TT, a3 = MM, a4 = y_t, a5 = num_counts,
#      a6 = za1_t, a7 =  za2_t, a8 =  za3_t, a9 =  za4_t, a10 =  za5_t, a11 =  za6_t,
#      a12 = c(prior_a, prior_b), a13 = par_init_cpp_version, a14 = deviate_states_init2),
#   f4(a1 = NN, a2 = TT, a3 = DD,
#      a4 = MM, a5 = y_t, a6 = num_counts, a7 = test_list_Z,
#      a8 = c(prior_a, prior_b), a9 = par_init_cpp_version, a10 = deviate_states_init))
# out_bench <- microbenchmark::microbenchmark(f1(a1 = NN, a2 = TT, a3 = MM, a4 = y_t, a5 = num_counts,
#                                                a6 = za1_t, a7 =  za2_t, a8 =  za3_t, a9 =  za4_t, a10 =  za5_t, a11 =  za6_t,
#                                                a12 = c(prior_a, prior_b), a13 = par_init_cpp_version, a14 = deviate_states_init2),
#                                             f2(a1 = NN, a2 = TT, a3 = DD,
#                                                a4 = MM, a5 = y_t, a6 = num_counts, a7 = test_list_Z,
#                                                a8 = c(prior_a, prior_b), a9 = par_init_cpp_version, a10 = deviate_states_init),
#                                             f3(a1 = NN, a2 = TT, a3 = MM, a4 = y_t, a5 = num_counts,
#                                                a6 = za1_t, a7 =  za2_t, a8 =  za3_t, a9 =  za4_t, a10 =  za5_t, a11 =  za6_t,
#                                                a12 = c(prior_a, prior_b), a13 = par_init_cpp_version, a14 = deviate_states_init2),
#                                             f4(a1 = NN, a2 = TT, a3 = DD,
#                                                a4 = MM, a5 = y_t, a6 = num_counts, a7 = test_list_Z,
#                                                a8 = c(prior_a, prior_b), a9 = par_init_cpp_version, a10 = deviate_states_init)
#                                             )
# print(out_bench)
