# rm(list = ls())
# sapply(paste0(getwd(),"/R/helper/", list.files(paste0(getwd(),"/R/helper/"), recursive = TRUE)), source)
sapply(paste0(getwd(),"/src/", list.files(paste0(getwd(),"/src/"), recursive = TRUE)), Rcpp::sourceCpp)
# set.seed(139423) # set.seed(3) #
# init_at_true <- TRUE
# source("./tests/00_settings_simulation_data.R")
# source("./tests/00_settings_simulation_init.R")
# #
# source("tests/03_pgas_testing_R_full.R")
source("tests/03_pgas_testing_R_short.R")
#
# set.seed(42)
# out_long_version <- pgas1(N = 100, MM = 5, TT = TT,
#                           y = y_t, num_counts = num_counts,
#                           Za1 = za1_t, Za2 = za2_t,
#                           Za3 = za3_t, Za4 = za4_t,
#                           Za5 = za5_t, Za6 = za6_t,
#                           priors = c(prior_a, prior_b),
#                           par_init = par_init,
#                           traj_init = deviate_states_init,
#                           filtering = TRUE)
#
#
#
#
#
set.seed(42)
out_short_version <- pgas1_short(N = 100, MM = 5, TT = TT, DD = ncol(y_t),
                                 y = y_t, num_counts = num_counts,
                                 Za1 = za1_t, Za2 = za2_t,
                                 Za3 = za3_t, Za4 = za4_t,
                                 Za5 = za5_t, Za6 = za6_t,
                                 priors = c(prior_a, prior_b),
                                 par_init = par_init,
                                 traj_init = deviate_states_init,
                                 filtering = TRUE)
print(all.equal(out_long_version, out_short_version))
# out_short_version_check <- out_short_version
