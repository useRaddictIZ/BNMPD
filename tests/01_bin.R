library(Rcpp)
library(Rmpfr)
source("tests/99_helper_diagnostics_model_comparison.R")
Sys.setenv("PKG_LIBS" = "-lmpfr -lgmp")
sourceCpp("tests/99_helper_model_comparison.cpp")
#
#
#
#
#
load("~/Dropbox/research/usa_energy/04-results/09_CA.RData")
DD <- 6
test_array <-  array(Reduce(cbind, out_pgas_CA_09$xtraj), c(num_mcmc, TT, DD))
test_array <- test_array[burnin:num_mcmc, , ]

pred_den(y = y_t[1, ], x = test_array[1, 1, ], num_counts = num_counts[1])
pred_den_cpp(y = y_t[1, ], x = test_array[1, 1, ], num_counts = num_counts[1], DD, TRUE)
