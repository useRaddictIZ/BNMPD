# pgas_run <- F
res <- out_pgas
par_mcmc  <- rbind(res$sigma_sq_xa1, res$phi_xa1, res$bet_xa1,
                   res$sigma_sq_xa2, res$phi_xa2, res$bet_xa2,
                   res$sigma_sq_xa3, res$phi_xa3, res$bet_xa3,
                   res$sigma_sq_xa4, res$phi_xa4, res$bet_xa4)
par_names <- c("sigma_sq_xa1", "phi_xa1",
               paste("bet_xa1", 1:length(true_bet_xa1), sep = "_"),
               "sigma_sq_xa2", "phi_xa2",
               paste("bet_xa2", 1:length(true_bet_xa2), sep = "_"),
               "sigma_sq_xa3", "phi_xa3",
               paste("bet_xa3", 1:length(true_bet_xa3), sep = "_"),
               "sigma_sq_xa4", "phi_xa4",
               paste("bet_xa4", 1:length(true_bet_xa4), sep = "_"))

path_t <- "/home/chief/Dropbox/research/GZ/analysis/2018-11-30/testing"
# path_c <- file.path(path_t, "test_correct")
path_n <- file.path(path_t, "test_new")

analyse_mcmc_convergence(mcmc_sims  = par_mcmc,
                         true_vals  = unlist(par_true[1:4]),
                         start_vals = unlist(par_init[1:4]),
                         par_names  = par_names,
                         states = res$xtraj,
                         burn = burnin,
                         table_view = TRUE,
                         table_save = TRUE,
                         table_path = path_t,
                         table_name = "test_new")
verify_test(make_correct_test = FALSE,
            path_test_new = path_n,
            path_test_sol = path_t)
