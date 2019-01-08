# pgas_run <- F
res <- out_pgas
par_mcmc  <- rbind(res$sigma_sq_xa, res$phi_xa, res$bet_xa,
                   res$sigma_sq_xb, res$phi_xb, res$bet_xb,
                   res$sigma_sq_xp, res$phi_xp, res$bet_xp,
                   res$sigma_sq_xq, res$phi_xq, res$bet_xq)
par_names <- c("sigma_sq_xa", "phi_xa",
               paste("bet_xa", 1:length(true_bet_xa), sep = "_"),
               "sigma_sq_xb", "phi_xb",
               paste("bet_xb", 1:length(true_bet_xb), sep = "_"),
               "sigma_sq_xp", "phi_xp",
               paste("bet_xp", 1:length(true_bet_xp), sep = "_"),
               "sigma_sq_xq", "phi_xq",
               paste("bet_xq", 1:length(true_bet_xq), sep = "_"))

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
