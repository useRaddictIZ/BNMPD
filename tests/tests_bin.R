# microbenchmark::microbenchmark(colSums(x_test * (solve(A_test) %*% x_test)),
#                                crossprod(crossprod(solve(A_test), x_test), x_test),
#                                .colSums(x_test * (solve(A_test) %*% x_test), 5000, 1))
helper_as <- function(M, x) {
  apply(X = x,
        MARGIN = 1,
        # function(x) {drop(crossprod(M, x))})
        function(x) {drop(crossprod(crossprod(M, x), x))})
}
# set.seed(42)
# xa1_r <- 1:TT
# xa2_r <- 1:TT
# xa3_r <- 1:TT
# xa4_r <- 1:TT
# xa5_r <- 1:TT
# xa6_r <- 1:TT
# eval_fa1 <- rnorm(1, mean = 1)
# eval_fa2 <- rnorm(1, mean = 2)
# eval_fa3 <- rnorm(1, mean = 3)
# eval_fa4 <- rnorm(1, mean = 4)
# eval_fa5 <- rnorm(1, mean = 5)
# eval_fa6 <- rnorm(1, mean = 6)
# t_take   <- 1
# m1 <- matrix(c(eval_fa1 - xa1_r[t_take],
#                eval_fa2 - xa2_r[t_take],
#                eval_fa3 - xa3_r[t_take],
#                eval_fa4 - xa4_r[t_take],
#                eval_fa5 - xa5_r[t_take],
#                eval_fa6 - xa6_r[t_take]), nrow = num_particles, ncol = 6) # num states, or later just D!
# m2 <- diag(c(true_sig_sq_xa1^{-1},
#              true_sig_sq_xa2^{-1},
#              true_sig_sq_xa3^{-1},
#              true_sig_sq_xa4^{-1},
#              true_sig_sq_xa5^{-1},
#              true_sig_sq_xa6^{-1}))
# m3 <- diag(m2)
#
m          <- -1/2 * helper_as(M = m2, x = m1)
#
#
w_log_as   <- rep(log(1/num_particles), times = num_particles) + m
w_max_as   <- max(w_log_as)
w_tilde_as <- exp(w_log_as - w_max_as)
w_as       <- w_tilde_as/sum(w_tilde_as)

w_as_2 <- as.vector(w_as_c(m1,
                           m3,
                           rep(log(1/num_particles),
                               times = num_particles)))
# w_as_2 <- t(w_as_c(m1, m3, rep(log(1/num_particles), times = num_particles)))
# all.equal(w_as[1], w_as_2[1], tolerance = 0)
boo_check <- logical(num_particles)
for (i in 2:length(boo_check)) {
  boo_check[i] <- all.equal(w_as[i], w_as_2[i], tolerance = 0)
  print(boo_check[i])
}

