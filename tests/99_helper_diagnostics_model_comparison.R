pred_den <- function(y, x, num_counts, log_value = TRUE) {
  # the log predictive density for one y_t
  y2 <- mpfr(y, 500)
  x2 <- mpfr(x, 500)
#
#   browser()
  log_lhs2 <- lfactorial(num_counts) + lgamma(sum(exp(x2)))
  log_lhs_diff2 <- log_lhs2 - lgamma(num_counts + sum(exp(x2)))

  # log_lhs <- lfactorial(num_counts) + lgamma(sum(exp(x)))
  # log_lhs_diff <- log_lhs - lgamma(num_counts + sum(exp(x)))

  log_rhs2_part1 <- lgamma(y2 + exp(x2))
  log_rhs2_part2 <-   -lfactorial(y2) - lgamma(exp(x2))
  log_rhs2 <-  log_rhs2_part1 + log_rhs2_part2
  log_rhs2_sum <- sum(log_rhs2)
  # sum(lgamma(y2 + exp(x2))  - lfactorial(y2) - lgamma(exp(x2)))

  # log_rhs_part1 <- lgamma(y + exp(x))
  # log_rhs_part2 <-  -lfactorial(y) - lgamma(exp(x))
  # log_rhs <-  log_rhs_part1 + log_rhs_part2
  # log_rhs_sum <- sum(log_rhs)
  # # sum(lgamma(y + exp(x))  -lfactorial(y) - lgamma(exp(x)))

  # browser()
  #
  # DD <- length(y)
  # for (d in 1:DD) {
  #   print(rbind(log_rhs_part1[d], log_rhs2_part1[d]), digits = 22)
  #   print(all.equal(log_rhs_part1[d], log_rhs2_part1[d]))
  #   print(rbind(log_rhs_part2[d], log_rhs2_part2[d]), digits = 22)
  #   print(all.equal(log_rhs_part2[d], log_rhs2_part2[d]))
  #   print(rbind(log_rhs[d], log_rhs2[d]), digits = 22)
  #   print(all.equal(log_rhs[d], log_rhs2[d]))
  # }
  # print(rbind(log_rhs_sum, log_rhs2_sum), digits = 22)
  # print(all.equal(log_rhs_sum, log_rhs2_sum))
  #
  # log_out <- log_lhs_diff + log_rhs_sum
  log_out2 <- log_lhs_diff2 + log_rhs2_sum
  #
  # browser()
  # while (DD < 10) {
  #   print(DD)
  # }
  # log_lhs <- lfactorial(num_counts) + lgamma(sum(exp(x2)))
  # log_lhs <- log_lhs - lgamma(num_counts + sum(exp(x2)))
  #
  # log_rhs <- sum(lgamma(y2 + exp(x2)) - lfactorial(y2) - lgamma(exp(x2)))
  #
  # log_out <- log_lhs + log_rhs

  if (log_value) {
    return(as.numeric(log_out2))
  } else {
    return(as.numeric(exp(log_out2)))
  }
}

lppd <- function(y, x, num_counts, TT, MM) {
  # browser()
  log_sum_ppd <- 0
  for (t in 1:TT) {
    sum_ppd <- 0
    for (m in 1:MM) {
      sum_ppd <- sum_ppd + pred_den(y = y[t, ], x[m, t, ], num_counts[t], log_value = FALSE)
    }
    # browser()
    sum_ppd <- log(sum_ppd/MM)
    log_sum_ppd <- log_sum_ppd + sum_ppd
  }
  # browser()
  return(log_sum_ppd)
}
dic <- function(y, X_list, num_counts, burnin) {
  TT <- nrow(y)
  DD <- ncol(y)
  MM <- nrow(X_list[[1]])

  X_array <- array(Reduce(cbind, X_list), c(MM, TT, DD))
  X_array <- X_array[burnin:MM, , ]
  X_post_means <- apply(X_array, MARGIN = c(2, 3), mean)
  # browser()
  X_array2 <- lapply(X_list, function(x) {as.vector(t(x)[, burnin:MM])})
  X_array2 <- matrix(Reduce(cbind, X_array2), TT*(MM - burnin + 1), D)

  MM <- MM - burnin + 1


  sum_log_lhs  <- 0
  for (t in 1:TT) {
    sum_log_lhs <- sum_log_lhs + pred_den(y[t, ], X_post_means[t, ], num_counts = num_counts[t])
  }
  ##############################################################################
  ###### DOES NOT WORK: THE FUNCTION pred_den_vech() CONTAINS ERRORS
  ###### sum_log_lhs2 <- pred_den_vech(y, X_post_means, num_counts)
  ##############################################################################
  sum_sum_log_rhs  <- 0
  sum_sum_log_rhs2  <- 0
  for (m in 1:MM) {
    sum_log_rhs2 <- 0
    for (t in 1:TT) {
      # browser()
      sum_log_rhs2 <- sum_log_rhs2 + pred_den(y[t, ], X_array[m, t, ], num_counts = num_counts[t])
      # sum_log_rhs2 <- sum_log_rhs2 + pred_den_cpp(y[t, ], X_array[m, t, ], num_counts[t], DD, TRUE)
    }
    # sum_log_rhs2 <- pred_den_cpp2(t(y), t(X_array[m, , ]), num_counts, DD, TT, TRUE)
    sum_sum_log_rhs2 <- sum_sum_log_rhs2 + sum_log_rhs2
    # sum_sum_log_rhs <- sum_sum_log_rhs + pred_den_vech(y, X_array[m, , ], num_counts = num_counts)
    # browser()
    # print(all.equal(sum_sum_log_rhs, sum_sum_log_rhs2))
    print(m)
  }
  # sum_sum_log_rhs <- pred_den_cpp3(y_cpp, X_array_cpp, num_counts, DD, TT, MM, TRUE)
  # all.equal(sum_sum_log_rhs2, sum_sum_log_rhs)
  # all.equal(sum_sum_log_rhs2, sum_sum_log_rhs3)
  sum_sum_log_rhs <- sum_sum_log_rhs2
  sum_sum_log_rhs <- sum_sum_log_rhs/MM

  computed_p_dic <- 2*(sum_log_lhs - sum_sum_log_rhs)
  dic <- -2*sum_log_lhs + 2*computed_p_dic
  return(dic)
}
dic_cpp <- function(y, X_list, num_counts, burnin) {
  TT <- nrow(y)
  DD <- ncol(y)
  MM <- nrow(X_list[[1]])

  X_array <- array(Reduce(cbind, X_list), c(MM, TT, DD))
  X_array <- X_array[burnin:MM, , ]
  X_post_means <- apply(X_array, MARGIN = c(2, 3), mean)

  y_cpp <- t(y)
  X_post_means_cpp <- t(X_post_means)
  X_array_cpp <- aperm(X_array, c(3, 2, 1))

  MM <- MM - burnin + 1

  # sum_log_lhs <- pred_den_cpp2(y_cpp, X_post_means_cpp, num_counts = num_counts, DD, TT, TRUE)
  # sum_sum_log_rhs <- pred_den_cpp3(y_cpp, X_array_cpp, num_counts, DD, TT, MM, TRUE)
  #
  # dic2 <- 2*(sum_log_lhs - sum_sum_log_rhs)
  dic <-  dic_core(y_cpp, X_post_means_cpp, X_array_cpp, num_counts, DD, TT, MM)
  return(dic)
}
waic <- function(y, X_list, num_counts, burnin) {
  TT <- nrow(y)
  DD <- ncol(y)
  MM <- nrow(X_list[[1]])

  X_array <- array(Reduce(cbind, X_list), c(MM, TT, DD))
  X_array <- X_array[burnin:MM, , ]

  MM <- MM - burnin + 1
  computed_var <- 0
  computed_p_waic <- 0
  for (t in 1:TT) {
    computed_var <- 0
    for (s in 1:MM) {
        log_pred_den_avg <- 0
        for (m in 1:MM) {
          log_pred_den_avg <- log_pred_den_avg + pred_den(y[t, ], X_array[m, t, ], num_counts = num_counts[t])
        }
        log_pred_den_avg <- log_pred_den_avg/MM
        computed_var <- computed_var + (pred_den(y[t, ], X_array[s, t, ], num_counts = num_counts[t]) - log_pred_den_avg)^2
    }
    computed_var <- computed_var/(MM - 1)
    computed_p_waic <- computed_p_waic + computed_var
    print(t)
  }
  lppd_out  <- lppd(y, X_array, num_counts, TT, MM)
  p_waic <- -2*(lppd_out - computed_p_waic)
  return(p_waic)
}
waic_cpp <- function(y, X_list, num_counts, burnin) {

  TT <- nrow(y)
  DD <- ncol(y)
  MM <- nrow(X_list[[1]])

  X_array <- array(Reduce(cbind, X_list), c(MM, TT, DD))
  X_array <- X_array[burnin:MM, , ]

  y_cpp <- t(y)
  X_array_cpp <- aperm(X_array, c(3, 2, 1))

  MM <- MM - burnin + 1

  waic <- waic_core(y_cpp,
                    X_array_cpp,
                    num_counts,
                    DD,
                    TT,
                    MM)
  return(waic)
}
lppd_dic_waic_test <- function(y, X_list, num_counts, burnin) {
  TT <- nrow(y)
  DD <- ncol(y)
  MM <- nrow(X_list[[1]])

  X_array <- array(Reduce(cbind, X_list), c(MM, TT, DD))
  X_array <- X_array[burnin:MM, , ]
  X_post_means <- apply(X_array, MARGIN = c(2, 3), mean)

  y_cpp <- t(y)
  X_post_means_cpp <- t(X_post_means)
  X_array_cpp <- aperm(X_array, c(3, 2, 1))

  MM <- MM - burnin + 1
  out <- lppd_dic_waic(y_cpp, X_post_means_cpp, X_array_cpp, num_counts,
                       DD, MM, TT)
  return(out)
}
test_fun1 <- function(y, X_list, num_counts, burnin) {

  TT <- nrow(y)
  DD <- ncol(y)
  MM <- nrow(X_list[[1]])

  X_array <- array(Reduce(cbind, X_list), c(MM, TT, DD))
  X_array <- X_array[burnin:MM, , ]

  y_cpp <- t(y)
  X_array_cpp <- aperm(X_array, c(3, 2, 1))

  MM <- MM - burnin + 1

  lppd <- lppd_core(y_cpp,
                    X_array_cpp,
                    num_counts,
                    DD,
                    TT,
                    MM)
  return(lppd)
  # out1 <- pred_den_2(y_cpp,
  #                        X_array_cpp,
  #                        num_counts,
  #                        DD,
  #                        2,
  #                        MM)
  # log_sum_ppd <- 0
  # sum_ppd <- numeric(MM)
  # for (t in 1:2) {
  #   # sum_ppd <- 0
  #   for (m in 1:MM) {
  #     # sum_ppd <- sum_ppd + pred_den(y = y[t, ], X_array[m, t, ], num_counts[t], log_value = FALSE)
  #     sum_ppd[m] <- pred_den(y = y[t, ], X_array[m, t, ], num_counts[t], log_value = TRUE)
  #   }
  #   max_sum_ppd <- max(sum_ppd)
  #   sum_ppd <- exp(sum_ppd - max_sum_ppd)
  #   log_sum_ppd <- log_sum_ppd + log(sum(sum_ppd)) + exp(max_sum_ppd)
  #   print(t)
  # }
  # log_sum_ppd_out <- log(log_sum_ppd)
  # browser()
  # all.equal(out1, log_sum_ppd_out)
  # all.equal(out1, out2)
  # return(out)
}
################################################################################
####### THIS FUNCTION HAS AN ERROR SOMEWHERE AND THEREFORE IS DEPRECATED #######
################################################################################
# pred_den_vech <- function(y, x, num_counts, log_value = TRUE) {
#   # the log predictive density for one y_t
#   y <- mpfr(y, 570)
#   x <- mpfr(x, 570)
#   exp_x <- exp(x)
#   rs_exp_x <- rowSums(exp_x)
#   # browser()
#   log_lhs <- lfactorial(num_counts) + lgamma(rs_exp_x)
#   log_lhs <- log_lhs - lgamma(num_counts + rs_exp_x)
#
#   log_rhs <- rowSums(lgamma(as.vector(t(y)) + t(exp_x)) - as.vector(t(lfactorial(y))) - t(lgamma(exp_x)))
#
#   log_out <- log_lhs + log_rhs
#   log_out <- log_lhs
#   # log_out <- log_rhs
#   log_out <- sum(log_out)
#   if (log_value) {
#     return(as.numeric(log_out))
#   } else {
#     return(as.numeric(exp(log_out)))
#   }
# }
################################################################################
################################################################################
################################################################################
################################################################################
#### THIS FUNCTION MAY HAVE AN ERROR SOMEWHERE AND THEREFORE IS DEPRECATED #####
################################################################################
# pred_den_super_vech <- function(y, x, num_counts, log_value = TRUE) {
#   # the log predictive density for one y_t
#   # browser()
#   y <- mpfr(y, 90)
#   x <- mpfr(x, 90)
#   exp_x <- exp(x)
#   rs_exp_x <- rowSums(exp_x)
#   # browser()
#   log_lhs <- lfactorial(num_counts) + lgamma(rs_exp_x)
#   log_lhs <- log_lhs - lgamma(num_counts + rs_exp_x)
#
#   log_rhs <- colSums(lgamma(as.vector(t(y)) + t(exp_x)) - as.vector(t(lfactorial(y))) - t(lgamma(exp(x))))
#
#   log_out <- log_lhs + log_rhs
#   log_out <- log_lhs
#   # log_out <- log_rhs
#   log_out <- sum(log_out)
#   if (log_value) {
#     return(as.numeric(log_out))
#   } else {
#     return(as.numeric(exp(log_out)))
#   }
# }
################################################################################
################################################################################
################################################################################
