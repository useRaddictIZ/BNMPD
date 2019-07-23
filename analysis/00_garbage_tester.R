col_seq <- RColorBrewer::brewer.pal(5, "Dark2")
single_plots <- rep(list(list()), times = 5)
test_mat <- y_t/rowSums(y_t)
# test_mat <- y_t
test_mat <- cbind(1:TT, test_mat)
test_mat <- as.data.frame(test_mat)
energy_types <- paste0("a", 1:5)
names(test_mat) <- c("t", energy_types)
all_plots <- ggplot(test_mat, aes(x = t))
for (i in 1:5) {
  all_plots <- all_plots + geom_line(aes_string(x = "t",
                                                y = energy_types[i]),
                                     col = col_seq[i]) +
    labs(x = "years", y = "share")
  single_plots[[i]] <- ggplot(test_mat, aes(x = Year_t)) +
    geom_line(aes_string(x = "t",
                         y = energy_types[i]),
              col = col_seq[i]) +
    ggtitle(energy_types[i]) +
    labs(x = "t", y = "share")

}
final_layout <- matrix(c(1, 1, 2:7), ncol = 2, byrow = TRUE)

gridExtra::grid.arrange(grobs = single_plots,
                        layout_matrix = matrix(1:6, ncol = 2))

plot_final <- gridExtra::grid.arrange(grobs = c(list(all_plots),
                                                single_plots),
                                      layout_matrix = final_layout,
                                      top = "Shares jointly and individually")









names_title <- paste("True states for ",
                     "xa1_t (black),", " xa2_t (red),",
                     " xa3_t (green),",  "xa4_t (blue)", " and", " xa5_t (blue)")
names_ylab  <- paste(" xa1_t,", " xa2_t,", " xa3_t,", " xa4_t",
                     " and", " xa5_t", " states")

par(mfrow = c(1,2))
test_mat <- cbind(xa1_t, xa2_t, xa3_t, xa4_t, xa5_t)
matplot(test_mat,
        type = "l",
        main = names_title,
        ylab = names_ylab)
matplot(test_mat/rowSums(test_mat),
        type = "l",
        main = names_title,
        ylab = names_ylab)


names_title <- paste("True states for ",
                     "y1_t (black),", " y2_t (red),",
                     " y3_t (green),",  "y4_t (blue)", " and", " y5_t (blue)")
names_ylab  <- paste(" y1_t,", " y2_t,", " y3_t,", " y4_t",
                     " and", " y5_t", " states")
test_mat <- y_t
matplot(test_mat,
        type = "l",
        main = names_title,
        ylab = names_ylab)
matplot(test_mat/rowSums(test_mat),
        type = "l",
        main = names_title,
        ylab = names_ylab)
for (i in 1:5) {
  plot((test_mat/rowSums(test_mat))[, i], type = "l")
}









set.seed(123)
z2 <- cbind(rep(c(0, 1), each = T/2), z)
bet2 <- c(20, bet_x)

x[1] <- f(x_tt = xinit, z = z2[1, ], phi_x = phi_x, bet_x = bet2)
x[1] <- x[1] + sqrt(sig_sq_x)*rnorm(n = 1)

for (t in 1:T) {
  if (t < T) {
    x[t + 1] <- f(x_tt = x[t], z = z2[t + 1, ],
                  phi_x = phi_x, bet_x = bet2)
    x[t + 1] <- x[t + 1] + sqrt(sig_sq_x)*rnorm(n = 1)
  }
}
if (process_log_scale) {
  x <- exp(x)
}

plot(x, type = "l")


zmean_test <- 1
last_zmean <- x_level * (1 - phi_x) - sum(zmean_test * bet_x[-dim_reg])
last_zmean <- last_zmean/bet_x[dim_reg]

x_all <- cbind(xa1, xa2, xa3, xa4, xa5)
matplot(x_all/rowSums(x_all),
        type = "l",
        main = names_title,
        ylab = names_ylab
)







c_log_par <- cbind(xa1[, 1], xa2[, 1], xa3[, 1], xa4[, 1], xa5[, 1])
analyse_weightfunction(log_particles = c_log_par,
                       true_states = c(xa1_t[1], xa2_t[1], xa3_t[1], xa4_t[1], xa5_t[1]),
                       plot_univ_ID = 1:5,
                       plot_pairwise = TRUE,
                       num_mesh_points = 1000, mesh_width = 0.5,
                       num_particle_plotted = 10,
                       precision_weights = 4,
                       delay = 0.2,
                       y = y_t[1, , drop = FALSE], num_counts = num_counts[1], D = 5)








set.seed(123)
test_part <- log(cbind(jitter(rep(xa1_t[1], times = 10), amount = ),
                       jitter(rep(xa2_t[1], times = 10), amount = 1),
                       jitter(rep(xa3_t[1], times = 10), amount = 1),
                       jitter(rep(xa4_t[1], times = 10), amount = 1),
                       jitter(rep(xa5_t[1], times = 10), amount = 1)))
test_true <- log(cbind(rep(xa1_t[1], times = 10),
                       rep(xa2_t[1], times = 10),
                       rep(xa3_t[1], times = 10),
                       rep(xa4_t[1], times = 10),
                       rep(xa5_t[1], times = 10)))

true_wf <- w_BPF(y = y_t[1, , drop = FALSE], N = 10,
                 xa1 = test_part[, 1], xa2 = test_true[, 2],
                 xa3 = test_true[, 3], xa4 = test_true[, 4],
                 xa5 = test_true[, 5],
                 num_counts =  num_counts[1],
                 D = 5)
plot_wf <- w_BPF_plot_univ(exp(test_part[, 1]), state_ID = 1, at_true_states = FALSE,
                           particles = exp(test_true),
                          # particles = c(xa1_t[1], xa2_t[1], xa3_t[1], xa4_t[1], xa5_t[1]),
                          y = y_t[1, , drop = FALSE], num_counts = num_counts[1], D = 5)
identical(true_wf, plot_wf)
#
analyse_weightfunction(log_particles = test_part,
                       true_states = c(xa1_t[1], xa2_t[1], xa3_t[1], xa4_t[1], xa5_t[1]),
                       plot_univ_ID = 1:5, plot_pairwise = TRUE, num_mesh_points = 100,
                       y = y_t[1, , drop = FALSE], num_counts = num_counts[1], D = 5)



#
# testM1 <- cbind(log_lhs, .rowSums(x = alphas, m = N, n = D), alphas)
# testM1 <- cbind(log_rhs, alphas)
# testM1 <- cbind(log_rhs, log_lhs, alphas)
# testM1 <- cbind(w, log_rhs, log_lhs, alphas)
# testM1 <- cbind(w, alphas)
# testM1[sort(testM1[,1], index.return = TRUE)[[2]],]



# ll1 <- function(x, remaining_alphas, y, ncount, ID) {
#   # browser()
#   # ncount <- num_counts[1]
#   # y <- y_t[1, ]
#   # all_alphas <- c(xa1_t[1], xa2_t[1], xa3_t[1], xa4_t[1], xa5_t[1])
#   all_alphas <- append(remaining_alphas, x, after = ID - 1)
#   lhs <- lgamma(sum(all_alphas))
#   lhs <- lhs - lgamma(ncount + sum(all_alphas))
#   rhs <- sum(lgamma(y + all_alphas) - lgamma(all_alphas))
#
#   return(lhs + rhs)
# }
# ll1(xa1_t[1], c(xa2_t[1], xa3_t[1], xa4_t[1], xa5_t[1]), ncount = num_counts[1], y_t[1,], ID  = 1)
#
#
# my_ID <- 1
#
# ll_plotable <- function(x) {
#   len <- length(x)
#   out <- numeric(len)
#   remaining_alphas <- c(xa1_t[2], xa2_t[2], xa3_t[2], xa4_t[2], xa5_t[2])[-my_ID]
#   for (i in 1:len) {
#     out[i] <- ll1(x[i], remaining_alphas, ncount = num_counts[2], y_t[2,], ID = my_ID)
#   }
#   out
# }
#
# arround_val <- c(xa1_t[1], xa2_t[1], xa3_t[1], xa4_t[1], xa5_t[1])[my_ID]
# mesh <- arround_val + 0.7*c(-arround_val, arround_val)
# curve(ll_plotable, from = mesh[1], to = mesh[2], n = 10000, xlab = arround_val)
# abline( v = arround_val, col = "red")

#
#
#
# w_BPF_plot_univ(exp(test_part[, 1]), state_ID = 1,
#                true_states = c(xa1_t[1], xa2_t[1], xa3_t[1], xa4_t[1], xa5_t[1]),
#                y = y_t[1, , drop = FALSE], num_counts = num_counts[1], D = 5)
#
# around_val1 <- xa1_t[1]
# mesh1 <- around_val1 + 0.8*c(-around_val1, around_val1)
# mesh1 <- seq(from = mesh1[1], to = mesh1[2], length.out = 1000)
# around_val2 <- xa2_t[1]
# mesh2 <- around_val2 + 0.8*c(-around_val2, around_val2)
# mesh2 <- seq(from = mesh2[1], to = mesh2[2], length.out = 1000)
# mesh3 <-
#
# wfct <- outer(mesh1, mesh2, FUN = w_BPF_plot_mult, state_ID = c(1, 2),
#               true_states = c(xa1_t[1], xa2_t[1], xa3_t[1], xa4_t[1], xa5_t[1]),
#               y = y_t[1, , drop = FALSE], num_counts = num_counts[1], D = 5)
# # print(wfct, digits = 12)
# image(mesh1, mesh2, wfct)
# contour(mesh1, mesh2, wfct, add = TRUE)
# points(exp(test_part[, 1]), exp(test_part[, 2]), col = "green", pch = 18)
# abline(v = xa1_t[1], col = "blue")
# abline(h = xa2_t[1], col = "blue")
# points(xa1_t[1], xa2_t[1], col = "blue")
# wfct2 <- w_BPF_plot_univ(exp(test_part[, 1]), state_ID = 1,
#                          true_states = c(xa1_t[1], exp(test_part[4, 2]), xa3_t[1], xa4_t[1], xa5_t[1]),
#                          y = y_t[1, , drop = FALSE], num_counts = num_counts[1], D = 5)
# print(wfct2, digits = 12)


# contour-plots -----------------------------------------------------------
# require("grDevices") # for colours
# x <- y <- seq(-4*pi, 4*pi, length.out = 27)
# r <- sqrt(outer(x^2, y^2, "+"))
# image(z = z <- cos(r^2)*exp(-r/6), col = gray.colors(33))
# image(z, axes = FALSE, main = "Math can be beautiful ...",
#       xlab = expression(cos(r^2) * e^{-r/6}))
# contour(z, add = TRUE, drawlabels = FALSE)
#
# # Volcano data visualized as matrix. Need to transpose and flip
# # matrix horizontally.
# image(t(volcano)[ncol(volcano):1,])
#
# # A prettier display of the volcano
# x <- 10*(1:nrow(volcano))
# y <- 10*(1:ncol(volcano))
# image(x, y, volcano, col = hcl.colors(100, "terrain"), axes = FALSE)
# contour(x, y, volcano, levels = seq(90, 200, by = 5),
#         add = TRUE, col = "brown")
# axis(1, at = seq(100, 800, by = 100))
# axis(2, at = seq(100, 600, by = 100))
# box()
# title(main = "Maunga Whau Volcano", font.main = 4)
#
#
# png("volcano_w_scale.png", width=7, height=4, units="in", res=200)
# layout(matrix(c(1,2,3,0,4,0), nrow=2, ncol=3), widths=c(4,4,1), heights=c(4,1))
# layout.show(4)
# pal.1=colorRampPalette(c("black", "red", "yellow"), space="rgb")
# pal.2=colorRampPalette(c("black", "blue", "cyan"), space="rgb")
#
# breaks <- seq(min(volcano), max(volcano),length.out=100)
# par(mar=c(1,1,1,1))
# image(seq(dim(volcano)[1]), seq(dim(volcano)[2]), volcano,   axes = TRUE,
#       col=pal.1(length(breaks)-1), breaks=breaks, xaxt="n", yaxt="n", ylab="", xlab="")
# #Add additional graphics
# highest <- which.max(volcano)
# points(highest %% dim(volcano)[1], highest %/% dim(volcano)[1],
#        pch=2, lwd=2, cex=2,col="blue")
#
# points(87, 30,
#        pch=2, lwd=2, cex=2,col="blue")

# pers-plots --------------------------------------------------------------
# require(grDevices) # for trans3d
# ## More examples in  demo(persp) !!
# # (1) The Obligatory Mathematical surface.
# #     Rotated sinc function.
#
# x <- seq(-10, 10, length = 30)
# y <- x
# f <- function(x, y) { r <- sqrt(x^2 + y^2); 10 * sin(r)/r }
# z <- outer(x, y, f)
# z[is.na(z)] <- 1
# op <- par(bg = "white")
# persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")
# persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
#       ltheta = 120, shade = 0.75, ticktype = "detailed",
#       xlab = "X", ylab = "Y", zlab = "Sinc( r )"
# ) -> res
# round(res, 3)
#
# # (2) Add to existing persp plot - using trans3d() :
#
# xE <- c(-10,10); xy <- expand.grid(xE, xE)
# points(trans3d(xy[,1], xy[,2], 6, pmat = res), col = 2, pch = 16)
# lines(trans3d(x, y = 10, z = 6 + sin(x), pmat = res), col = 3)
#
# phi <- seq(0, 2*pi, len = 201)
# r1 <- 7.725 # radius of 2nd maximum
# xr <- r1 * cos(phi)
# yr <- r1 * sin(phi)
# lines(trans3d(xr,yr, f(xr,yr), res), col = "pink", lwd = 2)
# ## (no hidden lines)
#
# # (3) Visualizing a simple DEM model
#
# z <- 2 * volcano        # Exaggerate the relief
# x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
# y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)
# ## Don't draw the grid lines :  border = NA
# par(bg = "slategray")
# persp(x, y, z, theta = 135, phi = 30, col = "green3", scale = FALSE,
#       ltheta = -120, shade = 0.75, border = NA, box = FALSE)
#
# # (4) Surface colours corresponding to z-values
#
# par(bg = "white")
# x <- seq(-1.95, 1.95, length = 30)
# y <- seq(-1.95, 1.95, length = 35)
# z <- outer(x, y, function(a, b) a*b^2)
# nrz <- nrow(z)
# ncz <- ncol(z)
# # Create a function interpolating colors in the range of specified colors
# jet.colors <- colorRampPalette( c("blue", "green") )
# # Generate the desired number of colors from this palette
# nbcol <- 100
# color <- jet.colors(nbcol)
# # Compute the z-value at the facet centres
# zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
# # Recode facet z-values into color indices
# facetcol <- cut(zfacet, nbcol)
# persp(x, y, z, col = color[facetcol], phi = 30, theta = -30)
#
# par(op)
#
#


# old stuff ---------------------------------------------------------------


# ll2 <- function(x, remaining_alphas, y, ncount) {
#   len <- length(x)
#   all_alphas <- matrix(rep(remaining_alphas, each = len), nrow = len)
#   all_alphas <- cbind(rep(x, times = len), all_alphas)
#   # all_alphas <- c(x, remaining_alphas)
#   lhs <- log(factorial(ncount))
#   lhs <- lhs + lgamma(rowSum(all_alphas))
#   lhs <- lhs - lgamma(rowsum(all_alphas))
#
#   rhs <- sum(lgamma(y + all_alphas) - lgamma(all_alphas) - factorial(y))
#
#   return(lhs + rhs)
# }
#
# ll_plot <- function(x,) {
#   ll1()
# }

# w_max2       <- max(w_test)
# w_tilde2     <- exp(w_test - w_max2)
# w_test_norm  <- w_tilde2/sum(w_tilde2)

# x, state_ID, true_states, y, num_counts, D = 5
# set.seed(123)
# n <- 10
# alpha <- 1:5
# l <- length(alpha)
# x <- matrix(rgamma(l * n, alpha), ncol = l, byrow = TRUE)
#
#
# f1 <- function(sm, l) {
#   as.vector(sm %*% rep(1, l))
# }
# f2 <- function(sm, n, l) {
#   .rowSums(sm, m = n, n = l)
# }
# f3 <- function(sm) {
#   rowSums(sm)
# }
#
# microbenchmark::microbenchmark(as.vector(x %*% rep(1, l)), .rowSums(x, m = n, n = l), rowSums(x))
#
#
# sm1 <- as.vector(x %*% rep(1, l))
# sm2 <- .rowSums(x, m = n, n = l)
#
# x/as.vector(sm)
#
#
# set.seed(123)
# rgamma(n = 5, shape = 1:5)
# rgamma(n = 5, shape = 11:15)
#
# matrix(c(1:5, 11:15), nrow = 2, byrow = TRUE)

# set.seed(123)
# n <- dim(alpha)[1]
# l <- dim(alpha)[2]
# x <- matrix(rgamma(n = n, shape = t(alpha)), ncol = l, byrow = TRUE)
# x_colsums <- as.vector(x %*% rep(1, l))
# x/x_colsums


#

#
# dirichlet1 <- function(x, alpha) {
#   logD <- sum(lgamma(alpha)) - lgamma(sum(alpha))
#   s <- (alpha - 1) * log(x)
#   s <- ifelse(alpha == 1 & x == 0, -Inf, s)
#   exp(sum(s) - logD)
# }
# dirichlet1(y_t[1,], alpha = c(xa1_t[1], xa2_t[1], xa3_t[1], xa4_t[1]))
# dirichlet1(y_t[1,], alpha = c(xa1_t[2], xa2_t[2], xa3_t[2], xa4_t[2]))
# dirichlet1(y_t[1,], alpha = c(xa1_t[1:2], xa2_t[1:2], xa3_t[1:2], xa4_t[1:2]))
#
# alpha1 <-  c(xa1_t[1], xa2_t[1], xa3_t[1], xa4_t[1])
# alpha2 <-  c(xa1_t[2], xa2_t[2], xa3_t[2], xa4_t[2])
# alpha3 <- matrix(c(alpha1, alpha2), byrow = TRUE, nrow = 2)
#
# ones <- rep(1, times = 4)
#
# logD1 <- sum(lgamma(alpha1)) - lgamma(sum(alpha1))
# logD2 <- sum(lgamma(alpha2)) - lgamma(sum(alpha2))
# logD3 <- rowSums(lgamma(alpha3)) - lgamma(rowSums(alpha3))
# logD4 <- as.vector(lgamma(alpha3) %*% ones) - lgamma(as.vector(alpha3 %*% ones))
#
# logD1
# logD2
# logD3
# logD4
#
#
# s1 <- (alpha1 - 1) * log(y_t[1, ])
# s2 <- (alpha2 - 1) * log(y_t[1, ])
# s3 <- (alpha3 - 1) %*% t(log(y_t[1, , drop = FALSE]))
#
# sum(s1)
# sum(s2)
# s3
#
#
# s <- ifelse(alpha == 1 & x == 0, -Inf, s)
# #
# #
# #
# #
# # FOR CUBIC SPLINES ORDER = 4!!!
# splines::splineDesign(knots = 1:15, x = 4:7, outer.ok = TRUE)
#
#
# #
# #
# #
# #
# #
#
#
# exp(sum(s1) - logD1)
# exp(sum(s2) - logD2)
# exp(s3 - logD3)
# # exp(as.vector(s3 %*% ones) - logD3)
#
#
#
#
#
#
# sort(w[, 1], decreasing = TRUE)[1:20]
# hist(sort(w[, 1], decreasing = TRUE)[1:20])
#
#
#
#
#
# alphas <- matrix(c(xa1, xa2, xa3, xa4), nrow = 1000)
# # first BUG
# # nrow = N num particles!
# log_Balpha <- rowSums(lgamma(alphas)) - lgamma(rowSums(alphas))
#
# log_denom  <- (alphas - 1) %*% t(log(y))
#
# exp(log_denom[1:5, ] - log_Balpha[1:5])
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
