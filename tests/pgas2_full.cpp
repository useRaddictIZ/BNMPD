#include <RcppArmadillo.h>
#include <RcppArmadilloExtensions/sample.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace arma;
using namespace Rcpp;

// [[Rcpp::export]]
arma::vec f_cpp(const arma::vec& x_tt,
                const double& phi_x,
                const double& z_add) {
  int n = x_tt.size();
  arma::vec x_t(n);
  x_t = phi_x * x_tt + z_add;
  // x_t <- phi_x*x_tt + z %*% bet_x
  // xt <- phi_x*xtt
  // xt <- phi_x*xtt + 8*cos(1.2*t)
  // xt <- phi_x*xtt + 25*xtt/(1 + xtt^2)
  // xt <- phi_x*xtt + 25*xtt/(1 + xtt^2) + 8*cos(1.2*t)
  return(x_t);
}

// [[Rcpp::export]]
arma::rowvec f_cpp_vech(const arma::rowvec& x_tt,
                        const double& phi_x,
                        const arma::vec& z_add) {
  int n = x_tt.size();
  arma::rowvec x_t(n);
  x_t = phi_x * x_tt;
  x_t +=  z_add.t();
  // x_t <- phi_x*x_tt + z %*% bet_x
  // xt <- phi_x*xtt
  // xt <- phi_x*xtt + 8*cos(1.2*t)
  // xt <- phi_x*xtt + 25*xtt/(1 + xtt^2)
  // xt <- phi_x*xtt + 25*xtt/(1 + xtt^2) + 8*cos(1.2*t)
  return(x_t);
}

// [[Rcpp::export]]
arma::vec w_as_c(const arma::mat& mean_diff,
                 const arma::rowvec& vcm_diag,
                 const arma::vec& log_weights) {
  int len = mean_diff.n_rows;
  int len2 = mean_diff.n_cols;
  double w_as_max;
  arma::vec w_as(len);
  arma::mat w_as2(len, len2);
  for(int i = 0;  i<len; i++) {
    w_as(i) =  -0.5*arma::as_scalar(dot(mean_diff.row(i), vcm_diag % mean_diff.row(i)));
  }
  w_as = w_as + log_weights;
  w_as_max = w_as.max();
  w_as =  arma::exp(w_as - w_as_max);
  return w_as/sum(w_as);
}

// [[Rcpp::export]]
arma::vec w_bpf_c(const int& N,
                  const int& num_counts,
                  const arma::rowvec& y,
                  const arma::vec& xa1,
                  const arma::vec& xa2,
                  const arma::vec& xa3,
                  const arma::vec& xa4,
                  const arma::vec& xa5,
                  const arma::vec& xa6) {
  arma::vec log_lhs;
  arma::vec log_rhs;
  arma::vec w_log;
  double w_max;
  arma::vec w_tilde;

  arma::mat alphas(N, 6);
  arma::mat alphasP1(N, 4);
  arma::mat alphasP2(N, 2);
  alphasP1 = arma::join_rows(xa1, xa2, xa3, xa4);
  alphasP2 = arma::join_rows(xa5, xa6);
  alphas = arma::join_rows(alphasP1, alphasP2);
  alphas = arma::exp(alphas);

  arma::vec rs_alphas(N);
  rs_alphas = sum(alphas, 1);

  arma::mat alphas_add_y;
  alphas_add_y = alphas;
  alphas_add_y.each_row() += y;
  // OLD WEIGHT FUNCTIONS
  // log_Balpha <- rowSums(lgamma(alphas)) - lgamma(rowSums(alphas))
  // log_denom  <- (alphas - 1) %*% t(log(y))
  // w <- log_denom - log_Balpha
  // browser() OLD WEIGHT FUNCTIONS
  log_lhs = arma::lgamma(rs_alphas) - arma::lgamma(rs_alphas + num_counts);
  log_rhs = arma::sum(arma::lgamma(alphas_add_y) - arma::lgamma(alphas), 1);
  w_log   = log_lhs + log_rhs;
  return w_log;
  // return -arma::lgamma(rs_alphas + num_counts); //
  // return arma::lgamma(rs_alphas);//
  // return arma::lgamma(rs_alphas) - arma::lgamma(rs_alphas + num_counts);//
  // return List::create(arma::lgamma(rs_alphas),
  //                     - arma::lgamma(rs_alphas + num_counts),
  //                     arma::lgamma(rs_alphas) - arma::lgamma(rs_alphas + num_counts));
  //   if (sum(is.nan(w) | is.na(w))) {
  //     stop("NAN or NA values in weight computation!")
  //   }
  //   w
}



// [[Rcpp::export]]
arma::mat cbpf_as_c2_full(const int& N,
                          const int& TT,
                          const arma::vec& num_counts,
                          arma::mat y,
                          const arma::mat& Za1,
                          const arma::mat& Za2,
                          const arma::mat& Za3,
                          const arma::mat& Za4,
                          const arma::mat& Za5,
                          const arma::mat& Za6,
                          const double& sig_sq_xa1,
                          const double& sig_sq_xa2,
                          const double& sig_sq_xa3,
                          const double& sig_sq_xa4,
                          const double& sig_sq_xa5,
                          const double& sig_sq_xa6,
                          const double& phi_xa1,
                          const double& phi_xa2,
                          const double& phi_xa3,
                          const double& phi_xa4,
                          const double& phi_xa5,
                          const double& phi_xa6,
                          const arma::vec& bet_xa1,
                          const arma::vec& bet_xa2,
                          const arma::vec& bet_xa3,
                          const arma::vec& bet_xa4,
                          const arma::vec& bet_xa5,
                          const arma::vec& bet_xa6,
                          const arma::rowvec& xa1_r,
                          const arma::rowvec& xa2_r,
                          const arma::rowvec& xa3_r,
                          const arma::rowvec& xa4_r,
                          const arma::rowvec& xa5_r,
                          const arma::rowvec& xa6_r) {
  // bool filtering
  int D = y.n_cols;
  arma::uvec ind(N);
  NumericVector test_vec(N);
  arma::vec test_vec2(N);
  arma::uvec test_vec3(N);
  NumericVector mmu2(N);

  arma::vec Za1_beta1(TT);
  Za1_beta1 = Za1 * bet_xa1;
  arma::vec Za2_beta2(TT);
  Za2_beta2 = Za2 * bet_xa2;
  arma::vec Za3_beta3(TT);
  Za3_beta3 = Za3 * bet_xa3;
  arma::vec Za4_beta4(TT);
  Za4_beta4 = Za4 * bet_xa4;
  arma::vec Za5_beta5(TT);
  Za5_beta5 = Za5 * bet_xa5;
  arma::vec Za6_beta6(TT);
  Za6_beta6 = Za6 * bet_xa6;

  double sdd = 0;
  double mmu = 0;
  arma::vec eval_f(N);
  arma::vec eval_f2(N);
  // DATA CONTAINERS
  // particle containers for state processes:
  arma::mat xa1(N, TT);
  arma::mat xa2(N, TT);
  arma::mat xa3(N, TT);
  arma::mat xa4(N, TT);
  arma::mat xa5(N, TT);
  arma::mat xa6(N, TT);
  // ancestors
  arma::umat a(N, TT);
  arma::uvec id_as = arma::linspace<arma::uvec>(0L, N - 1L, N);
  arma::uvec id_as_all = arma::linspace<arma::uvec>(0L, N - 1L, N);
  // weights
  double w_max;
  arma::vec w_norm(N);
  arma::vec w_log(N);
  NumericVector w_norm2(N);
  w_norm.fill(1.0/N);
  arma::mat w(N, TT);
  // ancestor weights
  arma::vec as_weights;
  NumericVector as_draw_vec(1);
  double as_draw;
  arma::rowvec vcm_diag = {pow(sig_sq_xa1, -1),
                           pow(sig_sq_xa2, -1),
                           pow(sig_sq_xa3, -1),
                           pow(sig_sq_xa4, -1),
                           pow(sig_sq_xa5, -1),
                           pow(sig_sq_xa6, -1)};
  arma::mat mean_diff(N, D);
  // draw trajectory
  NumericVector b_draw_vec(1);
  int b_draw;
  // output containter
  mat x_out(D, TT);
  //
  // I. INITIALIZATION (t = 0)
  // Sampling initial condition from prior
  mmu = Za1_beta1[0]/(1.0 - phi_xa1);
  sdd = sqrt(sig_sq_xa1/(1.0 - pow(phi_xa1, 2)));
  // xa1( _ , 0) = rnorm(N, mmu, sdd);
  // xa1.col(0) = mmu + sdd * arma::randn(N, 1);
  test_vec = rnorm(N, mmu, sdd);
  test_vec2 = as<arma::vec>(test_vec);
  xa1.col(0) = test_vec2;

  mmu = Za2_beta2[0]/(1.0 - phi_xa2);
  sdd = sqrt(sig_sq_xa2/(1.0 - pow(phi_xa2, 2)));
  // xa2( _ , 0) = rnorm(N, mmu, sdd);
  // xa2.col(0) = mmu + sdd * arma::randn(N, 1);
  test_vec = rnorm(N, mmu, sdd);
  test_vec2 = as<arma::vec>(test_vec);
  xa2.col(0) = test_vec2;

  mmu = Za3_beta3[0]/(1.0 - phi_xa3);
  sdd = sqrt(sig_sq_xa3/(1.0 - pow(phi_xa3, 2)));
  // xa3( _ , 0) = rnorm(N, mmu, sdd);
  // xa3.col(0) = mmu + sdd * arma::randn(N, 1);
  test_vec = rnorm(N, mmu, sdd);
  test_vec2 = as<arma::vec>(test_vec);
  xa3.col(0) = test_vec2;

  mmu = Za4_beta4[0]/(1.0 - phi_xa4);
  sdd = sqrt(sig_sq_xa4/(1.0 - pow(phi_xa4, 2)));
  // xa4( _ , 0) = rnorm(N, mmu, sdd);
  // xa4.col(0) = mmu + sdd * arma::randn(N, 1);
  test_vec = rnorm(N, mmu, sdd);
  test_vec2 = as<arma::vec>(test_vec);
  xa4.col(0) = test_vec2;

  mmu = Za5_beta5[0]/(1.0 - phi_xa5);
  sdd = sqrt(sig_sq_xa5/(1.0 - pow(phi_xa5, 2)));
  // xa5( _ , 0) = rnorm(N, mmu, sdd);
  // xa5.col(0) = mmu + sdd * arma::randn(N, 1);
  test_vec = rnorm(N, mmu, sdd);
  test_vec2 = as<arma::vec>(test_vec);
  xa5.col(0) = test_vec2;

  mmu = Za6_beta6[0]/(1.0 - phi_xa6);
  sdd = sqrt(sig_sq_xa6/(1.0 - pow(phi_xa6, 2)));
  // xa6( _ , 0) = rnorm(N, mmu, sdd);
  // xa6.col(0) = mmu + sdd * arma::randn(N, 1);
  test_vec = rnorm(N, mmu, sdd);
  test_vec2 = as<arma::vec>(test_vec);
  xa6.col(0) = test_vec2;

  // weighting (set to 1/N since there is no measurement y_t=0 at t=0)
  w.col(0) = w_norm;
  w_norm2 = as<NumericVector>(wrap(w_norm));
  // II. FIRST PERIOD APPROXIMATION (t = 1)
  // resampling
  // id_as = Rcpp::RcppArmadillo::sample(id_as, N, true, w_norm);
  test_vec = sample(N, N, true, w_norm2) - 1;
  test_vec3 = as<arma::uvec>(test_vec);
  id_as = test_vec3;
  a.col(0) = id_as;
  // propagation
  eval_f  = f_cpp(xa1.col(0), phi_xa1, Za1_beta1[0]);
  eval_f2 = eval_f.elem(id_as);
  mmu2 = as<NumericVector>(wrap(eval_f2));
  // xa1.col(0) = eval_f + sqrt(sig_sq_xa1)*arma::randn(N, 1);
  test_vec = mmu2 + sqrt(sig_sq_xa1) * rnorm(N);
  test_vec2 = as<arma::vec>(test_vec);
  xa1.col(0) = test_vec2;

  eval_f = f_cpp(xa2.col(0), phi_xa2, Za2_beta2[0]);
  eval_f2 = eval_f.elem(id_as);
  mmu2 = as<NumericVector>(wrap(eval_f2));
  // xa2.col(0) = eval_f + sqrt(sig_sq_xa2)*arma::randn(N, 1);
  test_vec = mmu2 + sqrt(sig_sq_xa2) * rnorm(N);
  test_vec2 = as<arma::vec>(test_vec);
  xa2.col(0) = test_vec2;

  eval_f = f_cpp(xa3.col(0), phi_xa3, Za3_beta3[0]);
  eval_f2 = eval_f.elem(id_as);
  mmu2 = as<NumericVector>(wrap(eval_f2));
  // xa3.col(0) = eval_f + sqrt(sig_sq_xa3)*arma::randn(N, 1);
  test_vec = mmu2 + sqrt(sig_sq_xa3) * rnorm(N);
  test_vec2 = as<arma::vec>(test_vec);
  xa3.col(0) = test_vec2;

  eval_f = f_cpp(xa4.col(0), phi_xa4, Za4_beta4[0]);
  eval_f2 = eval_f.elem(id_as);
  mmu2 = as<NumericVector>(wrap(eval_f2));
  // xa4.col(0) = eval_f + sqrt(sig_sq_xa4)*arma::randn(N, 1);
  test_vec = mmu2 + sqrt(sig_sq_xa4) * rnorm(N);
  test_vec2 = as<arma::vec>(test_vec);
  xa4.col(0) = test_vec2;

  eval_f = f_cpp(xa5.col(0), phi_xa5, Za5_beta5[0]);
  eval_f2 = eval_f.elem(id_as);
  mmu2 = as<NumericVector>(wrap(eval_f2));
  // xa5.col(0) = eval_f + sqrt(sig_sq_xa5)*arma::randn(N, 1);
  test_vec = mmu2 + sqrt(sig_sq_xa5) * rnorm(N);
  test_vec2 = as<arma::vec>(test_vec);
  xa5.col(0) = test_vec2;

  eval_f = f_cpp(xa6.col(0), phi_xa6, Za6_beta6[0]);
  eval_f2 = eval_f.elem(id_as);
  mmu2 = as<NumericVector>(wrap(eval_f2));
  // xa6.col(0) = eval_f + sqrt(sig_sq_xa6)*arma::randn(N, 1);
  test_vec = mmu2 + sqrt(sig_sq_xa6) * rnorm(N);
  test_vec2 = as<arma::vec>(test_vec);
  xa6.col(0) = test_vec2;

  // conditioning
  xa1(N - 1, 0) = xa1_r(0);
  xa2(N - 1, 0) = xa2_r(0);
  xa3(N - 1, 0) = xa3_r(0);
  xa4(N - 1, 0) = xa4_r(0);
  xa5(N - 1, 0) = xa5_r(0);
  xa6(N - 1, 0) = xa6_r(0);
  // weighting
  w_log = w_bpf_c(N, num_counts(0),
                  y.row(0),
                  xa1.col(0),
                  xa2.col(0),
                  xa3.col(0),
                  xa4.col(0),
                  xa5.col(0),
                  xa6.col(0));
  w_max   = w_log.max();
  w_norm = arma::exp(w_log - w_max);
  w_norm =  w_norm/arma::sum(w_norm);
  w.col(0) = w_norm;
  w_norm2 = as<NumericVector>(wrap(w_norm));
  //resampling
  // test_vec = sample(N, N, true, w_norm2) - 1;
  // id_as = test_vec3;
  // a.col(0) = id_as;
  // a[, 1]  <- sample.int(n = N, replace = TRUE, prob = w[, 1])
  // II. FOR t = 2,..,T
  for (int t = 1; t < TT; ++t)  {
    //resampling
    test_vec = sample(N, N, true, w_norm2) - 1;
    test_vec3 = as<arma::uvec>(test_vec);
    id_as = test_vec3;
    a.col(t) = id_as;
    // propagation
    eval_f = f_cpp(xa1.col(t - 1), phi_xa1, Za1_beta1[t]);
    mean_diff.col(0) = eval_f - xa1_r[t];
    eval_f2 = eval_f.elem(id_as);
    mmu2 = as<NumericVector>(wrap(eval_f2));
    // xa1.col(t) = eval_f + sqrt(sig_sq_xa1)*arma::randn(N, 1);
    test_vec = mmu2 + sqrt(sig_sq_xa1) * rnorm(N);
    test_vec2 = as<arma::vec>(test_vec);
    xa1.col(t) = test_vec2;

    eval_f = f_cpp(xa2.col(t - 1), phi_xa2, Za2_beta2[t]);
    mean_diff.col(1) = eval_f - xa2_r[t];
    eval_f2 = eval_f.elem(id_as);
    mmu2 = as<NumericVector>(wrap(eval_f2));
    // xa2.col(t) = eval_f + sqrt(sig_sq_xa2)*arma::randn(N, 1);
    test_vec = mmu2 + sqrt(sig_sq_xa2) * rnorm(N);
    test_vec2 = as<arma::vec>(test_vec);
    xa2.col(t) = test_vec2;

    eval_f = f_cpp(xa3.col(t - 1), phi_xa3, Za3_beta3[t]);
    mean_diff.col(2) = eval_f - xa3_r[t];
    eval_f2 = eval_f.elem(id_as);
    mmu2 = as<NumericVector>(wrap(eval_f2));
    // xa3.col(t) = eval_f + sqrt(sig_sq_xa3)*arma::randn(N, 1);
    test_vec = mmu2 + sqrt(sig_sq_xa3) * rnorm(N);
    test_vec2 = as<arma::vec>(test_vec);
    xa3.col(t) = test_vec2;

    eval_f = f_cpp(xa4.col(t - 1), phi_xa4, Za4_beta4[t]);
    mean_diff.col(3) = eval_f - xa4_r[t];
    eval_f2 = eval_f.elem(id_as);
    mmu2 = as<NumericVector>(wrap(eval_f2));
    // xa4.col(t) = eval_f + sqrt(sig_sq_xa4)*arma::randn(N, 1);
    test_vec = mmu2 + sqrt(sig_sq_xa4) * rnorm(N);
    test_vec2 = as<arma::vec>(test_vec);
    xa4.col(t) = test_vec2;

    eval_f = f_cpp(xa5.col(t - 1), phi_xa5, Za5_beta5[t]);
    mean_diff.col(4) = eval_f - xa5_r[t];
    eval_f2 = eval_f.elem(id_as);
    mmu2 = as<NumericVector>(wrap(eval_f2));
    // xa5.col(t) = eval_f + sqrt(sig_sq_xa5)*arma::randn(N, 1);
    test_vec = mmu2 + sqrt(sig_sq_xa5) * rnorm(N);
    test_vec2 = as<arma::vec>(test_vec);
    xa5.col(t) = test_vec2;

    eval_f = f_cpp(xa6.col(t - 1), phi_xa6, Za6_beta6[t]);
    mean_diff.col(5) = eval_f - xa6_r[t];
    eval_f2 = eval_f.elem(id_as);
    mmu2 = as<NumericVector>(wrap(eval_f2));
    // xa6.col(t) = eval_f + sqrt(sig_sq_xa6)*arma::randn(N, 1);
    test_vec = mmu2 + sqrt(sig_sq_xa6) * rnorm(N);
    test_vec2 = as<arma::vec>(test_vec);
    xa6.col(t) = test_vec2;
    // conditioning
    xa1(N - 1, t) = xa1_r(t);
    xa2(N - 1, t) = xa2_r(t);
    xa3(N - 1, t) = xa3_r(t);
    xa4(N - 1, t) = xa4_r(t);
    xa5(N - 1, t) = xa5_r(t);
    xa6(N - 1, t) = xa6_r(t);
    // ancestor sampling
    as_weights = w_as_c(mean_diff, vcm_diag, w_log);
    w_norm2 = as<NumericVector>(wrap(as_weights));
    as_draw_vec = sample(N, 1, true, w_norm2) - 1;
    as_draw = as_draw_vec(0);
    a(N - 1, t) = as_draw;
    // weighting
    w_log = w_bpf_c(N, num_counts(t),
                    y.row(t),
                    xa1.col(t),
                    xa2.col(t),
                    xa3.col(t),
                    xa4.col(t),
                    xa5.col(t),
                    xa6.col(t));
    w_max   = w_log.max();
    w_norm = arma::exp(w_log - w_max);
    w_norm =  w_norm/arma::sum(w_norm);
    w.col(t) = w_norm;
    w_norm2 = as<NumericVector>(wrap(w_norm));
  }
  ind = a.col(TT - 1);
  arma::uvec t_ind;
  for (arma::uword t = TT-2; t >= 1; --t) {
    arma::uvec t_ind = {t};
    // t_ind(0) = t;
    xa1.col(t) = xa1(ind, t_ind);
    xa2.col(t) = xa2(ind, t_ind);
    xa3.col(t) = xa3(ind, t_ind);
    xa4.col(t) = xa4(ind, t_ind);
    xa5.col(t) = xa5(ind, t_ind);
    xa6.col(t) = xa6(ind, t_ind);
    ind        = a(ind, t_ind);
  }
  t_ind = {0};
  xa1.col(0) = xa1(ind, t_ind);
  xa2.col(0) = xa2(ind, t_ind);
  xa3.col(0) = xa3(ind, t_ind);
  xa4.col(0) = xa4(ind, t_ind);
  xa5.col(0) = xa5(ind, t_ind);
  xa6.col(0) = xa6(ind, t_ind);

  w_norm2 = as<NumericVector>(wrap(w.col(TT - 1)));
  b_draw_vec = sample(N, 1, true, w_norm2) - 1;
  b_draw = b_draw_vec(0);

  x_out.row(0) = xa1.row(b_draw);
  x_out.row(1) = xa2.row(b_draw);
  x_out.row(2) = xa3.row(b_draw);
  x_out.row(3) = xa4.row(b_draw);
  x_out.row(4) = xa5.row(b_draw);
  x_out.row(5) = xa6.row(b_draw);
  return (x_out);
  // return (List::create(xa1.row(b_draw),
  //                     xa2.row(b_draw),
  //                     xa3.row(b_draw),
  //                     xa4.row(b_draw),
  //                     xa5.row(b_draw),
  //                     xa6.row(b_draw)));
  // return(List::create(w, xa1, xa2, xa3, xa4, xa5, xa6));
}


//[[Rcpp::export]]
List pgas2_full(const int& N,
                const int& TT,
                const int& MM,
                const mat& y,
                const vec& num_counts,
                const mat& Za1,
                const mat& Za2,
                const mat& Za3,
                const mat& Za4,
                const mat& Za5,
                const mat& Za6,
                const vec& priors,
                const List& par_init,
                const vec& traj_init) {
  // par_true = NULL,
  // filtering = TRUE,
  // num_plots_states
  int D = par_init.size();
  // Initialize result containers:
  vec w(N);
  mat Xa1(MM, TT);
  mat Xa2(MM, TT);
  mat Xa3(MM, TT);
  mat Xa4(MM, TT);
  mat Xa5(MM, TT);
  mat Xa6(MM, TT);

  mat phi_x(D, MM);
  mat sig_sq_x(D, MM);
  mat out_cPF(D, TT);
  // Initialize helper/garbage containers I.
  double err_siq_sq_x;
  vec z_add(TT - 1);
  vec temp_vec;
  rowvec temp_vec2(TT);
  // Initialize parameters:
  uvec dim_pars(D);
  uvec id_pars(D + 1);
  for(int d = 0; d < D; ++d) {
    temp_vec = as<vec>(par_init(d));
    dim_pars(d) = temp_vec.n_rows;
    sig_sq_x(d, 0) = temp_vec(0);
    phi_x(d, 0) = temp_vec(1);
  }
  double num_pars = sum(dim_pars);
  id_pars(0) = 0;
  temp_vec = cumsum(as<vec>(wrap(dim_pars)));
  id_pars.subvec(1, D) = cumsum(dim_pars - 2);
  // minus 2 because of minus phi and minus sigma
  mat bet(sum(dim_pars) - 2*D, MM);
  for(int d = 0; d < D; ++d) {
    temp_vec = as<vec>(par_init(d));
    int testval = temp_vec.n_rows - 1;
    bet.submat(id_pars(d), 0, id_pars(d + 1) - 1, 0) = temp_vec.subvec(2, testval);
  }
  // Initialize regressor containers:
  mat Z;
  Z = join_rows(Za1.submat(1, 0, TT - 1, dim_pars(0) - 2),
                Za2.submat(1, 0, TT - 1, dim_pars(1) - 2),
                Za3.submat(1, 0, TT - 1, dim_pars(2) - 2),
                Za4.submat(1, 0, TT - 1, dim_pars(3) - 2));
  Z = join_rows(Z,
                Za5.submat(1, 0, TT - 1, dim_pars(4) - 2),
                Za6.submat(1, 0, TT - 1, dim_pars(5) - 2));
  // Initialize priors:
  double prior_a = priors(0) + (TT - 1)/2.0;
  double prior_b = priors(1);
  temp_vec = ones(dim_pars(0) - 1)/1000;
  mat prior_V_xa1 = diagmat(temp_vec);
  temp_vec = ones(dim_pars(1) - 1)/1000;
  mat prior_V_xa2 = diagmat(temp_vec);
  temp_vec = ones(dim_pars(2) - 1)/1000;
  mat prior_V_xa3 = diagmat(temp_vec);
  temp_vec = ones(dim_pars(3) - 1)/1000;
  mat prior_V_xa4 = diagmat(temp_vec);
  temp_vec = ones(dim_pars(4) - 1)/1000;
  mat prior_V_xa5 = diagmat(temp_vec);
  temp_vec = ones(dim_pars(5) - 1)/1000;
  mat prior_V_xa6 = diagmat(temp_vec);
  //Initialize states
  // ## I. Set states to deterministic starting values
  temp_vec2.fill(traj_init(0));
  Xa1.row(0) = temp_vec2;
  temp_vec2.fill(traj_init(1));
  Xa2.row(0) = temp_vec2;
  temp_vec2.fill(traj_init(2));
  Xa3.row(0) = temp_vec2;
  temp_vec2.fill(traj_init(3));
  Xa4.row(0) = temp_vec2;
  temp_vec2.fill(traj_init(4));
  Xa5.row(0) = temp_vec2;
  temp_vec2.fill(traj_init(5));
  Xa6.row(0) = temp_vec2;
  // Initialize gahelper/garbage II
  mat Omega_xa1(dim_pars(0) -1, dim_pars(0) -1);
  mat Omega_xa2(dim_pars(1) -1, dim_pars(1) -1);
  mat Omega_xa3(dim_pars(2) -1, dim_pars(2) -1);
  mat Omega_xa4(dim_pars(3) -1, dim_pars(3) -1);
  mat Omega_xa5(dim_pars(4) -1, dim_pars(4) -1);
  mat Omega_xa6(dim_pars(5) -1, dim_pars(5) -1);
  vec mu_xa1(dim_pars(0) -1);
  vec mu_xa2(dim_pars(1) -1);
  vec mu_xa3(dim_pars(2) -1);
  vec mu_xa4(dim_pars(3) -1);
  vec mu_xa5(dim_pars(4) -1);
  vec mu_xa6(dim_pars(5) -1);
  // II. run cBPF and use output as first conditioning trajectory
  // monitor_pgas_states(states_drawn = cbind(exp(Xa1[1, ]), exp(Xa2[1, ]),
  //                                          exp(Xa3[1, ]), exp(Xa4[1, ]),
  //                                          exp(Xa5[1, ]), exp(Xa6[1, ])),
  //                     states_comp = cbind(exp(states_init_1), exp(states_init_2),
  //                                          exp(states_init_3), exp(states_init_4),
  //                                          exp(states_init_5), exp(states_init_6)),
  //                       // NULL,
  //                       // cbind(xa1_t, xa2_t,
  //                       //                    xa3_t, xa4_t,
  //                       //                    xa5_5, xa6_t),
  //                     current = 1, total = 1, num_prints = 1)
  out_cPF = cbpf_as_c2_full(N, TT,
                            num_counts, y,
                            Za1, Za2, Za3, Za4, Za5, Za6,
                            sig_sq_x(0, 0),
                            sig_sq_x(1, 0),
                            sig_sq_x(2, 0),
                            sig_sq_x(3, 0),
                            sig_sq_x(4, 0),
                            sig_sq_x(5, 0),
                            phi_x(0, 0),
                            phi_x(1, 0),
                            phi_x(2, 0),
                            phi_x(3, 0),
                            phi_x(4, 0),
                            phi_x(5, 0),
                            bet.submat(id_pars(0), 0, id_pars(0 + 1) - 1, 0),
                            bet.submat(id_pars(1), 0, id_pars(1 + 1) - 1, 0),
                            bet.submat(id_pars(2), 0, id_pars(2 + 1) - 1, 0),
                            bet.submat(id_pars(3), 0, id_pars(3 + 1) - 1, 0),
                            bet.submat(id_pars(4), 0, id_pars(4 + 1) - 1, 0),
                            bet.submat(id_pars(5), 0, id_pars(5 + 1) - 1, 0),
                            Xa1.row(0), Xa2.row(0), Xa3.row(0),
                            Xa4.row(0), Xa5.row(0), Xa6.row(0));
  Xa1.row(0) = out_cPF.row(0);
  Xa2.row(0) = out_cPF.row(1);
  Xa3.row(0) = out_cPF.row(2);
  Xa4.row(0) = out_cPF.row(3);
  Xa5.row(0) = out_cPF.row(4);
  Xa6.row(0) = out_cPF.row(5);
  // monitor_pgas_states(states_drawn = cbind(exp(Xa1[1, ]), exp(Xa2[1, ]),
  //                                          exp(Xa3[1, ]), exp(Xa4[1, ]),
  //                                          exp(Xa5[1, ]), exp(Xa6[1, ])),
  //                     // states_comp  = cbind(xa1_t, xa2_t,
  //                     //                      xa3_t, xa4_t,
  //                     //                      xa5_t, xa6_t),
  //                                  cbind(exp(states_init_1), exp(states_init_2),
  //                                        exp(states_init_3), exp(states_init_4),
  //                                        exp(states_init_5), exp(states_init_6)),
  //                     current = 1, total = 1, num_prints = 1)
  // Run MCMC loop
  // Z.col(id_pars(0)) = (Xa1.row(0)).t();
  // Z.col(id_pars(1)) = (Xa2.row(0)).t();
  // Z.col(id_pars(2)) = (Xa3.row(0)).t();
  // Z.col(id_pars(3)) = (Xa4.row(0)).t();
  // Z.col(id_pars(4)) = (Xa5.row(0)).t();
  // Z.col(id_pars(5)) = (Xa6.row(0)).t();
  temp_vec2.resize(TT - 1);
  for (int m = 1; m < 2; ++m) {
  // I. Run GIBBS part
  Z.col(id_pars(0)) = (Xa1.submat(m-1, 0, m-1, TT - 2)).t();
  Z.col(id_pars(1)) = (Xa2.submat(m-1, 0, m-1, TT - 2)).t();
  Z.col(id_pars(2)) = (Xa3.submat(m-1, 0, m-1, TT - 2)).t();
  Z.col(id_pars(3)) = (Xa4.submat(m-1, 0, m-1, TT - 2)).t();
  Z.col(id_pars(4)) = (Xa5.submat(m-1, 0, m-1, TT - 2)).t();
  Z.col(id_pars(5)) = (Xa6.submat(m-1, 0, m-1, TT - 2)).t();
  // 1. pars for xa1_t process --------------------------------------------
  temp_vec2 = Xa1.submat(m - 1, 1, m - 1, TT - 1);
  z_add =  Za1.submat(1, 0, TT - 1, dim_pars(0) - 3) * bet.submat(id_pars(0),  m - 1, id_pars(0 + 1) - 1,  m - 1);
  temp_vec2 -= f_cpp_vech(Xa1.submat(m - 1, 0, m - 1, TT - 2),
                          phi_x(0, m - 1),
                          z_add);
  err_siq_sq_x = (dot(temp_vec2, temp_vec2)) * 0.5;
  sig_sq_x(0, m)  = 1/(R::rgamma(prior_a, 1.0/(prior_b + err_siq_sq_x)));

  // regs_a1(, 1)  = Xa1(m - 1, 1:(TT - 1))
  // x_lhs        = Xa1(m - 1, 2:TT)
  Omega_xa1    = inv((trans(Z.cols(id_pars(0), id_pars(1) - 1)) * Z.cols(id_pars(0), id_pars(1) - 1))/sig_sq_x(0, m) + prior_V_xa1);
  // mu_xa1       = Omega_xa1 %*% (crossprod(regs_a1, x_lhs)/sig_sq_xa1(m)

  //  Z = join_rows(Za1.submat(1, 0, TT - 1, dim_pars(0) - 3),
  //               Za2.submat(1, 0, TT - 1, dim_pars(1) - 3),
  //               Za3.submat(1, 0, TT - 1, dim_pars(2) - 3),
  //               Za4.submat(1, 0, TT - 1, dim_pars(3) - 3));
  // Z = join_rows(Z,
  //               Za5.submat(1, 0, TT - 1, dim_pars(4) - 3),
  //               Za6.submat(1, 0, TT - 1, dim_pars(5) - 3));

  // beta_xa1     = rmvnorm(n = 1, mean = mu_xa1, sigma = Omega_xa1)
  // phi_xa1(m)   = beta_xa1(1)
  // bet_xa1(, m) = beta_xa1(-1)
  // while (near(abs(phi_xa1(m), 1, tol = 0.01) | abs(phi_xa1(m) > 1) {
  // beta_xa1     = rmvnorm(n = 1, mean = mu_xa1, sigma = Omega_xa1)
  // phi_xa1(m)   = beta_xa1(1)
  // bet_xa1(, m) = beta_xa1(-1)
  }
  // }
  //
  //
  //
  //
  //
  //
  // return(List::create(1, bet, Z, prior_V_xa1, //Z.cols(25, 29),
  //                     id_pars, dim_pars(0), id_pars(1),Xa1.row(0),
  //                     Xa2.row(0), Xa3.row(0), Xa4.row(0), Xa5.row(0), Xa6.row(0)));
  // return(List::create(Xa1.row(0),
  //                     Xa2.row(0),
  //                     Xa3.row(0),
  //                     Xa4.row(0),
  //                     Xa5.row(0),
  //                     Xa6.row(0)));
  // return(List::create(sig_sq_x.row(0)));
  // return(List::create((Z.col(id_pars(0)), (Xa1.submat(0, 0, 0, TT - 2)).t()),
  //        Z.col(id_pars(1)), (Xa2.submat(0, 0, 0, TT - 2)).t(),
  //        Z.col(id_pars(2)), (Xa3.submat(0, 0, 0, TT - 2)).t(),
  //        Z.col(id_pars(3)), (Xa4.submat(0, 0, 0, TT - 2)).t(),
  //        Z.col(id_pars(4)), (Xa5.submat(0, 0, 0, TT - 2)).t(),
  //        Z.col(id_pars(5)), (Xa6.submat(0, 0, 0, TT - 2)).t()
  //        ));
  return(List::create(trans(Z.cols(id_pars(0), id_pars(1) - 1)) * Z.cols(id_pars(0), id_pars(1) - 1)/sig_sq_x(0, 1),
                      prior_V_xa1, Omega_xa1,
         sig_sq_x(0, 1)));
}
// for (m in 2:MM) {
//   // I. Run GIBBS part
//   // 1. pars for xa1_t process --------------------------------------------
//   err_sig_sq_x = Xa1[m - 1, 2:TT] - f(x_tt = Xa1[m - 1, 1:(TT - 1)],
//                                     z = Za1[2:TT, , drop = F],
//                                     phi_x = phi_xa1[m - 1],
//                                     bet_x = bet_xa1[, m - 1])
//   sig_sq_xa1[m]  = 1/rgamma(n = 1, prior_a + (TT - 1)/2,
//                            prior_b + crossprod(err_sig_sq_x)/2)
//   regs_a1[, 1]  = Xa1[m - 1, 1:(TT - 1)]
//   x_lhs        = Xa1[m - 1, 2:TT]
//   Omega_xa1    = solve(crossprod(regs_a1, regs_a1)/sig_sq_xa1[m] + prior_V_xa1)
//   mu_xa1       = Omega_xa1 %*% (crossprod(regs_a1, x_lhs)/sig_sq_xa1[m])
//   beta_xa1     = rmvnorm(n = 1, mean = mu_xa1, sigma = Omega_xa1)
//   phi_xa1[m]   = beta_xa1[1]
//   bet_xa1[, m] = beta_xa1[-1]
//   while (near(abs(phi_xa1[m]), 1, tol = 0.01) | abs(phi_xa1[m]) > 1) {
//   beta_xa1     = rmvnorm(n = 1, mean = mu_xa1, sigma = Omega_xa1)
//   phi_xa1[m]   = beta_xa1[1]
//   bet_xa1[, m] = beta_xa1[-1]
//   }
//   // 2. pars for xa2_t process --------------------------------------------
//   err_sig_sq_x = Xa2[m - 1, 2:TT] - f(x_tt =  Xa2[m - 1, 1:(TT - 1)],
//                                     z = Za2[2:TT, , drop = F],
//                                     phi_x = phi_xa2[m - 1],
//                                     bet_x = bet_xa2[, m - 1])
//   sig_sq_xa2[m]  = 1/rgamma(n = 1, prior_a + (TT - 1)/2,
//                             prior_b + crossprod(err_sig_sq_x)/2)
//   regs_a2[, 1] = Xa2[m - 1, 1:(TT - 1)]
//   x_lhs        = Xa2[m - 1, 2:TT]
//   Omega_xa2    = solve(crossprod(regs_a2, regs_a2)/sig_sq_xa2[m] + prior_V_xa2)
//   mu_xa2       = Omega_xa2 %*% (crossprod(regs_a2, x_lhs)/sig_sq_xa2[m])
//   beta_xa2     = rmvnorm(n = 1, mean = mu_xa2, sigma = Omega_xa2)
//   phi_xa2[m]   = beta_xa2[1]
//   bet_xa2[, m] = beta_xa2[-1]
//   while (near(abs(phi_xa2[m]), 1, tol = 0.01) | abs(phi_xa2[m]) > 1) {
//     beta_xa2     = rmvnorm(n = 1, mean = mu_xa2, sigma = Omega_xa2)
//     phi_xa2[m]   = beta_xa2[1]
//     bet_xa2[, m] = beta_xa2[-1]
//   }
//   // 3. pars for xa3_t process --------------------------------------------
//   err_sig_sq_x = Xa3[m - 1, 2:TT] - f(x_tt =  Xa3[m - 1, 1:(TT - 1)],
//                                       z = Za3[2:TT, , drop = F],
//                                       phi_x = phi_xa3[m - 1],
//                                       bet_x = bet_xa3[, m - 1])
//   sig_sq_xa3[m]  = 1/rgamma(n = 1, prior_a + (TT - 1)/2,
//                             prior_b + crossprod(err_sig_sq_x)/2)
//   regs_a3[, 1] = Xa3[m - 1, 1:(TT - 1)]
//   x_lhs        = Xa3[m - 1, 2:TT]
//   Omega_xa3    = solve(crossprod(regs_a3, regs_a3)/sig_sq_xa3[m] + prior_V_xa3)
//   mu_xa3       = Omega_xa3 %*% (crossprod(regs_a3, x_lhs)/sig_sq_xa3[m])
//   beta_xa3     = rmvnorm(n = 1, mean = mu_xa3, sigma = Omega_xa3)
//   phi_xa3[m]   = beta_xa3[1]
//   bet_xa3[, m] = beta_xa3[-1]
//   while (near(abs(phi_xa3[m]), 1, tol = 0.01) | abs(phi_xa3[m]) > 1) {
//     beta_xa3     = rmvnorm(n = 1, mean = mu_xa3, sigma = Omega_xa3)
//     phi_xa3[m]   = beta_xa3[1]
//     bet_xa3[, m] = beta_xa3[-1]
//   }
//   // 4. pars for xa4_t process --------------------------------------------
//   err_sig_sq_x = Xa4[m - 1, 2:TT] - f(x_tt = Xa4[m - 1, 1:(TT - 1)],
//                                       z = Za4[2:TT, , drop = F],
//                                       phi_x = phi_xa4[m - 1],
//                                       bet_x = bet_xa4[, m - 1])
//   sig_sq_xa4[m]  = 1/rgamma(n = 1, prior_a + (TT - 1)/2,
//                             prior_b + crossprod(err_sig_sq_x)/2)
//   regs_a4[, 1] = Xa4[m - 1, 1:(TT - 1)]
//   x_lhs        = Xa4[m - 1, 2:TT]
//   Omega_xa4    = solve(crossprod(regs_a4, regs_a4)/sig_sq_xa4[m] + prior_V_xa4)
//   mu_xa4       = Omega_xa4 %*% (crossprod(regs_a4, x_lhs)/sig_sq_xa4[m])
//   beta_xa4     = rmvnorm(n = 1, mean = mu_xa4, sigma = Omega_xa4)
//   phi_xa4[m]   = beta_xa4[1]
//   bet_xa4[, m] = beta_xa4[-1]
//   while (near(abs(phi_xa4[m]), 1, tol = 0.01) | abs(phi_xa4[m]) > 1) {
//     beta_xa4     = rmvnorm(n = 1, mean = mu_xa4, sigma = Omega_xa4)
//     phi_xa4[m]   = beta_xa4[1]
//     bet_xa4[, m] = beta_xa4[-1]
//   }
//   // 5. pars for xa5_t process --------------------------------------------
//   err_sig_sq_x = Xa5[m - 1, 2:TT] - f(x_tt = Xa5[m - 1, 1:(TT - 1)],
//                                        z = Za5[2:TT, , drop = F],
//                                        phi_x = phi_xa5[m - 1],
//                                        bet_x = bet_xa5[, m - 1])
//   sig_sq_xa5[m]  = 1/rgamma(n = 1, prior_a + (TT - 1)/2,
//                              prior_b + crossprod(err_sig_sq_x)/2)
//   regs_a5[, 1] = Xa5[m - 1, 1:(TT - 1)]
//   x_lhs        = Xa5[m - 1, 2:TT]
//   Omega_xa5    = solve(crossprod(regs_a5, regs_a5)/sig_sq_xa5[m] + prior_V_xa5)
//   mu_xa5       = Omega_xa5 %*% (crossprod(regs_a5, x_lhs)/sig_sq_xa5[m])
//   beta_xa5     = rmvnorm(n = 1, mean = mu_xa5, sigma = Omega_xa5)
//   phi_xa5[m]   = beta_xa5[1]
//   bet_xa5[, m] = beta_xa5[-1]
//   while (near(abs(phi_xa5[m]), 1, tol = 0.01) | abs(phi_xa5[m]) > 1) {
//     beta_xa5     = rmvnorm(n = 1, mean = mu_xa5, sigma = Omega_xa5)
//     phi_xa5[m]   = beta_xa5[1]
//     bet_xa5[, m] = beta_xa5[-1]
//   }
//   // 6. pars for xa6_t process --------------------------------------------
//   err_sig_sq_x = Xa6[m - 1, 2:TT] - f(x_tt = Xa6[m - 1, 1:(TT - 1)],
//                                        z = Za6[2:TT, , drop = F],
//                                        phi_x = phi_xa6[m - 1],
//                                        bet_x = bet_xa6[, m - 1])
//   sig_sq_xa6[m]  = 1/rgamma(n = 1, prior_a + (TT - 1)/2,
//                              prior_b + crossprod(err_sig_sq_x)/2)
//   regs_a6[, 1] = Xa6[m - 1, 1:(TT - 1)]
//   x_lhs        = Xa6[m - 1, 2:TT]
//   Omega_xa6    = solve(crossprod(regs_a6, regs_a6)/sig_sq_xa6[m] + prior_V_xa6)
//   mu_xa6       = Omega_xa6 %*% (crossprod(regs_a6, x_lhs)/sig_sq_xa6[m])
//   beta_xa6     = rmvnorm(n = 1, mean = mu_xa6, sigma = Omega_xa6)
//   phi_xa6[m]   = beta_xa6[1]
//   bet_xa6[, m] = beta_xa6[-1]
//   while (near(abs(phi_xa6[m]), 1, tol = 0.01) | abs(phi_xa6[m]) > 1) {
//     beta_xa6     = rmvnorm(n = 1, mean = mu_xa6, sigma = Omega_xa6)
//     phi_xa6[m]   = beta_xa6[1]
//     bet_xa6[, m] = beta_xa6[-1]
//   }
//   // II. Run cBPF-AS part
//   out_cPF = cbpf_as_c2_full(y = y, num_counts = num_counts,
//                         Za1 = Za1, Za2 = Za2, Za3 = Za3,
//                         Za4 = Za4, Za5 = Za5, Za6 = Za6,
//                         N = N, TT = TT,
//                         sig_sq_xa1 = sig_sq_xa1[m],
//                         phi_xa1 = phi_xa1[m],
//                         bet_xa1 = bet_xa1[, m, drop = F],
//                         xa1_r = Xa1[m - 1,],
//                         sig_sq_xa2 = sig_sq_xa2[m],
//                         phi_xa2 = phi_xa2[m],
//                         bet_xa2 = bet_xa2[, m, drop = F],
//                         xa2_r = Xa2[m - 1,],
//                         sig_sq_xa3 = sig_sq_xa3[m],
//                         phi_xa3 = phi_xa3[m],
//                         bet_xa3 = bet_xa3[, m, drop = F],
//                         xa3_r = Xa3[m - 1,],
//                         sig_sq_xa4 = sig_sq_xa4[m],
//                         phi_xa4 = phi_xa4[m],
//                         bet_xa4 = bet_xa4[, m, drop = F],
//                         xa4_r = Xa4[m - 1, ],
//                         sig_sq_xa5 = sig_sq_xa5[m],
//                         phi_xa5 = phi_xa5[m],
//                         bet_xa5 = bet_xa5[, m, drop = F],
//                         xa5_r = Xa5[m - 1, ],
//                         sig_sq_xa6 = sig_sq_xa6[m],
//                         phi_xa6 = phi_xa6[m],
//                         bet_xa6 = bet_xa6[, m, drop = F],
//                         xa6_r = Xa6[m - 1, ])
//   w        = out_cPF[[1]][, TT]
//   b        = sample.int(n = N, size = 1, replace = TRUE, prob = w)
//   Xa1[m, ] = out_cPF[[2]][b, ]
//   Xa2[m, ] = out_cPF[[3]][b, ]
//   Xa3[m, ] = out_cPF[[4]][b, ]
//   Xa4[m, ] = out_cPF[[5]][b, ]
//   Xa5[m, ] = out_cPF[[6]][b, ]
//   Xa6[m, ] = out_cPF[[7]][b, ]
// monitor_pgas_states(states_drawn = cbind(exp(Xa1[m, ]), exp(Xa2[m, ]),
//                                          exp(Xa3[m, ]), exp(Xa4[m, ]),
//                                          exp(Xa5[m, ]), exp(Xa6[m, ])),
//                     // states_comp = cbind(exp(states_init_1), exp(states_init_2),
//                     //                      exp(states_init_3), exp(states_init_4),
//                     //                      exp(states_init_5), exp(states_init_6)),
//                     states_comp = cbind(exp(Xa1[m - 1, ]), exp(Xa2[m - 1, ]),
//                                         exp(Xa3[m - 1, ]), exp(Xa4[m - 1, ]),
//                                         exp(Xa5[m - 1, ]), exp(Xa6[m - 1, ])),
//                       // NULL,
//                       // cbind(xa1_t, xa2_t, xa3_t,
//                       //                    xa4_t, xa5_t, xa6_t),
//                     current = m, total = MM,
//                     num_prints = num_plots_states)
// monitor_pgas_time(m, MM, len = MM)
// monitor_pgas_mcmc2(m, MM, len = MM,
//                    val_init = par_init,
//                    current_pars = cbind(sig_sq_xa1[1:m], phi_xa1[1:m],
//                                         t(bet_xa1)[1:m,],
//                                         sig_sq_xa2[1:m], phi_xa2[1:m],
//                                         t(bet_xa2)[1:m,],
//                                         sig_sq_xa3[1:m], phi_xa3[1:m],
//                                         t(bet_xa3)[1:m,],
//                                         sig_sq_xa4[1:m], phi_xa4[1:m],
//                                         t(bet_xa4)[1:m,],
//                                         sig_sq_xa5[1:m], phi_xa5[1:m],
//                                         t(bet_xa5)[1:m,],
//                                         sig_sq_xa6[1:m], phi_xa6[1:m],
//                                         t(bet_xa6)[1:m,]),
//                    dim_all = dim_all)
// monitor_pgas_mcmc2(m, MM, len = MM,
//                    val_true = par_true,
//                    val_init = par_init,
//                    current_pars = cbind(sig_sq_xa1[1:m], phi_xa1[1:m],
//                                         t(bet_xa1)[1:m,],
//                                         sig_sq_xa2[1:m], phi_xa2[1:m],
//                                         t(bet_xa2)[1:m,],
//                                         sig_sq_xa3[1:m], phi_xa3[1:m],
//                                         t(bet_xa3)[1:m,],
//                                         sig_sq_xa4[1:m], phi_xa4[1:m],
//                                         t(bet_xa4)[1:m,],
//                                         sig_sq_xa5[1:m], phi_xa5[1:m],
//                                         t(bet_xa5)[1:m,],
//                                         sig_sq_xa6[1:m], phi_xa6[1:m],
//                                         t(bet_xa6)[1:m,]),
//                    dim_all = dim_all)
// }
// return(list(sigma_sq_xa1 = sig_sq_xa1,
//             phi_xa1 = phi_xa1,
//             bet_xa1 = bet_xa1,
//             sigma_sq_xa2 = sig_sq_xa2,
//             phi_xa2 = phi_xa2,
//             bet_xa2 = bet_xa2,
//             sigma_sq_xa3 = sig_sq_xa3,
//             phi_xa3 = phi_xa3,
//             bet_xa3 = bet_xa3,
//             sigma_sq_xa4 = sig_sq_xa4,
//             phi_xa4 = phi_xa4,
//             bet_xa4 = bet_xa4,
//             sigma_sq_xa5 = sig_sq_xa5,
//             phi_xa5 = phi_xa5,
//             bet_xa5 = bet_xa5,
//             sigma_sq_xa6 = sig_sq_xa6,
//             phi_xa6 = phi_xa6,
//             bet_xa6 = bet_xa6,
//             xtraj  = list(Xa1, Xa2, Xa3, Xa4, Xa5, Xa6)))

