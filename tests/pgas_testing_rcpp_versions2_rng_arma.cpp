// #define ARMA_NO_DEBUG
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
arma::vec f_cpp_vech(const arma::vec& x_tt,
                        const double& phi_x,
                        const arma::vec& z_add) {
  int n = x_tt.size();
  arma::vec x_t(n);
  x_t = phi_x * x_tt;
  x_t +=  z_add;
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
arma::vec mvrnorm_c(const arma::vec& mu, const arma::mat& Sigma){

  // Obtain environment containing function
  // Rcpp::Environment base("package:MASS");
  Environment pkg = Environment::namespace_env("MASS");
  // Make function callable from C++
  Rcpp::Function mvrnorm_c_internal = pkg["mvrnorm"];


  Rcpp::NumericVector mu2 = as<NumericVector>(wrap(mu));
  Rcpp::NumericMatrix Sigma2 = as<NumericMatrix>(wrap(Sigma));
  // Call the function and receive its list output
  Rcpp::NumericVector res;
  res = mvrnorm_c_internal(Rcpp::_["n"]         = 1,
                           Rcpp::_["mu"]        = mu2,
                           Rcpp::_["Sigma"]     = Sigma2,
                           Rcpp::_["tol"]       = 1e-06,
                           Rcpp::_["empirical"] = false,
                           Rcpp::_["EISPACK"]   = false);
  arma::vec res2 = as<arma::vec>(res);

  return res2;
}

// [[Rcpp::export]]
arma::mat cbpf_as_c5_full(const int& N,
                          const int& TT,
                          const arma::vec& num_counts,
                          const arma::mat& y,
                          const arma::mat& Z,
                          const arma::uvec& id_bet,
                          const arma::vec& sig_sq_x,
                          const arma::vec& phi_x,
                          const arma::vec& bet_x,
                          const arma::vec& x_r) {
  // bool filtering
  int D = y.n_cols;
  arma::uvec ind(N);
  NumericVector mmu2(N);

  arma::mat Za_beta(TT, D);
  for (int d = 0; d<D; ++d) {
    Za_beta.col(d) = Z.submat(0, id_bet(d), TT - 1, id_bet(d + 1) - 1) * bet_x.subvec(id_bet(d), id_bet(d + 1) - 1);
  }

  double sdd = 0;
  double mmu = 0;
  arma::vec eval_f(N);
  // DATA CONTAINERS
  // particle containers for state processes:
  arma::mat xa(D*N, TT);
  uvec id_x(D + 1);
  for (int d = 0; d<D+1; ++d) {
    id_x(d) = d*N;
  }

  arma::mat xa1(N, TT);
  arma::mat xa2(N, TT);
  arma::mat xa3(N, TT);
  arma::mat xa4(N, TT);
  arma::mat xa5(N, TT);
  arma::mat xa6(N, TT);
  // ancestors
  arma::umat a(N, TT);
  arma::uvec id_as = arma::linspace<arma::uvec>(0L, N - 1L, N);
  arma::uvec id_as_lnspc = arma::linspace<arma::uvec>(0L, N - 1L, N);
  // weights
  double w_max;
  arma::vec w_norm(N);
  arma::vec w_log(N);
  w_norm.fill(1.0/N);
  arma::mat w(N, TT);
  // ancestor weights
  arma::vec as_weights(N);
  arma::uvec as_draw_vec(1);
  double as_draw;
  arma::rowvec vcm_diag = {arma::pow(sig_sq_x.t(), -1)};

  arma::mat mean_diff(N, D);
  // draw trajectory
  NumericVector b_draw_vec(1);
  int b_draw;
  // output containter
  mat x_out(TT, D);
  // I. INITIALIZATION (t = 0)
  // Sampling initial condition from prior
  mmu = as_scalar(Za_beta.submat(0, 0, 0, 0))/(1.0 - phi_x(0));
  sdd = sqrt(sig_sq_x(0)/(1.0 - pow(phi_x(0), 2)));
  xa1.col(0) = mmu + sdd * arma::randn(N, 1);
  xa.submat(id_x(0), 0, id_x(1) - 1, 0) = xa1.col(0);

  mmu = as_scalar(Za_beta.submat(0, 1, 0, 1))/(1.0 - phi_x(1));
  sdd = sqrt(sig_sq_x(1)/(1.0 - pow(phi_x(1), 2)));
  xa2.col(0) = mmu + sdd * arma::randn(N, 1);
  xa.submat(id_x(1), 0, id_x(2) - 1, 0) = xa2.col(0);

  mmu = as_scalar(Za_beta.submat(0, 2, 0, 2))/(1.0 - phi_x(2));
  sdd = sqrt(sig_sq_x(2)/(1.0 - pow(phi_x(2), 2)));
  xa3.col(0) = mmu + sdd * arma::randn(N, 1);
  xa.submat(id_x(2), 0, id_x(3) - 1, 0) = xa3.col(0);

  mmu = as_scalar(Za_beta.submat(0, 3, 0, 3))/(1.0 - phi_x(3));
  sdd = sqrt(sig_sq_x(3)/(1.0 - pow(phi_x(3), 2)));
  xa4.col(0) = mmu + sdd * arma::randn(N, 1);
  xa.submat(id_x(3), 0, id_x(4) - 1, 0) = xa4.col(0);

  mmu = as_scalar(Za_beta.submat(0, 4, 0, 4))/(1.0 - phi_x(4));
  sdd = sqrt(sig_sq_x(4)/(1.0 - pow(phi_x(4), 2)));
  xa5.col(0) = mmu + sdd * arma::randn(N, 1);
  xa.submat(id_x(4), 0, id_x(5) - 1, 0) = xa5.col(0);

  mmu = as_scalar(Za_beta.submat(0, 5, 0, 5))/(1.0 - phi_x(5));
  sdd = sqrt(sig_sq_x(5)/(1.0 - pow(phi_x(5), 2)));
  xa6.col(0) = mmu + sdd * arma::randn(N, 1);
  xa.submat(id_x(5), 0, id_x(6) - 1, 0) = xa6.col(0);

  // weighting (set to 1/N since there is no measurement y_t=0 at t=0)
  w.col(0) = w_norm;
  // II. FIRST PERIOD APPROXIMATION (t = 1)
  // resampling
  id_as = Rcpp::RcppArmadillo::sample(id_as_lnspc, N, true, w_norm);
  a.col(0) = id_as;
  // propagation
  eval_f = f_cpp(xa1.col(0), phi_x(0), as_scalar(Za_beta.submat(0, 0, 0, 0)));
  eval_f = eval_f.elem(id_as);
  xa1.col(0) = eval_f + sqrt(sig_sq_x(0))*arma::randn(N, 1);

  eval_f = f_cpp(xa2.col(0), phi_x(1), as_scalar(Za_beta.submat(0, 1, 0, 1)));
  eval_f = eval_f.elem(id_as);
  xa2.col(0) = eval_f + sqrt(sig_sq_x(1))*arma::randn(N, 1);

  eval_f = f_cpp(xa3.col(0), phi_x(2), as_scalar(Za_beta.submat(0, 2, 0, 2)));
  eval_f = eval_f.elem(id_as);
  xa3.col(0) = eval_f + sqrt(sig_sq_x(2))*arma::randn(N, 1);

  eval_f = f_cpp(xa4.col(0), phi_x(3), as_scalar(Za_beta.submat(0, 3, 0, 3)));
  eval_f = eval_f.elem(id_as);
  xa4.col(0) = eval_f + sqrt(sig_sq_x(3))*arma::randn(N, 1);

  eval_f = f_cpp(xa5.col(0), phi_x(4), as_scalar(Za_beta.submat(0, 4, 0, 4)));
  eval_f = eval_f.elem(id_as);
  xa5.col(0) = eval_f + sqrt(sig_sq_x(4))*arma::randn(N, 1);

  eval_f = f_cpp(xa6.col(0), phi_x(5), as_scalar(Za_beta.submat(0, 5, 0, 5)));
  eval_f = eval_f.elem(id_as);
  xa6.col(0) = eval_f + sqrt(sig_sq_x(5))*arma::randn(N, 1);

  // conditioning
  xa1(N - 1, 0) = x_r(0);
  xa2(N - 1, 0) = x_r(TT + 0);
  xa3(N - 1, 0) = x_r(TT*2 + 0);
  xa4(N - 1, 0) = x_r(TT*3 + 0);
  xa5(N - 1, 0) = x_r(TT*4 + 0);
  xa6(N - 1, 0) = x_r(TT*5 + 0);
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
  // w_norm2 = as<NumericVector>(wrap(w_norm));
  // II. FOR t = 2,..,T
  for (int t = 1; t < TT; ++t)  {
    //resampling
    id_as = Rcpp::RcppArmadillo::sample(id_as_lnspc, N, true, w_norm);
    a.col(t) = id_as;

    // propagation
    eval_f = f_cpp(xa1.col(t - 1), phi_x(0), as_scalar(Za_beta.submat(t, 0, t, 0)));
    mean_diff.col(0) = eval_f - x_r(t);
    eval_f = eval_f.elem(id_as);
    xa1.col(t) = eval_f + sqrt(sig_sq_x(0))*arma::randn(N, 1);

    eval_f = f_cpp(xa2.col(t - 1), phi_x(1), as_scalar(Za_beta.submat(t, 1, t, 1)));
    mean_diff.col(1) = eval_f - x_r(TT + t);
    eval_f = eval_f.elem(id_as);
    xa2.col(t) = eval_f + sqrt(sig_sq_x(1))*arma::randn(N, 1);

    eval_f = f_cpp(xa3.col(t - 1), phi_x(2), as_scalar(Za_beta.submat(t, 2, t, 2)));
    mean_diff.col(2) = eval_f - x_r(TT*2 + t);
    eval_f = eval_f.elem(id_as);
    xa3.col(t) = eval_f + sqrt(sig_sq_x(2))*arma::randn(N, 1);

    eval_f = f_cpp(xa4.col(t - 1), phi_x(3), as_scalar(Za_beta.submat(t, 3, t, 3)));
    mean_diff.col(3) = eval_f - x_r(TT*3 + t);
    eval_f = eval_f.elem(id_as);
    xa4.col(t) = eval_f + sqrt(sig_sq_x(3))*arma::randn(N, 1);

    eval_f = f_cpp(xa5.col(t - 1), phi_x(4), as_scalar(Za_beta.submat(t, 4, t, 4)));
    mean_diff.col(4) = eval_f - x_r(TT*4 + t);
    eval_f = eval_f.elem(id_as);
    xa5.col(t) = eval_f + sqrt(sig_sq_x(4))*arma::randn(N, 1);

    eval_f = f_cpp(xa6.col(t - 1), phi_x(5), as_scalar(Za_beta.submat(t, 5, t, 5)));
    mean_diff.col(5) = eval_f - x_r(TT*5 + t);
    eval_f = eval_f.elem(id_as);
    xa6.col(t) = eval_f + sqrt(sig_sq_x(5))*arma::randn(N, 1);

    // conditioning
    xa1(N - 1, t) = x_r(t);
    xa2(N - 1, t) = x_r(TT*1 + t);
    xa3(N - 1, t) = x_r(TT*2 + t);
    xa4(N - 1, t) = x_r(TT*3 + t);
    xa5(N - 1, t) = x_r(TT*4 + t);
    xa6(N - 1, t) = x_r(TT*5 + t);
    // ancestor sampling
    as_weights = w_as_c(mean_diff, vcm_diag, w_log);
    as_draw_vec = Rcpp::RcppArmadillo::sample(id_as_lnspc, 1, true, as_weights);
    as_draw = arma::as_scalar(as_draw_vec);
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
  }
  ind = a.col(TT - 1);
  arma::uvec t_ind;
  for (arma::uword t = TT-2; t >= 1; --t) {
    arma::uvec t_ind = {t};
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

  w_norm = w.col(TT - 1);
  // b_draw_vec = sample(N, 1, true, w_norm2) - 1;
  // b_draw = b_draw_vec(0);
  b_draw_vec = Rcpp::RcppArmadillo::sample(id_as_lnspc, 1, true, w_norm);
  b_draw = arma::as_scalar(b_draw);

  x_out.col(0) = xa1.row(b_draw).t();
  x_out.col(1) = xa2.row(b_draw).t();
  x_out.col(2) = xa3.row(b_draw).t();
  x_out.col(3) = xa4.row(b_draw).t();
  x_out.col(4) = xa5.row(b_draw).t();
  x_out.col(5) = xa6.row(b_draw).t();
  return (x_out);
}

//[[Rcpp::export]]
List pgas2_full_short_rng_arma(const int& N,
                      const int& TT,
                      const int& MM,
                      const mat& y,
                      const vec& num_counts,
                      const mat& Z,
                      const vec& priors,
                      const List& par_init,
                      const vec& traj_init) {
  int D = par_init.size();
  // Initialize result containers:
  vec w(N, fill::zeros);
  mat Xa(TT*D, MM, fill::zeros);

  mat phi_x(D, MM, fill::zeros);
  mat sig_sq_x(D, MM, fill::zeros);
  mat out_cPF(D, TT, fill::zeros);
  // Initialize helper/garbage containers I.
  double err_siq_sq_x;
  vec z_add(TT - 1, fill::zeros);
  vec temp_vec_col;
  rowvec temp_vec_row(TT, fill::zeros);
  // Initialize parameters:
  uvec dim_pars(D);
  for(int d = 0; d < D; ++d) {
    temp_vec_col = as<vec>(par_init(d));
    dim_pars(d) = temp_vec_col.n_rows;
    sig_sq_x(d, 0) = temp_vec_col(0);
    phi_x(d, 0) = temp_vec_col(1);
  }
  double num_pars = sum(dim_pars);
  uvec id_bet(D + 1);
  uvec id_zet(D + 1);
  id_bet(0) = 0;
  id_bet.subvec(1, D) = cumsum(dim_pars - 2);
  id_zet(0) = 0;
  id_zet.subvec(1, D) = cumsum(dim_pars - 1);
  // minus 2 because of minus phi and minus sigma
  mat bet(num_pars - 2*D, MM, fill::zeros);
  for(int d = 0; d < D; ++d) {
    temp_vec_col = as<vec>(par_init(d));
    int testval = temp_vec_col.n_rows - 1;
    bet.submat(id_bet(d), 0, id_bet(d + 1) - 1, 0) = temp_vec_col.subvec(2, testval);
  }
  // Initialize regressor containers:
  temp_vec_col.set_size(TT - 1);
  temp_vec_col.zeros();
  mat Z_mcmc(TT - 1, num_pars -D*3);
  Z_mcmc = join_rows(temp_vec_col,
                     Z.submat(1, id_bet(0), TT - 1, id_bet(0 + 1) - 1),
                     temp_vec_col,
                     Z.submat(1, id_bet(1), TT - 1, id_bet(1 + 1) - 1));
  Z_mcmc = join_rows(Z_mcmc, temp_vec_col,
                     Z.submat(1, id_bet(2), TT - 1, id_bet(2 + 1) - 1),
                     temp_vec_col);
  Z_mcmc = join_rows(Z_mcmc,
                     Z.submat(1, id_bet(3), TT - 1, id_bet(3 + 1) - 1),
                     temp_vec_col,
                     Z.submat(1, id_bet(4), TT - 1, id_bet(4 + 1) - 1));
  Z_mcmc = join_rows(Z_mcmc, temp_vec_col,
                     Z.submat(1, id_bet(5), TT - 1, id_bet(5 + 1) - 1));
  // Initialize priors:
  double prior_a = priors(0) + (TT - 1)/2.0;
  double prior_b = priors(1);
  temp_vec_col = ones(dim_pars(0) - 1)/1000;
  mat prior_V_xa1 = diagmat(temp_vec_col);
  temp_vec_col = ones(dim_pars(1) - 1)/1000;
  mat prior_V_xa2 = diagmat(temp_vec_col);
  temp_vec_col = ones(dim_pars(2) - 1)/1000;
  mat prior_V_xa3 = diagmat(temp_vec_col);
  temp_vec_col = ones(dim_pars(3) - 1)/1000;
  mat prior_V_xa4 = diagmat(temp_vec_col);
  temp_vec_col = ones(dim_pars(4) - 1)/1000;
  mat prior_V_xa5 = diagmat(temp_vec_col);
  temp_vec_col = ones(dim_pars(5) - 1)/1000;
  mat prior_V_xa6 = diagmat(temp_vec_col);
  // Initialize states to deterministic starting values
  double check_state_init_type;
  check_state_init_type = traj_init.n_rows;
  // I. If initialization of trajectory is one (consant) value for all t=1,...,TT per state d
  if (check_state_init_type == D) {
    temp_vec_col.set_size(TT);
    for (int d = 1; d < D+1; ++d) {
      temp_vec_col.fill(traj_init(d-1));
      // temp_vec_col = as<vec>(traj_init(d-1));
      Xa.submat(TT*(d -1), 0, TT*d - 1, 0) = temp_vec_col;
    }
  }
  // II. If initialization of trajectory is one trajectory of length TT per state d
  if (check_state_init_type == D*TT) {
    Xa.col(0) = traj_init;
  }
  // Initialize helper/garbage II
  field<mat> Omega_xa(D, 1);
  mat Omega_xa1(dim_pars(0) -1, dim_pars(0) -1);
  Omega_xa(0, 0) = Omega_xa1;
  mat Omega_xa2(dim_pars(1) -1, dim_pars(1) -1);
  Omega_xa(1, 0) = Omega_xa2;
  mat Omega_xa3(dim_pars(2) -1, dim_pars(2) -1);
  Omega_xa(2, 0) = Omega_xa3;
  mat Omega_xa4(dim_pars(3) -1, dim_pars(3) -1);
  Omega_xa(3, 0) = Omega_xa4;
  mat Omega_xa5(dim_pars(4) -1, dim_pars(4) -1);
  Omega_xa(4, 0) = Omega_xa5;
  mat Omega_xa6(dim_pars(5) -1, dim_pars(5) -1);
  Omega_xa(5, 0) = Omega_xa6;

  field<vec> mu_xa(D, 1);
  vec mu_xa1(dim_pars(0) -1);
  mu_xa(0, 0) = mu_xa1;
  vec mu_xa2(dim_pars(1) -1);
  mu_xa(1, 0) = mu_xa2;
  vec mu_xa3(dim_pars(2) -1);
  mu_xa(2, 0) = mu_xa3;
  vec mu_xa4(dim_pars(3) -1);
  mu_xa(3, 0) = mu_xa4;
  vec mu_xa5(dim_pars(4) -1);
  mu_xa(4, 0) = mu_xa5;
  vec mu_xa6(dim_pars(5) -1);
  mu_xa(5, 0) = mu_xa6;
  // II. run cBPF and use output as first conditioning trajectory
  out_cPF = cbpf_as_c5_full(N, TT,
                            num_counts, y,
                            Z, id_bet,
                            sig_sq_x.col(0),
                            phi_x.col(0),
                            bet.col(0),
                            Xa.col(0));
  Xa.submat(0, 0, TT - 1, 0) = out_cPF.col(0);
  Xa.submat(TT, 0, TT*2 - 1, 0)= out_cPF.col(1);
  Xa.submat(TT*2, 0, TT*3 - 1, 0)= out_cPF.col(2);
  Xa.submat(TT*3, 0, TT*4 - 1, 0)= out_cPF.col(3);
  Xa.submat(TT*4, 0, TT*5 - 1, 0)= out_cPF.col(4);
  Xa.submat(TT*5, 0, TT*6 - 1, 0)= out_cPF.col(5);
  // // Run MCMC loop
  temp_vec_col.set_size(TT - 1);
  vec temp_vec_col2(TT - 1, fill::zeros);
  for (int m = 1; m < MM; ++m) {
    // I. Run GIBBS part
    Z_mcmc.col(id_zet(0)) = (Xa.submat(0, m-1, TT - 2, m-1));
    Z_mcmc.col(id_zet(1)) = (Xa.submat(TT, m-1, TT*2 - 2, m-1));
    Z_mcmc.col(id_zet(2)) = (Xa.submat(TT*2, m-1, TT*3 - 2, m-1));
    Z_mcmc.col(id_zet(3)) = (Xa.submat(TT*3, m-1, TT*4 - 2, m-1));
    Z_mcmc.col(id_zet(4)) = (Xa.submat(TT*4, m-1, TT*5 - 2, m-1));
    Z_mcmc.col(id_zet(5)) = (Xa.submat(TT*5, m-1, TT*6 - 2, m-1));
    for(int d = 0; d<D; ++d) {
      temp_vec_col = Xa.submat(TT*d + 1, m - 1, TT*(d + 1) - 1, m - 1);
      z_add =  Z_mcmc.submat(0, id_zet(d) + 1, TT - 2, id_zet(d + 1) - 1) * bet.submat(id_bet(d),  m - 1, id_bet(d + 1) - 1,  m - 1);
      temp_vec_col2 = temp_vec_col - f_cpp_vech(Xa.submat(TT*d, m - 1, TT*(d + 1) - 2, m - 1),
                                                phi_x(d, m - 1),
                                                z_add);
      err_siq_sq_x = (dot(temp_vec_col2, temp_vec_col2)) * 0.5;
      // sig_sq_x(d, m)  = 1/(R::rgamma(prior_a, 1.0/(prior_b + err_siq_sq_x)));
      sig_sq_x(d, m)  = 1/randg<double>(distr_param(prior_a, 1.0/(prior_b + err_siq_sq_x)));;
      Omega_xa(d, 0)  = inv((trans(Z_mcmc.cols(id_zet(d), id_zet(d + 1) - 1)) * Z_mcmc.cols(id_zet(d), id_zet(d + 1) - 1))/sig_sq_x(d, m) + prior_V_xa1);
      mu_xa(d, 0) = Omega_xa(d, 0) * (trans(Z_mcmc.cols(id_zet(d), id_zet(d + 1) - 1)) * temp_vec_col)/sig_sq_x(d, m);
      // // mu_xa(d, 0) = mvrnorm_c(mu_xa(d, 0), Omega_xa(d, 0));
      mu_xa(d, 0) = mvnrnd(mu_xa(d, 0), Omega_xa(d, 0));
      phi_x(d, m) = (mu_xa(d, 0))(0);
      bet.submat(id_bet(d), m, id_bet(d + 1) - 1, m) =  (mu_xa(d, 0)).subvec(1, (dim_pars(d) - 2));
    }
  // //   // double digits = 1000;
  // //   // sig_sq_x(2, m) = round(sig_sq_x(2, m)*digits)/digits;
  // //   // phi_x(2, m) = round(phi_x(2, m)*digits)/digits;
  // //   // bet.submat(id_bet(2), m, id_bet(2 + 1) - 1, m).transform([](double val){return(round(val*1000)/1000);});
  // //   // ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // //   // bet.submat(id_bet(0), m, id_bet(0 + 1) - 1, m).transform([](double val){return(round(val*1000)/1000);});
  // //   // Omega_xa(d, 0).transform([](double val){return(round(val*1000)/1000);});
  // //   // mu_xa(d, 0).transform([](double val){return(round(val*1000)/1000);});
  // //   // ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    out_cPF = cbpf_as_c5_full(N, TT,
                              num_counts, y,
                              Z, id_bet,
                              sig_sq_x.col(m),
                              phi_x.col(m),
                              bet.col(m),
                              Xa.col(m - 1));
    Xa.submat(0, m, TT - 1, m) = out_cPF.col(0);
    Xa.submat(TT, m, TT*2 - 1, m)= out_cPF.col(1);
    Xa.submat(TT*2, m, TT*3 - 1, m)= out_cPF.col(2);
    Xa.submat(TT*3, m, TT*4 - 1, m)= out_cPF.col(3);
    Xa.submat(TT*4, m, TT*5 - 1, m)= out_cPF.col(4);
    Xa.submat(TT*5, m, TT*6 - 1, m)= out_cPF.col(5);
  //
    Rprintf("Iteration number: %u \n", m);
  }
  List all_traj(D);
  for (int d = 0; d<D; ++d){
    all_traj(d) = Xa.submat(TT*d, 0, TT*(d + 1) - 1, MM - 1);
  }
  return(List::create(
  Rcpp::Named("sigma_sq_xa1") = sig_sq_x.row(0),
  Rcpp::Named("phi_xa1") = phi_x.row(0),
  Rcpp::Named("bet_xa1") = bet.submat(id_bet(0), 0, id_bet(0 + 1) - 1, MM - 1),
  Rcpp::Named("sigma_sq_xa2") = sig_sq_x.row(1),
  Rcpp::Named("phi_xa2") = phi_x.row(1),
  Rcpp::Named("bet_xa2") = bet.submat(id_bet(1), 0, id_bet(1 + 1) - 1, MM - 1),
  Rcpp::Named("sigma_sq_xa3") = sig_sq_x.row(2),
  Rcpp::Named("phi_xa3") = phi_x.row(2),
  Rcpp::Named("bet_xa3") = bet.submat(id_bet(2), 0, id_bet(2 + 1) - 1, MM - 1),
  Rcpp::Named("sigma_sq_xa4") = sig_sq_x.row(3),
  Rcpp::Named("phi_xa4") = phi_x.row(3),
  Rcpp::Named("bet_xa4") = bet.submat(id_bet(3), 0, id_bet(3 + 1) - 1, MM - 1),
  Rcpp::Named("sigma_sq_xa5") = sig_sq_x.row(4),
  Rcpp::Named("phi_xa5") = phi_x.row(4),
  Rcpp::Named("bet_xa5") = bet.submat(id_bet(4), 0, id_bet(4 + 1) - 1, MM - 1),
  Rcpp::Named("sigma_sq_xa6") = sig_sq_x.row(5),
  Rcpp::Named("phi_xa6") = phi_x.row(5),
  Rcpp::Named("bet_xa6") = bet.submat(id_bet(5), 0, id_bet(5 + 1) - 1, MM - 1),
  Rcpp::Named("xtraj")  = all_traj));
  // double d = 0;
  // double m = 1;
  // return(List::create(mu_xa(d, 0), (mu_xa(d, 0))(0), bet.submat(id_bet(d), m, id_bet(d + 1) - 1, m), mu_xa, dim_pars, id_bet));
  // (mu_xa(d, 0)).subvec(1, (D - 1)),
  // bet.submat(id_bet(d), m, id_bet(d + 1) - 1, m),
  // return(List::create(Za1, Za2, Za3, Za4, Za5, Za6));
}
