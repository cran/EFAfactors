#include <RcppArmadillo.h>
#include <algorithm>
#include <cmath>
#include <vector>
#include <random>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat calculateRMSE(arma::mat Pop, arma::vec EigsData, int NSamples, int NPop, int N) {

  arma::mat RMSR_Eigs(NSamples, 1);
  for (int j = 0; j < NSamples; j++) {
    arma::uvec indices = arma::randi<arma::uvec>(N, arma::distr_param(0, NPop - 1));
    arma::mat Samp = Pop.rows(indices);
    arma::mat cor_Samp = cor(Samp);
    arma::vec Eigs_Samp = eig_sym(cor_Samp);
    Eigs_Samp = sort(Eigs_Samp, "descend");
    double sum_squared_diff = sum(square(Eigs_Samp - EigsData));

    RMSR_Eigs(j, 0) = std::sqrt(sum_squared_diff);
  }

  return RMSR_Eigs;
}
