#include <RcppArmadillo.h>
#include <algorithm>
#include <cmath>
#include <vector>
#include <random>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

std::tuple<arma::vec, arma::mat> eigen_decomposition(const arma::mat& Cor_Matrix) {
  arma::vec eigval;
  arma::mat eigvec;
  eig_sym(eigval, eigvec, Cor_Matrix);
  return std::make_tuple(reverse(eigval), reverse(eigvec, 1));
}

// [[Rcpp::export]]
List PAFCpp(arma::mat Cor_Matrix, int N_Factors, int Max_Iter = 1000, double Criterion = 0.0001) {

  int k = Cor_Matrix.n_cols;

  arma::vec Old_H2(k);
  Old_H2.fill(0);

  arma::vec H2(k);
  H2.fill(0);

  double Change = 1.0;
  int Iter = 0;
  arma::mat Factor_Loadings(k, N_Factors);
  Factor_Loadings.fill(0);
  arma::vec Eig_Values;
  arma::mat Eig_Vectors;

  while ((Change >= Criterion) && (Iter < Max_Iter)) {
    Iter += 1;

    // eigen
    std::tie(Eig_Values, Eig_Vectors) = eigen_decomposition(Cor_Matrix);

    // calculate L & Factor Loadings
    arma::vec L = sqrt(Eig_Values.head(N_Factors));
    for (int i = 0; i < N_Factors; i++) {
      Factor_Loadings.col(i) = Eig_Vectors.col(i) * L(i);
    }

    // updata H2
    for (int i = 0; i < k; i++) {
      H2(i) = sum(Factor_Loadings.row(i) % Factor_Loadings.row(i));
    }

    // chekc if converge
    Change = sum(abs(Old_H2 - H2));
    Old_H2 = H2;
    Cor_Matrix.diag() = H2;
  }

  return List::create(
    Named("eigen") = Eig_Values,
    Named("loadings") = Factor_Loadings,
    Named("iterations") = Iter,
    Named("H2") = H2
  );
}
