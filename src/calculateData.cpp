#include <RcppArmadillo.h>
#include <algorithm>
#include <cmath>
#include <vector>
#include <random>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
NumericMatrix calculateData(int k,
                            const NumericMatrix& Shared_Comp,
                            const NumericMatrix& Shared_Load,
                            const NumericMatrix& Unique_Comp,
                            const NumericMatrix& Unique_Load) {

  arma::mat Data(Shared_Comp.nrow(), k, arma::fill::zeros);
  arma::mat sharedComp = as<arma::mat>(Shared_Comp);
  arma::mat sharedLoad = as<arma::mat>(Shared_Load);
  arma::mat uniqueComp = as<arma::mat>(Unique_Comp);
  arma::mat uniqueLoad = as<arma::mat>(Unique_Load);

  // Compute Shared.Comp %*% t(Shared.Load)
  arma::mat sharedCompLoad = sharedComp * sharedLoad.t();

  // Perform the operation column-wise
  for (int i = 0; i < k; ++i) {
    // Convert uniqueLoad(i, 0) to a column vector with the same size as uniqueComp.col(i)
    arma::vec uniqueLoadVec = arma::vec(uniqueComp.n_rows, arma::fill::value(uniqueLoad(i, 0)));

    // Perform element-wise multiplication
    Data.col(i) = sharedCompLoad.col(i) + uniqueComp.col(i) % uniqueLoadVec;
  }

  // Convert arma::mat back to Rcpp::NumericMatrix
  NumericMatrix resultMat = wrap(Data);

  return resultMat;
}

