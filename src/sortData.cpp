#include <RcppArmadillo.h>
#include <algorithm>
#include <cmath>
#include <vector>
#include <random>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::NumericMatrix sortData(NumericMatrix Data, NumericMatrix Distributions) {

  int k = Data.ncol();

  int nrows = Data.nrow();
  NumericMatrix sortedData(nrows, k);
  for (int i = 0; i < k; ++i) {
    NumericVector colData = Data(_, i);

    std::vector<int> indices(nrows);
    std::iota(indices.begin(), indices.end(), 0);
    std::sort(indices.begin(), indices.end(), [&colData](int a, int b) {
      return colData[a] < colData[b];
    });
    for (int j = 0; j < nrows; ++j) {
      sortedData(j, i) = Data(indices[j], i);
    }
    for (int j = 0; j < nrows; ++j) {
      sortedData(j, i) = Distributions(j, i);
    }
  }

  return Data;
}

