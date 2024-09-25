#include <RcppArmadillo.h>
#include <algorithm>
#include <cmath>
#include <vector>
#include <random>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
NumericMatrix generateDistributions(const NumericMatrix& suppliedData, int I, int Npop) {
  NumericMatrix distributions(Npop, I);

  std::random_device rd;
  std::mt19937 gen(rd());

  for (int i = 0; i < I; ++i) {
    std::vector<double> tempVec;

    for (int j = 0; j < suppliedData.nrow(); ++j) {
      if (!NumericVector::is_na(suppliedData(j, i))) {
        tempVec.push_back(suppliedData(j, i));
      }
    }

    std::uniform_int_distribution<> dis(0, tempVec.size() - 1);

    for (int j = 0; j < Npop; ++j) {
      distributions(j, i) = tempVec[dis(gen)];
    }

    std::sort(distributions.begin() + i * Npop, distributions.begin() + (i + 1) * Npop);
  }

  return distributions;
}

