#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix sampleResponse(const NumericMatrix& response, int N, int I) {

  NumericMatrix response_temp(N, I);
  for (int j = 0; j < I; j++) {
    NumericVector column = response(_, j);
    NumericVector sampled_column = sample(column, N, true);
    for (int i = 0; i < N; i++) {
      response_temp(i, j) = sampled_column[i];
    }
  }

  return response_temp;
}
