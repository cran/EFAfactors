
#'
#' @import Rcpp
#' @import RcppArmadillo
#' @useDynLib EFAfactors, .registration = TRUE
#'
cppFunction('arma::mat add_matrices(const arma::mat& A, const arma::mat& B) {
  return A + B;
}', depends = "RcppArmadillo")

