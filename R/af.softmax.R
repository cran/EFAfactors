#'
#' An Activation Function: Softmax
#'
#' @description
#' This function computes the softmax of a numeric vector. The softmax function
#' transforms a vector of real values into a probability distribution, where each element
#' is between 0 and 1 and the sum of all elements is 1. @seealso \link[EFAfactors]{DNN_predictor}
#'
#' @param x A numeric vector for which the softmax transformation is to be computed.
#'
#' @details
#' The softmax function is calculated as:
#' \deqn{softmax(x_i) = \frac{exp(x_i)}{\sum_{j} exp(x_j)}}
#' In the case of overflow (i.e., when \code{exp(x_i)} is too large), this function handles
#' \code{Inf} values by assigning \code{1} to the corresponding positions and \code{0} to the
#' others before Softmax. @seealso \link[EFAfactors]{DNN_predictor}
#'
#' @return A numeric vector representing the softmax-transformed values of \code{x}.
#'
#' @examples
#' x <- c(1, 2, 3)
#' af.softmax(x)
#'
#' @export
af.softmax <- function(x) {
  exp_x <- exp(x)
  if(any(exp_x == Inf)){
    exp_x[which(exp_x != Inf)] <- 0
    exp_x[which(exp_x == Inf)] <- 1
  }
  return(exp_x / sum(exp_x))
}
