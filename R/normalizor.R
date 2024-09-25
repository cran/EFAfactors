#'
#' Feature Normalization
#'
#' @description
#' This function normalizes a matrix of features using precomputed means and standard deviations.
#' The function automatically runs \link[EFAfactors]{load_scaler} to read the standard deviations and means of the features,
#' which are organized into a \code{list} object named \code{scaler}. These means and standard deviations are computed from
#' the 10,000,000 datasets \code{\link[EFAfactors]{data.datasets}} for training the Pre-Trained Deep Neural Network (DNN).
#'
#' @param features A numeric matrix where each row represents an observation and each column represents a feature.
#'
#' @details
#' The function applies z-score normalization to each element in the \code{features} matrix. It uses
#' the \code{scaler} object, which is expected to contain precomputed means and standard deviations for each feature.
#' The normalized value for each element is computed as:
#' \deqn{z = \frac{x - \mu}{\sigma}}
#' where \eqn{x} is the original value, \eqn{\mu} is the mean, and \eqn{\sigma} is the standard deviation.
#' @seealso \link[EFAfactors]{DNN_predictor}, \link[EFAfactors]{load_scaler}, \link[EFAfactors]{data.datasets}, \link[EFAfactors]{data.scaler}
#'
#' @return A matrix of the same dimensions as \code{features}, where each feature has been normalized.
#'
#' @export
normalizor <- function(features){

  data.scaler <- load_scaler()

  means <- matrix(data.scaler$means, nrow = nrow(features), ncol = ncol(features), byrow=TRUE)

  sds <- matrix(data.scaler$sds, nrow = nrow(features), ncol = ncol(features), byrow=TRUE)

  normalized_features <- (features - means) / sds

  return(normalized_features)
}
