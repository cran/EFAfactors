

#'
#' Scree Plot
#'
#' @description
#' This function generates a scree plot to display the eigenvalues of the correlation matrix
#' computed from the given response data. The scree plot helps in determining the number of
#' factors to retain in exploratory factor analysis by examining the point at which the
#' eigenvalues start to level off, indicating less variance explained by additional factors.
#'
#'
#' @param response A required \code{N} × \code{I} matrix or data.frame consisting of the responses of \code{N} individuals
#'          to \code{I} items.
#' @param cor.type A character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default),
#'          "kendall", or "spearman". @seealso \link[stats]{cor}.
#' @param use An optional character string giving a method for computing covariances in the presence of missing values. This
#'          must be one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs" (default).
#'          @seealso \link[stats]{cor}.
#'
#'
#' @return An object of class \code{EFAscreet} is a \code{list} containing the following components:
#' \item{eigen.value}{A vector containing the empirical eigenvalues}
#'
#' @seealso \link[EFAfactors]{plot.EFAscreet}
#'
#'
#'
#' @examples
#' library(EFAfactors)
#' set.seed(123)
#'
#' ##Take the data.bfi dataset as an example.
#' data(data.bfi)
#'
#' response <- as.matrix(data.bfi[, 1:25]) ## loading data
#' response <- na.omit(response) ## Remove samples with NA/missing values
#'
#' ## Transform the scores of reverse-scored items to normal scoring
#' response[, c(1, 9, 10, 11, 12, 22, 25)] <- 6 - response[, c(1, 9, 10, 11, 12, 22, 25)] + 1
#'
#'
#' ## Run EFAscreet function with default parameters.
#' \donttest{
#'  EFAscreet.obj <- EFAscreet(response)
#'
#'  plot(EFAscreet.obj)
#'
#' }
#'
#'
#' @importFrom stats cor
#'
#' @export
EFAscreet <- function(response, cor.type = "pearson", use = "pairwise.complete.obs"){

  if(!any(rep(use, 5) == c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs")))
    stop("'use' must be one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete', or 'pairwise.complete.obs' !")

  N <- dim(response)[1]
  I <- dim(response)[2]
  response <- scale(response)

  cor.response <- cor(response, method = cor.type, use = use)

  eigen.result <- eigen(cor.response)
  eigen.value <- eigen.result$values

  nfact <- which(eigen.value < 1)[1] - 1

  EFAscreet.obj <- list(eigen.value=eigen.value)
  class(EFAscreet.obj) <- "EFAscreet"

  plot(EFAscreet.obj)

  return(EFAscreet.obj)
}
