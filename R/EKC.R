#'
#' Empirical Kaiser Criterion
#'
#' @description
#' This function will apply the Empirical Kaiser Criterion (Braeken & van Assen, 2017) method to
#' determine the number of factors. The method assumes that the distribution of eigenvalues
#' asymptotically follows a Marcenko-Pastur distribution (Marcenko & Pastur, 1967). It calculates
#' the reference eigenvalues based on this distribution and determines whether to retain a factor
#' by comparing the size of the empirical eigenvalues to the reference eigenvalues.
#'
#' @param response A required \code{N} × \code{I} matrix or data.frame consisting of the responses of \code{N} individuals
#'          to \code{I} items.
#' @param cor.type A character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default),
#'          "kendall", or "spearman". @seealso \link[stats]{cor}.
#' @param use An optional character string giving a method for computing covariances in the presence of missing values. This
#'          must be one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs" (default).
#'          @seealso \link[stats]{cor}.
#' @param vis A Boolean variable that will print the factor retention results when set to TRUE, and will not print
#'          when set to FALSE. (default = TRUE)
#' @param plot A Boolean variable that will print the EKC plot when set to TRUE, and will not print it when set to
#'          FALSE. @seealso \link[EFAfactors]{plot.EKC}. (Default = TRUE)
#'
#' @return An object of class \code{EKC} is a \code{list} containing the following components:
#' \item{nfact}{The number of factors to be retained.}
#' \item{eigen.value}{A vector containing the empirical eigenvalues}
#' \item{eigen.ref}{A vector containing the reference eigenvalues}
#'
#' @details
#' The Empirical Kaiser Criterion (EKC; Auerswald & Moshagen, 2019; Braeken & van Assen, 2017)
#' refines Kaiser-Guttman Criterion
#' by accounting for random sample variations in eigenvalues. At the population level, the EKC is
#' equivalent to the original Kaiser-Guttman Criterion, extracting all factors whose eigenvalues
#' from the correlation matrix are greater than one. However, at the sample level, it adjusts for
#' the distribution of eigenvalues in normally distributed data. Under the null model, the eigenvalue
#' distribution follows the Marčenko-Pastur distribution (Marčenko & Pastur, 1967) asymptotically.
#' The upper bound of this distribution serves as the reference eigenvalue for the first eigenvalue \eqn{\lambda}, so
#'
#' \deqn{\lambda_{1,ref} = \left( 1 + \sqrt{\frac{I}{N}} \right)^2}
#'
#' , which is determined by N individuals and I items. For subsequent eigenvalues, adjustments are
#' made based on the variance explained by previous factors. The j-th reference eigenvalue is:
#'
#' \deqn{\lambda_{j,ref} = \max \left[ \frac{I - \sum_{i=0}^{j-1} \lambda_i}{I - j + 1} \left( 1 + \sqrt{\frac{I}{N}} \right)^2, 1 \right]}
#'
#' The j-th reference eigenvalue is reduced according to the magnitude of earlier eigenvalues
#' since higher previous values mean less unexplained variance remains. As in the original
#' Kaiser-Guttman Criterion, the reference eigenvalue cannot drop below one.
#'
#' \deqn{F = \sum_{i=1}^{I} I(\lambda_i > \lambda_{i,ref})}
#'
#' Here, \( F \) represents the number of factors determined by the EKC, and \eqn{I(\cdot)} is the
#' indicator function, which equals 1 when the condition is true, and 0 otherwise.
#'
#' @references
#' Auerswald, M., & Moshagen, M. (2019). How to determine the number of factors to retain in exploratory factor analysis: A comparison of extraction methods under realistic conditions. Psychological methods, 24(4), 468-491. https://doi.org/10.1037/met0000200.
#'
#' Braeken, J., & van Assen, M. A. L. M. (2017). An empirical Kaiser criterion. Psychological methods, 22(3), 450-466. https://doi.org/10.1037/met0000074.
#'
#' Marcˇenko, V. A., & Pastur, L. A. (1967). Distribution of eigenvalues for some sets of random matrices. Mathematics of the USSR-Sbornik, 1, 457–483. http://dx.doi.org/10.1070/SM1967v001n04ABEH001994
#'
#'
#' @author Haijiang Qin <Haijiang133@outlook.com>
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
#' ## Run EKC function with default parameters.
#' \donttest{
#' EKC.obj <- EKC(response)
#'
#' print(EKC.obj)
#'
#' plot(EKC.obj)
#'
#' ## Get the eigen.value, eigen.ref and  nfact results.
#' eigen.value <- EKC.obj$eigen.value
#' eigen.ref <- EKC.obj$eigen.ref
#' nfact <- EKC.obj$nfact
#'
#' print(eigen.value)
#' print(eigen.ref)
#' print(nfact)
#'
#' }
#'
#'
#' @importFrom stats cor
#'
#' @export
#'
EKC <- function(response, cor.type = "pearson", use = "pairwise.complete.obs",
                vis=TRUE, plot = TRUE){

  if(!any(rep(use, 5) == c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs")))
    stop("'use' must be one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete', or 'pairwise.complete.obs' !")

  N <- dim(response)[1]
  I <- dim(response)[2]
  response <- scale(response)

  cor.response <- cor(response, method = cor.type, use = use)

  eigen.result <- eigen(cor.response)
  eigen.value <- eigen.result$values

  eigen.ref <- eigen.value
  eigen.ref[1] <- (1+sqrt(I / N))^2

  for(j in 2:length(eigen.ref))
    eigen.ref[j] <- max(((I-sum(eigen.value[1:(j-1)])) / (I-j+1)) * eigen.ref[1], 1)
  nfact <- length(which(eigen.value > eigen.ref))

  EKC.obj <- list(nfact=nfact, eigen.value=eigen.value, eigen.ref=eigen.ref)
  class(EKC.obj) <- "EKC"

  if(vis) print(EKC.obj)
  if(plot) plot(EKC.obj)

  return(EKC.obj)
}
