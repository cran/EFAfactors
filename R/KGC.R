
#'
#' Kaiser-Guttman Criterion
#'
#' @description
#' This function implements the Kaiser-Guttman criterion (Guttman, 1954; Kaiser, 1960) for determining the number of factors to retain in factor analysis.
#' It is based on the eigenvalues of the correlation matrix of the responses. According to the criterion, factors are retained
#' if their corresponding eigenvalues are greater than 1.
#'
#' @param response A required \code{N} × \code{I} matrix or data.frame consisting of the responses of \code{N} individuals
#'          to \code{I} items.
#' @param fa A string that determines the method used to obtain eigenvalues. If 'pc', it represents
#'           Principal Component Analysis (PCA); if 'fa', it represents Principal Axis Factoring (a widely
#'           used Factor Analysis method; @seealso \code{\link[EFAfactors]{factor.analysis}};
#'           Auerswald & Moshagen, 2019). (Default = 'pc')
#' @param nfact A numeric value that specifies the number of factors to extract, only effective when \code{fa = 'fa'}. (Default = 1)
#' @param cor.type A character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default),
#'          "kendall", or "spearman". @seealso \link[stats]{cor}.
#' @param use An optional character string giving a method for computing covariances in the presence of missing values. This
#'          must be one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs" (default).
#'          @seealso \link[stats]{cor}.
#' @param vis A Boolean variable that will print the factor retention results when set to TRUE, and will not print
#'          when set to FALSE. (default = TRUE)
#' @param plot A Boolean variable that will print the KGC plot when set to TRUE, and will not print it when set to
#'          FALSE. @seealso \link[EFAfactors]{plot.KGC}. (Default = TRUE)
#'
#'
#' @return An object of class \code{KGC} is a \code{list} containing the following components:
#' \item{nfact}{The number of factors to be retained.}
#' \item{eigen.value}{A vector containing the empirical eigenvalues}
#'
#' @references
#' Guttman, L. (1954). Some necessary conditions for common-factor analysis. Psychometrika, 19, 149–161. http://dx.doi.org/10.1007/BF02289162.
#'
#' Kaiser, H. F. (1960). The application of electronic computers to factor analysis. Educational and Psychological Measurement, 20, 141–151. http://dx.doi.org/10.1177/001316446002000116.
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
#' ## Run KGC function with default parameters.
#' \donttest{
#' KGC.obj <- KGC(response)
#'
#' print(KGC.obj)
#'
#' plot(KGC.obj)
#'
#' ## Get the eigen.value, eigen.ref and  nfact results.
#' eigen.value <- KGC.obj$eigen.value
#' nfact <- KGC.obj$nfact
#'
#' print(eigen.value)
#' print(nfact)
#'
#' }
#'
#'
#' @importFrom stats cor
#'
#' @export
#'
#'
KGC <- function(response, fa = "pc", nfact = 1, cor.type = "pearson", use = "pairwise.complete.obs",
                vis=TRUE, plot=TRUE){

  if(!any(rep(use, 5) == c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs")))
    stop("'use' must be one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete', or 'pairwise.complete.obs' !")

  if(!any(rep(fa, 2) == c("pc", "fa")))
    stop("'type' must be one of the strings 'fa' or 'pc' !")

  N <- dim(response)[1]
  I <- dim(response)[2]
  response <- scale(response)

  if(fa == "pc"){
    cor.response <- cor(response, method = cor.type, use = use)
    eigen.result <- eigen(cor.response)
    eigen.value <- eigen.result$values
  }else{
    eigen.value <- as.vector(factor.analysis(response, nfact=nfact, cor.type=cor.type, use = use)$eigen.value)
  }

  nfact <- which(eigen.value < 1)[1] - 1

  KGC.obj <- list(nfact=nfact, eigen.value=eigen.value)
  class(KGC.obj) <- "KGC"

  if(plot) plot(KGC.obj)
  if(vis) print(KGC.obj)

  return(KGC.obj)
}
