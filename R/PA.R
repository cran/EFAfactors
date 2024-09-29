#' Parallel Analysis
#'
#' @description
#' This function performs Parallel Analysis (PA), which is a method used to determine the number of
#' factors to retain in exploratory factor analysis. It compares the empirical eigenvalues with those obtained
#' from simulated random data to identify the point where the observed eigenvalues are larger than those expected by chance.
#' The number of empirical eigenvalues that are greater than the corresponding reference eigenvalues is the number
#' of factors recommended to be retained by the PA method.
#'
#' @param response A required \code{N} × \code{I} matrix or data.frame consisting of the responses
#'        of \code{N} individuals to \code{I} items.
#' @param fa A string that determines the method used to obtain eigenvalues in PA. If 'pc', it represents
#'           Principal Component Analysis (PCA); if 'fa', it represents Principal Axis Factoring (a widely
#'           used Factor Analysis method; @seealso \code{\link[EFAfactors]{factor.analysis}};
#'           Auerswald & Moshagen, 2019). (Default = 'pc')
#' @param nfact A numeric value that specifies the number of factors to extract, only effective when \code{fa = 'fa'}. (Default = 1)
#' @param n.iter A numeric value that determines the number of simulations for the random data. (Default = 100)
#' @param type A string that determines the method used to calculate the reference eigenvalues from the simulated data.
#'        If 'mean', the reference eigenvalue (\code{eigen.ref}) is the mean of the simulated eigenvalues (\code{eigen.sim});
#'        if 'quant', the reference eigenvalue is the \code{quant} percentile of \code{eigen.sim}. (Default = 'quant')
#' @param quant A numeric value between 0 and 1, representing the quantile to be used for the reference
#'              eigenvalues calculation when \code{type = 'quant'}. (Default = 0.95)
#' @param cor.type A character string indicating the correlation coefficient (or covariance) to
#'                 be computed. One of "pearson" (default), "kendall", or "spearman". @seealso \link[stats]{cor}.
#' @param use An optional character string specifying the method for computing covariances when
#'            there are missing values. This must be one of "everything", "all.obs", "complete.obs",
#'            "na.or.complete", or "pairwise.complete.obs" (default). @seealso \link[stats]{cor}.
#' @param vis A Boolean that determines whether to print the factor retention results. Set to \code{TRUE}
#'            to print, or \code{FALSE} to suppress output. (Default = TRUE)
#' @param plot A Boolean that determines whether to display the PA plot. Set to \code{TRUE} to show the plot,
#'              or \code{FALSE} to suppress it. @seealso \link[EFAfactors]{plot.PA}. (Default = TRUE)
#'
#' @details
#' This function performs Parallel Analysis (PA; Horn, 1965; Auerswald & Moshagen, 2019) to determine the number of factors to retain.
#' PA is a widely used method and is considered the "gold standard" for factor retention due to its high accuracy and stability,
#' although it may underperform compared to methods like CD or EKC under certain conditions.
#' The core idea of PA is to simulate random data multiple times, for example, 100 times, and compute the eigenvalues from each simulation.
#' These simulated eigenvalues are then processed using either the mean or a quantile method to obtain the reference eigenvalues,
#' such as the i-th reference eigenvalue \eqn{\lambda_{i,ref}}.
#' The relationship between the i-th empirical eigenvalue \eqn{\lambda_{i}} and \eqn{\lambda_{i,ref}} indicates whether the i-th factor should be retained.
#' If \eqn{\lambda_{i} > \lambda_{i,ref}}, it suggests that the explanatory power of the i-th factor from the original data is stronger than that of the i-th factor from the random data,
#' and therefore the factor should be retained. Conversely, if \eqn{\lambda_{i} <= \lambda_{i,ref}},
#' it indicates that the explanatory power of the i-th factor from the original data is weaker or equal to that of the random data,
#' making it indistinguishable from noise, and thus the factor should not be retained. So,
#'
#' \deqn{F = \sum_{i=1}^{I} I(\lambda_i > \lambda_{i,ref})}
#'
#' Here, \( F \) represents the number of factors determined by the EKC, and \eqn{I(\cdot)} is the
#' indicator function, which equals 1 when the condition is true, and 0 otherwise.
#'
#' Auerswald & Moshagen (2019) found that the most accurate results for PA were obtained when
#' using PCA to extract eigenvalues and using the 95th percentile of the simulated
#' eigenvalues to calculate the reference eigenvalues. Therefore,
#' the recommended settings for this function are \code{fa = 'pc'}, \code{type = 'quant'}, and \code{quant = 0.95}.
#'
#' @return An object of class \code{PA}, which is a \code{list} containing the following components:
#' \item{nfact}{The number of factors to retain.}
#' \item{fa}{Indicates the method used to obtain eigenvalues in PA. 'pc' represents Principal Component Analysis, and 'fa' represents Principal Axis Factoring.}
#' \item{type}{Indicates the method used to calculate \code{eigen.ref}. If 'mean', \code{eigen.ref} is the mean of \code{eigen.sim}; if 'quant', \code{eigen.ref} is the \code{quant} percentile of \code{eigen.sim}.}
#' \item{eigen.value}{A vector containing the empirical eigenvalues.}
#' \item{eigen.ref}{A vector containing the reference eigenvalues, which depend on \code{type}.}
#' \item{eigen.sim}{A matrix containing the simulated eigenvalues for all iterations.}
#'
#' @references
#' Auerswald, M., & Moshagen, M. (2019). How to determine the number of factors to retain in exploratory factor analysis: A comparison of extraction methods under realistic conditions. Psychological methods, 24(4), 468-491. https://doi.org/10.1037/met0000200.
#'
#' Horn, J. L. (1965). A rationale and test for the number of factors in factor analysis. Psychometrika, 30, 179–185. http://dx.doi.org/10.1007/BF02289447.
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
#' ## Run PA function with default parameters.
#' \donttest{
#'  PA.obj <- PA(response)
#'
#'  print(PA.obj)
#'
#'  plot(PA.obj)
#'
#'  ## Get the eigen.value, eigen.ref and  nfact results.
#'  eigen.value <- PA.obj$eigen.value
#'  eigen.ref <- PA.obj$eigen.ref
#'  nfact <- PA.obj$nfact
#'
#'  print(eigen.value)
#'  print(eigen.ref)
#'  print(nfact)
#'
#' }
#'
#' @importFrom stats cor quantile
#'
#' @export
PA <- function(response, fa="pc", n.iter=100, type = "quant", nfact=1, quant=0.95,
               cor.type = "pearson", use = "pairwise.complete.obs",
               vis=TRUE, plot=TRUE) {

  if(!any(rep(type, 2) == c("quant", "mean")))
    stop("'type' must be one of the strings 'quant' or 'mean' !")

  if(!any(rep(fa, 2) == c("pc", "fa")))
    stop("'type' must be one of the strings 'fa' or 'pc' !")

  if(!any(rep(use, 5) == c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs")))
    stop("'use' must be one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete', or 'pairwise.complete.obs' !")

  N <- dim(response)[1]
  I <- dim(response)[2]
  response <- scale(response)
  cor.response <- cor(response, method = cor.type, use = use)

  eigen.value.pc <- eigen(cor.response)$values
  eigen.value.fa <- factor.analysis(response, nfact = nfact)$eigen.value
  if(fa == "pc"){
    eigen.value <- eigen.value.pc
  }else if(fa == "fa"){
    eigen.value <- eigen.value.fa
  }

  eigen.sim <- matrix(0, nrow=n.iter, ncol=I)

  for (it in 1:n.iter) {
    response.temp <- sampleResponse(response, N, I)
    if(fa == "pc"){
      cor.y <- cor(response.temp, method = cor.type, use = use)
      eigen.sim[it,] <- eigen(cor.y)$values
    }else{
      eigen.sim[it,] <- factor.analysis(response.temp, nfact=nfact, cor.type=cor.type, use = use)$eigen.value
    }
  }

  eigen.quant <- apply(eigen.sim, 2, quantile, probs=quant)
  eigen.mean <- colMeans(eigen.sim)

  if(type == "quant"){
    nfact <- which(eigen.value <= eigen.quant)[1] - 1
    eigen.ref <- eigen.quant

    PA.obj <- list(nfact=nfact, fa=fa, type=type, eigen.value=eigen.value,
                   eigen.ref=eigen.ref, quant=quant, eigen.sim=eigen.sim)
  }else if(type == "mean"){
    nfact <- which(eigen.value <= eigen.mean)[1] - 1
    eigen.ref <- eigen.mean
    PA.obj <- list(nfact=nfact, fa=fa, type=type, eigen.value=eigen.value,
                   eigen.ref=eigen.ref, eigen.sim=eigen.sim)
  }

  class(PA.obj) <- "PA"
  if(vis) print(PA.obj)
  if(plot) plot(PA.obj)

  return(PA.obj)
}
