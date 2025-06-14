
#'
#' Extracting features According to Goretzko & Buhner (2020)
#'
#' @description
#' This function will extract 181 features from the data according to the method by Goretzko & Buhner (2020).
#'
#' @param response A required \code{N} × \code{I} matrix or data.frame consisting of the responses of \code{N} individuals
#'          to \code{I} items.
#' @param cor.type A character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default),
#'          "kendall", or "spearman". @seealso \link[stats]{cor}.
#' @param use An optional character string giving a method for computing covariances in the presence of missing values. This
#'          must be one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs" (default).
#'          @seealso \link[stats]{cor}.
#'
#' @return A matrix (1×181) containing all the 181 features (Goretzko & Buhner, 2020).
#'
#' @details
#' The code for the \code{extractor.feature.FF} function is implemented based on the publicly available code by Goretzko & Buhner (2020) (https://osf.io/mvrau/).
#' The extracted features are completely consistent with the 181 features described in the original text by Goretzko & Buhner (2020).
#' These features include:
#'
#' \itemize{
#'   \item \code{1.} - Number of examinees
#'   \item \code{2.} - Number of items
#'   \item \code{3.} - Number of eigenvalues greater than 1
#'   \item \code{4.} - Proportion of variance explained by the 1st eigenvalue
#'   \item \code{5.} - Proportion of variance explained by the 2nd eigenvalue
#'   \item \code{6.} - Proportion of variance explained by the 3rd eigenvalue
#'   \item \code{7.} - Number of eigenvalues greater than 0.7
#'   \item \code{8.} - Standard deviation of the eigenvalues
#'   \item \code{9.} - Number of eigenvalues accounting for 50% cumulative variance
#'   \item \code{10.} - Number of eigenvalues accounting for 75% cumulative variance
#'   \item \code{11.} - L1-norm of the correlation matrix
#'   \item \code{12.} - Frobenius-norm of the correlation matrix
#'   \item \code{13.} - Maximum-norm of the correlation matrix
#'   \item \code{14.} - Average of the off-diagonal correlations
#'   \item \code{15.} - Spectral-norm of the correlation matrix
#'   \item \code{16.} - Number of correlations smaller or equal to 0.1
#'   \item \code{17.} - Average of the initial communality estimates
#'   \item \code{18.} - Determinant of the correlation matrix
#'   \item \code{19.} - Measure of sampling adequacy (MSA after Kaiser, 1970)
#'   \item \code{20.} - Gini coefficient (Gini, 1921) of the correlation matrix
#'   \item \code{21.} - Kolm measure of inequality (Kolm, 1999) of the correlation matrix
#'   \item \code{22-101.} - Eigenvalues from Principal Component Analysis (PCA), padded with -1000 if insufficient
#'   \item \code{102-181.} - Eigenvalues from Factor Analysis (FA), fixed at 1 factor, padded with -1000 if insufficient
#' }
#'
#'
#'
#' @references
#' Goretzko, D., & Buhner, M. (2020). One model to rule them all? Using machine learning algorithms to determine the number of factors in exploratory factor analysis. Psychol Methods, 25(6), 776-786. https://doi.org/10.1037/met0000262.
#'
#'
#'
#' @export
#'
#' @importFrom psych smc
#' @importFrom ineq ineq
#' @importFrom stats cor sd

extractor.feature.FF <- function(response, cor.type = "pearson", use = "pairwise.complete.obs"){

  if(!any(rep(use, 5) == c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs")))
    stop("'use' must be one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete', or 'pairwise.complete.obs' !")

  response <- scale(response)

  N <- nrow(response)
  I <- ncol(response)
  dat_cor <- cor(response, method = cor.type, use = use)
  eigval <- eigen(dat_cor)$values
  vareig <- cumsum(eigval)/I

  # eigenvalue features

  eiggreater1 <- sum(eigval > 1)
  releig1 <- eigval[1]/I
  releig2 <- sum(eigval[1:2])/I
  releig3 <- sum(eigval[1:3])/I
  eiggreater07 <- sum(eigval > 0.7)
  sdeigval <- sd(eigval)
  var50 <- min(which(vareig > 0.50))
  var75 <- min(which(vareig > 0.75))

  # matrix norm features
  onenorm <- norm(dat_cor,"O")
  frobnorm <- norm(dat_cor,"F")
  maxnorm <- norm(dat_cor-diag(I),"M")
  avgcor <- sum(abs(dat_cor-diag(I)))/(I*(I-1))
  specnorm <- sqrt(eigen(t(dat_cor)%*%dat_cor)$values[1])

  smlcor <- sum(dat_cor <= 0.1)
  avgcom <- mean(psych::smc(dat_cor))
  det <- det(dat_cor)

  KMO <- KMO(dat_cor)$MSA
  Gini <- ineq::ineq(lower.tri(dat_cor), type = "Gini")
  Kolm <- ineq::ineq(lower.tri(dat_cor), type = "Kolm")

  # parallel analysis
  fa_eigval <- factor.analysis(response, nfact = 1, iter.max = 100,
                               criterion = 0.001, cor.type = cor.type, use = use)$eigen.value

  # setting missing eigenvalues to -1000
  eigval[(length(eigval)+1):80] <- -1000
  fa_eigval[(length(fa_eigval)+1):80] <- -1000
  names(eigval) <- paste("eigval", 1:80, sep = "")
  names(fa_eigval) <- paste("fa_eigval", 1:80, sep = "")
  eigval <- eigval[1:80]
  fa_eigval <- fa_eigval[1:80]

  # combination of features
  features <- cbind(data.frame(N, I, eiggreater1, releig1, releig2,releig3,eiggreater07,sdeigval,var50,var75,onenorm,frobnorm,
                               maxnorm, avgcor, specnorm, smlcor, avgcom,det, KMO, Gini, Kolm),
                    t(eigval), t(fa_eigval))

  return(features)

}

#' @importFrom stats cov2cor
KMO <- function (r) {
  cl <- match.call()
  Q <- tryCatch({
    solve(r)
  }, error = function(e){
    r
  })
  S2 <- diag(1/diag(Q))
  IC <- S2 %*% Q %*% S2
  Q <- Image <- cov2cor(Q)
  diag(Q) <- 0
  diag(r) <- 0
  sumQ2 <- sum(Q^2)
  sumr2 <- sum(r^2)
  MSA <- sumr2/(sumr2 + sumQ2)
  MSAi <- colSums(r^2)/(colSums(r^2) + colSums(Q^2))
  results <- list(MSA = MSA, MSAi = MSAi, Image = Image, ImCov = IC,
                  Call = cl)
  class(results) <- c("psych", "KMO")
  return(results)
}
