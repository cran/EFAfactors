#' Various Indeces in EFA
#'
#' @description
#' A function performs clustering on items by calling \link[psych]{VSS} and \link[psych]{fa}.
#' Apply the Very Simple Structure (VSS), Comparative Fit Index (CFI), MAP, and other
#' criteria to determine the appropriate number of factors.
#'
#'
#' @param response A required \code{N} × \code{I} matrix or data.frame consisting of the responses of \code{N} individuals
#'          to \code{I} items.
#' @param nfact.max The maximum number of factors discussed by CD approach. (default = 10)
#' @param cor.type How to find the correlations: "cor" is Pearson", "cov" is covariance, "tet" is tetrachoric, "poly" is polychoric,
#'          "mixed" uses mixed cor for a mixture of tetrachorics, polychorics, Pearsons, biserials, and
#'          polyserials, Yuleb is Yulebonett, Yuleq and YuleY are the obvious Yule coefficients as appropriate.
#' @param use an optional character string giving a method for computing covariances in the presence of missing values. This
#'          must be one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs" (default).
#'          @seealso \link[stats]{cor}.
#'
#' @return A \code{matrix} with the following components:
#' \describe{
#'   \item{CFI}{the Comparative Fit Index}
#'   \item{RMSEA}{Root Mean Square Error of Approximation (RMSEA) for each number of factors.}
#'   \item{SRMR}{Standardized Root Mean Square Residual.}
#'   \item{MAP}{Velicer's MAP values (lower values are better).}
#'   \item{BIC}{Bayesian Information Criterion (BIC) for each number of factors.}
#'   \item{SABIC}{Sample-size Adjusted Bayesian Information Criterion (SABIC) for each number of factors.}
#'   \item{chisq}{Chi-square statistic from the factor analysis output.}
#'   \item{df}{Degrees of freedom.}
#'   \item{prob}{Probability that the residual matrix is greater than 0.}
#'   \item{eChiSq}{Empirically found chi-square statistic.}
#'   \item{eCRMS}{Empirically found mean residual corrected for degrees of freedom.}
#'   \item{eBIC}{Empirically found BIC based on the empirically found chi-square statistic.}
#'   \item{vss}{VSS fit with complexity 1.}
#'   \item{sqresid}{Squared residual correlations.}
#'   \item{fit}{Factor fit of the complete model.}
#' }
#'
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
#' ## Run EFAindex function with default parameters.
#' \donttest{
#' EFAindex.matrix <- EFAindex(response)
#'
#' print(EFAindex.matrix)
#'
#' }
#'
#'
#'
#' @export
#' @importFrom psych fa
#' @importFrom psych VSS
#' @importFrom ddpcr quiet
#'
EFAindex <- function(response, nfact.max = 10,
                     cor.type = "cor", use = "pairwise.complete.obs" ){

  response <- scale(response)

  VSS.obj <- VSS(response, n = nfact.max, cor = cor.type, use = use, plot=FALSE)
  EFAindex.obj <- VSS.obj$vss.stats[, c(1:13, 15)]
  EFAindex.obj[, 9] <- VSS.obj$map
  colnames(EFAindex.obj)[1] <- "df"
  colnames(EFAindex.obj)[9] <- "MAP"
  colnames(EFAindex.obj)[14] <- "vss"
  CFI <- c()
  for(nfact in 1:nfact.max){
    ddpcr::quiet(fa.obj <- psych::fa(response, nfact, cor = cor.type, use = use, rotate = "none", fm = "minres"))
    CFI <- c(CFI, fa.obj$CFI)
  }
  EFAindex.obj <- cbind(EFAindex.obj, CFI)
  EFAindex.obj <- EFAindex.obj[, c(15, 6, 11, 9, 7, 8, 2, 1, 3, 10, 12, 13, 14, 4, 5)]
  rownames(EFAindex.obj) <- paste0("nfact=", 1:nfact.max)

  return(EFAindex.obj)
}
