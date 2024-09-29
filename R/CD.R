#'
#' the Comparison Data (CD) Approach
#'
#' @description
#' This function runs the comparison data (CD) approach of Ruscio & Roche (2012).
#'
#'
#' @param response A required \code{N} × \code{I} matrix or data.frame consisting of the responses of \code{N} individuals
#'          to × \code{I} items.
#' @param nfact.max The maximum number of factors discussed by CD approach. (default = 10)
#' @param N.pop Size of finite populations of simulating.. (default = 10,000)
#' @param N.Samples Number of samples drawn from each population. (default = 500)
#' @param Alpha Alpha level when testing statistical significance (Wilcoxon Rank Sum and Signed Rank Tests) of
#'          improvement with additional factor. (default = .30)
#' @param cor.type A character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default),
#'          "kendall", or "spearman". @seealso \link[stats]{cor}.
#' @param use an optional character string giving a method for computing covariances in the presence of missing values. This
#'          must be one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs" (default).
#'          @seealso \link[stats]{cor}.
#' @param vis A Boolean variable that will print the factor retention results when set to TRUE, and will not print
#'          when set to FALSE. (default = TRUE)
#' @param plot A Boolean variable that will print the CD plot when set to TRUE, and will not print it when set to
#'          FALSE. @seealso \link[EFAfactors]{plot.CD}. (Default = TRUE)
#'
#'
#' @return An object of class \code{CD} is a \code{list} containing the following components:
#' \item{nfact}{The number of factors to be retained.}
#' \item{RMSE.Eigs}{A matrix containing the root mean square error (RMSE) of the eigenvalues
#'                  produced by each simulation for every discussed number of factors.}
#' \item{Sig}{A boolean variable indicating whether the significance level of the Wilcoxon
#'      Rank Sum and Signed Rank Tests has reached Alpha.}
#'
#'
#'
#' @details
#' Ruscio and Roche (2012) proposed a method for determining the number of factors through comparison data (CD).
#' This method identifies the appropriate number of factors by finding the solution that best reproduces the pattern
#' of eigenvalues. CD employs an iterative procedure when generating comparison data with a known factor structure,
#' taking into account previous factors. Initially, CD compares whether the simulated comparison data with one
#' latent factor (j=1) reproduces the empirical eigenvalue pattern significantly worse than the two-factor solution (j+1).
#' If so, CD increases the value of j until further improvements are no longer significant or a preset maximum number of
#' factors is reached. Specifically, CD involves five steps:
#'
#' 1. Generate random data with either j or j+1 latent factors and calculate the eigenvalues of the respective correlation matrices.
#'
#' 2. Compute the root mean square error (RMSE) of the difference between the empirical and simulated eigenvalues using the formula
#' \deqn{
#'    RMSE = \sqrt{\sum_{i=1}^{p} (\lambda_{emp,i} - \lambda_{sim,i})^2}
#' }
#' , where:
#' \itemize{
#'   \item \eqn{\lambda_{emp,i}}: The i-th empirical eigenvalue.
#'   \item \eqn{\lambda_{sim,i}}: The i-th simulated eigenvalue.
#'   \item \eqn{p}: The number of items or eigenvalues.
#' }
#' . This step produces two RMSEs, corresponding to the different numbers of latent factors.
#'
#' 3. Repeat steps 1 and 2, 500 times ( default in the Package ).
#'
#' 4. Use a one-sided Wilcoxon test (alpha = 0.30) to assess whether the RMSE is significantly reduced under the two-factor condition.
#'
#' 5. If the difference in RMSE is not significant, CD suggests selecting j factors. Otherwise, j is increased by 1, and steps 1 to 4 are repeated.
#'
#' The code is implemented based on the resources available at:
#' \itemize{
#'   \item \url{https://ruscio.pages.tcnj.edu/quantitative-methods-program-code/}
#'   \item \url{https://osf.io/gqma2/?view_only=d03efba1fd0f4c849a87db82e6705668}
#'   \item \url{https://osf.io/mvrau/}
#' }
#'
#' Since the CD approach requires extensive data simulation and computation, C++ code is used to speed up the process.
#' @seealso \link[EFAfactors]{GenData}
#'
#'
#' @references
#' Auerswald, M., & Moshagen, M. (2019). How to determine the number of factors to retain in exploratory factor analysis: A comparison of extraction methods under realistic conditions. Psychological methods, 24(4), 468-491. https://doi.org/https://doi.org/10.1037/met0000200.
#'
#' Goretzko, D., & Buhner, M. (2020). One model to rule them all? Using machine learning algorithms to determine the number of factors in exploratory factor analysis. Psychol Methods, 25(6), 776-786. https://doi.org/10.1037/met0000262.
#'
#' Ruscio, J., & Roche, B. (2012). Determining the number of factors to retain in an exploratory factor analysis using comparison data of known factorial structure. Psychological Assessment, 24, 282–292. http://dx.doi.org/10.1037/a0025697.
#'
#'
#' @author Haijiang Qin <Haijiang133@outlook.com>
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
#' ## Run CD function with default parameters.
#' \donttest{
#' CD.obj <- CD(response)
#'
#' print(CD.obj)
#'
#' ## CD plot
#' plot(CD.obj)
#'
#' ## Get the RMSE.Eigs and nfact results.
#' RMSE.Eigs <- CD.obj$RMSE.Eigs
#' nfact <- CD.obj$nfact
#'
#' head(RMSE.Eigs)
#' print(nfact)
#'
#' }
#'
#' ## Limit the maximum number of factors to 8, with populations set to 5000.
#' \donttest{
#' CD.obj <- CD(response, nfact.max=8, N.pop = 5000)
#'
#' print(CD.obj)
#'
#' ## CD plot
#' plot(CD.obj)
#'
#' ## Get the RMSE.Eigs and nfact results.
#' RMSE.Eigs <- CD.obj$RMSE.Eigs
#' nfact <- CD.obj$nfact
#'
#' head(RMSE.Eigs)
#' print(nfact)
#'
#' }
#'
#'
#'
#'
#' @export
#' @importFrom stats cor wilcox.test
#'
CD <- function(response, nfact.max=10, N.pop = 10000, N.Samples = 500, Alpha = .30,
               cor.type = "pearson", use = "pairwise.complete.obs",
               vis=TRUE, plot = TRUE) {

  if(!any(rep(use, 5) == c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs")))
    stop("'use' must be one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete', or 'pairwise.complete.obs' !")

  N <- dim(response)[1]
  I <- dim(response)[2]

  response <- as.matrix(scale(response))

  cor.response <- cor(response, method = cor.type, use = use)
  Eigs.response <- eigen(cor.response)$values
  RMSE.Eigs <- matrix(0, nrow = N.Samples, ncol = nfact.max)
  Sig <- T
  nfact <- 1

  while ((nfact <= nfact.max) & (Sig)) {
    if(vis)
      cat("\rCD is simulating data:", paste0("nfact=", sprintf("%2d", nfact), "/", nfact.max))

    Pop <- GenData(response, nfact = nfact, N.pop = N.pop, cor.type = cor.type, use = use)
    RMSE.Eigs[, nfact] <- calculateRMSE(Pop, Eigs.response, N.Samples, N.pop, N)

    if (nfact > 1) Sig <- (wilcox.test(RMSE.Eigs[, nfact], RMSE.Eigs[,(nfact - 1)], "less")$p.value < Alpha)
    if (Sig) nfact <- nfact + 1
  }
  nfact <- nfact - 1

  CD.obj <- list(nfact=nfact, RMSE.Eigs=RMSE.Eigs, Sig=Sig)
  class(CD.obj) <- "CD"

  if(vis){
    cat("\n")
    print(CD.obj)
  }
  if(plot) plot(CD.obj)

  return(CD.obj)
}

