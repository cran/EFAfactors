
#'
#' Simulating Data Following John Ruscio's RGenData
#'
#'
#' @description
#' This function simulates data with \eqn{nfact} factors based on empirical data.
#' It represents the simulation data part of the \link[EFAfactors]{CD} function
#' and the \link[EFAfactors]{CDF} function. This function improves upon
#' \link[RGenData]{GenDataPopulation} by utilizing C++ code to achieve faster data simulation.
#'
#' @param response A required \code{N} × \code{I} matrix or data.frame consisting of the responses of \code{N} individuals
#'          to \code{I} items.
#' @param nfact The number of factors to extract in factor analysis. (default = 1)
#' @param N.pop Size of finite populations for simulating. (default = 10,000)
#' @param Max.Trials The maximum number of consecutive trials without obtaining a lower RMSR. (default = 5)
#' @param lr The learning rate for updating the correlation matrix during iteration. (default = 1)
#' @param cor.type A character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default),
#'          "kendall", or "spearman". @seealso \link[stats]{cor}.
#' @param use An optional character string specifying a method for computing covariances in the presence of missing values. This
#'          must be one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs" (default).
#'          @seealso \link[stats]{cor}.
#' @param isSort Logical, determines whether the simulated data needs to be sorted in descending order. (default = FALSE)
#'
#' @return A \code{N.pop} * \code{I} matrix containing the simulated data.
#'
#'
#' @details
#' The core idea of \code{GenData} is to start with the empirical data's correlation matrix
#' and iteratively approach data with \code{nfact} factors. Any value in the simulated data must come
#' from the empirical data. The specific steps of \code{GenData} are as follows:
#'
#' \describe{
#'   \item{(1)}{Use the empirical data (\eqn{\mathbf{Y}_{emp}}) correlation matrix as the target, \eqn{\mathbf{R}_{targ}}.}
#'   \item{(2)}{Simulate scores for \eqn{N.pop} examinees on \eqn{nfact} factors using a multivariate standard normal distribution:
#'         \deqn{\mathbf{S}_{(N.pop \times nfact)} \sim \mathcal{N}(0, 1)}
#'         Simulate noise for \eqn{N.pop} examinees on \eqn{I} items:
#'         \deqn{\mathbf{U}_{(N.pop \times I)} \sim \mathcal{N}(0, 1)}}
#'   \item{(3)}{Initialize \eqn{\mathbf{R}_{temp} = \mathbf{R}_{targ}}, and set the minimum Root
#'         Mean Square Residual \eqn{RMSR_{min} = \text{Inf}}. Start the iteration process.}
#'   \item{(4)}{Extract \code{nfact} factors from \eqn{\mathbf{R}_{temp}}, and obtain the factor
#'              loadings matrix \eqn{\mathbf{L}_{shar}}. Ensure that the first element of
#'              \eqn{\mathbf{L}_{share}} is positive to standardize the direction.}
#'   \item{(5)}{Calculate the unique factor matrix \eqn{\mathbf{L}_{uniq, (I \times 1)}}:
#'         \deqn{L_{uniq,i} = \sqrt{1 - \sum_{j=1}^{nfact} L_{share, i, j}^2}}}
#'   \item{(6)}{Calculate the simulated data \eqn{\mathbf{Y}_{sim}}:
#'         \deqn{Y_{sim, i, j} = \mathbf{S}_{i} \mathbf{L}_{shar, j}^T + U_{i, j} L_{uniq,i}}}
#'   \item{(7)}{Compute the correlation matrix of the simulated data, \eqn{\mathbf{R}_{simu}}.}
#'   \item{(8)}{Calculate the residual correlation matrix \eqn{\mathbf{R}_{resi}} between the
#'         target matrix \eqn{\mathbf{R}_{targ}} and the simulated data's correlation matrix \eqn{\mathbf{R}_{simu}}:
#'         \deqn{\mathbf{R}_{resi} = \mathbf{R}_{targ} - \mathbf{R}_{simu}}}
#'   \item{(9)}{Calculate the current RMSR:
#'         \deqn{RMSR_{cur} = \sqrt{\frac{\sum_{i < j} \mathbf{R}_{resi, i, j}^2}{0.5 \times (I^2 - I)}}}}
#'   \item{(10)}{If \eqn{RMSR_{cur} < RMSR_{min}}, update \eqn{\mathbf{R}_{temp} = \mathbf{R}_{temp} +
#'               lr \times \mathbf{R}_{resi}}, \eqn{RMSR_{min} = RMSR_{cur}}, set \eqn{\mathbf{R}_{min, resi} = \mathbf{R}_{resi}},
#'               and reset the count of consecutive trials without improvement \eqn{cou = 0}.
#'               If \eqn{RMSR_{cur} \geq RMSR_{min}}, update \eqn{\mathbf{R}_{temp} = \mathbf{R}_{temp} +
#'               0.5 \times cou \times lr \times \mathbf{R}_{min, resi}} and increment \eqn{cou = cou + 1}.}
#'   \item{(11)}{Repeat steps (4) through (10) until \eqn{cou \geq Max.Trials}.}
#' }
#'
#' Of course C++ code is used to speed up.
#'
#'
#'
#' @references
#' Ruscio, J., & Roche, B. (2012). Determining the number of factors to retain in an exploratory factor analysis using comparison data of known factorial structure. Psychological Assessment, 24, 282–292. http://dx.doi.org/10.1037/a0025697.
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
#' \donttest{
#' data.simulated <- GenData(response, nfact = 1, N.pop = 10000)
#' head(data.simulated)
#' }
#'
#'
#'
#' @export
#' @importFrom stats cor rnorm
#'
GenData <- function(response, nfact = 1, N.pop = 10000, Max.Trials = 5, lr = 1,
                    cor.type = "pearson", use = "pairwise.complete.obs", isSort = FALSE){

  I <- dim(response)[2] ## item number
  Data <- matrix(0, nrow = N.pop, ncol = I)            # Matrix to store the simulated data
  Iteration <- 0                                   # Iteration counter
  Best.RMSR <- 1                                   # Lowest RMSR correlation
  Trials.Without.Improvement <- 0                  # Trial counter

  # Generate distribution for each variable (step 2) -------------------------------------------------------------
  ## Matrix to store each variable's score distribution
  Distributions <- generateDistributions(response, I, N.pop)

  # Calculate and store a copy of the target correlation matrix (step 3) -----------------------------------------
  if(dim(response)[1] != I){
    Target.Corr <- cor(response, method = cor.type, use = use)
  } else {
    Target.Corr <- response
  }
  Best.Corr <- Intermediate.Corr <- Target.Corr

  # Generate random normal data for shared and unique components, initialize factor loadings (steps 5, 6) --------
  Shared.Comp <- matrix(rnorm(N.pop * nfact, 0, 1), nrow = N.pop, ncol = nfact)
  Unique.Comp <- matrix(rnorm(N.pop * I, 0, 1), nrow = N.pop, ncol = I)
  Shared.Load <- matrix(0, nrow = I, ncol = nfact)
  Unique.Load <- matrix(0, nrow = I, ncol = 1)

  # Begin loop that ends when specified number of iterations pass without improvement in RMSR correlation --------
  while (Trials.Without.Improvement < Max.Trials) {
    Iteration <- Iteration + 1

    # Calculate factor loadings and apply to reproduce desired correlations (steps 7, 8) ---------------------------
    fact.anal <- factor.analysis(Intermediate.Corr, nfact = nfact, cor.type = cor.type, use = use)
    if (nfact == 1) {
      Shared.Load[, 1] <- fact.anal$loadings
    } else {
      Shared.Load[, 1:nfact] <- fact.anal$loadings[, 1:nfact]
    }
    Shared.Load[Shared.Load > 1] <- 1
    Shared.Load[Shared.Load < -1] <- -1
    if (Shared.Load[1,1] < 0) {
      Shared.Load <- Shared.Load * -1
    }
    for (i in 1:I){
      if (sum(Shared.Load[i,] * Shared.Load[i,]) < 1) {
        Unique.Load[i, 1] <- (1 - sum(Shared.Load[i,] * Shared.Load[i,]))
      } else {
        Unique.Load[i, 1] <- 0
      }
    }
    Unique.Load <- sqrt(Unique.Load)

    Data <- calculateData(I, Shared.Comp, Shared.Load, Unique.Comp, Unique.Load)
    if(isSort)
      Data <- sortData(Data, Distributions)

    # Calculate RMSR correlation, compare to lowest value, take appropriate action (steps 10, 11, 12) --------------
    Reproduced.Corr <- cor(Data, method = cor.type, use = use)
    Residual.Corr <- Target.Corr - Reproduced.Corr
    RMSR <- sqrt(sum(Residual.Corr[lower.tri(Residual.Corr)] * Residual.Corr[lower.tri(Residual.Corr)]) / (0.5 * (I * I - I)))
    if (RMSR < Best.RMSR) {
      Best.RMSR <- RMSR
      Best.Corr <- Intermediate.Corr
      Best.Res <- Residual.Corr
      Intermediate.Corr <- Intermediate.Corr + lr * Residual.Corr
      Trials.Without.Improvement <- 0
    } else {
      Trials.Without.Improvement <- Trials.Without.Improvement + 1
      Current.Multiplier <- lr * 0.5 ^ Trials.Without.Improvement
      Best.Res <- Residual.Corr
      Intermediate.Corr <- Best.Corr + Current.Multiplier * Best.Res
    }
  }

  fact.anal <- factor.analysis(Best.Corr, nfact = nfact, cor.type = cor.type, use = use)

  if (nfact == 1) {
    Shared.Load[,1] <- fact.anal$loadings
  } else {
    for (i in 1:nfact)
      Shared.Load[,i] <- fact.anal$loadings[,i]
  }
  Shared.Load[Shared.Load > 1] <- 1
  Shared.Load[Shared.Load < -1] <- -1
  if (Shared.Load[1,1] < 0) {
    Shared.Load <- Shared.Load * -1
  }
  for (i in 1:I){
    if (sum(Shared.Load[i,] * Shared.Load[i,]) < 1) {
      Unique.Load[i,1] <- (1 - sum(Shared.Load[i,] * Shared.Load[i,]))
    } else {
      Unique.Load[i,1] <- 0
    }
  }

  Unique.Load <- sqrt(Unique.Load)
  for (i in 1:I){
    Data[,i] <- (Shared.Comp %*% t(Shared.Load))[,i] + Unique.Comp[,i] * Unique.Load[i,1]
  }

  Data <- apply(Data, 2, scale) # standardizes each variable in the matrix
  for (i in 1:I) {
    Data <- Data[sort.list(Data[, i]),]
    Data[,i] <- Distributions[, i]
  }

  return(Data)
}
