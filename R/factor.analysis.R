#' Factor Analysis by Principal Axis Factoring
#'
#' @description
#' This function performs factor analysis using the Principal Axis Factoring (PAF) method.
#' The process involves extracting factors from an initial correlation matrix and iteratively
#' refining the factor estimates until convergence is achieved.
#'
#' @param data A data.frame or matrix of response If the matrix is square, it is assumed to
#'          be a correlation matrix. Otherwise, correlations (with pairwise deletion) will be computed.
#' @param nfact The number of factors to extract. (default = 1)
#' @param iter.max The maximum number of iterations for the factor extraction process. Default is 1000.
#' @param criterion The convergence criterion for the iterative process. The extraction process will
#'          stop when the change in communalities is less than this value. Default is 0.001
#' @param cor.type A character string indicating which correlation coefficient (or covariance) is to be computed.
#'          One of "pearson" (default), "kendall", or "spearman". @seealso \link[stats]{cor}.
#' @param use An optional character string giving a method for computing covariances in the presence of missing values.
#'          This must be one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs" (default).
#'          @seealso \link[stats]{cor}.
#'
#' @return A list containing:
#' \item{loadings}{The extracted factor loadings.}
#' \item{eigen.value}{The eigenvalues of the correlation matrix.}
#' \item{H2}{A vector that contains the explanatory power of the factor model for all items.}
#'
#'
#' @details
#' The Principal Axis Factoring (PAF) method involves the following steps:
#'
#' Step 1. **Basic Principle**:
#'    The core principle of factor analysis using Principal Axis Factoring (PAF) is expressed as:
#'    \deqn{\mathbf{R} = \mathbf{\Lambda} \mathbf{\Lambda}^T + \mathbf{\Phi}}
#'    \deqn{R_{ii} = H_i^2 + \Phi_{ii}}
#'    where \eqn{\mathbf{\Lambda}} is the matrix of factor loadings, and \eqn{\mathbf{\Phi}} is the diagonal
#'    matrix of unique variances. Here, \eqn{H_i^2} represents the portion of the i-th item's variance explained by the factor model.
#'    \eqn{\mathbf{H}^2} reflects the amount of total variance in the variable accounted for by the factors in the model, indicating the
#'    explanatory power of the factor model for that variable.
#'
#' Step 2. **Factor Extraction by Iteratoin**:
#'
#'    - Initial Communalities:
#'      Compute the initial communalities as the squared multiple correlations:
#'      \deqn{H_{i(t)}^2 = R_{ii(t)}}
#'      where \eqn{H_{i(t)}^2} is the communality of i-th item in the \eqn{t}-th iteration, and \eqn{R_{ii(t)}} is the i-th
#'      diagonal element of the correlation matrix in the \eqn{t}-th iteration.
#'
#'    - Extract Factors and Update Communalities:
#'      \deqn{\Lambda_{ij} = \sqrt{\lambda_j} \times v_{ij}}
#'      \deqn{H_{i(t+1)}^2 = \sum_j \Lambda_{ij}^2}
#'      \deqn{R_{ii(t+1)} = H_{i(t+1)}^2}
#'      where \eqn{\Lambda_{ij}} represents the j-th factor loading for the i-th item, \eqn{\lambda_j} is the j-th
#'      eigenvalue, \eqn{H_{i(t+1)}^2} is the communality of i-th item in the \eqn{t+1}-th iteration, and \eqn{v_{ij}} is
#'      the j-th value of the i-th item in the eigen vector matrix \eqn{\mathbf{v}}.
#'
#' Step 3. **Iterative Refinement**:
#'
#'    - Calculate the Change between \eqn{\mathbf{H}_{t}^2} and \eqn{\mathbf{H}_{t+1}^2}:
#'      \deqn{\Delta H_i^2 = \lvert H_{i(t+1)}^2 - H_{i(t)}^2 \lvert}
#'      where \eqn{\Delta H_i^2} represents the change in communalities between iterations \eqn{t} and \eqn{t+1}.
#'
#'    - Convergence Criterion:
#'      Continue iterating until the change in communalities is less than the specified criterion \eqn{criterion}:
#'      \deqn{\sum_i \Delta H_i^2 < criterion}
#'
#' The iterative process is implemented using C++ code to ensure computational speed.
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
#' ## Run factor.analysis function to extract 5 factors
#' \donttest{
#' PAF.obj <- factor.analysis(response, nfact = 5)
#'
#'
#' ## Get the loadings, eigen.value and  H2 results.
#' loadings <- PAF.obj$loadings
#' eigen.value <- PAF.obj$eigen.value
#' H2 <- PAF.obj$H2
#'
#' print(loadings)
#' print(eigen.value)
#' print(H2)
#'
#' }
#'
#'
#'
#' @importFrom stats cor
#'
#' @export
#'
factor.analysis <- function(data, nfact = 1, iter.max = 1000, criterion = 0.001,
                            cor.type = "pearson", use = "pairwise.complete.obs") {

  if(!any(rep(use, 5) == c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs")))
    stop("'use' must be one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete', or 'pairwise.complete.obs' !")

  data <- as.matrix(data)

  if(nfact > ncol(data))
    stop("The number of factors cannot exceed the number of items !")

  if (ncol(data) != nrow(data)) {
    Cor.Matrix <- as.matrix(cor(data, method = cor.type, use = use))
  } else {
    if(!is.Symmetric(data)){
      Cor.Matrix <- as.matrix(cor(data, method = cor.type, use = use))
    }else{
      Cor.Matrix <- data
    }
  }

  I <- dim(data)[2]

  FactorAnalysisCpp.obj <- PAFCpp(Cor.Matrix, nfact, Max_Iter = iter.max, Criterion = criterion)

  loadings <- FactorAnalysisCpp.obj$loadings
  eigen.value <- FactorAnalysisCpp.obj$eigen
  H2 <- FactorAnalysisCpp.obj$H2

  return(list(loadings = loadings[, 1:nfact], eigen.value = eigen.value, H2=H2))
}

is.Symmetric <- function(mat) {
  all(mat == t(mat))
}
