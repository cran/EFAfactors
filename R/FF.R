#'
#' Factor Forest (FF) Powered by An Tuned XGBoost Model for Determining the Number of Factors
#'
#' @description
#' This function will invoke a tuned XGBoost model (Goretzko & Buhner, 2020; Goretzko, 2022; Goretzko & Ruscio, 2024) that can reliably
#' perform the task of determining the number of factors. The maximum number of factors that the network can discuss is 8.
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
#' @param plot A Boolean variable that will print the FF plot when set to TRUE, and will not print it when set to
#'          FALSE. @seealso \link[EFAfactors]{plot.FF}. (Default = TRUE)
#'
#' @return An object of class \code{FF} is a \code{list} containing the following components:
#' \item{nfact}{The number of factors to be retained.}
#' \item{probability}{A matrix containing the probabilities for factor numbers ranging from 1
#'                    to 8 (1x8), where the number in the f-th column represents the probability
#'                    that the number of factors for the response is f.}
#' \item{features}{A matrix (1×184) containing all the features for determining the number of
#'       factors by the tuned XGBoost Model.}
#'
#' @details
#' A total of 500,000 datasets were simulated to extract features for training the tuned XGBoost
#' model (Goretzko & Buhner, 2020; Goretzko, 2022).
#' Each dataset was generated according to the following specifications:
#'
#' \itemize{
#'   \item Factor number: \emph{F} ~ U(1,8)
#'   \item Sample size: \emph{N} ~ (200,1000)
#'   \item Number of variables per factor: \emph{vpf} ~ (3,10)
#'   \item Factor correlation: \emph{fc} ~ U(0.0,0.4)
#'   \item Primary loadings: \emph{pl} ~ (0.35,0.80)
#'   \item Cross-loadings: \emph{cl} ~ (0.0,0.2)
#' }
#'
#' A population correlation matrix was created for each data set based on the following decomposition:
#' \deqn{\mathbf{\Sigma} = \mathbf{\Lambda} \mathbf{\Phi} \mathbf{\Lambda}^T + \mathbf{\Delta}}
#' where \eqn{\mathbf{\Lambda}} is the loading matrix, \eqn{\mathbf{\Phi}} is the factor correlation
#' matrix, and \eqn{\mathbf{\Delta}} is a diagonal matrix,
#' with \eqn{\mathbf{\Delta} = 1 - \text{diag}(\mathbf{\Lambda} \mathbf{\Phi} \mathbf{\Lambda}^T)}.
#' The purpose of \eqn{\mathbf{\Delta}} is to ensure that the diagonal elements of \eqn{\mathbf{\Sigma} } are 1.
#'
#' The response data for each subject were simulated using the following formula:
#' \deqn{X_i = L_i + \epsilon_i, \quad 1 \leq i \leq I}
#' where \eqn{L_i} follows a normal distribution \eqn{N(0, \sigma)}, representing the contribution of latent factors,
#' and \eqn{\epsilon_i} is the residual term following a standard normal distribution. \eqn{L_i} and \eqn{\epsilon_i}
#' are uncorrelated, and \eqn{\epsilon_i} and \eqn{\epsilon_j} are also uncorrelated.
#'
#' For each simulated dataset, a total of 184 features are extracted and compiled into a feature vector.
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
#'   \item \code{21.} - Number of factors retained by the PA method @seealso \link[EFAfactors]{PA}
#'   \item \code{23.} - Number of factors retained by the EKC method @seealso \link[EFAfactors]{EKC}
#'   \item \code{24.} - Number of factors retained by the CD method @seealso \link[EFAfactors]{CD}
#'   \item \code{25-104.} - Eigenvalues from Principal Component Analysis (PCA), padded with -1000 if insufficient
#'   \item \code{105-184.} - Eigenvalues from Factor Analysis (FA), fixed at 1 factor, padded with -1000 if insufficient
#' }
#'
#' The code for the \code{FF} function is implemented based on the publicly available code by Goretzko & Buhner (2020) (https://osf.io/mvrau/).
#' The Tuned XGBoost Model is also obtained from this site. However, to meet the requirements for a streamlined R package, we can only
#' save the core components of the Tuned XGBoost Model. Although these non-core parts do not affect performance, they include a lot of information
#' about the model itself, such as the number of features, subsets of samples, and data from the training process, among others.
#' For the complete Tuned XGBoost Model, please download it from https://osf.io/mvrau/.
#'
#'
#' @references
#' Goretzko, D., & Buhner, M. (2020). One model to rule them all? Using machine learning algorithms to determine the number of factors in exploratory factor analysis. Psychol Methods, 25(6), 776-786. https://doi.org/10.1037/met0000262.
#'
#' Goretzko, D. (2022). Factor Retention in Exploratory Factor Analysis With Missing Data. Educ Psychol Meas, 82(3), 444-464. https://doi.org/10.1177/00131644211022031.
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
#' ## Run FF function with default parameters.
#' \donttest{
#' FF.obj <- FF(response)
#'
#' print(FF.obj)
#'
#' plot(FF.obj)
#'
#' ## Get the probability and nfact results.
#' probability <- FF.obj$probability
#' nfact <- FF.obj$nfact
#'
#' print(probability)
#' print(nfact)
#'
#' }
#'
#'
#' @export
#'
#' @import xgboost
#' @importFrom ineq ineq
#' @importFrom psych smc
#' @importFrom psych KMO
#' @importFrom psych fa.parallel
#' @importFrom ddpcr quiet
#' @importFrom ineq ineq
#' @import BBmisc
#' @import mlr
#' @importFrom BBmisc convertDataFrameCols
#' @importFrom stats cor predict sd
#' @importFrom mlr getTaskClassLevels
#' @importFrom ParamHelpers makeParamSet makeNumericLearnerParam makeIntegerLearnerParam makeUntypedLearnerParam makeLogicalLearnerParam makeDiscreteLearnerParam
#'
FF <- function(response, cor.type = "pearson", use = "pairwise.complete.obs", vis=TRUE, plot = TRUE){

  if(!any(rep(use, 5) == c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs")))
    stop("'use' must be one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete', or 'pairwise.complete.obs' !")

  model.xgb <- load_xgb()

  response <- scale(response)

  features <- extractor.feature.FF(response, cor.type=cor.type, use=use)
  names(features)[2] <- "p"

  ddpcr::quiet(pa <- PA(response, fa="fa", n.iter = 20, plot = FALSE))
  pa_solution <- pa$nfact
  ekc <- EKC(response, cor.type = cor.type, use=use, vis = FALSE, plot = FALSE)$nfact
  cd <- CD(response, nfact.max = 8, use = use, vis = FALSE, plot = FALSE)$nfact

  # combination of features
  features <- cbind(data.frame(features[1:21], pa_solution, ekc, cd), features[22:181])

  ddpcr::quiet(probability <- predict(model.xgb, newdata = features)$data)

  nfact <- as.numeric(probability$response)
  probability <- as.matrix(probability[-9])
  colnames(probability) <- NULL

  FF.obj <- list(nfact=nfact, probability=probability, features=features)
  class(FF.obj) <- "FF"

  if(vis) print(FF.obj)
  if(plot) plot(FF.obj)

  return(FF.obj)
}


#' @title Prediction Function for the Tuned XGBoost Model with Early Stopping
#'
#' @description
#' This function performs predictions using a trained XGBoost model with early stopping.
#' The function itself does not have any specific purpose; its existence is solely to
#' ensure the proper operation of \link[EFAfactors]{FF}.
#'
#' @param .learner An object representing the learner.
#' @param .model The trained XGBoost model used to make predictions.
#' @param .newdata A data frame or matrix containing new observations for which predictions are to be made.
#' @param ... Additional parameters passed to the \code{predict} function in XGBoost.
#'
#' @return A vector of predicted class labels or a matrix of predicted probabilities.
#'
#' @export
#' @import mlr
predictLearner.classif.xgboost.earlystop = function(.learner,  .model, .newdata, ...) {

  m = .model$learner.model

  cls = as.character(1:8)
  nc = length(cls)

  p = predict(m, newdata = data.matrix(BBmisc::convertDataFrameCols(.newdata, ints.as.num = TRUE)), ...)

  p = matrix(p, nrow = length(p) / nc, ncol = nc, byrow = TRUE)
  colnames(p) = cls

  return(p)
}
