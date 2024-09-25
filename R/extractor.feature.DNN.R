#'
#' Extracting features for the Pre-Trained Deep Neural Network (DNN)
#'
#' @description
#' This function is used to extract the features required by the Pre-Trained Deep
#' Neural Network (DNN).  @seealso \link[EFAfactors]{DNN_predictor}
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
#' @return A matrix (1×54) containing all the features for determining the number of
#'       factors by the DNN.
#'
#'
#' @details
#'  A total of two types of features (6 kinds, making up 54 features in total) will be extracted, and they are as follows:
#' 1. Clustering-Based Features
#' \describe{
#'   \item{(1)}{Hierarchical clustering is performed with correlation coefficients as dissimilarity.
#'              The top 9 tree node heights are calculated, and all heights are divided by the maximum
#'              height. The heights from the 2nd to 9th nodes are used as features. @seealso \link[EFAfactors]{EFAhclust}}
#'   \item{(2)}{Hierarchical clustering with Euclidean distance as dissimilarity is performed. The top 9
#'              tree node heights are calculated, and all heights are divided by the maximum height. The
#'              heights from the 2nd to 9th nodes are used as features. @seealso \link[EFAfactors]{EFAhclust}}
#'   \item{(3)}{K-means clustering is applied with the number of clusters ranging from 1 to 9. The
#'              within-cluster sum of squares (WSS) for clusters 2 to 9 are divided by the WSS for a single
#'              cluster. @seealso \link[EFAfactors]{EFAkmeans}}
#' }
#' These three features are based on clustering algorithms. The purpose of division is to normalize the
#' data. These clustering metrics often contain information unrelated to the number of factors, such as
#' the number of items and the number of respondents, which can be avoided by normalization. The reason
#' for using the 2nd to 9th data is that only the top F-1 data are needed to determine the number of factors F.
#' The first data point is fixed at 1 after the division operations, so it is excluded. This approach
#' helps in model simplification.
#'
#' 2. Traditional Exploratory Factor Analysis Features (Eigenvalues)
#' \describe{
#'   \item{(4)}{The top 10 largest eigenvalues.}
#'   \item{(5)}{The ratio of the top 10 largest eigenvalues to the corresponding reference eigenvalues from
#'              Empirical Kaiser Criterion (EKC; Braeken & van Assen, 2017). @seealso \link[EFAfactors]{EKC}}
#'   \item{(6)}{The cumulative variance proportion of the top 10 largest eigenvalues.}
#' }
#' Only the top 10 elements are used to simplify the model.
#' @seealso \link[EFAfactors]{DNN_predictor}
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
#' ## Run extractor.feature.DNN function with default parameters.
#' \donttest{
#'  features <- extractor.feature.DNN(response)
#'
#'  print(features)
#'
#' }
#'
#'
#'
#'
#' @export
#'
#' @importFrom stats cor
#'
extractor.feature.DNN <- function(response,
                                  cor.type = "pearson", use = "pairwise.complete.obs"){

  if(!any(rep(use, 5) == c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs")))
    stop("'use' must be one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete', or 'pairwise.complete.obs' !")

  response <- scale(response)

  ehc.obj <- EFAhclust(response, dissimilarity.type="E", plot = FALSE)
  rhc.obj <- EFAhclust(response, dissimilarity.type="R", plot = FALSE)
  kmeans.obj <- EFAkmeans(response, nfact.max = 9, plot = FALSE)

  eheight <- ehc.obj$heights[2:(10-1)] / ehc.obj$heights[1]
  rheight <- rhc.obj$heights[2:(10-1)] / rhc.obj$heights[1]
  wss <- kmeans.obj$wss[2:(10-1)] / kmeans.obj$wss[1]

  EKC.obj <- EKC(response, cor.type = cor.type, use = use, vis = 0, plot = FALSE)
  eigen.value <- EKC.obj$eigen.value[1:10]
  eigen.ref <- eigen.value / EKC.obj$eigen.ref[1:10]
  var.account <- eigen.value / sum(EKC.obj$eigen.value)
  for(i in 2:10)
    var.account[i] <-  var.account[i-1] + var.account[i]

  features <- c(eigen.value, eigen.ref, var.account, rheight, eheight, wss)

  return(features)
}
