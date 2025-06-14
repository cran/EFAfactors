#'
#' Hierarchical Clustering for EFA
#'
#' @description
#' A function performs clustering on items by calling \link[stats]{hclust}.
#' Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.
#' The items will be continuously clustered in pairs until all items are grouped
#' into a single cluster, at which point the process will stop.
#'
#'
#' @param response A required \code{N} × \code{I} matrix or data.frame consisting of the responses of \code{N} individuals
#'          to \code{I} items.
#' @param dissimilarity.type A character indicating which kind of dissimilarity is to be computed. One of "R" or "E" (default)
#'          for the correlation coefficient or Euclidean distance.
#' @param method the agglomeration method to be used. This should be (an unambiguous abbreviation of) one of "ward.D",
#'          "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
#'          (default = "ward.D")
#'          @seealso \code{\link[stats]{hclust}}
#' @param cor.type A character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default),
#'          "kendall", or "spearman". @seealso \link[stats]{cor}.
#' @param use An optional character string giving a method for computing covariances in the presence of missing values. This
#'          must be one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs" (default).
#'          @seealso \link[stats]{cor}.
#' @param nfact.max The maximum number of factors discussed by the Second-Order Difference (SOD) approach. (default = 10)
#' @param plot A Boolean variable that will print the EFAhclust plot when set to TRUE, and will not print it when set to
#'          FALSE. @seealso \link[EFAfactors]{plot.EFAhclust}. (Default = TRUE)
#'
#' @return An object of class \code{EFAhclust} is a \code{list} containing the following components:
#' \item{hc}{An object of class \code{hclust} that describes the tree produced by the clustering process. @seealso \link[stats]{hclust}}
#' \item{cor.response}{A matrix of dimension \code{I × I} containing all the correlation coefficients of items.}
#' \item{clusters}{A list containing all the clusters.}
#' \item{heights}{A vector containing all the heights of the cluster tree. The heights are arranged in descending order.}
#' \item{nfact.SOD}{The number of factors to be retained by the Second-Order Difference (SOD) approach.}
#'
#' @details
#' Hierarchical cluster analysis always merges the two nodes with the smallest dissimilarity,
#' forming a new node in the process. This continues until all nodes are merged into one
#' large node, at which point the algorithm terminates. This method undoubtedly creates a hierarchical
#' structure by the end of the process, which encompasses the relationships between all items:
#' items with high correlation have short connecting lines between them, while items with low correlation
#' have longer lines. This hierarchical structure is well-suited to be represented as a binary tree.
#' In this representation, the dissimilarity between two nodes can be indicated by the height of the
#' tree nodes; the greater the difference between nodes, the higher the height of the tree nodes connecting
#' them (the longer the line). Researchers can decide whether two nodes belong to the same cluster
#' based on the height differences between nodes, which, in exploratory factor analysis, represents
#' whether these two nodes belong to the same latent factor.
#'
#' The Second-Order Difference (SOD) approach is a commonly used method for finding the "elbow"
#' (the point of greatest slope change). According to the principles of exploratory factor analysis,
#' items belonging to different latent factors have lower correlations, while items under the same
#' factor are more highly correlated. In hierarchical clustering, this is reflected in the height of
#' the nodes in the dendrogram, with differences in node heights representing the relationships between items.
#' By sorting all node heights in descending order and applying the SOD method to locate the elbow,
#' the number of factors can be determined. @seealso \link[EFAfactors]{EFAkmeans}
#'
#'
#'
#' @references
#' Batagelj, V. (1988). Generalized Ward and Related Clustering Problems. In H. H. Bock, Classification and Related Methods of Data Analysis the First Conference of the International Federation of Classification Societies (IFCS), Amsterdam.
#'
#' Murtagh, F., & Legendre, P. (2014). Ward’s Hierarchical Agglomerative Clustering Method: Which Algorithms Implement Ward’s Criterion? Journal of Classification, 31(3), 274-295. https://doi.org/10.1007/s00357-014-9161-z.
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
#' ## Run EFAhclust function with default parameters.
#' \donttest{
#' EFAhclust.obj <- EFAhclust(response)
#'
#' plot(EFAhclust.obj)
#'
#' ## Get the heights.
#' heights <- EFAhclust.obj$heights
#' print(heights)
#'
#' ## Get the nfact retained by SOD
#' nfact.SOD <- EFAhclust.obj$nfact.SOD
#' print(nfact.SOD)
#' }
#'
#'
#'
#' @importFrom proxy dist as.dist
#' @importFrom stats cor hclust cutree
#'
#' @export
#'
EFAhclust <- function(response, dissimilarity.type = "R", method = "ward.D",
                      cor.type = "pearson", use = "pairwise.complete.obs",
                      nfact.max = 10,
                      plot = TRUE){

  if(!any(rep(dissimilarity.type, 2) == c("R", "E")))
    stop("'type' must be one of the strings 'R' or 'E' !")

  if(!any(rep(method, 8) == c('ward.D', 'ward.D2', 'single', 'complete', 'average', 'mcquitty', 'median', 'centroid')))
    stop("'type' must be one of the strings 'ward.D', 'ward.D2', 'single', 'complete', 'average', 'mcquitty', 'median' or 'centroid' !")

  if(!any(rep(use, 5) == c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs")))
    stop("'use' must be one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete', or 'pairwise.complete.obs' !")

  response <- scale(response)
  cor.response <- cor(response, method = cor.type, use = use)

  if(dissimilarity.type == "R"){
    distance.matrix <- as.dist(1 - abs(cor.response))
  }else if(dissimilarity.type == "E"){
    distance.matrix <- as.dist(as.matrix(dist(t(response), method = "Euclidean"))^2)
  }

  hc <- hclust(distance.matrix, method = method)
  heights <- sort(hc$height, decreasing = TRUE)

  clusters <- list()
  for(cut_height in heights){
    clusters.cur <- cutree(hc, h = cut_height)
    clusters.order <- unique(clusters.cur)
    clusters.items <- list()
    for(co in clusters.order)
      clusters.items[[co]] <- which(clusters.cur == co)
    clusters[[length(clusters.order)]] <- clusters.items
  }

  nfact.SOD <- which.max(diff(diff(heights[1:min(length(hc$height), nfact.max + 1)]))) + 1
  if(length(nfact.SOD) == 0)
    nfact.SOD <- 1

  EFAhclust.obj <- list(hc=hc, cor.response=cor.response, clusters=clusters, heights=heights, nfact.SOD=nfact.SOD)
  class(EFAhclust.obj) <- "EFAhclust"

  if(plot) plot(EFAhclust.obj)

  return(EFAhclust.obj)
}
