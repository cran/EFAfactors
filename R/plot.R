#' Plot Hull Plot for Factor Analysis
#'
#' @description
#' This function creates a Hull plot to visualize the relationship between
#' the Comparative Fit Index (CFI) and the degrees of freedom (df) for a
#' range of models with different numbers of factors. The Hull plot helps
#' in assessing model fit and identifying optimal models.
#' @seealso \link[EFAfactors]{Hull}
#'
#'
#' @param x An object of class \code{Hull}, representing the results to be plotted.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (plotting).
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
#' \donttest{
#'  Hull.obj <- CD(response)
#'
#'  ## Hull plot
#'  plot(Hull.obj)
#'
#' }
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics plot abline axis barplot legend lines mtext par pie points text
#' @export
plot.Hull <- function(x, ...) {
  obj <- x

  nfact <- obj$nfact
  CFI <- obj$CFI
  df <- obj$df
  Hull.CFI <- obj$Hull.CFI
  Hull.df <- obj$Hull.df

  index <- obj$index
  if(nfact != 0){
    posi <- match(Hull.CFI, CFI)
    # par(mar = c(5, 4, 6, 4))
    plot(Hull.df, Hull.CFI,
         type = "o",
         xlab = "Degree of freedom (parameters)",
         ylab = "Comparative Fit Index",
         pch = 2,
         col = "blue")

    points(df[-posi], CFI[-posi], pch = 1, col = "black")
    if(nfact >= 1)
      points(df[posi[1:which(posi - 1 == nfact)]], CFI[posi[1:which(posi - 1 == nfact)]], pch = 17, col = "blue")

    axis(3, at = df, labels = c(0:(length(df)-1)), tick = TRUE)
    mtext("Hull plot", side = 3, line = 4, cex = 1.5)
    mtext("Number of Factors", side = 3, line = 2)
  }else{
    message("Hull exception !")
  }

}

#' Plot Comparison Data for Factor Analysis
#'
#' @description
#' This function generates a Comparison Data plot to visualize the Root Mean Square Error (RMSE) of
#' eigenvalues for various numbers of factors. This plot helps in evaluating the fit of different
#' factor models and identifying the optimal number of factors based on RMSE values.
#' @seealso \link[EFAfactors]{CD}
#'
#'
#' @param x An object of class \code{CD}, representing the results to be plotted.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (plotting).
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
#' \donttest{
#' CD.obj <- CD(response)
#'
#' ## CD plot
#' plot(CD.obj)
#'
#' }
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics plot abline axis barplot legend lines mtext par pie points text
#' @export
plot.CD <- function(x, ...) {
  obj <- x

  Sig <- obj$Sig
  RMSE.Eigs <- obj$RMSE.Eigs
  nfact <- obj$nfact

  x.max <- ifelse(Sig, nfact, nfact + 1)
  ys <- apply(RMSE.Eigs[, 1:x.max], 2, mean)

  plot(x = 1:x.max, y = ys, ylim = c(0, max(ys)),
       xlab = "Number of Factors", ylab = "RMSE Eigenvalue", type = "o",
       main = "Comparison Data",
       pch = 2,
       col = "blue")

  if(nfact >= 1)
    points(1:nfact, ys[1:nfact], pch = 17, col = "blue")

}

#' Plot Parallel Analysis Scree Plot
#'
#' @description
#' This function creates a Parallel Analysis (PA) scree plot to compare the eigenvalues of the actual
#' data with the eigenvalues from simulated data. The plot helps in determining the number of factors
#' by visualizing where the eigenvalues of the actual data intersect with those from simulated data.
#' It provides a graphical representation of the results from a parallel analysis to aid in factor selection.
#' @seealso \link[EFAfactors]{PA}
#'
#' @param x An object of class \code{PA}, representing the results to be plotted.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (plotting).
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
#' \donttest{
#'  PA.obj <- PA(response)
#'
#'  ## PA plot
#'  plot(PA.obj)
#'
#' }
#'
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics plot abline axis barplot legend lines mtext par pie points text
#' @export
plot.PA <- function(x, ...) {
  obj <- x

  nfact <- obj$nfact
  eigen.value <- obj$eigen.value
  eigen.ref <- obj$eigen.ref
  type <- obj$type
  fa <- obj$fa
  if(fa == "pc"){
    fa <- "Principal Component Analysis"
  }else{
    fa <- "Factor Analysis"
  }

  if(type == "quant"){
    type = paste0(type, "=", obj$quant)
  }

  x_axis <- 1:length(eigen.value)
  plot(x_axis, eigen.value, type = "o", col = "blue",
       ylim = range(c(eigen.value, eigen.ref)),
       pch = 2,
       xlab = "Number of Factors", ylab = paste0("Eigenvalue of ", fa), main = paste("Parallel Analysis Scree Plots", paste0("(", type, ")")))

  lines(x_axis, eigen.ref, col = "red", lty = 2)
  if(nfact >= 1)
    points(1:nfact, eigen.value[1:nfact], pch = 17, col = "blue")

  legend("topright", legend = c("Actual Data", "Simulated Data"),
         col = c("blue", "red"), pch = c(17, 46), lty = 1:2)
  abline(h = 1, col = "black", lty = 2)

}

#' Plot Empirical Kaiser Criterion (EKC) Plot
#'
#' @description
#' This function generates an Empirical Kaiser Criterion (EKC) plot to visualize the eigenvalues
#' of the actual data. The EKC method helps in determining the number of factors to retain by
#' identifying the point where the eigenvalues exceed the reference eigenvalue.
#' The plot provides a graphical representation to assist in factor selection.
#' @seealso \link[EFAfactors]{EKC}
#'
#' @param x An object of class \code{EKC}, representing the results to be plotted.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (plotting).
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
#' \donttest{
#'  EKC.obj <- EKC(response)
#'
#'  ## EKC plot
#'  plot(EKC.obj)
#'
#' }
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics plot abline axis barplot legend lines mtext par pie points text
#' @export
plot.EKC <- function(x, ...) {
  obj <- x

  nfact <- obj$nfact
  eigen.value <- obj$eigen.value
  eigen.ref <- obj$eigen.ref

  x_axis <- 1:length(eigen.value)
  plot(x_axis, eigen.value, type = "o", col = "blue",
       ylim = range(c(eigen.value, eigen.ref)),
       pch = 2,
       xlab = "Number of Factors", ylab = "Eigenvalue", main = "Empirical Kaiser Criterion")

  lines(x_axis, eigen.ref, col = "red", lty = 2)
  if(nfact >= 1)
    points(1:nfact, eigen.value[1:nfact], pch = 17, col = "blue")

  legend("topright", legend = c("Actual Eigenvalue", "Reference Eigenvalue"),
         col = c("blue", "red"), pch = c(17, 46), lty = 1:2)

}

#'Plot Kaiser-Guttman Criterion (KGC) Plot
#'
#' @description
#' This function generates a Kaiser-Guttman Criterion (KGC) plot to visualize the eigenvalues
#' of the actual data. The Kaiser-Guttman Criterion, also known as the Kaiser criterion,
#' suggests retaining factors with eigenvalues greater than 1. The plot shows the eigenvalues
#' and includes a reference line at 1 to indicate the threshold for factor retention.
#' @seealso \link[EFAfactors]{KGC}
#'
#'
#' @param x An object of class \code{KGC}, representing the results to be plotted.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (plotting).
#'
#'
#' @examples
#' library(EFAfactors)
#' set.seed(123)
#'
#' ## Take the data.bfi dataset as an example.
#' data(data.bfi)
#'
#' response <- as.matrix(data.bfi[, 1:25]) ## Load data
#' response <- na.omit(response) ## Remove samples with NA/missing values
#'
#' ## Transform the scores of reverse-scored items to normal scoring
#' response[, c(1, 9, 10, 11, 12, 22, 25)] <- 6 - response[, c(1, 9, 10, 11, 12, 22, 25)] + 1
#'
#' \donttest{
#'   KGC.obj <- KGC(response)
#'
#'   ## Plot the Kaiser-Guttman Criterion
#'   plot(KGC.obj)
#' }
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics plot abline axis barplot legend lines mtext par pie points text
#' @export
plot.KGC <- function(x, ...) {
  obj <- x

  eigen.value <- obj$eigen.value
  nfact <- obj$nfact

  x_axis <- 1:length(eigen.value)
  plot(x_axis, eigen.value, type = "o", col = "blue",
       ylim = range(c(eigen.value)),
       pch = 2,
       xlab = "Number of Factors", ylab = "Eigenvalue", main = "Kaiser-Guttman Criterion")

  abline(h = 1, col = "black", lty = 2)

  if(nfact >= 1)
    points(1:nfact, eigen.value[1:nfact], pch = 17, col = "blue")

}

#'
#' Plot EFA K-means Clustering Results
#'
#' @description
#' This function creates a plot to visualize the Within-cluster Sum of Squares (WSS) for different numbers of clusters (K)
#' in the context of exploratory factor analysis. The plot helps identify the most appropriate number of factors by showing
#' how WSS decreases as the number of factors (or clusters) increases.
#' @seealso \link[EFAfactors]{EFAkmeans}
#'
#' @param x An object of class \code{EFAkmeans}, representing the results to be plotted.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (plotting).
#'
#' @examples
#' library(EFAfactors)
#' set.seed(123)
#'
#' ## Take the data.bfi dataset as an example.
#' data(data.bfi)
#'
#' response <- as.matrix(data.bfi[, 1:25]) ## Load data
#' response <- na.omit(response) ## Remove samples with NA/missing values
#'
#' ## Transform the scores of reverse-scored items to normal scoring
#' response[, c(1, 9, 10, 11, 12, 22, 25)] <- 6 - response[, c(1, 9, 10, 11, 12, 22, 25)] + 1
#'
#' \donttest{
#'   EFAkmeans.obj <- EFAkmeans(response)
#'
#'   ## Plot the EFA K-means clustering results
#'   plot(EFAkmeans.obj)
#' }
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics plot abline axis barplot legend lines mtext par pie points text
#' @export
plot.EFAkmeans <- function(x, ...) {
  obj <- x

  wss <- obj$wss
  x_axis <- 1:length(wss)
  plot(x_axis, wss, type = "o", col = "blue",
       ylim = range(c(wss)),
       pch = 17,
       xlab = "Number of Factors", ylab = "Within-cluster Sum of Squares", main = "EFAkmeans")

}

#' Plot Hierarchical Cluster Analysis Dendrogram
#'
#' @description
#' This function generates a dendrogram from hierarchical cluster analysis results. The hierarchical clustering method
#' merges the two nodes with the smallest dissimilarity at each step, forming a new node until all nodes are combined
#' into a single hierarchical structure. The resulting dendrogram represents the hierarchical relationships between items,
#' where items with high correlation are connected by shorter lines, and items with low correlation are connected by longer lines.
#' The height of the tree nodes indicates the dissimilarity between nodes: a greater height reflects a larger difference.
#' Researchers can use this representation to determine if two nodes belong to the same cluster, which in exploratory factor analysis,
#' helps identify whether items belong to the same latent factor.
#' @seealso \link[EFAfactors]{EFAhclust}
#'
#' @param x An object of class \code{EFAhclust}, representing the results to be plotted.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (plotting).
#'
#' @examples
#' library(EFAfactors)
#' set.seed(123)
#'
#' ## Take the data.bfi dataset as an example.
#' data(data.bfi)
#'
#' response <- as.matrix(data.bfi[, 1:25]) ## Load data
#' response <- na.omit(response) ## Remove samples with NA/missing values
#'
#' ## Transform the scores of reverse-scored items to normal scoring
#' response[, c(1, 9, 10, 11, 12, 22, 25)] <- 6 - response[, c(1, 9, 10, 11, 12, 22, 25)] + 1
#'
#' \donttest{
#'   EFAhclust.obj <- EFAhclust(response)
#'
#'   ## Plot the hierarchical clustering dendrogram
#'   plot(EFAhclust.obj)
#' }
#'
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics plot abline axis barplot legend lines mtext par pie points text
#' @export
plot.EFAhclust <- function(x, ...) {
  obj <- x

  hc <- obj$hc
  plot(hc, main = "Clusters of Items", xlab = "Items", ylab = "Distance")

}

#' Plot DNN Predictor Classification Probability Distribution
#'
#' @description
#' This function generates a bar plot of the classification probabilities predicted by the pre-trained deep neural network
#' for determining the number of factors. The plot displays the probability distribution across different numbers of factors,
#' with each bar representing the probability for a specific number of factors. The maximum number of factors that the network
#' can evaluate is 10. The function also annotates each bar with its probability value.
#' @seealso \link[EFAfactors]{DNN_predictor}
#'
#' @param x An object of class \code{DNN_predictor}, representing the results to be plotted.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (plotting).
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics plot abline axis barplot legend lines mtext par pie points text
#' @export
plot.DNN_predictor <- function(x, ...) {
  obj <- x

  probability <- obj$probability
  bp <- barplot(probability,
                beside = TRUE,
                col = gray.colors(ncol(probability)),
                xlab = "Number of Factor",
                ylab = "Probability",
                main = "Probability Distribution of the Number of Factors",
                ylim = c(0, max(probability) * 1.1),
                las = 2)

  text(x = bp, y = probability, label = round(probability, digits = 3),
       pos = 3, cex = 0.8, col = "black")
  axis(1, at = bp, labels = 1:ncol(probability))

}

#' Plot Factor Forest (FF) Classification Probability Distribution
#'
#' @description
#' This function generates a bar plot of the classification probabilities predicted by the Factor Forest
#' for determining the number of factors. The plot displays the probability distribution across different numbers of factors,
#' with each bar representing the probability for a specific number of factors. Unlike the deep neural network (DNN) model,
#' the Factor Forest can evaluate up to a maximum of 8 factors. The function also annotates each bar with its probability value.
#' @seealso \link[EFAfactors]{FF}
#'
#' @param x An object of class \code{FF}, representing the results to be plotted.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (plotting).
#'
#' @examples
#' library(EFAfactors)
#' set.seed(123)
#'
#' ## Take the data.bfi dataset as an example.
#' data(data.bfi)
#'
#' response <- as.matrix(data.bfi[, 1:25]) ## Load data
#' response <- na.omit(response) ## Remove samples with NA/missing values
#'
#' ## Transform the scores of reverse-scored items to normal scoring
#' response[, c(1, 9, 10, 11, 12, 22, 25)] <- 6 - response[, c(1, 9, 10, 11, 12, 22, 25)] + 1
#'
#' \donttest{
#' FF.obj <- FF(response)
#'
#' ## Plot the FF probabilities
#' plot(FF.obj)
#' }
#'
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics plot abline axis barplot legend lines mtext par pie points text
#' @export
plot.FF <- function(x, ...) {
  obj <- x

  probability <- obj$probability
  bp <- barplot(probability,
                beside = TRUE,
                col = gray.colors(ncol(probability)),
                xlab = "Number of Factor",
                ylab = "Probability",
                main = "Probability Distribution of the Number of Factors",
                ylim = c(0, max(probability) * 1.1),
                las = 2)

  text(x = bp, y = probability, label = round(probability, digits = 3),
       pos = 3, cex = 0.8, col = "black")
  axis(1, at = bp, labels = 1:length(probability))

}


#' Plot Comparison Data Forest (CDF) Classification Probability Distribution
#'
#' @description
#' This function generates a bar plot of the classification probabilities predicted by the Comparison Data Forest
#' for determining the number of factors. The plot displays the probability distribution across different numbers of factors,
#' with each bar representing the probability for a specific number of factors.
#' @seealso \link[EFAfactors]{CDF}
#'
#' @param x An object of class \code{CDF}, representing the results to be plotted.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (plotting).
#'
#' @examples
#' library(EFAfactors)
#' set.seed(123)
#'
#' ## Take the data.bfi dataset as an example.
#' data(data.bfi)
#'
#' response <- as.matrix(data.bfi[, 1:25]) ## Load data
#' response <- na.omit(response) ## Remove samples with NA/missing values
#'
#' ## Transform the scores of reverse-scored items to normal scoring
#' response[, c(1, 9, 10, 11, 12, 22, 25)] <- 6 - response[, c(1, 9, 10, 11, 12, 22, 25)] + 1
#'
#' \donttest{
#' CDF.obj <- CDF(response)
#'
#' ## Plot the CDF probabilities
#' plot(CDF.obj)
#' }
#'
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics plot abline axis barplot legend lines mtext par pie points text
#' @export
plot.CDF <- function(x, ...) {
  obj <- x

  probability <- obj$probability
  bp <- barplot(probability,
                beside = TRUE,
                col = gray.colors(ncol(probability)),
                xlab = "Number of Factor",
                ylab = "Probability",
                main = "Probability Distribution of the Number of Factors",
                ylim = c(0, max(probability) * 1.1),
                las = 2)

  text(x = bp, y = probability, label = round(probability, digits = 3),
       pos = 3, cex = 0.8, col = "black")
  axis(1, at = bp, labels = 1:length(probability))

}

#' Plot Voting Results for Number of Factors
#'
#' @description
#' This function creates a pie chart to visualize the results of a voting method used to
#' determine the number of factors in exploratory factor analysis (EFA). The voting method combines
#' the results from multiple EFA techniques, and the pie chart displays the proportions of votes
#' each number of factors received. Each slice of the pie represents the percentage of votes
#' for a specific number of factors, providing a clear visual representation of the most
#' commonly suggested number of factors.
#' @seealso \link[EFAfactors]{EFAvote}
#'
#' @param x An object of class \code{EFAvote}, representing the results to be plotted.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (plotting).
#'
#'
#' @examples
#' library(EFAfactors)
#'
#' \donttest{
#' nfacts <- c(5, 5, 5, 6, 6, 4)
#' names(nfacts) <- c("Hull", "CD", "PA", "EKC", "XGB","DNN")
#'
#' EFAvote.obj <- EFAvote(votes = nfacts)
#' plot(EFAvote.obj)
#' }
#'
#'
#' @importFrom grDevices gray.colors
#' @importFrom graphics plot abline axis barplot legend lines mtext par pie points text
#' @export
plot.EFAvote <- function(x, ...) {
  obj <- x

  votes <- obj$votes
  freq_table <- table(votes)
  proportions <- prop.table(freq_table)
  labels <- paste(round(proportions * 100, 2), "%")

  pie(freq_table,
      labels = labels,
      main = "Number of Factors",
      col = gray.colors(length(freq_table)))

  legend("topright",
         legend = paste("Factor ", names(freq_table)),
         fill = gray.colors(length(freq_table)))

}


#' Plots the Scree Plot
#'
#' @description
#' Plots the Scree Plot from an object of class \code{EFAscreet}. The scree plot visualizes the eigenvalues
#' of the correlation matrix in descending order and helps in identifying the optimal number of factors
#' by showing where the eigenvalues start to plateau.
#'
#' @param x An object of class \code{EFAscreet}, which contains the eigenvalues from the factor analysis.
#' @param ... Additional arguments to be passed to the \code{plot} function (not used).
#'
#' @details
#' The scree plot is a graphical tool used in exploratory factor analysis. It shows the eigenvalues
#' corresponding to the factors. The number of factors is typically determined by finding the point
#' where the plot levels off ("elbow" point).
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
#' ## Run EFAscreet function with default parameters.
#' \donttest{
#'  EFAscreet.obj <- EFAscreet(response)
#'
#'  plot(EFAscreet.obj)
#'
#' }
#'
#' @return A scree plot displaying the eigenvalues against the number of factors.
#'
#' @export
#'
#' @importFrom graphics plot
plot.EFAscreet <- function(x, ...) {
  obj <- x

  eigen.value <- obj$eigen.value

  x_axis <- 1:length(eigen.value)
  plot(x_axis, eigen.value, type = "o", col = "blue",
       ylim = range(eigen.value),
       pch = 2,
       xlab = "Number of Factors", ylab = "Eigenvalue", main = "Scree Plot")
}


