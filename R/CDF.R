#'
#' the  Comparison Data Forest (CDF) Approach
#'
#' @description
#' The Comparison Data Forest (CDF; Goretzko & Ruscio, 2019) approach is a combination of Random Forest with the comparison data (CD) approach.
#'
#'
#'
#' @param response A required \code{N} × \code{I} matrix or data.frame consisting of the responses of \code{N} individuals
#'          to × \code{I} items.
#' @param num.trees the number of trees in the Random Forest. (default = 500) See details.
#' @param mtry the maximum depth for each tree, can be a number or a character (\code{"sqrt"}).
#'          When \code{mtry = "sqrt"}, it means that the maximum depth of each tree will be determined by the square root of
#'          the number of available features (converted to an integer by \link[base]{round}).default = \code{"sqrt"}. See details.
#' @param nfact.max The maximum number of factors discussed by CD approach. (default = 10)
#' @param N.pop Size of finite populations of simulating.. (default = 10,000)
#' @param N.Samples Number of samples drawn from each population. (default = 500)
#' @param cor.type A character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default),
#'          "kendall", or "spearman". @seealso \link[stats]{cor}.
#' @param use an optional character string giving a method for computing covariances in the presence of missing values. This
#'          must be one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs" (default).
#'          @seealso \link[stats]{cor}.
#' @param vis A Boolean variable that will print the factor retention results when set to TRUE, and will not print
#'          when set to FALSE. (default = TRUE)
#' @param plot A Boolean variable that will print the CDF plot when set to TRUE, and will not print it when set to
#'          FALSE. @seealso \link[EFAfactors]{plot.CDF}. (Default = TRUE)
#'
#'
#' @return An object of class \code{CDF} is a \code{list} containing the following components:
#' \item{nfact}{The number of factors to be retained.}
#' \item{RF}{the trained Random Forest model}
#' \item{probability}{A matrix containing the probabilities for factor numbers ranging from 1
#'                    to nfact.max (1xnfact.max), where the number in the f-th column represents the probability
#'                    that the number of factors for the response is f.}
#' \item{features}{A matrix (1×181) containing all the features for determining the number of
#'       factors. @seealso \link[EFAfactors]{extractor.feature.FF}}
#'
#'
#'
#' @details
#' The Comparison Data Forest (CDF; Goretzko & Ruscio, 2019) Approach is a combination of
#' random forest with the comparison data (CD) approach. Its basic steps involve using the method
#' of Ruscio & Roche (2012) to simulate data with different factor counts, then extracting features
#' from this data to train a random forest model. Once the model is trained, it can be used to
#' predict the number of factors in empirical data. The algorithm consists of the following steps:
#'
#'1. **Simulation Data:**
#'
#' \describe{
#'    \item{(1)}{For each value of \eqn{nfact} in the range from 1 to \eqn{nfact_{max}},
#'               generate a population data using the \link[EFAfactors]{GenData} function.}
#'    \item{(2)}{Each population is based on \eqn{nfact} factors and consists of \eqn{N_{pop}} observations.}
#'    \item{(3)}{For each generated population, repeat the following for \eqn{N_{rep}} times, For the \eqn{j}-th in \eqn{N_{rep}}:
#'                a. Draw a sample \eqn{N_{sam}} from the population that matches the size of the empirical data;
#'                b. Compute a feature set \eqn{\mathbf{fea}_{nfact,j}} from each \eqn{N_{sam}}.}
#'    \item{(4)}{Combine all the generated feature sets \eqn{\mathbf{fea}_{nfact,j}}
#'               into a data frame as \eqn{\mathbf{data}_{train, nfact}}.}
#'    \item{(5)}{Combine all \eqn{\mathbf{data}_{train, nfact}} into a final data frame as the training dataset \eqn{\mathbf{data}_{train}}.}
#' }
#'
#' 2. **Training RF:**
#'
#'    Train a Random Forest model \eqn{RF} using the combined \eqn{\mathbf{data}_{train}}.
#'
#' 3. **Prediction the Empirical Data:**
#'
#' \describe{
#'    \item{(1)}{Calculate the feature set \eqn{\mathbf{fea}_{emp}}for the empirical data.}
#'    \item{(2)}{Use the trained Random Forest model \eqn{RF} to predict the number of factors \eqn{nfact_{emp}} for the empirical data:
#'               \deqn{nfact_{emp} = RF(\mathbf{fea}_{emp})}}
#' }
#'
#' According to Goretzko & Ruscio (2024) and Breiman (2001), the number of
#' trees in the Random Forest \code{num.trees} is recommended to be 500.
#' The Random Forest in CDF performs a classification task, so the recommended maximum
#' depth for each tree \code{mtry} is \eqn{\sqrt{q}} (where \eqn{q} is the number of features),
#' which results in \eqn{m_{try}=\sqrt{181}=13}.
#'
#' Since the CDF approach requires extensive data simulation and computation, which is much more time consuming
#' than the \link[EFAfactors]{CD} Approach, C++ code is used to speed up the process.
#' @seealso \link[EFAfactors]{GenData}
#'
#'
#' @references
#' Breiman, L. (2001). Random Forests. Machine Learning, 45(1), 5-32. https://doi.org/10.1023/A:1010933404324
#'
#' Goretzko, D., & Ruscio, J. (2024). The comparison data forest: A new comparison data approach to determine the number of factors in exploratory factor analysis. Behavior Research Methods, 56(3), 1838-1851. https://doi.org/10.3758/s13428-023-02122-4
#'
#' Ruscio, J., & Roche, B. (2012). Determining the number of factors to retain in an exploratory factor analysis using comparison data of known factorial structure. Psychological Assessment, 24, 282–292. http://dx.doi.org/10.1037/a0025697.
#'
#' @author Haijiang Qin <Haijiang133@outlook.com>
#'
#'
#'
#' @export
#'
#' @import ranger
#' @importFrom ranger ranger
#'
#'
CDF <- function(response,
                num.trees = 500, mtry = "sqrt",
                nfact.max=10, N.pop = 10000, N.Samples = 500,
                cor.type = "pearson", use = "pairwise.complete.obs",
                vis=TRUE, plot = TRUE){

  N <- dim(response)[1]
  I <- dim(response)[2]
  response <- scale(as.matrix(response))

  features.empirical <- extractor.feature.FF(response, cor.type = cor.type, use = use)
  feature_names <- colnames(features.empirical)
  datasets <- matrix(0, nrow = nfact.max * N.Samples, ncol = length(feature_names) + 1)

  for (nfact in 1:nfact.max) {
    Pop <- GenData(response, nfact = nfact, N.pop = N.pop, cor.type = cor.type, use = use)
    posi <- (nfact - 1) * N.Samples
    datasets[(posi+1):((posi+N.Samples)), ] <-
      t(sapply(1:N.Samples, function(ns) N.Samples.loop(ns, cor.type, use, Pop, N, posi, nfact, nfact.max, N.Samples, vis)))
  }
  if(vis){
    cat("\n")
  }

  datasets <- as.data.frame(datasets)
  colnames(datasets) <- c(feature_names, "nfact")
  datasets$nfact <- factor(datasets$nfact)

  fea.null <- which(apply(datasets[, -ncol(datasets)], 2, function(col) length(unique(col)) == 1))
  if (length(fea.null) > 0) {
    datasets[, fea.null] <- NULL
  }
  if(mtry == "sqrt")
    mtry <- round(sqrt(ncol(datasets)))

  RF <- ranger(nfact ~ ., data = datasets, num.trees = num.trees, mtry = mtry, probability = TRUE)
  pred <- predict(RF, data = features.empirical)
  probability <- as.numeric(pred$predictions)
  nfact <- which.max(probability)[1]

  CDF.obj <- list(nfact=nfact, RF=RF, probability=probability, features=features.empirical)
  class(CDF.obj) <-"CDF"

  if(vis){
    cat("\n")
    print(CDF.obj)
  }
  if(plot) plot(CDF.obj)

  return(CDF.obj)
}

N.Samples.loop <- function(ns, cor.type, use, Pop, N, posi, nfact, nfact.max, N.Samples, vis){
  data.cur <- Pop[sample(1:nrow(Pop), N, replace = TRUE), ]
  features <- extractor.feature.FF(data.cur, cor.type = cor.type, use = use)

  if (vis) {
    cat("\rCDF is simulating data:",
        paste0("nfact=", sprintf("%2d", nfact), "/", nfact.max),
        "-", paste0("N_rep=", sprintf("%4d", ns), "/", N.Samples))
  }
  return(unlist(c(features, nfact)))
}
