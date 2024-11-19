#'
#' A Pre-Trained Deep Neural Network (DNN) for Determining the Number of Factors
#'
#' @description
#' This function will invoke a pre-trained deep neural network that can reliably
#' perform the task of determining the number of factors. The maximum number of
#' factors that the network can discuss is 10. The DNN model is implemented in Python
#' and trained on PyTorch (https://pytorch.org/) with
#' CUDA 11.8 for acceleration. After training, the DNN was saved as a \code{DNN.onnx}
#' file. The \emph{DNN_predictor} function performs inference by loading the \code{DNN.onnx}
#' file in both Python and R environments. Therefore, please note that Python (suggested >= 3.10) and the
#' libraries \code{numpy} and \code{onnxruntime} are required.
#'
#' To run this function, Python is required, along with the installation of \code{numpy} and \code{onnxruntime}. See more in Details and Note.
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
#' @param plot A Boolean variable that will print the DNN_predictor plot when set to TRUE, and will not print it when set to
#'          FALSE. @seealso \link[EFAfactors]{plot.DNN_predictor}. (Default = TRUE)
#'
#' @return An object of class \code{DNN_predictor} is a \code{list} containing the following components:
#' \item{nfact}{The number of factors to be retained.}
#' \item{features}{A matrix (1×54) containing all the features for determining the number of
#'       factors by the DNN.}
#' \item{probability}{A matrix containing the probabilities for factor numbers ranging from 1
#'                    to 10 (1x10), where the number in the f-th column represents the probability
#'                    that the number of factors for the response is f.}
#'
#' @details
#' Due to the improved performance of deep learning models with larger datasets (Chen et al., 2017),
#' a total of 10,000,000 datasets (\link[EFAfactors]{data.datasets}) were simulated
#' to extract features for training deep learning neural networks.
#' Each dataset was generated following the methods described by Auerswald & Moshagen (2019) and Goretzko & Buhner (2020),
#' with the following specifications:
#'
#' \itemize{
#'   \item Factor number: \emph{F} ~ U[1,10]
#'   \item Sample size: \emph{N} ~ U[100,1000]
#'   \item Number of variables per factor: \emph{vpf} ~ [3,20]
#'   \item Factor correlation: \emph{fc} ~ U[0.0,0.4]
#'   \item Primary loadings: \emph{pl} ~ U[0.35,0.80]
#'   \item Cross-loadings: \emph{cl} ~ U[-0.2,0.2]
#' }
#'
#' A population correlation matrix was created for each data set based on the following decomposition:
#' \deqn{\mathbf{\Sigma} = \mathbf{\Lambda} \mathbf{\Phi} \mathbf{\Lambda}^T + \mathbf{\Delta}}
#' where \eqn{\mathbf{\Lambda}} is the loading matrix, \eqn{\mathbf{\Phi}} is the factor correlation
#' matrix, and \eqn{\mathbf{\Delta}} is a diagonal matrix,
#' with \eqn{\mathbf{\Delta} = 1 - \text{diag}(\mathbf{\Lambda} \mathbf{\Phi} \mathbf{\Lambda}^T)}.
#' The purpose of \eqn{\mathbf{\Delta}} is to ensure that the diagonal elements of \eqn{\mathbf{\Sigma} } are 1.
#'
#' The response data for each subject was simulated using the following formula:
#' \deqn{X_i = L_i + \epsilon_i, \quad 1 \leq i \leq I}
#' where \eqn{L_i} follows a normal distribution \eqn{N(0, \sigma)}, representing the contribution of latent factors,
#' and \eqn{\epsilon_i} is the residual term following a standard normal distribution. \eqn{L_i} and \eqn{\epsilon_i}
#' are uncorrelated, and \eqn{\epsilon_i} and \eqn{\epsilon_j} are also uncorrelated.
#'
#' For each simulated dataset, a total of 6 types of features (which can be
#' classified into 2 types; @seealso \link[EFAfactors]{extractor.feature.DNN})
#' are extracted and compiled into a feature vector, consisting of 54 features: 8 + 8 + 8 + 10 + 10 + 10.
#' These features are as follows:
#'
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
#'
#' The DNN model is implemented in Python and trained on PyTorch (https://download.pytorch.org/whl/cu118) with
#' CUDA 11.8 for acceleration. After training, the DNN was saved as a \code{DNN.onnx} file. The \emph{DNN_predictor} function
#' performs inference by loading the \code{DNN.onnx} file in both Python and R environments.
#'
#' @note
#' Note that Python and the libraries \code{numpy} and \code{onnxruntime} are required.
#'
#' First, please ensure that Python is installed on your computer and that Python is
#' included in the system's PATH environment variable. If not,
#' please download and install it from the official website (https://www.python.org/).
#'
#' If you encounter an error when running this function stating that the \code{numpy} and \code{onnxruntime}
#' modules are missing:
#'
#'  \code{Error in py_module_import(module, convert = convert) :}
#'
#'    \code{ModuleNotFoundError: No module named 'numpy'}
#'
#' or
#'
#'  \code{Error in py_module_import(module, convert = convert) :}
#'
#'    \code{ModuleNotFoundError: No module named 'onnxruntime'}
#'
#' this means that the \code{numpy} or \code{onnxruntime} library is missing from your Python environment. If you are using Windows or macOS,
#' please run the command \code{pip install numpy} or \code{pip install onnxruntime} in Command Prompt or Windows PowerShell (Windows), or Terminal (macOS).
#' If you are using Linux, please ensure that \code{pip} is installed and use the command \code{pip install numpy} or
#' \code{pip install onnxruntime} to install the missing libraries.

#'
#' @references
#' Auerswald, M., & Moshagen, M. (2019). How to determine the number of factors to retain in exploratory factor analysis: A comparison of extraction methods under realistic conditions. Psychological methods, 24(4), 468-491. https://doi.org/10.1037/met0000200.
#'
#' Braeken, J., & van Assen, M. A. L. M. (2017). An empirical Kaiser criterion. Psychological methods, 22(3), 450-466. https://doi.org/10.1037/met0000074.
#'
#' Goretzko, D., & Buhner, M. (2020). One model to rule them all? Using machine learning algorithms to determine the number of factors in exploratory factor analysis. Psychol Methods, 25(6), 776-786. https://doi.org/10.1037/met0000262.
#'
#' @author Haijiang Qin <Haijiang133@outlook.com>
#'
#'
#' @export
#'
#' @import reticulate
#' @importFrom reticulate import
#' @importFrom reticulate py_run_string
#' @importFrom stats cor
#'
DNN_predictor <- function(response, cor.type = "pearson", use = "pairwise.complete.obs",
                          vis=TRUE, plot = TRUE){

  if(!any(rep(use, 5) == c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs")))
    stop("'use' must be one of the strings 'everything', 'all.obs', 'complete.obs', 'na.or.complete', or 'pairwise.complete.obs' !")

  model.DNN <- load_DNN()
  data.scalor <- load_scaler()

  features <- extractor.feature.DNN(response, cor.type, use)

  names(features) <- c(paste0("eigen.value", 1:10), paste0("eigen.ref", 1:10), paste0("var.account", 1:10),
                       paste0("rheight", 2:9), paste0("eheight", 2:9), paste0("wss", 2:9))

  features <- t(features)
  features[which(is.na(features))] <- -10
  normalized.features <- normalizor(features)

  py$features <- normalized.features
  py$DNN <- model.DNN
  py_run_string("
import numpy
import onnxruntime

features = features.astype(numpy.float32)
outputs = DNN.run(None, {'onnx::Gemm_0': features})
                ")

  outputs <- py$outputs[[1]]
  probability  <- af.softmax(outputs)
  nfact <- which.max(probability)

  DNN.obj <- list(nfact=nfact, features=features, probability=probability)
  class(DNN.obj) <- "DNN_predictor"

  if(vis) print(DNN.obj)
  if(plot) plot(DNN.obj)

  return(DNN.obj)
}
