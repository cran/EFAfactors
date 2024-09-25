#'
#' Load the Scaler for the Pre-Trained Deep Neural Network (DNN)
#'
#' @description
#' Loads the scaler object within the \code{EFAfactors} package. This object is a \code{list} containing a mean vector and
#' a standard deviation vector, which were computed from the 10,000,000 datasets \link[EFAfactors]{data.datasets}
#' used for training the Pre-Trained Deep Neural Network (DNN). It serves as a tool for normalizing features in
#' \link[EFAfactors]{DNN_predictor}.
#' @seealso \link[EFAfactors]{DNN_predictor}, \link[EFAfactors]{normalizor}, \link[EFAfactors]{data.datasets}, \link[EFAfactors]{data.scaler}
#'
#' @return scaler objective.
#'
#' @examples
#' library(EFAfactors)
#'
#' scaler <- load_scaler()
#' print(scaler)
#'
#'
#' @export
#'
load_scaler <- function() {
  scaler_path <- system.file("data", "data.scaler.rda", package = "EFAfactors")

  local_env <- new.env()
  load(scaler_path, envir = local_env)
  data.scaler <- get(ls(local_env)[1], envir = local_env)

  return(data.scaler)
}

#'
#' Load the Tuned XGBoost Model
#'
#'
#' @description
#' Loads the tuned XGBoost model object within the \code{EFAfactors} package
#' into the global environment and retrieves it for use. Only the core model is retained to reduce the size.
#'
#' @return The tuned XGBoost model object
#'
#' @examples
#' library(EFAfactors)
#'
#' xgb_model <- load_xgb()
#' print(xgb_model)
#'
#'
#' @export
#'
load_xgb <- function() {
  xgb_path <- system.file("data", "model.xgb.rda", package = "EFAfactors")

  local_env <- new.env()
  load(xgb_path, envir = local_env)
  model.xgb <- get(ls(local_env)[1], envir = local_env)

  return(model.xgb)
}

#'
#' Load the Trained Deep Neural Network (DNN)
#'
#' @description
#' Loads the Pre-Trained Deep Neural Network (DNN) from the \code{DNN.onnx}.
#' The function uses the \code{reticulate} package to import the \code{onnxruntime} Python library
#' and create an inference session for the model.
#' @seealso \link[EFAfactors]{DNN_predictor}
#'
#' @return An ONNX runtime inference session object for the DNN model.
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
#'
#' @export
#' @import reticulate
#' @importFrom reticulate import
#'
load_DNN <- function() {
  DNN_path <- system.file("extdata", "DNN.onnx", package = "EFAfactors")
  onnxruntime <- import("onnxruntime")
  DNN <- onnxruntime$InferenceSession(DNN_path)

  return(DNN)
}
