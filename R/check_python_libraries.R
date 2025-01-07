#' Check and Install Python Libraries (numpy and onnxruntime)
#'
#' @description
#' This function checks whether the Python libraries `numpy` and `onnxruntime` are installed. If not, it will prompt
#' the user to decide whether to install them. If the user chooses 'y', the required library will be installed using
#' the `reticulate` package. If the user chooses 'n', the installation will be skipped. @seealso \link[EFAfactors]{DNN_predictor}
#'
#' @return A list indicating whether `numpy` and `onnxruntime` are installed.
#' The list contains the following logical elements:
#' \item{numpy_installed}{TRUE if `numpy` is installed, FALSE otherwise.}
#' \item{onnxruntime_installed}{TRUE if `onnxruntime` is installed, FALSE otherwise.}
#'
#' @examples
#' # Check and install necessary Python libraries
#' check_python_libraries()
#'
#' @import reticulate
#' @export
check_python_libraries <- function() {

  # Check if 'numpy' is installed in the Python environment
  numpy_installed <- py_module_available("numpy")

  # Check if 'onnxruntime' is installed in the Python environment
  onnxruntime_installed <- py_module_available("onnxruntime")

  # If 'numpy' is not installed, prompt user for installation
  if (!numpy_installed) {
    message("The 'numpy' library is not installed.")
    install_numpy <- readline(prompt = "Do you want to install 'numpy'? (y/n): ")
    if (tolower(install_numpy) == "y") {
      py_install("numpy")
      message("numpy has been installed.")
    } else {
      message("numpy installation skipped.")
    }
  }

  # If 'onnxruntime' is not installed, prompt user for installation
  if (!onnxruntime_installed) {
    message("The 'onnxruntime' library is not installed.")
    install_onnxruntime <- readline(prompt = "Do you want to install 'onnxruntime'? (y/n): ")
    if (tolower(install_onnxruntime) == "y") {
      py_install("onnxruntime")
      message("onnxruntime has been installed.")
    } else {
      message("onnxruntime installation skipped.")
    }
  }

  # Return a list indicating the installation status of numpy and onnxruntime
  return(list(numpy_installed = numpy_installed, onnxruntime_installed = onnxruntime_installed))
}
