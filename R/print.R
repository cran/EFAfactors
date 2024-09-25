#'
#' Print Hull Method Results
#'
#' @description
#' This function prints the number of factors suggested by the Hull method.
#' @seealso \link[EFAfactors]{Hull}
#'
#' @param x An object of class \code{Hull}, representing the results to be printed.
#' @param ... Additional arguments to be passed to the print function.
#' @return None. This function is used for side effects (printing).
#'
#' @export
print.Hull <- function(x, ...) {
  obj <- x
  cat("The number of factors suggested by Hull is", obj$nfact, ".\n")
}

#'
#' Print Comparison Data Method Results
#'
#' @description
#' This function prints the number of factors suggested by the Comparison Data (CD) method.
#' @seealso \link[EFAfactors]{CD}
#'
#'
#' @param x An object of class \code{CD}, representing the results to be printed.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (printing).
#'
#' @export
print.CD <- function(x, ...) {
  obj <- x
  cat("The number of factors suggested by CD is", obj$nfact, ".\n")
}

#'
#' Print Parallel Analysis Method Results
#'
#' @description
#' This function prints the number of factors suggested by the Parallel Analysis (PA) method.
#' @seealso \link[EFAfactors]{PA}
#'
#' @param x An object of class \code{PA}, representing the results to be printed.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (printing).
#'
#' @export
print.PA <- function(x, ...) {
  obj <- x
  type <- obj$type
  if(type == "quant"){
    type = paste0(type, "=", obj$quant)
  }
  cat("The number of factors suggested by PA ", paste0("(", type, ")"), " is", obj$nfact, ".\n")
}

#'
#' Print Empirical Kaiser Criterion Results
#'
#' @description
#' This function prints the number of factors suggested by the Empirical Kaiser Criterion (EKC).
#' @seealso \link[EFAfactors]{EKC}
#'
#' @param x An object of class \code{EKC}, representing the results to be printed.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (printing).
#'
#' @export
print.EKC <- function(x, ...) {
  obj <- x
  cat("The number of factors suggested by EKC is", obj$nfact, ".\n")
}

#'
#' Print Kaiser-Guttman Criterion Results
#'
#' @description
#' This function prints the number of factors suggested by the Kaiser-Guttman Criterion (KGC).
#' @seealso \link[EFAfactors]{KGC}
#'
#'
#' @param x An object of class \code{KGC}, representing the results to be printed.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (printing).
#'
#' @export
print.KGC <- function(x, ...) {
  obj <- x
  cat("The number of factors suggested by KGC is", obj$nfact, ".\n")
}

#'
#' Print EFAkmeans Method Results
#'
#' @description
#' This function prints the number of factors suggested by the EFAkmeans method using the Second-Order Difference (SOD) approach.
#' @seealso \link[EFAfactors]{EFAkmeans}
#'
#' @param x An object of class \code{EFAkmeans}, representing the results to be printed.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (printing).
#'
#' @export
print.EFAkmeans <- function(x, ...) {
  obj <- x
  cat("The number of factors suggested by EFAkmeans-SOD is", obj$nfact.SOD, ".\n")
}

#'
#' Print EFAhclust Method Results
#'
#' @description
#' This function prints the number of factors suggested by the EFAhclust method using the Second-Order Difference (SOD) approach.
#' @seealso \link[EFAfactors]{EFAhclust}
#'
#' @param x An object of class \code{EFAhclust}, representing the results to be printed.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (printing).
#'
#' @export
print.EFAhclust <- function(x, ...) {
  obj <- x
  cat("The number of factors suggested by EFAhclust-SOD is", obj$nfact.SOD, ".\n")
}

#'
#' Print DNN Predictor Method Results
#'
#' @description
#' This function prints the number of factors suggested by the deep neural network (DNN) predictor.
#' @seealso \link[EFAfactors]{DNN_predictor}
#'
#' @param x An object of class \code{DNN_predictor}, representing the results to be printed.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (printing).
#'
#' @export
print.DNN_predictor <- function(x, ...) {
  obj <- x
  cat("The number of factors suggested by DNN is", obj$nfact, ".\n")
}

#'
#' Print Factor Forest (FF) Results
#'
#' @description
#' This function prints the number of factors suggested by the Factor Forest.
#' @seealso \link[EFAfactors]{FF}
#'
#' @param x An object of class \code{FF}, representing the results to be printed.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (printing).
#'
#' @export
print.FF <- function(x, ...) {
  obj <- x
  cat("The number of factors suggested by FF is", obj$nfact, ".\n")
}

#'
#' Print Comparison Data Forest (CDF) Results
#'
#' @description
#' This function prints the number of factors suggested by the Comparison Data Forest.
#' @seealso \link[EFAfactors]{CDF}
#'
#' @param x An object of class \code{CDF}, representing the results to be printed.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (printing).
#'
#' @export
print.CDF <- function(x, ...) {
  obj <- x
  cat("The number of factors suggested by CDF is", obj$nfact, ".\n")
}

#'
#' Print Voting Method Results
#'
#' @description
#' This function prints the number of factors suggested by the voting.
#' @seealso \link[EFAfactors]{EFAvote}
#'
#' @param x An object of class \code{EFAvote}, representing the results to be printed.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (printing).
#'
#' @export
print.EFAvote <- function(x, ...) {
  obj <- x
  cat("The number of factors suggested by EFAvote is", obj$nfact, ".\n")
}

#'
#' Print the EFAsim.data
#'
#' @description
#' This function prints the items in factors.
#' @seealso \link[EFAfactors]{EFAsim.data}
#'
#' @param x An object of class \code{EFA.data}, representing the results to be printed.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (printing).
#'
#' @export
print.EFAdata <- function(x, ...) {
  obj <- x
  print(obj$items)
}

#'
#' Print the Scree Plot
#'
#' @description
#' Prints a brief summary of an object of class \code{EFAscreet}. This function will display the scree plot
#' of the eigenvalues when called, providing a visual representation of the factor analysis results.
#'
#' @param x An object of class \code{EFAscreet}, which contains the eigenvalues from the factor analysis.
#' @param ... Additional arguments to be passed to the plotting function.
#' @return None. This function is used for side effects (printing).
#'
#' @export
print.EFAscreet <- function(x, ...) {
  plot(x)
}
