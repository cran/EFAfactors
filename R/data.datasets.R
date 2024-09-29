#' Subset Dataset for Training the Pre-Trained Deep Neural Network (DNN)
#'
#' @description
#' This dataset is a subset of the full datasets, consisting of 1,000 samples
#' from the original 10,000,000-sample datasets.
#'
#' @format A 1,000×55 matrix, where the first 54 columns represent feature values and
#' the last column represents the labels, which correspond to the number of factors associated with the features.
#' @seealso \link[EFAfactors]{DNN_predictor}, \link[EFAfactors]{load_scaler}, \link[EFAfactors]{data.scaler}, \link[EFAfactors]{normalizor}
#'
#' @note
#' Methods for generating and extracting features from the dataset can be found in \link[EFAfactors]{DNN_predictor}.
#'
#' @examples
#' data(data.datasets)
#' head(data.datasets)
#'
#' @docType data
#' @name data.datasets
NULL
