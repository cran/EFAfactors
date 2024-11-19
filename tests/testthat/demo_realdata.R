# library(EFAfactors)

# set.seed(222)
# data(data.bfi)
#
# response <- as.matrix(data.bfi[, 1:25]) ## loading data
# response <- as.matrix(na.omit(response)) ## Remove samples with NA/missing values
#
# ## Transform the scores of reverse-scored items to normal scoring
# response[, c(1, 9, 10, 11, 12, 22, 25)] <- 6 - response[, c(1, 9, 10, 11, 12, 22, 25)] + 1

# CD.obj <- CD(response)
# FF.obj <- FF(response)

# Hull.obj <- Hull(response)
# DNN.obj <- DNN_predictor(response)
# EKC.obj <- EKC(response)
# PA.obj <- PA(response, fa = "fa", nfact = 1)
#
# CDF.obj <- CDF(response)
#
# methods.names <- c("Hull", "CD", "PA", "EKC", "FF", "CDF","DNN")
# retained.factors <- c(Hull.obj$nfact, CD.obj$nfact, PA.obj$nfact, EKC.obj$nfact, FF.obj$nfact, CDF.obj$nfact, DNN.obj$nfact)
# names(retained.factors) <- methods.names
# print(retained.factors)
#
# EFAvote.obj <- EFAvote(votes = retained.factors)
# plot(EFAvote.obj)
