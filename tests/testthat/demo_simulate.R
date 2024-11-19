# library(EFAfactors)
#
# # set.seed(123)  ## 423 for example R R R nn
#
# NF <- c(4)
# FC <- c(0)
# VPF <- c(4)
# PL <- c("L")
# CL <- c("L")
# N <- c(100)
# distribution <- "normal"
#
# trials.length <- length(NF) * length(FC) * length(VPF) * length(PL) * length(CL) * length(N)
# trial.list <- list(NF=NF, FC=FC, VPF=VPF, PL=PL, CL=CL, N=N)
# condition.names <- c("F", "FC", "VPF", "PL", "CL", "N")
# methods.names <- c("Hull", "CD", "PA", "EKC", "FF", "CDF","DNN")
# results.names <- c("nfact", "ACC")
#
# times <- 100
# results.pre <- matrix(0, trials.length, length(methods.names) + 1)
# conditions <- matrix(0, trials.length, length(trial.list))
# results <- array(dim = c(length(NF), length(FC), length(VPF), length(PL), length(CL), length(N),
#                          length(results.names), length(methods.names), times),
#                  dimnames = list(paste0("F=", NF), paste0("FC=", FC), paste0("VPF=", VPF),
#                                  paste0("PL=", PL), paste0("CL=", CL), paste0("N=", N),
#                                  results.names, methods.names, paste0("times=", 1:times)))
#
# vis <- TRUE
# CDF.obj <- FF.obj <- CD.obj <- NULL
# CDF.obj$nfact <- FF.obj$nfact <- CD.obj$nfact <- 0
# posi.start <- 1
# for(posi in posi.start:trials.length){
#   runs.cur <- EFAfactors:::get.runs(posi, trial.list)
#
#   NF.cur <- NF[runs.cur[1]]; FC.cur <- FC[runs.cur[2]]; VPF.cur <- VPF[runs.cur[3]]
#   PL.cur <- PL[runs.cur[4]]; CL.cur <- CL[runs.cur[5]]; N.cur <- N[runs.cur[6]]
#   conditions[posi, ] <- c(NF.cur, FC.cur, VPF.cur, PL.cur, CL.cur, N.cur)
#
#   time.posi <- 0
#   results.cur <- NULL
#   for(t in 1:times){
#     cat("===============================", paste0(posi, "/", trials.length), ":",
#         paste0(t, "/", times), "===============================\n")
#
#     time.cur <- system.time({
#       response <- NULL
#       while (is.null(response)) {
#         data <- NULL
#         while (is.null(data)) {
#           data <- tryCatch(
#             EFAsim.data(nfact=NF.cur, vpf=VPF.cur, N=N.cur,
#                      distri = distribution, fc=FC.cur, pl=PL.cur, cl=CL.cur,
#                      vis = vis),
#             error = function(e){ NULL })
#         }
#         response <- data$response
#
#       }
#
#       # EFAhclust.obj <- EFAhclust(response)
#       # EFAkmeans.obj <- EFAkmeans(response)
#       # plot(EFAkmeans.obj)
#       Hull.obj <- Hull(response)
#       PA.obj <- PA(response)
#       EKC.obj <- EKC(response)
#       DNN.obj <- DNN_predictor(response)
#
#       # FF.obj <- FF(response)
#       # CD.obj <- CD(response)
#       # CDF.obj <- CDF(response)
#
#       methods.names <- c("Hull", "CD", "PA", "EKC", "FF", "CDF","DNN")
#
#       retained.factors <- c(Hull.obj$nfact, CD.obj$nfact, PA.obj$nfact, EKC.obj$nfact, FF.obj$nfact, CDF.obj$nfact, DNN.obj$nfact)
#
#
#       results.cur <- rbind(results.cur, (retained.factors == rep(NF.cur, length(methods.names))))
#       results[paste0("F=", NF.cur), paste0("FC=", FC.cur), paste0("VPF=", VPF.cur),
#               paste0("PL=", PL.cur), paste0("CL=", CL.cur), paste0("N=", N.cur),
#               "nfact", , t] <- retained.factors
#       results[paste0("F=", NF.cur), paste0("FC=", FC.cur), paste0("VPF=", VPF.cur),
#               paste0("PL=", PL.cur), paste0("CL=", CL.cur), paste0("N=", N.cur),
#               "ACC", , t] <- results.cur[t, ]
#
#     })
#     time.posi <- time.cur[3] + time.posi
#     cat("mean of time cost in", paste0(posi, "/", trials.length), ":", round(time.posi / t, 3), "\n\n")
#     results.pre[posi, ] <- round(c(apply(results.cur, 2, sum)/t, time.posi / t), 3)
#     if(posi == 1){
#       res_preview <- data.frame(t(conditions[1:posi, ]), t(results.pre[1:posi, ]))
#     }else{
#       res_preview <- data.frame(conditions[1:posi, ], results.pre[1:posi, ])
#     }
#     rownames(res_preview) <- 1:posi
#     colnames(res_preview) <- c(condition.names, methods.names, "mean time")
#     cat("------------------- current accurace pre-view -------------------\n")
#     print(res_preview)
#     cat("\n")
#     cat("------------------- current retained factors  -------------------\n")
#     print(results[paste0("F=", NF.cur), paste0("FC=", FC.cur), paste0("VPF=", VPF.cur),
#                   paste0("PL=", PL.cur), paste0("CL=", CL.cur), paste0("N=", N.cur),
#                   "nfact", , t])
#     cat("\n")
#     cat("=============================================================================\n")
#     cat("\n")
#   }
#
# }
#
