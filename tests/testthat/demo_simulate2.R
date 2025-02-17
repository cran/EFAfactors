# library(EFAfactors)
#
# ################################################# data conditions ###################################################
# FC <- c("0.25")
# PL <- c("H")
# CL <- c("H")
# NF <- c(1, 2, 4, 6, 8, 10)
# VPF <- c(4, 7, 10)
# N <- c(100, 200, 500, 1000)
#
# distribution <- "normal"
# trials.length <- length(NF) * length(FC) * length(VPF) * length(PL) * length(CL) * length(N)
# trial.list <- list(NF=NF, FC=FC, VPF=VPF, PL=PL, CL=CL, N=N)
# condition.names <- c("NF", "FC", "VPF", "PL", "CL", "N")
# methods.names <- c("CDF", "PA","LSTM")
# results.names <- c("nfact", "ACC")
#
# times <- 100
# results.pre <- matrix(0, trials.length, length(methods.names) + 1)
# conditions <- matrix(0, trials.length, length(trial.list))
#
# ######################## load or create result file ########################
# res.path <- paste0("results/study II/results", "_", FC, "_", PL, "_", CL, ".rds")
# if(file.exists(res.path)){
#   results <- readRDS(res.path)
# }else{
#   results <- array(dim = c(length(NF), length(FC), length(VPF), length(PL), length(CL), length(N),
#                            length(results.names), length(methods.names), times),
#                    dimnames = list(paste0("NF=", NF), paste0("FC=", FC), paste0("VPF=", VPF),
#                                    paste0("PL=", PL), paste0("CL=", CL), paste0("N=", N),
#                                    results.names, methods.names, paste0("times=", 1:times)))
# }
#
# transform.type <- function(x){
#   if(!is.na(as.numeric(x))){
#     return(as.numeric(x))
#   }
#   return(x)
# }
#
# vis <- TRUE
# CDF.obj <-  NULL
# CDF.obj$nfact <- 0
# len <- 10
# low.vpf <- 3
# up.vpf <- 10
#
# ################################################# run new ###################################################
# posi.start <- 1
# for(posi in posi.start:trials.length){
#
#   ## get order of current data condition
#   runs.cur <- EFAfactors:::get.runs(posi, trial.list)
#
#   ## load current condition
#   NF.cur <- NF[runs.cur[1]]; NF.type <- transform.type(NF.cur)
#   FC.cur <- FC[runs.cur[2]]; FC.type <- transform.type(FC.cur)
#   VPF.cur <- VPF[runs.cur[3]]; VPF.type <- transform.type(VPF.cur)
#   PL.cur <- PL[runs.cur[4]]; PL.type <- transform.type(PL.cur)
#   CL.cur <- CL[runs.cur[5]]; CL.type <- transform.type(CL.cur)
#   N.cur <- N[runs.cur[6]]; N.type <- transform.type(N.cur)
#   conditions[posi, ] <- c(NF.cur, FC.cur, VPF.cur, PL.cur, CL.cur, N.cur)
#
#   if(!is.na(results[paste0("NF=", NF.cur), paste0("FC=", FC.cur), paste0("VPF=", VPF.cur),
#                     paste0("PL=", PL.cur), paste0("CL=", CL.cur), paste0("N=", N.cur),
#                     "ACC", 1, 1])){
#     next
#   }
#
#   ## start loop of corrent data condition (`times` in total)
#   time.posi <- 0
#   results.cur <- NULL
#   for(t in 1:times){
#     cat("===============================", paste0(posi, "/", trials.length), ":",
#         paste0(t, "/", times), "===============================\n")
#
#     time.cur <- system.time({
#
#       ####### simulate data ########
#       response <- NULL
#       while (is.null(response)) {
#         data <- NULL
#         while (is.null(data)) {
#           data <- tryCatch(
#             EFAsim.data(nfact=NF.type, vpf=VPF.type, N=N.type,
#                         distri = distribution, fc=FC.type, pl=PL.type, cl=CL.type,
#                         low.vpf=low.vpf, up.vpf=up.vpf,
#                         vis = vis),
#             error = function(e){ NULL })
#         }
#         response <- data$response
#       }
#
#       ####### PA ########
#       # PA.obj <- PA(response, vis = vis, plot = FALSE)
#
#       ####### CDF ########
#       CDF.obj <- CDF(response, nfact.max = min(c(10, round(ncol(response)/3)+1)),
#                      N.pop = 10000, N.Samples = 2000, mtry = "sqrt",
#                      vis = vis, plot = FALSE)
#
#       ####### LSTM ########
#       # LSTM.obj <- LSTM_predictor(response, len = len, vis = vis, plot = FALSE)
#
#       ####### record result ########
#       retained.factors <- c(CDF.obj$nfact, PA.obj$nfact, LSTM.obj$nfact)
#       results.cur <- rbind(results.cur, (retained.factors == rep(NF.type, length(methods.names))))
#       ## current numbers of factors by different methods
#       results[paste0("NF=", NF.cur), paste0("FC=", FC.cur), paste0("VPF=", VPF.cur),
#               paste0("PL=", PL.cur), paste0("CL=", CL.cur), paste0("N=", N.cur),
#               "nfact", , t] <- retained.factors
#       ## current accuracy
#       results[paste0("NF=", NF.cur), paste0("FC=", FC.cur), paste0("VPF=", VPF.cur),
#               paste0("PL=", PL.cur), paste0("CL=", CL.cur), paste0("N=", N.cur),
#               "ACC", , t] <- results.cur[t, ]
#
#     })
#
#     ####### preview result ########
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
#     print(results[paste0("NF=", NF.cur), paste0("FC=", FC.cur), paste0("VPF=", VPF.cur),
#                   paste0("PL=", PL.cur), paste0("CL=", CL.cur), paste0("N=", N.cur),
#                   "nfact", , t])
#     cat("\n")
#     cat("=============================================================================\n")
#     cat("\n")
#   }
#
#   # ####### save results after each condition ########
#   # saveRDS(results, res.path)
# }
