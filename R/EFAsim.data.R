
#' Simulate Data that Conforms to the theory of Exploratory Factor Analysis.
#'
#' @description
#' This function is used to simulate data that conforms to the theory of exploratory factor analysis,
#' with a high degree of customization for the variables involved.
#'
#' @param nfact A numeric value specifying the number of factors to simulate.
#' @param vpf A numeric or character value specifying the number of items under each factor.
#'            If a numeric value is provided, the numeric must be larger than 2,
#'            and the number of items under each factor will be fixed
#'            to this value. If a character value is provided, it must be one of 'S', 'M', 'L', or 'R'.
#'            These represent random selection of items under each factor from \eqn{U(5, 10)}, \eqn{U(5, 15)},
#'            \eqn{U(5, 20)}, or \eqn{U(low.vpf up.vpf)}, respectively.
#' @param N A numeric value specifying the number of examinees to simulate.
#' @param distri A character, either 'normal' or 'beta', indicating whether the simulated data
#'               will follow a standard multivariate normal distribution or a multivariate beta distribution.
#' @param fc A numeric or character value specifying the degree of correlation between factors.
#'           If a numeric value is provided, it must be within the range of 0 to 0.75, and the correlation
#'           between all factors will be fixed at this value. If a character value is provided, it must be 'R',
#'           and the correlations between factors will be randomly selected from \eqn{U(0.0, 0.5)}.
#' @param pl A numeric or character value specifying the size of the primary factor loadings.
#'           If a numeric value is provided, it must be within the range of 0 to 1, and all primary factor
#'           loadings in the loading matrix will be fixed at this value. If a character value is provided,
#'           it must be one of 'L', 'M', 'H', or 'R', representing \eqn{pl~U(0.35, 0.50)}, \eqn{pl~U(0.50, 0.65)},
#'           \eqn{pl~U(0.65, 0.80)}, or \eqn{pl~U(0.35, 0.80)}, respectively, consistent with the settings in Goretzko & Buhner (2020).
#' @param cl A numeric or character value specifying the size of cross-loadings.
#'           If a numeric value is provided, it must be within the range of 0 to 0.5, and all cross-loadings
#'           in the loading matrix will be fixed at this value. If a character value is provided, it must be
#'           one of 'L', 'H', 'None', or 'R', representing \eqn{cl~U(-0.1, 0.1)}, \eqn{cl~U(-0.2, -0.1) \cup U(0.1, 0.2)},
#'           \eqn{cl = 0}, or \eqn{cl~U(-0.2, 0.2)}, respectively, consistent with the settings in Auerswald & Moshagen (2019).
#' @param low.vpf A numeric value specifying the minimum number of items per factor, must be larger than 2, effective only when \code{vpf} is 'R'. (default = 5)
#' @param up.vpf A numeric value specifying the maximum number of items per factor, effective only when \code{vpf} is 'R'. (default = 15)
#' @param a A numeric or NULL specifying the 'a' parameter of the beta distribution, effective only when \code{distri = 'beta'}.
#'          If a numeric value is provided, it will be used as the 'a' parameter of the beta distribution.
#'          If NULL, a random integer between 1 and 10 will be used. (default = NULL)
#' @param b A numeric or NULL specifying the 'b' parameter of the beta distribution, effective only when \code{distri = 'beta'}.
#'          If a numeric value is provided, it will be used as the 'b' parameter of the beta distribution.
#'          If NULL, a random integer between 1 and 10 will be used. (default = NULL)
#' @param vis A logical value indicating whether to print process information. (default = TRUE)
#' @param seed A numeric or NULL specifying the random seed. If a numeric value is provided, it will be used as the seed.
#'             If NULL, the current timestamp will be used. (default = NULL)
#'
#'
#' @return An object of class \code{EFAdata} is a \code{list} containing the following components:
#' \item{loadings}{A simulated loading matrix.}
#' \item{items}{A \code{list} containing all factors and the item indices under each factor.}
#' \item{cor.factors}{A simulated factor correlation matrix.}
#' \item{cor.items}{A simulated item correlation matrix.}
#' \item{response}{A simulated response data matrix.}
#'
#' @details
#' A population correlation matrix was created for each data set based on the following decomposition:
#' \deqn{\mathbf{\Sigma} = \mathbf{\Lambda} \mathbf{\Phi} \mathbf{\Lambda}^T + \mathbf{\Delta}}
#' where \eqn{\mathbf{\Lambda}} is the loading matrix, \eqn{\mathbf{\Phi}} is the factor correlation
#' matrix, and \eqn{\mathbf{\Delta}} is a diagonal matrix,
#' with \eqn{\mathbf{\Delta} = 1 - \text{diag}(\mathbf{\Lambda} \mathbf{\Phi} \mathbf{\Lambda}^T)}.
#' The purpose of \eqn{\mathbf{\Delta}} is to ensure that the diagonal elements of \eqn{\mathbf{\Sigma} } are 1.
#'
#' The response data for each subject was simulated using the following formula:
#' \deqn{X_i = L_i + \epsilon_i, \quad 1 \leq i \leq I}
#' where \eqn{L_i} follows a a standard normal distribution (\code{distri = 'normal'}) or a beta
#' distribution (\code{distri = 'beta'}), representing the contribution of latent factors.
#' And \eqn{\epsilon_i} is the residual term following a standard normal distribution
#' (\code{distri = 'normal'}) or a beta distribution (\code{distri = 'beta'}) . \eqn{L_i} and \eqn{\epsilon_i}
#' are uncorrelated, and \eqn{\epsilon_i} and \eqn{\epsilon_j} are also uncorrelated.
#'
#'
#'
#' @references
#' Goretzko, D., & Buhner, M. (2020). One model to rule them all? Using machine learning algorithms to determine the number of factors in exploratory factor analysis. Psychological Methods, 25(6), 776-786. https://doi.org/10.1037/met0000262.
#'
#' Auerswald, M., & Moshagen, M. (2019). How to determine the number of factors to retain in exploratory factor analysis: A comparison of extraction methods under realistic conditions. Psychological methods, 24(4), 468-491. https://doi.org/https://doi.org/10.1037/met0000200
#'
#'
#' @examples
#' library(EFAfactors)
#'
#' ## Run EFAsim.data function with default parameters.
#' data.obj <- EFAsim.data(nfact = 3, vpf = 5, N=500, distri="normal", fc="R", pl="R", cl="R",
#'                      low.vpf = 5, up.vpf = 15, a = NULL, b = NULL, vis = TRUE, seed = NULL)
#'
#' head(data.obj$loadings)
#'
#'
#'
#'
#' @export
#' @importFrom MASS mvrnorm
#' @importFrom Matrix nearPD
#' @importFrom SimCorMultRes rnorta
#' @importFrom stats rnorm runif
#' @importFrom ddpcr quiet
#'
EFAsim.data <- function(nfact, vpf, N=500,
                     distri="normal", fc="R", pl="R", cl="R",
                     low.vpf = 5, up.vpf = 15,
                     a = NULL, b = NULL,
                     vis = TRUE, seed = NULL){

  input.check(nfact, vpf, N, distri, fc, pl, cl, low.vpf, up.vpf, a, b, vis, seed)

  if(is.null(seed)){
    set.seed(as.numeric(Sys.time()))
  }else{
    set.seed(seed)
  }

  if(pl == "H"){
    p.up <- 0.80; p.low=0.65
  }else if(pl == "M"){
    p.up <- 0.65; p.low=0.50
  }else if(pl == "L"){
    p.up <- 0.50; p.low=0.35
  }else if(pl == "R"){
    p.up <- 0.80; p.low=0.35
  }else if(is.numeric(pl)){
    p.up <- p.low <- pl
  }else{
    stop("Your `pl` is invalid. It must be 'L', 'M', 'H', 'R', or any number between 0 and 1")
  }

  if(!is.null(vpf) && vpf != 0 && is.numeric(vpf)){
    I <- nfact * vpf
    items.random <- sample(1:I, I, replace = FALSE)
    items.spilts <- c(0, unlist(lapply(vpf, function(x){
      num <- c()
      for(nf in 1:nfact){ num <- c(num, nf*x) }
      num[nfact] <- I
      return(num)
    })))

    items <- list()
    for(nf in 2:(nfact+1))
      items[[nf-1]] <- sort(items.random[(items.spilts[(nf-1)]+1):(items.spilts[nf])])

  }else if(!is.null(vpf) && is.character(vpf) && any(rep(vpf, 3) == c("S", "M", "L"))) {

    if(vpf == "L")
      up.vpf <- 20
    if(vpf == "M")
      up.vpf <- 15
    if(vpf == "S")
      up.vpf <- 10

    items.random <- c()
    vpf <- rep(0, nfact)
    for(nf in 1:nfact){
      vpf[nf] <- sample(low.vpf:up.vpf, 1)
      items.random <- c(items.random, rep(nf, vpf[nf]))
    }
    I <- sum(vpf)
    items.random <- sample(items.random, I, replace = FALSE)
    names(items.random) <- 1:I

    items <- NULL
    for(nf in 1:nfact){
      items[[nf]] <- sort(as.numeric(names(items.random[which(items.random == nf)])))
    }
  }else if(!is.null(vpf) && is.character(vpf) && vpf == "R"){
    items.random <- c()
    vpf <- rep(0, nfact)
    for(nf in 1:nfact){
      vpf[nf] <- sample(low.vpf:up.vpf, 1)
      items.random <- c(items.random, rep(nf, vpf[nf]))
    }
    I <- sum(vpf)
    items.random <- sample(items.random, I, replace = FALSE)
    names(items.random) <- 1:I

    items <- NULL
    for(nf in 1:nfact){
      items[[nf]] <- sort(as.numeric(names(items.random[which(items.random == nf)])))
    }
  }


  loadings <- matrix(0, I, nfact)
  for(nf in 2:(nfact+1))
    loadings[items[[nf-1]], nf-1] <- runif(length(items[[nf-1]]), p.low, p.up)

  if(is.numeric(cl)){
    loadings[which(loadings == 0)] <- cl
  }else{
    if(cl == "R"){
      loadings[which(loadings == 0)] <- runif(length(which(loadings == 0)), -0.20, 0.20)
    }else if(cl == "L"){
      loadings[which(loadings == 0)] <- runif(length(which(loadings == 0)), -0.10, 0.10)
    }else if(cl == "H"){
      loadings[which(loadings == 0)] <- runif(length(which(loadings == 0)), 0.10, 0.20) *
        sample(c(-1, 1), length(which(loadings == 0)), replace = TRUE)
    }else if(cl == "None"){
      loadings[which(loadings == 0)] <- 0
    }
  }


  if(fc == "R"){
    cor.factors <- matrix(runif(nfact * nfact, 0.0, 0.5), nfact, nfact)
    cor.factors <- cor.factors * upper.tri(cor.factors, diag = TRUE)
    if(nfact > 1)
      cor.factors <- cor.factors + t(cor.factors) - diag(diag(cor.factors))
  }else if(is.numeric(as.numeric(fc))){
    cor.factors <- matrix(fc, nfact, nfact)
  }
  diag(cor.factors) <- 1


  cor.items <- loadings %*%  cor.factors %*% t(loadings) + diag(1 - diag(loadings %*%  cor.factors %*% t(loadings)))
  cor.items.nearPD <- nearPD(cor.items)$mat

  if(distri == "beta"){
    distr_latent <- distr_error <- rep("qbeta", I)
    shape_latent <- shape_error <- NULL
    for(i in 1:I){
      if(is.null(a)){
        shape1 <- sample(1:10, 1)
      }else{
        shape1 <- a
      }
      if(is.null(b)){
        shape2 <- sample(1:10, 1)
      }else{
        shape2 <- b
      }

      shape_latent[[i]] <- list(shape1=shape1, shape2=shape2)
      shape_error[[i]] <- list(shape1=shape1, shape2=shape2)
    }

    quiet(response <- rnorta(R = N, cor.matrix = cor.items,
                                   distr = distr_latent, qparameters=shape_latent) +
            rnorta(R = N, cor.matrix = diag(rep(1, I)),
                   distr = distr_error, qparameters=shape_error))

  }else if(distri == "normal"){

    response <- mvrnorm(n=N, rep(0, I), Sigma = cor.items.nearPD) +
      mvrnorm(n=N, rep(0, I), Sigma = diag(rep(1, I)))

  }

  names(items) <- colnames(loadings) <- colnames(cor.factors) <- rownames(cor.factors) <- paste0("factor", 1:nfact)
  rownames(loadings) <- rownames(cor.items) <- colnames(cor.items) <- colnames(response) <- paste0("item ", 1:I)

  EFAdata.obj <-list(loadings=loadings, items=items, cor.factors=cor.factors,
                     cor.items=cor.items, response=response)
  class(EFAdata.obj) <- "EFAdata"

  if(vis) print(EFAdata.obj)

  return(EFAdata.obj)
}

input.check <- function(nfact, vpf, N, distri, fc, pl, cl, low.vpf, up.vpf, a, b, vis, seed){

  if(!is.numeric(nfact))
    stop("'nfact' must be a numeric !")

  if(is.numeric(vpf) | is.character(vpf)){
    if(is.numeric(vpf)){
      if(vpf < 3)
        stop("'vpf' must be larger than 2 when it is a numeric !")
    }else{
      if(!any(rep(vpf, 4) == c("S", "M", "L", "R")))
        stop("'vpf' must be on of 'S', 'M', 'L' or 'R' when it is a character !")
    }
  }else{
    stop("'vpf' must be a numeric or character !")
  }

  if(!is.numeric(N))
    stop("'N' must be a numeric !")

  if(is.character(distri)){
    if(!any(rep(distri, 2) == c('normal', 'beta'))){
      stop("'distri' must be on of 'normal' or 'beta' !")
    }
  }else{
    stop("'distri' must be on of 'normal' or 'beta' !")
  }

  if(is.numeric(fc) | is.character(fc)){
    if(is.numeric(fc)){
      if(fc > 0.75 || fc < 0.00)
        stop("'fc' must be 0 ~ 0.75 when it is a numeric !")
    }else{
      if(fc != "R")
        stop("'fc' must be 'R' when it is a character !")
    }
  }else{
    stop("'fc' must be a numeric or character !")
  }

  if(is.numeric(pl) | is.character(pl)){
    if(is.numeric(fc)){
      if(fc > 1 || fc < 0)
        stop("'fc' must be 0.00 ~ 1.00 when it is a numeric !")
    }else{
      if(!any(rep(fc, 4) == c('L', 'M', 'H', 'R')))
        stop("'fc' must be one of 'L', 'M', 'H' or 'R' when it is a character !")
    }
  }else{
    stop("'fc' must be a numeric or character !")
  }

  if(is.numeric(cl) | is.character(cl)){
    if(is.numeric(cl)){
      if(cl > 0.5 || cl < 0)
        stop("'cl' must be 0.00 ~ 0.50 when it is a numeric !")
    }else{
      if(!any(rep(cl, 4) == c('L', 'H', 'None', 'R')))
        stop("'cl' must be one of 'L', 'H', 'None' or 'R' when it is a character !")
    }
  }else{
    stop("'cl' must be a numeric or character !")
  }

  if(!is.numeric(low.vpf)){
    stop("'low.vpf' must be a numeric !")
  }else if(low.vpf < 3){
    stop("'low.vpf' must be larger than 2 !")
  }

  if(!is.null(a) && !is.numeric(a))
    stop("'a' must be NULL or numeric !")

  if(!is.null(b) && !is.numeric(b))
    stop("'b' must be NULL or numeric !")

  if(!is.logical(vis))
    stop("'vis' must be logical (TRUE or FALSE) !")

  if(!is.null(seed) && !is.numeric(seed))
    stop("'seed' must be NULL or numeric !")
}
