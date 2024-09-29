#'
#' the Hull Approach
#'
#' @description
#' The Hull method is a heuristic approach used to determine the optimal number of common factors
#' in factor analysis. It evaluates models with increasing numbers of factors and uses goodness-of-fit
#' indices relative to the model degrees of freedom to select the best-fitting model. The method is known
#' for its effectiveness and reliability compared to other methods like the scree plot.
#'
#' @details
#' The Hull method (Lorenzo-Seva & Timmerman, 2011) is a heuristic approach used to determ
#' ine the number of common factors in factor analysis. This method is similar to
#' non-graphical variants of Cattell's scree plot but relies on goodness-of-fit indices
#' relative to the model degrees of freedom. The Hull method finds the optimal number of
#' factors by following these steps:
#'
#' @param response A required \code{N} × \code{I} matrix or data.frame consisting of the responses of \code{N} individuals
#'          to \code{I} items.
#' @param fa A string that determines the method used to obtain eigenvalues in PA. If 'pc', it represents
#'           Principal Component Analysis (PCA); if 'fa', it represents Principal Axis Factoring (a widely
#'           used Factor Analysis method; @seealso \code{\link[EFAfactors]{factor.analysis}};
#'           Auerswald & Moshagen, 2019). (Default = 'pc')
#' @param nfact A numeric value that specifies the number of factors to extract, only effective when \code{fa = 'fa'}. (Default = 1)
#' @param cor.type A character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default),
#'          "kendall", or "spearman". @seealso \link[stats]{cor}.
#' @param use an optional character string giving a method for computing covariances in the presence of missing values. This
#'          must be one of the strings "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs" (default).
#'          @seealso \link[stats]{cor}.
#' @param vis A Boolean variable that will print the factor retention results when set to TRUE, and will not print
#'          when set to FALSE. (default = TRUE)
#' @param plot A Boolean variable that will print the Hull plot when set to TRUE, and will not print it when set to
#'          FALSE. @seealso \link[EFAfactors]{plot.Hull}. (Default = TRUE)
#'
#' \enumerate{
#'   \item Calculate the goodness-of-fit index (CFI)
#'         and model degrees of freedom (df; Lorenzo-Seva & Timmerman, 2011; \eqn{df = I × F - 0.5 × F * (F - 1)},
#'         \eqn{I} is the number of items, and \eqn{F} is the number of factors)
#'         for models with an increasing number of factors, up to a prespecified maximum,
#'         which is equal to the \item{nfact} of \link[EFAfactors]{PA} method. the GOF will always be
#'         Comparative Fit Index (CFI), for it performs best under various conditions than other GOF (Auerswald & Moshagen, 2019;
#'         Lorenzo-Seva & Timmerman, 2011), such as RMSEA and SRMR. @seealso \link[EFAfactors]{EFAindex}
#'   \item Identify and exclude solutions that are less complex (with fewer factors)
#'         but have a higher fit index.
#'   \item Further exclude solutions if their fit indices fall below the line connecting
#'         adjacent viable solutions.
#'   \item Determine the number of factors where the ratio of the difference in
#'         goodness-of-fit indices to the difference in degrees of freedom is maximized.
#' }
#'
#' @param response A required \code{N} × \code{I} matrix or data.frame consisting of the responses of \code{N} individuals
#'          to × \code{I} items.
#' @param vis A Boolean variable that will print the factor retention results when set to TRUE, and will not print
#'          when set to FALSE. (default = TRUE)
#' @param plot A Boolean variable that will print the CD plot when set to TRUE, and will not print it when set to
#'          FALSE. @seealso \link[EFAfactors]{plot.Hull}. (Default = TRUE)
#'
#' @return A list with the following components:
#' \item{nfact}{The optimal number of factors according to the Hull method.}
#' \item{CFI}{A numeric vector of CFI values for each number of factors considered.}
#' \item{df}{A numeric vector of model degrees of freedom for each number of factors considered.}
#' \item{Hull.CFI}{A numeric vector of CFI values with points below the convex Hull curve removed.}
#' \item{Hull.df}{A numeric vector of model degrees of freedom with points below the convex Hull curve removed.}
#'
#' @references
#' Auerswald, M., & Moshagen, M. (2019). How to determine the number of factors to retain in exploratory factor analysis: A comparison of extraction methods under realistic conditions. Psychological methods, 24(4), 468-491. https://doi.org/https://doi.org/10.1037/met0000200.
#'
#' Lorenzo-Seva, U., Timmerman, M. E., & Kiers, H. A. L. (2011). The Hull Method for Selecting the Number of Common Factors. Multivariate Behavioral Research, 46(2), 340-364. https://doi.org/10.1080/00273171.2011.564527.
#'
#'
#' @author Haijiang Qin <Haijiang133@outlook.com>
#'
#' @examples
#' library(EFAfactors)
#' set.seed(123)
#'
#' ##Take the data.bfi dataset as an example.
#' data(data.bfi)
#'
#' response <- as.matrix(data.bfi[, 1:25]) ## loading data
#' response <- na.omit(response) ## Remove samples with NA/missing values
#'
#' ## Transform the scores of reverse-scored items to normal scoring
#' response[, c(1, 9, 10, 11, 12, 22, 25)] <- 6 - response[, c(1, 9, 10, 11, 12, 22, 25)] + 1
#'
#'
#' ## Run EKC function with default parameters.
#' \donttest{
#'  Hull.obj <- Hull(response)
#'
#'  print(Hull.obj)
#'
#'  plot(Hull.obj)
#'
#'  ## Get the CFI, df and  nfact results.
#'  CFI <- Hull.obj$CFI
#'  df <- Hull.obj$df
#'  nfact <- Hull.obj$nfact
#'
#'  print(CFI)
#'  print(df)
#'  print(nfact)
#'
#' }
#'
#'
#' @export
#' @importFrom psych fa
#' @importFrom ddpcr quiet
Hull <- function(response,
                 fa = "pc", nfact = 1,
                 cor.type = "pearson", use = "pairwise.complete.obs",
                 vis = TRUE, plot = TRUE) {

  response <- scale(response)
  cor.response <- cor(response, method = cor.type, use = use)

  pa <- PA(response, fa = fa, nfact = nfact, cor.type = cor.type, use=use, vis = FALSE, plot = FALSE)
  nfact.max <- pa$nfact + 1
  if (nfact.max == 0) return(0)

  I <- dim(response)[2]
  N <- dim(response)[1]
  df <- CFI <- numeric(nfact.max + 1)

  CFI <- c()
  for (nfact.cur in 1:nfact.max) {
    ddpcr::quiet(fa.obj <- psych::fa(response, nfactors = nfact.cur, rotate = "none", fm = "minres"))
    CFI <- c(CFI, fa.obj$CFI)

    df[nfact.cur + 1] <- I * nfact.cur - 0.5 * nfact.cur * (nfact.cur - 1)
  }
  CFI <- c(0, CFI)

  if (any(is.na(CFI))) {
    return(list(nfact = 0, CFI = CFI, df = df))
  }

  elim.ct <- sapply(2:length(CFI), function(iii) CFI[iii] < max(CFI[1:(iii - 1)]))
  last.elim <- elim.ct[length(elim.ct)] == 1
  Hull.CFI <- CFI[elim.ct == 0]
  Hull.df <- df[elim.ct == 0]

  elim.ct <- 1
  while (sum(elim.ct) > 0 & length(Hull.CFI) > 2) {
    elim.ct <- c(0, sapply(2:(length(Hull.CFI) - 1), function(iii) {
      (Hull.CFI[iii] - Hull.CFI[iii - 1]) * (Hull.df[iii + 1] - Hull.df[iii - 1]) <=
        (Hull.CFI[iii + 1] - Hull.CFI[iii - 1]) * (Hull.df[iii] - Hull.df[iii - 1])
    }), 0)
    Hull.CFI <- Hull.CFI[elim.ct == 0]
    Hull.df <- Hull.df[elim.ct == 0]
  }

  sti <- sapply(2:nfact.max, function(nnn) {
    ((CFI[nnn] - CFI[nnn - 1]) / (df[nnn] - df[nnn - 1])) /
      ((CFI[nnn + 1] - CFI[nnn]) / (df[nnn + 1] - df[nnn]))
  })
  sti <- c(0, sti, 0)
  viable.sti <- sti[match(Hull.CFI, CFI)]

  nfact <- if (length(viable.sti) == 2) {
    if (!last.elim) nfact.max else nfact.max - 1
  } else {
    which(sti == max(viable.sti)) - 1
  }

  nfact <- ifelse(is.na(nfact), 0, nfact)
  Hull.obj <- list(nfact = nfact, CFI = CFI, df = df, Hull.CFI = Hull.CFI, Hull.df = Hull.df)
  class(Hull.obj) <- "Hull"

  if (vis) print(Hull.obj)
  if (plot) plot(Hull.obj)

  return(Hull.obj)
}
