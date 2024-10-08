% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EFAsim.data.R
\name{EFAsim.data}
\alias{EFAsim.data}
\title{Simulate Data that Conforms to the theory of Exploratory Factor Analysis.}
\usage{
EFAsim.data(
  nfact,
  vpf,
  N = 500,
  distri = "normal",
  fc = "R",
  pl = "R",
  cl = "R",
  low.vpf = 5,
  up.vpf = 15,
  a = NULL,
  b = NULL,
  vis = TRUE,
  seed = NULL
)
}
\arguments{
\item{nfact}{A numeric value specifying the number of factors to simulate.}

\item{vpf}{A numeric or character value specifying the number of items under each factor.
If a numeric value is provided, the numeric must be larger than 2,
and the number of items under each factor will be fixed
to this value. If a character value is provided, it must be one of 'S', 'M', 'L', or 'R'.
These represent random selection of items under each factor from \eqn{U(5, 10)}, \eqn{U(5, 15)},
\eqn{U(5, 20)}, or \eqn{U(low.vpf up.vpf)}, respectively.}

\item{N}{A numeric value specifying the number of examinees to simulate.}

\item{distri}{A character, either 'normal' or 'beta', indicating whether the simulated data
will follow a standard multivariate normal distribution or a multivariate beta distribution.}

\item{fc}{A numeric or character value specifying the degree of correlation between factors.
If a numeric value is provided, it must be within the range of 0 to 0.75, and the correlation
between all factors will be fixed at this value. If a character value is provided, it must be 'R',
and the correlations between factors will be randomly selected from \eqn{U(0.0, 0.5)}.}

\item{pl}{A numeric or character value specifying the size of the primary factor loadings.
If a numeric value is provided, it must be within the range of 0 to 1, and all primary factor
loadings in the loading matrix will be fixed at this value. If a character value is provided,
it must be one of 'L', 'M', 'H', or 'R', representing \eqn{pl~U(0.35, 0.50)}, \eqn{pl~U(0.50, 0.65)},
\eqn{pl~U(0.65, 0.80)}, or \eqn{pl~U(0.35, 0.80)}, respectively, consistent with the settings in Goretzko & Buhner (2020).}

\item{cl}{A numeric or character value specifying the size of cross-loadings.
If a numeric value is provided, it must be within the range of 0 to 0.5, and all cross-loadings
in the loading matrix will be fixed at this value. If a character value is provided, it must be
one of 'L', 'H', 'None', or 'R', representing \eqn{cl~U(-0.1, 0.1)}, \eqn{cl~U(-0.2, -0.1) \cup U(0.1, 0.2)},
\eqn{cl = 0}, or \eqn{cl~U(-0.2, 0.2)}, respectively, consistent with the settings in Auerswald & Moshagen (2019).}

\item{low.vpf}{A numeric value specifying the minimum number of items per factor, must be larger than 2, effective only when \code{vpf} is 'R'. (default = 5)}

\item{up.vpf}{A numeric value specifying the maximum number of items per factor, effective only when \code{vpf} is 'R'. (default = 15)}

\item{a}{A numeric or NULL specifying the 'a' parameter of the beta distribution, effective only when \code{distri = 'beta'}.
If a numeric value is provided, it will be used as the 'a' parameter of the beta distribution.
If NULL, a random integer between 1 and 10 will be used. (default = NULL)}

\item{b}{A numeric or NULL specifying the 'b' parameter of the beta distribution, effective only when \code{distri = 'beta'}.
If a numeric value is provided, it will be used as the 'b' parameter of the beta distribution.
If NULL, a random integer between 1 and 10 will be used. (default = NULL)}

\item{vis}{A logical value indicating whether to print process information. (default = TRUE)}

\item{seed}{A numeric or NULL specifying the random seed. If a numeric value is provided, it will be used as the seed.
If NULL, the current timestamp will be used. (default = NULL)}
}
\value{
An object of class \code{EFAdata} is a \code{list} containing the following components:
\item{loadings}{A simulated loading matrix.}
\item{items}{A \code{list} containing all factors and the item indices under each factor.}
\item{cor.factors}{A simulated factor correlation matrix.}
\item{cor.items}{A simulated item correlation matrix.}
\item{response}{A simulated response data matrix.}
}
\description{
This function is used to simulate data that conforms to the theory of exploratory factor analysis,
with a high degree of customization for the variables involved.
}
\details{
A population correlation matrix was created for each data set based on the following decomposition:
\deqn{\mathbf{\Sigma} = \mathbf{\Lambda} \mathbf{\Phi} \mathbf{\Lambda}^T + \mathbf{\Delta}}
where \eqn{\mathbf{\Lambda}} is the loading matrix, \eqn{\mathbf{\Phi}} is the factor correlation
matrix, and \eqn{\mathbf{\Delta}} is a diagonal matrix,
with \eqn{\mathbf{\Delta} = 1 - \text{diag}(\mathbf{\Lambda} \mathbf{\Phi} \mathbf{\Lambda}^T)}.
The purpose of \eqn{\mathbf{\Delta}} is to ensure that the diagonal elements of \eqn{\mathbf{\Sigma} } are 1.

The response data for each subject was simulated using the following formula:
\deqn{X_i = L_i + \epsilon_i, \quad 1 \leq i \leq I}
where \eqn{L_i} follows a a standard normal distribution (\code{distri = 'normal'}) or a beta
distribution (\code{distri = 'beta'}), representing the contribution of latent factors.
And \eqn{\epsilon_i} is the residual term following a standard normal distribution
(\code{distri = 'normal'}) or a beta distribution (\code{distri = 'beta'}) . \eqn{L_i} and \eqn{\epsilon_i}
are uncorrelated, and \eqn{\epsilon_i} and \eqn{\epsilon_j} are also uncorrelated.
}
\examples{
library(EFAfactors)

## Run EFAsim.data function with default parameters.
data.obj <- EFAsim.data(nfact = 3, vpf = 5, N=500, distri="normal", fc="R", pl="R", cl="R",
                     low.vpf = 5, up.vpf = 15, a = NULL, b = NULL, vis = TRUE, seed = NULL)

head(data.obj$loadings)




}
\references{
Goretzko, D., & Buhner, M. (2020). One model to rule them all? Using machine learning algorithms to determine the number of factors in exploratory factor analysis. Psychological Methods, 25(6), 776-786. https://doi.org/10.1037/met0000262.

Auerswald, M., & Moshagen, M. (2019). How to determine the number of factors to retain in exploratory factor analysis: A comparison of extraction methods under realistic conditions. Psychological methods, 24(4), 468-491. https://doi.org/https://doi.org/10.1037/met0000200
}
