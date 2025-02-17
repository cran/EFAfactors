#'
#'Voting Method for Number of Factors in EFA
#'
#' @description
#' This function implements a voting method to determine the most appropriate number of factors
#' in exploratory factor analysis (EFA). The function accepts a vector of votes, where each value
#' represents the number of factors suggested by different EFA approaches. If there is a clear
#' winner (a single number of factors with the most votes), that number is returned. In case of
#' a tie, the function returns the first value among the tied results and outputs a message. The
#' result is returned as an object of class \code{vote}, which can be printed and plotted.
#'
#' @param votes A vector of integers, where each element corresponds to the number of factors suggested
#' by an EFA method.
#' @param vis Logical, whether to print the results of the voting. Defaults to \code{TRUE}.
#' @param plot Logical, whether to display a pie chart of the voting results. Defaults to \code{TRUE}.
#'
#' @return An object of class \code{EFAvote}, which is a list containing:
#' \item{nfact}{The number of factors with the most votes. If there is a tie, the first one in the order is returned.}
#' \item{votes}{The original vector of votes.}
#'
#' @seealso \link[EFAfactors]{plot.EFAvote}
#'
#' @examples
#' library(EFAfactors)
#'
#' nfacts <- c(5, 5, 5, 6, 6, 4)
#' names(nfacts) <- c("Hull", "CD", "PA", "EKC", "FF", "DNN")
#'
#' EFAvote.obj <- EFAvote(votes = nfacts)
#'
#' # Visualize the voting results
#' plot(EFAvote.obj)
#'
#' @export
#'
EFAvote <- function(votes, vis=TRUE, plot = TRUE){

  voting <- table(votes)
  if(length(which(voting == max(voting))) == 1){

    nfact <- as.numeric(names(which.max(voting)))

  }else{

    message("Multiple max nfact gained from the Voting, the fist max nfact of votes are returned !")
    nfact <-  as.numeric(names(which.max(voting)[1]))

  }
  vote.obj <- list(nfact=nfact, votes=votes)

  class(vote.obj) <- "EFAvote"

  if(vis) print(vote.obj)
  if(plot) plot(vote.obj)

  return(vote.obj)
}
