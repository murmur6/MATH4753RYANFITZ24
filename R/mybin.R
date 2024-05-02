# creates a binomial distribution and puts the result into a barplot
#' mybin
#'
#' @param iter number of iterations
#' @param n number of trials
#' @param p probability in each trial
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot
#' @importFrom graphics curve
#' @importFrom graphics polygon
#' @importFrom stats dnorm
#' @importFrom stats pnorm
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' mybin(iter=1000,n=18, p=0.3)
mybin=function(iter=100,n=10, p=0.5){

  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  succ=c()
  for( i in 1:iter){

    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))

    succ[i]=sum(sam.mat[,i])
  }

  succ.tab=table(factor(succ,levels=0:n))
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}

