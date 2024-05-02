#' myncurve
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a area from to (-inf, a)
#'
#' @return graph and area
#' @export
#'
#' @examples
#' myncurve(mu=10,sigma=5, a=6)
myncurve = function(mu, sigma, a){
  x = seq(mu - 3 * sigma, a, length = 1000)

  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  # the x values we want
  xcurve=seq(mu - 3 * sigma,a,length=1000)

  # the y values we want
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  # fill in the area we want
  polygon(c(mu - 3 * sigma,xcurve,a),c(0,ycurve,0),col="purple")

  # Area
  prob=pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)

  cat("Area =", prob, "\n")
}
