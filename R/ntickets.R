#' ntickets
#'
#' @param N Number of seats
#' @param gamma Probability threshold between 0 and 1
#' @param p probability of passengers showing up for flight
#'
#' @importFrom stats pbinom qbinom uniroot
#' @importFrom graphics legend lines
#'
#' @return printed list and 2 plots
#' @export
#'
#' @examples
#' ntickets(N=400,gamma = 0.02, p = 0.95)
ntickets = function(N, gamma, p) {
  # calculate nd using the appropriate discrete distribution
  findN = qbinom(1 - gamma, N:(N + 1000), p)
  idx = N:(N + 1000)
  nd = idx[findN == N]

  # calculate nc using normal approximation
  objCont = function(n) {
    np = n * p
    1 - gamma - pnorm(N, np, sqrt(np * (1 - p)))
  }
  nc = uniroot(objCont, c(N, N + 1000))$root

  # print list
  print(
    list(
      nd = nd,
      nc = nc,
      N = N,
      p = p,
      gamma = gamma
    )
  )

  objDis = function(n) {
    1 - gamma - pbinom(N, n, p)
  }

  #plot the discrete
  nValues = seq(N, N + 50)
  objValDis = sapply(nValues, objDis)
  plot(nValues, objValDis, type = "l", xlab = "n", ylab = "Objective",
       main = paste("Objective vs n to find (",nd,") Discrete"), col = "purple")
  abline(h = 0, col = "red")
  abline(v = nd + 1, col = "blue")

  #plot the continuous
  objValCont = sapply(nValues, objCont)
  plot(nValues, objValCont, type = "l", xlab = "n", ylab = "Objective",
       main = paste("Objective vs n to find (",nc,") Continuous"), col = "limegreen")
  abline(h = 0, col = "red")
  abline(v = nc, col = "blue")
}
