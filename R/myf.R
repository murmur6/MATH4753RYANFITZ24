## Model y = b0 + b1x + b2(x-xk)*(x>xk) with x_k being 18 taken from lab4
#' myf
#'
#' @param x number
#' @param coef a coef of data
#'
#' @return numeric
#' @export
#'
#' @examples
#' myf(32, coef = c(1, 2, 3))
myf = function(x,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-18)*(x-18>0)
}
