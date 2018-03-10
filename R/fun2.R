
#'  compute likehood of a distribution for data x .
#'
#'  @param x vector
#'  @param 'function(theta,x) dgamma(x,shape=theta,log=TRUE)'
#'  @param interval vector for optimize function
#'  @return scalar
#'  @export
#'  @examples
#'  x1 <- rgamma(100,3)
#'  func10 = function(theta, x) dgamma(x, shape = theta, log = TRUE)
#'  result7_gamma <-func10(x1, func1, c(0,3))
#'  result7_gamma
#'

func7 <- function( x, func, interval)
{
  f7<- function(theta,x)
  { sum (func(theta,x))}

  oout <-optimize(f7,maximum = TRUE,interval,x=x)
  return(oout$maximum)
}

