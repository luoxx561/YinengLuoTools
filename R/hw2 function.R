#'  Question1
#'  @param x vector 
#'   
#'  @return list
#'  @export
#'  @examples
#'  load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
#'  problem1(a, x)
problem1 <- function(a, x) {
  stopifnot(is.matrix(a))
  stopifnot(is.numeric(a))
  stopifnot(is.numeric(x))
  stopifnot(nrow(a) == ncol(a))
  stopifnot(nrow(a) == length(x))
  b <- solve(a, x)
  sum(x * b)
}


#'  Question2
#'  @param x vector 
#'   
#'  @return list
#'  @export
#'  @examples
#'  load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
#'  a %problem2% x


'%problem2%' <- function(a, x) {
  stopifnot(is.numeric(a))
  stopifnot(is.matrix(a))
  stopifnot(is.numeric(x))
  stopifnot(nrow(a) == ncol(a))
  stopifnot(nrow(a) == length(x))
  b <- solve(a, x)
  sum(x * b)
}

#'  Question3
#'  @param x vector 
#'   
#'  @return list
#'  @export
#'  @examples
#'  load(url("http://www.stat.umn.edu/geyer/3701/data/q2p3.rda"))
#'  problem3(a)


problem3<- function(x) {
  stopifnot(is.numeric(x))
  stopifnot(is.matrix(x))
  for (i in 1:ncol(a)) {
    standard <- a[ , i]
    a[ , i] <- (standard - mean(standard)) / sd(standard)
  }    
  return(a)
}


#'  Question4
#'  @param x vector 
#'   
#'  @return list
#'  @export
#'  @examples
#'  load(url("http://www.stat.umn.edu/geyer/3701/data/q2p3.rda"))
#'  stand(a)

stand <- function(a) 
{
  stopifnot(is.matrix(a))
  stopifnot(is.numeric(a))
  apply(a, 2, function(x) (x - mean(x)) / sd(x))
}

#'  Question5
#'  @param x vector 
#'   
#'  @return list
#'  @export
#'  @examples
#'   
#'   myapply(fred,2,"quantile",prob=0.75)
#'   myapply(fred,1,"quantile",prob=(1:3)/4)

myapply <- function (X, MARGIN, FUN, ...)
{
  
  #stopifnot(length(dim(X))==2)
  
  if(length(dim(X))!=2)
  {
    stop("matrix is not 2d")
  }
  if(!(MARGIN %in% c(1,2)))
  {
    stop("matrix is not 1 or 2")
  }
  R = dim(X)[1]
  C = dim(X)[2]
  f = match.fun(FUN)
  
  if (MARGIN == 1)
  { 
    result = list()
    for(i in 1:R)
    {
      result[[i]] = f(X[i,],...)
    }
  }else if (MARGIN == 2)
  {
    result = list()
    for(j in 1:C)
    {
      result [[j]] = f(X[,j],...)
    }
  }
  return(simplify2array(result))
}





