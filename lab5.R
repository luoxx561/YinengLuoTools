
ga_data <- scan(url("http://www.stat.umn.edu/geyer/3701/data/q1p3.txt"))
cau_data <- scan(url("http://www.stat.umn.edu/geyer/3701/data/q1p7c.txt"))
bin_data <- scan(url("http://www.stat.umn.edu/geyer/3701/data/q1p7b.txt"))



ga  = function(theta, x) dgamma(x, shape = theta, log = TRUE)
cau = function(theta, x) dcauchy(x, location = theta, log = TRUE)
bin = function(theta, x) dbinom(x, 20, prob = 1 / (1 + exp(- theta)), log = TRUE)


lab5 = function(X,density,interval)
{
  
  log1 = function(theta,x=X)
  {
    sum(density(theta,x))
  }
  oout <- optimize(log1, maximum = TRUE, interval)
  return(oout$maximum)
}

lab5(ga_data,ga,c(0,3))


lab5(bin_data,bin,c(-100,100))