# logical function tests if an object is a jags object, 
is.jags <- function(x){
  is(x, 'rjags') || is(x, 'mcmc.list')  || is(x, 'rjags.parallel')
  }