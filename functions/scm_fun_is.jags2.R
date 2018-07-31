# logical function tests if an object is a jags object, 
is.jags2 <- function(x){
  
  source("functions/scm_fun_is.jags.R")
  
  is.jags(x$model.fit.m) && is.jags(x$model.fit.b)
  
  }