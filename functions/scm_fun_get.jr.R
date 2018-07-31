get.jr <- function(x, mod, params = c("N", "pi", "r")){
  
  library("jagstools")
  
  if(mod == "m"){
    
    jr <- jagsresults(x = x$model.fit.m,
                      params = params)
    
  } else if (mod == "b"){
    
    jr <- jagsresults(x = x$model.fit.b,
                      params = params)
    
  }
  
  return(jr)
  
}