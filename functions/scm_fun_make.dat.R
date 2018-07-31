make.dat <- function (N, pi, noccasions, nyears = 1, r = 1, years.type = "single"){
  
  if (sum(pi) != 1) { stop ("\n\nYou bloody idiot.\n\nProbabilities do not sum to 1")}
  
  ngroups <- length(pi)
  
  if (nyears == 1 && years.type == "single"){
    
    simdat <- rmultinom(noccasions, N, pi)
    
  } else {
    
    simdat <- array(data = NA, dim = c(ngroups, noccasions, nyears))
    
    n<- NULL
    
    for(year in 1:nyears){
      n[year] <- round(N*((r)^(year-1)))
      
      simdat[,,year] <- rmultinom(noccasions, n[year], pi)
    }
  }
  
  return(simdat)
  
}