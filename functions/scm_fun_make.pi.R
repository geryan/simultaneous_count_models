# makes vector of probabilities given number of sites, probability individual is observed, and  method of split of observed population

make.pi <- function(nsites, p.prime, pi.split){
  
  pis <- vector(length = (nsites + 1))
  
  if(pi.split == "even"){
    
    pis[1:nsites] <- p.prime/nsites
    pis[nsites + 1] <- 1 - p.prime
    
  } else if (pi.split == "uneven"){
    
    if (nsites > 1){
      
      pis[1] <- p.prime/2
      pis[2:nsites] <-  (p.prime / 2)/(nsites - 1)
      
    } else if (nsites == 1) {
      
      pis[1] <-  p.prime
    }
    
    pis[nsites + 1] <-  1 - p.prime
    
  } else if (pi.split == "ramp"){
    nt <- nsites*(nsites + 1)/2 # calculates nth triangular number = sum of sequences from 1 to nsites
    
    for (i in 1:nsites){
      pis[i] <- (i*p.prime)/nt
    }
    
    pis[nsites + 1] <-  1 - p.prime
  }
  
  return(pis)
}
