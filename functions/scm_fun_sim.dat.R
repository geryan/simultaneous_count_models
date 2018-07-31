sim.dat <- function(N,
                    pi,
                    noccasions,
                    nyears = 1,
                    r = 1,
                    years.type = "single",
                    nsims = 2){
  
  sim.data <- list()
  
  for (i in 1:nsims){
    
    sim.data[[i]] <- make.dat(N = N,
                              pi = pi,
                              noccasions = noccasions,
                              nyears = nyears,
                              r = r,
                              years.type = years.type)
  }
  
  sim.tbl <- tibble(nsim = c(1:nsims), sim.data = sim.data)
  
  return(sim.tbl)
  
}



