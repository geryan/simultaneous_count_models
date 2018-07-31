## simspec is a function to take a series of vectors of input parameters and cre

## relies on functions `make.pi` `make.mod` 'sim.dat' and 'make.dat'

mod.spec2 <- function(occasions = c(2, 4, 8), # integer vector of number of sampling occasions
                    sites = c(1, 3, 7), # integer vector of number of sites
                    p.primes = c(0.1, 0.5, 0.9), # integer vector of probabilities an individual is unobserved = proportion of unobserved population
                    pi.split = "even", # string vector, "even", "uneven", or "ramp" where observed population is split evenly among sites, uneven with half of observed population in one site and the remainder evenly among the other sites, or ramp, where the observed population is split in a ration of 1:2:3:...:nsites
                    pop.sizes = 1000, # integer vector of (initial) population sizes
                    growth.rates = 1, # integer vector of growth rates
                    years = 1, # integer vector of number of year
                    years.type = "single", # character either "single" or "many" (seems redundant from `years` argument, but allows for the looped model to be used with a single year of data in a 3-d array, as opposed to single year in 2-d array for single year type)
                    pi.priors = "alpha.flat", #
                    N.priors = "lambda", #
                    truncs = NA,
                    nsims = 2
                    ){
  
  source("functions/scm_fun_make.pi.R")
  source("functions/scm_fun_make.mod.R")
  source("functions/scm_fun_make.dat.R")
  source("functions/scm_fun_sim.dat.R")
  
  library("tibble")
  library("tidyr")
  
  
  specs <- expand.grid(occasions, sites, p.primes, pi.split, pop.sizes, growth.rates, years, years.type, pi.priors, N.priors, truncs)
  
  colnames(specs) <- c("noccasions", "nsites", "p.prime", "pi.split", "N", "r", "nyears", "years.type", "pi.prior", "N.prior", "trunc")
  
  specs <- as.tibble(specs)
  
  pi <- list()
  sims <- list()
  model.file.m <- vector()
  model.file.b <- vector()
  
  
  
  for(i in 1:nrow(specs)){
    pi[[i]] <- make.pi(nsites = specs$nsites[i],
                        p.prime = specs$p.prime[i],
                        pi.split = specs$pi.split[i])
    
    sims[[i]] <- sim.dat(N = specs$N[i],
                         pi = pi[[i]],
                         noccasions = specs$noccasions[i],
                         nyears = specs$nyears[i],
                         r = specs$r[i],
                         years.type = specs$years.type[i],
                         nsims = nsims)
    
    model.file.m[i] <- make.mod(model.type = "multinomial",
                                years.type = specs$years.type[i],
                                pi.prior = specs$pi.prior[i],
                                N.prior = specs$N.prior[i],
                                trunc = specs$trunc[i])
    
    model.file.b[i] <- make.mod(model.type = "binomial",
                                years.type = specs$years.type[i],
                                pi.prior = specs$pi.prior[i],
                                N.prior = specs$N.prior[i],
                                trunc = specs$trunc[i])
    
    }
  
  specs$pi <- pi
  specs$sims <- sims
  specs$model.file.m <- model.file.m
  specs$model.file.b <- model.file.b
  
  specs <- unnest(specs, sims)
  
  
  
  return(specs)
  
}

