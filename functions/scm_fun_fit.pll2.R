fit.pll2 <- function(sim.specs, n.clusters, jags.controls = c(3, 1000, 500, 1, 5)){
  
  library("dplyr")
  library("parallel")
  
  source("functions/scm_fun_is.jags.R")
  source("functions/scm_fun_fit.mod.R")
  source("functions/scm_fun_fit.sim.R")
  source("functions/scm_fun_spec.list.R")
  
  slist <- spec.list2(sim.specs)
  
  assign("slist", slist, envir = .GlobalEnv)
  
  jags.controls <- jags.controls
  
  cl <- makeCluster(n.clusters)
  
  clusterExport(cl = cl, varlist = c("fit.sim", "slist", "jags.controls", "fit.mod", "is.jags"))
  
  sim.fit <- sim.specs %>%
    mutate(model.fit = parLapply(cl = cl,
                                 X = slist,
                                 fun = fit.sim2,
                                 jags.controls = jags.controls))
  
  stopCluster(cl)
  
  rm(slist, envir = .GlobalEnv)
  
  return(sim.fit)
  
}