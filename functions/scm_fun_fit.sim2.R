fit.sim2 <- function(sim.specs,
                    jags.controls = c(3, 1000, 500, 1, 5)){
  
  library("tibble")
  
  source("functions/scm_fun_fit.mod.R")
  
  model.fit.m <- fit.mod(sim.data = sim.specs$sim.data[[1]],
                         model.file = sim.specs$model.file.m,
                         jags.controls = jags.controls,
                         model.type = "multinomial",
                         years.type = sim.specs$years.type)
  
  model.fit.b <- fit.mod(sim.data = sim.specs$sim.data[[1]],
                         model.file = sim.specs$model.file.b,
                         jags.controls = jags.controls,
                         model.type = "binomial",
                         years.type = sim.specs$years.type)
  
  model.fit <- tibble(model.fit.m = model.fit.m,
                      model.fit.b = model.fit.b)
  
  return(model.fit)
}