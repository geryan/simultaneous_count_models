spec.list2 <- function(mod.specs){
  
  sl <- split(mod.specs[,c("model.file.m", "model.file.b", "years.type", "sim.data")],
              seq(nrow(mod.specs[,c("model.file.m", "model.file.b", "years.type", "sim.data")])))

  return(sl)
}