fit.mod <- function(sim.data,
                    model.file,
                    jags.controls = c(3, 1000, 500, 1, 5),
                    nsites = NA,
                    noccasions = NA,
                    nyears = NA,
                    model.type = "multinomial",
                    years.type = "single",
                    silent = TRUE){
  
  source("functions/scm_fun_is.jags.R")
  
  library("R2jags")
  
  nsites <- ifelse(!is.na(nsites),
                   nsites,
                   dim(sim.data)[1] - 1)
  
  noccasions <- ifelse(!is.na(noccasions),
                       noccasions,
                       dim(sim.data)[2])
  
  nyears <- ifelse(!is.na(nyears),
                   nyears,
                   dim(sim.data)[3])
  
  
  dim.list <- as.list(rep(x = TRUE, length(dim(sim.data))))
  dim.list[1] <- nsites + 1
  
  if(model.type == "multinomial"){
    
    y <- do.call('[<-', c(list(sim.data), dim.list, list(NA))) # thes2e lines overwrite the last observation of y in group nsites+1 (the "unobserved" population) with NA, regardless of dimensions of y
    
    nseen1 <- apply(y, 2:(length(dim(y))), sum, na.rm = TRUE)[1] # number of individuals seen on first occasion for initials
  
    } else if (model.type == "binomial"){
    
      ylong <- do.call('[<-', c(list(sim.data), dim.list, list(NA)))
      
      y <- apply(X = ylong, MARGIN = 2:(length(dim(ylong))), sum, na.rm = TRUE)
      
      nseen1 <- y[1]
  }
  
  
  CC <- 10000
  if(years.type == "single"){
    ones <- rep(x = 1, times = noccasions)
  } else if (years.type == "many"){
    ones <- matrix(data = 1, nrow = noccasions, ncol = nyears)
  }
  
  jags.data <- list("y", "nsites", "noccasions", "nyears", "ones", "CC")
  
  jags.params <- c("N", "pi", "r")
  
  
  lambda.inits <- c(1.1,1.5,2,3,5,7,9,20,50,100,1000)
  
  model.fit <- NA
  
    for (i in 1:length(lambda.inits)){
      
      jags.inits <- function(){
        list(lambda = nseen1*lambda.inits[i],
             lambda1 = nseen1*lambda.inits[i],
             r = 1)
      }
      
      try(model.fit <- jags(data = jags.data,
                            inits = jags.inits,
                            parameters.to.save = jags.params,
                            model.file = model.file,
                            n.chains = jags.controls[1],
                            n.iter = jags.controls[2],
                            n.burnin = jags.controls[3],
                            n.thin = jags.controls[4],
                            DIC = FALSE,
                            progress.bar = "none"),
          silent = silent)
      
      if(is.jags(model.fit) == TRUE) {break}
      
      
    }
  
  if(is.jags(model.fit) == TRUE){
    if(jags.controls[5] > 0){
      recompile(model.fit,
                progress.bar = "none")
      try(
        model.fit <- autojags(object = model.fit,
                              n.iter = jags.controls[2],
                              n.thin = jags.controls[4],
                              n.update = jags.controls[5],
                              progress.bar = "none"),
        silent = silent)
    }
  }
    

  return(model.fit)
  
}



