make.mod <- function(model.type = "multinomial",
                     years.type = "single",
                     pi.prior = "alpha.flat",
                     N.prior = "lambda",
                     r.prior = "runif",
                     trunc = NA,
                     print.only = FALSE){
# data.brace
    
  if (model.type == "multinomial"){
    if (years.type == "single"){
      data.brace <- '
data{
  for (j in 1:noccasions){
    ones[j] <- 1
  }

CC <- 10000

}
      '
    } else if (years.type == "many"){
      data.brace <- '
data{
  for (j in 1:noccasions){
    for (k in 1:nyears){
      ones[j,k] <- 1
    }
  }

CC <- 10000

}      
      '
    }
  } else if (model.type == "binomial"){
    data.brace <- ''
  }

# model.open
  model.open <- '
model{  
  '
## pi.prior
  
  if (pi.prior == "alpha.flat"){
    if (model.type == "multinomial"){
      pi.prior <- '
for (i in 1:(nsites + 1)){
  alpha[i] <- 1
}

pi[1:(nsites + 1)] ~ ddirich(alpha[1:(nsites + 1)])      
      '
    } else if (model.type == "binomial"){
      pi.prior <- '
for (i in 1:2){
  alpha[i] <- 1
}
      
pi[1:2] ~ ddirich(alpha[1:2])      

p <-  pi[1]
      '
    } 

  } else if (pi.prior == "alpha.stacked"){
    if (model.type == "multinomial"){
      pi.prior <- '
        for (i in 1:nsites){
        alpha[i] <- (1/nsites)
        }
        alpha[nsites + 1] <- 1
        
        pi[1:(nsites + 1)] ~ ddirich(alpha[1:(nsites + 1)])      
        '
    } else if (model.type == "binomial"){
      pi.prior <- '
        for (i in 1:2){
        alpha[i] <- 1
        }
        
        pi[1:2] ~ ddirich(alpha[1:2])      
        
        p <-  pi[1]
        '
    }
  }

## N.prior

  if (N.prior == "lambda"){
    if(years.type == "single"){
      N.prior <- c('
lambda ~ dgamma(1e-6, 1e-6)',
                   ifelse(is.na(trunc),
                          '
                          ',
                          sprintf('T(,%s)
                                  ', trunc)),
                   '
N ~ dpois(lambda)
                   ')
    } else if (years.type == "many"){
      N.prior <- c('
lambda1 ~ dgamma(1e-6, 1e-6)',
                   ifelse(is.na(trunc),
                          '
                          ',
                          sprintf('T(,%s)
                                  ', trunc)),
                   '
for(k in 1:nyears){
  log(lambdak[k]) <-  log(lambda1) + log(r)*(k-1)
  N[k] ~ dpois(lambdak[k])
}
                   ')
    }
  }

# r.prior

  if(r.prior == "runif"){
    if(years.type == "many"){
      r.prior <- '
      r ~ dunif(0.7, 1.3)
      '
    } else if (years.type == "single"){
      r.prior <- ''
    }
  }


## likelihood

  if (model.type == "multinomial"){
    if(years.type == "single"){
      likelihood <- '
for (j in 1:noccasions){
  seen[j] <- sum(y[1:nsites,j])
  y[(nsites + 1),j] <- N - seen[j]
      
  for (i in 1:(nsites + 1)){
    lp[i,j] <- y[i,j]*log(pi[i]) - logfact(y[i,j])
  }
      
  lnL[j] <- sum(lp[1:(nsites+1),j]) + logfact(N)
      
  log(P[j]) <- lnL[j] - log(CC)
      
  ones[j] ~ dbern(P[j])
}
            '
    } else if (years.type == "many"){
      likelihood <- '
for (k in 1:nyears){
  for (j in 1:noccasions){
    seen[j,k] <- sum(y[1:nsites,j,k])
    y[(nsites + 1),j,k] <- N[k]- seen[j,k]
      
    for (i in 1:(nsites + 1)){
      lp[i,j,k] <- y[i,j,k]*log(pi[i]) - logfact(y[i,j,k])
    }
    
    lnL[j,k] <- sum(lp[1:(nsites +1),j,k]) + logfact(N[k])

    log(P[j,k]) <- lnL[j,k] - log(CC)

    ones[j,k] ~ dbern(P[j,k])
    }
}
      '
    }
  } else if (model.type == "binomial"){
    if(years.type == "single"){
      likelihood <- '
for (j in 1:noccasions){
  y[j] ~ dbin(p, N)
}
    '
    } else if (years.type == "many"){
      likelihood <- '
        for (j in 1:noccasions){
          for (k in 1:nyears){
            y[j, k] ~ dbin(p, N[k])
          }
        }
  '
    }
  }
  
# model.close
  model.close <- '
}
  '

# concatenate bits
 if (print.only == TRUE) {
   model.file <- cat(
     #data.brace,
     model.open,
     pi.prior,
     N.prior,
     r.prior,
     likelihood,
     model.close,
     sep = ''
   )
 } else if (print.only == FALSE){
   cat(
     #data.brace,
     model.open,
     pi.prior,
     N.prior,
     r.prior,
     likelihood,
     model.close,
     file = {model.file <- tempfile()},
     sep = ''
   )
 }
  
  return(model.file)
  
}