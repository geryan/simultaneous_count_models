library("R2jags")
library("tibble")
library("dplyr")
library("magrittr")
library("tidyr")
library("parallel")
library("jagstools")
library("purrr")


source("functions/scm_fun_make.pi.R")
source("functions/scm_fun_make.mod.R")
source("functions/scm_fun_make.dat.R")
source("functions/scm_fun_sim.dat.R")
source("functions/scm_fun_mod.spec2.R")
source("functions/scm_fun_is.jags.R")
source("functions/scm_fun_fit.mod.R")
source("functions/scm_fun_fit.sim2.R")
source("functions/scm_fun_spec.list2.R")
source("functions/scm_fun_fit.pll2.R")
source("functions/scm_fun_is.jags2.R")
source("functions/scm_fun_get.res2.R")
source("functions/scm_fun_sum.res2.R")

trend.specs.0.90 <- mod.spec2(occasions = c(2, 4, 8),
                              sites = c(1, 3, 7),
                              p.primes = c(0.1, 0.5, 0.9),
                              pi.split = "even",
                              pop.sizes = 1000,
                              growth.rates = c(0.90, 0.98, 1.00, 1.02, 1.10),
                              years = c(2, 4, 8),
                              years.type = "many",
                              pi.priors = c("alpha.stacked"),
                              truncs = 5000,
                              nsims = 100)


jags.controls <- c(3, 100000, 50000, 20, 3)



n.clusters <- 20

trend.fit.0.90 <- fit.pll2(sim.specs = trend.specs.0.90,
                           n.clusters = n.clusters,
                           jags.controls = jags.controls)


saveRDS(object = trend.fit.0.90, file = "output/trend.fit.0.90.Rds")

trend.res.0.90 <- get.res2(fit.specs = trend.fit.0.90)

saveRDS(object = trend.res.0.90, file = "output/trend.res.0.90.Rds")


trend.sum.0.90 <- sum.res2(res = trend.res.0.90)

saveRDS(object = trend.sum.0.90, file = "output/trend.sum.0.90.Rds")

