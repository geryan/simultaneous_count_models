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

sites.specs <- mod.spec2(occasions = c(2, 4, 8, 16, 32, 64),
                         sites = c(1, 2, 3, 5, 7, 9),
                         p.primes = c(0.1, 0.5, 0.9),
                         pi.split = "even",
                         pop.sizes = 1000,
                         years = 1,
                         years.type = "single",
                         pi.priors = c("alpha.stacked"),
                         truncs = 5000,
                         nsims = 100)


jags.controls <- c(3, 100000, 50000, 20, 3)



n.clusters <- 20

sites.fit <- fit.pll2(sim.specs = sites.specs,
                      n.clusters = n.clusters,
                      jags.controls = jags.controls)


saveRDS(object = sites.fit, file = "output/sites.fit.Rds")

sites.res <- get.res2(fit.specs = sites.fit)

saveRDS(object = sites.res, file = "output/sites.res.Rds")


sites.sum <- sum.res2(res = sites.res)

saveRDS(object = sites.sum, file = "output/sites.sum.Rds")

