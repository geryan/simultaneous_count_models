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

pop.specs <- mod.spec2(occasions = c(2, 4, 8, 16, 32, 64),
                       sites = c(1, 3, 7),
                       p.primes = c(0.1, 0.5, 0.9),
                       pi.split = "even",
                       pop.sizes = c(10, 100, 1000, 10000),
                       years = 1,
                       years.type = "single",
                       pi.priors = c("alpha.stacked"),
                       truncs = NA,
                       nsims = 17)


jags.controls <- c(3, 100000, 50000, 20, 3)



n.clusters <- 20

pop.fit <- fit.pll2(sim.specs = pop.specs,
                    n.clusters = n.clusters,
                    jags.controls = jags.controls)


saveRDS(object = pop.fit, file = "output/pop.fit.Rds")

pop.res <- get.res2(fit.specs = pop.fit)

saveRDS(object = pop.res, file = "output/pop.res.Rds")


pop.sum <- sum.res2(res = pop.res)

saveRDS(object = pop.sum, file = "output/pop.sum.Rds")

