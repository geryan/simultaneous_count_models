sum.res2 <- function(res, rhat.limit = 1.1){
  library("dplyr")
  library("tidyr")
  
  unul <- function(x){
    y <- unname(unlist(x))
  }
  
  res.sum <- res %>% gather(key,
                            value,
                            -noccasions,
                            -nsites,
                            -p.prime,
                            -pi.split,
                            -N,
                            -r,
                            -nyears,
                            -years.type,
                            -pi.prior,
                            -N.prior,
                            -trunc,
                            -model.file.m,
                            -model.file.b,
                            -nsim,
                            -sim.data) %>%
    extract(key, c("model", "thing"), "(.)\\.(.*)") %>%
    filter(model != "d") %>%
    spread(thing, value) %>%
    mutate_at(colnames(.)[18:36], unul) %>%
    filter(rhatmax <= rhat.limit) %>%
    group_by(noccasions,
             nsites,
             p.prime,
             pi.split,
             N,
             r,
             nyears,
             model,
             years.type,
             pi.prior,
             N.prior,
             trunc) %>%
    summarise(N.mean.mean = mean(N.mean),
              N.2.5.mean = mean(N.2.5),
              N.50.mean = mean(N.50),
              N.97.5.mean = mean(N.97.5),
              N.mean.2.5 = unname(quantile(N.mean, 0.025)),
              N.2.5.2.5 = unname(quantile(N.2.5, 0.025)),
              N.50.2.5 = unname(quantile(N.50, 0.025)),
              N.97.5.2.5 = unname(quantile(N.97.5, 0.025)),
              N.mean.97.5 = unname(quantile(N.mean, 0.975)),
              N.2.5.97.5 = unname(quantile(N.2.5, 0.975)),
              N.50.97.5 = unname(quantile(N.50, 0.975)),
              N.97.5.97.5 = unname(quantile(N.97.5, 0.975)),
              N.pc.in.ci = 100*sum(N.in.ci)/n(),
              N.prec.mean = mean(N.prec),
              p.prime.mean.mean = mean(p.prime.mean),
              p.prime.2.5.mean = mean(p.prime.2.5),
              p.prime.50.mean = mean(p.prime.50),
              p.prime.97.5.mean = mean(p.prime.97.5),
              p.prime.mean.2.5 = unname(quantile(p.prime.mean, 0.025)),
              p.prime.2.5.2.5 = unname(quantile(p.prime.2.5, 0.025)),
              p.prime.50.2.5 = unname(quantile(p.prime.50, 0.025)),
              p.prime.97.5.2.5 = unname(quantile(p.prime.97.5, 0.025)),
              p.prime.mean.97.5 = unname(quantile(p.prime.mean, 0.975)),
              p.prime.2.5.97.5 = unname(quantile(p.prime.2.5, 0.975)),
              p.prime.50.97.5 = unname(quantile(p.prime.50, 0.975)),
              p.prime.97.5.97.5 = unname(quantile(p.prime.97.5, 0.975)),
              p.prime.pc.in.ci = 100*sum(p.prime.in.ci)/n(),
              p.prime.prec.mean = mean(p.prime.prec),
              r.mean.mean = mean(r.mean),
              r.2.5.mean = mean(r.2.5),
              r.50.mean = mean(r.50),
              r.97.5.mean = mean(r.97.5),
              r.mean.2.5 = unname(quantile(r.mean, 0.025, na.rm = TRUE)),
              r.2.5.2.5 = unname(quantile(r.2.5, 0.025, na.rm = TRUE)),
              r.50.2.5 = unname(quantile(r.50, 0.025, na.rm = TRUE)),
              r.97.5.2.5 = unname(quantile(r.97.5, 0.025, na.rm = TRUE)),
              r.mean.97.5 = unname(quantile(r.mean, 0.975, na.rm = TRUE)),
              r.2.5.97.5 = unname(quantile(r.2.5, 0.975, na.rm = TRUE)),
              r.50.97.5 = unname(quantile(r.50, 0.975, na.rm = TRUE)),
              r.97.5.97.5 = unname(quantile(r.97.5, 0.975, na.rm = TRUE)),
              r.pc.in.ci = 100*sum(r.in.ci)/n(),
              r.prec.mean = mean(r.prec),
              rhatmax = max(rhatmax),
              nsim.converged = n()) %>%
    ungroup %>%
    mutate(model = case_when(model == "m" ~ "Multinomial",
                             model == "b" ~ "Binomial")) %>%
    mutate_at(colnames(.)[2:12], as.factor)
    
  
  return(res.sum)
  
}