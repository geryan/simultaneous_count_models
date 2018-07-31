sum.cmp <- function(res){
  library("dplyr")
  
  res.cmp <- res %>%
    filter(m.rhatmax <= 1.1 & b.rhatmax <= 1.1) %>%
    group_by(noccasions,
             nsites,
             p.prime,
             pi.split,
             N,
             r,
             nyears,
             years.type,
             pi.prior,
             N.prior,
             trunc) %>%
    summarise(d.N.mean.mean = mean(d.N.mean),
              d.N.mean.2.5 = unname(quantile(d.N.mean, 0.025)),
              d.N.mean.97.5 = unname(quantile(d.N.mean, 0.975)),
              d.N.prec.mean = mean(d.N.prec),
              d.p.prime.mean.mean = mean(d.p.prime.mean),
              d.p.prime.mean.2.5 = unname(quantile(d.p.prime.mean, 0.025)),
              d.p.prime.mean.97.5 = unname(quantile(d.p.prime.mean, 0.975)),
              d.p.prime.prec.mean = mean(d.p.prime.prec),
              d.r.mean.mean = mean(d.r.mean),
              d.r.mean.2.5 = unname(quantile(d.r.mean, 0.025, na.rm = TRUE)),
              d.r.mean.97.5 = unname(quantile(d.r.mean, 0.975, na.rm = TRUE)),
              d.r.prec.mean = mean(d.r.prec),
              #
              nsim.converged = n()) %>%
    ungroup %>%
    mutate_at(colnames(.)[1:11], as.factor)
  
  return(res.cmp)
  
}