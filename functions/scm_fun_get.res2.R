get.res2 <- function(fit.specs){
  
  library("dplyr")
  library("jagstools")
  library("purrr")
  
  source("functions/scm_fun_get.jr.R")
  
  res.specs <- fit.specs %>%
    
    # get jags results out
    
    filter(map_lgl(.x = model.fit,
                   .f = is.jags2)) %>%
    mutate(m.mod.res = map(.x = model.fit,
                           .f = get.jr,
                           mod = "m"),
           b.mod.res = map(.x = model.fit,
                           .f = get.jr,
                           mod = "b")) %>%
    select(-model.fit) %>%
    
    # multinomial model results organisation
    
    mutate(m.mod.res = map(.x = m.mod.res,
                         .f = as.data.frame),
           m.mod.res = map(.x = m.mod.res,
                         .f = rownames_to_column,
                         var = "pm"),
           m.mod.res = map(.x = m.mod.res,
                         .f = as_tibble),
           m.mod.res = map(.x = m.mod.res,
                         .f = rename,
                         pc2.5 = `2.5%`,
                         pc25 = `25%`,
                         pc50 = `50%`,
                         pc75 = `75%`,
                         pc97.5 = `97.5%`),
           m.mod.res = map(.x = m.mod.res,
                         .f = ~ .x %>%
                           mutate(param = sub("\\[.*", "", pm))),
           m.mod.res = map(.x = m.mod.res,
                         .f = ~ .x %>%
                           mutate(pn = sub("\\]", "", sub(".*\\[", "", pm)))),
           m.mod.res = map(.x = m.mod.res,
                         .f = ~ .x %>%
                           mutate(pn = as.integer(pn))),
           m.rhatmax = map(.x = m.mod.res,
                         .f = ~ .x$Rhat),
           m.rhatmax = map_dbl(.x = m.rhatmax,
                             .f = max),
           m.N.mean = map_dbl(.x = m.mod.res,
                            .f = ~ .x %>%
                              filter(param == "N") %>%
                              filter(is.na(pn) | pn == 1) %>%
                              select(mean) %>%
                              unlist),
           m.N.2.5 = map_dbl(.x = m.mod.res,
                           .f = ~ .x %>%
                             filter(param == "N") %>%
                             filter(is.na(pn) | pn == 1) %>%
                             select(pc2.5) %>%
                             unlist),
           m.N.50 = map_dbl(.x = m.mod.res,
                          .f = ~ .x %>%
                            filter(param == "N") %>%
                            filter(is.na(pn) | pn == 1) %>%
                            select(pc50) %>%
                            unlist),
           m.N.97.5 = map_dbl(.x = m.mod.res,
                            .f = ~ .x %>%
                              filter(param == "N") %>%
                              filter(is.na(pn) | pn == 1) %>%
                              select(pc97.5) %>%
                              unlist),
           m.p.prime.mean = map_dbl(.x = m.mod.res,
                                  .f = ~ 1 - .x %>%
                                    filter(param == "pi") %>%
                                    filter(pn == max(pn)) %>%
                                    select(mean) %>%
                                    unlist),
           m.p.prime.2.5 = map_dbl(.x = m.mod.res,
                                 .f = ~ 1 - .x %>%
                                   filter(param == "pi") %>%
                                   filter(pn == max(pn)) %>%
                                   select(pc97.5) %>%
                                   unlist),
           m.p.prime.50 = map_dbl(.x = m.mod.res,
                                .f = ~ 1 - .x %>%
                                  filter(param == "pi") %>%
                                  filter(pn == max(pn)) %>%
                                  select(pc50) %>%
                                  unlist),
           m.p.prime.97.5 = map_dbl(.x = m.mod.res,
                                  .f = ~ 1 - .x %>%
                                    filter(param == "pi") %>%
                                    filter(pn == max(pn)) %>%
                                    select(pc2.5) %>%
                                    unlist),
           m.r.mean = map_dbl(.x = m.mod.res,
                            .f = ~ ifelse(dim(.x %>% filter(param == "r"))[1] == 0,
                                          NA_real_,
                                          .x %>%
                                            filter(param == "r") %>%
                                            select(mean) %>%
                                            unlist)),
           m.r.2.5 = map_dbl(.x = m.mod.res,
                           .f = ~ ifelse(dim(.x %>% filter(param == "r"))[1] == 0,
                                         NA_real_,
                                         .x %>%
                                           filter(param == "r") %>%
                                           select(pc2.5) %>%
                                           unlist)),
           m.r.50 = map_dbl(.x = m.mod.res,
                          .f = ~ ifelse(dim(.x %>% filter(param == "r"))[1] == 0,
                                        NA_real_,
                                        .x %>%
                                          filter(param == "r") %>%
                                          select(pc50) %>%
                                          unlist)),
           m.r.97.5 = map_dbl(.x = m.mod.res,
                            .f = ~ ifelse(dim(.x %>% filter(param == "r"))[1] == 0,
                                          NA_real_,
                                          .x %>%
                                            filter(param == "r") %>%
                                            select(pc97.5) %>%
                                            unlist))) %>%
    
    # binomial model manipulations
    
    mutate(b.mod.res = map(.x = b.mod.res,
                           .f = as.data.frame),
           b.mod.res = map(.x = b.mod.res,
                           .f = rownames_to_column,
                           var = "pm"),
           b.mod.res = map(.x = b.mod.res,
                           .f = as_tibble),
           b.mod.res = map(.x = b.mod.res,
                           .f = rename,
                           pc2.5 = `2.5%`,
                           pc25 = `25%`,
                           pc50 = `50%`,
                           pc75 = `75%`,
                           pc97.5 = `97.5%`),
           b.mod.res = map(.x = b.mod.res,
                           .f = ~ .x %>%
                             mutate(param = sub("\\[.*", "", pm))),
           b.mod.res = map(.x = b.mod.res,
                           .f = ~ .x %>%
                             mutate(pn = sub("\\]", "", sub(".*\\[", "", pm)))),
           b.mod.res = map(.x = b.mod.res,
                           .f = ~ .x %>%
                             mutate(pn = as.integer(pn))),
           b.rhatmax = map(.x = b.mod.res,
                           .f = ~ .x$Rhat),
           b.rhatmax = map_dbl(.x = b.rhatmax,
                               .f = max),
           b.N.mean = map_dbl(.x = b.mod.res,
                              .f = ~ .x %>%
                                filter(param == "N") %>%
                                filter(is.na(pn) | pn == 1) %>%
                                select(mean) %>%
                                unlist),
           b.N.2.5 = map_dbl(.x = b.mod.res,
                             .f = ~ .x %>%
                               filter(param == "N") %>%
                               filter(is.na(pn) | pn == 1) %>%
                               select(pc2.5) %>%
                               unlist),
           b.N.50 = map_dbl(.x = b.mod.res,
                            .f = ~ .x %>%
                              filter(param == "N") %>%
                              filter(is.na(pn) | pn == 1) %>%
                              select(pc50) %>%
                              unlist),
           b.N.97.5 = map_dbl(.x = b.mod.res,
                              .f = ~ .x %>%
                                filter(param == "N") %>%
                                filter(is.na(pn) | pn == 1) %>%
                                select(pc97.5) %>%
                                unlist),
           b.p.prime.mean = map_dbl(.x = b.mod.res,
                                    .f = ~ 1 - .x %>%
                                      filter(param == "pi") %>%
                                      filter(pn == max(pn)) %>%
                                      select(mean) %>%
                                      unlist),
           b.p.prime.2.5 = map_dbl(.x = b.mod.res,
                                   .f = ~ 1 - .x %>%
                                     filter(param == "pi") %>%
                                     filter(pn == max(pn)) %>%
                                     select(pc97.5) %>%
                                     unlist),
           b.p.prime.50 = map_dbl(.x = b.mod.res,
                                  .f = ~ 1 - .x %>%
                                    filter(param == "pi") %>%
                                    filter(pn == max(pn)) %>%
                                    select(pc50) %>%
                                    unlist),
           b.p.prime.97.5 = map_dbl(.x = b.mod.res,
                                    .f = ~ 1 - .x %>%
                                      filter(param == "pi") %>%
                                      filter(pn == max(pn)) %>%
                                      select(pc2.5) %>%
                                      unlist),
           b.r.mean = map_dbl(.x = b.mod.res,
                              .f = ~ ifelse(dim(.x %>% filter(param == "r"))[1] == 0,
                                            NA_real_,
                                            .x %>%
                                              filter(param == "r") %>%
                                              select(mean) %>%
                                              unlist)),
           b.r.2.5 = map_dbl(.x = b.mod.res,
                             .f = ~ ifelse(dim(.x %>% filter(param == "r"))[1] == 0,
                                           NA_real_,
                                           .x %>%
                                             filter(param == "r") %>%
                                             select(pc2.5) %>%
                                             unlist)),
           b.r.50 = map_dbl(.x = b.mod.res,
                            .f = ~ ifelse(dim(.x %>% filter(param == "r"))[1] == 0,
                                          NA_real_,
                                          .x %>%
                                            filter(param == "r") %>%
                                            select(pc50) %>%
                                            unlist)),
           b.r.97.5 = map_dbl(.x = b.mod.res,
                              .f = ~ ifelse(dim(.x %>% filter(param == "r"))[1] == 0,
                                            NA_real_,
                                            .x %>%
                                              filter(param == "r") %>%
                                              select(pc97.5) %>%
                                              unlist))) %>%
    
    # sorting
    
    mutate(m.rhatmax = unname(m.rhatmax),
           m.N.mean = unname(m.N.mean),
           m.N.2.5 = unname(m.N.2.5),
           m.N.50 = unname(m.N.50),
           m.N.97.5 = unname(m.N.97.5),
           m.p.prime.mean = unname(m.p.prime.mean),
           m.p.prime.2.5 = unname(m.p.prime.2.5),
           m.p.prime.50 = unname(m.p.prime.50),
           m.p.prime.97.5 = unname(m.p.prime.97.5),
           m.r.mean = unname(m.r.mean),
           m.r.2.5 = unname(m.r.2.5),
           m.r.50 = unname(m.r.50),
           m.r.97.5 = unname(m.r.97.5),
           b.rhatmax = unname(b.rhatmax),
           b.N.mean = unname(b.N.mean),
           b.N.2.5 = unname(b.N.2.5),
           b.N.50 = unname(b.N.50),
           b.N.97.5 = unname(b.N.97.5),
           b.p.prime.mean = unname(b.p.prime.mean),
           b.p.prime.2.5 = unname(b.p.prime.2.5),
           b.p.prime.50 = unname(b.p.prime.50),
           b.p.prime.97.5 = unname(b.p.prime.97.5),
           b.r.mean = unname(b.r.mean),
           b.r.2.5 = unname(b.r.2.5),
           b.r.50 = unname(b.r.50),
           b.r.97.5 = unname(b.r.97.5)) %>%
    
    mutate(m.N.in.ci = ifelse(N >= m.N.2.5 & N <= m.N.97.5, 1, 0),
           m.p.prime.in.ci = ifelse(p.prime >= m.p.prime.2.5 & p.prime <= m.p.prime.97.5, 1, 0),
           m.r.in.ci = ifelse(r >= m.r.2.5 & r <= m.r.97.5, 1, 0),
           m.N.prec = m.N.97.5 - m.N.2.5,
           m.p.prime.prec = m.p.prime.97.5 - m.p.prime.2.5,
           m.r.prec = m.r.97.5 - m.r.2.5,
           b.N.in.ci = ifelse(N >= b.N.2.5 & N <= b.N.97.5, 1, 0),
           b.p.prime.in.ci = ifelse(p.prime >= b.p.prime.2.5 & p.prime <= b.p.prime.97.5, 1, 0),
           b.r.in.ci = ifelse(r >= b.r.2.5 & r <= b.r.97.5, 1, 0),
           b.N.prec = b.N.97.5 - b.N.2.5,
           b.p.prime.prec = b.p.prime.97.5 - b.p.prime.2.5,
           b.r.prec = b.r.97.5 - b.r.2.5) %>%
    
    # m and b comparisons
    
    mutate(d.N.mean = m.N.mean - b.N.mean,
           d.p.prime.mean = m.p.prime.mean - b.p.prime.mean,
           d.r.mean = m.r.mean - b.r.mean,
           d.N.prec = m.N.prec - b.N.prec,
           d.p.prime.prec = m.p.prime.prec - b.p.prime.prec,
           d.r.prec = m.r.prec - b.r.prec)
    
  
  return(res.specs)
  
}