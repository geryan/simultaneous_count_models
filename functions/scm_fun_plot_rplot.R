rplot <- function(dat,
                  mod = "both",
                  pski = NA,
                  pfat = NA,
                  lfat = NA,
                  lski = NA,
                  the.cols = NA,
                  the.theme = NA,
                  the.lts = NA){
  
  library("dplyr")
  library("ggplot2")

  ns <- length(unique(dat$nsites))
  ny <- length(unique(dat$nyears))
  np <- length(unique(dat$p.prime))
    
  if(mod == "m"){
    dat <- dat %>%
      filter(model == "Multinomial")
  }
  
  if(mod == "b"){
    dat <- dat %>%
      filter(model == "Binomial")
  }
  
  pski <- ifelse(is.na(pski), 2, pski)
  pfat <- ifelse(is.na(pfat), 5, pfat)
  lski <- ifelse(is.na(lski), 0.6, lski)
  lfat <- ifelse(is.na(lfat), 0.6, lfat)
  
  if(is.na(the.cols)){
    if(np == 1){
      the.cols <- c("grey68")
    } else if(np == 3){
      the.cols <- c("grey68", "sienna2", "red4")
    } else if(np == 5){
      the.cols <- c("grey68", "yellow2", "sienna2", "tomato", "red4")
    }
  }
  
  
  if(is.na(the.theme)){
    the.theme <- theme_classic() + theme( axis.line.x = element_line(colour = "grey50"),
                                          axis.line.y = element_line(colour = "grey50"),
                                          panel.border = element_rect(colour = "grey", fill = NA, size = 0.4),
                                          strip.background = element_rect(colour = "grey", fill = NA, size = 0.4))
  }
  

  
  if(is.na(the.lts)){
    if(ns == 1){
      the.lts <- "solid"
    } else if(ns == 2){
      the.lts <- c("solid", "12")
    } else if(ns == 3){
      the.lts <- c("solid", "42", "24")
    } else if(ns == 6){
      the.lts <- c("solid", "51", "42", "33", "24", "15")
    }
  }
  
  
  rfs <- as.factor(unique(dat$r))
  rns <- as.numeric(as.character(unique(dat$r)))
  yfs <- as.factor(unique(dat$nyears))
  
  lines <- data.frame(int    = rep(x = rns, each = length(yfs)),
                      r      = rep(x = rfs, each = length(yfs)),
                      nyears = rep(x = yfs, each = length(rfs)))
  
  
  
  the.plot <- ggplot(data = dat,
                     aes(x = noccasions,
                         y = r.mean.mean)) +
    geom_hline(aes(yintercept = int),
               lines,
               size = lfat) +
    geom_line(aes(colour = p.prime,
                  linetype = nsites),
              size = lski) +
    scale_y_continuous(breaks = c(0.90, 1, 1.1)) +
    scale_x_continuous(trans = "log2", breaks = c(2, 4, 8, 16, 32, 64)) +
    scale_colour_manual(values = the.cols,
                        guide = guide_legend(title = expression(italic("p'")))) +
    scale_linetype_manual(values = the.lts,
                          guide = guide_legend(title = "Number of sites")) +
    labs(x="Sampling occasions",
         y = "Mean estimated abundance") +
    the.theme
  
  
  if(mod == "both"){
      the.plot <- the.plot +
        facet_grid(model + nyears ~ r)
  } else {
    the.plot <- the.plot +
        facet_grid(nyears ~ r)
  }
  
  return(the.plot)
  
}


