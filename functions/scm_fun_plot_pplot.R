pplot <- function(dat,
                  mod = "both",
                  pski = NA,
                  pfat = NA,
                  lfat = NA,
                  lski = NA,
                  the.cols = NA,
                  the.theme = NA,
                  the.lts = NA,
                  rib = NA,
                  bin = NA){
  
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
      if(is.na(bin)){
        the.lts <- "solid"
      } else if(bin == "bin"){
        the.lts <- c("solid", "12")
      }
    } else if(ns == 2){
      the.lts <- c("solid", "12")
    } else if(ns == 3){
      the.lts <- c("solid", "42", "24")
    } else if(ns == 6){
      the.lts <- c("solid", "51", "42", "33", "24", "15")
    }
  }
  
  lines <- data.frame(p.prime = as.factor(unique(dat$p.prime)),
                      int = as.numeric(as.character(unique(dat$p.prime))))
  
  the.plot <- ggplot(data = dat,
               aes(x = noccasions,
                   y = p.prime.mean.mean)) +
    geom_hline(aes(yintercept = int),
               lines,
               size = lfat)
  
  if(!is.na(rib)){
    if(rib == "mean.ci"){
      
      the.plot <- the.plot +
        geom_ribbon(aes(ymin = p.prime.mean.2.5,
                        ymax = p.prime.mean.97.5,
                        linetype = nsites,
                        colour = p.prime),
                    alpha = 0.05)
      
    } else if(rib == "ci.mean"){
      the.plot <- the.plot +
        geom_ribbon(aes(ymin = p.prime.2.5.mean,
                        ymax = p.prime.97.5.mean,
                        linetype = nsites,
                        colour = p.prime),
                    alpha = 0.05)
      
    }
  }
  
  if(is.na(bin)){
    the.plot <- the.plot +
      geom_line(aes(colour = p.prime,
                    linetype = nsites),
                size = lski) +
      scale_linetype_manual(values = the.lts,
                            guide = guide_legend(title = "Number\nof sites"))
  } else if(bin == "bin"){
    the.plot <- the.plot +
      geom_line(aes(colour = p.prime,
                    linetype = model),
                size = lski) +
      scale_linetype_manual(values = the.lts,
                            guide = guide_legend(title = "Model"))
  }
  
      the.plot <- the.plot +
      scale_y_continuous(limits = c(0, NA)) +
      scale_x_continuous(trans = "log2", breaks = c(2, 4, 8, 16, 32, 64)) +
      scale_colour_manual(values = the.cols,
                          guide = guide_legend(title = expression(italic("p'")))) +
      labs(x="Sampling occasions",
           y = expression(paste("Mean estimated net\nprobability of detection, ",
                                italic(" p'")))) +
      the.theme +
      theme(plot.margin = unit(c(6, 6, 6, 19), "points"))
  
  
  
  if(mod == "both"){
    if(ny == 1){
      if(is.na(bin)) {
        the.plot <- the.plot +
          facet_grid(model ~ p.prime)
      } else if(bin == "bin"){
        the.plot <- the.plot +
          facet_grid(. ~ p.prime)
      }
    } else if(ny > 1){
      the.plot <- the.plot +
        facet_grid(model + nyears ~ p.prime)
    }
  } else {
    if(ny == 1){
      the.plot <- the.plot +
        facet_grid(. ~ p.prime)
    } else if(ny > 1){
      the.plot <- the.plot +
        facet_grid(nyears ~ p.prime)
    }
  }
  
  return(the.plot)
  
}


