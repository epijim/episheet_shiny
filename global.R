library(dplyr)
library(tidyr)
library(ggplot2)

jb_plotfunction <- function(
  est1.ll,
  est1.ul,
  est2.ll = NA,
  est2.ul = NA,
  label1 = "Estimate 1",
  label2 = "Estimate 2",
  xlabel = "Relative Risk",
  citype = "95%CI",
  labelsize = NULL,
  functionwidth = NULL,
  referencewidth = NULL
){
  # fake global variables to pass CMD check
  pvalue <- NULL
  updown <- NULL
  xvalue <- NULL
  zvalue <- NULL
  
  # which ci type?
  
  citype.pow <- ifelse(
    citype == "90%CI", 3.29,
    ifelse(citype == "99%CI", 5.16,
           3.84)
  )
  
  citype.pval <- ifelse(
    citype == "90%CI", 0.1,
    ifelse(citype == "99%CI", 0.01,
           0.05)
  )
  
  message(paste("Confidence interval type is ",citype))
  
  # calculate relative risk
  # Gardner M J and Altman D G. Statisitics with confidence.
  # BMJ publications. Reprint 1994 p 51-52
  jb_getrr <- function(ll,ul){
    exp((log(ll) + log(ul))/2)
  }
  
  # calculate standard errors
  jb_getse <- function(ll,ul,citype.pow){
    (log(ul) - log(ll))/citype.pow
  }
  
  # calculate curve value
  # up
  jb_getcurveup <- function(rr,se,pvalue){
    exp(log(rr) - pvalue*(se))
  }
  # down
  jb_getcurvedown <- function(rr,se,pvalue){
    exp(log(rr) + pvalue*(se))
  }
  
  # p-values of interest
  d.frame <- data.frame(
    # pvalues to plot
    pvalue = c(seq(0.01,1,0.01),seq(0.99,0.01,-0.01)),
    # formula changes if going up or down x values
    updown = c(rep("up",100),rep("down",99))
  ) %>%
    dplyr::mutate(
      zvalue = qnorm(1 - pvalue/2)
    )
  
  # breaks if less than 10 (in plot)
  jb_makebreaks <- function(
    xvalue
  ){
    minvalue <- min(xvalue, na.rm = T)
    maxvalue <- max(xvalue, na.rm = T)
    
    if (minvalue > 0.5) minvalue <- 0.49
    
    maxvalue <- ifelse(
      maxvalue < 3,
      3,
      ifelse(
        maxvalue < 5, 5,
        ifelse(
          maxvalue < 10, 10,
          maxvalue
        )
      )
    )
    
    xlimits <- c(minvalue,maxvalue)
    
    return(xlimits)
  }
  
  xbreaks <- c(0,0.5,1,1.5,2,5,10,15)
  
  
  
  
  # calculate the curves ------------------------------------------------
  ### If one estimate -------------------------------------------------
  if (is.na(est2.ll) | is.na(est2.ul)) {
    
    plot.data <- d.frame %>%
      dplyr::mutate(
        curve1 = ifelse(
          updown == "up",
          jb_getcurveup(
            jb_getrr(est1.ll,est1.ul),
            jb_getse(est1.ll,est1.ul,citype.pow),
            zvalue),
          jb_getcurvedown(
            jb_getrr(est1.ll,est1.ul),
            jb_getse(est1.ll,est1.ul,citype.pow),
            zvalue)
        )
      )
    
    # plot it
    xlimits <- jb_makebreaks(plot.data$curve1)
    
    plotout <- ggplot2::ggplot(plot.data,
                               ggplot2::aes_string('curve1', 'pvalue')
    ) +
      ggplot2::geom_line(size = functionwidth, colour = "#0033cc") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = labelsize),
        axis.title = ggplot2::element_text(size = labelsize)
        ) +
      ggplot2::ylab("p-value") +
      ggplot2::xlab(xlabel) +
      ggplot2::geom_vline(
        size = referencewidth,
        xintercept = 1,
        colour = "red",
        linetype = 2) +
      ggplot2::geom_hline(
        size = referencewidth,
        yintercept = citype.pval,
        colour = "red",
        linetype = 2) +
      ggplot2::scale_x_log10(
        breaks = xbreaks,
        limits = xlimits
      ) +
      ggplot2::scale_y_continuous(
        expand = c(0, 0), limits = c(0,1)
      )
  }
  
  ### If two estimates ---------------------------------------------------------------
  if (!is.na(est2.ll) | !is.na(est2.ul)) {
    d.frame <- d.frame %>%
      dplyr::mutate(
        curve1 = ifelse(
          updown == "up",
          jb_getcurveup(
            jb_getrr(est1.ll,est1.ul),
            jb_getse(est1.ll,est1.ul, citype.pow),
            zvalue),
          jb_getcurvedown(
            jb_getrr(est1.ll,est1.ul),
            jb_getse(est1.ll,est1.ul, citype.pow),
            zvalue)
        ),
        curve2 = ifelse(
          updown == "up",
          jb_getcurveup(
            jb_getrr(est2.ll,est2.ul),
            jb_getse(est2.ll,est2.ul, citype.pow),
            zvalue),
          jb_getcurvedown(
            jb_getrr(est2.ll,est2.ul),
            jb_getse(est2.ll,est2.ul, citype.pow),
            zvalue)
        )
      )
    
    # reshape data
    plot.data <- d.frame %>%
      dplyr::select(-c(zvalue,updown)) %>%
      tidyr::gather(curve,xvalue,-pvalue)
    
    # plot it
    xlimits <- jb_makebreaks(plot.data$xvalue)
    
    plotout <- ggplot2::ggplot(plot.data,
                               ggplot2::aes_string(
                                 'xvalue',
                                 'pvalue',
                                 colour = 'curve')
    ) +
      ggplot2::geom_line(size = 2) +
      ggplot2::theme_bw() +
      ggplot2::ylab("p-value") +
      ggplot2::xlab(xlabel) +
      ggplot2::geom_vline(
        xintercept = 1,
        colour = "red",
        linetype = 2) +
      ggplot2::geom_hline(
        yintercept = citype.pval,
        colour = "red",
        linetype = 2) +
      ggplot2::scale_color_manual(
        name = "Estimate: ",
        labels = c(label1, label2),
        values = c("#0033cc", "#00cc00")
      ) +
      ggplot2::theme(legend.position = "top") +
      ggplot2::scale_x_log10(
        breaks = xbreaks,
        limits = xlimits
      ) +
      ggplot2::scale_y_continuous(
        expand = c(0, 0), limits = c(0,1)
      )
  }
  return(plotout)
}