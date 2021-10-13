# Clear Envi
rm(list=ls())


# Import Data:
library(fixest)
library(lubridate)
library(tidyverse)

sites <- c("cam", "tex", "mad")

for (num in c(1,2,3)) {
  fig6 <- read_csv(paste("intermediates/stata_fig_6_",sites[num],".csv", sep = ""))

  site_dat <- subset(fig6, out_resids >= -1 & out_resids <= 1 & year <= 2003)
  
  plt <- ggplot(site_dat, aes(date, out_resids + pred_zeroed)) +
    geom_point(size = 0.75) + 
    geom_line(color = "blue", data = site_dat, aes(x = date, y = pred_zeroed)) +
    theme_bw() +
    ggtitle(paste("Residuals+Predicted Values for", sites[num])) +
    xlab("Date") +
    ylab("Residual + Predicted Value")

  
  plot(plt)
}

cal_sites <- c("cal1", "cal2")
for (num in c(1,2)) {
  fig6 <- read_csv(paste("intermediates/stata_fig_8_",cal_sites[num],".csv", sep = ""))
  
  site_dat <- subset(fig6, out_resids >= -1 & out_resids <= 1 & year <= 2003)
  
  plt <- ggplot(site_dat, aes(date, out_resids + pred+zeored)) +
    geom_point(size = 0.75) + 
    geom_line(color = "blue", data = site_dat, aes(x = date, y = pred_zeroed)) +
    theme_bw() +
    ggtitle(paste("Residuals+Predicted Values for", cal_sites[num])) +
    xlab("Date") +
    ylab("Residual + Predicted Value")
  
  
  plot(plt)
}

