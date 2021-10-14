# Clear Envi
rm(list=ls())


# Import Data:
library(fixest)
library(lubridate)
library(tidyverse)

sites <- c("cam", "tex", "mad")
names <- c("Camden County", "Harris County", "Madison County")

for (num in c(1,2,3)) {
  fig6 <- read_csv(paste("intermediates/stata_fig_6_",sites[num],".csv", sep = ""))

  site_dat <- subset(fig6, out_resids >= -1 & out_resids <= 1 & year <= 2003)
  
  plt <- ggplot(site_dat, aes(date, out_resids + pred_zeroed)) +
    geom_point(size = 0.6, shape = 1) + 
    geom_line(color = "black", data = site_dat, aes(x = date, y = pred_zeroed), size = 1) +
    theme_bw() +
    ggtitle(paste("Residuals and Predicted Values for", names[num])) +
    xlab("Date") +
    ylab("Residual + Predicted Value")

  
  plot(plt)
  ggsave(file = paste(sites[num],"_plot.png", sep = ""), plt, path = "outputs", device = "png", width = 20, height = 20, units = "cm")
  
}

cal_sites <- c("cal1", "cal2")
cal_names <- c("Site 1201", "Site 1701")
for (num in c(1,2)) {
  fig6 <- read_csv(paste("intermediates/stata_fig_8_",cal_sites[num],".csv", sep = ""))
  
  site_dat <- subset(fig6, out_resids >= -1 & out_resids <= 1 & year <= 2003)
  
  plt <- ggplot(site_dat, aes(date, out_resids + pred_zeroed)) +
    geom_point(size = 0.6, shape = 1) + 
    geom_line(color = "black", data = site_dat, aes(x = date, y = pred_zeroed)) +
    theme_bw() +
    ggtitle(paste("Residuals and Predicted Values for", cal_names[num])) +
    xlab("Date") +
    ylab("Residual + Predicted Value")
  
  
  plot(plt)
  ggsave(file = paste(cal_sites[num],"_plot.png", sep = ""), plt, path = "outputs", device = "png", width = 20, height = 20, units = "cm")
  
  
}

