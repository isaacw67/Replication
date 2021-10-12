# Clear Envi
rm(list=ls())


# Import Data:
library(fixest)
library(lubridate)
library(tidyverse)

rfg_sub <- read_csv("intermediates/stata_resids.csv")
rfg_data <- tibble("residuals" = rfg_sub$out_resids, "year" = rfg_sub$year, "treated_rvp" = rfg_sub$rvpcty, "baseline" = rfg_sub$baseline, "treated_carb" = rfg_sub$carbcty, "treated_rfg" = rfg_sub$rfgcty)
rfg_data <- subset(rfg_data, baseline == 1 | (treated_rvp == 0 & treated_carb == 0 & treated_rfg == 1))


rfg_plotter <- rfg_data %>%
  group_by(year, treated_rfg, baseline) %>%
    summarise(mean = mean(residuals, na.rm = TRUE))

rfg_plot <- ggplot(subset(rfg_plotter, year <= 2003), aes(y=mean, x=year, color = factor(treated_rfg))) +
  geom_line() +
  geom_vline(xintercept = 1995) +
  theme_bw()

plot(rfg_plot)



rvp_data <- tibble("residuals" = rfg_sub$out_resids, "year" = rfg_sub$year, "treated_rvp" = rfg_sub$rvpcty, "baseline" = rfg_sub$baseline, "treated_carb" = rfg_sub$carbcty, "treated_rfg" = rfg_sub$rfgcty)
rvp_data <- subset(rvp_data, baseline == 1 | (treated_rvp == 1 & treated_carb == 0 & treated_rfg == 0))

rvp_plotter <- rvp_data %>% 
  group_by(year, treated_rvp, baseline) %>%
  summarise(mean = mean(residuals, na.rm = TRUE))

rvp_plot <- ggplot(subset(rvp_plotter, year <= 2003), aes(y=mean, x=year, color = factor(treated_rvp))) +
  geom_line() +
  geom_vline(xintercept = 1992) +
  theme_bw()


plot(rvp_plot)



carb_data <- tibble("residuals" = rfg_sub$out_resids, "year" = rfg_sub$year, "treated_rvp" = rfg_sub$rvpcty, "baseline" = rfg_sub$baseline, "treated_carb" = rfg_sub$carbcty, "treated_rfg" = rfg_sub$rfgcty)
carb_data <- subset(carb_data, baseline == 1 | (treated_rvp == 0 & treated_carb == 1 & treated_rfg == 0))


carb_plotter <- carb_data %>%
  group_by(year, treated_carb, baseline) %>%
  summarise(mean = mean(residuals, na.rm = TRUE))

carb_plot <- ggplot(subset(carb_plotter, year <= 2003), aes(y=mean, x=year, color = factor(treated_carb))) +
  geom_line() +
  geom_vline(xintercept = 1995) +
  theme_bw()

plot(carb_plot)
