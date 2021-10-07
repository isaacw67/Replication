# Clear Envi
rm(list=ls())


# Import Data:
library(fixest)
library(lubridate)
library(tidyverse)

rfg_sub <- read_csv("intermediates/stata_resids.csv")
rfg_plotter <- tibble("residuals" = rfg_sub$out_resids, "year" = rfg_sub$year, "treated_rvp" = rfg_sub$rvpcty, "baseline" = rfg_sub$baseline, "treated_carb" = rfg_sub$carbcty, "treated_rfg" = rfg_sub$rfgcty)
rfg_plotter <- subset(rfg_plotter, baseline == 1 | treated_rfg == 1)


rfg_plotter <- mutate(group_by(select(rfg_plotter, residuals, year, treated_rfg), year, treated_rfg),
                  avg_residual = mean(residuals, na.rm = TRUE))

rfg_plot <- ggplot(subset(rfg_plotter, year <= 2003), aes(y=avg_residual, x=year, color = factor(treated_rfg))) +
  geom_line() +
  geom_vline(xintercept = 1995) +
  theme_bw()

plot(rfg_plot)


rvp_sub <- read_csv("intermediates/stata_resids.csv")
rvp_plotter <- tibble("residuals" = rvp_sub$out_resids, "year" = rvp_sub$year, "treated_rvp" = rvp_sub$rvpcty, "baseline" = rvp_sub$baseline, "treated_carb" = rvp_sub$carbcty, "treated_rfg" = rvp_sub$rfgcty)
rvp_plotter <- subset(rvp_plotter, baseline == 1 | (treated_rvp == 1 & treated_carb == 0 & treated_rfg == 0))


rvp_plotter <- mutate(group_by(select(rvp_plotter, residuals, year, treated_rvp), year, treated_rvp),
                      avg_residual = mean(residuals, na.rm = TRUE))

rvp_plot <- ggplot(subset(rvp_plotter, year <= 2003), aes(y=avg_residual, x=year, color = factor(treated_rvp))) +
  geom_line() +
  geom_vline(xintercept = 1992) +
  theme_bw()

drop(rvp_sub)
plot(rvp_plot)


