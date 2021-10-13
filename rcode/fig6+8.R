# Clear Envi
rm(list=ls())


# Import Data:
library(fixest)
library(lubridate)
library(tidyverse)

fig6 <- read_csv("intermediates/stata_fig_6.csv")
sites <- unique(fig6$site_id)

site_dat <- subset(fig6, site_id == sites[1])
site_dat <- mutate(rowwise(site_dat),
               y_ax = out_resids + days_since*date_1 + days_since*date_2^2 + days_since*date_3^3 + days_since*date_4^4 + days_since*date_5^5 + days_since*date_6^6 + days_since*date_7^7 + days_since*date_8^8
               )

plt <- ggplot(site_dat, aes(date, y_ax)) +
  geom_point()

plot(plt)
