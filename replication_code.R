# Clear Envi
rm(list=ls())


# Import Data:
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(fixest)
counties <- read_csv("AER20090377_CountyList.csv")
CNOx <- read_csv("AER20090377_CumulativeNOxInstallations.csv")
finaldata <- read_csv("AER20090377_FinalData.csv")
income <- read_csv("AER20090377_IncomeData.csv")
neighbor <- read_csv("AER20090377_NeighborData.csv")
seed <- 10
final_subset <- finaldata
#final_subset <- sample_n(finaldata, size = 1000000)
drop(finaldata)
final_subset$Date <- as.Date(final_subset$Date, format = "%d %b %Y")

# Clean data a bit

neighbor_treated = subset(neighbor, treated_neighbor == 1)
neighbor_treated = select(neighbor_treated, c("fips"))

# Add neighbor_treated and ever treated to finaldata
#finaldata = mutate(rowwise(finaldata), 
#       neighbor_treated = case_when(fips %in% neighbor_treated ~ 1,
#                                    TRUE ~ 0),
#       ever_treated = case_when(1 %in% select(contains(c("treat"))) ~ 1,
#                                TRUE ~ 0)
#)

final_subset = mutate(rowwise(final_subset), 
                   neighbor_treated = case_when(fips %in% neighbor_treated ~ 1,
                                                TRUE ~ 0),
                   ever_treated = case_when(RVPCty == 1 ~ 1, 
                                            RFGCty == 1 ~ 1,
                                            CARBCty == 1 ~ 1,
                                            TRUE ~ 0),
                    baseline = case_when(ever_treated == 1 ~ 0,
                                         neighbor_treated == 1 ~ 0,
                                         TRUE ~ 1),
)

final_subset$tempmax_lag <- lag(final_subset$TempMax, n = 1)
final_subset$tempmin_lag <- lag(final_subset$TempMin, n = 1)

day_of_year <- function(date) {
  doy = week(date) + wday(date)
  doy
}

final_subset <- mutate(final_subset,
                       day_year = day_of_year(Date),
                       day_of_week = wday(Date, label = TRUE)
)

# Replicate Figure 5:

# We want to plot residuals for different 
# regressions on RVP counties vs. baseline and RFG vs. baseline. 


# Lets get RFG vs. Baseline
final_subset <- arrange(final_subset, Date)
final_subset <- subset(final_subset, neighbor_treated == 0)
final_subset$sy_code <- final_subset$year * final_subset$state_code

rfg <- feols(ozone_max ~ RFGCty+factor(panelid)+day_year+day_of_week+TempMax^3+TempMin^3+TempMax^2 + TempMax + TempMin^2 
             + TempMin +TempMax*TempMin+Rain^2+Snow^2+TempMax*Rain+TempMax*tempmax_lag+TempMax*tempmin_lag,
             data = final_subset, subset = final_subset$baseline == 1 || final_subset$RFGCty == 1, nthreads = 3, cluster = c("sy_code"))

rfg_sub <- slice(final_subset, -rfg$obsRemoved)
rfg_plotter <- tibble("residuals" = rfg$residuals, "year" = rfg_sub$year, "treated" = rfg_sub$RFGCty)


rfg_plotter <- mutate(group_by(select(rfg_plotter, residuals, year, treated), year, treated),
                  avg_residual = mean(residuals, na.rm = TRUE))

rfg_plot <- ggplot(subset(rfg_plotter, year <= 2003), aes(y=avg_residual, x=year, color = factor(treated))) +
  geom_line() +
  geom_vline(xintercept = 1995) +
  theme_bw()

drop(rfg_sub)
plot(rfg_plot)
rvp_sub <- subset(final_subset,(baseline == 1 | (RVPCty == 1 & RFGCty == 0 & CARBCty == 0)))
rvp <- feols(ozone_max ~ RVPCty+year+factor(panelid)+day_year+day_of_week+TempMax^3+TempMin^3+TempMax^2 + TempMax + TempMin^2 + TempMin + TempMax*TempMin+Rain^2+Snow^2+TempMax*Rain+TempMax*tempmax_lag+TempMax*tempmin_lag
          , data = rvp_sub, nthreads = 2)



rvp_sub <- slice(rvp_sub, -rvp$obsRemoved)
rvp_plotter <- tibble("residuals" = rvp$residuals, "year" = rvp_sub$year, "treated" = rvp_sub$RFGCty)


rvp_plotter <- mutate(group_by(select(rvp_plotter, residuals, year, treated), year, treated),
                      avg_residual = mean(residuals, na.rm = TRUE))

drop(rvp_sub)

 
rvp_plot <- ggplot(subset(rvp_plotter, year <= 2003), aes(y=avg_residual, x=year, color = factor(treated))) +
  geom_line() +
  geom_vline(xintercept = 1992) +
  theme_bw() 
  
plot(rvp_plot)

