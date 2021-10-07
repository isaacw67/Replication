# Clear Envi
rm(list=ls())


# Import Data:
library(fixest)
library(lubridate)
library(tidyverse)
counties <- read_csv("data/AER20090377_CountyList.csv")
CNOx <- read_csv("data/AER20090377_CumulativeNOxInstallations.csv")
finaldata <- read_csv("data/AER20090377_FinalData.csv")
income <- read_csv("data/AER20090377_IncomeData.csv")
neighbor <- read_csv("data/AER20090377_NeighborData.csv")
seed <- 10
final_subset <- finaldata
rm(finaldata)
#final_subset <- sample_n(finaldata, size = 2000000)
final_subset$Date <- as.Date(final_subset$Date, format = "%d %b %Y")

# Clean data a bit

neighbor_treated = subset(neighbor, treated_neighbor == 1)
fips_vec <- neighbor_treated$fips
# Add neighbor_treated and ever treated to finaldata
#finaldata = mutate(rowwise(finaldata), 
#       neighbor_treated = case_when(fips %in% neighbor_treated ~ 1,
#                                    TRUE ~ 0),
#       ever_treated = case_when(1 %in% select(contains(c("treat"))) ~ 1,
#                                TRUE ~ 0)
#)

final_subset = mutate(final_subset, 
                   ever_treated = case_when(RVPCty == 1 ~ 1, 
                                            RFGCty == 1 ~ 1,
                                            CARBCty == 1 ~ 1,
                                            TRUE ~ 0),
                    baseline = case_when(ever_treated == 1 ~ 0,
                                         TRUE ~ 1),
)

final_subset$tempmax_lag <- lag(final_subset$TempMax, n = 1)
final_subset$tempmin_lag <- lag(final_subset$TempMin, n = 1)
final_subset <- filter(final_subset, !(fips %in% fips_vec))

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
final_subset <- arrange(final_subset, Date) # fips*statid, dow^cenr, cr^y, dow^Temp
final_subset <- drop_na(final_subset, any_of(c("TempMax", "TempMin", "Rain", "Snow")))
final_subset <- select(final_subset, any_of(c("TempMax", "TempMin", "Rain", "Snow", "day_year", "day_of_week",
                                              "tempmax_lag", "tempmin_lag", "CARBCty", "RFGCty", "RVPCty", "treat_rvpII", 
                                              "treat_rvpI", "treat_rfg", "treat_carb", "fips", "panelid",  "state_code", "year",
                                              "Date", "baseline", "ever_treated", "TreatRVPII", "ozone_max", "census_region", "county_code",
                                              "year")))

write_csv(final_subset, "intermediates/fig_5.csv")
# fips*statid, dow^cenr, cr^y, dow^Temp
# I've removed a bunch of interaction terms from the year_weather one since it ate too much memory
setFixest_fml(..weather = ~ poly(TempMax, 3, simple = TRUE) + (poly(TempMin, 3, simple = TRUE)) + 
                TempMax*TempMin + (poly(Snow, 2, simple = TRUE)) + (poly(Rain, 2, simple = TRUE)) + TempMax*Rain 
              + TempMax*tempmax_lag + TempMax*tempmin_lag + tempmax_lag + tempmin_lag,
              ..year_weather = ~ i(day_year, TempMax) + i(day_year, TempMin) +  i(day_year, Rain) + i(day_year, Snow),
              ..dates = ~ day_year + day_of_week,
              ..year_fe = ~ day_year^TempMax + day_year^TempMin + day_year^Rain + day_year^Rain + day_year^Snow,
              ..week_weather = ~ day_of_week^TempMax + day_of_week^TempMin + day_of_week^Rain + day_of_week^Snow
) 

rfg <- feols(ozone_max ~ ..weather + ..dates | fips^state_code + ..week_weather + county_code^year + ..year_fe,
             data = final_subset, nthreads = 5, mem.clean  = TRUE, cluster = c("state_code", "year"))

# fips*statid, dow^cenr, cr^y, dow^Temp
rfg_sub <- final_subset
rfg_sub <- slice(rfg_sub, rfg$obs_selection$subset)
rfg_sub <- slice(rfg_sub, rfg$obs_selection$obsRemoved)
rfg_plotter <- tibble("residuals" = rfg$residuals, "year" = rfg_sub$year, "treated_rvp" = rfg_sub$RVPCty, "baseline" = rfg_sub$baseline, "treated_carb" = rfg_sub$CARBCty, "treated_rfg" = rfg_sub$treat_rfg)
rfg_plotter <- subset(rfg_plotter, baseline == 1 | treated_rfg == 1)


rfg_plotter <- mutate(group_by(select(rfg_plotter, residuals, year, treated_rfg), year, treated_rfg),
                  avg_residual = mean(residuals, na.rm = TRUE))

rfg_plot <- ggplot(subset(rfg_plotter, year <= 2003), aes(y=avg_residual, x=year, color = factor(treated_rfg))) +
  geom_line() +
  geom_vline(xintercept = 1995) +
  theme_bw()

drop(rfg_sub)
plot(rfg_plot)


rvp_sub <- final_subset
rvp_sub <- slice(rvp_sub, rfg$obs_selection$subset)
rvp_sub <- slice(rvp_sub, rfg$obs_selection$obsRemoved)
rvp_plotter <- tibble("residuals" = rfg$residuals, "year" = rvp_sub$year, "treated_rvp" = rvp_sub$RVPCty, "baseline" = rvp_sub$baseline, "treated_carb" = rvp_sub$CARBCty, "treated_rfg" = rvp_sub$treat_rfg)
rvp_plotter <- subset(rvp_plotter, baseline == 1 | (treated_rvp == 1 & treated_carb == 0 & treated_rfg == 0))


rvp_plotter <- mutate(group_by(select(rvp_plotter, residuals, year, treated_rvp), year, treated_rvp),
                      avg_residual = mean(residuals, na.rm = TRUE))

rvp_plot <- ggplot(subset(rvp_plotter, year <= 2003), aes(y=avg_residual, x=year, color = factor(treated_rvp))) +
  geom_line() +
  geom_vline(xintercept = 1992) +
  theme_bw()

drop(rvp_sub)
plot(rvp_plot)


