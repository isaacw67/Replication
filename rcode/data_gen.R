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
final_subset <- subset(final_subset, month %in% c(6, 7, 8) & year <= 2003 & ozone_max > 0)

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

south = c(10, 11, 12, 13, 24, 37, 45, 51, 54, 01, 21, 28, 47, 05, 22, 40, 48)
north = c(09, 23, 25, 33, 44, 50, 34, 36, 42)
midwest = c(18, 17, 26, 39, 55, 19, 20, 27, 29, 31, 38, 46)
west = c(04, 08, 16, 35, 30, 49, 32, 56, 02, 06, 15, 41, 53)


final_subset = mutate(final_subset, 
                   ever_treated = case_when(RVPCty == 1 ~ 1, 
                                            RFGCty == 1 ~ 1,
                                            CARBCty == 1 ~ 1,
                                            TRUE ~ 0),
                    baseline = case_when(ever_treated == 1 ~ 0,
                                         TRUE ~ 1),
                   census_region = case_when(state_code %in% north ~ 0,
                                        state_code %in% south ~ 1,
                                        state_code %in% midwest ~ 2,
                                        state_code %in% west ~ 3,
                                        TRUE ~ -1)
)

final_subset <- subset(final_subset, !(census_region == -1))

final_subset$tempmax_lag <- lag(final_subset$TempMax, n = 1)
final_subset$tempmin_lag <- lag(final_subset$TempMin, n = 1)
final_subset <- filter(final_subset, !(fips %in% fips_vec))

day_of_year <- function(date) {
  doy = week(date) + wday(date)
  doy
}

final_subset <- mutate(final_subset,
                       day_year = day_of_year(Date),
                       day_of_week = wday(Date)
)

# Replicate Figure 5:

# We want to plot residuals for different 
# regressions on RVP counties vs. baseline and RFG vs. baseline. 


# Lets get RFG vs. Baseline
final_subset <- arrange(final_subset, Date) # fips*statid, dow^cenr, cr^y, dow^Temp
fig5_sub <- drop_na(final_subset, any_of(c("TempMax", "TempMin", "Rain", "Snow")))
fig5_sub <- select(final_subset, any_of(c("TempMax", "TempMin", "Rain", "Snow", "day_year", "day_of_week",
                                              "tempmax_lag", "tempmin_lag", "CARBCty", "RFGCty", "RVPCty", "treat_rvpII", 
                                              "treat_rvpI", "treat_rfg", "treat_carb", "fips", "panelid",  "state_code", "year",
                                              "Date", "baseline", "ever_treated", "TreatRVPII", "ozone_max", "census_region", "county_code",
                                              "year", "site_id")))

write_csv(fig5_sub, "intermediates/fig_5.csv")
rm(fig5_sub)

## Lets get table 2 data

# Want, RFG, RVPI, RVPII, CARB, ln(ozone), income
# Need to do some work on income,

income$grouped = paste(income$state_code, income$county_code, income$year)
test<- subset(income, grouped == paste(1, 1, 1991))$income

length(test)
test[1]
get_income <- function(val) {
  temp <- filter(income, grouped == val)$income
  if (length(temp) > 1) {
    temp[1]
  }
  temp[1]
    
}



col1 <- mutate(subset(final_subset, ozone_max > 0),
               ln_ozone = log(ozone_max),
               inc = get_income(paste(state_code, county_code, year)),
               treat = case_when(CARBCty == 1 ~ 0,
                                 RFGCty == 1 ~ 1,
                                 RVPCty == 1 ~ 2,
                                 TRUE ~ 3))

col1 <- select(col1, any_of(c("TempMax", "TempMin", "Rain", "Snow", "day_year", "day_of_week",
                                          "tempmax_lag", "tempmin_lag", "CARBCty", "RFGCty", "RVPCty", "treat_rvpII", 
                                          "treat_rvpI", "treat_rfg", "treat_CARB", "fips", "panelid",  "state_code", "year",
                                          "Date", "baseline", "ever_treated", "TreatRVPII", "ozone_max", "census_region", "county_code",
                                          "year", "inc", "treat", "ln_ozone", "site_id")))
write_csv(col1, "intermediates/table2.csv")
rm(col1)
