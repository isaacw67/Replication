# Clear Envi
rm(list=ls())


# Import Data:
library(fixest)
library(lubridate)
library(tidyverse)
library(data.table)

make_clean <- function() {
  print("| Making Clean Data")
  counties <- read_csv("data/AER20090377_CountyList.csv")
  CNOx <- read_csv("data/AER20090377_CumulativeNOxInstallations.csv")
  finaldata <- read_csv("data/AER20090377_FinalData.csv")
  income <- read_csv("data/AER20090377_IncomeData.csv")
  neighbor <- read_csv("data/AER20090377_NeighborData.csv")
  final_subset <- finaldata
  rm(finaldata)
  
  print("| Imported")
  final_subset$Date <- as.Date(final_subset$Date, format = "%d %b %Y")
  final_subset <- subset(final_subset, ozone_max > 0)
  
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
  
  
  final_subset <- mutate(final_subset,
                         day_year = yday(Date),
                         day_of_week = wday(Date)
  )

  
  final_subset <- mutate(subset(final_subset),
                 ln_ozone = log(ozone_max),
                 treat = case_when(CARBCty == 1 ~ 0,
                                   RFGCty == 1 ~ 1,
                                   RVPCty == 1 ~ 2,
                                   TRUE ~ 3))
  
  final_subset <- inner_join(final_subset, income, by = c("state_code" = "state_code", "county_code" = "county_code", 
                                          "year" = "year"))
  CNOx <- mutate(CNOx,
                 Date = as.Date(Date, format = "%d%b%y"),
                 CumNOx = case_when(month(Date) %in% c(6, 7, 8) ~ CumNOx,
                                  TRUE ~ 0)
          )
  
  final_subset <- inner_join(final_subset, CNOx, by = c("Date" = "Date"))
  print("| Saving clean")
  data.table::fwrite(final_subset, "intermediates/clean_data.csv")
  print("| Saved")
}

import_clean <- function() {
  return (read_csv("intermediates/clean_data.csv"))
}

speed_test <- function() {
  test<- read_csv("intermediates/clean_data.csv")
  setDTthreads(4)
  data.table::fwrite(test, "intermediates/clean_data.csv")
}

make_fig5_data <- function(final_subset) {
  final_subset <- import_clean()
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
                                                "year", "site_id", "income","ln_ozone")))
  
  print("| Saving fig5")
  data.table::fwrite(fig5_sub, "intermediates/fig_5.csv")
  print("| Saved")
  rm(fig5_sub)
}

make_table2 <- function(final_subset) {
  final_subset <- import_clean()
  ## Lets get table 2 data
  
  # Want, RFG, RVPI, RVPII, CARB, ln(ozone), income
  
  # Want only summer months
  final_subset <- subset(final_subset, month %in% c(6, 7, 8))
  
  

  col1 <- select(final_subset, any_of(c("TempMax", "TempMin", "Rain", "Snow", "day_year", "day_of_week",
                                            "tempmax_lag", "tempmin_lag", "CARBCty", "RFGCty", "RVPCty", "treat_rvpII", 
                                            "treat_rvpI", "treat_rfg", "treat_CARB", "fips", "panelid",  "state_code", "year",
                                            "Date", "baseline", "ever_treated", "TreatRVPII", "ozone_max", "census_region", "county_code",
                                            "year", "income", "treat", "ln_ozone", "site_id","epa_8hr")))
  print("| Saving table2")
  data.table::fwrite(col1, "intermediates/table2.csv")
  print("| Saved")
}

## Figure 6 data gen

make_poly <- function(sample_data) {
  year_start <- year(min(sample_data$Date, na.rm = TRUE))
  month_start <- min(subset(sample_data, year == year_start)$month, na.rm = TRUE)
  day_start <- min(subset(sample_data, year == year_start & month == month_start)$day, na.rm = TRUE)
  date_start <- dmy(paste(day_start, month_start, year_start, sep = "/"))
  
  cur <- mutate(sample_data,
                         days_since =  as.numeric(as.period(interval(date_start, Date), unit = "day"), "days")
  )
  
  date_poly <- polym(cur$days_since, degree = 8)
  
  # Add date_poly to sample:
  date_poly <- as_tibble(date_poly)
  date_poly <- rename(date_poly,
                      "date_1" = '1',
                      "date_2" = '2',
                      "date_3" = '3',
                      "date_4" = '4',
                      "date_5" = '5',
                      "date_6" = '6',
                      "date_7" = '7',
                      "date_8" = '8')
  
  cur <- bind_cols(cur, date_poly)
  cur
}

make_fig6 <- function() {
  final_subset <- import_clean()

  # We need eigth degree polynomials over the Date 
  # controls (doy and dow):
  cam_data <- filter(final_subset, site_id == 1001 & state_code == 34)
  mad_data <- filter(final_subset, site_id == 3007 & state_code == 17)
  tex_data <- filter(final_subset, site_id == 47 & state_code == 48)
  
  cam_data <- make_poly(cam_data)
  mad_data <- make_poly(mad_data)
  tex_data <- make_poly(tex_data)
  
  ## Cam, mad and tex will all be small enough to just write on their own:
  
  print("| Saving fig6")
  data.table::fwrite(rbind(cam_data, mad_data, tex_data), "intermediates/fig6.csv")
  print("| Saved")

}

make_fig8 <- function() {
  final_subset <- import_clean()
  

  cal1_data <- filter(final_subset, site_id == 1201 & state_code == 6)
  cal2_data <- filter(final_subset, site_id == 1701 & state_code == 6)
  
  cal1_data <- make_poly(cal1_data)
  cal2_data <- make_poly(cal2_data)
  print("| Saving fig8")
  data.table::fwrite(rbind(cal1_data, cal2_data), "intermediates/fig8.csv")
  print("| Saved")
  
}


## Controller:

run_data_get <- function(rp) {
  if (rp$clean) {
    make_clean()
  }
  
  if (rp$fig5) {
    make_fig5_data()
  }
  
  if (rp$table2) {
    make_table2()
  }
  
  if (rp$fig6) {
    make_fig6()
  }
  
  if (rp$fig8) {
    make_fig8()
  }
  
}


