
// import delimited "C:\Users\isaac\OneDrive\Documents\R\Replication\intermediates\fig_5.csv"
// import delimited "C:\Users\isaac\Documents\r\replication\intermediates\fig_5.csv"
clear
import delimited "intermediates\fig_5.csv", stringcols(6 19) numericcols(1 2 3 4 5 7 8 9 10 11 12 13 14 15 16 17 18 20 21 22 23 24 25)

local weather tempmax^3  tempmax^2  tempmax  tempmin^3  tempmin^2  tempmin ///
			tempman*tempmin  snow^2  snow  rain^2  rain  tempmax*rain  ///
			tempmax*tempmax_lag  tempmax*tempmin_lag  tempmax_lag  tempmin_lag ///
			
local dates  day_year  day_of_week

local year_fe i(day_year#`weather`')

local year_test day_year*tempmax^3 day_year*tempmax^2  day_year*tempmax  day_year*tempmin^3  day_year*tempmin^2  day_year*tempmin ///
			day_year*tempman*tempmin  day_year*snow^2  day_year*snow  day_year*rain^2  day_year*rain  day_year*tempmax*rain  ///
			day_year*tempmax*tempmax_lag  day_year*tempmax*tempmin_lag  day_year*tempmax_lag  day_year*tempmin_lag ///

local week_fe i.(day_of_week#tempmax)  i.(day_of_week#tempmin)  i.(day_of_week#rain)  i.(day_of_week#snow)

reghdfe ozone_max $weather $dates $year_test $week_fe, absorb(i.fips#i.site_id i.(census_region#year)) cluster(i.(state_code#year)) residuals(out_resids)

// Retrieve residuals:
export delimited using "intermediates\stata_resids.csv", replace
clear


local weather tempmax^3  tempmax^2  tempmax  tempmin^3  tempmin^2  tempmin ///
			tempman*tempmin  snow^2  snow  rain^2  rain  tempmax*rain  ///
			tempmax*tempmax_lag  tempmax*tempmin_lag  tempmax_lag  tempmin_lag ///
			
local dates  day_year  day_of_week

local year_fe i(day_year#`weather`')

local year_test day_year*tempmax^3 day_year*tempmax^2  day_year*tempmax  day_year*tempmin^3  day_year*tempmin^2  day_year*tempmin ///
			day_year*tempman*tempmin  day_year*snow^2  day_year*snow  day_year*rain^2  day_year*rain  day_year*tempmax*rain  ///
			day_year*tempmax*tempmax_lag  day_year*tempmax*tempmin_lag  day_year*tempmax_lag  day_year*tempmin_lag ///

local week_fe day_of_week#tempmax  day_of_week#tempmin  day_of_week#rain  day_of_week#snow

import delimited "intermediates\table2.csv"

reghdfe ln_ozone treat_rvpi treat_rvpii treat_rfg treat_carb, absorb(i.fips#i.site_id i.(census_region#year)) cluster(i.(state_code#year))

reghdfe ln_ozone treat_rvpi treat_rvpii treat_rfg treat_carb $weather, absorb(i.fips#i.state_code i.(census_region#year) i.(census_region#day_of_week)) cluster(i.(state_code#year))

reghdfe ln_ozone treat_rvpi treat_rvpii treat_rfg treat_carb $weather, absorb(i.fips#i.state_code i.(census_region#year) i.(census_region#day_of_week)) cluster(i.(state_code#year))