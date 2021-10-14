
// import delimited "C:\Users\isaac\OneDrive\Documents\R\Replication\intermediates\fig_5.csv"
// import delimited "C:\Users\isaac\Documents\r\replication\intermediates\fig_5.csv"
clear
import delimited "intermediates\fig_5.csv", stringcols(19) numericcols(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 20 21 22 23 24 25)

local weather tempmaxcube tempmaxsq tempmax tempmincube tempminsq tempmin ///
			c.tempmax#c.tempmin snowsq snow rainsq rain c.tempmax#c.rain ///
			c.tempmax#c.tempmax_lag c.tempmax#c.tempmin_lag tempmax_lag tempmin_lag ///
			
local dates i.day_year i.day_of_week

local year_int day_year#c.tempmaxcube day_year#c.tempmaxsq day_year#c.tempmax day_year#c.tempmincube day_year#c.tempminsq day_year#c.tempmin ///
			day_year#c.tempmax#c.tempmin day_year#c.snowsq day_year#c.snow day_year#c.rainsq day_year#c.rain day_year#c.tempmax#c.rain ///
			day_year#c.tempmax#c.tempmax_lag day_year#c.tempmax#c.tempmin_lag day_year#c.tempmax_lag day_year#c.tempmin_lag ///

local week_int i.day_of_week#c.tempmax i.day_of_week#c.tempmin i.day_of_week#c.rain i.day_of_week#c.snow


// use i.(census_region#year) maybe?

reghdfe ozone_max rvpcty rfgcty carbcty $weather $dates $year_int $week_int income, absorb(i.fips#i.site_id) cluster(i.(state_code#year)) residuals(out_resids)


// Retrieve residuals:
export delimited using "intermediates\stata_resids.csv", replace


clear

local weather tempmaxcube tempmaxsq tempmax tempmincube tempminsq tempmin ///
			c.tempmax#c.tempmin snowsq snow rainsq rain c.tempmax#c.rain ///
			c.tempmax#c.tempmax_lag c.tempmax#c.tempmin_lag tempmax_lag tempmin_lag ///
			
local dates day_year day_of_week

local year_int day_year#c.tempmaxcube day_year#c.tempmaxsq day_year#c.tempmax day_year#c.tempmincube day_year#c.tempminsq day_year#c.tempmin ///
			day_year#c.tempmax#c.tempmin day_year#c.snowsq day_year#c.snow day_year#c.rainsq day_year#c.rain day_year#c.tempmax#c.rain ///
			day_year#c.tempmax#c.tempmax_lag day_year#c.tempmax#c.tempmin_lag day_year#c.tempmax_lag day_year#c.tempmin_lag ///

local week_int i.day_of_week#c.tempmax i.day_of_week#c.tempmin i.day_of_week#c.rain i.day_of_week#c.snow

import delimited "intermediates\table2.csv"

// Fix epa_8hr 
destring epa_8hr, generate(epa_8hr_num) force

// Col 1
reghdfe ln_ozone treat_rvpi treat_rvpii treat_rfg treat_carb, absorb(i.fips#i.site_id i.(census_region#year)) cluster(i.(state_code#year))
eststo col1

// Col 2
reghdfe ln_ozone treat_rvpi treat_rvpii treat_rfg treat_carb $weather $year_int , absorb(i.fips#i.state_code i.(census_region#year) i.(census_region#day_of_week)) cluster(i.(state_code#year))
eststo col2

// Col 3
reghdfe ln_ozone treat_rvpi treat_rvpii treat_rfg treat_carb income $weather $year_int, absorb(i.fips#i.state_code i.(census_region#year) i.(census_region#day_of_week)) cluster(i.(state_code#year))
eststo col3

// Col 6
reghdfe ln(epa_8hr) treat_rvpi treat_rvpii treat_rfg treat_carb, absorb(i.fips#i.site_id i.(census_region#year)) cluster(i.(state_code#year))
eststo col6

// Col 7 

reghdfe ln(epa_8hr) treat_rvpi treat_rvpii treat_rfg treat_carb $weather $year_int , absorb(i.fips#i.state_code i.(census_region#year) i.(census_region#day_of_week)) cluster(i.(state_code#year))
eststo col7

esttab using table2.tex, replace label title(Table 2) nonumbers mtitles("Column 1" "Column 2" "Column 3" "Column 6" "Column 7") 



// Now for figures 6 and 8:



clear
import delimited "intermediates\fig6_cam.csv"

// Redefine locals
local weather tempmaxcube tempmaxsq tempmax tempmincube tempminsq tempmin ///
			c.tempmax#c.tempmin snowsq snow rainsq rain c.tempmax#c.rain ///
			c.tempmax#c.tempmax_lag c.tempmax#c.tempmin_lag tempmax_lag tempmin_lag ///
			
local dates day_year day_of_week

local year_int day_year#c.tempmaxcube day_year#c.tempmaxsq day_year#c.tempmax day_year#c.tempmincube day_year#c.tempminsq day_year#c.tempmin ///
			day_year#c.tempmax#c.tempmin day_year#c.snowsq day_year#c.snow day_year#c.rainsq day_year#c.rain day_year#c.tempmax#c.rain ///
			day_year#c.tempmax#c.tempmax_lag day_year#c.tempmax#c.tempmin_lag day_year#c.tempmax_lag day_year#c.tempmin_lag ///

local week_int i.day_of_week#c.tempmax i.day_of_week#c.tempmin i.day_of_week#c.rain i.day_of_week#c.snow
			
local season_int i.season#c.tempmaxcube i.season#c.tempmaxsq i.season#c.tempmax i.season#c.tempmincube i.season#c.tempminsq i.season#c.tempmin ///
			i.season#c.tempmax#c.tempmin i.season#c.snowsq i.season#c.snow i.season#c.rainsq i.season#c.rain i.season#c.tempmax#c.rain ///
			i.season#c.tempmax#c.tempmax_lag i.season#c.tempmax#c.tempmin_lag i.season#c.tempmax_lag i.season#c.tempmin_lag ///


// We can add month dummies and 

gen dates = date(date, "YMD")
gen DateS = dates / 365 /* scale */
egen MaxDate = max(DateS)
egen MinDate = min(DateS)
gen Z = 2 * (DateS - MinDate) / (MaxDate - MinDate) - 1
gen Time1 = Z 
gen Time2 = 2 * Z^2 - 1
gen Time3 = 4 * Z^3 - 3 * Z
gen Time4 = 2 * Z * Time3 - Time2
gen Time5 = 2 * Z * Time4 - Time3
gen Time6 = 2 * Z * Time5 - Time4
gen Time7 = 2 * Z * Time6 - Time5
gen Time8 = 2 * Z * Time7 - Time6
gen Time9 = 2 * Z * Time8 - Time7
gen Time10 = 2 * Z * Time9 - Time8
drop Z


reghdfe ln_ozone $season_int treat_rfg $weather $year_int i.month Time1 Time2 Time3 Time4 Time5 Time6 Time7 Time8 Time9 Time10 cumnox, noabsorb cluster(i.year#i.season) residuals(out_resids)

gen pred_val = _b[treat_rfg] * treat_rfg + _b[Time1]*Time1 + _b[Time2]*Time2 + _b[Time3] * Time3 + _b[Time4] * Time4 + _b[Time5] * Time5 + _b[Time6] * Time6 + _b[Time7]*Time7 + _b[Time8]*Time8 + _b[Time9]*Time9 + _b[Time10]*Time10


egen means = mean(pred_val)
gen pred_zeroed = (pred_val-means)

export delimited using "intermediates\stata_fig_6_cam.csv", replace

// MADISON

clear
import delimited "intermediates\fig6_mad.csv"

// Redefine locals
local weather tempmaxcube tempmaxsq tempmax tempmincube tempminsq tempmin ///
			c.tempmax#c.tempmin snowsq snow rainsq rain c.tempmax#c.rain ///
			c.tempmax#c.tempmax_lag c.tempmax#c.tempmin_lag tempmax_lag tempmin_lag ///
			
local dates day_year day_of_week

local year_int day_year#c.tempmaxcube day_year#c.tempmaxsq day_year#c.tempmax day_year#c.tempmincube day_year#c.tempminsq day_year#c.tempmin ///
			day_year#c.tempmax#c.tempmin day_year#c.snowsq day_year#c.snow day_year#c.rainsq day_year#c.rain day_year#c.tempmax#c.rain ///
			day_year#c.tempmax#c.tempmax_lag day_year#c.tempmax#c.tempmin_lag day_year#c.tempmax_lag day_year#c.tempmin_lag ///

local week_int i.day_of_week#c.tempmax i.day_of_week#c.tempmin i.day_of_week#c.rain i.day_of_week#c.snow
			
local season_int i.season#c.tempmaxcube i.season#c.tempmaxsq i.season#c.tempmax i.season#c.tempmincube i.season#c.tempminsq i.season#c.tempmin ///
			i.season#c.tempmax#c.tempmin i.season#c.snowsq i.season#c.snow i.season#c.rainsq i.season#c.rain i.season#c.tempmax#c.rain ///
			i.season#c.tempmax#c.tempmax_lag i.season#c.tempmax#c.tempmin_lag i.season#c.tempmax_lag i.season#c.tempmin_lag ///


// We can add month dummies and 

gen dates = date(date, "YMD")
gen DateS = dates / 365 /* scale */
egen MaxDate = max(DateS)
egen MinDate = min(DateS)
gen Z = 2 * (DateS - MinDate) / (MaxDate - MinDate) - 1
gen Time1 = Z 
gen Time2 = 2 * Z^2 - 1
gen Time3 = 4 * Z^3 - 3 * Z
gen Time4 = 2 * Z * Time3 - Time2
gen Time5 = 2 * Z * Time4 - Time3
gen Time6 = 2 * Z * Time5 - Time4
gen Time7 = 2 * Z * Time6 - Time5
gen Time8 = 2 * Z * Time7 - Time6
gen Time9 = 2 * Z * Time8 - Time7
gen Time10 = 2 * Z * Time9 - Time8
drop Z


reghdfe ln_ozone $season_int treat_rvpii $weather $year_int i.month Time1 Time2 Time3 Time4 Time5 Time6 Time7 Time8 Time9 Time10, noabsorb cluster(i.year#i.season) residuals(out_resids)

gen pred_val = _b[treat_rvpii] * treat_rvpii+ _b[Time1]*Time1 + _b[Time2]*Time2 + _b[Time3] * Time3 + _b[Time4] * Time4 + _b[Time5] * Time5 + _b[Time6] * Time6 + _b[Time7]*Time7 + _b[Time8]*Time8 + _b[Time9]*Time9 + _b[Time10]*Time10


egen means = mean(pred_val)
gen pred_zeroed = (pred_val-means)

export delimited using "intermediates\stata_fig_6_mad.csv", replace



// HARRIS TEXAS

clear
import delimited "intermediates\fig6_tex.csv"

// Redefine locals
local weather tempmaxcube tempmaxsq tempmax tempmincube tempminsq tempmin ///
			c.tempmax#c.tempmin snowsq snow rainsq rain c.tempmax#c.rain ///
			c.tempmax#c.tempmax_lag c.tempmax#c.tempmin_lag tempmax_lag tempmin_lag ///
			
local dates day_year day_of_week

local year_int day_year#c.tempmaxcube day_year#c.tempmaxsq day_year#c.tempmax day_year#c.tempmincube day_year#c.tempminsq day_year#c.tempmin ///
			day_year#c.tempmax#c.tempmin day_year#c.snowsq day_year#c.snow day_year#c.rainsq day_year#c.rain day_year#c.tempmax#c.rain ///
			day_year#c.tempmax#c.tempmax_lag day_year#c.tempmax#c.tempmin_lag day_year#c.tempmax_lag day_year#c.tempmin_lag ///

local week_int i.day_of_week#c.tempmax i.day_of_week#c.tempmin i.day_of_week#c.rain i.day_of_week#c.snow
			
local season_int i.season#c.tempmaxcube i.season#c.tempmaxsq i.season#c.tempmax i.season#c.tempmincube i.season#c.tempminsq i.season#c.tempmin ///
			i.season#c.tempmax#c.tempmin i.season#c.snowsq i.season#c.snow i.season#c.rainsq i.season#c.rain i.season#c.tempmax#c.rain ///
			i.season#c.tempmax#c.tempmax_lag i.season#c.tempmax#c.tempmin_lag i.season#c.tempmax_lag i.season#c.tempmin_lag ///



// We can add month dummies and 

gen dates = date(date, "YMD")
gen DateS = dates / 365 /* scale */
egen MaxDate = max(DateS)
egen MinDate = min(DateS)
gen Z = 2 * (DateS - MinDate) / (MaxDate - MinDate) - 1
gen Time1 = Z 
gen Time2 = 2 * Z^2 - 1
gen Time3 = 4 * Z^3 - 3 * Z
gen Time4 = 2 * Z * Time3 - Time2
gen Time5 = 2 * Z * Time4 - Time3
gen Time6 = 2 * Z * Time5 - Time4
gen Time7 = 2 * Z * Time6 - Time5
gen Time8 = 2 * Z * Time7 - Time6
gen Time9 = 2 * Z * Time8 - Time7
gen Time10 = 2 * Z * Time9 - Time8
drop Z


reghdfe ln_ozone $season_int treat_rfg treat_rvpii $weather $year_int i.month Time1 Time2 Time3 Time4 Time5 Time6 Time7 Time8 Time9 Time10, noabsorb cluster(i.year#i.season) residuals(out_resids)

gen pred_val = _b[treat_rfg] * treat_rfg + _b[treat_rvpii] * treat_rvpii + _b[Time1]*Time1 + _b[Time2]*Time2 + _b[Time3] * Time3 + _b[Time4] * Time4 + _b[Time5] * Time5 + _b[Time6] * Time6 + _b[Time7]*Time7 + _b[Time8]*Time8 + _b[Time9]*Time9 + _b[Time10]*Time10


egen means = mean(pred_val)
gen pred_zeroed = (pred_val-means)

export delimited using "intermediates\stata_fig_6_tex.csv", replace


///////// FIGURE 8 //////////////////


clear
import delimited "intermediates\fig8_cal1.csv"

// Redefine locals
local weather tempmaxcube tempmaxsq tempmax tempmincube tempminsq tempmin ///
			c.tempmax#c.tempmin snowsq snow rainsq rain c.tempmax#c.rain ///
			c.tempmax#c.tempmax_lag c.tempmax#c.tempmin_lag tempmax_lag tempmin_lag ///
			
local dates day_year day_of_week

local year_int day_year#c.tempmaxcube day_year#c.tempmaxsq day_year#c.tempmax day_year#c.tempmincube day_year#c.tempminsq day_year#c.tempmin ///
			day_year#c.tempmax#c.tempmin day_year#c.snowsq day_year#c.snow day_year#c.rainsq day_year#c.rain day_year#c.tempmax#c.rain ///
			day_year#c.tempmax#c.tempmax_lag day_year#c.tempmax#c.tempmin_lag day_year#c.tempmax_lag day_year#c.tempmin_lag ///

local week_int i.day_of_week#c.tempmax i.day_of_week#c.tempmin i.day_of_week#c.rain i.day_of_week#c.snow
			
local season_int i.season#c.tempmaxcube i.season#c.tempmaxsq i.season#c.tempmax i.season#c.tempmincube i.season#c.tempminsq i.season#c.tempmin ///
			i.season#c.tempmax#c.tempmin i.season#c.snowsq i.season#c.snow i.season#c.rainsq i.season#c.rain i.season#c.tempmax#c.rain ///
			i.season#c.tempmax#c.tempmax_lag i.season#c.tempmax#c.tempmin_lag i.season#c.tempmax_lag i.season#c.tempmin_lag ///

// We can add month dummies and 

gen dates = date(date, "YMD")
gen DateS = dates / 365 /* scale */
egen MaxDate = max(DateS)
egen MinDate = min(DateS)
gen Z = 2 * (DateS - MinDate) / (MaxDate - MinDate) - 1
gen Time1 = Z 
gen Time2 = 2 * Z^2 - 1
gen Time3 = 4 * Z^3 - 3 * Z
gen Time4 = 2 * Z * Time3 - Time2
gen Time5 = 2 * Z * Time4 - Time3
gen Time6 = 2 * Z * Time5 - Time4
gen Time7 = 2 * Z * Time6 - Time5
gen Time8 = 2 * Z * Time7 - Time6
gen Time9 = 2 * Z * Time8 - Time7
gen Time10 = 2 * Z * Time9 - Time8
drop Z


reghdfe ln_ozone $season_int treat_rvpii  treat_carb treat_rfg $weather $year_int i.month Time1 Time2 Time3 Time4 Time5 Time6 Time7 Time8 Time9 Time10, noabsorb cluster(i.year#i.season) residuals(out_resids)

gen pred_val = _b[treat_rvpii]*treat_rvpii + _b[treat_carb]*treat_carb + _b[treat_rfg]*treat_rfg + _b[Time1]*Time1 + _b[Time2]*Time2 + _b[Time3] * Time3 + _b[Time4] * Time4 + _b[Time5] * Time5 + _b[Time6] * Time6 + _b[Time7]*Time7 + _b[Time8]*Time8 + _b[Time9]*Time9 + _b[Time10]*Time10


egen means = mean(pred_val)
gen pred_zeroed = (pred_val-means)

export delimited using "intermediates\stata_fig_8_cal1.csv", replace


/// CAL 2


clear
import delimited "intermediates\fig8_cal2.csv"

// Redefine locals
local weather tempmaxcube tempmaxsq tempmax tempmincube tempminsq tempmin ///
			c.tempmax#c.tempmin snowsq snow rainsq rain c.tempmax#c.rain ///
			c.tempmax#c.tempmax_lag c.tempmax#c.tempmin_lag tempmax_lag tempmin_lag ///
			
local dates day_year day_of_week

local year_int day_year#c.tempmaxcube day_year#c.tempmaxsq day_year#c.tempmax day_year#c.tempmincube day_year#c.tempminsq day_year#c.tempmin ///
			day_year#c.tempmax#c.tempmin day_year#c.snowsq day_year#c.snow day_year#c.rainsq day_year#c.rain day_year#c.tempmax#c.rain ///
			day_year#c.tempmax#c.tempmax_lag day_year#c.tempmax#c.tempmin_lag day_year#c.tempmax_lag day_year#c.tempmin_lag ///

local week_int i.day_of_week#c.tempmax i.day_of_week#c.tempmin i.day_of_week#c.rain i.day_of_week#c.snow
			
local season_int i.season#c.tempmaxcube i.season#c.tempmaxsq i.season#c.tempmax i.season#c.tempmincube i.season#c.tempminsq i.season#c.tempmin ///
			i.season#c.tempmax#c.tempmin i.season#c.snowsq i.season#c.snow i.season#c.rainsq i.season#c.rain i.season#c.tempmax#c.rain ///
			i.season#c.tempmax#c.tempmax_lag i.season#c.tempmax#c.tempmin_lag i.season#c.tempmax_lag i.season#c.tempmin_lag ///

// We can add month dummies and 

gen dates = date(date, "YMD")
gen DateS = dates / 365 /* scale */
egen MaxDate = max(DateS)
egen MinDate = min(DateS)
gen Z = 2 * (DateS - MinDate) / (MaxDate - MinDate) - 1
gen Time1 = Z 
gen Time2 = 2 * Z^2 - 1
gen Time3 = 4 * Z^3 - 3 * Z
gen Time4 = 2 * Z * Time3 - Time2
gen Time5 = 2 * Z * Time4 - Time3
gen Time6 = 2 * Z * Time5 - Time4
gen Time7 = 2 * Z * Time6 - Time5
gen Time8 = 2 * Z * Time7 - Time6
gen Time9 = 2 * Z * Time8 - Time7
gen Time10 = 2 * Z * Time9 - Time8
drop Z

set emptycells drop

reghdfe ln_ozone `season_int' treat_rvpii treat_carb treat_rfg `weather' i.month `year_int' Time1 Time2 Time3 Time4 Time5 Time6 Time7 Time8 Time9 Time10, noabsorb cluster(i.year#i.season) residuals(out_resids)

gen pred_val = _b[treat_rvpii]*treat_rvpii + _b[treat_carb]*treat_carb + _b[treat_rfg]*treat_rfg + _b[Time1]*Time1 + _b[Time2]*Time2 + _b[Time3] * Time3 + _b[Time4] * Time4 + _b[Time5] * Time5 + _b[Time6] * Time6 + _b[Time7]*Time7 + _b[Time8]*Time8 + _b[Time9]*Time9 + _b[Time10]*Time10


egen means = mean(pred_val)
gen pred_zeroed = (pred_val-means)

export delimited using "intermediates\stata_fig_8_cal2.csv", replace

