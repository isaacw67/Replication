
import delimited "C:\Users\isaac\Documents\r\replication\intermediates\fig_5.csv"

$weather tempmax^3  tempmax^2  tempmax  tempmin^3  tempmin^2  tempmin ///
			tempman*tempmin  snow^2  snow  rain^2  rain  tempmax*rain  ///
			tempmax*tempmax_lag  tempmax*tempmin_lag  tempmax_lag  tempmin_lag ///
			
$dates  day_year  day_of_week

$year_fe i(day_year $weather)

$week_fe day_of_week#tempmax  day_of_week#tempmin  day_of_week#rain  day_of_week#snow

redhdfe ozone_max `$weather`' `$dates`' `$year_fe`' `$week_fe`, absord(fips#state_code county_code#year)