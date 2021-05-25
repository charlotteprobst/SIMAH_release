// NIH2020 project. 
// Aggregate population counts by demographic subgroup for years 2000 to 2018

/* This do file 
- reads in the state codes (requires state variable in mortality data)
- reads in combined march CPS data for the years 2000 to 2018
- Calculates population counts for demographic groups correspoinding to those 
  used in the aggregated mortality file
*/

// set global directory
global dir `"C:/Users/marie/Dropbox/NIH2020/Demography"'


// population counts by cell

// make state identifiers compatible between deaths records and CPS
use "${dir}/1_raw CPS data/STCROSS.dta", clear
ren State state
merge 1:m  state using "${dir}/3_out CPS data/cepr_march_0018_final.dta"
drop _m

gen sex = female + 1

// generate age groups corresponding to those used in the mortality files
gen age_gp=.
replace age_gp=18 if age>=18 & age<=24
forval i = 25(5)75 {
replace age_gp=`i' if age>=`i' & age<=`i'+4 
}
replace age_gp = 80 if age>=80 & age<=100
drop if age_gp==.

bysort year sex age_gp edclass wbho: egen TPop = total(wgt)
bysort year sex age_gp edclass wbho: keep if _n==1

rename wbho race
keep year sex age_gp edclass race TPop 

save "${dir}/3_out CPS data/STCROSSCEPRMarchcomb.dta", replace


