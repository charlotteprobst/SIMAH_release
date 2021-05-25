// NIH2020 project. 
// Calculate mortality rates for years 2000 to 2018

/* This do file 
- requires population counts for demographic groups (based on March CPS surveys) 
  correspoinding to those used in the aggregated mortality file (based on CDC)
- calculates crude mortality rates in each sociodemographic group and for each 
  cause of death category (has to match the mortality data)
*/

// set global directory
global dir `"C:/Users/marie/Dropbox/NIH2020"'

// combine deaths with population by cell
// read in population counts
use "${dir}/Demography/1_CPS data/outfiles/STCROSSCEPRMarchcomb.dta", clear

merge 1:1 year age_gp sex edclass race using "${dir}/Mortality/3_out data/allethn_sumCOD_0018_final.dta"

/* _m==1 is for groups that recorded no deaths in that year (those that were not merged but only appear in master...) */
keep if year == 2000 | year == 2015

foreach var in T CD_NR CD_LR CD_HR NCD_NR NCD_LR NCD_HR NCD_AAF1 Inj_NR Inj_LR Inj_AAF1 HS_NR lowrisk HS_AAF1 {
   replace `var'mort = 0 if _m==1 
   gen `var'rate   = (`var'mort/TPop)*100000
   }

tab year if _m==2
drop _m

label var Trate "all cause"
label var CD_NRrate "CD no risk"
label var CD_LRrate "CD low risk"
label var CD_HRrate "CD high risk"
label var NCD_NRrate "NCD no risk"
label var NCD_LRrate "NCD low risk"
label var NCD_HRrate "NCD high risk"
label var NCD_AAF1rate "NCD AAF1"
label var Inj_NRrate "Injury no risk"
label var Inj_LRrate "Injury low risk"
label var Inj_AAF1rate "Injury AAF1"
label var HS_NRrate "Health status no risk"
label var lowriskrate "Health status low risk"
label var HS_AAF1rate "Health status AAF1"

  
save "${dir}/Mortality/3_out data/allethn_rates_0018_final.dta", replace

outsheet using "${dir}/Mortality/3_out data/allethn_rates_0018_final.csv" , comma replace
