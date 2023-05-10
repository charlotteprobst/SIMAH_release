// NIH2020 project. 
// Calculate mortality rates for years 2000 to 2020

/* This do file 
- requires population counts for demographic groups (based on March CPS surveys) -> CK: use ACS data
  correspoinding to those used in the aggregated mortality file (based on CDC)
- calculates crude mortality rates in each sociodemographic group and for each 
  cause of death category (has to match the mortality data)
*/

// difference to v2 is that cancer and pancreatitis were added as causes of death
// set global directory
// global dir `"C:/Users/marie/Dropbox/NIH2020"'
global dir `"/Users/carolinkilian/Desktop/SIMAH_workplace/"'

// import STATE csv file and SAVE as .dta
import delimited "${dir}/mortality/3_out data/allethn_sumCOD_0020_STATE.csv", clear

	// collapse age groups
//	gen age_gp_NEW = age_gp
//	recode age_gp_NEW (30 = 25) (40 = 35) (50 = 45) (55 = 45) (60 = 45) (70 = 65) (75 = 65) (80 = 65)
//	bysort age_gp_NEW sex edclass fipsstr month year: egen TMORT = total(tmort)
//	bysort age_gp_NEW sex edclass fipsstr month year: egen ACUTMORT = total(acutmort)
//	bysort age_gp_NEW sex edclass fipsstr month year: egen CHRONMORT = total(chronmort)
//	bysort age_gp_NEW sex edclass fipsstr month year: egen ALCLIVMORT = total(alclivmort)
//	bysort age_gp_NEW sex edclass fipsstr month year: egen OTHLIVMORT = total(othlivmort)
//	bysort age_gp_NEW sex edclass fipsstr month year: egen UIJMORT = total(uijmort)
//	bysort age_gp_NEW sex edclass fipsstr month year: egen SIJMORT = total(sijmort)
//	bysort age_gp_NEW sex edclass fipsstr month year: egen MVACCMORT = total(mvaccmort)
//	bysort age_gp_NEW sex edclass fipsstr month year: egen RESTMORT = total(restmort)
	
//	keep age_gp_NEW sex edclass fipsstr month year TMORT ACUTMORT CHRONMORT ALCLIVMORT OTHLIVMORT UIJMORT SIJMORT MVACCMORT RESTMORT
//	rename age_gp_NEW age_gp
	
// export as .DTA	
save "${dir}/mortality/3_out data/allethn_sumCOD_0020_STATE.dta", replace

	// only 2010 to 2020 data
//	keep if year >= 2010

// export as .DTA	
save "${dir}/mortality/3_out data/allethn_sumCOD_1020_STATE.dta", replace

// combine deaths with population by cell
// read in population counts
//import delimited "${dir}/SIMAH_workplace/demography/pop_counts_simulation_2000_2020.csv", clear
import delimited "${dir}/mortality/3_out data/20230417_ACS_DEMOGRAPHY.csv", clear

merge m:m fipsstr year age_gp sex edclass using "${dir}/mortality/3_out data/allethn_sumCOD_0020_STATE.dta"

/* _m==1 is for groups that recorded no deaths in that year (those that were not merged but only appear in master...) */

// FOR NOW ONLY 
keep if _m == 3

 foreach var in T ACUT CHRON ALCLIV OTHLIV UIJ SIJ MVACC REST {
//   replace `var'MORT = 0 if _m==1 
   gen `var'rate   = (`var'MORT/pop)*100000
   }

tab year if _m==2
drop _m

label var Trate "all cause"
label var ACUTrate "acute 100% AA conditions" 
label var CHRONrate "chronic 100% AA conditions" 
label var ALCLIVrate "alcoholic liver disease"
label var OTHLIVrate "other liver diseases"
label var UIJrate "unintentional injuries"
label var SIJrate "intentional injuries"
label var MVACCrate "motorvehicle accidents"
label var RESTrate  "other causes"


save "${dir}/mortality/3_out data/allethn_rates_1020_STATE.dta", replace

outsheet using "${dir}/mortality/3_out data/allethn_rates_1020_STATE.csv" , comma replace
