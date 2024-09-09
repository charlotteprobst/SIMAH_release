// NIH2020 project. 
// Process mortality data for years 2000 to 2021

/* This do file 
- reads in the raw mortality data 
- uses ICD 10 codes to generate cause of death categories
- aggregates deaths for each category by sociodemographic characteristics
- redistributes deaths with missing information for education (only applicable when 
  education is used)
*/

// Only run if changes were made to the CPS data or do files
/*
do "C:/Users/marie/Dropbox/NIH2020/Demography/2_do/1_CPS_March_read_merge_0019.do"
do "C:/Users/marie/Dropbox/NIH2020/Demography/2_do/2_read_cps_march_0019.do"
do "C:/Users/marie/Dropbox/NIH2020/Demography/2_do/3_aggregate_population_counts_0019.do"
*/

set more off

//global dir `"C:/Users/marie/Dropbox/NIH2020/SIMAH_workplace/"'
global dir `"/Users/carolinkilian/Desktop/SIMAH_workplace/"'

cd "${dir}/mortality/"

use "3_out data/mort_0021_complete.dta", clear
//use "3_out data/allethn_sumCOD_0021_10perc sample.dta", clear

//rename race race18 
//gen race = .
//replace race = 1 if race18 == 1
//replace race = 2 if race18 == 2
//replace race = 3 if race18 == 3
//replace race = 4 if race18 == .
//lab var race "Race/ethnicity"
//label define racelab 1 "White" 2 "Black" 3 "Hispanic" 4 "Other"
//label values race racelab
//drop race18

drop if age_gp == .

keep  icd10 year sex age_gp edclass
gen str1 xcode = icd10 


///////////////////////////////////////////////////////////////////////////////

// specific causes of death (using icd codes)

// Alcoholic Liver Disease and Cirrhosis [K70]
generate alvdc = 1 if inrange(icd10, "K70", "K709") 

// Generate rest category
gen rest = .
replace rest = 1 if (alvdc == . )

save "3_out data/1_alvdcmort_0021_SIMAH.dta", replace

///////////////////////////////////////////////////////////////////////////////

//sum by COD 

//use "3_out data/1_alvdcmort_0021_SIMAH.dta", clear


/////////////////////////////////////////////////////////////////////////
 
gen one=1

bysort age_gp sex edclass year: egen ALVDCmort = total(alvdc)
bysort age_gp sex edclass year: egen RESTmort = total(rest)

by age_gp sex edclass  year, sort: keep if _n==1
//save "3_out data/1_alvdcmort_0021_SIMAH.dta", replace

//assigning deaths without education information
//Deaths with missing data on SES were assigned to an education category based 
//on the proportion in each education group by year, race/ethnicity, sex, age group, and cause of death.

//use "3_out data/1_alvdcmort_0021_SIMAH.dta", clear

keep year sex age_gp edclass *mort
reshape wide ALVDCmort RESTmort, i(year age_gp sex ) j(edclass)
sum *mort99

foreach i in ALVDC REST {
gen MORT`i' = `i'mort1 + `i'mort2 + `i'mort3
	replace `i'mort1 = `i'mort1 + (`i'mort1/MORT`i') * `i'mort99 if MORT`i' != 0 
	replace `i'mort2 = `i'mort2 + (`i'mort2/MORT`i') * `i'mort99 if MORT`i' != 0 
	replace `i'mort3 = `i'mort3 + (`i'mort3/MORT`i') * `i'mort99 if MORT`i' != 0 
}

//deal with very rare case in which the deaths only fall into "missing" education 
//Split the deaths evenly across the three eduation categories 
foreach i in ALVDC REST {
	replace `i'mort1 = 1/3*`i'mort99 if MORT`i' == 0 & `i'mort99 != 0
	replace `i'mort2 = 1/3*`i'mort99 if MORT`i' == 0 & `i'mort99 != 0
	replace `i'mort3 = 1/3* `i'mort99 if MORT`i' == 0 & `i'mort99 != 0
}

keep year sex age_gp *mort1 *mort2 *mort3   

reshape long ALVDCmort RESTmort, ///
	i(year age_gp sex ) j(edclass)
	
lab define edlab 1 "LEHS" 2 "SomeC" 3 "College", modify
 
save "3_out data/alvdcmort_0021_SIMAH.dta", replace
outsheet using "${dir}mortality/3_out data/alvdcmort_0021_SIMAH.csv" , comma replace
