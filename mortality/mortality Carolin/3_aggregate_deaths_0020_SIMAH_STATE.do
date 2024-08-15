// NIH2020 project. 
// Process mortality data for years 2000 to 2021
// INCLUDE STATE AND MONTH

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


use "3_out data/mort_0021_complete_STATE.dta", clear

rename race race18 
gen race = .
replace race = 1 if race18 == 1
replace race = 2 if race18 == 2
replace race = 3 if race18 == 3
replace race = 4 if race18 == .
lab var race "Race/ethnicity"
label define racelab 1 "White" 2 "Black" 3 "Hispanic" 4 "Other"
label values race racelab
drop race18

drop if age_gp == .

// NEW TO INCLUDE STATE // 
tab fipsstr, mis

keep icd10 year month fipsstr race sex age_gp edclass R_condition*


///////////////////////////////////////////////////////////////////////////////

// specific causes of death (using icd codes)

/*specific causes of death (using icd codes)
url for ICD-10 2019 : https://icd.who.int/browse10/2019/en     */

  // acute conditions 100% alcohol 
  // acute intoxication, alcohol poisonings, finding alcohol in blood
   gen aud_acute = 1 if inrange(icd10, "X45", "X459") | inlist(icd10, "X65") | inrange(icd10, "Y15", "Y159") ///
		| inlist(icd10, "F100") | inlist(icd10, "R780") | inrange(icd10, "Y90", "Y909") | inrange(icd10, "Y91", "Y919") ///
		| inrange(icd10, "T51", "T519")
 
  // chronic conditions 100% alcohol
  // AUD
   gen aud_chronic = 1 if inlist(icd10, "E244") | inrange(icd10, "F101", "F109") | inlist(icd10, "G312") ///
		| inlist(icd10, "G621") | inlist(icd10, "G721") | inlist(icd10, "I426") | inlist(icd10, "K292") ///
		| inlist(icd10, "K852") | inlist(icd10, "K860") 
  
  // ALCOHOLIC LIVER DISEASE
	gen alc_liver = 1 if inrange(icd10, "K70", "K709")
  
  // OTHER LIVER DISEASE
	generate oth_liver = 1 if inrange(icd10, "K73", "K739") | inrange(icd10, "K74", "K749")  ///
		| inlist(icd10, "K760", "K766") | inrange(icd10, "B18", "B189")
	replace oth_liver = 1 if icd10 == "B171"  | icd10 == "B924" 

  // other unintentional injuries: same as SIMAH codes
 * gen uij_temp = 1 if inrange(icd10, "V01", "X599") | inrange(icd10, "Y85", "Y869")
  gen uij_temp = 1 if inrange(icd10, "V01", "X40") | inrange(icd10, "X43", "X43") ///
   | inrange(icd10, "X46", "X599")	| inrange(icd10, "Y40", "Y869") ///
   | inrange(icd10, "Y88", "Y899")
   
   // Motor Vehicle accident: same as SIMAH codes
  gen mvacc = 1 if inrange(icd10, "V02", "V049") | inlist(icd10, "V090", "V092")  ///
	| inrange(icd10, "V12", "V149") | inrange(icd10, "V190", "V192") ///
	| inrange(icd10, "V194", "V196") ///
	| inrange(icd10, "V20", "V799") | inrange(icd10, "V803", "V805") ///
	| inrange(icd10, "V810", "V811") | inrange(icd10, "V820", "V821") ///
	| inrange(icd10, "V83", "V869") | inrange(icd10, "V870", "V878") ///
	| inrange(icd10, "V880", "V888") | inlist(icd10, "V890",  "V892")
   
    // final unintentional injury, excl MVA and all poisoning 
   gen uij = 1 if uij_temp == 1 & mvacc == . & aud_acute == .
   drop uij_temp
  
    // Suicide (Intentional self harm) [X60-X84, Y87.0]
   gen sij_temp = 1 if inrange(icd10, "X60", "X849") | inrange(icd10, "Y870", "Y870")
   gen sij = 1 if sij_temp == 1 & aud_acute == .
   drop sij_temp
   
   	//test each category is coded once 
	 egen test = rowtotal(aud_acute aud_chronic alc_liver oth_liver mvacc uij sij)
    tab test, m	
	drop test 

	
   // all remaining
   gen rest = 1
   foreach v of varlist aud_acute aud_chronic alc_liver oth_liver mvacc uij sij {
	  	replace rest = . if `v' == 1
	  } 	       
	  	
	 //test each category is coded once 
	 egen test = rowtotal(aud_acute aud_chronic alc_liver oth_liver mvacc uij sij rest)
    tab test, m
    drop test 
	
    tabstat aud_acute aud_chronic alc_liver oth_liver mvacc uij sij rest, by(year) stat(sum)
		
save "3_out data/1_allethn_mortbycause_0021_STATE.dta", replace

///////////////////////////////////////////////////////////////////////////////

//sum by COD 

use "3_out data/1_allethn_mortbycause_0021_STATE.dta", clear

keep year month fipsstr age_gp sex edclass ///
		aud_acute aud_chronic alc_liver oth_liver ///
		mvacc uij sij rest

/////////////////////////////////////////////////////////////////////////

// aggregate by quarter (IF REQUIRED)

//gen quarter = .
//recode quarter (. = 1) if month < 4
//recode quarter (. = 2) if month >= 4 & month < 7
//recode quarter (. = 3) if month >= 6 & month < 10
//recode quarter (. = 4) if month >= 10

// Sum by COD (Catgories + Rest)
 
gen one=1

// acute, chronic AUD
bysort age_gp sex edclass fipsstr month year: egen ACUTmort = total(aud_acute)
bysort age_gp sex edclass fipsstr month year: egen CHRONmort = total(aud_chronic)

// liver disease
bysort age_gp sex edclass fipsstr month year: egen ALCLIVmort = total(alc_liver)
bysort age_gp sex edclass fipsstr month year: egen OTHLIVmort = total(oth_liver)

// suicide, injury, traffic injury
bysort age_gp sex edclass fipsstr month year: egen UIJmort = total(uij)
bysort age_gp sex edclass fipsstr month year: egen SIJmort = total(sij)
bysort age_gp sex edclass fipsstr month year: egen MVACCmort = total(mvacc)

// rest
bysort age_gp sex edclass fipsstr month year: egen RESTmort = total(rest)

by age_gp sex edclass fipsstr month year, sort: keep if _n==1

save "3_out data/2_allethn_sumCOD_0021_STATE.dta", replace

//assigning deaths without education information
//Deaths with missing data on SES were assigned to an education category based 
//on the proportion in each education group by year, race/ethnicity, sex, age group, and cause of death.

use "3_out data/2_allethn_sumCOD_0021_STATE.dta", clear

keep fipsstr year month sex age_gp edclass *mort
reshape wide ACUTmort CHRONmort ALCLIVmort OTHLIVmort ///
	UIJmort SIJmort MVACCmort RESTmort, i(fipsstr year month age_gp sex) j(edclass)
sum *mort99

foreach i in ACUT CHRON ALCLIV OTHLIV UIJ SIJ MVACC REST {
gen MORT`i' = `i'mort1 + `i'mort2 + `i'mort3
	replace `i'mort1 = `i'mort1 + (`i'mort1/MORT`i') * `i'mort99 if MORT`i' != 0 
	replace `i'mort2 = `i'mort2 + (`i'mort2/MORT`i') * `i'mort99 if MORT`i' != 0 
	replace `i'mort3 = `i'mort3 + (`i'mort3/MORT`i') * `i'mort99 if MORT`i' != 0 
}

//deal with very rare case in which the deaths only fall into "missing" education 
//Split the deaths evenly across the three eduation categories 
foreach i in ACUT CHRON ALCLIV OTHLIV UIJ SIJ MVACC REST {
	replace `i'mort1 = 1/3*`i'mort99 if MORT`i' == 0 & `i'mort99 != 0
	replace `i'mort2 = 1/3*`i'mort99 if MORT`i' == 0 & `i'mort99 != 0
	replace `i'mort3 = 1/3* `i'mort99 if MORT`i' == 0 & `i'mort99 != 0
}

keep fipsstr year month sex age_gp *mort1 *mort2 *mort3   

reshape long ACUTmort CHRONmort ALCLIVmort OTHLIVmort ///
	UIJmort SIJmort MVACCmort RESTmort, i(fipsstr year month age_gp sex) j(edclass)

lab define edlab 1 "LEHS" 2 "SomeC" 3 "College", modify


foreach v of varlist ACUTmort-RESTmort {
	rename `v', lower
}


tabstat *mort, stat(sum) by(year) format(%12.0f)
   
sort fipsstr year month age_gp sex edclass 
 
save "3_out data/allethn_sumCOD_0021_STATE.dta", replace
outsheet using "${dir}mortality/3_out data/allethn_sumCOD_0021_STATE.csv" , comma replace
