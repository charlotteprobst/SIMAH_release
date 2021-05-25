// NIH2020 project. 
// Process mortality data for years 2000 to 2019

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

global dir `"C:/Users/marie/Dropbox/NIH2020/"'

cd "${dir}/Mortality/"


use "3_out data/mort_0019_complete.dta", clear

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

keep  icd10 year race sex age_gp edclass
gen str1 xcode = icd10 


///////////////////////////////////////////////////////////////////////////////

// specific causes of death (using icd codes)

/*specific causes of death (using icd codes)
url for ICD-10 2019 : https://icd.who.int/browse10/2019/en     */

// Unintentional inurues

gen uij_temp = 1 if inrange(icd10, "V01", "X40") | inrange(icd10, "X43", "X43") ///
	| inrange(icd10, "X46", "X599")	| inrange(icd10, "Y40", "Y869") ///
	| inrange(icd10, "Y88", "Y899")

gen mvacc = 1 if inrange(icd10, "V02", "V049") | inlist(icd10, "V090", "V092")  ///
	| inrange(icd10, "V12", "V149") | inrange(icd10, "V190", "V192") ///
	| inrange(icd10, "V194", "V196") ///
	| inrange(icd10, "V20", "V799") | inrange(icd10, "V803", "V805") ///
	| inrange(icd10, "V810", "V811") | inrange(icd10, "V820", "V821") ///
	| inrange(icd10, "V83", "V869") | inrange(icd10, "V870", "V878") ///
	| inrange(icd10, "V880", "V888") | inlist(icd10, "V890",  "V892")


gen uij = 1 if uij_temp == 1 & mvacc == . 
drop uij_temp


// AUD F10-F10.9, G31.2, G72.1, Q86.0, R78.0, X45-X45.9, Y15-Y15.9
gen aud = 1 if inrange(icd10, "F10", "F109") | inlist(icd10, "G312", "G312")  ///
	| inrange(icd10, "G721", "G721") | inrange(icd10, "Q860", "Q860") ///
	| inrange(icd10, "R780", "R780") ///
	| inrange(icd10, "X45", "X459") | inrange(icd10, "Y15", "Y159") 



//////////////////////////////////////////////////////////////////////

// Suicide (Intentional self harm) [X60-X84, Y87.0]

generate ij = 1 if inrange(icd10, "X60", "X84") | inrange(icd10, "Y870", "Y870")


////////////////////////////////////////////////////////////////////

// Liver Disease and Cirrhosis [K70, K71.3-K71.5, K71.7, K72-K74]

generate lvdc = 1 if inrange(icd10, "K70", "K709") | inrange(icd10, "K713", "K715") ///
	| inrange(icd10, "K717", "K717")  | inrange(icd10, "K72", "K749") 

////////////////////////////////////////////////////////////////

// Diabetes mellitus [E10-E14]

generate dm = 1 if inrange(icd10, "E100", "E149")

/////////////////////////////////////////////////////////////////

// Ischemic Heart Disease [I20-I25]

generate ihd = 1 if inrange(icd10, "I20", "I259")

/////////////////////////////////////////////////////////////////

// Stroke [G45-G46.8, I60-I63.9, I65-I66.9, I67.0-I67.3, I67.5-I67.6, I68.1-I68.2, I69.0-I69.3]

generate str = 1 if inrange(icd10, "G45","G468") | inrange(icd10, "I60", "I639") ///
	| inrange(icd10, "I65", "I673")  | inrange(icd10, "I675", "I676")  ///
	| inrange(icd10, "I681", "I682") | inrange(icd10, "I690", "I693")


////////////////////////////////////////////////////////////////////

// Hypertensive Heart Disease [I11] 

generate hyphd = 1 if inrange(icd10, "I110", "I119")


// Make sure that no death was coded double and that all ICD 10 codes were assigned
// tab icd 10 if all categories are 0 

//tab icd10 if  (lvdc == . & dm == . & ihd == . & str == . & hyphd == . ///
//	& aud == . & uij == . & mvacc == . & ij == . )


// Generate rest category
gen rest = .
replace rest = 1 if (lvdc == . & dm == . & ihd == . & str == . & hyphd == . ///
	& aud == . & uij == . & mvacc == . & ij == . )


// Update excel worksheet to include the variable names and the codes assigned

save "3_out data/1_allethn_mortbycause_0019.dta", replace

///////////////////////////////////////////////////////////////////////////////

//sum by COD 

use "3_out data/1_allethn_mortbycause_0019.dta", clear

/////////////////////////////////////////////////////////////////////////

// Sum by COD (TOTAL + 7 Catgories + Rest)

gen one=1
bysort age_gp sex edclass race year: egen Tmort = total(one)

bysort age_gp sex edclass race year: egen LVDCmort = total(lvdc)
bysort age_gp sex edclass race year: egen DMmort = total(dm)
bysort age_gp sex edclass race year: egen IHDmort = total(ihd)
bysort age_gp sex edclass race year: egen STRmort = total(str)
bysort age_gp sex edclass race year: egen HYPHDmort = total( hyphd )
bysort age_gp sex edclass race year: egen AUDmort = total(aud)
bysort age_gp sex edclass race year: egen UIJmort = total(uij)
bysort age_gp sex edclass race year: egen MVACCmort = total(mvacc)
bysort age_gp sex edclass race year: egen IJmort = total(ij)
bysort age_gp sex edclass race year: egen RESTmort = total(rest)

by age_gp sex edclass race year, sort: keep if _n==1
save "3_out data/2_allethn_sumCOD_0019.dta", replace

//assigning deaths without education information

use "3_out data/2_allethn_sumCOD_0019.dta", clear

keep year sex age_gp edclass race *mort
reshape wide Tmort LVDCmort DMmort IHDmort STRmort HYPHDmort AUDmort UIJmort MVACCmort IJmort RESTmort, i(year age_gp sex race) j(edclass)
sum *mort99

foreach i in T LVDC DM IHD STR HYPHD AUD UIJ MVACC IJ REST {
gen MORT`i' = `i'mort1 + `i'mort2 + `i'mort3
	replace `i'mort1 = `i'mort1 + (`i'mort1/MORT`i') * `i'mort99 if MORT`i' != 0 
	replace `i'mort2 = `i'mort2 + (`i'mort2/MORT`i') * `i'mort99 if MORT`i' != 0 
	replace `i'mort3 = `i'mort3 + (`i'mort3/MORT`i') * `i'mort99 if MORT`i' != 0 
}

keep year sex race age_gp *mort1 *mort2 *mort3   

reshape long Tmort LVDCmort DMmort IHDmort STRmort HYPHDmort AUDmort UIJmort MVACCmort IJmort RESTmort, ///
 i(year age_gp sex race) j(edclass)

save "3_out data/allethn_sumCOD_0019_final.dta", replace
outsheet using "${dir}/Mortality/3_out data/allethn_sumCOD_0019_final.csv" , comma replace
