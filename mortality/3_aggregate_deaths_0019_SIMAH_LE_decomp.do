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

cd "${dir}/SIMAH_workplace/mortality/"


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


// Pancreatitis [K85.0, K85.1, K85.2, K85.8, K85.9, K86.0-K86.99]
generate panc = 1 if inrange(icd10, "K850", "K852") | inrange(icd10, "K858", "K859") ///
	| inrange(icd10, "K860", "K869") 
replace panc = 1 if icd10 == "K85.8" | icd10 == "K85.9"

	////////////////////////////////////////////////////////////////

// Diabetes mellitus [E10-E14]

generate dm = 1 if inrange(icd10, "E100", "E149")

/////////////////////////////////////////////////////////////////

// Ischemic Heart Disease [I20-I25]

generate ihd = 1 if inrange(icd10, "I20", "I259")

/////////////////////////////////////////////////////////////////

// Ischemic  Stroke [G45-G46.8, I63–I63.9, I65–I66.9, I67.2-–I67.848, I69.3-–I69.4
generate istr = 1 if inrange(icd10, "G45","G468") | inrange(icd10, "I63", "I639") ///
	| inrange(icd10, "I65", "I669") | inrange(icd10, "I672", "I678") ///
	| inrange(icd10, "I693", "I694") 
 
/// Hemorrhagic stroke [I60–I62.9, I67.0-–I67.1, I69.0-–I69.298]
generate hstr = 1 if  inrange(icd10, "I60", "I629")  ///
	| inrange(icd10, "I670", "I671")  ///
	| inrange(icd10, "I690", "I692") 

	
////////////////////////////////////////////////////////////////////

// Hypertensive Heart Disease [I11] 
generate hyphd = 1 if inrange(icd10, "I110", "I119")

//generate cancer category 
  //breast cancer C50
  gen breast_cancer = 1 if inrange(icd10, "C500", "C509")
  //Pancreatic cancer C25
  gen pancreatic_cancer = 1 if inrange(icd10, "C250", "C259")
  //Colorectal cancer
  //Malignant neoplsm of colon C18, malignant neoplasm of rectogsigmoid junction C19 ///
  //and malignant neoplsm of rectum C20, and Malignant neoplasm of anus and anal canal C21
  //C18.0 to C21.8 to include all colon and rectum cancers
  gen colorectal_cancer = 1 if inrange(icd10, "C180", "C218") 
  //Oesophageal cancer C15
  gen oesophageal_cancer = 1 if inrange(icd10, "C150", "C159")
  //Larnygeal cancer (larynx cancer?) C32
  gen larynx_cancer = 1 if inrange(icd10, "C320", "C329")
  //Liver cancer C22
  gen liver_cancer = 1 if inrange(icd10, "C220", "C229")
  // Should include C00 to C14 for Lip, Oral cavity and pharynx cancer 
  gen oral_cancer = 1 if inrange(icd10, "C00", "C149")
  //combine all cancer types above
  gen cancer = 1 if inlist(1,breast_cancer,pancreatic_cancer,colorectal_cancer,oesophageal_cancer, ///
    larynx_cancer,liver_cancer,oral_cancer)
  drop breast_cancer-oral_cancer	

// Generate rest category
gen rest = .
replace rest = 1 if (lvdc == . & panc == . & dm == . & ihd == . & istr == . & hstr == . & hyphd == . ///
	& aud == . & uij == . & mvacc == . & ij == . & cancer == .)


// Update excel worksheet to include the variable names and the codes assigned

save "3_out data/1_allethn_mortbycause_0019_LE_decomp.dta", replace

///////////////////////////////////////////////////////////////////////////////

//sum by COD 

use "3_out data/1_allethn_mortbycause_0019_LE_decomp.dta", clear

// Test that all deaths have been assigned exactly once
egen test = rowtotal(lvdc panc dm ihd istr hstr hyphd aud uij mvacc ij cancer)
tab test
drop test	

/////////////////////////////////////////////////////////////////////////

// Sum by COD (TOTAL + 7 Catgories + Rest)

gen one=1

egen test = rowtotal(lvdc panc dm ihd istr hstr hyphd aud uij mvacc ij cancer rest one)
tab test
drop test	


bysort age_gp sex edclass race year: egen Tmort = total(one)

bysort age_gp sex edclass race year: egen LVDCmort = total(lvdc)
bysort age_gp sex edclass race year: egen PANCmort = total(panc)
bysort age_gp sex edclass race year: egen DMmort = total(dm)
bysort age_gp sex edclass race year: egen IHDmort = total(ihd)

bysort age_gp sex edclass race year: egen ISTRmort = total(istr)
bysort age_gp sex edclass race year: egen HSTRmort = total(hstr)

bysort age_gp sex edclass race year: egen HYPHDmort = total( hyphd )
bysort age_gp sex edclass race year: egen AUDmort = total(aud)
bysort age_gp sex edclass race year: egen UIJmort = total(uij)
bysort age_gp sex edclass race year: egen MVACCmort = total(mvacc)
bysort age_gp sex edclass race year: egen IJmort = total(ij)
bysort age_gp sex edclass race year: egen CANmort = total(cancer)

bysort age_gp sex edclass race year: egen RESTmort = total(rest)

by age_gp sex edclass race year, sort: keep if _n==1
save "3_out data/2_allethn_sumCOD_0019_LE_decomp.dta", replace

//assigning deaths without education information
//Deaths with missing data on SES were assigned to an education category based 
//on the proportion in each education group by year, race/ethnicity, sex, age group, and cause of death.

use "3_out data/2_allethn_sumCOD_0019_LE_decomp.dta", clear

keep year sex age_gp edclass race *mort
reshape wide Tmort LVDCmort PANCmort DMmort IHDmort ISTRmort HSTRmort HYPHDmort AUDmort UIJmort MVACCmort IJmort CANmort RESTmort, i(year age_gp sex race) j(edclass)
sum *mort99

foreach i in T LVDC PANC DM IHD ISTR HSTR HYPHD AUD UIJ MVACC IJ CAN REST {
gen MORT`i' = `i'mort1 + `i'mort2 + `i'mort3
	replace `i'mort1 = `i'mort1 + (`i'mort1/MORT`i') * `i'mort99 if MORT`i' != 0 
	replace `i'mort2 = `i'mort2 + (`i'mort2/MORT`i') * `i'mort99 if MORT`i' != 0 
	replace `i'mort3 = `i'mort3 + (`i'mort3/MORT`i') * `i'mort99 if MORT`i' != 0 
}

keep year sex race age_gp *mort1 *mort2 *mort3   

reshape long Tmort LVDCmort PANCmort DMmort IHDmort ISTRmort HSTRmort HYPHDmort AUDmort UIJmort MVACCmort IJmort CANmort RESTmort, ///
 i(year age_gp sex race) j(edclass)

save "3_out data/allethn_sumCOD_0019_LE_decomp.dta", replace
outsheet using "${dir}/mortality/3_out data/allethn_sumCOD_0019_LE_decomp.csv" , comma replace
