//This file sets up the most recent SIMAH mortality data after supplement funding 
//The major change is to 1) use a new AUD definition, 2) redefine liver cirrhosis by adding hepatitis cirrhosis  
//The codes read in Yu's mortality data 
//The file is an updated from "Replicate Charlotte's codes in Github.do" under the same folder (read notes there)
//This file is used to modify Charlott's codes which read her own saved mortality data 

use c:\temp\mort2000.dta, clear 
forvalues i = 2001/2020 {
	append using c:\temp\mort`i'.dta
}

**save "${dir}/mortality/3_out data/mort_0020_complete.dta", replace
save "c:\temp\mort_0020_complete.dta", replace

use "c:\temp\mort_0020_complete.dta", clear 

//the codes are specific for my data 
//not needed for Charlotte's data 
drop if age_gp == 999
drop if age_gp == 17
recode age_gp 85=80 
lab drop age_gplab 

//the codes below doesn't need to run 
*rename race race18 
*gen race = .
*replace race = 1 if race18 == 1
*replace race = 2 if race18 == 2
*replace race = 3 if race18 == 3
*replace race = 4 if race18 == .
*lab var race "Race/ethnicity"
*label define racelab 1 "White" 2 "Black" 3 "Hispanic" 4 "Other"
*label values race racelab
*drop race18

drop if age_gp == .

keep  icd10 year race sex age_gp edclass
gen str1 xcode = icd10 


///////////////////////////////////////////////////////////////////////////////

// specific causes of death (using icd codes)

/*specific causes of death (using icd codes)
url for ICD-10 2019 : https://icd.who.int/browse10/2019/en     */

// COVID (defined according to Gundlapalli et al. 2021)

gen cov = 1 if inlist(icd10, "U071")

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


// old AUD, not used any more
// AUD F10-F10.9, G31.2, G72.1, Q86.0, R78.0, X45-X45.9, Y15-Y15.9
*gen aud = 1 if inrange(icd10, "F10", "F109") | inlist(icd10, "G312", "G312")  ///
*	| inrange(icd10, "G721", "G721") | inrange(icd10, "Q860", "Q860") ///
*	| inrange(icd10, "R780", "R780") ///
*	| inrange(icd10, "X45", "X459") | inrange(icd10, "Y15", "Y159") 

// updated AUD 
// E24.4, F10-F10.9, G62.1, G31.2, G72.1, I42.6, K29.2, K85.2, K86.0, 
// R78.0, X45-X45.9, Y15-Y15.9, Y90, Y91, Y91.0 – Y91.3, Y91.9

gen aud = 1 if inlist(icd10, "E244") | inrange(icd10, "F10", "F109") | inlist(icd10, "G312")  ///
	| inlist(icd10, "G721") | inlist(icd10, "I426") | inlist(icd10, "K292") ///
	| inlist(icd10, "K852") | inlist(icd10, "K860") | inlist(icd10, "R780") ///
	| inrange(icd10, "X45", "X459") | inrange(icd10, "Y15", "Y159") ///
	| inrange(icd10, "Y90", "Y909") | inrange(icd10, "Y91", "Y919")
	

//////////////////////////////////////////////////////////////////////

// Suicide (Intentional self harm) [X60-X84, Y87.0]

generate ij = 1 if inrange(icd10, "X60", "X84") | inrange(icd10, "Y870", "Y870")


////////////////////////////////////////////////////////////////////

// Liver Disease and Cirrhosis [K70, K71.3-K71.5, K71.7, K72-K74]
// old codes, not used any more 
// generate lvdc = 1 if inrange(icd10, "K70", "K709") | inrange(icd10, "K713", "K715") ///
//	| inrange(icd10, "K717", "K717")  | inrange(icd10, "K72", "K749") 
	
// update liver disease and cirrhosis Oct 2022 
// [K70, K73, K74, K76.0, K76.6]
generate lvdc = 1 if inrange(icd10, "K70", "K709") | inrange(icd10, "K73", "K739") ///
	| inrange(icd10, "K74", "K749")  | inlist(icd10, "K760", "K766") 

// add liver disease and cirrhosis due to chronic hepatitis Oct 2022
// B18 
generate hlvdc = 1 if inrange(icd10, "B18", "B189")

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
replace rest = 1 if (cov == . & lvdc == . & hlvdc == . & panc == . & dm == . & ihd == . & istr == . & hstr == . & hyphd == . ///
	& aud == . & uij == . & mvacc == . & ij == . & cancer == .)
	
*tab year icd10 if aud == 1 & panc == 1 
 	
save "c:\temp\1_allethn_mortbycause_0020_LE_decomp.dta", replace	


use "c:\temp\1_allethn_mortbycause_0020_LE_decomp.dta", clear

// Test that all deaths have been assigned exactly once
egen test = rowtotal(cov lvdc hlvdc panc dm ihd istr hstr hyphd aud uij mvacc ij cancer)
tab test rest, m
drop test	

/////////////////////////////////////////////////////////////////////////

// Sum by COD (TOTAL + 14 Catgories + Rest)
 
gen one=1

tabstat cov lvdc hlvdc panc dm ihd istr hstr hyphd aud uij mvacc ij cancer rest one, ///
   stat(sum) col(stat) format(%12.0f)

*egen test = rowtotal(cov lvdc panc dm ihd istr hstr hyphd aud uij mvacc ij cancer rest one)
*tab test
*drop test	


bysort age_gp sex edclass race year: egen Tmort = total(one)

bysort age_gp sex edclass race year: egen COVmort = total(cov)

bysort age_gp sex edclass race year: egen LVDCmort = total(lvdc)
bysort age_gp sex edclass race year: egen HLVDCmort = total(hlvdc)
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
save "c:\temp\2_allethn_sumCOD_0020_LE_decomp.dta", replace


//assigning deaths without education information
//Deaths with missing data on SES were assigned to an education category based 
//on the proportion in each education group by year, race/ethnicity, sex, age group, and cause of death.

use "c:\temp\2_allethn_sumCOD_0020_LE_decomp.dta", clear

keep year sex age_gp edclass race *mort
reshape wide Tmort COVmort LVDCmort HLVDCmort PANCmort DMmort IHDmort ISTRmort HSTRmort HYPHDmort ///
	AUDmort UIJmort MVACCmort IJmort CANmort RESTmort, i(year age_gp sex race) j(edclass)
sum *mort99

foreach i in T COV LVDC HLVDC PANC DM IHD ISTR HSTR HYPHD AUD UIJ MVACC IJ CAN REST {
gen MORT`i' = `i'mort1 + `i'mort2 + `i'mort3
	replace `i'mort1 = `i'mort1 + (`i'mort1/MORT`i') * `i'mort99 if MORT`i' != 0 
	replace `i'mort2 = `i'mort2 + (`i'mort2/MORT`i') * `i'mort99 if MORT`i' != 0 
	replace `i'mort3 = `i'mort3 + (`i'mort3/MORT`i') * `i'mort99 if MORT`i' != 0 
}

//deal with very rare case in which the deaths only fall into "missing" education 
//Split the deaths evenly across the three eduation categories 
foreach i in T COV LVDC HLVDC PANC DM IHD ISTR HSTR HYPHD AUD UIJ MVACC IJ CAN REST {
	replace `i'mort1 = 1/3*`i'mort99 if MORT`i' == 0 & `i'mort99 != 0
	replace `i'mort2 = 1/3*`i'mort99 if MORT`i' == 0 & `i'mort99 != 0
	replace `i'mort3 = 1/3* `i'mort99 if MORT`i' == 0 & `i'mort99 != 0
}

keep year sex race age_gp *mort1 *mort2 *mort3   

reshape long Tmort COVmort LVDCmort HLVDCmort PANCmort DMmort IHDmort ISTRmort HSTRmort HYPHDmort ///
	AUDmort UIJmort MVACCmort IJmort CANmort RESTmort, ///
	i(year age_gp sex race) j(edclass)

lab define edlab 1 "LEHS" 2 "SomeC" 3 "College", modify

foreach v of varlist Tmort-RESTmort {
	rename `v', lower
}

tabstat *mort, stat(sum) col(stat) format(%12.0f)
   
sort year age_gp sex race edclass 
save "c:\temp\allethn_sumCOD_0020_LE_decomp.dta", replace
cd "I:\Charlotte R01\William Kerr_12-29-2020\stata codes\New Data setup\New Data  setup after supplement"
**outsheet using "${dir}mortality/3_out data/allethn_sumCOD_0020_LE_decomp.csv" , comma replace
outsheet using "allethn_sumCOD_0020_LE_decomp.csv" , comma replace

//now check the replicated data is exactly the same as Charlott's 
cd "I:\Charlotte R01\William Kerr_12-29-2020\stata codes\New Data setup\New Data  setup after supplement"

clear
import delimited allethn_sumCOD_0020_LE_decomp.csv

gen racenum = 1 if race == "White"
replace racenum = 2 if race == "Black"
replace racenum = 3 if race == "Hispanic"
replace racenum = 4 if race == "Other"
drop race 
rename racenum race

gen edclassnum = 1 if edclass == "LEHS"
replace edclassnum = 2 if edclass == "SomeC"
replace edclassnum = 3 if edclass == "College"
drop edclass 
rename edclassnum edclass

foreach v of varlist tmort-restmort {
	rename `v' `v'_1 
}
sort year age_gp sex race edclass 
merge 1:1 year age_gp sex race edclass using "c:\temp\allethn_sumCOD_0020_LE_decomp.dta"

foreach v of varlist tmort-restmort {
	gen `v'dif = `v'-`v'_1 
}
tabstat tmort-restmort, stat(sum) format(%9.0f) col(stat)
tabstat tmort_1-restmort_1, stat(sum) format(%9.0f) col(stat)

tabstat tmort-restmort, by(year) stat(sum) format(%9.0f)
tabstat tmort_1-restmort_1, by(year) stat(sum) format(%9.0f)  