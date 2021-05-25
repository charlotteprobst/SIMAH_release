// https://ceprdata.org/cps-uniform-data-extracts/march-cps-supplement/march-cps-data/ 
set more off

// CPS data (March) 2000 - 2018

global dir `"C:/Users/marie/Dropbox/NIH2020/"'

cd "${dir}/Demography/"


forval i = 2000(1)2018 {
display `i'	
use "1_raw CPS data/cepr_march_`i'.dta", clear

	
	keep month state mis hhid hhseq perno year age id famno famhh famtyp p*fam research st*yr female *wgt r*all female educ ///
		married marstat empl unem nilf csr centcity ///
		suburb rural occly* rhr* dis* relhdh* wbho state 
		
		
save "3_out CPS data/cepr_march_`i'_edit.dta", replace
}

cd "${dir}/Demography/3_out CPS data"

append using cepr_march_2000_edit cepr_march_2001_edit cepr_march_2002_edit ////
cepr_march_2003_edit cepr_march_2004_edit cepr_march_2005_edit ////
cepr_march_2006_edit cepr_march_2007_edit cepr_march_2008_edit ////
cepr_march_2009_edit cepr_march_2010_edit cepr_march_2011_edit ////
cepr_march_2012_edit cepr_march_2013_edit cepr_march_2014_edit ////
cepr_march_2015_edit cepr_march_2016_edit cepr_march_2017_edit
	
save cepr_march_0018, replace

gen edclass = educ
recode edclass (1 2 = 1) (3 = 2) (4 5 = 3)
label def edlab 1 LEHS 2 SomeC 3 College
label val edclass edlab
label var edclass "Level of education"

save cepr_march_0018, replace

forval i = 2000(1)2018 {
	erase cepr_march_`i'_edit.dta
}

