set more off
clear


// dofile year 2000 - 2002
// no tempfile used
global dir `"C:/Users/marie/Dropbox/NIH2020/SIMAH_workplace/"'

cd "${dir}/mortality/3_out data"
#delimit cr


use "Mort2021.dta", clear


// ren fipsstr stlab
// merge m:1 stlab using state_lab_fips, keepusing(stlab st_FIPS)
        // tab stlab if _m==1
        // drop if _m==1 
		// drop _m
        // ren st_FIPS fipsstr


drop if resstatus>3
keep if ageunit==1

//gen ageint = int(age/1000);
//keep if ageint==1;
//replace age = age-1000;
//replace age =. if age==999;

keep if age>=18 & age<=150


keep year educ* hisprec sex age fipsstr fipsctyr icd10 icd358 ///
      E_condition* R_condition*

        gen sexx=1 if sex=="M"
        replace sexx=2 if sex=="F"
        drop sex
		ren sexx sex

        gen icd358R = real(icd358)
        drop icd358
		ren icd358R icd358

        gen ind_WNH = hisprec==6
		replace ind_WNH=. if hisprec==9
        gen ind_BNH = hisprec==7
		replace ind_BNH=. if hisprec==9
        gen ind_hisp = hisprec>=1 & hisprec<=5
		replace ind_hisp=. if hisprec==9

//rename educ89 educ not recognized in 2005 file
		
gen edclass=1 if educ89<=12 
replace edclass=2 if educ89>12  & educ89<=15 
replace edclass=3 if educ89>=16  & educ89<=17 
replace edclass=1 if educ20003<=3 & educflag==1
replace edclass=2 if educ20003>=4 & educ20003<=5 & educflag==1
replace edclass=3 if educ20003>=6 & educ20003<=8 & educflag==1

save Mort_2021_edit.dta, replace
