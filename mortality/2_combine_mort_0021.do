
set more off
clear


// dofile year 2000 - 2002
// no tempfile used
global dir `"C:/Users/marie/Dropbox/NIH2020/SIMAH_workplace/"'

cd "${dir}/mortality/3_out data"
#delimit cr

forval i = 2000(1)2002 { 
display `i'
use "Mort`i'.dta", clear

//infile/2000 - 2002/

        drop if resstatus>3
        keep if ageunit==0
        keep if age>=18 & age<=150

keep year region month educ89 hisprec sex age fipsstr fipsctyr icd10 icd358 ///
      E_condition* R_condition*

        gen icd358R = real(icd358)
        drop icd358 
		ren icd358R icd358

        gen ind_WNH = hisprec==6 
		replace ind_WNH=. if hisprec==9
        gen ind_BNH = hisprec==7 
		replace ind_BNH=. if hisprec==9
        gen ind_hisp = hisprec>=1 & hisprec<=5 
		replace ind_hisp=. if hisprec==9

        gen edclass=1 if educ89<=12
        replace edclass=2 if educ89>=13 & educ89<=15
        replace edclass=3 if educ89>=16 & educ89<=17

save Mort_`i'_edit.dta, replace
}

append using Mort_2000_edit Mort_2001_edit
save Mort_0002_edit, replace



// dofile 2003 - 2004
// with state fips codes matched to death records


#delimit cr

forval i = 2003(1)2004 {
display `i'
use "Mort`i'.dta", clear


ren fipsstr stlab
merge m:1 stlab using "${dir}/mortality/1_raw data/state_lab_fips.dta", keepusing(stlab st_FIPS)
        tab stlab if _m==1
        drop if _m==1 
		drop _m
        ren st_FIPS fipsstr


drop if resstatus>3
keep if ageunit==1
keep if age>=18 & age<=150

keep year month educ* hisprec sex age fipsstr fipsctyr icd10 icd358 ///
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

gen edclass=1 if educ89<=12 
replace edclass=2 if educ89>12  & educ89<=15 
replace edclass=3 if educ89>=16  & educ89<=17 
replace edclass=1 if educ20003<=3 & educflag==1
replace edclass=2 if educ20003>=4 & educ20003<=5 & educflag==1
replace edclass=3 if educ20003>=6 & educ20003<=8 & educflag==1

save Mort_`i'_edit.dta, replace

}

append using  Mort_2003_edit 

save Mort_0304_edit, replace

append using Mort_0002_edit.dta

save Mort_0004_edit.dta, replace

tostring fipsstr, generate(fipsstr_s)
drop fipsstr
ren fipsstr_s fipsstr
save Mort_0004_edit.dta, replace

// dofile 2005 - 2019
// state fips codes not attached (state information missing in public death records since 2005)

forval i = 2005(1)2020 {
display `i'
use "Mort`i'.dta", clear


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


keep year month educ* hisprec sex age fipsstr fipsctyr icd10 icd358 ///
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

save Mort_`i'_edit.dta, replace

}

append using Mort_2005_edit Mort_2006_edit Mort_2007_edit Mort_2008_edit Mort_2009_edit Mort_2010_edit Mort_2011_edit Mort_2012_edit Mort_2013_edit Mort_2014_edit Mort_2015_edit Mort_2016_edit Mort_2017_edit Mort_2018_edit Mort_2019_edit

save Mort_0520_edit, replace
append using Mort_0004_edit.dta


forval i = 2021(1)2021 {
display `i'
use "Mort`i'.dta", clear


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


keep year month educ* sex age race40 hispanic fipsstr fipsctyr icd10 icd358 ///
      E_condition* R_condition*

        gen sexx=1 if sex=="M"
        replace sexx=2 if sex=="F"
        drop sex
		ren sexx sex

        gen icd358R = real(icd358)
        drop icd358
		ren icd358R icd358
// TODO: revise the way race is coded in 2021 (and do the same approach in 2020
// to see if recodes are comparable)
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

save Mort_`i'_edit.dta, replace

}



/* these are NH other races and hisp origin UNK */
*drop if race ==.;

gen race=1 if ind_WNH==1
replace race=2 if ind_BNH==1
replace race=3 if ind_hisp==1
tab hisprec if race==.

label var race "1 WNH 2 BNH 3 Hisp"

    gen age_gp=18 if age>=18 & age<=24
replace age_gp=25 if age>=25 & age<=29
replace age_gp=30 if age>=30 & age<=34
replace age_gp=35 if age>=35 & age<=39
replace age_gp=40 if age>=40 & age<=44
replace age_gp=45 if age>=45 & age<=49
replace age_gp=50 if age>=50 & age<=54
replace age_gp=55 if age>=55 & age<=59
replace age_gp=60 if age>=60 & age<=64
replace age_gp=65 if age>=65 & age<=69
replace age_gp=70 if age>=70 & age<=74
replace age_gp=75 if age>=75 & age<=79
replace age_gp=80 if age>=80 & age<=150

label var age_gp "18-24, 25-29, 30-34, etc."

replace edclass = 99 if edclass==.
label def edlab 1 "LEHS" 2 "SomeC" 3 "4+yrs" 99 "missing"
label val edclass edlab

save "${dir}/mortality/3_out data/mort_0020_complete.dta", replace

/*
local datafiles: dir "`workdir'" files "*.dta"

foreach datafile of local datafiles {
        rm `datafile'
}
*/

forval i = 2000(1)2020 {
	erase Mort_`i'_edit.dta
	erase Mort`i'.dta

}
