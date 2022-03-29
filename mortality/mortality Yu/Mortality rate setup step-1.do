

****************************************************************************
//Set up new mortality file according to the Cause of Disease in word file 
//in the same folder
*****************************************************************************

*************************************************************************
***First step: rename variable to accomodate mismatch variable type**
*************************************************************************

global dirname "C:\Users\yye\Documents\Yu\work\William Kerr_12-29-2020\Mortality stata files"

forvalues i=2000/2002 {
 use "${dirname}\original\mort`i'.dta", clear
 keep year resstatus educ89 hisprec sex age ageunit fipssto fipsstr fipsctyr icd10 icd358 ///
      E_condition* R_condition*
 rename fipssto fipssto_num
 rename fipsstr fipsstr_num
 
 **recode state to be comparable with 2003-2018***
  gen fipssto = "AL" if fipssto_num == 1
      replace fipssto = "AK" if fipssto_num == 2
      replace fipssto = "AZ" if fipssto_num == 4
      replace fipssto = "AR" if fipssto_num == 5
      replace fipssto = "CA" if fipssto_num == 6
      replace fipssto = "CO" if fipssto_num == 8
      replace fipssto = "CT" if fipssto_num == 9
      replace fipssto = "DE" if fipssto_num == 10
      replace fipssto = "DC" if fipssto_num == 11
      replace fipssto = "FL" if fipssto_num == 12
      replace fipssto = "GA" if fipssto_num == 13
      replace fipssto = "HI" if fipssto_num == 15
      replace fipssto = "ID" if fipssto_num == 16
      replace fipssto = "IL" if fipssto_num == 17
      replace fipssto = "IN" if fipssto_num == 18
      replace fipssto = "IA" if fipssto_num == 19
      replace fipssto = "KS" if fipssto_num == 20
      replace fipssto = "KY" if fipssto_num == 21	  
      replace fipssto = "LA" if fipssto_num == 22
      replace fipssto = "ME" if fipssto_num == 23
      replace fipssto = "MD" if fipssto_num == 24
      replace fipssto = "MA" if fipssto_num == 25
      replace fipssto = "MI" if fipssto_num == 26
      replace fipssto = "MN" if fipssto_num == 27
      replace fipssto = "MS" if fipssto_num == 28
      replace fipssto = "MO" if fipssto_num == 29
      replace fipssto = "MT" if fipssto_num == 30
      replace fipssto = "NE" if fipssto_num == 31
      replace fipssto = "NV" if fipssto_num == 32
      replace fipssto = "NH" if fipssto_num == 33
      replace fipssto = "NJ" if fipssto_num == 34
      replace fipssto = "NM" if fipssto_num == 35
      replace fipssto = "NY" if fipssto_num == 36
      replace fipssto = "NC" if fipssto_num == 37
      replace fipssto = "ND" if fipssto_num == 38
      replace fipssto = "OH" if fipssto_num == 39
      replace fipssto = "OK" if fipssto_num == 40
      replace fipssto = "OR" if fipssto_num == 41
      replace fipssto = "PA" if fipssto_num == 42
      replace fipssto = "RI" if fipssto_num == 44
      replace fipssto = "SC" if fipssto_num == 45
      replace fipssto = "SD" if fipssto_num == 46
      replace fipssto = "TN" if fipssto_num == 47
      replace fipssto = "TX" if fipssto_num == 48
      replace fipssto = "UT" if fipssto_num == 49
      replace fipssto = "VT" if fipssto_num == 50
      replace fipssto = "VA" if fipssto_num == 51
      replace fipssto = "WA" if fipssto_num == 53	 
      replace fipssto = "WV" if fipssto_num == 54
      replace fipssto = "WI" if fipssto_num == 55
      replace fipssto = "WY" if fipssto_num == 56	  
  gen fipsstr = "AL" if fipsstr_num == 1
      replace fipsstr = "AK" if fipsstr_num == 2
      replace fipsstr = "AZ" if fipsstr_num == 4
      replace fipsstr = "AR" if fipsstr_num == 5
      replace fipsstr = "CA" if fipsstr_num == 6
      replace fipsstr = "CO" if fipsstr_num == 8
      replace fipsstr = "CT" if fipsstr_num == 9
      replace fipsstr = "DE" if fipsstr_num == 10
      replace fipsstr = "DC" if fipsstr_num == 11
      replace fipsstr = "FL" if fipsstr_num == 12
      replace fipsstr = "GA" if fipsstr_num == 13
      replace fipsstr = "HI" if fipsstr_num == 15
      replace fipsstr = "ID" if fipsstr_num == 16
      replace fipsstr = "IL" if fipsstr_num == 17
      replace fipsstr = "IN" if fipsstr_num == 18
      replace fipsstr = "IA" if fipsstr_num == 19
      replace fipsstr = "KS" if fipsstr_num == 20
      replace fipsstr = "KY" if fipsstr_num == 21	  
      replace fipsstr = "LA" if fipsstr_num == 22
      replace fipsstr = "ME" if fipsstr_num == 23
      replace fipsstr = "MD" if fipsstr_num == 24
      replace fipsstr = "MA" if fipsstr_num == 25
      replace fipsstr = "MI" if fipsstr_num == 26
      replace fipsstr = "MN" if fipsstr_num == 27
      replace fipsstr = "MS" if fipsstr_num == 28
      replace fipsstr = "MO" if fipsstr_num == 29
      replace fipsstr = "MT" if fipsstr_num == 30
      replace fipsstr = "NE" if fipsstr_num == 31
      replace fipsstr = "NV" if fipsstr_num == 32
      replace fipsstr = "NH" if fipsstr_num == 33
      replace fipsstr = "NJ" if fipsstr_num == 34
      replace fipsstr = "NM" if fipsstr_num == 35
      replace fipsstr = "NY" if fipsstr_num == 36
      replace fipsstr = "NC" if fipsstr_num == 37
      replace fipsstr = "ND" if fipsstr_num == 38
      replace fipsstr = "OH" if fipsstr_num == 39
      replace fipsstr = "OK" if fipsstr_num == 40
      replace fipsstr = "OR" if fipsstr_num == 41
      replace fipsstr = "PA" if fipsstr_num == 42
      replace fipsstr = "RI" if fipsstr_num == 44
      replace fipsstr = "SC" if fipsstr_num == 45
      replace fipsstr = "SD" if fipsstr_num == 46
      replace fipsstr = "TN" if fipsstr_num == 47
      replace fipsstr = "TX" if fipsstr_num == 48
      replace fipsstr = "UT" if fipsstr_num == 49
      replace fipsstr = "VT" if fipsstr_num == 50
      replace fipsstr = "VA" if fipsstr_num == 51
      replace fipsstr = "WA" if fipsstr_num == 53	 
      replace fipsstr = "WV" if fipsstr_num == 54
      replace fipsstr = "WI" if fipsstr_num == 55
      replace fipsstr = "WY" if fipsstr_num == 56	   
  drop fipssto_num fipsstr_num
  ***end state recode***
 
 
  gen edclass = .
 replace edclass=1 if educ89<=12 & inrange(year,2000,2002)
 replace edclass=2 if educ89>=13 & educ89<=15 & inrange(year,2000,2002)
 replace edclass=3 if educ89>=16 & educ89<=17 & inrange(year,2000,2002)
 replace edclass=99 if educ89==99 & inrange(year,2000,2002)
 label def edlab 1 "LEHS" 2 "SomeC" 3 "4+yrs" 99 "missing"
 label val edclass edlab
 
 save c:\temp\mort`i'_original.dta, replace
} 

forvalues i=2003/2004 {
 use "${dirname}\original\mort`i'.dta", clear
 keep year resstatus educ89 educ20003 educflag hisprec sex age ageunit fipssto fipsstr fipsctyr icd10 icd358 ///
    E_condition* R_condition* 
	
 gen sex_num=1 if sex=="M"
 replace sex_num=2 if sex=="F"
 drop sex
 rename sex_num sex
 lab def sexlab 1 "1-Male" 2 "2-Famale"
 lab val sex sexlab 	
 
 gen edclass=1 if educ89<=12 & inrange(year,2003,2018)
 replace edclass=2 if educ89>=13  & educ89<=15 & inrange(year,2003,2018) 
 replace edclass=3 if educ89>=16  & educ89<=17 & inrange(year,2003,2018) 
 replace edclass=1 if educ20003<=3 & educflag==1 & inrange(year,2003,2018)
 replace edclass=2 if educ20003>=4 & educ20003<=5 & educflag==1 & inrange(year,2003,2020)
 replace edclass=3 if educ20003>=6 & educ20003<=8 & educflag==1 & inrange(year,2003,2020)
 replace edclass=99 if edclass==. & inrange(year,2003,2020)
 label def edlab 1 "LEHS" 2 "SomeC" 3 "4+yrs" 99 "missing"
 label val edclass edlab

 save c:\temp\mort`i'_original.dta, replace
} 

forvalues i=2005/2020 {
 use "${dirname}\original\mort`i'.dta", clear
 keep year resstatus educ89 educ20003 educflag hisprec sex age ageunit fipssto fipsstr fipsctyr icd10 icd358 ///
    E_condition* R_condition*
	
 gen sex_num=1 if sex=="M"
 replace sex_num=2 if sex=="F"
 drop sex
 rename sex_num sex
 lab def sexlab 1 "1-Male" 2 "2-Famale"
 lab val sex sexlab 	
 
 gen edclass=1 if educ89<=12 & inrange(year,2003,2020)
 replace edclass=2 if educ89>=13  & educ89<=15 & inrange(year,2003,2020) 
 replace edclass=3 if educ89>=16  & educ89<=17 & inrange(year,2003,2020) 
 replace edclass=1 if educ20003<=3 & educflag==1 & inrange(year,2003,2020)
 replace edclass=2 if educ20003>=4 & educ20003<=5 & educflag==1 & inrange(year,2003,2020)
 replace edclass=3 if educ20003>=6 & educ20003<=8 & educflag==1 & inrange(year,2003,2020)
 replace edclass=99 if edclass==. & inrange(year,2003,2020)
 label def edlab 1 "LEHS" 2 "SomeC" 3 "4+yrs" 99 "missing"
 label val edclass edlab
 
 save c:\temp\mort`i'_original.dta, replace
} 

**recode the entity contributing causes by dropping the first two numbers**
forvalues year = 2000/2020 {
  use "c:\temp\mort`year'_original.dta", clear
  forvalues i=1/20 {
    gen E_condition_`i'new = substr(E_condition_`i',3,4)
    drop E_condition_`i'
    rename E_condition_`i'new E_condition_`i'
  }
  save "c:\temp\mort`year'_original.dta", replace
}

*************************************************************************
***Second step: preliminary processing**
*************************************************************************

forvalues year = 2000/2020 {
  use "c:\temp\mort`year'_original.dta", clear
    
drop if resstatus>3
keep if (ageunit == 0 & inrange(year,2000,2002)) | (ageunit == 1 & inrange(year,2003,2020))
drop E_condition_1-E_condition_20
**include all age, restricting age in next step
*keep if age>=18 & age<=74

gen ind_WNH = hisprec==6
replace ind_WNH=. if hisprec==9
gen ind_BNH = hisprec==7
replace ind_BNH=. if hisprec==9
gen ind_hisp = hisprec>=1 & hisprec<=5
replace ind_hisp=. if hisprec==9

gen race=1 if ind_WNH==1
replace race=2 if ind_BNH==1
replace race=3 if ind_hisp==1
replace race=4 if race==.
tab hisprec if race==.

label define racelab 1 "1-WNH" 2 "2-BNH" 3 "3-Hisp" 4 "4-Others"
label val race racelab

*tab educ89 if inrange(year,2000,2002), m
*tab educ89 educflag if inrange(year,2003,2004), m
*tab educ20003 educflag if inrange(year,2003,2004), m
*tab educ89 educflag if inrange(year,2005,2018), m
*tab educ20003 educflag if inrange(year,2005,2018), m


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
replace age_gp=17 if age<=17 
replace age_gp=75 if age>=75 & age<=79
replace age_gp=80 if age>=80 & age<=84
replace age_gp=85 if age>=85 & age<=200
replace age_gp=999 if age==999
label def age_gplab 18 "18:18-24" 25 "25:25-29" 30 "30:30-34" ///
   35 "35:35-39" 40 "40:40-44" 45 "45:45-49" 50 "50:50-54" ///
   55 "55:55-59" 60 "60:60-64" 65 "65:65-69" 70 "70:70-74" ///
   17 "17:1-17" 75 "75:75-79" 80 "80:80-84" 85 "85:85+" 999 "999:Missing"
lab val age_gp age_gplab
 
gen icd358R = real(icd358)
drop icd358
ren icd358R icd358
		
save "c:\temp\mort`year'.dta", replace

}		
