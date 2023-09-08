// TEST VERSION MORTALITY DATA SIMAH POLICY ANALYSIS //
// Carolin Kilian
// 15/03/2022

//1_Read_preprocess_mort_0021.do

//version 15.1
clear
cap log close

cd "/Users/carolinkilian/Desktop/SIMAH_workplace/mortality"

#delimit ;
	clear;
	*2001;
		infix
			resstatus 20
			region 26
			divstoc 27-28
			str popsizr 50
			str popsmsa 51
			educ89 52-53
			month 55-56
			sex 59
			race 60-61
			racerec3 62
			ageunit 64
			age 65-66
			marstat 77
			state_born 78-79
			hispanic 80-81
			hisprec 82
			fipssto 119-120
			fipsctyo 121-123
			fipsstr 124-125
			fipsctyr 126-128 str
			fipssmsar 129-132 str
			icd10 142-145 str
			icd358 146-148
			str E_condition_1 165-171 str E_condition_2 172-178  
	        str E_condition_3 179-185 str E_condition_4 186-192 str E_condition_5 193-199  
	        str E_condition_6 200-206  str E_condition_7 207-213 str E_condition_8 214-220 
	        str E_condition_9 221-227 str E_condition_10 228-234 str E_condition_11 235-241 
	        str E_condition_12 242-248 str E_condition_13 249-255 str E_condition_14 256-262 
	        str E_condition_15 263-269 str E_condition_16 270-276 str E_condition_17 277-283 
	        str E_condition_18 284-290 str E_condition_19 291-297 str E_condition_20 298-304 
			str R_condition_1 344-348 str R_condition_2 349-353		
			str R_condition_3 354-358 str R_condition_4 359-363					
		    str R_condition_5 364-368 str R_condition_6 369-373	
			str R_condition_7 374-378 str R_condition_8 379-383	
			str R_condition_9 384-388 str R_condition_10 389-393	
			str R_condition_11 394-398 str R_condition_12 399-403	
			str R_condition_13 404-408 str R_condition_14 409-413	
			str R_condition_15 414-418 str R_condition_16 419-423	
			str R_condition_17 424-428 str R_condition_18 429-433	
			str R_condition_19 434-438 str R_condition_20 439-443					

		using "1_raw data/Mort01us.dat";
		gen year=2001;
		compress;
		save "3_out data/Mort2001.dta", replace;

#delimit ;
	clear;
	*2002;
		infix
			resstatus 20
			region 26
			divstoc 27-28
			str popsizr 50
			str popsmsa 51
			educ89 52-53
			month 55-56
			sex 59
			race 60-61
			racerec3 62
			ageunit 64
			age 65-66
			marstat 77
			state_born 78-79
			hispanic 80-81
			hisprec 82
			fipssto 119-120
			fipsctyo 121-123
			fipsstr 124-125
			fipsctyr 126-128 str
			fipssmsar 129-132 str
			icd10 142-145 str
			icd358 146-148
			str E_condition_1 165-171 str E_condition_2 172-178  
	        str E_condition_3 179-185 str E_condition_4 186-192 str E_condition_5 193-199  
	        str E_condition_6 200-206  str E_condition_7 207-213 str E_condition_8 214-220 
	        str E_condition_9 221-227 str E_condition_10 228-234 str E_condition_11 235-241 
	        str E_condition_12 242-248 str E_condition_13 249-255 str E_condition_14 256-262 
	        str E_condition_15 263-269 str E_condition_16 270-276 str E_condition_17 277-283 
	        str E_condition_18 284-290 str E_condition_19 291-297 str E_condition_20 298-304 
			str R_condition_1 344-348 str R_condition_2 349-353		
			str R_condition_3 354-358 str R_condition_4 359-363					
		    str R_condition_5 364-368 str R_condition_6 369-373	
			str R_condition_7 374-378 str R_condition_8 379-383	
			str R_condition_9 384-388 str R_condition_10 389-393	
			str R_condition_11 394-398 str R_condition_12 399-403	
			str R_condition_13 404-408 str R_condition_14 409-413	
			str R_condition_15 414-418 str R_condition_16 419-423	
			str R_condition_17 424-428 str R_condition_18 429-433	
			str R_condition_19 434-438 str R_condition_20 439-443					

		using "1_raw data/Mort02us.dat" ;
		gen year=2002;
		compress;
		save "3_out data/Mort2002.dta", replace;
		
// 2_combine_mort_0021.do	

set more off
clear


// dofile year 2001 - 2002
// no tempfile used
global dir `"/Users/carolinkilian/Desktop/SIMAH_workplace/mortality"'

cd "${dir}/3_out data"
#delimit cr

forval i = 2001(1)2002 { 
display `i'
use "Mort`i'.dta", clear

//infile/2001 - 20002/

        drop if resstatus>3
        keep if ageunit==0
        keep if age>=18 & age<=150

keep year region educ89 hisprec month sex age fipsstr fipsctyr icd10 icd358 ///
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

append using Mort_2001_edit
save Mort_0002_edit, replace


