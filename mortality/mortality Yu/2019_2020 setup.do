*******************************************************
//This do file set up the 2019 and 2020 data in addition to 2000-18 data
//These two year data were directly downloaded from CDC website
//https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm#Mortality_Multiple
*******************************************************


clear
global dirname "C:\Users\yye\Documents\Yu\work\William Kerr_12-29-2020\"

**2019**
 #delimit ;
	clear;	
		infix
			resstatus 20
			str fipssto 21-22
			str fipsctyo 23-25
			str fipsstr 29-30
			fipsctyr 35-37
			str popcyr 43
			fipssmsar 47-50 		
			str popsizr 51
			str popsmsa 52
			str state_bornA 55-56
			educ89 61-62
			educ20003 63
			educflag 64
			month 65-66
			str sex 69
			ageunit 70
			age 71-73
			str marstatA 84
			str icd10 146-149
			str icd358 150-152
			race 445-446
			racerec3 449			
			hispanic2003 484-486
			hisprec 488
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
		using "${dirname}\Mort2019\VS19MORT.DUSMCPUB_r20210304";
		gen year=2019;
		compress;
		save "${dirname}\Mortality stata files\Original\Mort2019.dta", replace;	
 #delimit cr

 **2020**
 #delimit ;
	clear;	
		infix
			resstatus 20
			str fipssto 21-22
			str fipsctyo 23-25
			str fipsstr 29-30
			fipsctyr 35-37
			str popcyr 43
			fipssmsar 47-50 		
			str popsizr 51
			str popsmsa 52
			str state_bornA 55-56
			educ89 61-62
			educ20003 63
			educflag 64
			month 65-66
			str sex 69
			ageunit 70
			age 71-73
			str marstatA 84
			str icd10 146-149
			str icd358 150-152
			race 445-446
			racerec3 449			
			hispanic2003 484-486
			hisprec 488
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
		using "${dirname}\Mort2020\VS20MORT.DUSMCPUB_r20220105";
		gen year=2020;
		compress;
		save "${dirname}\Mortality stata files\Original\Mort2020.dta", replace;	
 #delimit cr