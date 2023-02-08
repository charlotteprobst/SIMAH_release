//version 15.1
clear
cap log close

cd "/Users/marie/Dropbox/NIH2020/SIMAH_workplace/mortality"

*set more off

#delimit ;
	clear;
	*2000;
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

		using "1_raw data/Mort00us.dat";
		gen year=2000;
		compress;
		save "3_out data/Mort2000.dta", replace;
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
		
		
#delimit ;
	clear;	
	*2003;
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

		using "1_raw data/Mort03us.dat" ;
		gen year=2003;
		compress;
		save "3_out data/Mort2003.dta", replace;
#delimit ;
	clear;	
	*2004;
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

		using "1_raw data/Mort04us.dat" ;
		gen year=2004;
		compress;
		save "3_out data/Mort2004.dta", replace;
#delimit ;
	clear;	
	*2005;
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

		using "1_raw data/Mort05uspb.dat" ;
		gen year=2005;
		compress;
		save "3_out data/Mort2005.dta", replace;
#delimit ;
	clear;	
	*2006;
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

		using "1_raw data/MORT06.DUSMCPUB" ;
		gen year=2006;
		compress;
		save "3_out data/Mort2006.dta", replace;
#delimit ;
	clear;	
	*2007;
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

		using "1_raw data/VS07MORT.DUSMCPUB" ;
		gen year=2007;
		compress;
		save "3_out data/Mort2007.dta", replace;
#delimit ;
	clear;	
	*2008;
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

		using "1_raw data/Mort2008us.dat" ;
		gen year=2008;
		compress;
		save "3_out data/Mort2008.dta", replace;
#delimit ;
	clear;	
	*2009;
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

		using "1_raw data/VS09MORT.DUSMCPUB" ;
		gen year=2009;
		compress;
		save "3_out data/Mort2009.dta", replace;
#delimit ;
	clear;	
	*2010;
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

		using "1_raw data/VS10MORT.DUSMCPUB" ;
		gen year=2010;
		compress;
		save "3_out data/Mort2010.dta", replace;

#delimit ;
	clear;	
	*2011;
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

		using "1_raw data/VS11MORT.DUSMCPUB" ;
		gen year=2011;
		compress;
		save "3_out data/Mort2011.dta", replace;
#delimit ;
	clear;	
	*2012;
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

		using "1_raw data/VS12MORT.DUSMCPUB" ;
		gen year=2012;
		compress;
		save "3_out data/Mort2012.dta", replace;
#delimit ;
	clear;	
	*2013;
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

		using "1_raw data/VS13MORT.DUSMCPUB" ;
		gen year=2013;
		compress;
		save "3_out data/Mort2013.dta", replace;
#delimit ;
	clear;	
	*2014;
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

		using "1_raw data/VS14MORT.DUSMCPUB";
		gen year=2014;
		compress;
		save "3_out data/Mort2014.dta", replace;

	
#delimit ;
	clear;	
	*2015;
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
			str ucod 146-162
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

		using "1_raw data/VS15MORT.DUSMCPUB" ;
		gen year=2015;
		compress;
		save "3_out data/Mort2015.dta", replace;
		
		
#delimit ;
	clear;	
	*2016;
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

		using "1_raw data/VS16MORT.DUSMCPUB" ;
		gen year=2016;
		compress;
		save "3_out data/Mort2016.dta", replace;
#delimit ;
	clear;	
	*2017;
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

		using "1_raw data/VS17MORT.DUSMCPUB" ;
		gen year=2017;
		compress;
		save "3_out data/Mort2017.dta", replace;
#delimit ;
	clear;
	*2018 ;
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

		using "1_raw data/Mort2018US.PubUse.txt" ;
		gen year=2018 ;
		compress;
		save "3_out data/Mort2018.dta", replace;


#delimit ;
	clear;
	*2019 ;
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

		using "1_raw data/VS19MORT.DUSMCPUB_r20210304" ;
		gen year=2019 ;
		compress;
		save "3_out data/Mort2019.dta", replace;

#delimit ;
	clear;
	*2020 ;
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

		using "1_raw data/VS20MORT.DUSMCPUB_r20220105" ;
		gen year=2020 ;
		compress;
		save "3_out data/Mort2020.dta", replace;
		
#delimit ;
	clear;
	*2021 ;
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

		using "1_raw data/VS20MORT.DUSMCPUB_r20220105" ;
		gen year=2020 ;
		compress;
		save "3_out data/Mort2021.dta", replace;		
