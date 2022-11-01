
/********************************************************************************************************************/
/*******************************************SAS SYNTAX TO CALCULATE AGES ********************************************/
/********************************************************************************************************************/


/* Test data*/
/*
data data_name;
  input DOBDAY DOBMONTH DOBYEAR DODDAY DODMONTH DODYEAR INTVDAY INTVMONTH INTVYEAR;
datalines;
25 06 1991 14 06 2050 13 06 2022
25 03 1990 14 06 2040 23 01 2020
20 06 1991 . . . 19 08 2021 
;
*/

data data_name; set data_name;
	dob = mdy(DOBMONTH, DOBDAY, DOBYEAR);
	intv = mdy(INTVMONTH, INTVDAY, INTVYEAR);
	dod = mdy(DODMONTH, DODDAY, DODYEAR);
	last = mdy (12, 31, 2019);
	format dob intv dod last date9.; 
	bl_age = round(yrdif (dob, intv, "ACTUAL"),0.01);    /*baseline age*/
	death_age = round(yrdif (dob, dod, "ACTUAL"), 0.01); /*age at death*/
	end_age = death_age; if end_age =. then end_age =  round(yrdif (dob, last, "ACTUAL"), 0.01);  /* end age (death or last followup)*/ 	
run;


