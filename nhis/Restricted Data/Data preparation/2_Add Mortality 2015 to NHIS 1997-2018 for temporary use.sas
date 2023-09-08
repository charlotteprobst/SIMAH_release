
/********************************************************************************************************************/
/*************SAS SYNTAX TO ADD 2015 MORTALITY DATA TO NHIS 1997-2018 (TEMPORARY TO TEST CODE) **********************/
/********************************************************************************************************************/

/*Location of data */
libname combined "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Processed data\Restricted Use Data\";
libname mort    "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\Mortality Data\1997-2014";
OPTIONS nofmterr;    /*needed since we're not loading the format for each NHIS data file*/

data nhis; set combined.rdcp2058dataset_temp; run;
data mortality; set mort.nhis_1997_to_2014_mort_2015; run;


/* Combine NHIS and Mortality data*/
proc sort data=nhis; by PUBLICID; run;       
proc sort data=mortality; by PUBLICID; run;

Proc sql;
		create table nhis_mort as
		select L.*, R.*
		from nhis as L
		LEFT JOIN mortality as R
		on L.PUBLICID = R.PUBLICID;
	quit;

/*Calculate follow-up time and sample weights*/
data nhis_mort; set nhis_mort;
	if MORTSTAT = . then delete;
	if MORTSTAT = 0 then DODYEAR = 2015;  	/*assign last possible follow-up to those alive */
	if MORTSTAT = 0 then DODQTR = 4;		/*assign last possible follow-up to those alive */
	yrs_followup = (DODYEAR - SRVY_YR) + ((DODQTR - INTV_QRT)*.25);
	
	/*for the public use file; some noise is introduced so follow-up time may be negative*/
	if (DODYEAR = SRVY_YR) and (DODQTR <= INTV_QRT) then yrs_followup = 0.1; 
run;  
	

proc contents data=nhis_mort; run;

/*Save file*/
data combined.rdcp2058dataset_temp_mort; set nhis_mort; run;
