
/********************************************************************************************************************/
/******************SAS SYNTAX TO EXTRACT AND COMBINE NHIS 1997-2018 DATA *************************/
/********************************************************************************************************************/

/*Location of individual data files*/
libname lib1997 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\1997";
libname lib1998 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\1998";
libname lib1999 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\1999";
libname lib2000 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\2000";
libname lib2001 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\2001";
libname lib2002 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\2002";
libname lib2003 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\2003";
libname lib2004 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\2004";
libname lib2005 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\2005";
libname lib2006 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\2006";
libname lib2007 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\2007";
libname lib2008 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\2008";
libname lib2009 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\2009";
libname lib2010 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\2010";
libname lib2011 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\2011";
libname lib2012 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\2012";
libname lib2013 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\2013";
libname lib2014 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\2014";
libname lib2015 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\2015";
libname lib2016 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\2016";
libname lib2017 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\2017";
libname lib2018 "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\NHIS Raw Data\2018";
libname mort    "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Original data\Mortality Data\1997-2014";

/*Location of combined, cleaned data file */
libname combined "C:\Users\klajd\OneDrive\SIMAH\SIMAH_workspace\nhis\Processed data\Restricted Use Data\";

OPTIONS nofmterr;    /*needed since we're not loading the format for each NHIS data file*/
options nonotes;	 /*suppress notes in log - too many notes about missing formats*/



/*********************************************************************************************************************/
/****************************PART 1: EXTRACT ALL OF THE DATA FROM PUBLIC USE DATA FILES*******************************/
/********************************************************************************************************************/

/************ NHIS 1997 ***************/

/*Sample Adult File*/
data samadult1997; set lib1997.samadult;
	/* Create unique identifier*/
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||PX)); 
	/*select variables to keep*/
	keep PUBLICID SRVY_YR INTV_QRT HHX FMX PX WTFA_SA STRATUM PSU ALC1YR ALCLIFE ALC12MYR ALCAMT ALC5UPYR	
	SMKSTAT2 BMI HYPEV HYPDIFV DIBEV VIGFREQW VIGMIN MODFREQW MODMIN SAD NERVOUS RESTLESS HOPELESS 
	EFFORT WORTHLS;
run;

/*Person File*/
data personsx1997; set lib1997.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||PX));	
	keep PUBLICID SEX AGE_P R_MARITL DOINGLW WHYNOWRK ORIGIN RACEREC MRACE_P EDUC RAT_CAT PHSTAT USBORN_P; 	
run;


		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis1997 as
				select L.*, R.*
				from samadult1997 as L
				LEFT JOIN personsx1997 as R
				on L.PUBLICID = R.PUBLICID;
			quit;



/************ NHIS 1998 ***************/

data samadult1998; set lib1998.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||PX));					
	keep PUBLICID SRVY_YR INTV_QRT HHX FMX PX WTFA_SA STRATUM PSU ALC1YR ALCLIFE ALC12MYR ALCAMT ALC5UPYR	
		SMKSTAT2 BMI HYPEV HYPDIFV DIBEV VIGFREQW VIGMIN MODFREQW MODMIN 
		SAD NERVOUS RESTLESS HOPELESS EFFORT WORTHLS;
	run;

data personsx1998; set lib1998.personsx;
	length PUBLICID $14;PUBLICID = trim(left(SRVY_YR||HHX||FMX||PX)); 						
	keep PUBLICID SEX AGE_P R_MARITL DOINGLW WHYNOWRK HISPCODE MRACE_P EDUC RAT_CAT PHSTAT USBORN_P;	 
run;

		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
			create table nhis1998 as
			select L.*, R.*
			from samadult1998 as L
			LEFT JOIN personsx1998 as R
			on L.PUBLICID = R.PUBLICID;
		quit;





/************ NHIS 1999 ***************/

data samadult1999; set lib1999.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||PX));  				 
	keep PUBLICID SRVY_YR INTV_QRT HHX FMX PX WTFA_SA STRATUM PSU ALC1YR ALCLIFE ALC12MYR ALCAMT ALC5UPYR  	 	
		SMKSTAT2 BMI HYPEV HYPDIFV DIBEV VIGFREQW VIGMIN MODFREQW MODMIN 
		SAD NERVOUS RESTLESS HOPELESS EFFORT WORTHLS MHDINTWY MHDSAD2W MHDSADWY; 
	run;
	

data personsx1999; set lib1999.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||PX));  							
	keep PUBLICID SEX AGE_P R_MARITL DOINGLW WHYNOWRK HISPCODR MRACBR_P EDUC RAT_CAT PHSTAT USBORN_P;  
		run;

		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis1999 as
				select L.*, R.*
				from samadult1999 as L
				LEFT JOIN personsx1999 as R
				on L.PUBLICID = R.PUBLICID;
			quit;



/************ NHIS 2000 ***************/

data samadult2000; set lib2000.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||PX));  				 
	keep PUBLICID SRVY_YR INTV_QRT HHX FMX PX WTFA_SA STRATUM PSU ALC1YR ALCLIFE ALC12MYR ALCAMT ALC5UPYR  	 	
		SMKSTAT2 BMI HYPEV HYPDIFV DIBEV VIGFREQW VIGMIN MODFREQW MODMIN 
		SAD NERVOUS RESTLESS HOPELESS EFFORT WORTHLS;
run;

data personsx2000; set lib2000.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||PX));  							
	keep PUBLICID SEX AGE_P R_MARITL DOINGLW WHYNOWRK HISCOD_I MRACBP_I EDUC RAT_CAT PHSTAT USBRTH_P;  
run;

		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis2000 as
				select L.*, R.*
				from samadult2000 as L
				LEFT JOIN personsx2000 as R
				on L.PUBLICID = R.PUBLICID;
		quit;



/************ NHIS 2001 ***************/

data samadult2001; set lib2001.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||PX));  				 
	keep PUBLICID SRVY_YR INTV_QRT HHX FMX PX WTFA_SA STRATUM PSU ALC1YR ALCLIFE ALC12MYR ALCAMT ALC5UPYR  	 	
		SMKSTAT2 BMI HYPEV HYPDIFV DIBEV VIGFREQW VIGMIN MODFREQW MODMIN 
		SAD NERVOUS RESTLESS HOPELESS EFFORT WORTHLS;
	run;

data personsx2001; set lib2001.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||PX));  							
	keep PUBLICID SEX AGE_P R_MARITL DOINGLW1 WHYNOWK1 HISCOD_I MRACBP_I EDUC RAT_CAT PHSTAT USBRTH_P;  
run;


		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis2001 as
				select L.*, R.*
				from samadult2001 as L
				LEFT JOIN personsx2001 as R
				on L.PUBLICID = R.PUBLICID;
			quit;


/************ NHIS 2002 ***************/

data samadult2002; set lib2002.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||PX));  				 
	keep PUBLICID SRVY_YR INTV_QRT HHX FMX PX WTFA_SA STRATUM PSU ALC1YR ALCLIFE ALC12MYR ALCAMT ALC5UPYR  	 	 
		SMKSTAT2 BMI HYPEV HYPDIFV DIBEV VIGFREQW VIGMIN MODFREQW MODMIN 
		SAD NERVOUS RESTLESS HOPELESS EFFORT WORTHLS;
run;

data personsx2002; set lib2002.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||PX));  							
	keep PUBLICID SEX AGE_P R_MARITL DOINGLW1 WHYNOWK1 HISCOD_I MRACBP_I EDUC RAT_CAT PHSTAT GEOBRTH;  
run;

		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis2002 as
				select L.*, R.*
				from samadult2002 as L
				LEFT JOIN personsx2002 as R
				on L.PUBLICID = R.PUBLICID;
			quit;


/************ NHIS 2003 ***************/

data samadult2003; set lib2003.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||PX));  				 
	keep PUBLICID SRVY_YR INTV_QRT HHX FMX PX WTFA_SA STRATUM PSU ALC1YR ALCLIFE ALC12MYR ALCAMT ALC5UPYR   
		 SMKSTAT2 BMI HYPEV HYPDIFV DIBEV VIGFREQW VIGMIN MODFREQW MODMIN 
		SAD NERVOUS RESTLESS HOPELESS EFFORT WORTHLS;
run;
		
data personsx2003; set lib2003.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||PX));  							
	keep PUBLICID SEX AGE_P R_MARITL DOINGLW1 WHYNOWK1 HISCODI2 MRACBPI2 EDUC RAT_CAT PHSTAT GEOBRTH;  
run;


		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis2003 as
				select L.*, R.*
				from samadult2003 as L
				LEFT JOIN personsx2003 as R
				on L.PUBLICID = R.PUBLICID;
			quit;




/************ NHIS 2004 ***************/

/*Sample Adult File*/
data samadult2004; set lib2004.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX));   
	length FAM_ID $14; FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep PUBLICID FAM_ID SRVY_YR HHX FMX FPX WTFA_SA ALC1YR ALCLIFE ALC12MYR ALCAMT ALC5UPYR 
		SMKSTAT2 BMI HYPEV HYPDIFV DIBEV DOINGLWA WHYNOWKA VIGFREQW VIGMIN MODFREQW MODMIN SAD NERVOUS RESTLESS HOPELESS EFFORT WORTHLS;
run;

/*Person File*/
data personsx2004; set lib2004.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX));  						 
	keep PUBLICID SEX AGE_P R_MARITL HISCODI2 MRACBPI2 EDUC1 STRATUM PSU PHSTAT GEOBRTH;   /*in 2004 the PSU and STRATUM variables were only available on the person file*/ 
run;

/*Household File*/
data househld2004; set lib2004.househld;   /*in 2004 the Quater that the survey was completed in was only available on the household file*/ 
	keep HHX INTV_QRT;
run;

/*Family File*/
data familyxx2004; set lib2004.familyxx;  
	length FAM_ID $14; FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep FAM_ID RAT_CAT;
run;


		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis2004_pre1 as
				select L.*, R.*
				from samadult2004 as L
				LEFT JOIN personsx2004 as R
				on L.PUBLICID = R.PUBLICID;
			quit;

		Proc sql;
				create table nhis2004_pre2 as
				select L.*, R.*
				from nhis2004_pre1 as L
				LEFT JOIN househld2004 as R
				on L.HHX = R.HHX;
			quit;

		Proc sql;
				create table nhis2004 as
				select L.*, R.*
				from nhis2004_pre2 as L
				LEFT JOIN familyxx2004 as R
				on L.FAM_ID = R.FAM_ID;
			quit;



/************ NHIS 2005 ***************/

data samadult2005; set lib2005.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX));   
	length FAM_ID $14; FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep PUBLICID FAM_ID SRVY_YR INTV_QRT HHX FMX FPX WTFA_SA STRATUM PSU ALC1YR ALCLIFE ALC12MYR ALCAMT 
		ALC5UPYR SMKSTAT2 BMI HYPEV HYPDIFV DIBEV DOINGLWA WHYNOWKA VIGFREQW VIGMIN MODFREQW MODMIN
		SAD NERVOUS RESTLESS HOPELESS EFFORT WORTHLS;
run;

data personsx2005; set lib2005.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX));  						 
	keep PUBLICID SEX AGE_P R_MARITL HISCODI2 MRACBPI2 EDUC1 PHSTAT GEOBRTH;  
run;

data familyxx2005; set lib2005.familyxx;  
	length FAM_ID $14; FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep FAM_ID RAT_CAT;
run;

		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis2005_pre1 as
				select L.*, R.*
				from samadult2005 as L
				LEFT JOIN personsx2005 as R
				on L.PUBLICID = R.PUBLICID;
			quit;

		Proc sql;
				create table nhis2005 as
				select L.*, R.*
				from nhis2005_pre1 as L
				LEFT JOIN familyxx2005 as R
				on L.FAM_ID = R.FAM_ID;
			quit;


/************ NHIS 2006 ***************/

data samadult2006; set lib2006.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX)); 				 
	length FAM_ID $14;     FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep PUBLICID FAM_ID SRVY_YR INTV_QRT HHX FMX FPX WTFA_SA STRAT_P PSU_P ALC1YR ALCLIFE ALC12MYR ALCAMT 
		ALC5UPYR SMKSTAT2 BMI HYPEV HYPDIFV DIBEV DOINGLWA WHYNOWKA VIGFREQW VIGMIN MODFREQW MODMIN
		SAD NERVOUS RESTLESS HOPELESS EFFORT WORTHLS;
run;

data personsx2006; set lib2006.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX));  						 
	keep PUBLICID SEX AGE_P R_MARITL HISCODI3 MRACBPI2 EDUC1 PHSTAT GEOBRTH;  
run;

data familyxx2006; set lib2006.familyxx;  
	length FAM_ID $14; FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep FAM_ID RAT_CAT;
run;


		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis2006_pre1 as
				select L.*, R.*
				from samadult2006 as L
				LEFT JOIN personsx2006 as R
				on L.PUBLICID = R.PUBLICID;
			quit;

		Proc sql;
				create table nhis2006 as
				select L.*, R.*
				from nhis2006_pre1 as L
				LEFT JOIN familyxx2006 as R
				on L.FAM_ID = R.FAM_ID;
			quit;


/************ NHIS 2007 ***************/

data samadult2007; set lib2007.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX)); 				 
	length FAM_ID $14;     FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep PUBLICID FAM_ID SRVY_YR INTV_QRT HHX FMX FPX WTFA_SA STRAT_P PSU_P ALC1YR ALCLIFE ALC12MYR ALCAMT 
		ALC5UPYR SMKSTAT2 BMI HYPEV HYPDIFV DIBEV DOINGLWA WHYNOWKA VIGFREQW VIGMIN MODFREQW MODMIN SAD 
		NERVOUS RESTLESS HOPELESS EFFORT WORTHLS DEPYR;
run;

data personsx2007; set lib2007.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX));  						 
	keep PUBLICID SEX AGE_P R_MARITL HISCODI3 MRACBPI2 EDUC1 PHSTAT GEOBRTH;  
run;

data familyxx2007; set lib2007.familyxx;  
	length FAM_ID $14; FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep FAM_ID RAT_CAT3;
run;


		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis2007_pre1 as
				select L.*, R.*
				from samadult2007 as L
				LEFT JOIN personsx2007 as R
				on L.PUBLICID = R.PUBLICID;
			quit;
			Proc sql;
				create table nhis2007 as
				select L.*, R.*
				from nhis2007_pre1 as L
				LEFT JOIN familyxx2007 as R
				on L.FAM_ID = R.FAM_ID;
			quit;


/************ NHIS 2008 ***************/

data samadult2008; set lib2008.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX)); 				 
	length FAM_ID $14;     FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep PUBLICID FAM_ID SRVY_YR INTV_QRT HHX FMX FPX WTFA_SA STRAT_P PSU_P ALC1YR ALCLIFE ALC12MYR ALCAMT 
		ALC5UPYR SMKSTAT2 BMI HYPEV HYPDIFV DIBEV DOINGLWA WHYNOWKA VIGFREQW VIGMIN MODFREQW MODMIN
		SAD NERVOUS RESTLESS HOPELESS EFFORT WORTHLS DEPRESS;
	run;

data personsx2008; set lib2008.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX));  						 
	keep PUBLICID SEX AGE_P R_MARITL HISCODI3 MRACBPI2 EDUC1 PHSTAT GEOBRTH;  
run;

data familyxx2008; set lib2008.familyxx;  
	length FAM_ID $14; FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep FAM_ID RAT_CAT3;
run;

		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis2008_pre1 as
				select L.*, R.*
				from samadult2008 as L
				LEFT JOIN personsx2008 as R
				on L.PUBLICID = R.PUBLICID;
			quit;
		Proc sql;
				create table nhis2008 as
				select L.*, R.*
				from nhis2008_pre1 as L
				LEFT JOIN familyxx2008 as R
				on L.FAM_ID = R.FAM_ID;
			quit;

/************ NHIS 2009 ***************/

data samadult2009; set lib2009.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX)); 				 
	length FAM_ID $14;     FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep PUBLICID FAM_ID SRVY_YR INTV_QRT HHX FMX FPX WTFA_SA STRAT_P PSU_P ALC1YR ALCLIFE ALC12MYR ALCAMT 
		ALC5UPYR SMKSTAT2 BMI HYPEV HYPDIFV DIBEV DOINGLWA WHYNOWKA VIGFREQW VIGMIN MODFREQW MODMIN
		SAD NERVOUS RESTLESS HOPELESS EFFORT WORTHLS;
run;

data personsx2009; set lib2009.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX));  						 
	keep PUBLICID SEX AGE_P R_MARITL HISCODI3 MRACBPI2 EDUC1 PHSTAT GEOBRTH;  
run;

data familyxx2009; set lib2009.familyxx;  
	length FAM_ID $14; FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep FAM_ID RAT_CAT3;
run;

		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis2009_pre1 as
				select L.*, R.*
				from samadult2009 as L
				LEFT JOIN personsx2009 as R
				on L.PUBLICID = R.PUBLICID;
			quit;
		Proc sql;
				create table nhis2009 as
				select L.*, R.*
				from nhis2009_pre1 as L
				LEFT JOIN familyxx2009 as R
				on L.FAM_ID = R.FAM_ID;
			quit;


/************ NHIS 2010 ***************/

data samadult2010; set lib2010.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX)); 				 
	length FAM_ID $14;     FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep PUBLICID FAM_ID SRVY_YR INTV_QRT HHX FMX FPX WTFA_SA STRAT_P PSU_P ALC1YR ALCLIFE ALC12MYR ALCAMT 
		ALC5UPYR SMKSTAT2 BMI HYPEV HYPDIFV DIBEV DOINGLWA WHYNOWKA VIGFREQW VIGMIN MODFREQW MODMIN
		SAD NERVOUS RESTLESS HOPELESS EFFORT WORTHLS;
run;

data personsx2010; set lib2010.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX));  						 
	keep PUBLICID SEX AGE_P R_MARITL HISCODI3 MRACBPI2 EDUC1 PHSTAT GEOBRTH;  
run;

data familyxx2010; set lib2010.familyxx;  
	length FAM_ID $14; FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep FAM_ID RAT_CAT3;
run;


		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis2010_pre1 as
				select L.*, R.*
				from samadult2010 as L
				LEFT JOIN personsx2010 as R
				on L.PUBLICID = R.PUBLICID;
			quit;
		Proc sql;
				create table nhis2010 as
				select L.*, R.*
				from nhis2010_pre1 as L
				LEFT JOIN familyxx2010 as R
				on L.FAM_ID = R.FAM_ID;
			quit;




/************ NHIS 2011 ***************/

data samadult2011; set lib2011.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX)); 				 
	length FAM_ID $14;     FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep PUBLICID FAM_ID SRVY_YR INTV_QRT HHX FMX FPX WTFA_SA STRAT_P PSU_P ALC1YR ALCLIFE ALC12MYR ALCAMT 
		ALC5UPYR SMKSTAT2 BMI HYPEV HYPDIFV DIBEV DOINGLWA WHYNOWKA VIGFREQW VIGMIN MODFREQW MODMIN
		SAD NERVOUS RESTLESS HOPELESS EFFORT WORTHLS;
run;

data personsx2011; set lib2011.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX));						 
	keep PUBLICID SEX AGE_P R_MARITL HISCODI3 MRACBPI2 EDUC1 PHSTAT GEOBRTH;  
run;

data familyxx2011; set lib2011.familyxx;  
	length FAM_ID $14; FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep FAM_ID RAT_CAT3;
run;

		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis2011_pre1 as
				select L.*, R.*
				from samadult2011 as L
				LEFT JOIN personsx2011 as R
				on L.PUBLICID = R.PUBLICID;
			quit ;

		Proc sql;
				create table nhis2011 as
				select L.*, R.*
				from nhis2011_pre1 as L
				LEFT JOIN familyxx2011 as R
				on L.FAM_ID = R.FAM_ID;
			quit;



/************ NHIS 2012 ***************/

data samadult2012; set lib2012.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX)); 				 
	length FAM_ID $14;     FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep PUBLICID FAM_ID SRVY_YR INTV_QRT HHX FMX FPX WTFA_SA STRAT_P PSU_P ALC1YR ALCLIFE ALC12MYR ALCAMT 
		ALC5UPYR SMKSTAT2 BMI HYPEV HYPDIFV DIBEV DOINGLWA WHYNOWKA VIGFREQW VIGMIN MODFREQW MODMIN
		SAD NERVOUS RESTLESS HOPELESS EFFORT WORTHLS ADEPRSYR ADEPRSEV;
	run;

data personsx2012; set lib2012.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX));  						 
	keep PUBLICID SEX AGE_P R_MARITL HISCODI3 MRACBPI2 EDUC1 PHSTAT GEOBRTH;  
run;

data familyxx2012; set lib2012.familyxx;  
	length FAM_ID $14; FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep FAM_ID RAT_CAT3;
run;

		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis2012_pre1 as
				select L.*, R.*
				from samadult2012 as L
				LEFT JOIN personsx2012 as R
				on L.PUBLICID = R.PUBLICID;
			quit;

		Proc sql;
				create table nhis2012 as
				select L.*, R.*
				from nhis2012_pre1 as L
				LEFT JOIN familyxx2012 as R
				on L.FAM_ID = R.FAM_ID;
			quit;




/************ NHIS 2013 ***************/

data samadult2013; set lib2013.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX)); 							 
	length FAM_ID $14;     FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep PUBLICID FAM_ID SRVY_YR INTV_QRT HHX FMX FPX WTFA_SA STRAT_P PSU_P ALC1YR ALCLIFE ALC12MYR ALCAMT 
		ALC5UPYR SMKSTAT2 BMI HYPEV HYPDIFV DIBEV DOINGLWA WHYNOWKA VIGFREQW VIGMIN MODFREQW MODMIN
		ASISAD ASINERV ASIRSTLS ASIHOPLS ASIEFFRT ASIWTHLS ; 
run;

data personsx2013; set lib2013.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX));  						 
	keep PUBLICID SEX AGE_P R_MARITL HISCODI3 MRACBPI2 EDUC1 PHSTAT GEOBRTH;		  
run;
	
data familyxx2013; set lib2013.familyxx;  
	length FAM_ID $14; FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep FAM_ID RAT_CAT3;
run;

		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
			create table nhis2013_pre1 as
			select L.*, R.*
			from samadult2013 as L
			LEFT JOIN personsx2013 as R
			on L.PUBLICID = R.PUBLICID;
		quit;

		Proc sql;
				create table nhis2013 as
				select L.*, R.*
				from nhis2013_pre1 as L
				LEFT JOIN familyxx2013 as R
				on L.FAM_ID = R.FAM_ID;
			quit;



/************ NHIS 2014 ***************/

data samadult2014; set lib2014.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX)); 							 
	length FAM_ID $14;     FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep PUBLICID FAM_ID SRVY_YR INTV_QRT HHX FMX FPX WTFA_SA STRAT_P PSU_P ALC1YR ALCLIFE ALC12MYR ALCAMT 
		ALC5UPY1 SMKSTAT2 BMI HYPEV HYPDIFV DIBEV DOINGLWA WHYNOWKA VIGFREQW VIGMIN MODFREQW MODMIN
		ASISAD ASINERV ASIRSTLS ASIHOPLS ASIEFFRT ASIWTHLS ;
run;

data personsx2014; set lib2014.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX)); 						 
	keep PUBLICID SEX AGE_P R_MARITL HISCODI3 MRACBPI2 EDUC1 PHSTAT GEOBRTH; 		 
run;

data familyxx2014; set lib2014.familyxx;  
	length FAM_ID $14; FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep FAM_ID RAT_CAT5;
run;

		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis2014_pre1 as
				select L.*, R.*
				from samadult2014 as L
				LEFT JOIN personsx2014 as R
				on L.PUBLICID = R.PUBLICID;
			quit;

		Proc sql;
				create table nhis2014 as
				select L.*, R.*
				from nhis2014_pre1 as L
				LEFT JOIN familyxx2014 as R
				on L.FAM_ID = R.FAM_ID;
			quit;







/************ NHIS 2015 ***************/

data samadult2015; set lib2015.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX)); 							 
	length FAM_ID $14;     FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep PUBLICID FAM_ID SRVY_YR INTV_QRT HHX FMX FPX WTFA_SA STRAT_P PSU_P ALC1YR ALCLIFE ALC12MYR ALCAMT 
		ALC5UPY1 SMKSTAT2 BMI HYPEV HYPDIFV DIBEV DOINGLWA WHYNOWKA VIGFREQW VIGMIN MODFREQW MODMIN
		ASISAD ASINERV ASIRSTLS ASIHOPLS ASIEFFRT ASIWTHLS ;
run;

data personsx2015; set lib2015.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX)); 						 
	keep PUBLICID SEX AGE_P R_MARITL HISCODI3 MRACBPI2 EDUC1 PHSTAT GEOBRTH; 		 
run;

data familyxx2015; set lib2015.familyxx;  
	length FAM_ID $14; FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep FAM_ID RAT_CAT5;
run;

		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis2015_pre1 as
				select L.*, R.*
				from samadult2015 as L
				LEFT JOIN personsx2015 as R
				on L.PUBLICID = R.PUBLICID;
			quit;

		Proc sql;
				create table nhis2015 as
				select L.*, R.*
				from nhis2015_pre1 as L
				LEFT JOIN familyxx2015 as R
				on L.FAM_ID = R.FAM_ID;
			quit;




			

/************ NHIS 2016 ***************/

data samadult2016; set lib2016.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX)); 							 
	length FAM_ID $14;     FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep PUBLICID FAM_ID SRVY_YR INTV_QRT HHX FMX FPX WTFA_SA PSTRAT PPSU ALC1YR ALCLIFE ALC12MYR ALCAMT 
		ALC5UPY1 SMKSTAT2 BMI HYPEV HYPDIFV DIBEV1 DOINGLWA WHYNOWKA VIGFREQW VIGMIN MODFREQW MODMIN
		ASISAD ASINERV ASIRSTLS ASIHOPLS ASIEFFRT ASIWTHLS ;
run;

data personsx2016; set lib2016.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX)); 						 
	keep PUBLICID SEX AGE_P R_MARITL HISCODI3 MRACBPI2 EDUC1 PHSTAT GEOBRTH; 		 
run;

data familyxx2016; set lib2016.familyxx;  
	length FAM_ID $14; FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep FAM_ID RAT_CAT5;
run;

		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis2016_pre1 as
				select L.*, R.*
				from samadult2016 as L
				LEFT JOIN personsx2016 as R
				on L.PUBLICID = R.PUBLICID;
			quit;

		Proc sql;
				create table nhis2016 as
				select L.*, R.*
				from nhis2016_pre1 as L
				LEFT JOIN familyxx2016 as R
				on L.FAM_ID = R.FAM_ID;
			quit;





			

/************ NHIS 2017 ***************/

data samadult2017; set lib2017.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX)); 							 
	length FAM_ID $14;     FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep PUBLICID FAM_ID SRVY_YR INTV_QRT HHX FMX FPX WTFA_SA PSTRAT PPSU ALC1YR ALCLIFE ALC12MYR ALCAMT 
		ALC5UPY1 SMKSTAT2 BMI HYPEV HYPDIFV DIBEV1 DOINGLWA WHYNOWKA VIGFREQW VIGMIN MODFREQW MODMIN
		ASISAD ASINERV ASIRSTLS ASIHOPLS ASIEFFRT ASIWTHLS ;
run;

data personsx2017; set lib2017.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX)); 						 
	keep PUBLICID SEX AGE_P R_MARITL HISCODI3 MRACBPI2 EDUC1 PHSTAT GEOBRTH; 		 
run;

data familyxx2017; set lib2017.familyxx;  
	length FAM_ID $14; FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep FAM_ID RAT_CAT5;
run;

		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis2017_pre1 as
				select L.*, R.*
				from samadult2017 as L
				LEFT JOIN personsx2017 as R
				on L.PUBLICID = R.PUBLICID;
			quit;

		Proc sql;
				create table nhis2017 as
				select L.*, R.*
				from nhis2017_pre1 as L
				LEFT JOIN familyxx2017 as R
				on L.FAM_ID = R.FAM_ID;
			quit;





		

/************ NHIS 2018 ***************/

data samadult2018; set lib2018.samadult;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX)); 							 
	length FAM_ID $14;     FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep PUBLICID FAM_ID SRVY_YR INTV_QRT HHX FMX FPX WTFA_SA PSTRAT PPSU ALC1YR ALCLIFE ALC12MYR ALCAMT 
		ALC5UPY1 SMKSTAT2 BMI HYPEV HYPDIFV DIBEV1 DOINGLWA WHYNOWKA VIGFREQW VIGMIN MODFREQW MODMIN
		ASISAD ASINERV ASIRSTLS ASIHOPLS ASIEFFRT ASIWTHLS;
run;

data personsx2018; set lib2018.personsx;
	length PUBLICID $14; PUBLICID = trim(left(SRVY_YR||HHX||FMX||FPX)); 						 
	keep PUBLICID SEX AGE_P R_MARITL HISCODI3 MRACBPI2 EDUC1 PHSTAT GEOBRTH; 		 
run;

data familyxx2018; set lib2018.familyxx;  
	length FAM_ID $14; FAM_ID = trim(left(SRVY_YR||HHX||FMX));   
	keep FAM_ID RAT_CAT5;
run;


		/* Merge NHIS Data, keep those with data on the sample adult file */
		Proc sql;
				create table nhis2018_pre1 as
				select L.*, R.*
				from samadult2018 as L
				LEFT JOIN personsx2018 as R
				on L.PUBLICID = R.PUBLICID;
			quit;

		Proc sql;
				create table nhis2018 as
				select L.*, R.*
				from nhis2018_pre1 as L
				LEFT JOIN familyxx2018 as R
				on L.FAM_ID = R.FAM_ID;
			quit;

options notes;  /*Stop suppressing the notes in the log file */



/**************************************************************************************************************/
/********************************** Combine and save all NHIS data *********************************************/
/**************************************************************************************************************/
data combined.RDCp2058dataset; 
	set nhis1997 nhis1998 nhis1999 nhis2000 nhis2001 nhis2002 nhis2003 nhis2004 nhis2005 nhis2006 nhis2007 
		nhis2008 nhis2009 nhis2010 nhis2011 nhis2012 nhis2013 nhis2014 nhis2015 nhis2016 nhis2017 nhis2018;
	DROP AGE_P; /* remove the 'AGE_P' variable; this was included up until now so that it can be used in the temporary file described below */
run;
/* Note: Final file is called RDCp2058dataset and is stored in the location 'combined' (defined at the start of the file) */


/* View summary of data file*/
proc contents data=combined.RDCp2058dataset; run;



/*Create a TEMPORARY VERSION to test the syntax (includes age)*/
data combined.RDCp2058dataset_temp; 
	set nhis1997 nhis1998 nhis1999 nhis2000 nhis2001 nhis2002 nhis2003 nhis2004 nhis2005 nhis2006 nhis2007 
		nhis2008 nhis2009 nhis2010 nhis2011 nhis2012 nhis2013 nhis2014 nhis2015 nhis2016 nhis2017 nhis2018;
run;

proc contents data=combined.RDCp2058dataset_temp; run;


