
# ********************************************************************************************************************
# ********************************** R SYNTAX TO EXTRACT AND COMBINE NHIS 1997-2018 DATA *****************************
# ********************************************************************************************************************

# Load libraries  
library(tidyverse)
library(haven)

# Location of data files 
locationRAW   <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nhis/Original data/NHIS Raw Data/"
locationMORT  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nhis/Original data/Mortality Data/1997-2014/"
locationFINAL <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nhis/Processed data/Restricted Use Data/"



# **********************************************************************************************************************
# **************************** PART 1: EXTRACT ALL OF THE DATA FROM PUBLIC USE DATA FILES ******************************
# **********************************************************************************************************************

# Note: The mutate() function creates the unique identifer
# Note: The select() function selects the variables to keep


# NHIS 1997 ************************************************************************************************************

# Sample Adult File
samadult1997 <- read_sas(paste0(locationRAW, "1997/samadult.sas7bdat")) %>% 
  mutate(PUBLICID=paste0(SRVY_YR,HHX,FMX,PX)) %>%
  select(PUBLICID, SRVY_YR, INTV_QRT, HHX, FMX, PX, WTFA_SA, STRATUM, PSU, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, ALC5UPYR,	
         SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV, VIGFREQW, VIGMIN, MODFREQW, MODMIN, SAD, NERVOUS, RESTLESS, HOPELESS, 
         EFFORT, WORTHLS)

# Person File
personsx1997 <- read_sas(paste0(locationRAW, "1997/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, PX)) %>%
  select(PUBLICID, SEX, AGE_P, R_MARITL, DOINGLW, WHYNOWRK, ORIGIN, RACEREC, MRACE_P, EDUC, RAT_CAT, PHSTAT, USBORN_P)

#  Merge NHIS Data, keep those with data on the sample adult file 
nhis1997 <- left_join(samadult1997, personsx1997, by="PUBLICID")



# NHIS 1998 ************************************************************************************************************
samadult1998 <- read_sas(paste0(locationRAW, "1998/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, PX)) %>%				
	select (PUBLICID, SRVY_YR, INTV_QRT, HHX, FMX, PX, WTFA_SA, STRATUM, PSU, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, ALC5UPYR,	
		SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV, VIGFREQW, VIGMIN, MODFREQW, MODMIN, 
		SAD, NERVOUS, RESTLESS, HOPELESS, EFFORT, WORTHLS)

personsx1998 <- read_sas(paste0(locationRAW, "1998/personsx.sas7bdat")) %>% 
  mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, PX)) %>%						
	select (PUBLICID, SEX, AGE_P, R_MARITL, DOINGLW, WHYNOWRK, HISPCODE, MRACE_P, EDUC, RAT_CAT, PHSTAT, USBORN_P)

nhis1998 <- left_join(samadult1998, personsx1998, by="PUBLICID")





# NHIS 1999 ************************************************************************************************************
samadult1999 <- read_sas(paste0(locationRAW, "1999/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, PX)) %>%  				 
	select (PUBLICID, SRVY_YR, INTV_QRT, HHX, FMX, PX, WTFA_SA, STRATUM, PSU, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, ALC5UPYR, 	
      		SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV, VIGFREQW, VIGMIN, MODFREQW, MODMIN, 
      		SAD, NERVOUS, RESTLESS, HOPELESS, EFFORT, WORTHLS, MHDINTWY, MHDSAD2W, MHDSADWY)
	
personsx1999 <- read_sas(paste0(locationRAW, "1999/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, PX)) %>%						
	select (PUBLICID, SEX, AGE_P, R_MARITL, DOINGLW, WHYNOWRK, HISPCODR, MRACBR_P, EDUC, RAT_CAT, PHSTAT, USBORN_P)

nhis1999 <- left_join(samadult1999, personsx1999, by="PUBLICID")



# NHIS 2000 ************************************************************************************************************
samadult2000 <- read_sas(paste0(locationRAW, "2000/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, PX)) %>%  				 
	select (PUBLICID, SRVY_YR, INTV_QRT, HHX, FMX, PX, WTFA_SA, STRATUM, PSU, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, ALC5UPYR,
		SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV, VIGFREQW, VIGMIN, MODFREQW, MODMIN, 
		SAD, NERVOUS, RESTLESS, HOPELESS, EFFORT, WORTHLS)

personsx2000 <- read_sas(paste0(locationRAW, "2000/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, PX)) %>%  							
	select (PUBLICID, SEX, AGE_P, R_MARITL, DOINGLW, WHYNOWRK, HISCOD_I, MRACBP_I, EDUC, RAT_CAT, PHSTAT, USBRTH_P)

nhis2000 <- left_join(samadult2000, personsx2000, by="PUBLICID")



# NHIS 2001 ************************************************************************************************************
samadult2001 <- read_sas(paste0(locationRAW, "2001/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, PX)) %>%  				 
	select (PUBLICID, SRVY_YR, INTV_QRT, HHX, FMX, PX, WTFA_SA, STRATUM, PSU, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, ALC5UPYR,	
		SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV, VIGFREQW, VIGMIN, MODFREQW, MODMIN, 
		SAD, NERVOUS, RESTLESS, HOPELESS, EFFORT, WORTHLS)

personsx2001 <- read_sas(paste0(locationRAW, "2001/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, PX)) %>%  							
	select (PUBLICID, SEX, AGE_P, R_MARITL, DOINGLW1, WHYNOWK1, HISCOD_I, MRACBP_I, EDUC, RAT_CAT, PHSTAT, USBRTH_P)

nhis2001 <- left_join(samadult2001, personsx2001, by="PUBLICID")



# NHIS 2002 ************************************************************************************************************
samadult2002 <- read_sas(paste0(locationRAW, "2002/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, PX)) %>%  				 
	select (PUBLICID, SRVY_YR, INTV_QRT, HHX, FMX, PX, WTFA_SA, STRATUM, PSU, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, ALC5UPYR, 
		SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV, VIGFREQW, VIGMIN, MODFREQW, MODMIN, 
		SAD, NERVOUS, RESTLESS, HOPELESS, EFFORT, WORTHLS)

personsx2002 <- read_sas(paste0(locationRAW, "2002/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, PX)) %>%  							
	select (PUBLICID, SEX, AGE_P, R_MARITL, DOINGLW1, WHYNOWK1, HISCOD_I, MRACBP_I, EDUC, RAT_CAT, PHSTAT, GEOBRTH)

nhis2002 <- left_join(samadult2002, personsx2002, by="PUBLICID")



# NHIS 2003 ************************************************************************************************************
samadult2003 <- read_sas(paste0(locationRAW, "2003/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, PX)) %>%  				 
	select (PUBLICID, SRVY_YR, INTV_QRT, HHX, FMX, PX, WTFA_SA, STRATUM, PSU, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, ALC5UPYR, 
		SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV, VIGFREQW, VIGMIN, MODFREQW, MODMIN, 
		SAD, NERVOUS, RESTLESS, HOPELESS, EFFORT, WORTHLS)
		
personsx2003 <- read_sas(paste0(locationRAW, "2003/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, PX)) %>%  							
	select (PUBLICID, SEX, AGE_P, R_MARITL, DOINGLW1, WHYNOWK1, HISCODI2, MRACBPI2, EDUC, RAT_CAT, PHSTAT, GEOBRTH)

nhis2003 <- left_join(samadult2003, personsx2003, by="PUBLICID")



# NHIS 2004 ************************************************************************************************************
# Sample Adult File
samadult2004 <- read_sas(paste0(locationRAW, "2004/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>%   
	mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (PUBLICID, FAM_ID, SRVY_YR, HHX, FMX, FPX, WTFA_SA, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, ALC5UPYR, 
		SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV, DOINGLWA, WHYNOWKA, VIGFREQW, VIGMIN, MODFREQW, MODMIN, SAD, NERVOUS, 
	  RESTLESS, HOPELESS, EFFORT, WORTHLS)

# Person File
personsx2004 <- read_sas(paste0(locationRAW, "2004/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>%  						 
	select (PUBLICID, SEX, AGE_P, R_MARITL, HISCODI2, MRACBPI2, EDUC1, STRATUM, PSU, PHSTAT, GEOBRTH)   # in 2004 the PSU and STRATUM variables were only available on the person file 

# Household File
househld2004 <- read_sas(paste0(locationRAW, "2004/househld.sas7bdat")) %>%    # in 2004 the Quater that the survey was completed in was only available on the household file 
	select (HHX, INTV_QRT)

# Family File
familyxx2004 <- read_sas(paste0(locationRAW, "2004/familyxx.sas7bdat")) %>%   
	mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (FAM_ID, RAT_CAT)


nhis2004 <- left_join(samadult2004, personsx2004, by="PUBLICID") %>%
            left_join(househld2004, by="HHX") %>%
            left_join(familyxx2004, by="FAM_ID")



# NHIS 2005 ************************************************************************************************************
samadult2005 <- read_sas(paste0(locationRAW, "2005/samadult.sas7bdat")) %>% 
	mutate(PUBLICID = paste0(SRVY_YR, HHX, FMX, FPX),
         FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (PUBLICID, FAM_ID, SRVY_YR, INTV_QRT, HHX, FMX, FPX, WTFA_SA, STRATUM, PSU, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, 
		ALC5UPYR, SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV, DOINGLWA, WHYNOWKA, VIGFREQW, VIGMIN, MODFREQW, MODMIN,
		SAD, NERVOUS, RESTLESS, HOPELESS, EFFORT, WORTHLS)

personsx2005 <- read_sas(paste0(locationRAW, "2005/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>%  						 
	select (PUBLICID, SEX, AGE_P, R_MARITL, HISCODI2, MRACBPI2, EDUC1, PHSTAT, GEOBRTH)

familyxx2005 <- read_sas(paste0(locationRAW, "2005/familyxx.sas7bdat")) %>%   
	mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (FAM_ID, RAT_CAT)

nhis2005 <- left_join(samadult2005, personsx2005, by="PUBLICID") %>%
  left_join(familyxx2005, by="FAM_ID") 


# NHIS 2006 ************************************************************************************************************
samadult2006 <- read_sas(paste0(locationRAW, "2006/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX), 				 
         FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (PUBLICID, FAM_ID, SRVY_YR, INTV_QRT, HHX, FMX, FPX, WTFA_SA, STRAT_P, PSU_P, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, 
		ALC5UPYR, SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV, DOINGLWA, WHYNOWKA, VIGFREQW, VIGMIN, MODFREQW, MODMIN,
		SAD, NERVOUS, RESTLESS, HOPELESS, EFFORT, WORTHLS)

personsx2006 <- read_sas(paste0(locationRAW, "2006/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>%  						 
	select (PUBLICID, SEX, AGE_P, R_MARITL, HISCODI3, MRACBPI2, EDUC1, PHSTAT, GEOBRTH)

familyxx2006 <- read_sas(paste0(locationRAW, "2006/familyxx.sas7bdat")) %>%   
	mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (FAM_ID, RAT_CAT)

nhis2006 <- left_join(samadult2006, personsx2006, by="PUBLICID") %>%
  left_join(familyxx2006, by="FAM_ID") 



# NHIS 2007 ************************************************************************************************************
samadult2007 <- read_sas(paste0(locationRAW, "2007/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX),
	      FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (PUBLICID, FAM_ID, SRVY_YR, INTV_QRT, HHX, FMX, FPX, WTFA_SA, STRAT_P, PSU_P, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, 
		ALC5UPYR, SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV, DOINGLWA, WHYNOWKA, VIGFREQW, VIGMIN, MODFREQW, MODMIN, SAD, 
		NERVOUS, RESTLESS, HOPELESS, EFFORT, WORTHLS, DEPYR)

personsx2007 <- read_sas(paste0(locationRAW, "2007/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>%  						 
	select (PUBLICID, SEX, AGE_P, R_MARITL, HISCODI3, MRACBPI2, EDUC1, PHSTAT, GEOBRTH)

familyxx2007 <- read_sas(paste0(locationRAW, "2007/familyxx.sas7bdat")) %>% 
	mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (FAM_ID, RAT_CAT3)

nhis2007 <- left_join(samadult2007, personsx2007, by="PUBLICID") %>%
  left_join(familyxx2007, by="FAM_ID") 



# NHIS 2008 ************************************************************************************************************
samadult2008 <- read_sas(paste0(locationRAW, "2008/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX),
	  FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (PUBLICID, FAM_ID, SRVY_YR, INTV_QRT, HHX, FMX, FPX, WTFA_SA, STRAT_P, PSU_P, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, 
		ALC5UPYR, SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV, DOINGLWA, WHYNOWKA, VIGFREQW, VIGMIN, MODFREQW, MODMIN,
		SAD, NERVOUS, RESTLESS, HOPELESS, EFFORT, WORTHLS, DEPRESS)

personsx2008 <- read_sas(paste0(locationRAW, "2008/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>%  						 
	select (PUBLICID, SEX, AGE_P, R_MARITL, HISCODI3, MRACBPI2, EDUC1, PHSTAT, GEOBRTH)

familyxx2008 <- read_sas(paste0(locationRAW, "2008/familyxx.sas7bdat")) %>% 
	mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (FAM_ID, RAT_CAT3)

nhis2008 <- left_join(samadult2008, personsx2008, by="PUBLICID") %>%
  left_join(familyxx2008, by="FAM_ID") 



# NHIS 2009 ************************************************************************************************************
samadult2009 <- read_sas(paste0(locationRAW, "2009/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX),
	      FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (PUBLICID, FAM_ID, SRVY_YR, INTV_QRT, HHX, FMX, FPX, WTFA_SA, STRAT_P, PSU_P, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, 
		ALC5UPYR, SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV, DOINGLWA, WHYNOWKA, VIGFREQW, VIGMIN, MODFREQW, MODMIN,
		SAD, NERVOUS, RESTLESS, HOPELESS, EFFORT, WORTHLS)

personsx2009 <- read_sas(paste0(locationRAW, "2009/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>%  						 
	select (PUBLICID, SEX, AGE_P, R_MARITL, HISCODI3, MRACBPI2, EDUC1, PHSTAT, GEOBRTH)

familyxx2009 <- read_sas(paste0(locationRAW, "2009/familyxx.sas7bdat")) %>% 
	mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (FAM_ID, RAT_CAT3)

nhis2009 <- left_join(samadult2009, personsx2009, by="PUBLICID") %>%
  left_join(familyxx2009, by="FAM_ID") 


# NHIS 2010 ************************************************************************************************************
samadult2010 <- read_sas(paste0(locationRAW, "2010/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>% 				 
mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (PUBLICID, FAM_ID, SRVY_YR, INTV_QRT, HHX, FMX, FPX, WTFA_SA, STRAT_P, PSU_P, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, 
		ALC5UPYR, SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV, DOINGLWA, WHYNOWKA, VIGFREQW, VIGMIN, MODFREQW, MODMIN,
		SAD, NERVOUS, RESTLESS, HOPELESS, EFFORT, WORTHLS)

personsx2010 <- read_sas(paste0(locationRAW, "2010/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>%  						 
	select (PUBLICID, SEX, AGE_P, R_MARITL, HISCODI3, MRACBPI2, EDUC1, PHSTAT, GEOBRTH)

familyxx2010 <- read_sas(paste0(locationRAW, "2010/familyxx.sas7bdat")) %>%  
	mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (FAM_ID, RAT_CAT3)


nhis2010 <- left_join(samadult2010, personsx2010, by="PUBLICID") %>%
  left_join(familyxx2010, by="FAM_ID") 



# NHIS 2011 ************************************************************************************************************
samadult2011 <- read_sas(paste0(locationRAW, "2011/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>% 				 
mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (PUBLICID, FAM_ID, SRVY_YR, INTV_QRT, HHX, FMX, FPX, WTFA_SA, STRAT_P, PSU_P, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, 
		ALC5UPYR, SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV, DOINGLWA, WHYNOWKA, VIGFREQW, VIGMIN, MODFREQW, MODMIN,
		SAD, NERVOUS, RESTLESS, HOPELESS, EFFORT, WORTHLS)

personsx2011 <- read_sas(paste0(locationRAW, "2011/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>%						 
	select (PUBLICID, SEX, AGE_P, R_MARITL, HISCODI3, MRACBPI2, EDUC1, PHSTAT, GEOBRTH)

familyxx2011 <- read_sas(paste0(locationRAW, "2011/familyxx.sas7bdat")) %>% 
	mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (FAM_ID, RAT_CAT3)

nhis2011 <- left_join(samadult2011, personsx2011, by="PUBLICID") %>%
  left_join(familyxx2011, by="FAM_ID") 



# NHIS 2012 ************************************************************************************************************
samadult2012 <- read_sas(paste0(locationRAW, "2012/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>% 				 
mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (PUBLICID, FAM_ID, SRVY_YR, INTV_QRT, HHX, FMX, FPX, WTFA_SA, STRAT_P, PSU_P, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, 
		ALC5UPYR, SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV, DOINGLWA, WHYNOWKA, VIGFREQW, VIGMIN, MODFREQW, MODMIN,
		SAD, NERVOUS, RESTLESS, HOPELESS, EFFORT, WORTHLS, ADEPRSYR, ADEPRSEV)

personsx2012 <- read_sas(paste0(locationRAW, "2012/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>%  						 
	select (PUBLICID, SEX, AGE_P, R_MARITL, HISCODI3, MRACBPI2, EDUC1, PHSTAT, GEOBRTH)

familyxx2012 <- read_sas(paste0(locationRAW, "2012/familyxx.sas7bdat")) %>%   
	mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (FAM_ID, RAT_CAT3)

nhis2012 <- left_join(samadult2012, personsx2012, by="PUBLICID") %>%
  left_join(familyxx2012, by="FAM_ID") 



# NHIS 2013 ************************************************************************************************************
samadult2013 <- read_sas(paste0(locationRAW, "2013/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>% 							 
mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (PUBLICID, FAM_ID, SRVY_YR, INTV_QRT, HHX, FMX, FPX, WTFA_SA, STRAT_P, PSU_P, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, 
		ALC5UPYR, SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV, DOINGLWA, WHYNOWKA, VIGFREQW, VIGMIN, MODFREQW, MODMIN,
		ASISAD, ASINERV, ASIRSTLS, ASIHOPLS, ASIEFFRT, ASIWTHLS)


personsx2013 <- read_sas(paste0(locationRAW, "2013/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>%  						 
	select (PUBLICID, SEX, AGE_P, R_MARITL, HISCODI3, MRACBPI2, EDUC1, PHSTAT, GEOBRTH)

	
familyxx2013 <- read_sas(paste0(locationRAW, "2013/familyxx.sas7bdat")) %>%   
	mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (FAM_ID, RAT_CAT3)


nhis2013 <- left_join(samadult2013, personsx2013, by="PUBLICID") %>%
  left_join(familyxx2013, by="FAM_ID") 
	  


# NHIS 2014 ************************************************************************************************************
samadult2014 <- read_sas(paste0(locationRAW, "2014/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX),
	        FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (PUBLICID, FAM_ID, SRVY_YR, INTV_QRT, HHX, FMX, FPX, WTFA_SA, STRAT_P, PSU_P, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, 
		ALC5UPY1, SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV, DOINGLWA, WHYNOWKA, VIGFREQW, VIGMIN, MODFREQW, MODMIN,
		ASISAD, ASINERV, ASIRSTLS, ASIHOPLS, ASIEFFRT, ASIWTHLS)


personsx2014 <- read_sas(paste0(locationRAW, "2014/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>% 						 
	select (PUBLICID, SEX, AGE_P, R_MARITL, HISCODI3, MRACBPI2, EDUC1, PHSTAT, GEOBRTH)

familyxx2014 <- read_sas(paste0(locationRAW, "2014/familyxx.sas7bdat")) %>%   
	mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (FAM_ID, RAT_CAT5)

nhis2014 <- left_join(samadult2014, personsx2014, by="PUBLICID") %>%
  left_join(familyxx2014, by="FAM_ID") 
	  




# NHIS 2015 ************************************************************************************************************
samadult2015 <- read_sas(paste0(locationRAW, "2015/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX),
	        FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (PUBLICID, FAM_ID, SRVY_YR, INTV_QRT, HHX, FMX, FPX, WTFA_SA, STRAT_P, PSU_P, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, 
		ALC5UPY1, SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV, DOINGLWA, WHYNOWKA, VIGFREQW, VIGMIN, MODFREQW, MODMIN,
		ASISAD, ASINERV, ASIRSTLS, ASIHOPLS, ASIEFFRT, ASIWTHLS)


personsx2015 <- read_sas(paste0(locationRAW, "2015/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>% 						 
	select (PUBLICID, SEX, AGE_P, R_MARITL, HISCODI3, MRACBPI2, EDUC1, PHSTAT, GEOBRTH) 


familyxx2015 <- read_sas(paste0(locationRAW, "2015/familyxx.sas7bdat")) %>%   
	mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (FAM_ID, RAT_CAT5)


nhis2015 <- left_join(samadult2015, personsx2015, by="PUBLICID") %>%
  left_join(familyxx2015, by="FAM_ID") 
	  

			

# NHIS 2016 ************************************************************************************************************
samadult2016 <- read_sas(paste0(locationRAW, "2016/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX),
	        FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (PUBLICID, FAM_ID, SRVY_YR, INTV_QRT, HHX, FMX, FPX, WTFA_SA, PSTRAT, PPSU, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, 
		ALC5UPY1, SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV1, DOINGLWA, WHYNOWKA, VIGFREQW, VIGMIN, MODFREQW, MODMIN,
		ASISAD, ASINERV, ASIRSTLS, ASIHOPLS, ASIEFFRT, ASIWTHLS)

personsx2016 <- read_sas(paste0(locationRAW, "2016/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>% 						 
	select (PUBLICID, SEX, AGE_P, R_MARITL, HISCODI3, MRACBPI2, EDUC1, PHSTAT, GEOBRTH) 

familyxx2016 <- read_sas(paste0(locationRAW, "2016/familyxx.sas7bdat")) %>% 
	mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (FAM_ID, RAT_CAT5)

nhis2016 <- left_join(samadult2016, personsx2016, by="PUBLICID") %>%
  left_join(familyxx2016, by="FAM_ID") 
	  


# NHIS 2017 ************************************************************************************************************
samadult2017 <- read_sas(paste0(locationRAW, "2017/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX),
	        FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (PUBLICID, FAM_ID, SRVY_YR, INTV_QRT, HHX, FMX, FPX, WTFA_SA, PSTRAT, PPSU, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, 
		ALC5UPY1, SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV1, DOINGLWA, WHYNOWKA, VIGFREQW, VIGMIN, MODFREQW, MODMIN,
		ASISAD, ASINERV, ASIRSTLS, ASIHOPLS, ASIEFFRT, ASIWTHLS)

personsx2017 <- read_sas(paste0(locationRAW, "2017/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>% 						 
	select (PUBLICID, SEX, AGE_P, R_MARITL, HISCODI3, MRACBPI2, EDUC1, PHSTAT, GEOBRTH) 

familyxx2017 <- read_sas(paste0(locationRAW, "2017/familyxx.sas7bdat")) %>%  
	mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%   
	select (FAM_ID, RAT_CAT5)

nhis2017 <- left_join(samadult2017, personsx2017, by="PUBLICID") %>%
  left_join(familyxx2017, by="FAM_ID") 
	  


# NHIS 2018 ************************************************************************************************************
samadult2018 <- read_sas(paste0(locationRAW, "2018/samadult.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX),
	       FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%
	select (PUBLICID, FAM_ID, SRVY_YR, INTV_QRT, HHX, FMX, FPX, WTFA_SA, PSTRAT, PPSU, ALC1YR, ALCLIFE, ALC12MYR, ALCAMT, 
		ALC5UPY1, SMKSTAT2, BMI, HYPEV, HYPDIFV, DIBEV1, DOINGLWA, WHYNOWKA, VIGFREQW, VIGMIN, MODFREQW, MODMIN,
		ASISAD, ASINERV, ASIRSTLS, ASIHOPLS, ASIEFFRT, ASIWTHLS)

personsx2018 <- read_sas(paste0(locationRAW, "2018/personsx.sas7bdat")) %>% 
	mutate(PUBLICID=paste0(SRVY_YR, HHX, FMX, FPX)) %>% 					 
	select (PUBLICID, SEX, AGE_P, R_MARITL, HISCODI3, MRACBPI2, EDUC1, PHSTAT, GEOBRTH)

familyxx2018 <- read_sas(paste0(locationRAW, "2018/familyxx.sas7bdat")) %>%   
	mutate(FAM_ID = paste0(SRVY_YR, HHX, FMX)) %>%  
	select (FAM_ID, RAT_CAT5)

nhis2018 <- left_join(samadult2018, personsx2018, by="PUBLICID") %>%
  left_join(familyxx2018, by="FAM_ID") 
	  

	  

# *********************************************************************************************************************
# ******************************************* Combine all NHIS data ***************************************************
# *********************************************************************************************************************

combined <- bind_rows(nhis1997, nhis1998, nhis1999, nhis2000, nhis2001, nhis2002, nhis2003, nhis2004, nhis2005, nhis2006, nhis2007, 
		                  nhis2008, nhis2009, nhis2010, nhis2011, nhis2012, nhis2013, nhis2014, nhis2015, nhis2016, nhis2017, nhis2018) %>%
            select (-AGE_P)  #  remove the 'AGE_P' variable; this was included up until now so that it can be used in the temporary file described below 


#  Summary of data file
str(combined)
summary(combined)

# Save data file
# write_sas (combined, paste0(locationFINAL, "rdcp2058dataset_fromR.sas7bdat")) # The SAS data file created does not work; therefore a csv version is saved below
write.csv(combined, paste0(locationFINAL, "rdcp2058dataset.csv"), na="", row.names = FALSE)


# Create a TEMPORARY VERSION to test the syntax (includes age)
# combined_temp <- bind_rows(nhis1997, nhis1998, nhis1999, nhis2000, nhis2001, nhis2002, nhis2003, nhis2004, nhis2005, nhis2006, nhis2007, 
#   nhis2008, nhis2009, nhis2010, nhis2011, nhis2012, nhis2013, nhis2014, nhis2015, nhis2016, nhis2017, nhis2018)
# 
# str(combined_temp)
# summary(combined_temp)

