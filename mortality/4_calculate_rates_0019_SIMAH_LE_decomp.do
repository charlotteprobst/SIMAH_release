// NIH2020 project. 
// Calculate mortality rates for years 2000 to 2019

/* This do file 
- requires population counts for demographic groups (based on March CPS surveys) 
  correspoinding to those used in the aggregated mortality file (based on CDC)
- calculates crude mortality rates in each sociodemographic group and for each 
  cause of death category (has to match the mortality data)
*/

// difference to v2 is that cancer and pancreatitis were added as causes of death
// set global directory
global dir `"C:/Users/marie/Dropbox/NIH2020"'

// combine deaths with population by cell
// read in population counts
use "${dir}/SIMAH_workplace/demography/3_out CPS data/STCROSSCEPRMarchcomb.dta", clear

merge 1:1 year age_gp sex edclass race using "${dir}/SIMAH_workplace/mortality/3_out data/allethn_sumCOD_0019_LE_decomp.dta"

/* _m==1 is for groups that recorded no deaths in that year (those that were not merged but only appear in master...) */

 foreach var in T LVDC PANC DM IHD ISTR HSTR HYPHD AUD UIJ MVACC IJ CAN LRI REST {
   replace `var'mort = 0 if _m==1 
   gen `var'rate   = (`var'mort/TPop)*100000
   }

tab year if _m==2
drop _m
label var Trate "all cause"
label var LVDCrate "liver disease and cirrohosis" 
label var PANCrate "Pancreatitis" 
label var DMrate "diabbetes mellitus"
label var IHDrate "ischemic heart disease"
label var HSTRrate "Hemorrhagic Stroke"
label var HYPHDrate "hypertensive heart disease"
label var AUDrate "alcohol use disorders"
label var UIJrate "unintentional injuries"
label var MVACCrate "motorvehicle accidents"
label var IJrate  "intentional injuries"
label var LRIrate  "lower respiratory infections"
label var CANrate  "Cancer mortality"


save "${dir}/SIMAH_workplace/mortality/3_out data/allethn_rates_0019_LE_decomp.dta", replace

outsheet using "${dir}/SIMAH_workplace/mortality/3_out data/allethn_rates_0019_LE_decomp.csv" , comma replace
