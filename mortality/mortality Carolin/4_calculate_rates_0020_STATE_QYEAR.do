// NIH2020 project. 
// Calculate mortality rates for years 2000 to 2020

/* This do file 
- requires population counts for demographic groups (based on ACS data)
  correspoinding to those used in the aggregated mortality file (based on CDC)
- calculates crude mortality rates in each sociodemographic group and for each 
  cause of death category (has to match the mortality data)
*/

// set global directory
global dir `"/Users/carolinkilian/Desktop/SIMAH_workplace/"'

// import STATE csv file and SAVE as .dta
import delimited "${dir}/mortality/3_out data/allethn_sumCOD_0020_STATE.csv", clear
	
// include quarters
gen Q = 1 if month < 4
replace Q = 2 if month > 3 & month < 7
replace Q = 3 if month > 6 & month < 10
replace Q = 4 if month > 9
	
// aggregate to quarterly counts
collapse (sum) tmort=tmort acutmort=acutmort chronmort=chronmort alclivmort=alclivmort ///
othlivmort=othlivmort uijmort=uijmort sijmort=sijmort mvaccmort=mvaccmort restmort=restmort ///
, by(year Q fipsstr sex age_gp edclass) 	
	
// export as .DTA	
save "${dir}/mortality/3_out data/allethn_sumCOD_0020_STATE_QYEAR.dta", replace


// prepare population data for merging
// read in population counts
import delimited "${dir}/demography/ACS_popcounts_2000_2021_bystate_age_gp.csv", clear

// recode state
gen fipsstr = state
replace fipsstr = "AL" if fipsstr == "Alabama"
replace fipsstr = "AK" if fipsstr == "Alaska"
replace fipsstr = "AZ" if fipsstr == "Arizona"
replace fipsstr = "AR" if fipsstr == "Arkansas"
replace fipsstr = "CA" if fipsstr == "California"
replace fipsstr = "CO" if fipsstr == "Colorado"
replace fipsstr = "CT" if fipsstr == "Connecticut"
replace fipsstr = "DE" if fipsstr == "Delaware"
replace fipsstr = "DC" if fipsstr == "District of Columbia"
replace fipsstr = "FL" if fipsstr == "Florida"
replace fipsstr = "GA" if fipsstr == "Georgia"
replace fipsstr = "HI" if fipsstr == "Hawaii"
replace fipsstr = "ID" if fipsstr == "Idaho"
replace fipsstr = "IL" if fipsstr == "Illinois"
replace fipsstr = "IN" if fipsstr == "Indiana"
replace fipsstr = "IA" if fipsstr == "Iowa"
replace fipsstr = "KS" if fipsstr == "Kansas"
replace fipsstr = "KY" if fipsstr == "Kentucky"
replace fipsstr = "LA" if fipsstr == "Louisiana"
replace fipsstr = "ME" if fipsstr == "Maine"
replace fipsstr = "MD" if fipsstr == "Maryland"
replace fipsstr = "MA" if fipsstr == "Massachusetts"
replace fipsstr = "MI" if fipsstr == "Michigan"
replace fipsstr = "MN" if fipsstr == "Minnesota"
replace fipsstr = "MS" if fipsstr == "Mississippi"
replace fipsstr = "MO" if fipsstr == "Missouri"
replace fipsstr = "MT" if fipsstr == "Montana"
replace fipsstr = "NE" if fipsstr == "Nebraska"
replace fipsstr = "NV" if fipsstr == "Nevada"
replace fipsstr = "NH" if fipsstr == "New Hampshire"
replace fipsstr = "NJ" if fipsstr == "New Jersey"
replace fipsstr = "NM" if fipsstr == "New Mexico"
replace fipsstr = "NY" if fipsstr == "New York"
replace fipsstr = "NC" if fipsstr == "North Carolina"
replace fipsstr = "ND" if fipsstr == "North Dakota"
replace fipsstr = "OH" if fipsstr == "Ohio"
replace fipsstr = "OK" if fipsstr == "Oklahoma"
replace fipsstr = "OR" if fipsstr == "Oregon"
replace fipsstr = "PA" if fipsstr == "Pennsylvania"
replace fipsstr = "RI" if fipsstr == "Rhode Island"
replace fipsstr = "SC" if fipsstr == "South Carolina"
replace fipsstr = "SD" if fipsstr == "South Dakota"
replace fipsstr = "TN" if fipsstr == "Tennessee"
replace fipsstr = "TX" if fipsstr == "Texas"
replace fipsstr = "UT" if fipsstr == "Utah"
replace fipsstr = "VT" if fipsstr == "Vermont"
replace fipsstr = "VA" if fipsstr == "Virginia"
replace fipsstr = "WA" if fipsstr == "Washington"
replace fipsstr = "WV" if fipsstr == "West Virginia"
replace fipsstr = "WI" if fipsstr == "Wisconsin"
replace fipsstr = "WY" if fipsstr == "Wyoming"

// recode age grouping
gen age_gp_OLD = age_gp
replace age_gp = "18" if age_gp_OLD == "18-24" 
replace age_gp = "25" if age_gp_OLD == "25-29" 
replace age_gp = "30" if age_gp_OLD == "30-34" 
replace age_gp = "35" if age_gp_OLD == "35-39" 
replace age_gp = "40" if age_gp_OLD == "40-44" 
replace age_gp = "45" if age_gp_OLD == "45-49" 
replace age_gp = "50" if age_gp_OLD == "50-54" 
replace age_gp = "55" if age_gp_OLD == "55-59" 
replace age_gp = "60" if age_gp_OLD == "60-64" 
replace age_gp = "65" if age_gp_OLD == "65-69" 
replace age_gp = "70" if age_gp_OLD == "70-74" 
replace age_gp = "75" if age_gp_OLD == "75-79" 
replace age_gp = "80" if age_gp_OLD == "80+" 
destring age_gp, replace

// aggregate across racial ethnic groups
collapse (sum) pop = tpop, by(year fipsstr sex age_gp edclass) 

// interpolate for quarterly data
// replicate rows
gen n = 4
expandcl = n, cluster(year fipsstr sex age_gp edclass) generate(repl)

// generate variable indicate quarters 
bysort year fipsstr sex age_gp edclass: gen Q1 = _n == 1
bysort year fipsstr sex age_gp edclass: gen Q2 = _n == 2
bysort year fipsstr sex age_gp edclass: gen Q3 = _n == 3
bysort year fipsstr sex age_gp edclass: gen Q4 = _n == 4

gen Q = 1 if Q1 == 1 
replace Q = 2 if Q2 == 1
replace Q = 3 if Q3 == 1
replace Q = 4 if Q4 == 1

replace pop = . if Q > 1

drop n repl Q1 Q2 Q3 Q4

// linear interpolation
gen QYEAR = year
replace QYEAR = year + 0.25 if Q == 2
replace QYEAR = year + 0.5 if Q == 3
replace QYEAR = year + 0.75 if Q == 4

bysort fipsstr sex age_gp edclass: ipolate pop QYEAR, generate(pop_int)

// remove observations after 2020 and fipsstr == USA
drop if year == 2021
drop if fipsstr == "USA"

// combine deaths with population by cell
merge m:m fipsstr year Q age_gp sex edclass using "${dir}/mortality/3_out data/allethn_sumCOD_0020_STATE_QYEAR.dta"

/* _m==1 is for groups that recorded no deaths in that year (those that were not merged but only appear in master...) */
/* _m==2 is for very few states (AK, SD, VT, WV) and age groups (18/75/80) without population counts in 2000 */

keep if _m == 3

/* RUN ONLY ONE OF THE BELOW OR NON FOR OUTCOME BY SEX x EDUCATION */

// SEX ONLY
collapse (sum) pop_int = pop_int tmort = tmort acutmort = acutmort chronmort = chronmort ///
alclivmort = alclivmort othlivmort = othlivmort uijmort = uijmort sijmort = sijmort mvaccmort = mvaccmort ///
restmort = restmort, by(year Q fipsstr age_gp sex) 

// EDUCATION ONLY
collapse (sum) pop_int = pop_int tmort = tmort acutmort = acutmort chronmort = chronmort ///
alclivmort = alclivmort othlivmort = othlivmort uijmort = uijmort sijmort = sijmort mvaccmort = mvaccmort ///
restmort = restmort, by(year Q fipsstr age_gp edclass) 

/* CONTINUE HERE */

 foreach var in t acut chron alcliv othliv uij sij mvacc rest {
   gen `var'rate   = (`var'mort/pop_int)*100000
   }

label var trate "all cause"
label var acutrate "acute 100% AA conditions" 
label var chronrate "chronic 100% AA conditions" 
label var alclivrate "alcoholic liver disease"
label var othlivrate "other liver diseases"
label var uijrate "unintentional injuries"
label var sijrate "intentional injuries"
label var mvaccrate "motorvehicle accidents"
label var restrate  "other causes"


save "${dir}/mortality/3_out data/allethn_rates_1020_STATE_QYEAR.dta", replace

outsheet using "${dir}/mortality/3_out data/allethn_rates_1020_STATE_QYEAR.csv" , comma replace
