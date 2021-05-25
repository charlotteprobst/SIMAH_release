set more off

cd "C:/Users/marie/Dropbox/NIH2020/Demography/3_out CPS data/"
use "cepr_march_0018.dta", clear

/* make an identifier by each hh */
bysort year hhseq: gen one=1 if _n==1


/* identify a head in this order:
   for relhdh: 1=head, 2=primary individual, 3=husband, 4=wife of head
   for relhdh8088: 1=head, 2=primary individual, 3 =spouse of head */
// note: relhdh8088 was used in earlier surveys but not in the time range included here
gen hhead=1 if relhdh8088==1
replace hhead=1 if relhdh==1
bysort year hhseq: egen hashead=max(hhead)

count if one==1
count if one==1 & hashead==.

replace hhead=1 if relhdh8088==2 & hashead==.
replace hhead=1 if relhdh==2 & hashead==.

drop hashead
bysort year hhseq: egen hashead=max(hhead)
count if one==1 & hashead==.


replace hhead=1 if relhdh8088==3 & hashead==.
replace hhead=1 if relhdh==3 & hashead==.

drop hashead
bysort year hhseq: egen hashead=max(hhead)
count if one==1
count if one==1 & hashead==.

replace hhead=1 if relhdh==4 & hashead==.

drop hashead
bysort year hhseq: egen hashead=max(hhead)
count if one==1
count if one==1 & hashead==.


/* what's left unidentified is a head in 1% of cases where highest ranked person is son/daughter of head */
label var hashead "=1 if a householder has been identified"
label var hhead "=1 if person is householder"

tab relhdh if hhead==1
tab relhdh8088 if hhead==1




/*make some variables for head of household */
foreach var in age edclass married marstat female wbho {
   gen hhead`var' = `var'*hhead 
   bysort year hhseq: egen H`var'=max(hhead`var') 
   label var H`var' "householder `var'" 
   drop hhead`var' 
}

lab value Hmarstat marstat
lab value Hedclass edlab

save "cepr_march_0018_final.dta", replace

