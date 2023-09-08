********************************************************************************
/* Stata do-file to undertake multilevel logistic regression models of HED

Replicating the method of analysis undertaken in Axelsson Fisk, S., Mulinari, S., Wemrell, M., Leckie, G., Perez Vicente, R., Merlo, J. Chronic Obstructive Pulmonary Disease in Sweden: an intersectional multilevel analysis of individual heterogeneity and discriminatory accuracy */
********************************************************************************

* Change working directory
cd "U:\STATA HED\logit model"

capture log close
log using "analysis.smcl", replace

global MLwiN_path C:\Program Files\MLwiN v3.05\mlwin.exe // home computer
// global MLwiN_path C:\Program Files\MLwiN v3.01\mlwin.exe // Uni computer

********************************************************************************
* TABLE 1 NUMBER AND % OF INDIVIDUALS WHO ARE HEDs
********************************************************************************

* Load the data
use "nhis_data.dta", clear

/* generate dummy variables for runmlwin to being able to calculate predicted effects:
foreach var of varlist SEX age_3_cats race_5_cats education_3_cats decade {
tabulate `var', generate(`var'_dum)
}

* rename dummy variables
rename SEX_dum1 female
rename SEX_dum2 male
rename age_3_cats_dum1 younger_adult
rename age_3_cats_dum2 adult
rename age_3_cats_dum3 older_adult
rename race_5_cats_dum1 Hispanic
rename race_5_cats_dum2 Asian
rename race_5_cats_dum3 Black
rename race_5_cats_dum4 Other
rename race_5_cats_dum5 White
rename education_3_cats_dum1 high
rename education_3_cats_dum2 low
rename education_3_cats_dum3 med
rename decade_dum1 first_decade
rename decade_dum2 second_decade

* sort by intersections
sort intersections

save nhis_data, replace */

* Overall number and percentage of HEDs using the sample size for that group as the 'weight'
tabstat proportion [fweight = denominator], statistics(count sum mean)
tabstat percentage [fweight = denominator], statistics(count mean)

* Age
tabstat percentage [fweight = denominator], statistics(count mean) by(age_3_cats) ///
  nototal

* Gender
tabstat percentage [fweight = denominator], statistics(count mean) ///
  by(SEX) nototal

* Education
tabstat percentage [fweight = denominator], statistics(count mean) by(education_3_cats) ///
  nototal

* Race
tabstat percentage [fweight = denominator], statistics(count mean) by(race_5_cats) ///
  nototal

* Decade
tabstat percentage [fweight = denominator], statistics(count mean) ///
  by(decade) nototal
  
* Intersections
tabstat percentage [fweight = denominator], statistics(count mean) ///
  by(intersections) nototal
  

********************************************************************************
* NULL INTERSECTIONAL MODEL
********************************************************************************
* The first model is an unadjusted, random intercepts model (i.e., a variance components model) with individuals nested within intersectional strata. The purpose of this model is two-fold. First, we performed simple analysis of components of variance in order to calculate the ICC. This measure expresses the share of the total individual variance in the propensity for being a hed that is at the intersectional intersections level. The higher the ICC, the greater the degree of similarity in COPD incidence within the strata and the greater the difference in incidence between the strata. Models with higher ICCs are therefore better at discriminating individuals that are hed from those that are not, compared to models with lower ICCs. In summary, the ICC evaluates the relevance of the intersectional strata for understanding individual risk heterogeneity. The ICC also informs on the DA of the intersectional categorization for distinguishing individuals with COPD from those without.To calculate the ICC, we used the most popular version of the ICC derived from the latent response formulation of the model. *

* Fit model 1 by PQL2
runmlwin proportion cons, ///
  level2(intersections: cons) ///
  level1(intersections:) ///
  discrete(distribution(binomial) link(logit) denominator(denominator) pql2) ///
  rigls ///
  nopause

* Fit model 1 by MCMC
runmlwin proportion cons, ///
  level2(intersections: cons, residuals(u, savechains("m1u.dta", replace))) ///
  level1(intersections:) ///
  discrete(distribution(binomial) link(logit) denominator(denominator)) ///
  mcmc(burnin(5000) chain(50000) thinning(50) ///
    savechains("m1b.dta", replace)) initsprevious ///
  nopause
rename u0 m1u // Save the residuals as the variable m1u (appending to the full dataset)
drop u0se
estimates store model_1

* Calculate the ICC from the parameter point estimates
scalar m1sigma2u = [RP2]var(cons) // Saves the level 2 variance (1.42) as a scalar variable named m1sigma2u
scalar m1sigma2e = _pi^2/3  // Saves the level 1 variance (pi^2/3 ) as a scalar variable named m1sigma2e
display "ICC = " %9.3f m1sigma2u/(m1sigma2u + m1sigma2e) // Utilise standard VPC formula to calculate VPC = 0.302

* Calculate the ICC from the chains
use "m1b.dta", clear
rename RP2_var_cons_ sigma2u
generate sigma2e =  _pi^2/3
generate icc = sigma2u/(sigma2u + sigma2e)
mcmcsum icc, variables // generate an ICC based on the level 2 variance predicted from each iteration and take the mean and CI

*------------------------------------------------------------------------------*
* PREPARE FIXED-PART PAREMETER CHAINS i.e stores all of the predicted constants for each iteration as a variable b_cons, and sorts in order of iteration number
*------------------------------------------------------------------------------*
use "m1b.dta", clear
drop deviance RP2_var_cons_ OD_bcons_1 
rename FP1_* b_* 
format %9.2f b_*  
compress
save "m1b_prepped.dta", replace 
isid iteration 
codebook iteration, compact 

*------------------------------------------------------------------------------*
* PREPARE intersections RANDOM EFFECTS CHAINS i.e. sorts the order of the predicted residuals (so that all iterations for one intersection grouped together)
*------------------------------------------------------------------------------*
use "m1u.dta", clear 
drop residual idnum
rename value u
format %9.2f u
sort intersections iteration
order intersections iteration
compress
save "m1u_prepped.dta", replace 
isid intersections iteration
codebook iteration, compact

*------------------------------------------------------------------------------*
* MERGE DATA, FIXED-PART PARAMETER AND RANDOM EFFECT CHAINS TOGETHER
*------------------------------------------------------------------------------*
use "nhis_data.dta", clear
isid intersections
cross using "m1b_prepped.dta"
isid intersections iteration
sort intersections iteration
merge 1:1 intersections iteration using "m1u_prepped.dta", nogenerate assert(match)
isid intersections iteration
compress
save "m1data_prepped.dta", replace // now have a constant and a residual for each intersection for each iteration

*------------------------------------------------------------------------------*
* CALCULATE PERCENTAGES OF INTEREST (p = pA + pB)
*------------------------------------------------------------------------------*
* Percentage p based on fixed and random part
use "m1data_prepped.dta", clear
generate p = 100*invlogit(b_cons*cons + u) // adds the constant and residual together for each intersection to generate the estimated incidence for each iteration (the constant and the residual vary for every row).  Back-transforms the data so that a % rather than on log scale.
label var p "Percentage based on main effects and interactions"
format %9.3f p // (to 3dp)

* Percentage p based only on the fixed-part
generate pA = 100*invlogit(b_cons*cons)
label var pA "Percentage based only on main effects"
format %9.3f pA

* Percentage pB calculated as the difference between p and pA  
generate pB = p - pA
label var pB "Percentage point difference based on interaction effects"
format %9.3f pB

* Calculate the mean, 2.5th and 97.5th percentiles of the MCMC chains
bysort intersections (iteration): egen pmn = mean(p) // the estimated % for each intersection (total), taking the mean across all iterations
bysort intersections (iteration): egen plo = pctile(p), p(2.5)
bysort intersections (iteration): egen phi = pctile(p), p(97.5)
format %9.3f pmn plo phi
label var pmn "Percentage based on main effects and interactions"
label var plo "Percentage based on main effects and interactions"
label var phi "Percentage based on main effects and interactions"


bysort intersections (iteration): egen pAmn = mean(pA) // the estimated % for each intersection (additive effects only), taking the mean across all iterations
bysort intersections (iteration): egen pAlo = pctile(pA), p(2.5)
bysort intersections (iteration): egen pAhi = pctile(pA), p(97.5)
format %9.3f pAmn pAlo pAhi
label var pAmn "Percentage based on main effects"
label var pAlo "Percentage based on main effects"
label var pAhi "Percentage based on main effects"

bysort intersections (iteration): egen pBmn = mean(pB) // the estimated % for each intersection (multiplicative effects only), taking the mean across all iterations
bysort intersections (iteration): egen pBlo = pctile(pB), p(2.5)
bysort intersections (iteration): egen pBhi = pctile(pB), p(97.5)
format %9.3f pBmn pBlo pBhi
label var pBmn "Percentage point difference based on interaction effects"
label var pBlo "Percentage point difference based on interaction effects"
label var pBhi "Percentage point difference based on interaction effects"

* Drop chains and just keep their summaries (mean, 2.5th and 97.5th)
drop iteration b* u* p pA pB
duplicates drop
isid intersections

* Ranks
sort pmn
generate pmnrank = _n
order pmnrank, after(phi)
sort pAmn
generate pAmnrank = _n
order pAmnrank, after(pAhi)
sort pBmn
generate pBmnrank = _n
order pBmnrank, after(pBhi)

* Sort the data
sort intersections
isid intersections

* Compress and save the data
compress
save "m1results.dta", replace
  
********************************************************************************
* INTERSECTIONAL MODEL WITH MAIN EFFECTS
********************************************************************************

*------------------------------------------------------------------------------*
* FIT THE MODEL
*------------------------------------------------------------------------------*
* Load the data
use "nhis_data", clear

* Fit model by PQL2
runmlwin proportion cons female ///
younger_adult older_adult ///
Black Asian Other Hispanic ///
med high ///
second_decade, ///
  level2(intersections: cons) ///
  level1(intersections:) ///
  discrete(distribution(binomial) link(logit) denominator(denominator) pql2) ///
  rigls ///
  nopause
 
* Fit model by MCMC
runmlwin proportion cons female ///
younger_adult older_adult ///
Black Asian Other Hispanic ///
med high ///
second_decade, ///
  level2(intersections: cons, residuals(u, savechains("m3u.dta", replace))) ///
  level1(intersections:) ///
  discrete(distribution(binomial) link(logit) denominator(denominator)) ///
  mcmc(burnin(5000) chain(50000) thinning(50) ///
    savechains("m3b.dta", replace)) initsprevious ///
  nopause
 estimates store model_3

* Present the regression coefficients as odds ratios
runmlwin, or

* Calcualte the ICC from the parameter point estimates
scalar m3sigma2u = [RP2]var(cons)
scalar m3sigma2e = _pi^2/3
display "ICC = " %9.3f m3sigma2u/(m3sigma2u + m3sigma2e) // 0.032

* Calculate the ICC from the chains
use "m3b.dta", clear
rename RP2_var_cons_ sigma2u
generate sigma2e =  _pi^2/3
generate icc = sigma2u/(sigma2u + sigma2e)
mcmcsum icc, variables // .0317 (95% CI 0.024-0.042) 

*------------------------------------------------------------------------------*
* PREPARE FIXED-PART PAREMETER CHAINS i.e stores the constant and the predicted coefficeints for each iteration as a variables name b_variable e.g. b_sex, and sorts in order of iteration number
*------------------------------------------------------------------------------*
use "m3b.dta", clear
drop deviance RP2_var_cons_ OD_bcons_1
rename FP1_* b_*
format %9.2f b_*
compress
save "m3b_prepped.dta", replace
isid iteration
codebook iteration, compact

*------------------------------------------------------------------------------*
* PREPARE intersections RANDOM EFFECTS CHAINS i.e. sorts the order of the predicted residuals (so that all iterations for one intersection grouped together)
*------------------------------------------------------------------------------*
use "m3u.dta", clear
drop residual idnum
rename value u
format %9.2f u
sort intersections iteration
order intersections iteration
compress
save "m3u_prepped.dta", replace
isid intersections iteration
codebook iteration, compact

*------------------------------------------------------------------------------*
* MERGE DATA, FIXED-PART PARAMETER AND RANDOM EFFECT CHAINS TOGETHER
*------------------------------------------------------------------------------*
use "nhis_data", clear
isid intersections
cross using "m3b_prepped.dta"
isid intersections iteration
sort intersections iteration
merge 1:1 intersections iteration using "m3u_prepped.dta", nogenerate assert(match)
isid intersections iteration
compress
save "m3data_prepped.dta", replace // saves table with a constant and a residual for each intersection, for each iteration

*------------------------------------------------------------------------------*
* CALCULATE PERCENTAGES OF INTEREST (p = pA + pB)
*------------------------------------------------------------------------------*
* Percentage p based on fixed and random part
use "m3data_prepped.dta", clear
generate p = 100*invlogit( ///
      b_cons*cons ///
    + b_female*female ///
	+ b_younger_adult*younger_adult ///
    + b_older_adult*older_adult ///
    + b_Black*Black ///
    + b_Asian*Asian ///
    + b_Other*Other ///
    + b_Hispanic*Hispanic ///
	+ b_med*med ///
	+ b_high*high ///
	+ b_second_decade*second_decade ///
    + u ///
  )
label var p "Percentage based on main effects and interactions"
format %9.3f p

* Percentage p based only on the fixed-part
generate pA = 100*invlogit( ///
      b_cons*cons ///
    + b_female*female ///
	+ b_younger_adult*younger_adult ///
    + b_older_adult*older_adult ///
    + b_Black*Black ///
    + b_Asian*Asian ///
    + b_Other*Other ///
    + b_Hispanic*Hispanic ///
	+ b_med*med ///
	+ b_high*high ///
	+ b_second_decade*second_decade ///
  )
label var pA "Percentage based only on main effects"
format %9.3f pA

* Percentage pB calculated as the difference between p and pA  
generate pB = p - pA
label var pB "Percentage point difference based on interaction effects"
format %9.3f pB

* Calculate the mean, 2.5th and 97.5th percentiles of the MCMC chains
bysort intersections (iteration): egen pmn = mean(p)
bysort intersections (iteration): egen plo = pctile(p), p(2.5)
bysort intersections (iteration): egen phi = pctile(p), p(97.5)
format %9.3f pmn plo phi
label var pmn "Percentage based on main effects and interactions"
label var plo "Percentage based on main effects and interactions"
label var phi "Percentage based on main effects and interactions"

bysort intersections (iteration): egen pAmn = mean(pA)
bysort intersections (iteration): egen pAlo = pctile(pA), p(2.5)
bysort intersections (iteration): egen pAhi = pctile(pA), p(97.5)
format %9.3f pAmn pAlo pAhi
label var pAmn "Percentage based on main effects"
label var pAlo "Percentage based on main effects"
label var pAhi "Percentage based on main effects"

bysort intersections (iteration): egen pBmn = mean(pB)
bysort intersections (iteration): egen pBlo = pctile(pB), p(2.5)
bysort intersections (iteration): egen pBhi = pctile(pB), p(97.5)
format %9.3f pBmn pBlo pBhi
label var pBmn "Percentage point difference based on interaction effects"
label var pBlo "Percentage point difference based on interaction effects"
label var pBhi "Percentage point difference based on interaction effects"

* Drop chains and just keep their summaries (mean, 2.5th and 97.5th)
drop iteration b* u* p pA pB
duplicates drop
isid intersections

* Ranks
sort pmn
generate pmnrank = _n
order pmnrank, after(phi)
sort pAmn
generate pAmnrank = _n
order pAmnrank, after(pAhi)
sort pBmn
generate pBmnrank = _n
order pBmnrank, after(pBhi)

* Sort the data
sort intersections
isid intersections

* Compress and save the data
compress
save "m3results.dta", replace

*------------------------------------------------------------------------------*
* VIEW RESULTS ON THE LOG SCALE
*------------------------------------------------------------------------------*

use "m3data_prepped.dta", clear
* log odds based on fixed and random part
generate log_p = ( ///
      b_cons*cons ///
    + b_female*female ///
	+ b_younger_adult*younger_adult ///
    + b_older_adult*older_adult ///
    + b_Black*Black ///
    + b_Asian*Asian ///
    + b_Other*Other ///
    + b_Hispanic*Hispanic ///
	+ b_med*med ///
	+ b_high*high ///
	+ b_second_decade*second_decade ///
    + u ///
  )
label var log_p "Log odds based on main effects and interactions"
format %9.3f log_p

* Log odds based only on the fixed-part
generate log_pA = ( ///
      b_cons*cons ///
    + b_female*female ///
	+ b_younger_adult*younger_adult ///
    + b_older_adult*older_adult ///
    + b_Black*Black ///
    + b_Asian*Asian ///
    + b_Other*Other ///
    + b_Hispanic*Hispanic ///
	+ b_med*med ///
	+ b_high*high ///
	+ b_second_decade*second_decade ///
  )
label var log_pA "Log odds based only on main effects"
format %9.3f log_pA

* Calculate the mean, 2.5th and 97.5th percentiles of the MCMC chains
bysort intersections (iteration): egen log_pmn = mean(log_p)
bysort intersections (iteration): egen log_plo = pctile(log_p), p(2.5)
bysort intersections (iteration): egen log_phi = pctile(log_p), p(97.5)
format %9.3f log_pmn log_plo log_phi
label var log_pmn "Log odds based on main effects and interactions"
label var log_plo "Log odds  based on main effects and interactions"
label var log_phi "Log odds  based on main effects and interactions"

bysort intersections (iteration): egen log_pAmn = mean(log_pA)
bysort intersections (iteration): egen log_pAlo = pctile(log_pA), p(2.5)
bysort intersections (iteration): egen log_pAhi = pctile(log_pA), p(97.5)
format %9.3f log_pAmn log_pAlo log_pAhi
label var log_pAmn "Log odds  based on main effects"
label var log_pAlo "Log odds  based on main effects"
label var log_pAhi "Log odds  based on main effects"

* Drop chains and just keep their summaries (mean, 2.5th and 97.5th)
drop iteration b* u* log_p log_pA
duplicates drop
isid intersections

generate pmn = 100*invlogit(log_pmn)
generate pAmn = 100*invlogit(log_pAmn)

save "m3results_logodds.dta"

********************************************************************************
* TABLES AND FIGURES FOR THE MANUSCRIPT
********************************************************************************

*------------------------------------------------------------------------------*
* Model 1 estimates per intersectional group
*------------------------------------------------------------------------------*
use "m1results.dta", clear
keep intersections denominator numerator ///
age_3_cats SEX race_5_cats education_3_cats decade ///
male female ///
younger_adult adult older_adult ///
White Black Asian Other Hispanic ///
high_school_or_less some_college four_plus_years_college ///
first_decade second_decade ///
  pmn plo phi
sort pmn
list intersections SEX age_3_cats race_5_cats education_3_cats decade denominator numerator pmn plo phi, ///
  abbreviate(11) noobs separator(0)
compress
sort pmn
compress
save "table2.dta", replace

* Table of the highest and lowest groups
use "table2.dta", clear
format %9.2f pmn plo phi
list intersections SEX age_3_cats race_5_cats education_3_cats decade denominator numerator pmn plo phi in 1/5, abbreviate(11) nolabel 
list intersections SEX age_3_cats race_5_cats education_3_cats decade denominator numerator pmn plo phi in -5/l, abbreviate(11) nolabel

* Table of all intersections, ordered lowest to highest
use "table2.dta", clear
format %9.2f pmn plo phi
list intersections SEX age_3_cats race_5_cats education_3_cats decade denominator numerator pmn plo phi, ///
  abbreviate(11) nolabel separator(0)
  
  
*------------------------------------------------------------------------------*
* Model 3 results
*------------------------------------------------------------------------------*
use "m3results.dta", clear
keep intersections SEX age_3_cats race_5_cats education_3_cats decade denominator numerator ///
 age_3_cats SEX race_5_cats education_3_cats decade ///
male female ///
younger_adult adult older_adult ///
White Black Asian Other Hispanic ///
low med high ///
first_decade second_decade ///
  pmn plo phi pAmn pAlo pAhi pBmn pBlo pBhi
sort pmn
list intersections SEX age_3_cats race_5_cats education_3_cats decade denominator numerator ///
  pmn plo phi pAmn pAlo pAhi pBmn pBlo pBhi, ///
  abbreviate(11) noobs separator(0)
compress
sort pBmn
compress
save "table3.dta", replace

* Table of the highest and lowest groups
use "table3.dta", clear
format %9.2f p_total-p_interact_hi
list intersections SEX age_3_cats race_5_cats education_3_cats decade p_total - p_interact_hi in 1/5, nolabel
list intersections SEX age_3_cats race_5_cats education_3_cats decade p_total - p_interact_hi in -5/l, nolabel


*------------------------------------------------------------------------------*
* GRAPHS
*------------------------------------------------------------------------------*

** Figure 2
* Fig. 2 demonstrates the heterogeneity between intersectional strata in predicted HED incidence based on model 1 and thus conflating main and interaction effects of the social dimensions.


* Load the data
use "m1results.dta", clear
* Figure 1. Predicted percentages based on main effects and interactions with 95% CI by stratum
twoway ///
  (scatter pmn pmnrank, mcolor(black) msymbol(smcircle)) ///
  (rspike phi plo pmnrank, lcolor(black)) ///
  , ///
  ytitle("Model 1 predicted incidence of HED (%)") ///
  ylabel(, angle(horizontal) format(%9.2f)) ///
  xtitle("Stratum rank") ///
  legend(off) ///
  scheme(s1mono)
graph export "figure2.png", replace width(1000)


** FIGURE 3
* Fig. 3 demonstrates the small changes in predicted incidence in model 3 when comparing predictions based on the total effects with predictions based on main effects only. The difference between these predictions represent the interaction effects. 

* Load the data
use "m3results.dta", clear
twoway ///
  (rspike pmn pAmn pmnrank, lcolor(black)) ///
  (scatter pmn pmnrank, mcolor(black) msymbol(smcircle)) ///
  (scatter pAmn pmnrank, mlcolor(black) mfcolor(white) msymbol(smcircle)) ///
   , ///
  ytitle("Full intersectional model - predicted incidence (%)") ///
  ylabel(, angle(horizontal) format(%9.2f)) ///
  xtitle("Stratum rank") ///
  legend(order( ///
    2 "Based on main effects and interaction" ///
    3 "Based only on the main effects") ///
  row(2))  ///
  scheme(s1mono)
graph export "figure3.png", replace width(1000)


** FIGURE 4
/* Fig. 4. shows the intersectional interaction effects on incidence of HED by intersectional strata. Point estimates of the incidences attributable to intersectional interaction and their 95% CIs are based on the full intersectional model.
Interaction effects are calculated as the incidence according to the total effect (intersectional effects and main effects) minus incidence according to main effect only, for each intersectional stratum.*/

* Load the data
use "m3results.dta", clear
* Difference in predicted incidence with 95% CI when we naively gnore the 
* interactions
twoway ///
  (scatter pBmn pBmnrank, mcolor(black) msymbol(smcircle)) ///
  (rspike pBhi pBlo pBmnrank, lcolor(black)) ///
  , ///
  ytitle("Model 3 interaction effect" ///
    "on the predicted incidence (%)") ///
  ylabel(, angle(horizontal) format(%9.2f)) ///
  yline(0) ///
  xtitle("Stratum rank") ///
  legend(off) ///
  scheme(s1mono)
graph export "figure4.png", replace width(1000)

* List strata with statistically significant interaction effects on the predicted incidence
list intersections SEX age_3_cats race_5_cats education_3_cats decade pBmn pBlo pBhi if pBhi<0, noobs
list intersections SEX age_3_cats race_5_cats education_3_cats decade pBmn pBlo pBhi if pBlo>0, noobs

