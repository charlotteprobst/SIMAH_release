********************************************************************************
* Stata do-file to replicate 
*
* Axelsson Fisk, S., Mulinari, S., Wemrell, M., Leckie, G., Perez Vicente, R.,
*   Merlo, J. Chronic Obstructive Pulmonary Disease in Sweden: an intersectional 
*   multilevel analysis of individual heterogeneity and discriminatory accuracy
********************************************************************************

* Change working directory
cd "Z:\manuscripts-3-under-review\axelsson-fisk2018intersectionality"
// You will need to edit this file directory to wherever you choose to save the 
// dataset "data.dta". The additional datasets, and graphs will be saved to this 
// location as well.

capture log close
log using "analysis.smcl", replace

********************************************************************************
* REPLICATE TABLE 1 NUMBER AND % OF INDIVIDUALS WITH COPD
********************************************************************************

* Load the data
use "data.dta", clear

* Overall number and percentage of new cases of COPD - SB: Uses the sample size for that group as the 'weight'
tabstat proportion [fweight = denominator], statistics(count sum mean)
tabstat percentage [fweight = denominator], statistics(count mean)

* Age
tabstat percentage [fweight = denominator], statistics(count mean) by(old) ///
  nototal

* Gender
tabstat percentage [fweight = denominator], statistics(count mean) ///
  by(female) nototal

* Income
generate income = .
replace income = 1 if medinc==0 & lowinc==0
replace income = 2 if medinc==1 & lowinc==0
replace income = 3 if medinc==0 & lowinc==1
tabstat percentage [fweight = denominator], statistics(count mean) ///
  by(income) nototal
drop income

* Education
tabstat percentage [fweight = denominator], statistics(count mean) by(lowed) ///
  nototal

* Living alone
tabstat percentage [fweight = denominator], statistics(count mean) by(alone) ///
  nototal

* Immigrant
tabstat percentage [fweight = denominator], statistics(count mean) ///
  by(immigrant) nototal

********************************************************************************
* REPLICATE TABLE 1 MODEL 1 ESTIMATES
********************************************************************************
*

*------------------------------------------------------------------------------*
* FIT THE MODEL
*------------------------------------------------------------------------------*

* Fit model 1 by PQL2
runmlwin proportion cons, ///
  level2(stratum: cons) ///
  level1(stratum:) ///
  discrete(distribution(binomial) link(logit) denominator(denominator) pql2) ///
  rigls ///
  nopause
  
* Fit model 1 in manuscript by MCMC
runmlwin proportion cons, ///
  level2(stratum: cons, residuals(u, savechains("m1u.dta", replace))) ///
  level1(stratum:) ///
  discrete(distribution(binomial) link(logit) denominator(denominator)) ///
  mcmc(burnin(5000) chain(50000) thinning(50) ///
    savechains("m1b.dta", replace)) initsprevious ///
  nopause
rename u0 m1u
drop u0se
  
* Present the regression coefficients as odds ratios
runmlwin, or

* Calculate the ICC from the parameter point estimates
scalar m1sigma2u = [RP2]var(cons)
scalar m1sigma2e = _pi^2/3
display "ICC = " %9.3f m1sigma2u/(m1sigma2u + m1sigma2e)

* Calculate the ICC from the chains
use "m1b.dta", clear
rename RP2_var_cons_ sigma2u
generate sigma2e =  _pi^2/3
generate icc = sigma2u/(sigma2u + sigma2e)
mcmcsum icc, variables



*------------------------------------------------------------------------------*
* PREPARE FIXED-PART PAREMETER CHAINS
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
* PREPARE STRATUM RANDOM EFFECTS CHAINS
*------------------------------------------------------------------------------*

use "m1u.dta", clear
drop residual idnum
rename value u
format %9.2f u
sort stratum iteration
order stratum iteration
compress
save "m1u_prepped.dta", replace
isid stratum iteration
codebook iteration, compact



*------------------------------------------------------------------------------*
* MERGE DATA, FIXED-PART PARAMETER AND RANDOM EFFECT CHAINS TOGETHER
*------------------------------------------------------------------------------*

use "data.dta", clear
isid stratum
cross using "m1b_prepped.dta"
isid stratum iteration
sort stratum iteration
merge 1:1 stratum iteration using "m1u_prepped.dta", nogenerate assert(match)
isid stratum iteration
compress
save "m1data_prepped.dta", replace



*------------------------------------------------------------------------------*
* CALCULATE PERCENTAGES OF INTEREST (p = pA + pB)
*------------------------------------------------------------------------------*

* Percentage p based on fixed and random part
use "m1data_prepped.dta", clear
generate p = 100*invlogit(b_cons*cons + u)
label var p "Percentage based on main effects and interactions"
format %9.3f p

* Percentage p based only on the fixed-part
generate pA = 100*invlogit(b_cons*cons)
label var pA "Percentage based only on main effects"
format %9.3f pA

* Percentage pB calculated as the difference between p and pA  
generate pB = p - pA
label var pB "Percentage point difference based on interaction effects"
format %9.3f pB

* Calculate the mean, 2.5th and 97.5th percentiles of the MCMC chains
bysort stratum (iteration): egen pmn = mean(p)
bysort stratum (iteration): egen plo = pctile(p), p(2.5)
bysort stratum (iteration): egen phi = pctile(p), p(97.5)
format %9.3f pmn plo phi
label var pmn "Percentage based on main effects and interactions"
label var plo "Percentage based on main effects and interactions"
label var phi "Percentage based on main effects and interactions"


bysort stratum (iteration): egen pAmn = mean(pA)
bysort stratum (iteration): egen pAlo = pctile(pA), p(2.5)
bysort stratum (iteration): egen pAhi = pctile(pA), p(97.5)
format %9.3f pAmn pAlo pAhi
label var pAmn "Percentage based on main effects"
label var pAlo "Percentage based on main effects"
label var pAhi "Percentage based on main effects"

bysort stratum (iteration): egen pBmn = mean(pB)
bysort stratum (iteration): egen pBlo = pctile(pB), p(2.5)
bysort stratum (iteration): egen pBhi = pctile(pB), p(97.5)
format %9.3f pBmn pBlo pBhi
label var pBmn "Percentage point difference based on interaction effects"
label var pBlo "Percentage point difference based on interaction effects"
label var pBhi "Percentage point difference based on interaction effects"

* Drop chains and just keep their summaries (mean, 2.5th and 97.5th)
drop iteration b* u* p pA pB
duplicates drop
isid stratum

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
sort stratum
isid stratum

* Compress and save the data
compress
save "m1results.dta", replace



********************************************************************************
* REPLICATE TABLE 1 MODEL 2
********************************************************************************

* Load the data
use "data", clear

* Fit model 2 in manuscript by PQL2
runmlwin proportion cons old, ///
  level2(stratum: cons) ///
  level1(stratum:) ///
  discrete(distribution(binomial) link(logit) denominator(denominator) pql2) ///
  rigls ///
  nopause
  
* Fit model 2 in manuscript by MCMC
runmlwin proportion cons old, ///
  level2(stratum: cons, residuals(u, savechains("m2u.dta", replace))) ///
  level1(stratum:) ///
  discrete(distribution(binomial) link(logit) denominator(denominator)) ///
  mcmc(burnin(5000) chain(50000) thinning(50) ///
    savechains("m2b.dta", replace)) initsprevious ///
  nopause

* Present the regression coefficients as odds ratios
runmlwin, or

* Calcualte the ICC from the parameter point estimates
scalar m2sigma2u = [RP2]var(cons)
scalar m2sigma2e = _pi^2/3
display "ICC = " %9.3f m2sigma2u/(m2sigma2u + m2sigma2e)

* Calculate the ICC from the chains
use "m2b.dta", clear
rename RP2_var_cons_ sigma2u
generate sigma2e =  _pi^2/3
generate icc = sigma2u/(sigma2u + sigma2e)
mcmcsum icc, variables



********************************************************************************
* REPLICATE TABLE 1 MODEL 3
********************************************************************************

*------------------------------------------------------------------------------*
* FIT THE MODEL
*------------------------------------------------------------------------------*

* Load the data
use "data", clear

* Fit model 3 in manuscript by PQL2
runmlwin proportion cons old female medinc lowinc lowed alone immigrant, ///
  level2(stratum: cons) ///
  level1(stratum:) ///
  discrete(distribution(binomial) link(logit) denominator(denominator) pql2) ///
  rigls ///
  nopause
  
* Fit model 3 in manuscript by MCMC
runmlwin proportion cons old female medinc lowinc lowed alone immigrant, ///
  level2(stratum: cons, residuals(u, savechains("m3u.dta", replace))) ///
  level1(stratum:) ///
  discrete(distribution(binomial) link(logit) denominator(denominator)) ///
  mcmc(burnin(5000) chain(50000) thinning(50) ///
    savechains("m3b.dta", replace)) initsprevious ///
  nopause

* Present the regression coefficients as odds ratios
runmlwin, or

* Calcualte the ICC from the parameter point estimates
scalar m3sigma2u = [RP2]var(cons)
scalar m3sigma2e = _pi^2/3
display "ICC = " %9.3f m3sigma2u/(m3sigma2u + m3sigma2e)

* Calculate the ICC from the chains
use "m3b.dta", clear
rename RP2_var_cons_ sigma2u
generate sigma2e =  _pi^2/3
generate icc = sigma2u/(sigma2u + sigma2e)
mcmcsum icc, variables



*------------------------------------------------------------------------------*
* PREPARE FIXED-PART PAREMETER CHAINS
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
* PREPARE STRATUM RANDOM EFFECTS CHAINS
*------------------------------------------------------------------------------*

use "m3u.dta", clear
drop residual idnum
rename value u
format %9.2f u
sort stratum iteration
order stratum iteration
compress
save "m3u_prepped.dta", replace
isid stratum iteration
codebook iteration, compact



*------------------------------------------------------------------------------*
* MERGE DATA, FIXED-PART PARAMETER AND RANDOM EFFECT CHAINS TOGETHER
*------------------------------------------------------------------------------*

use "data", clear
isid stratum
cross using "m3b_prepped.dta"
isid stratum iteration
sort stratum iteration
merge 1:1 stratum iteration using "m3u_prepped.dta", nogenerate assert(match)
isid stratum iteration
compress
save "m3data_prepped.dta", replace



*------------------------------------------------------------------------------*
* CALCULATE PERCENTAGES OF INTEREST (p = pA + pB)
*------------------------------------------------------------------------------*

* Percentage p based on fixed and random part
use "m3data_prepped.dta", clear
generate p = 100*invlogit( ///
      b_cons*cons ///
    + b_old*old ///
    + b_female*female ///
    + b_medinc*medinc ///
    + b_lowinc*lowinc ///
    + b_lowed*lowed ///
    + b_alone*alone ///
    + b_immigrant*immigrant ///
    + u ///
  )
label var p "Percentage based on main effects and interactions"
format %9.3f p

* Percentage p based only on the fixed-part
generate pA = 100*invlogit( ///
      b_cons*cons ///
    + b_old*old ///
    + b_female*female ///
    + b_medinc*medinc ///
    + b_lowinc*lowinc ///
    + b_lowed*lowed ///
    + b_alone*alone ///
    + b_immigrant*immigrant ///
  )
label var pA "Percentage based only on main effects"
format %9.3f pA

* Percentage pB calculated as the difference between p and pA  
generate pB = p - pA
label var pB "Percentage point difference based on interaction effects"
format %9.3f pB

* Calculate the mean, 2.5th and 97.5th percentiles of the MCMC chains
bysort stratum (iteration): egen pmn = mean(p)
bysort stratum (iteration): egen plo = pctile(p), p(2.5)
bysort stratum (iteration): egen phi = pctile(p), p(97.5)
format %9.3f pmn plo phi
label var pmn "Percentage based on main effects and interactions"
label var plo "Percentage based on main effects and interactions"
label var phi "Percentage based on main effects and interactions"

bysort stratum (iteration): egen pAmn = mean(pA)
bysort stratum (iteration): egen pAlo = pctile(pA), p(2.5)
bysort stratum (iteration): egen pAhi = pctile(pA), p(97.5)
format %9.3f pAmn pAlo pAhi
label var pAmn "Percentage based on main effects"
label var pAlo "Percentage based on main effects"
label var pAhi "Percentage based on main effects"

bysort stratum (iteration): egen pBmn = mean(pB)
bysort stratum (iteration): egen pBlo = pctile(pB), p(2.5)
bysort stratum (iteration): egen pBhi = pctile(pB), p(97.5)
format %9.3f pBmn pBlo pBhi
label var pBmn "Percentage point difference based on interaction effects"
label var pBlo "Percentage point difference based on interaction effects"
label var pBhi "Percentage point difference based on interaction effects"

* Drop chains and just keep their summaries (mean, 2.5th and 97.5th)
drop iteration b* u* p pA pB
duplicates drop
isid stratum

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
sort stratum
isid stratum

* Compress and save the data
compress
save "m3results.dta", replace



********************************************************************************
* TABLES AND FIGURES FOR THE MANUSCRIPT
********************************************************************************

*------------------------------------------------------------------------------*
* TABLE 2A AND 2B
*------------------------------------------------------------------------------*

use "m1results.dta", clear
keep stratum stratumlabel denominator numerator ///
  old female medinc lowinc lowed alone immigrant ///
  pmn plo phi
sort pmn
list stratum denominator numerator pmn plo phi, ///
  abbreviate(11) noobs separator(0)
compress
generate young = 1 - old
generate male = 1 - female
generate highinc = (medinc==0 & lowinc==0)
generate highed = 1 - lowed
generate cohabiting = 1 - alone
generate swedish = 1 - immigrant
order young old male female highinc medinc lowinc highed lowed cohabiting ///
  alone immigrant swedish, after(stratumlabel)
sort pmn
compress
save "table2.dta", replace

* Table 2A
use "table2.dta", clear
format %9.2f pmn plo phi
list stratum denominator numerator pmn plo phi in 1/5, abbreviate(11) nolabel 
list stratum denominator numerator pmn plo phi in -5/l, abbreviate(11) nolabel

* Table 2B
use "table2.dta", clear
format %9.2f pmn plo phi
list stratum denominator numerator pmn plo phi, ///
  abbreviate(11) nolabel separator(0)


*------------------------------------------------------------------------------*
* TABLE 3A AND 3B
*------------------------------------------------------------------------------*

use "m3results.dta", clear
keep stratum stratumlabel denominator numerator ///
  old female medinc lowinc lowed alone immigrant ///
  pmn plo phi pAmn pAlo pAhi pBmn pBlo pBhi
sort pmn
list stratum denominator numerator ///
  pmn plo phi pAmn pAlo pAhi pBmn pBlo pBhi, ///
  abbreviate(11) noobs separator(0)
compress
generate young = 1 - old
generate male = 1 - female
generate highinc = (medinc==0 & lowinc==0)
generate highed = 1 - lowed
generate cohabiting = 1 - alone
generate swedish = 1 - immigrant
order young old male female highinc medinc lowinc highed lowed cohabiting ///
  alone immigrant swedish, after(stratumlabel)
sort pBmn
compress
save "table3.dta", replace

* Table 3A
use "table3.dta", clear
format %9.2f pmn-pBhi
list stratum pmn-pBhi in 1/5, nolabel
list stratum pmn-pBhi in -5/l, nolabel


*------------------------------------------------------------------------------*
* FIGURE 2
*------------------------------------------------------------------------------*

* Load the data
use "m1results.dta", clear

* Figure 1. Predicted percentages based on main effects and interactions with 
* 95% CI by stratum
twoway ///
  (scatter pmn pmnrank, mcolor(black) msymbol(smcircle)) ///
  (rspike phi plo pmnrank, lcolor(black)) ///
  , ///
  ytitle("Model 1 predicted incidence (%)") ///
  ylabel(, angle(horizontal) format(%9.2f)) ///
  xtitle("Stratum rank") ///
  legend(off) ///
  scheme(s1mono)
graph export "figure2.png", replace width(1000)



*------------------------------------------------------------------------------*
* FIGURE 3
*------------------------------------------------------------------------------*

* Load the data
use "m3results.dta", clear

* Figure 2. Difference in predicted percentage when when we naively ignore the 
* iterations
twoway ///
  (rspike pmn pAmn pmnrank, lcolor(black)) ///
  (scatter pmn pmnrank, mcolor(black) msymbol(smcircle)) ///
  (scatter pAmn pmnrank, mlcolor(black) mfcolor(white) msymbol(smcircle)) ///
   , ///
  ytitle("Model 3 predicted incidence (%)") ///
  ylabel(, angle(horizontal) format(%9.2f)) ///
  xtitle("Stratum rank") ///
  legend(order( ///
    2 "Based on main effects and interaction" ///
    3 "Based only on the main effects") ///
  row(2))  ///
  scheme(s1mono)
graph export "figure3.png", replace width(1000)



*------------------------------------------------------------------------------*
* FIGURE 4
*------------------------------------------------------------------------------*

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

* List strata with statistically significant interaction effects on the 
* predicted incidene
list stratum stratumlabel pBmn pBlo pBhi if pBhi<0, noobs
list stratum stratumlabel pBmn pBlo pBhi if pBlo>0, noobs



********************************************************************************
* END
********************************************************************************

* Close log file
capture log close
translate "analysis.smcl" "analysis.pdf", replace



********************************************************************************
exit
