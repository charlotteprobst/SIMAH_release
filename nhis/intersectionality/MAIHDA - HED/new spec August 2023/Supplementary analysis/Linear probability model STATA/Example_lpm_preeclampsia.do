********************************************************************************
* Multilevel modeling for measuring interaction of effects between multiple
* categorical variables: an illustrative application using risk factors for
* preeclampsia
********************************************************************************
********************************************************************************
cd "U:\STATA HED\linear probability model\example"
 
********************************************************************************
* (1) PREPARE THE DATA
********************************************************************************

/* Make sure stratum start at #1 and that categorical variables have 1 as their lwoest category (rather than zero).
Drop any missing data */


* Load the data
use "dataoriginal.dta", clear
drop ove? pari?
drop cons
format %9.3f prop
codebook grupo
generate stratum = grupo + 1, after(grupo)
label var stratum "Stratum"
codebook stratum
drop grupo
label var numer "Number of women with preeclampsia"
label var denom "Number of women"
label var denom "Proportion of women with preeclampsia"
codebook parypre
generate parpre = parypre + 1, after(parypre)
label var parpre "Parity and previous preclampsia"
label define parpre ///
1 "Nulliparous" ///
2 "Parous without previous preclampsia" ///
3 "Parous with previous preclampsia"
label values parpre parpre
tabulate parpre parypre, missing
drop parypre
codebook hyperton
generate hyper = ., after(hyperton)
replace hyper = hyperton + 1
label var hyper "Chronic hypertension"
label define hyper ///
1 "No" ///
2 "Yes"
label values hyper hyper
tabulate hyper hyperton, missing
drop hyperton
codebook bordf2
generate multiple = ., after(bordf2)
replace multiple = bordf2 + 1
label var multiple "Multiple pregnancy"
label define multiple ///
1 "No" ///
2 "Yes"
label values multiple multiple
tabulate multiple bordf2, missing
drop bordf2
codebook bmiclass
generate bmi = bmiclass + 1, after(bmiclass)
label var bmi "Body mass index (BMI) category"
label define bmi ///
1 "Underweight/normal" ///
2 "Overweight" ///
3 "Obese"
label values bmi bmi
tabulate bmi bmiclass, missing
drop bmiclass

* Manually add any missing stratum (in this case just one, stratum 36)
set obs 36
replace stratum = 36 in 36
replace numer = 0 in 36
replace denom = 0 in 36
replace prop = 0 in 36
replace parpre = 3 in 36
replace hyper = 2 in 36
replace multiple = 2 in 36
replace bmi = 3 in 36

* Make dummy variables
tabulate parpre, generate(parpre)
tabulate hyper, generate(hyper)
tabulate multiple, generate(multiple)
tabulate bmi, generate(bmi)
egen pickone = tag(stratum)
* Compress and save the data
compress
save "datashort.dta", replace

* Make a long version of the data
use "datashort.dta", clear
drop if denom==0
expand denom // generates the same number of indiviudals (row) as the denominator. 
bysort stratum: generate y = (_n<=numer) // assigns the value of 1 to X of the individuals (where X is the numerator)
sort stratum y
generate id = _n, after(stratum) // generates an individual ID for each person (row)
compress
save "datalong.dta", replace

********************************************************************************
* (2) FIT MODELS
********************************************************************************
// REML preferred when J is low as otherwise sigma2u notably biased down
// However...
// REML not supported with weights (so must work on long data which is computationally slower)
// REML not supported with robust variance estimation (so most use model-based standard errors which assume homoskedasticity)
// Can fit MLE versions to cluster robust standard errors to assess importance
// Weights are not allowed with the bootstrap prefix
* Load the data
use "datalong.dta", clear
set cformat %9.3f

* Multilevel linear regression: Model 1
/* mixed fits a multilevel model (aka a hierarchical linear model). The overall error distribution of the linear mixed-effects model is assumed to be Gaussian, and heteroskedasticity and correlations within lowest-level groups also may be modeled. */

mixed y || stratum:, reml // Linear mixed-effects model of y with random intercepts by stratum, with restricted maximum-likelihood (REML) estimation instead of the default maximum likelihood (ML) estimation

/* in the output:
var(_cons) is the variance of the level-two errors - in this case estimated as 0.015 with standard error 0.004.
var(Residual) displays the estimated variance of the overall error term. This is the variance of the level-one errors, that is, the residuals */

estimates store m1 // store model results
estimates dir // displays the details of the stored model
estimates table m1 // displayes the model coefficients
matrix list e(b) // displays the name of the random effects

// Transform the variance values (the residuals)
scalar m1sigma2u = exp(_b[lns1_1_1:_cons])^2 // level 2 variance as a scaler (lns1_1_1 is the logarithm of the level 2 variance).  
scalar m1sigma2e = exp(_b[lnsig_e:_cons])^2 // level 1 variance as a scalar (lnsig_e is the logarithm of the level 1 variance).
// Estimate the VPC
scalar m1sigma2r = m1sigma2u + m1sigma2e // Total variance = level 1 + level 2 variance
scalar m1vpc = m1sigma2u/(m1sigma2u + m1sigma2e) // VPC (the proportion of variance at the stratum level) i.e. the between group variance

// store the transformed variance, vpc and number of intersections as scalars
scalar m1J = e(N_g)[1,1] // The number of interesections included in the analysis
estadd scalar sigma2u = m1sigma2u // add the level 2 variance (residual) to the e() object (and call it sigma2u) 
estadd scalar sigma2e = m1sigma2e  // add the level 1 variance (residual) to the e() object (and call it sigma2e) 
estadd scalar sigma2r = m1sigma2r // add the total variance to the e() object (and call it sigma2r)
estadd scalar vpc = m1vpc //  // add the VPC to the e() object (and call it vpc)
estadd scalar J = m1J // Add the number of intersections to the e() object (and call it J)

// Model predictions and confidence intervals
predict m1xb, xb // The predicted proportion or 'AR' across all strata (20%)
predict m1xbse, stdp // the standard error of the predicted proportion 2%


generate m1r = y - m1xb // The raw residual rij (the difference between the individual outcome (1 or 0) and the predicted proportion across all strata (i.e. 1 - 0.2 or  - 0.2). 
generate m1s = m1sigma2u/(m1sigma2u + (m1sigma2e/denom)) // Calculate the shrinkage factor for each intersection.

predict m1u, reffects // predict the random effects (intercept) for each strata 
predict m1use, reses // and the standard error around the random effects (varies by strata)

generate m1usig = (m1uhi<0 | m1ulo>0) if m1uhi<. & m1ulo<. // check if the strata predicted prevelance is significant or not (if CI != 0)
generate m1xbu = m1xb + m1u // *** combine the overall estimate (0.2) and the strata residuals to get the total predicted proportion for each strata)

// Estimate the area under the reciever operator curve as a measure of DA, and CIs around it
roctab y m1xb // reciever operator curve
estadd scalar aurocxb = r(area) // add the area under the ROC area to the e() object (and call it aurocxb) 
roctab y m1xbu // reciever operator curve
estadd scalar aurocxbu = r(area) // add the area under the ROC area to the e() object (and call it aurocxb) 
estimates save "m1.ster", replace // save the model estimates as a .ster file so that can be used later

generate m1xblo = m1xb -1.96*m1xbse // overall group prediction lower CI (16%)
generate m1xbhi = m1xb +1.96*m1xbse // overall prediction upper CI (25%)

generate m1ulo = m1u -1.96*m1use // stratum random effects lower CI (varies by strata)
generate m1uhi = m1u +1.96*m1use // stratum random effects upper CI (varies by strata)

generate m1xbulo = m1xbu -1.96*sqrt(m1xbse^2 + m1use^2) // overall estimate lower CI (using combined SE estimate)
generate m1xbuhi = m1xbu +1.96*sqrt(m1xbse^2 + m1use^2) // overall estimate upper CI (using combined SE estimate)

regress m1r i.stratum // estimates of the raw residual based on the stratum ?
predict m1a, xb // the predicted residual for all strata
predict m1ase, stdp
generate m1xba = m1xb + m1a

* Multilevel linear regression: Model 2
mixed y parpre2 parpre3 hyper2 multiple2 bmi2 bmi3 || stratum:, reml // include the dummy variables as fixed effects
estimates store m2

scalar m2sigma2u = exp(_b[lns1_1_1:_cons])^2 // level 2 variance
scalar m2sigma2e = exp(_b[lnsig_e:_cons])^2 // level 1 variance
scalar m2sigma2r = m2sigma2u + m2sigma2e // total variance 
scalar m2vpc = m2sigma2u/(m2sigma2u + m2sigma2e) // VPC
scalar m2J = e(N_g)[1,1] //number of itersectional groups
estadd scalar sigma2u = m2sigma2u  // sigma2u = level 2 variance
estadd scalar sigma2e = m2sigma2e // sigma2e = level 1 variance
estadd scalar sigma2r = m2sigma2r // sigma2r = total variance 
estadd scalar vpc = m2vpc // vpc = vpc
estadd scalar J = m2J // J = number of intersectional groups

// Model predictions
predict m2xb, xb // ARDM. calculates the linear prediction xβ based on the estimated fixed effects (coefficients) in the model (i.e. additive effects only).
// This is equivalent to fixing all random effects in the model to their theoretical mean value of 0
predict m2xbse, stdp // SE around the additive effects
generate m2r = y - m2xb // Difference between the observed and predicted values
generate m2s = m2sigma2u/(m2sigma2u + (m2sigma2e/denom)) // shrinkage factor
predict m2u, reffects // ARDI. predict the random intercept (interactions) for each strata 
predict m2use, reses // and their SEs
generate m2xbu = m2xb + m2u // AR. The 'best' estimate, combining the additive and multiplicative effects (i.e. the total predicted proportion for each strata)

// NB (for understading):  predictions would be = coefficeints of the fixed effects + m2u ? (where m2u incorporates both the individual and group variance)

// AUC
roctab y m2xb 
estadd scalar aurocxb = r(area)
roctab y m2xbu
estadd scalar aurocxbu = r(area)
estimates save "m2.ster", replace

// Confidence intervals
generate m2xblo = m2xb - 1.96*m2xbse // lower CI for additive effects (ARDM)
generate m2xbhi = m2xb + 1.96*m2xbse // upper CI for additive effects (ARDM)
generate m2ulo = m2u - 1.96*m2use // lower CI around the random effects for each strata (ARDI)
generate m2uhi = m2u + 1.96*m2use // upper CI around the random effects for each strata (ARDI)
generate m2xbulo = m2xbu -1.96*sqrt(m2xbse^2 + m2use^2) // overall estimate lower CI (using combined SE estimate) (AR)
generate m2xbuhi = m2xbu +1.96*sqrt(m2xbse^2 + m2use^2) // overall estimate upper CI (using combined SE estimate) (AR)
generate m2usig = (m2uhi<0 | m2ulo>0) if m2uhi<. & m2ulo<. // identify which random effects (interactions) are significant

// Predicting the proportion due to interaction effects?
regress m2r i.stratum // model to predicct the difference between the observed and predicted values, based on strata (? similar to the mean difference?)
predict m2a, xb // the predicted difference per strata 
predict m2ase, stdp // SE
generate m2xba = m2xb + m2a //the predicted difference + the standard estimate (nb. almost identifcal to m2xbu)


* Format variables
* Compress and save the data
compress
save "resultslong.dta", replace
* Save a short version of the data
use "resultslong.dta", clear
drop id y m1r m2r
duplicates drop
isid stratum
compress
save "resultsshort.dta", replace

********************************************************************************
* (3) ANALYSE RESULTS
********************************************************************************
*-------------------------------------------------------------------------------
* STATISTICS PRESENTED IN THE ABSTRACT
*-------------------------------------------------------------------------------
* Number of strata
use "datashort.dta", clear
count
// 36
* Number of women
use "datalong.dta", clear
count
// 652,603
* List of risk factors
use "datashort.dta", clear
codebook parpre hyper multiple bmi, compact
// Note that the variables parity and previous eclampsia are combined in the single variable parpre
* Observed risk of preeclampsia
use "datashort.dta", clear
summarize prop [fweight = denom]
// .0383756
* Range in predicted risk of preeclampsia across the strata
use "resultsshort.dta", clear
replace prop = 100*prop
replace m1xbu = 100* m1xbu // null model predicted proportions
replace m2xbu = 100* m2xbu // full model predicted proportions
tabstat prop m1xbu m2xbu, stat(mean min p10 p25 p50 p75 p90 max) format(%2.0f) // comparing observed and predicted proportions
* Model 1 AUROC and VPC
estimates use "m1.ster"
estimates store m1
estimates use "m2.ster"
estimates store m2
esttab m1 m2, wide b(%9.3f) t se order(_cons) stats(sigma2u sigma2e sigma2r vpc aurocx b aurocxbu J N) 
// VPC 29.8% in null model and 1.9% in full model
// AUC equal in both (0.732)

* Proportion of the variation attributable to interaction effects
estimates use "m1.ster"
scalar m1sigma2u = e(sigma2u)
estimates use "m2.ster"
scalar m2sigma2u = e(sigma2u)
display "Percentage of varation between strata attributable to interaction effects = " %2.0f 100*m2sigma2u/m1sigma2u "%" // between strata variance in model 2 / between strata variance in model 3

* Interactions
use "resultsshort.dta", clear
tabulate m2u m2usig // table showing residual and if sifnificant
// One strata presented a risk above the expected and two other strata presented a risk below the expected.

*-------------------------------------------------------------------------------
* STATISTICS PRESENTED IN THE METHODS SECTION
*-------------------------------------------------------------------------------
* Number of women
use "datalong.dta", clear
count
// 652,603
* Table 1
use "resultsshort.dta", clear
label define parpre ///
1 "Nulliparous" ///
2 "Parous + no pre." ///
3 "Parous + pre." ///
, modify
label values parpre parpre
label define bmi ///
1 "Normal" ///
2 "Over." ///
3 "Obese" ///
, modify
label values bmi bmi
replace prop = 100*prop
replace m1xbu = 100*m1xbu
format %4.2f prop m1xbu
list stratum parpre hyper multiple bmi denom numer prop m1xbu, sepby(parpre) noobs
// Note that stratum 36 with 0 women is not listed
* Number of women
use "datalong.dta", clear
count
// 652,603
* Number of cases of preeclampsia
egen total = total(y)
summarize total
* Percentage of cases of preeclampsia
summarize y
display %2.1f 100*r(mean) "%"

*-------------------------------------------------------------------------------
* STATISTICS PRESENTED IN THE RESULTS SECTION
*-------------------------------------------------------------------------------

* Observed risk in median stratum
use "resultsshort.dta", clear
count
replace prop = 100*prop
replace m1xbu = 100*m1xbu
replace m2xbu = 100*m2xbu
format %4.2f prop m1xbu m2xbu
tabstat prop m1xbu m2xbu, stat(mean min p10 p25 p50 p75 p90 max) format(%2.0f)
* Smallest strata
sort denom
list stratum denom

* Figure 1 - Absolute risk map. Stratum-specific predicted absolute risks of preeclampsia with 95% confidence intervals (circle) obtained from model 1 of the multicategorical multilevel modelling of preeclampsia risk.
use "resultsshort.dta", clear
replace prop = 100*prop
replace m1xbu = 100*m1xbu
replace m1xbuhi = 100*m1xbuhi
replace m1xbulo = 100*m1xbulo
format %2.0f prop m1xbu m1xbuhi m1xbulo
twoway ///
(scatter prop stratum, mcolor(black) msymbol(lgx)) ///
(scatter m1xbu stratum, mcolor(black) msymbol(circle)) ///
(rspike m1xbuhi m1xbulo stratum, lcolor(black)) ///
(scatteri 60 6.5 "Nulliparous", msymbol(none) mlabcolor(black) mlabposition(0)) ///
(scatteri 60 18.5 "Parous without previous preclampsia", msymbol(none) mlabcolor(black) mlabposition(0)) ///
(scatteri 60 30.5 "Parous with previous preclampsia", msymbol(none) mlabcolor(black) mlabposition(0)), ///
ytitle("Model 1: Predicted risk of preeclampsia") ///
yscale(range(-0 60)) ///
ylabel(0(10)60, angle(horizontal)) ///
xtitle("Stratum") ///
xlabel(1(1)36) ///
note("") ///
legend(order(1 "Observed risk" 2 "Predicted risk")) ///
scheme(s1mono) ///
xsize(7.25) ysize(4)
graph export "figure1.png", replace width(1000)

* Table 2 - Compares the estimates and 95% CCI for model 1 and 2, including VPC, PCV and AUC
estimates use "m1.ster"
estimates store m1
estimates use "m2.ster"
estimates store m2
esttab m1 m2, wide b(%9.2f) ci order(_cons) stats(sigma2u sigma2e sigma2r vpc auroc J N) // displays raw point estimates (co-efficients) and confidence intervals.  The fixed effects are analogous to standard regression coefficients and are estimated directly.
esttab m1 m2, wide b(%9.3f) ci order(_cons) stats(sigma2u sigma2e sigma2r vpc auroc J N)
* PCV
estimates use "m1.ster"
scalar m1sigma2u = e(sigma2u)
estimates use "m2.ster"
scalar m2sigma2u = e(sigma2u)
display "PCV = " %3.1f 100*(-(m2sigma2u - m1sigma2u)/m1sigma2u) "%" // The degree to which the between-stratum variance reduces as we move from model 1 to 2 can be expressed by the PCV. 
// The PCV is interpreted as the proportion of the outcome variation between strata, which is attributable to the main effects of the covariates.
display "1 - PCV = " %3.1f 100*(1 - -(m2sigma2u - m1sigma2u)/m1sigma2u) "%" // 1 − PCV measures the proportion of outcome variation between strata attributable to the interaction of effects.

* Table 3 - Overall estimates table (Values are on the additive percentage point scale obtained from model 2)
use "resultsshort.dta", clear
foreach var of varlist m2xbu m2xbulo m2xbuhi m2xb m2xblo m2xbhi m2u m2ulo m2uhi {
replace `var' = 100*`var'
format %5.2f `var'
}
list stratum m2xbu m2xbulo m2xbuhi m2xb m2xblo m2xbhi m2u m2ulo m2uhi

* Figure 2
use "resultsshort.dta", clear
replace prop = 100*prop
replace m2xb = 100*m2xb
replace m2xbu = 100*m2xbu
format %2.0f prop m2xb m2xbu
twoway ///
(scatter m2xb stratum, mcolor(black) msymbol(triangle)) ///
(scatter m2xbu stratum, mcolor(white) msymbol(square) mlcolor(black)) ///
(rspike m2xb m2xbu stratum, lcolor(black)) ///
(scatteri 50 6.5 "Nulliparous", msymbol(none) mlabcolor(black) mlabposition(0)) ///
(scatteri 50 18.5 "Parous without previous preclampsia", msymbol(none) mlabcolor(bla
> ck) mlabposition(0)) ///
(scatteri 50 30.5 "Parous with previous preclampsia", msymbol(none) mlabcolor(black)
> mlabposition(0)) ///
, ///
ytitle("Model 2: Predicted risk of preeclampsia") ///
yscale(range(-8 50)) ///
ylabel(0(10)50, angle(horizontal)) ///
xtitle("Stratum") ///
xlabel(1(1)36) ///
note("") ///
legend(order(1 "Main effects" 2 "Main and interaction effects")) ///
scheme(s1mono) ///
xsize(7.25) ysize(4)
graph export "figure2.png", replace width(1000)
*-------------------------------------------------------------------------------
* STATISTICS PRESENTED IN THE DISCUSSION SECTION
*-------------------------------------------------------------------------------
* Intermediate models
// The VPC in model 1 indicates that about 30% of individual variation to suffer from PE was at the stratum level. The PCV in model 2 shows that 95% of the initial between-stratum differences were due to the main effects of the variables defining the strata. The remaining 5% was therefore due to interaction effects...
use "datalong.dta", clear
mixed y parpre2 parpre3 || stratum:, reml
scalar m2Asigma2u = exp(_b[lns1_1_1:_cons])^2
estimates use "m1.ster"
scalar m1sigma2u = e(sigma2u)
display "PCV = " %3.1f 100*(-(m2Asigma2u - m1sigma2u)/m1sigma2u) "%" // Parity combined with previous PE (parapre) explained 24.8% of the between-stratum variability 

use "datalong.dta", clear
mixed y hyper2|| stratum:, reml 
scalar m2Asigma2u_hyper = exp(_b[lns1_1_1:_cons])^2
estimates use "m1.ster"
scalar m1sigma2u = e(sigma2u)
display "PCV = " %3.1f 100*(-(m2Asigma2u_hyper - m1sigma2u)/m1sigma2u) "%" // chronic hypertension explained 16.1%

// etc.
mixed y multiple2|| stratum:, reml // multiple pregnancy explained 7.2%  
mixed y bmi2 bmi3|| stratum:, reml // BMI did not explain the between-stratum variation

* Identify clusters which exhibit lots or little shrinkage
use "resultsshort.dta", clear
replace m1xbu = 100*m1xbu
replace prop = 100*prop
format %3.0f m1xbu prop
format %3.2f m1s
twoway ///
(function y = x, range(0 50) lcolor(black)) ///
(scatter m1xbu prop if m1s>=.9, msymbol(O) mfcolor(white) mlcolor(black)) ///
(scatter m1xbu prop if m1s<.9, msymbol(O) mcolor(black)) ///
(scatter m1xbu prop if m1s<.9, msymbol(i) mlabel(stratum) mlabpos(12) mlabcol(blue))
> ///
(scatter m1xbu prop if m1s<.9, msymbol(i) mlabel(denom) mlabpos(9) mlabcol(red)) ///
(scatter m1xbu prop if m1s<.9, msymbol(i) mlabel(m1s) mlabpos(3) mlabcol(green)) ///
(scatter m1xbu prop if m1s<0, msymbol(O) mcolor(blue)) ///
(scatter m1xbu prop if m1s<0, msymbol(O) mcolor(red)) ///
(scatter m1xbu prop if m1s<0, msymbol(O) mcolor(green)) ///
, ///
ytitle("Model 1: Predicted stratum risk") ///
yline(20.4) ///
ylabel(0(10)50, angle(horizontal)) ///
xtitle("Observed stratum risk") ///
xlabel(0(10)50) ///
legend(order(7 "Stratum ID" 8 "Stratum size" 9 "Shrinkage factor") col(1) position(3
> )) ///
scheme(s1mono) ///
aspectratio(1) ///
xsize(7.25) ysize(4)

* MLE-robustness check
use "datalong.dta", clear
mixed y parpre2 parpre3 hyper2 multiple2 bmi2 bmi3 || stratum:, mle vce(robust)

********************************************************************************
