// MAIHDA analysis of alcohol_grams_day

clear all
cd "U:\SIMAH\SIMAH_code\nhis\intersectionality"
capture log close
log using "MAIHDA_alc_consumption.smcl", replace
set maxvar 100000
ssc install runmlwin
global MLwiN_path "C:\Program Files\MLwiN v3.05\mlwin.exe"

insheet using "U:\SIMAH\SIMAH_workplace\nhis\intersectionality\cleaned_data\6_nhis_alc_clean_transformed_drinkers.csv", comma clear

* Convert variables from strings to new numeric categorical variable
encode sex, generate(sex_factor)
encode education_3_cats, generate(education_3_cats_factor)
encode age_3_cats, generate(age_3_cats_factor)
encode race_5_cats, generate(race_5_cats_factor)

* Null model
regress transformed_grams_lambda_1 i.sex_factor i.education_3_cats_factor i.age_3_cats_factor i.race_5_cats_factor
estimates store model_1
ovtest // H0: that the model has no omitted variables.  In this case, p-value =0.000 implying the model DOES have missing variables
rvfplot // residuals does not appear uniformly distributed
hettest // checks for heteroscedascity. H0: There is constant variance.  In this case, p-value = 0.0000 implying that there is NOT constant variance
linktest // checks for a link error. significant hat test and significant _hatsq therefore there is a link error

* MAIHDA model

*generate the intersections
egen intersections = group(sex age_3_cats education_3_cats race_5_cats)
egen justoneintersections = tag(intersections) // purely needed for the graphs later and descriptive stats (e.g. tab justoneintersection mean).  Once you have an estimate for the intersection you can just assign it to the one intersection.  Using the indivudal as a proxy for the intersection.

*generate constant for runmlwin
gen cons=1

*generate dummy variables for runmlwin to being able to calculate predicted effects:
foreach var of varlist sex age_3_cats education_3_cats race_5_cats {
tabulate `var', generate(`var'_dum)
}  // Mlwin can't treat data as a categorical variable.  Produces dummy variables manually so that you don't have to use the operator (i).

/*      SEX |      Freq.     Percent        Cum.
------------+-----------------------------------
     Female |    172,637       51.25       51.25
       Male |    164,187       48.75      100.00
------------+-----------------------------------
      Total |    336,824      100.00

 age_3_cats |      Freq.     Percent        Cum.
------------+-----------------------------------
      18-24 |     33,375        9.91        9.91
      25-69 |    269,312       79.96       89.87
        70+ |     34,137       10.13      100.00
------------+-----------------------------------
      Total |    336,824      100.00

   education_3_cats |      Freq.     Percent        Cum.
--------------------+-----------------------------------
   4+ years college |    110,410       32.78       32.78
high school or less |    118,473       35.17       67.95
       some college |    107,941       32.05      100.00
--------------------+-----------------------------------
              Total |    336,824      100.00

           race_5_cats |      Freq.     Percent        Cum.
-----------------------+-----------------------------------
                 Asian |     12,527        3.72        3.72
Black/African American |     38,073       11.30       15.02
       Hispanic, White |     41,843       12.42       27.45
                 Other |     12,859        3.82       31.26
                 White |    231,522       68.74      100.00
-----------------------+-----------------------------------
                 Total |    336,824      100.00
*/

// Multilevel model, null
sort intersections nhispid // an mlwin specific request to sort by the nhis patient ID - doesn't actually mean anything but mlwin wants them in numerical order.

/* The following model syntax is: 
command (runmlwin) 
the response variable (alc_daily_g)
the list of predictors included in the fixed part of the model (here only cons). 
a comma which denotes the end of the fixed part of the model and the start of the random part of the model
The first term after the comma, level2(intersections: cons), specifies the randompart of the model at level 2. 
Within the parentheses, the level identifier, intersections, is specified before the colon while all variables made random at level 2 (here only cons), are specified after the colon. 
The second term after the comma, level1(nhispid: cons), specifies the random part of the model at level 1. 
Again the level identifier (here individual id, nhispid), is specified before the colon while all variables made random at this level (here only cons) are specified after the colon.*/

runmlwin transformed_grams_lambda_1 cons, level2(intersections: cons) level1(nhispid: cons) nopause

/* 
MLwiN 3.05 multilevel model                     Number of obs      =    336824
Normal response model
Estimation algorithm: IGLS

-----------------------------------------------------------
                |   No. of       Observations per Group
 Level Variable |   Groups    Minimum    Average    Maximum
----------------+------------------------------------------
   intersecti~s |       90         47     3742.5      38669
-----------------------------------------------------------

Run time (seconds)   =       5.34
Number of iterations =          3
Log likelihood       =  -706547.7
Deviance             =  1413095.4
------------------------------------------------------------------------------
transforme~1 |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
        cons |   .8053813    .075174   10.71    0.000     .6580431    .9527196
------------------------------------------------------------------------------

------------------------------------------------------------------------------
   Random-effects Parameters |   Estimate   Std. Err.     [95% Conf. Interval]
-----------------------------+------------------------------------------------
Level 2: intersections       |
                   var(cons) |   .4962014     .07551      .3482044    .6441984
-----------------------------+------------------------------------------------
Level 1: nhispid             |
                   var(cons) |   3.881381   .0094593      3.862841    3.899921
------------------------------------------------------------------------------

*/

* Interpretation:  β0 = 0.8053813
*				   between-intersection variance = 0.4962014
*				   between individual variance = 3.881381

estat ic 

/* 
Akaike's information criterion and Bayesian information criterion

-----------------------------------------------------------------------------
       Model |          N   ll(null)  ll(model)      df        AIC        BIC
-------------+---------------------------------------------------------------
           . |    336,824          .  -706547.7       3    1413101    1413134
-----------------------------------------------------------------------------

Note: BIC uses N = number of observations. See [R] BIC note.
 */

/* Calculate the ICC using the display command, where we refer to the parameter estimates by their internal runmlwin names: [RP2]var(cons) is the level 2 variance 
and [RP1]var(cons) is the level 1 variance. */
display [RP2]var(cons)/([RP2]var(cons) + [RP1]var(cons)) // estimate ICC (the same as VPC for simple models) // RESULT 0.11335055
// ~11% of variation at the intersectional level

// NULL model with burn in period and 50,000 iterations (using Bayesian MC simulations & IGLS estimation).  First regression acts as the prior. 
// runmlwin transformed_grams_lambda_1 cons, level2(intersections: cons) level1(nhispid: cons) nopause mcmc(burnin(5000) chain(50000)) initsprevious

/*  

*/

display [RP2]var(cons)/([RP2]var(cons) + [RP1]var(cons)) // ICC/VPC = 

*Mulilevel model, main efffects

// Adding all individual demographic attributes as main effects i.e. sex, race, education and age at level 1, and keeping intersections at level 2
sort intersections nhispid

runmlwin transformed_grams_lambda_1 i.sex_factor i.age_3_cats_factor i.race_5_cats_factor i.education_3_cats_factor cons, level2(intersections: cons) level1(nhispid: cons) nopause
/*  

note: 1b.sex_factor omitted because of collinearity
note: 1b.age_3_cats_factor omitted because of collinearity
note: 1b.race_5_cats_factor omitted because of collinearity
note: 1b.education_3_cats_factor omitted because of collinearity
MLwiN 3.05 multilevel model                     Number of obs      =    336824
Normal response model
Estimation algorithm: IGLS

-----------------------------------------------------------
                |   No. of       Observations per Group
 Level Variable |   Groups    Minimum    Average    Maximum
----------------+------------------------------------------
   intersecti~s |       90         47     3742.5      38669
-----------------------------------------------------------

Run time (seconds)   =       2.70
Number of iterations =          3
Log likelihood       = -706450.39
Deviance             =  1412900.8
------------------------------------------------------------------------------
transforme~1 |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
_2_sex_fac~r |   1.108484   .0509596   21.75    0.000     1.008605    1.208363
_2_age_3_c~r |   -.355418   .0604702   -5.88    0.000    -.4739374   -.2368985
_3_age_3_c~r |  -.6269786   .0653547   -9.59    0.000    -.7550715   -.4988858
_2_race_5_~r |   .1538378   .0823632    1.87    0.062     -.007591    .3152667
_3_race_5_~r |    .214562   .0827221    2.59    0.009     .0524297    .3766944
_4_race_5_~r |   .4035535   .0858901    4.70    0.000     .2352119     .571895
_5_race_5_~r |   .7170748   .0802506    8.94    0.000     .5597864    .8743631
_2_educati~r |  -.2732319    .062808   -4.35    0.000    -.3963334   -.1501305
_3_educati~r |   -.177652   .0632081   -2.81    0.005    -.3015375   -.0537665
        cons |   .4215053   .0837337    5.03    0.000     .2573903    .5856203
------------------------------------------------------------------------------

------------------------------------------------------------------------------
   Random-effects Parameters |   Estimate   Std. Err.     [95% Conf. Interval]
-----------------------------+------------------------------------------------
Level 2: intersections       |
                   var(cons) |   .0489848   .0086038      .0321218    .0658479
-----------------------------+------------------------------------------------
Level 1: nhispid             |
                   var(cons) |   3.881356   .0094591      3.862817    3.899896
------------------------------------------------------------------------------
*/

estat ic
/* 
Akaike's information criterion and Bayesian information criterion
-----------------------------------------------------------------------------
       Model |          N   ll(null)  ll(model)      df        AIC        BIC
-------------+---------------------------------------------------------------
           . |    336,824          .  -706450.4      12    1412925    1413054
-----------------------------------------------------------------------------
Note: BIC uses N = number of observations. See [R] BIC note.
 */

display [RP2]var(cons)/([RP2]var(cons) + [RP1]var(cons)) // ICC = .012463256 (~1%)

// Run main effects model again but with a burn in period and 50000 iterations (i.e. using Bayesian MC simulations & IGLS estimation.  First regression acts as the prior:
//runmlwin transformed_grams_lambda_1 i.sex_factor i.age_3_cats_factor i.race_5_cats_factor i.education_3_cats_factor cons, level2(intersections: cons) level1(nhispid: cons) nopause mcmc(burnin(5000) chain(50000)) initsprevious
/* 

*/
display [RP2]var(cons)/([RP2]var(cons) + [RP1]var(cons)) // ICC = 

//////////////////////////////////////////////////////////////////////////////// GRAPHS
// Outputs of interest: 
// Actual estimate + SE plot (based on the null model)
// multiplicative residuals + SE plot (from the maihda model)
// Best estimate & SE of that (combining the two)

// Add residuals into model formula
runmlwin transformed_grams_lambda_1 i.sex_factor i.age_3_cats_factor i.race_5_cats_factor i.education_3_cats_factor cons, level2(intersections: cons, residuals(u)) level1(nhispid: cons) nopause mcmc(burnin(5000) chain(50000)) initsprevious
/*
 note: 1b.sex_factor omitted because of collinearity
note: 1b.age_3_cats_factor omitted because of collinearity
note: 1b.race_5_cats_factor omitted because of collinearity
note: 1b.education_3_cats_factor omitted because of collinearity
MLwiN 3.05 multilevel model                     Number of obs      =    336824
Normal response model
Estimation algorithm: MCMC

-----------------------------------------------------------
                |   No. of       Observations per Group
 Level Variable |   Groups    Minimum    Average    Maximum
----------------+------------------------------------------
   intersecti~s |       90         47     3742.5      38669
-----------------------------------------------------------

Burnin                     =       5000
Chain                      =      50000
Thinning                   =          1
Run time (seconds)         =        985
Deviance (dbar)            = 1412662.01
Deviance (thetabar)        = 1412582.89
Effective no. of pars (pd) =      79.12
Bayesian DIC               = 1412741.13
------------------------------------------------------------------------------
transforme~1 |      Mean    Std. Dev.     ESS     P       [95% Cred. Interval]
-------------+----------------------------------------------------------------
_2_sex_fac~r |   1.107584   .0553593      396   0.000       1.0007    1.217205
_2_age_3_c~r |  -.3570061   .0650923      407   0.000    -.4886885   -.2334804
_3_age_3_c~r |  -.6286679   .0699917     1395   0.000    -.7650286   -.4878931
_2_race_5_~r |   .1469592   .0877659     1341   0.046     -.025017    .3180986
_3_race_5_~r |   .2125448   .0889873     1266   0.008     .0384904    .3890161
_4_race_5_~r |   .3999166   .0928043     2019   0.000      .220104    .5799714
_5_race_5_~r |   .7094044   .0850044      344   0.000     .5422838    .8789217
_2_educati~r |  -.2727321   .0690917      401   0.000    -.4072608   -.1369541
_3_educati~r |  -.1790866   .0688034      340   0.007    -.3092239   -.0377766
        cons |   .4275736   .0901447     1122   0.000     .2514679    .6037892
------------------------------------------------------------------------------

------------------------------------------------------------------------------
   Random-effects Parameters |     Mean   Std. Dev.   ESS     [95% Cred. Int]
-----------------------------+------------------------------------------------
Level 2: intersections       |
                   var(cons) |  .0576116  .0107146  19495   .0401049  .0816213
-----------------------------+------------------------------------------------
Level 1: nhispid             |
                   var(cons) |   3.88131  .0094981  49621   3.862799  3.899813
------------------------------------------------------------------------------
*/

///////////////////////////////////////////////////////////////////////////////// MANUALLY SAVE full output as dataframe called "MAIHDA_data_post_analysis"

// Use filter justoneintersections = 1 to see one record for each intersections.  Can then see the mean, sample standard error etc. for that intersection.
keep if justoneintersections==1
sort intersections

//////////////////////////////////////////////////////////////////////////////// MANUALLY SAVE as dataframe called "MAIHDA_justoneintersections"

////////////////
//////////////////
/////////////////
///////////////// stop here for now


// Following code can be used to plot graphs in STATA, but for now trialling plotting in R

* gen graphorder = [_n] // if want to order the graph by intersectional groups
* Or if want to order by outputs use:
egen u0rank = rank(u0)
serrbar u0 u0se u0rank, scale(1.96) yline(0)

/* serrbar uses these data means, standard deviations or standard errors, and an xvar to create a standard error bar chart. 
The means are plotted against xvar, and error bars around the means have a width determined by the standard deviation or standard error. 
The following plot shows the mean residuals for the full model (intersections plus main effects)*/

serrbar u0 u0se graphorder, scale(1.96) ylabel(-0.1(0.1)0.1, ang(90)) ytitle("daily_grams_alc", size (8pt)) scheme(plottig) xtitle("18-24           25-59         60+               l18-24          25-59         60+         18-24          25-59         60+", size(small)margin(small), ) xlabel( ///
1 "white" ///
2 "black" ///
3 "hispanic" ///
4 "other" ///
5 "white" ///
6 "black" ///
7 "hispanic" ///
8 "other" ///
9 "white" ///
10 "black" ///
11 "hispanic" ///
12 "other" ///
13 "white" ///
14 "black" ///
15 "hispanic" ///
16 "other" ///
17 "medium wealth" ///
18 "high wealth" ///
19
20
21
22
23
24
25
26
27
28
29
30
31
32
33
34
35
36
37
38
39
40
41
42
43
44
45
46
47
48
49
50
51
52
53
54
55
56
57
58
59
60
61
62
63
64
65
66
67
68
69
70
71
72
, angle(90) labsize(vsmall)) xline(3.5 6.5 9.5 12.5 15.5 18.5, lstyle(major_grid) lwidth(medthick)) title("Multiplicative effects of intersections on alc_daily_g", size (12pt))
graph export alc_daily_g_residuals.tif, replace

drop(u0)
drop(u0se)

/* Graph 2 - To show mean alc_daily_g for each intersection
sort intersections id
runmlwin wlk_speed_WV9 sex_factor sex_factor_dum1 qual3_dum1 qual3_dum2 qual3_dum3 total_wealth_3_dum1 total_wealth_3_dum2 total_wealth_3_dum3 cons, level2(intersections: cons) level1(id: cons) nopause
estat ic
display [RP2]var(cons)/([RP2]var(cons) + [RP1]var(cons)) //estimate icc/vpc
runmlwin wlk_speed_WV9 sex_dum1 sex_dum2 qual3_dum1 qual3_dum2 qual3_dum3 total_wealth_3_dum1 total_wealth_3_dum2 total_wealth_3_dum3 cons, level2(intersections: cons, residuals(u)) level1(id: cons)  nopause mcmc(burnin(5000) chain(50000)) initsprevious
display [RP2]var(cons)/([RP2]var(cons) + [RP1]var(cons)) //estimate icc/vpc

*predicted effects for graph  /// this is the code to run one line at a time to see what is coming out
predict yhat_wlk_speed_WV9 // mean for each intersection
predict yhat_wlk_speed_WV9_se, stdp // as the standard error of the predicted expected value for the observation's covariate pattern.
gen mean=yhat_wlk_speed_WV9+u0 // adding in multiplicative residual (should slightly adjust them)
gen se=(sqrt((yhat_wlk_speed_WV9_se*yhat_wlk_speed_WV9_se)+(u0se*u0se)))/2  

*to produce graphs
drop intersections justoneintersections
fillin sex qual3 total_wealth_3
egen intersections = group(sex qual3 total_wealth_3)
egen justoneintersections = tag(intersections)
keep if justoneintersections==1
sort intersections

separate mean, by(intersections) veryshortlabel

gen upper= mean + (1.96*se)
gen lower= mean - (1.96*se) 
separate upper, by(intersections) veryshortlabel
separate lower, by(intersections) veryshortlabel 

levelsof intersections, local(INTERSECTIONS)

// Intersectional graph:
twoway scatter mean intersections, title("Walking speed by intersections (mean and SE)", size(medium)) ytitle("Usual walking speed, m/s") scheme(plottig) yscale(range(0.5 1)) ylabel(0.0(0.1)1.5)  xlabel(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18, nolabel notick) xtitle("low edu.          medium edu.         high edu.               low edu.         medium edu.         high edu.", size(small)margin(small), ) xmlabel(1 "low wealth" 2 "medium wealth" 3 "high wealth" 4 "low wealth" 5 "medium wealth" 6 "high wealth" 7 "low wealth" 8 "medium wealth" 9 "high wealth" 10 "low wealth" 11 "medium wealth" 12 "high wealth" 13 "low wealth" 14 "medium wealth" 15 "high wealth" 16 "low wealth" 17 "medium wealth" 18 "high wealth",  nogrid notick angle(90)) xline(3.5 6.5 9.5 12.5 15.5 18.5, lstyle(major_grid) lwidth(medthick)) || rcap upper lower intersections, lc(black) legend(off) */

capture log close


// Sensitivity analysis:

// 1. Running a standard main effects regression with/without sample weighted data
*? How to actually use the sample weights?

// 2. Using income instead of education as measure of SES

// 3. imputing missing education data?