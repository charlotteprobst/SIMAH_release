// MAIHDA analysis of alcohol_grams_day

clear all
cd "C:\Users\cmp21seb\Documents\SIMAH\SIMAH_code\nhis\intersectionality"
capture log close // will close a log if any is open and do nothing if no log is open.
* log using "MAIHDA_alc_consumption.smcl", replace // Generate a log file if wanting to record all your Stata commands and output in a given session, with the exception of graphs.
set maxvar 100000 // set the maximum number of allowed variables in a dataset
* ssc install runmlwin // install mlwin if not already installed
global MLwiN_path "C:\Program Files\MLwiN v3.05\mlwin.exe"

insheet using "C:\Users\cmp21seb\Documents\SIMAH\SIMAH_workplace\nhis\intersectionality\cleaned_data\new Spec August 2023\grams\grams_data_pre_maihda_drinkers.csv", comma clear // Read in the data in csv format

* Convert variables from strings to new numeric categorical variable
encode sex, generate(sex_factor)
encode education_3_cats, generate(education_3_cats_factor)
encode age_diaz, generate(age_diaz_factor)
encode race_6_cats, generate(race_6_cats_factor)

*generate dummy variables for runmlwin to be able to calculate predicted effects:
foreach var of varlist sex age_diaz education_3_cats race_6_cats year {
tabulate `var', generate(`var'_dum)
} 

* View the distribution of the raw variable of interest (grams of alcohol consumed per day, capped at 200 grams)
histogram alc_daily_g_capped_200

* View the distribution of the outcome variable post log transformation
histogram capped_daily_grams_log

* Run a simple multiple regression model on the transformed data
regress capped_daily_grams_log i.sex_factor i.education_3_cats_factor i.age_diaz_factor i.race_6_cats_factor i.year
estimates store model_Nov_23_simple
ovtest // H0: that the model has no omitted variables.  In this case, p-value =0.000 implying the model DOES have missing variables
rvfplot // residuals does not appear uniformly distributed
hettest // checks for heteroscedascity. H0: There is constant variance.  In this case, p-value = 0.0000 implying that there is NOT constant variance
linktest // checks for a link error. significant hat test and significant _hatsq therefore there is a link error

* MAIHDA model

*generate constant for runmlwin
gen cons=1

// Multilevel model, null
sort intersections nhispid // an mlwin specific request to sort by the nhis ID

/* The following model syntax is: 
command (runmlwin) 
the response variable (alc_daily_g)
the list of predictors included in the fixed part of the model (here only cons). 
a comma which denotes the end of the fixed part of the model and the start of the random part of the model
The first term after the comma, level2(intersections: cons), specifies the randompart of the model at level 2. 
Within the parentheses, the level identifier, intersections, is specified before the colon while all variables made random at level 2 (here only cons), are specified after the colon. 
The second term after the comma, level1(nhispid: cons), specifies the random part of the model at level 1. 
Again the level identifier (here individual id, nhispid), is specified before the colon while all variables made random at this level (here only cons) are specified after the colon.*/

runmlwin capped_daily_grams_log i.year cons, level2(intersections: cons) level1(nhispid: cons) nopause

/* 

note: 2000b.year omitted because of collinearity
MLwiN 3.05 multilevel model                     Number of obs      =    321242
Normal response model
Estimation algorithm: IGLS

-----------------------------------------------------------
                |   No. of       Observations per Group
 Level Variable |   Groups    Minimum    Average    Maximum
----------------+------------------------------------------
   intersect~ns |      108          2     2974.5      32671
-----------------------------------------------------------

Run time (seconds)   =       5.77
Number of iterations =          3
Log likelihood       = -633778.08
Deviance             =  1267556.2
------------------------------------------------------------------------------
capped_dai~g |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  _2001_year |   .0701024   .0188631    3.72    0.000     .0331314    .1070734
  _2002_year |    .052065   .0192158    2.71    0.007     .0144027    .0897273
  _2003_year |   .0645212    .019196    3.36    0.001     .0268976    .1021447
  _2004_year |   .0235003   .0190947    1.23    0.218    -.0139246    .0609252
  _2005_year |   .1200333   .0190359    6.31    0.000     .0827236     .157343
  _2006_year |   .0966096   .0205221    4.71    0.000      .056387    .1368321
  _2007_year |   .0988286   .0206721    4.78    0.000      .058312    .1393452
  _2008_year |   .1137755   .0207231    5.49    0.000     .0731589    .1543921
  _2009_year |   .1406091   .0193293    7.27    0.000     .1027245    .1784938
  _2010_year |   .1023531    .019465    5.26    0.000     .0642024    .1405038
  _2011_year |   .0950643   .0184552    5.15    0.000     .0588928    .1312357
  _2012_year |   .1443654   .0183563    7.86    0.000     .1083876    .1803431
  _2013_year |   .1574768   .0182886    8.61    0.000     .1216318    .1933218
  _2014_year |    .181954   .0180081   10.10    0.000     .1466589    .2172492
  _2015_year |   .1360354   .0183404    7.42    0.000     .1000888    .1719819
  _2016_year |   .2118497   .0182922   11.58    0.000     .1759976    .2477019
  _2017_year |   .2162925   .0191636   11.29    0.000     .1787325    .2538525
  _2018_year |   .1963213   .0194264   10.11    0.000     .1582462    .2343963
        cons |    .559601    .065171    8.59    0.000     .4318682    .6873339
------------------------------------------------------------------------------

------------------------------------------------------------------------------
   Random-effects Parameters |   Estimate   Std. Err.     [95% Conf. Interval]
-----------------------------+------------------------------------------------
Level 2: intersections       |
                   var(cons) |   .4186289   .0590118      .3029678    .5342899
-----------------------------+------------------------------------------------
Level 1: nhispid             |
                   var(cons) |   3.023539   .0075455       3.00875    3.038328
------------------------------------------------------------------------------
*/

* Interpretation:  β0 = -1.313
*				   between-intersection variance = 0.42
*				   between individual variance = 3.02

estat ic 

/* Calculate the ICC using the display command, where we refer to the parameter estimates by their internal runmlwin names: [RP2]var(cons) is the level 2 variance 
and [RP1]var(cons) is the level 1 variance. */
display [RP2]var(cons)/([RP2]var(cons) + [RP1]var(cons)) // estimate ICC (the same as VPC for simple models) // RESULT .16080049
// .12 i.e. ~12% of variation at the intersectional level

* Generate predicted effects for graphs (BASED ON NULL MODEL, with residuals saved)
runmlwin capped_daily_grams_log i.year cons, level2(intersections: cons, residuals(u)) level1(nhispid: cons) nopause mcmc(burnin(5000) chain(50000)) initsprevious

// Save the model estimates:
estimates save "C:\Users\cmp21seb\Documents\SIMAH\SIMAH_workplace\nhis\intersectionality\models\new Spec August 2023\grams\STATA\null_grams_model_coefs_Nov_drinkers_only", replace

*Rename the dummy variables to match the model coefficient names
rename year_dum1 _2001_year
rename year_dum2 _2002_year
rename year_dum3 _2003_year
rename year_dum4 _2004_year
rename year_dum5 _2005_year
rename year_dum6 _2006_year
rename year_dum7 _2007_year
rename year_dum8 _2008_year
rename year_dum9 _2009_year
rename year_dum10 _2010_year
rename year_dum11 _2011_year
rename year_dum12 _2012_year
rename year_dum13 _2013_year
rename year_dum14 _2014_year
rename year_dum15 _2015_year
rename year_dum16 _2016_year
rename year_dum17 _2017_year
rename year_dum18 _2018_year
rename sex_dum1 _1_sex_factor
rename sex_dum2 _2_sex_factor
rename age_diaz_dum1 _1_age_diaz_factor
rename age_diaz_dum2 _2_age_diaz_factor
rename age_diaz_dum3 _3_age_diaz_factor
rename race_6_cats_dum1 _1_race_6_cats_factor
rename race_6_cats_dum2 _2_race_6_cats_factor
rename race_6_cats_dum3 _3_race_6_cats_factor
rename race_6_cats_dum4 _4_race_6_cats_factor
rename race_6_cats_dum5 _5_race_6_cats_factor
rename race_6_cats_dum6 _6_race_6_cats_factor
rename education_3_cats_dum1 _1_education_3_cats_factor
rename education_3_cats_dum2 _2_education_3_cats_factor
rename education_3_cats_dum3 _3_education_3_cats_factor

predict yhat_capped_daily_grams_log
predict yhat_capped_daily_grams_log_se, stdp
gen null_mean=yhat_capped_daily_grams_log+u0
gen null_se=(sqrt((yhat_capped_daily_grams_log_se*yhat_capped_daily_grams_log_se)+(u0se*u0se)))/2

*Back transform estimates
generate null_grams_yhat_back_transformed = exp(null_mean)
generate null_grams_se_back_transformed = exp(null_se)

// Use filter justoneintersections = 1 to see one record for each intersections.  Can then see the mean, sample standard error etc. for that intersection.
 * NEED TO CONSIDER ADDING IN 2018 FOR THE LEVELLING YEAR

*generate the intersections
egen justoneintersections = tag(intersections) // purely needed for the graphs later and descriptive stats (e.g. tab justoneintersection mean).  Once you have an estimate for the intersection you can just assign it to the one intersection.  Using the indivudal as a proxy for the intersection.

keep if justoneintersections==1
sort intersections

//////////////////////////////////////////////////////////////////////////////// MANUALLY SAVE as dataframe called "MAIHDA_justoneintersections"

*List the 5 intersections with the highest estimated grams
gsort -null_grams_yhat_back_transformed
list intersectional_names null_grams_yhat_back_transformed in 1/5

/*
     +----------------------------------------------+
     |              intersectional_names   n~yhat~d |
     |----------------------------------------------|
  1. |         Male 21-24 White 4+ years college   3.633141 |
  2. |             Male 21-24 White some college   2.297322 |
  3. | Male 21-24 Multiple race 4+ years college   1.937925 |
  4. |             Male 21-24 AI/AN some college    1.87462 |
  5. |     Male 21-24 Multiple race some college   1.728926 |
     +------------------------------------------------------+


*/

*List the 5 intersections with the lowest estimated grams
gsort null_grams_yhat_back_transformed
list intersectional_names null_grams_yhat_back_transformed in 1/5

/*
     +----------------------------------------------------------+
     |                          intersectional_names   n~yhat~d |
     |----------------------------------------------------------|
  1. |          Female 60+ Asian high school or less   .0287717 |
  2. |          Female 60+ AI/AN high school or less   .0316211 |
  3. |          Female 60+ Black high school or less   .0360624 |
  4. | Female 60+ Hispanic White high school or less   .0363131 |
  5. |             Female 60+ Asian 4+ years college   .0469398 |
     +----------------------------------------------------------+

*/

*Mulilevel model, main efffects

// Adding all individual demographic attributes as main effects i.e. sex, race, education and age at level 1, and keeping intersections at level 2
sort intersections nhispid

runmlwin capped_daily_grams_log i.year i.sex_factor i.age_diaz_factor i.race_6_cats_factor i.education_3_cats_factor cons, level2(intersections: cons) level1(nhispid: cons) nopause
/*  
note: 1b.sex_factor omitted because of collinearity
note: 1b.age_diaz_factor omitted because of collinearity
note: 1b.race_6_cats_factor omitted because of collinearity
note: 1b.education_3_cats_factor omitted because of collinearity
MLwiN 3.05 multilevel model                     Number of obs      =    516439
Normal response model
Estimation algorithm: IGLS

-----------------------------------------------------------
                |   No. of       Observations per Group
 Level Variable |   Groups    Minimum    Average    Maximum
----------------+------------------------------------------
   intersect~ns |      108          3     4781.8      40193
-----------------------------------------------------------

Run time (seconds)   =      17.41
Number of iterations =          4
Log likelihood       = -1201764.9
Deviance             =  2403529.7
------------------------------------------------------------------------------
capped_dai~g |      Coef.   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
  _2001_year |   .0769575   .0208393    3.69    0.000     .0361131    .1178019
  _2002_year |   .0470821   .0212011    2.22    0.026     .0055288    .0886355
  _2003_year |   .0140936   .0210665    0.67    0.503     -.027196    .0553832
  _2004_year |   .0022209   .0209681    0.11    0.916    -.0388759    .0433176
  _2005_year |   .0612455   .0209146    2.93    0.003     .0202536    .1022374
  _2006_year |   .0523665   .0224756    2.33    0.020     .0083152    .0964179
  _2007_year |    .084406   .0227141    3.72    0.000     .0398872    .1289248
  _2008_year |   .2102829   .0230703    9.11    0.000     .1650659    .2554999
  _2009_year |   .2672156   .0215603   12.39    0.000     .2249582     .309473
  _2010_year |   .2325841   .0216828   10.73    0.000     .1900867    .2750816
  _2011_year |   .2527457    .020607   12.27    0.000     .2123568    .2931346
  _2012_year |   .2349584    .020411   11.51    0.000     .1949536    .2749632
  _2013_year |   .2760837   .0203907   13.54    0.000     .2361187    .3160486
  _2014_year |   .3080964   .0201127   15.32    0.000     .2686762    .3475165
  _2015_year |   .2791277   .0205057   13.61    0.000     .2389373    .3193181
  _2016_year |   .4013543   .0206302   19.45    0.000     .3609199    .4417886
  _2017_year |   .4460298   .0217703   20.49    0.000     .4033609    .4886988
  _2018_year |   .4119537   .0220104   18.72    0.000     .3688141    .4550934
_2_sex_fac~r |   1.115836   .0535527   20.84    0.000     1.010874    1.220797
_2_age_dia~r |  -.5695335    .066995   -8.50    0.000    -.7008413   -.4382257
_3_age_dia~r |  -1.611348   .0683188  -23.59    0.000     -1.74525   -1.477446
_2_race_6_~r |  -.5452759   .1056777   -5.16    0.000    -.7524004   -.3381515
_3_race_6_~r |  -.1596229   .1041751   -1.53    0.125    -.3638024    .0445567
_4_race_6_~r |   .0953984   .1043161    0.91    0.360    -.1090575    .2998543
_5_race_6_~r |   .5290136   .1094228    4.83    0.000     .3145489    .7434784
_6_race_6_~r |   .8689229   .1033201    8.41    0.000     .6664193    1.071427
_2_educati~r |  -.8336982   .0662994  -12.57    0.000    -.9636425   -.7037538
_3_educati~r |  -.3020921   .0664403   -4.55    0.000    -.4323127   -.1718715
        cons |  -.8911528   .1114808   -7.99    0.000    -1.109651   -.6726545
------------------------------------------------------------------------------

------------------------------------------------------------------------------
   Random-effects Parameters |   Estimate   Std. Err.     [95% Conf. Interval]
-----------------------------+------------------------------------------------
Level 2: intersections       |
                   var(cons) |   .0632903   .0101753      .0433472    .0832335
-----------------------------+------------------------------------------------
Level 1: nhispid             |
                   var(cons) |   6.144866   .0120937      6.121163    6.168569
------------------------------------------------------------------------------
*/

estat ic
/* 
Akaike's information criterion and Bayesian information criterion

-----------------------------------------------------------------------------
       Model |          N   ll(null)  ll(model)      df        AIC        BIC
-------------+---------------------------------------------------------------
           . |    516,439          .   -1201765      31    2403592    2403937
-----------------------------------------------------------------------------

Note: BIC uses N = number of observations. See [R] BIC note.
 */

display [RP2]var(cons)/([RP2]var(cons) + [RP1]var(cons)) // ICC = .01019471 (~1%)

// Run main effects model again but with a burn in period and 50000 iterations (i.e. using Bayesian MC simulations & IGLS estimation.  First regression acts as the prior:
//runmlwin transformed_grams_lambda_1 i.sex_factor i.age_3_cats_factor i.race_5_cats_factor i.education_3_cats_factor cons, level2(intersections: cons) level1(nhispid: cons) nopause mcmc(burnin(5000) chain(50000)) initsprevious
// Adding residuals into formula

runmlwin capped_daily_grams_log i.year i.sex_factor i.age_diaz_factor i.race_6_cats_factor i.education_3_cats_factor cons, level2(intersections: cons, residuals(u)) level1(nhispid: cons) nopause mcmc(burnin(5000) chain(50000)) initsprevious

/*

-----------------------------------------------------------
                |   No. of       Observations per Group
 Level Variable |   Groups    Minimum    Average    Maximum
----------------+------------------------------------------
   intersect~ns |      108          3     4781.8      40193
-----------------------------------------------------------

Burnin                     =       5000
Chain                      =      50000
Thinning                   =          1
Run time (seconds)         =       2490
Deviance (dbar)            = 2403264.52
Deviance (thetabar)        = 2403154.00
Effective no. of pars (pd) =     110.53
Bayesian DIC               = 2403375.05
------------------------------------------------------------------------------
capped_dai~g |      Mean    Std. Dev.     ESS     P       [95% Cred. Interval]
-------------+----------------------------------------------------------------
  _2001_year |   .0768384   .0208177    50553   0.000     .0362412    .1173918
  _2002_year |   .0470107   .0212915    50097   0.014     .0050922    .0888777
  _2003_year |   .0140626   .0210767    50951   0.252    -.0271611    .0551896
  _2004_year |   .0021157   .0210073    50669   0.461    -.0389953    .0431144
  _2005_year |   .0612035   .0209178    49869   0.001     .0202288    .1023529
  _2006_year |   .0524137   .0224179    51287   0.009     .0087099    .0963507
  _2007_year |   .0843696   .0227315    51187   0.000     .0399115    .1287825
  _2008_year |   .2101133   .0230594    49994   0.000     .1650212    .2548308
  _2009_year |   .2670343   .0215893    49260   0.000     .2243871    .3091744
  _2010_year |   .2324473   .0217144    50070   0.000     .1892963    .2748176
  _2011_year |   .2526532    .020602    50515   0.000     .2123784    .2930467
  _2012_year |   .2348358   .0204813    50499   0.000     .1942974    .2744336
  _2013_year |   .2759861   .0204017    50207   0.000     .2362082    .3160967
  _2014_year |   .3078645   .0201834    50068   0.000     .2684044    .3472079
  _2015_year |   .2789784   .0205403    49182   0.000     .2386993    .3194152
  _2016_year |   .4012724   .0206807    51351   0.000     .3605873    .4419128
  _2017_year |   .4459282   .0218475    49564   0.000     .4032587    .4880984
  _2018_year |    .411994   .0220243    48654   0.000     .3686713    .4549135
_2_sex_fac~r |   1.114692   .0595378      350   0.000     .9928735    1.228277
_2_age_dia~r |  -.5762975   .0705365      573   0.000    -.7177798    -.441193
_3_age_dia~r |  -1.616592   .0731926      863   0.000    -1.762028   -1.473922
_2_race_6_~r |  -.5452464   .1112607     3381   0.000    -.7638045   -.3281829
_3_race_6_~r |  -.1608009   .1109666     1414   0.073    -.3760469    .0565014
_4_race_6_~r |   .0966874   .1105423     1511   0.190    -.1209104    .3151293
_5_race_6_~r |   .5281557   .1167585     6272   0.000     .2998592    .7585151
_6_race_6_~r |   .8598847   .1086904      448   0.000     .6448781    1.072734
_2_educati~r |   -.828752   .0708312      383   0.000    -.9657511   -.6932634
_3_educati~r |  -.2998275   .0730271      389   0.000    -.4412084   -.1542385
        cons |  -.8870117   .1174622     1549   0.000    -1.115358    -.655238
------------------------------------------------------------------------------

------------------------------------------------------------------------------
   Random-effects Parameters |     Mean   Std. Dev.   ESS     [95% Cred. Int]
-----------------------------+------------------------------------------------
Level 2: intersections       |
                   var(cons) |  .0743591  .0131785  13966   .0523989  .1035269
-----------------------------+------------------------------------------------
Level 1: nhispid             |
                   var(cons) |  6.145059  .0120672  49090   6.121499   6.16858
------------------------------------------------------------------------------

*/

// Save the model estimates:
estimates save "C:\Users\cmp21seb\Documents\SIMAH\SIMAH_workplace\nhis\intersectionality\models\new Spec August 2023\grams\STATA\full_grams_model_coefs_Aug"

// Re-estimate VPC
display [RP2]var(cons)/([RP2]var(cons) + [RP1]var(cons)) // ICC = 0.011

//////////////////////////////////////////////////////////////////////////////// GRAPHS
// Outputs of interest: 
// Actual estimate + SE plot (based on the null model)
// multiplicative residuals + SE plot (from the maihda model)
// Best estimate & SE of that (combining the two)


///////////////////////////////////////////////////////////////////////////////// MANUALLY SAVE full output as dataframe called "MAIN grams, data post analysis, Aug 2023"

// Use filter justoneintersections = 1 to see one record for each intersections.  Can then see the mean, sample standard error etc. for that intersection.
keep if justoneintersections==1
sort intersections

//////////////////////////////////////////////////////////////////////////////// MANUALLY SAVE as dataframe called "MAIHDA_justoneintersections"

// Generate predicted effects for graphs (BASED ON FULL MODEL)

*Rename the dummy variables to match the model coefficient names

rename year_dum1 _2001_year
rename year_dum2 _2002_year
rename year_dum3 _2003_year
rename year_dum4 _2004_year
rename year_dum5 _2005_year
rename year_dum6 _2006_year
rename year_dum7 _2007_year
rename year_dum8 _2008_year
rename year_dum9 _2009_year
rename year_dum10 _2010_year
rename year_dum11 _2011_year
rename year_dum12 _2012_year
rename year_dum13 _2013_year
rename year_dum14 _2014_year
rename year_dum15 _2015_year
rename year_dum16 _2016_year
rename year_dum17 _2017_year
rename year_dum18 _2018_year
rename sex_dum1 _1_sex_factor
rename sex_dum2 _2_sex_factor
rename age_diaz_dum1 _1_age_diaz_factor
rename age_diaz_dum2 _2_age_diaz_factor
rename age_diaz_dum3 _3_age_diaz_factor
rename race_6_cats_dum1 _1_race_6_cats_factor
rename race_6_cats_dum2 _2_race_6_cats_factor
rename race_6_cats_dum3 _3_race_6_cats_factor
rename race_6_cats_dum4 _4_race_6_cats_factor
rename race_6_cats_dum5 _5_race_6_cats_factor
rename race_6_cats_dum6 _6_race_6_cats_factor
rename education_3_cats_dum1 _1_education_3_cats_factor
rename education_3_cats_dum2 _2_education_3_cats_factor
rename education_3_cats_dum3 _3_education_3_cats_factor

predict yhat_capped_daily_grams_log
predict yhat_capped_daily_grams_log_se, stdp
gen mean=yhat_capped_daily_grams_log+u0
gen se=(sqrt((yhat_capped_daily_grams_log_se*yhat_capped_daily_grams_log_se)+(u0se*u0se)))/2

*Back transform estimates
generate grams_yhat_back_transformed = exp(mean)
generate grams_se_back_transformed = exp(se)

*List the 5 intersections with the highest estimated grams
gsort -grams_yhat_back_transformed
list intersectional_names grams_yhat_back_transformed in 1/5

/*
     +------------------------------------------------------+
     |                      intersectional_names   g~yhat~d |
     |------------------------------------------------------|
  1. |         Male 21-24 White 4+ years college   3.637997 |
  2. |             Male 21-24 White some college   2.305121 |
  3. | Male 21-24 Multiple race 4+ years college   2.218963 |
  4. |     Male 21-24 Multiple race some college   1.786183 |
  5. |       Female 21-24 White 4+ years college   1.625186 |
     +------------------------------------------------------+
*/

*List the 5 intersections with the lowest estimated grams
gsort grams_yhat_back_transformed
list intersectional_names grams_yhat_back_transformed in 1/5

/*
     +----------------------------------------------------------+
     |                          intersectional_names   g~yhat~d |
     |----------------------------------------------------------|
  1. |          Female 60+ Asian high school or less    .028238 |
  2. |          Female 60+ AI/AN high school or less   .0321122 |
  3. |          Female 60+ Black high school or less   .0359688 |
  4. | Female 60+ Hispanic White high school or less   .0363302 |
  5. |                 Female 60+ Asian some college   .0465944 |
     +----------------------------------------------------------+
*/

