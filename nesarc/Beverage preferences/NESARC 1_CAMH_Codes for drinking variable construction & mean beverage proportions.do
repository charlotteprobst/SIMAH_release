
//////////////////Wave 1 drinking variables

//current drinker=havign 1 drink, past year

gen w1curdyr1=s2aq3
replace w1curdyr1=1 if s2aq3==1   
replace w1curdyr1=. if s2aq3==9

//Any info about beverage type

gen w1bevany=0
replace  w1bevany=1 if s2aq4a==1|s2aq5a==1|s2aq6a==1|s2aq7a==1

//Beverage-specific frequencies and quantities
gen w1coolusualf=s2aq4b 
replace w1coolusualf=. if w1coolusualf==99
tab w1coolusualf

gen w1coolusualq=s2aq4d  
replace w1coolusualq=. if w1coolusualq==99 
tab w1coolusualq

gen w1cool5plusf=s2aq4g 
replace w1cool5plusf=. if w1cool5plusf==99
tab w1cool5plusf

gen w1coolmaxf=s2aq4f  
replace w1coolmaxf=. if w1coolmaxf==99
tab w1coolmaxf

gen w1coolmaxq=s2aq4e
replace w1coolmaxq=. if w1coolmaxq==99 
tab w1coolmaxq  

gen w1beerusualf=s2aq5b 
replace w1beerusualf=. if w1beerusualf==99
tab w1beerusualf

gen w1beer5plusf=s2aq5g 
replace w1beer5plusf=. if w1beer5plusf==99
tab w1beer5plusf

gen w1beerusualq=s2aq5d  
replace w1beerusualq=. if w1beerusualq==99 

gen w1beermaxf=s2aq5f  
replace w1beermaxf=. if w1beermaxf==99
tab w1beermaxf

gen w1beermaxq=s2aq5e  
replace w1beermaxq=. if w1beermaxq==99
tab w1beermaxq

gen w1wineusualf=s2aq6b 
replace w1wineusualf=. if w1wineusualf==99 
tab w1wineusualf

gen w1wineusualq=s2aq6d  
replace w1wineusualq=. if w1wineusualq==99 
tab w1wineusualq

gen w1winemaxf=s2aq6f  
replace w1winemaxf=. if w1winemaxf==99 
tab w1winemaxf

gen w1wine5plusf=s2aq6g 
replace w1wine5plusf=. if w1wine5plusf==99
tab w1wine5plusf

gen w1winemaxq=s2aq6e  
replace w1winemaxq=. if w1winemaxq==99
tab w1winemaxq

gen w1spiritsusualf=s2aq7b 
replace w1spiritsusualf=. if w1spiritsusualf==99 
tab w1spiritsusualf

gen w1spiritsusualq=s2aq7d  
replace w1spiritsusualq=. if w1spiritsusualq==99 
tab w1spiritsusualq

gen w1spirits5plusf=s2aq7g 
replace w1spirits5plusf=. if w1spirits5plusf==99
tab w1spirits5plusf

gen w1spiritsmaxf=s2aq7f  
replace w1spiritsmaxf=. if w1spiritsmaxf==99 
tab w1spiritsmaxf

gen w1spiritsmaxq=s2aq7e  
replace w1spiritsmaxq=. if w1spiritsmaxq==99  
tab w1spiritsmaxq

//converting categorical frequency to numerical (number of days per year), W1

gen w1cusualfd=w1coolusualf
recode w1cusualfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.
tab w1cusualfd

gen w1busualfd=w1beerusualf
recode w1busualfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.

gen w1wusualfd=w1wineusualf
recode w1wusualfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.

gen w1susualfd=w1spiritsusualf
recode w1susualfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.

gen w1cmaxfd=w1coolmaxf
recode w1cmaxfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.

gen w1bmaxfd=w1beermaxf
recode w1bmaxfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.

gen w1wmaxfd=w1winemaxf
recode w1wmaxfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.

gen w1smaxfd=w1spiritsmaxf
recode w1smaxfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.
tab w1smaxfd
tab w1spiritsmaxf

gen w1c5plusfd=w1cool5plusf
recode w1c5plusfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.
tab w1cool5plusf
tab w1c5plusfd

gen w1w5plusfd=w1wine5plusf
recode w1w5plusfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.

gen w1b5plusfd=w1beer5plusf
recode w1b5plusfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.

gen w1s5plusfd=w1spirits5plusf
recode w1s5plusfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.

//Creating variables for drink sizes in ounce, W1_Reflecting Bill's recodes

gen w1csizeounce=s2aq4cr 
recode w1csizeounce 2=1.5 3=2 4=3 5=4 6=5 7=6 8=7 9=8 10=9 11=10 13=15 14=16 15=18 16=20 17=23.5 18=32 19=40 20=40 21=1.5 22=3 23=4.5 24=5 25=1.7 26=6.3 ///
   27=12.7 28=25.4 29=16.9 30=33.8 31=40 32=39.2 33=40 34=40 35=6.8 36=16 37=25.6 38=32 39=40 41=16 42=40 ///
   43=40 44=8 45=12 46=16 47=12 99=12
replace w1csizeounce=0 if w1csizeounce==. & w1bevany==1 
   
gen w1bsizeounce=s2aq5cr
recode w1bsizeounce 2=1.5 3=2 4=3 5=4 6=5 7=6 8=7 9=8 10=9 11=10 13=15 14=16 15=18 16=20 17=23.5 18=32 19=40 20=40 21=1.5 22=3 23=4.5 24=5 25=1.7 26=6.3 ///
   27=12.7 28=25.4 29=16.9 30=33.8 31=40 32=39.2 33=40 34=40 35=6.8 36=16 37=25.6 38=32 39=40 41=16 42=40 ///
   43=40 44=8 45=12 46=16 47=12 99=12
replace w1bsizeounce=0 if w1bsizeounce==. & w1bevany==1 
   
gen w1wsizeounce=s2aq6cr
recode w1wsizeounce 2=1.5 3=2 4=3 5=4 6=5 7=6 8=7 9=8 10=9 11=10 13=15 14=16 15=16 16=16 17=6 18=6 19=6 20=6 21=1.5 22=3 23=4.5 24=5 25=1.7 26=6.3 ///
   27=6 28=6 29=6 30=6 31=6 32=6 33=6 34=6 35=6.8 36=16 37=6 38=6 39=6 40=6 41=6 42=6 ///
   43=6 44=8 45=6 46=6 47=6 99=6
replace w1wsizeounce=0 if w1wsizeounce==. & w1bevany==1
 
gen w1ssizeounce=s2aq7cr
recode w1ssizeounce 1=1.5 3=3 4=4.5 5=6 6=5 7=6 8=6 9=8 10=2 11=2 12=2 13=2 14=2 15=2 16=2 17=2 18=2 19=2 20=2 21=1.5 22=3 23=4.5 24=6 25=1.7 ///
   26=6 27=2 28=2 29=2 30=2 31=2 32=2 33=2 34=2 35=2 36=2 37=2 38=2 39=2 40=2 41=2 42=2 ///
   43=2 44=2 45=2 46=2 47=2 99=1.5
replace w1ssizeounce=0 if w1ssizeounce==. & w1bevany==1 

//To construct annual volume variables
gen w1cyrvollt6=w1coolusualq*(w1cusualfd-w1cmaxfd)+w1coolmaxq*w1cmaxfd //annual cooler volume for 5 or fewer drinkers
gen w1cq5=exp((log(max(5, w1coolusualq)) + log (w1coolmaxq-1))/2)
gen w1coolq5=(w1c5plusfd-w1cmaxfd)*w1cq5 //intermediate component for 6+ drinkers
gen w1cyrvol6plus=w1coolusualq*(w1cusualfd-w1c5plusfd) + w1coolq5 + w1coolmaxq*w1cmaxfd //annual cooler volume for 6+ drinkers
gen w1cyrvol=w1cyrvollt6 if s2aq4e<6
replace w1cyrvol=w1cyrvol6plus if s2aq4e>5 & s2aq4e<. 
replace w1cyrvol=0 if s2aq4a==2 & w1bevany==1 //assigning 0 for respondents who did not drink cooler but other beverage(s)
gen w1cdailyvol=w1cyrvol/365

gen w1byrvollt6=w1beerusualq*(w1busualfd-w1bmaxfd)+w1beermaxq*w1bmaxfd //annual beer volume for 5 or fewer drinkers
gen w1bq5=exp((log(max(5, w1beerusualq)) + log (w1beermaxq-1))/2)
gen w1beerq5=(w1b5plusfd-w1bmaxfd)*w1bq5 //intermediate component for 6+ drinkers
gen w1byrvol6plus=w1beerusualq*(w1busualfd-w1b5plusfd) + w1beerq5 + w1beermaxq*w1bmaxfd //annual beer volume for 6+ drinkers
gen w1byrvol=w1byrvollt6 if s2aq5e<6
replace w1byrvol=w1byrvol6plus if s2aq5e>5 & s2aq5e<.
replace w1byrvol=0 if s2aq5a==2 & w1bevany==1 //assigning 0 for respondents who did not drink beer but other beverage(s)
gen w1bdailyvol=w1byrvol/365

gen w1wyrvollt6=w1wineusualq*(w1wusualfd-w1wmaxfd)+w1winemaxq*w1wmaxfd //annual wineer volume for 5 or fewer drinkers
gen w1wq5=exp((log(max(5, w1wineusualq)) + log (w1winemaxq-1))/2)
gen w1wineq5=(w1w5plusfd-w1wmaxfd)*w1wq5 //intermediate component for 6+ drinkers
gen w1wyrvol6plus=w1wineusualq*(w1wusualfd-w1w5plusfd) + w1wineq5 + w1winemaxq*w1wmaxfd //annual wine volume for 6+ drinkers
gen w1wyrvol=w1wyrvollt6 if s2aq6e<6
replace w1wyrvol=w1wyrvol6plus if s2aq6e>5 & s2aq6e<.
replace w1wyrvol=0 if s2aq6a==2 & w1bevany==1 //assigning 0 for respondents who did not drink wine but other beverage(s)
gen w1wdailyvol=w1wyrvol/365

gen w1syrvollt6=w1spiritsusualq*(w1susualfd-w1smaxfd)+w1spiritsmaxq*w1smaxfd //annual spirits volume for 5 or feser drinkers
gen w1sq5=exp((log(max(5, w1spiritsusualq)) + log (w1spiritsmaxq-1))/2)
gen w1spiritsq5=(w1s5plusfd-w1smaxfd)*w1sq5 //intermediate component for 6+ drinkers
gen w1syrvol6plus=w1spiritsusualq*(w1susualfd-w1s5plusfd) + w1spiritsq5 + w1spiritsmaxq*w1smaxfd //annual spirits volume for 6+ drinkers
gen w1syrvol=w1syrvollt6 if s2aq7e<6
replace w1syrvol=w1syrvol6plus if s2aq7e>5 & s2aq7e<.
replace w1syrvol=0 if s2aq7a==2 & w1bevany==1 //assigning 0 for respondents who did not drink liquor but other beverage(s)
gen w1sdailyvol=w1syrvol/365

gen w1coolecf=coolecf
replace w1coolecf=0 if coolecf==. & w1bevany==1
gen w1beerecf=beerecf
replace w1beerecf=0 if beerecf==. & w1bevany==1
gen w1wineecf=wineecf
replace w1wineecf=0 if wineecf==. & w1bevany==1
gen w1liqrecf=liqrecf
replace w1liqrecf=0 if liqrecf==. & w1bevany==1

gen w1cdailyeth=w1cdailyvol*w1csizeounce*w1coolecf 
replace w1cdailyeth=0 if w1cdailyeth==. & w1bevany==1 
gen w1bdailyeth=w1bdailyvol*w1bsizeounce*w1beerecf 
replace w1bdailyeth=0 if w1bdailyeth==. & w1bevany==1 
gen w1wdailyeth=w1wdailyvol*w1wsizeounce*w1wineecf 
replace w1wdailyeth=0 if w1wdailyeth==. & w1bevany==1 
gen w1sdailyeth=w1sdailyvol*w1ssizeounce*w1liqrecf 
replace w1sdailyeth=0 if w1sdailyeth==. & w1bevany==1 

gen w1dailyeth=w1cdailyeth+w1bdailyeth+w1wdailyeth+w1sdailyeth
sum w1dailyeth

gen w1dailyethgram=w1dailyeth * 28.35

//To construct bevreage proportion variables

gen w1cprop=w1cdailyeth/w1dailyeth
replace w1cprop=0 if w1cdailyeth==0 & w1bevany==1
label var w1cprop "cooler proportion"

gen w1bprop=w1bdailyeth/w1dailyeth
replace w1bprop=0 if w1bdailyeth==0 & w1bevany==1
label var w1bprop "beer proportion"

gen w1wprop=w1wdailyeth/w1dailyeth
replace w1wprop=0 if w1wdailyeth==0 & w1bevany==1
label var w1wprop "wine proportion"

gen w1sprop=w1sdailyeth/w1dailyeth
replace w1sprop=0 if w1sdailyeth==0 & w1bevany==1
label var w1sprop "liquor proportion"

gen coolprop=w1cprop //Dropping w1 to keep the variable names consistent with other waves
gen beerprop=w1bprop
gen wineprop=w1wprop
gen liqprop=w1sprop
label var coolprop "cooler proportion"
label var beerprop "beer proportion"
label var wineprop "wine proportion"
label var liqprop "liquor proportion"

//Drinking dcategories for men

gen w1drnkcatm=. 
replace w1drnkcatm=1 if male==1 & w1curdyr1==0  //abstainers
replace w1drnkcatm=2 if male==1 & w1dailyethgram>0 & w1dailyethgram<=40 //low risk
replace w1drnkcatm=3 if male==1 & w1dailyethgram>40 & w1dailyethgram<=60 //medium risk
replace w1drnkcatm=4 if male==1 & w1dailyethgram>60 & w1dailyethgram<=100 //high risk
replace w1drnkcatm=5 if male==1 & w1dailyethgram>100 & w1dailyethgram<.  //very high risk
label define w1drnkcatm 1 "abstainers" 2 "low risk" 3 "medium risk" 4 "high risk" 5 "very high risk"
label values w1drnkcatm w1drnkcatm

gen w1drnkcatm4=.
replace w1drnkcatm4=1 if w1drnkcatm==1   
replace w1drnkcatm4=2 if w1drnkcatm==2 
replace w1drnkcatm4=3 if w1drnkcatm==3 
replace w1drnkcatm4=4 if w1drnkcatm==4 | w1drnkcatm==5
label define w1drnkcatm4 1 "abstainers" 2 "low risk" 3 "medium risk" 4 "high/very high risk"
label values w1drnkcatm4 w1drnkcatm4

//Drinking dcategories for women

gen w1drnkcatf=.
replace w1drnkcatf=1 if male==0 & w1curdyr1==0 //abstainers
replace w1drnkcatf=2 if male==0 & w1dailyethgram>0 & w1dailyethgram<=20 //low risk
replace w1drnkcatf=3 if male==0 & w1dailyethgram>20 & w1dailyethgram<=40 //medium risk
replace w1drnkcatf=4 if male==0 & w1dailyethgram>40 & w1dailyethgram<=60 //hi risk
replace w1drnkcatf=5 if male==0 & w1dailyethgram>60 & w1dailyethgram<.  //very hi risk
label define w1drnkcatf 1 "abstainers" 2 "low risk" 3 "medium risk" 4 "high risk" 5 "very high risk"
label values w1drnkcatf w1drnkcatf

gen w1drnkcatf4=.
replace w1drnkcatf4=1 if w1drnkcatf==1   
replace w1drnkcatf4=2 if w1drnkcatf==2 
replace w1drnkcatf4=3 if w1drnkcatf==3 
replace w1drnkcatf4=4 if w1drnkcatf==4 | w1drnkcatf==5
label define w1drnkcatf4 1 "abstainers" 2 "low risk" 3 "medium risk" 4 "high/very high risk"
label values w1drnkcatf4 w1drnkcatf4

//Wave 1 demographic variables

gen male=0
replace male=1 if sex==1

gen female=0
replace female=1 if sex==2

gen age_cat=.
replace age_cat=1 if age>17 & age<31
replace age_cat=2 if age>30 & age<41
replace age_cat=3 if age>40 & age<51
replace age_cat=4 if age>50 & age<61
replace age_cat=5 if age>60
lab def agecat 1 "18-30 yrs" 2 "31-40 yrs" 3 "41-50 yrs" 4 "51-60 yrs" 5 "61+ yrs"
lab val age_cat agecat
tab age_cat

gen educ_cat=.
replace educ_cat=1 if s1q6a<8 /*lt HS*/
replace educ_cat=2 if s1q6a==8 | s1q6a==9 /*HS grad/GED*/
replace educ_cat=3 if s1q6a==10 | s1q6a==11 /*some coll*/
replace educ_cat=4 if s1q6a>11 /*college (ba)+*/
lab def educ_cat 1 "less than HS" 2 "HS grad/GED" 3 "some college" 4 " 4-yr college/advanced degree" 
lab val educ_cat educ_cat
lab var educ_cat "education"
tab educ_cat

destring ethrace2, generate(ethrace5)

lab def ethrace5 1 "White" 2 "Black" 3 "AI/AN" 4 "AA/NHPI" 5 "Hispanic"
lab val ethrace5 ethrace5 
lab var ethrace5 "race/ethnicity"
tab ethrace5


//Recoding demographic variables
Race/ethnicity: White, Black, Hispanic, All others
Age: 18-24, 25-34, 35-44, 45-54, 55-64, 65+
Education: Less than HS, HS, Some college, College+
Drinking: low, medium, high, very high

lab def male 1 "male" 0 "female"
lab val male male

gen age6=.
replace age6=1 if age<25 
replace age6=2 if age>24 & age<35
replace age6=3 if age>34 & age<45
replace age6=4 if age>44 & age<55
replace age6=5 if age>54 & age<65
replace age6=6 if age>64

lab def age6 1 "18-24 yrs" 2 "25-34 yrs" 3 "35-44 yrs" 4 "45-54 yrs" 5 "55-64 yrs" 6 "65+ yrs"
lab val age6 age6

lab def race4 1 "White" 2 "Black" 3 "Hispanic" 4 "Other race" 
lab val race4 race4

gen age3=.
replace age3=1 if age<35
replace age3=2 if age>34 & age<55
replace age3=3 if age>54
lab def age3 1 "18-34 yrs" 2 "35-54 yrs" 3 "55+ yrs"
lab val age3 age3
tab age3

gen educ_cat=.
replace educ_cat=1 if s1q6a<8 /*lt HS*/
replace educ_cat=2 if s1q6a==8 | s1q6a==9 /*HS grad/GED*/
replace educ_cat=3 if s1q6a==10 | s1q6a==11 /*some coll*/
replace educ_cat=4 if s1q6a>11 /*college (ba)+*/
lab def educ_cat 1 "less than HS" 2 "HS grad/GED" 3 "some college" 4 " 4-yr college/advanced degree" 
lab val educ_cat educ_cat
lab var educ_cat "education"

gen educ2=.
replace educ2=1 if educ_cat<3
replace educ2=2 if educ_cat==3 | educ_cat==4
label define educ2 1 "HS grad or less" 2 "some college plus"
label values educ2 educ2

gen race4=ethrace5
recode race4 1=1 2=2 5=3 3 4=4 
lab def race4 1 "white" 2 "black" 3 "hispanic" 4 "other"
lab val race4 race4
tab race4

gen white=0
replace white=1 if race4==1

gen black=0
replace black=1 if race4==2

gen hispanic=0
replace hispanic=1 if race4==3

gen otherrace=0
replace otherrace=1 if race4==4

////To compute means, SEs, and 95% CIs and construct Excel data file for them***

////Below are mostly Yu Ye's (he helped with the macros), which I modified as needed


svyset psu [pweight=weight], strata(stratum) vce(linear) singleunit(centered) 

*svyset psu [pweight=weight], /*strata(stratum)*/ vce(linear) singleunit(centered) 

*svyset /*psu*/ [pweight=weight], /*strata(stratum)*/ vce(linear) singleunit(centered) 


****************************************************************
***********Step 1: processing variables*************************
****************************************************************

**create three-category drinking pattern combining male and female**
**the new pattern variable values 1-3 
gen riskpttn3 = w1drnkcatm4 if sex == 1
replace riskpttn3 = w1drnkcatf4 if sex == 2
replace riskpttn3 = riskpttn3 - 1

**create four-category drinking pattern combining male and female**
**the new pattern variable values 1-4 
gen riskpttn4 = w1drnkcatm if sex == 1
replace riskpttn4 = w1drnkcatf if sex == 2
replace riskpttn4 = riskpttn4 - 1

tab race4, m
tab1 educ*, m
tab1 age3 age6, m

tab riskpttn4, gen(riskpttn4_)
tab riskpttn3, gen(riskpttn3_)

****************************************************************
***********Step 2: Count ns in subgroups*************************
************For white, black, Hisp, Others separately***********
****************************************************************

**white, male**
**using sex, age6, educ_cat, riskpttn4**
table age6 educ_cat if sex==1 & race4==1, ///
   stat(sum riskpttn4_1 riskpttn4_2 riskpttn4_3 riskpttn4_4) ///
   nototals 
**white, female**
**using sex, age6, educ_cat, riskpttn4**
table age6 educ_cat if sex==2 & race4==1, ///
   stat(sum riskpttn4_1 riskpttn4_2 riskpttn4_3 riskpttn4_4) ///
   nototals    
   
**black, male**
**using sex, age3, educ2, riskpttn3**
table age3 educ2 if sex==1 & race4==2, ///
   stat(sum riskpttn3_1 riskpttn3_2 riskpttn3_3) ///
   nototals 
**black, female**
**using sex, age3, educ2, riskpttn3**
table age3 educ2 if sex==2 & race4==2, ///
   stat(sum riskpttn3_1 riskpttn3_2 riskpttn3_3) ///
   nototals 
   
**Hisp, male**
**using sex, age3, educ2, riskpttn3**
table age3 educ2 if sex==1 & race4==3, ///
   stat(sum riskpttn3_1 riskpttn3_2 riskpttn3_3) ///
   nototals 
**Hisp, female**
**using sex, age3, educ2, riskpttn3**
table age3 educ2 if sex==2 & race4==3, ///
   stat(sum riskpttn3_1 riskpttn3_2 riskpttn3_3) ///
   nototals   
   
**Others, male**
**using sex, age3, educ2, riskpttn3**
table age3 educ2 if sex==1 & race4==4, ///
   stat(sum riskpttn3_1 riskpttn3_2 riskpttn3_3) ///
   nototals 
**Others, female**
**using sex, age3, educ2, riskpttn3**
table age3 educ2 if sex==2 & race4==4, ///
   stat(sum riskpttn3_1 riskpttn3_2 riskpttn3_3) ///
   nototals  
   
****************************************************************
***********Step 3: write codes to export results****************
************run the macro first before step 4***********
****************************************************************   

***write the program to predict change**
capture program drop outtable 
program outtable, rclass
	version 16.0
	syntax, racevalue(numlist) outcome(varlist) sexvar(varname) agevar(varname) educvar(varname) drinkvar(varname)
	
	**select the specific race***
	tempvar select 
	quietly gen `select' = race4 == `racevalue'
	
	**count n of outcomes**
	local outcomecnt: word count `outcome'
	
	**count each varible categories*** 
	quietly tab `sexvar' if `select'
	   local sexcnt = r(r)
	quietly tab `agevar' if `select' 
	   local agecnt = r(r)	  	   
	quietly tab `educvar' if `select' 
	   local educcnt = r(r)	   
	quietly tab `drinkvar' if `select'
	   local drinkcnt = r(r)
	   
	**matrix n of rows and columns** 
	local rowcnt = `sexcnt' * `agecnt' * `educcnt' * `drinkcnt'
	local colcnt = `outcomecnt' * 5 + 5
	
	**set up matrix for output** 
	tempname tablemat
	matrix `tablemat' = J((`rowcnt'),`colcnt',.)
	local colname `sexvar' `agevar' `educvar' `drinkvar' n 
	forvalues i = 1/`outcomecnt' {
			local out`i': word `i' of `outcome'
			local colname = "`colname' `out`i''mn `out`i''se `out`i''lo `out`i''hi `out`i''n"			
		} 
	matrix colnames `tablemat' = `colname'
	
	tempvar one 
	quietly gen `one' = 1
	
	local row = 0
	forvalues i = 1/`sexcnt' {
		forvalues j = 1/`agecnt' {
			forvalues k = 1/`educcnt' {
				forvalues m = 1/`drinkcnt' {
				   local row = `row' + 1	
				   quietly sum `one' if `select' & `sexvar'==`i' & `agevar'==`j' & `educvar'==`k' & `drinkvar'==`m'
				   		matrix `tablemat'[`row', 1] = `i'
				   		matrix `tablemat'[`row', 2] = `j'					
				   		matrix `tablemat'[`row', 3] = `k'	
				   		matrix `tablemat'[`row', 4] = `m'
				   		matrix `tablemat'[`row', 5] = r(sum)	
					forvalues n = 1/`outcomecnt' {
						local outcome`n': word `n' of `outcome'
						local col1 = 5 + (`n'-1)*5 + 1
						local col2 = 5 + (`n'-1)*5 + 2
						local col3 = 5 + (`n'-1)*5 + 3		
						local col4 = 5 + (`n'-1)*5 + 4	
						local col5 = 5 + (`n'-1)*5 + 5							
						capture {
							quietly svy, subpop(if `select' & ///
								`sexvar'==`i' & `agevar'==`j' & `educvar'==`k' & `drinkvar'==`m'): mean `outcome`n'' 
						  matrix `tablemat'[`row', `col1'] = r(table)[1,1]
						  matrix `tablemat'[`row', `col2'] = r(table)[2,1]	
						  matrix `tablemat'[`row', `col3'] = r(table)[5,1]	
						  matrix `tablemat'[`row', `col4'] = r(table)[6,1]							  
						  matrix `tablemat'[`row', `col5'] = e(N)						  
*						if _rc == 2000 {
*						  matrix `tablemat'[`row', `col3'] = e(N)	
*						}  
						}	  
					}	
				}
			}
		}
	}
    matrix list `tablemat'
	return matrix table = `tablemat'
end 


****************************************************************
***********Step 4: First run the macro****************
*****************  Then export to Stata dataset***********
**************************************************************** 

**rename outcome variables to make it short**
//rename coolprop coolp 
//rename beerprop beerp 
//rename wineprop winep
//rename liqprop liqp 

**Note to run macro**
**1: Separately for white, black, Hispanic, Other using "racevalue", meaning race4=1-4 **
**2: Subgroup categories must be the same for each race (e.g. can't have two drinking patterns by sex)
**3: Subgroup categories must be in the form of 1,2,...etc (e.g. can't be 0,1)
**4: Run the matrix to data convert codes after macro, save to Stata datasets

**white**
outtable, racevalue(1) outcome(coolp beerp winep liqp) sexvar(sex) agevar(age6) educvar(educ_cat) drinkvar(riskpttn4)	
preserve 
clear
svmat double r(table), names(col)
gen race4 = 1
save "C:\Back up\CAMH\Deliverables\NESARC 1_white.dta", replace 
restore 

**black**
outtable, racevalue(2) outcome(coolp beerp winep liqp) sexvar(sex) agevar(age3) educvar(educ2) drinkvar(riskpttn3)	
preserve 
clear
svmat double r(table), names(col)
gen race4 = 2
save "C:\Back up\CAMH\Deliverables\NESARC 1_black.dta", replace 
restore 

**hisp***
outtable, racevalue(3) outcome(coolp beerp winep liqp) sexvar(sex) agevar(age3) educvar(educ2) drinkvar(riskpttn3)	
preserve 
clear
svmat double r(table), names(col)
gen race4 = 3
save "C:\Back up\CAMH\Deliverables\NESARC 1_hisp.dta", replace 
restore 

**others**
outtable, racevalue(4) outcome(coolp beerp winep liqp) sexvar(sex) agevar(age3) educvar(educ2) drinkvar(riskpttn3)		
preserve 
clear
svmat double r(table), names(col)
gen race4 = 4
save "C:\Back up\CAMH\Deliverables\NESARC 1_other.dta", replace 
restore 	

**Examine different set up of svy structure for CI estimates**
svyset psu [pweight=weight], strata(stratum) vce(linear) singleunit(centered) 
svy, subpop(if race4 == 4 & sex == 1 & age3 == 1 & educ2 == 1 & riskpttn3 == 2): mean coolp

svyset psu [pweight=weight], /*strata(stratum)*/ vce(linear) singleunit(centered)	
svy, subpop(if race4 == 4 & sex == 1 & age3 == 1 & educ2 == 1 & riskpttn3 == 2): mean coolp
	
	
***Here we can combine black/Hisp/others, as they are using the same variables***
***But we can't combine white with them*** 
	
use "C:\Back up\CAMH\Deliverables\NESARC 1_black.dta", clear 
append using "C:\Back up\CAMH\Deliverables\NESARC 1_hisp.dta"
append using "C:\Back up\CAMH\Deliverables\NESARC 1_other.dta"
save "C:\Back up\CAMH\Deliverables\NESARC 1_BlackHispOther.dta"

******To define output file for black/Hisp/others and to convert it to Excel file

lab def race4 1 "White" 2 "Black" 3 "Hispanic" 4 "Other race" 
lab val race4 race4

lab def age3 1 "18-34 yrs" 2 "35-54 yrs" 3 "55+ yrs"
lab val age3 age3

lab var riskpttn3 "drinking level, 3 cat"
label define riskpttn3 1 "low risk" 2 "medium risk" 3 "high/very high risk"
label values riskpttn3 riskpttn3

lab var educ2 "education, 2 cat"
label define educ2 1 "HS grad or less" 2 "some college+"
label values educ2 educ2

label var coolpmn "cooler prop mean"
label var coolpse "cooler prop SE"
label var coolplo "cooler lower CI
label var coolphi "cooler upper CI"

label var beerpmn "beer prop mean"
label var beerpse "beer prop SE"
label var beerplo "beer lower CI"
label var beerphi "beer upper CI"

label var winepmn "wine prop mean"
label var winepse "wine prop SE"
label var wineplo "wine lower CI"
label var winephi "wine upper CI"

label var liqpmn "liquor prop mean"
label var liqpse "liquor prop SE"
label var liqplo "liquor lower CI"
label var liqphi "liquor upper CI"

save "C:\Back up\CAMH\Deliverables\NESARC 1_BlackHispOther.dta", replace

******To define output file for whites and to convert it to Excel file

use "c:\temp\nesarc_summarize_white.dta", clear 
tab race4, m
sum *mn *se n

lab def sex 1 "male" 2 "female"
lab val sex sex

lab def race4 1 "White" 2 "Black" 3 "Hispanic" 4 "Other race" 
lab val race4 race4

lab def age6 1 "18-24 yrs" 2 "25-34 yrs" 3 "35-44 yrs" 4 "45-54 yrs" 5 "55-64 yrs" 6 "65+ yrs"
lab val age6 age6

label var riskpttn4 "drinking levels"
label define riskpttn4 1 "low risk" 2 "medium risk" 3 "high risk" 4 "very high risk"
label values riskpttn4 riskpttn4

lab var educ_cat "education"
lab def educ_cat 1 "less than HS" 2 "HS grad/GED" 3 "some college" 4 " 4-yr college/advanced degree" 
lab val educ_cat educ_cat

label var coolpmn "cooler prop mean"
label var coolpse "cooler prop SE"
label var coolplo "cooler lower CI
label var coolphi "cooler upper CI"

label var beerpmn "beer prop mean"
label var beerpse "beer prop SE"
label var beerplo "beer lower CI"
label var beerphi "beer upper CI"

label var winepmn "wine prop mean"
label var winepse "wine prop SE"
label var wineplo "wine lower CI"
label var winephi "wine upper CI"

label var liqpmn "liquor prop mean"
label var liqpse "liquor prop SE"
label var liqplo "liquor lower CI"
label var liqphi "liquor upper CI"

use "C:\Back up\CAMH\Deliverables\NESARC 1_white.dta"
export delimited using "C:\Back up\CAMH\Deliverables\Excel files\NESARC 1_white", nolabel datafmt 

use "C:\Back up\CAMH\Deliverables\NESARC 1_BlackHispOther.dta"
export delimited using "C:\Back up\CAMH\Deliverables\Excel files\NESARC 1_BlackHispOther", nolabel datafmt 

use "C:\Back up\CAMH\Deliverables\NESARC 2_white.dta"
export delimited using "C:\Back up\CAMH\Deliverables\Excel files\NESARC 2_white", nolabel datafmt 

use "C:\Back up\CAMH\Deliverables\NESARC 2_BlackHispOther.dta"
export delimited using "C:\Back up\CAMH\Deliverables\Excel files\NESARC 2_BlackHispOther", nolabel datafmt 

use "C:\Back up\CAMH\Deliverables\NESARC 3_white.dta"
export delimited using "C:\Back up\CAMH\Deliverables\Excel files\NESARC 3_white", nolabel datafmt 

use "C:\Back up\CAMH\Deliverables\NESARC 3_BlackHispOther.dta"
export delimited using "C:\Back up\CAMH\Deliverables\Excel files\NESARC 3_BlackHispOther", nolabel datafmt 









