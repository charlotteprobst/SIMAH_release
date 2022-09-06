//I used merged NESARC 2 and 3 files as I had them merged already for another project
//You could use "if survey==2" (or "if survey==3") to subset NESARC 2 data (or NESARC 3)

////Current drinker variables

//1 drink past year, which was used in these analyses

gen currdyr1=0
replace currdyr1=1 if w2s2aq3==1 | n2aq3==1
replace currdyr1=. if currdyr1==9 | currdyr1==.
tab currdyr1

//12 drinks past year

gen currdyr12=0
replace currdyr12=1 if w2s2aq2==1 | n2aq2==1
replace currdyr12=. if currdyr12==9
tab currdyr12

//Variable for reporting any beverage-specific consumption

gen bevanyw2=0
replace  bevanyw2=1 if w2s2aq5a==1|w2s2aq6a==1|w2s2aq7a==1|w2s2aq8a==1
gen bevanyw3=0
replace  bevanyw3=1 if n2aq5a==1|n2aq6a==1|n2aq7a==1|n2aq8a==1
gen bevanyw23=0
replace bevanyw23=1 if bevanyw2==1|bevanyw3==1

//5+/4+ at least monthly

gen freq5plus=0
replace  freq5plus=1 if w2s2aq4f<8 //5+ at least monthly
replace  freq5plus=. if w2s2aq4f==99 | w2s2aq4f==.
tab w2s2aq4f if survey==2 & male==1
tab freq5plus if survey==2 & male==1

gen freq4plus=0
replace  freq4plus=1 if w2s2aq4e<8 //4+ at least monthly, women only
replace  freq4plus=. if w2s2aq4e==99 | w2s2aq4e==.
tab w2s2aq4e if survey==2 & male==0
tab freq4plus if survey==2 & male==0

/////Beverage-specific frequencies and quantities

gen coolusualf=0 //cooler usual frequency
replace coolusualf=w2s2aq5b if survey==2
replace coolusualf=n2aq5b if survey==3
replace coolusualf=. if coolusualf==99
tab coolusualf

gen coolusualq=0 //cooler usual quantity
replace coolusualq=w2s2aq5d  if survey==2
replace coolusualq=n2aq5d if survey==3
replace coolusualq=. if coolusualq==99
tab coolusualq

gen cool5plusf=0 //5+ freq
replace cool5plusf=w2s2aq5g if survey==2
replace cool5plusf=n2aq5g if survey==3
replace cool5plusf=. if cool5plusf==99
tab cool5plusf

gen coolmaxf=0 //max freq
replace coolmaxf=w2s2aq5f  if survey==2
replace coolmaxf=n2aq5f  if survey==3
replace coolmaxf=. if coolmaxf==99
tab coolmaxf

gen coolmaxq=0 //max quantity
replace coolmaxq=w2s2aq5e  if survey==2
replace coolmaxq=n2aq5e  if survey==3
replace coolmaxq=. if coolmaxq==99
tab coolmaxq

gen beerusualf=0
replace beerusualf=w2s2aq6b if survey==2
replace beerusualf=n2aq6b if survey==3
replace beerusualf=. if beerusualf==99
tab beerusualf

gen beer5plusf=0
replace beer5plusf=w2s2aq6g if survey==2
replace beer5plusf=n2aq6g if survey==3
replace beer5plusf=. if beer5plusf==99
tab beer5plusf

gen beerusualq=0
replace beerusualq=w2s2aq6d  if survey==2
replace beerusualq=n2aq6d if survey==3
replace beerusualq=. if beerusualq==99 
tab beerusualq

gen beermaxf=0
replace beermaxf=w2s2aq6f  if survey==2
replace beermaxf=n2aq6f  if survey==3
replace beermaxf=. if beermaxf==99
tab beermaxf

gen beermaxq=0
replace beermaxq=w2s2aq6e  if survey==2
replace beermaxq=n2aq6e  if survey==3
replace beermaxq=. if beermaxq==99
tab beermaxq

gen wineusualf=0
replace wineusualf=w2s2aq7b if survey==2
replace wineusualf=n2aq7b if survey==3
replace wineusualf=. if wineusualf==99 
tab wineusualf

gen wineusualq=0
replace wineusualq=w2s2aq7d  if survey==2
replace wineusualq=n2aq7d if survey==3
replace wineusualq=. if wineusualq==99 
tab wineusualq

gen winemaxf=0
replace winemaxf=w2s2aq7f  if survey==2
replace winemaxf=n2aq7f  if survey==3
replace winemaxf=. if winemaxf==99 
tab winemaxf

gen wine5plusf=0
replace wine5plusf=w2s2aq7g if survey==2
replace wine5plusf=n2aq7g if survey==3
replace wine5plusf=. if wine5plusf==99
tab wine5plusf

gen winemaxq=0
replace winemaxq=w2s2aq7e  if survey==2
replace winemaxq=n2aq7e  if survey==3
replace winemaxq=. if winemaxq==99
tab winemaxq

gen spiritsusualf=0
replace spiritsusualf=w2s2aq8b if survey==2
replace spiritsusualf=n2aq8b if survey==3
replace spiritsusualf=. if spiritsusualf==99 
tab spiritsusualf

gen spiritsusualq=0
replace spiritsusualq=w2s2aq8d  if survey==2
replace spiritsusualq=n2aq8d if survey==3
replace spiritsusualq=. if spiritsusualq==99 
tab spiritsusualq

gen spirits5plusf=0
replace spirits5plusf=w2s2aq8g if survey==2
replace spirits5plusf=n2aq8g if survey==3
replace spirits5plusf=. if spirits5plusf==99
tab spirits5plusf

gen spiritsmaxf=0
replace spiritsmaxf=w2s2aq8f  if survey==2
replace spiritsmaxf=n2aq8f  if survey==3
replace spiritsmaxf=. if spiritsmaxf==99 
tab spiritsmaxf

gen spiritsmaxq=0
replace spiritsmaxq=w2s2aq8e  if survey==2
replace spiritsmaxq=n2aq8e  if survey==3
replace spiritsmaxq=. if spiritsmaxq==99 
tab spiritsmaxq

//converting categorical drinking frequencies to numerical variables for number of days per year

gen cusualfd=coolusualf
recode cusualfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.
tab cusualfd
tab coolusualf

gen busualfd=beerusualf
recode busualfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.

gen wusualfd=wineusualf
recode wusualfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.

gen susualfd=spiritsusualf
recode susualfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.

gen cmaxfd=coolmaxf
recode cmaxfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.
tab cmaxfd
tab coolmaxf

gen bmaxfd=beermaxf
recode bmaxfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.

gen wmaxfd=winemaxf
recode wmaxfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.

gen smaxfd=spiritsmaxf
recode smaxfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.
tab smaxfd
tab spiritsmaxf

gen c5plusfd=cool5plusf
recode c5plusfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.
tab cool5plusf
tab c5plusfd

gen w5plusfd=wine5plusf
recode w5plusfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.

gen b5plusfd=beer5plusf
recode b5plusfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.

gen s5plusfd=spirits5plusf
recode s5plusfd 1=365 2=273 3=182 4=104 5=52 6=30 7=12 8=9 9=4.5 10=1.5 11=0 99=.

//To generate annual volume variables, beverage-specific

gen cyrvollt6=coolusualq*(cusualfd-cmaxfd)+coolmaxq*cmaxfd //annual cooler volume for 5 or fewer drinkers
gen cq5=exp((log(max(5, coolusualq)) + log (coolmaxq-1))/2)
gen coolq5=(c5plusfd-cmaxfd)*cq5 //intermediate component for 6+ drinkers
gen cyrvol6plus=coolusualq*(cusualfd-c5plusfd) + coolq5 + coolmaxq*cmaxfd  
gen cyrvol=cyrvollt6 if (w2s2aq5e<6|n2aq5e<6)
replace cyrvol=cyrvol6plus if (w2s2aq5e>5|n2aq5e>5)&(w2s2aq5e<.|n2aq5e<.)
replace cyrvol=0 if (n2aq5a==2 | w2s2aq5a==2) & bevanyw23==1 
gen cdailyvol=cyrvol/365

gen byrvollt6=beerusualq*(busualfd-bmaxfd)+beermaxq*bmaxfd //annual beerer volume for 5 or fewer drinkers
gen bq5=exp((log(max(5, beerusualq)) + log (beermaxq-1))/2)
gen beerq5=(b5plusfd-bmaxfd)*bq5 //intermediate component for 6+ drinkers
gen byrvol6plus=beerusualq*(busualfd-b5plusfd) + beerq5 + beermaxq*bmaxfd  
gen byrvol=byrvollt6 if (w2s2aq6e<6|n2aq6e<6)
replace byrvol=byrvol6plus if (w2s2aq6e>5|n2aq6e>5)&(w2s2aq6e<.|n2aq6e<.)
replace byrvol=0 if (w2s2aq6a==2| n2aq6a==2) & bevanyw23==1
gen bdailyvol=byrvol/365

gen wyrvollt6=wineusualq*(wusualfd-wmaxfd)+winemaxq*wmaxfd //annual wineer volume for 5 or fewer drinkers
gen wq5=exp((log(max(5, wineusualq)) + log (winemaxq-1))/2)
gen wineq5=(w5plusfd-wmaxfd)*wq5 //intermediate component for 6+ drinkers
gen wyrvol6plus=wineusualq*(wusualfd-w5plusfd) + wineq5 + winemaxq*wmaxfd  
gen wyrvol=wyrvollt6 if (w2s2aq7e<6|n2aq7e<6)
replace wyrvol=wyrvol6plus if (w2s2aq7e>5|n2aq7e>5)&(w2s2aq7e<.|n2aq7e<.)
replace wyrvol=0 if (w2s2aq7a==2| n2aq7a==2) & bevanyw23==1

gen wdailyvol=wyrvol/365
gen syrvollt6=spiritsusualq*(susualfd-smaxfd)+spiritsmaxq*smaxfd //annual spiritser volume for 5 or fewer drinkers
gen sq5=exp((log(max(5, spiritsusualq)) + log (spiritsmaxq-1))/2)
gen spiritsq5=(s5plusfd-smaxfd)*sq5 //intermediate component for 6+ drinkers
gen syrvol6plus=spiritsusualq*(susualfd-s5plusfd) + spiritsq5 + spiritsmaxq*smaxfd  
gen syrvol=syrvollt6 if (w2s2aq8e<6|n2aq8e<6)
replace syrvol=syrvol6plus if (w2s2aq8e>5|n2aq8e>5)&(w2s2aq8e<.|n2aq8e<.)
replace syrvol=0 if (w2s2aq8a==2| n2aq8a==2) & bevanyw23==1 
gen sdailyvol=syrvol/365

gen tdailyvol=cdailyvol+bdailyvol+wdailyvol+sdailyvol

//////Creating variables for drink sizes in ounce (reflecting Bill's recodes)

//cooler
gen csizeounce=n2aq5cr 
replace csizeounce=w2s2aq5cr if survey==2
recode csizeounce 2=1.5 3=2 4=3 5=4 6=5 7=6 8=7 9=8 10=9 11=10 13=15 14=16 15=18 16=20 17=23.5 18=32 19=40 20=40 21=1.5 22=3 23=4.5 24=5 25=1.7 26=6.3 ///
   27=12.7 28=25.4 29=16.9 30=33.8 31=40 32=39.2 33=40 34=40 35=6.8 36=16 37=25.6 38=32 39=40 41=16 42=40 ///
   43=40 44=8 45=12 46=16 47=12 99=12
replace csizeounce=0 if csizeounce==. & bevanyw23==1 
//beer
gen bsizeounce=n2aq6cr 
replace bsizeounce=w2s2aq6cr if survey==2
recode bsizeounce 2=1.5 3=2 4=3 5=4 6=5 7=6 8=7 9=8 10=9 11=10 13=15 14=16 15=18 16=20 17=23.5 18=32 19=40 20=40 21=1.5 22=3 23=4.5 24=5 25=1.7 26=6.3 ///
   27=12.7 28=25.4 29=16.9 30=33.8 31=40 32=39.2 33=40 34=40 35=6.8 36=16 37=25.6 38=32 39=40 41=16 42=40 ///
   43=40 44=8 45=12 46=16 47=12 99=12
replace bsizeounce=0 if bsizeounce==. & bevanyw23==1 
//wine
gen wsizeounce=n2aq7cr  
replace wsizeounce=w2s2aq7cr if survey==2
recode wsizeounce 2=1.5 3=2 4=3 5=4 6=5 7=6 8=7 9=8 10=9 11=10 13=15 14=16 15=16 16=16 17=6 18=6 19=6 20=6 21=1.5 22=3 23=4.5 24=5 25=1.7 26=6.3 ///
   27=6 28=6 29=6 30=6 31=6 32=6 33=6 34=6 35=6.8 36=16 37=6 38=6 39=6 40=6 41=6 42=6 ///
   43=6 44=8 45=6 46=6 47=6 99=6
replace wsizeounce=0 if wsizeounce==. & bevanyw23==1
//spirits
gen ssizeounce=n2aq8cr
replace ssizeounce=w2s2aq8cr if survey==2
recode ssizeounce 1=1.5 2=1.5 3=3 4=4.5 5=6 6=5 7=6 8=6 9=2 10=2 11=2 12=2 13=2 14=2 15=2 16=2 17=2 18=2 19=2 20=2 21=1.5 22=3 23=4.5 24=6 25=1.7 ///
   26=6 27=2 28=2 29=2 30=2 31=2 32=2 33=2 34=2 35=6 36=2 37=2 38=2 39=2 40=2 41=2 42=2 ///
   43=2 44=2 45=2 46=2 47=2 99=1.5
replace ssizeounce=0 if ssizeounce==. & bevanyw23==1

//combining Wave 2 and 3 coolecf, etc.
gen coolecf=w2coolecf
replace coolecf=ncoolecf if survey==3
replace coolecf=0 if coolecf==. & bevanyw23==1
gen beerecf=w2beerecf
replace beerecf=nbeerecf if survey==3
replace beerecf=0 if beerecf==. & bevanyw23==1
gen wineecf=w2wineecf
replace wineecf=nwineecf if survey==3
replace wineecf=0 if wineecf==. & bevanyw23==1
gen liqrecf=w2liqrecf
replace liqrecf=nliqrecf if survey==3
replace liqrecf=0 if liqrecf==. & bevanyw23==1

////////////daily ethanol consumed,  ounce
gen cdailyeth=cdailyvol*csizeounce*coolecf
replace cdailyeth=0 if cdailyeth==. & bevanyw23==1 
gen bdailyeth=bdailyvol*bsizeounce*beerecf 
replace bdailyeth=0 if bdailyeth==. & bevanyw23==1 
gen wdailyeth=wdailyvol*wsizeounce*wineecf
replace wdailyeth=0 if wdailyeth==. & bevanyw23==1  
gen sdailyeth=sdailyvol*ssizeounce*liqrecf
replace sdailyeth=0 if sdailyeth==. & bevanyw23==1 
sum cdailyeth
sum bdailyeth
sum wdailyeth
sum sdailyeth

//sum of daily ethanol consumption 
gen w23dailyeth=cdailyeth + bdailyeth +wdailyeth + sdailyeth

gen w23dailyethgram=w23dailyeth * 28.35


//daily ethanol consumption converted to grams
gen cdailyethgram=cdailyeth * 28.35
gen bdailyethgram=bdailyeth * 28.35
gen wdailyethgram=wdailyeth * 28.35
gen sdailyethgram=sdailyeth * 28.35

sum cdailyethgram if survey==2
sum cdailyethgram if survey==3

sum bdailyethgram if survey==2
sum bdailyethgram if survey==3

sum wdailyethgram if survey==2
sum wdailyethgram if survey==3

sum sdailyethgram if survey==2
sum sdailyethgram if survey==3


//To construct bevreage proportion variables

gen cprop=cdailyeth/w23dailyeth
replace cprop=0 if cdailyeth==0 & bevanyw23==1

gen bprop=bdailyeth/w23dailyeth
replace bprop=0 if bdailyeth==0 & bevanyw23==1

gen wprop=wdailyeth/w23dailyeth
replace wprop=0 if wdailyeth==0 & bevanyw23==1

gen sprop=sdailyeth/w23dailyeth
replace sprop=0 if sdailyeth==0 & bevanyw23==1


//Drinking dcategories for men
gen drnkcatm=.
replace drnkcatm=1 if male==1 & currdyr1==0  //abstainers
replace drnkcatm=2 if male==1 & w23dailyethgram>0 & w23dailyethgram<=40 //low risk
replace drnkcatm=3 if male==1 & w23dailyethgram>40 & w23dailyethgram<=60 //medium risk
replace drnkcatm=4 if male==1 & w23dailyethgram>60 & w23dailyethgram<=100 //high risk
replace drnkcatm=5 if male==1 & w23dailyethgram>100 & w23dailyethgram<.  //very high risk
tab drnkcatm if male==1 & survey==2
tab drnkcatm if male==1 & survey==3
label define drnkcatm 1 "abstainers" 2 "low risk" 3 "medium risk" 4 "high risk" 5 "very high risk"
label values drnkcatm drnkcatm

gen drnkcatm4=.
replace drnkcatm4=1 if drnkcatm==1   
replace drnkcatm4=2 if drnkcatm==2 
replace drnkcatm4=3 if drnkcatm==3 
replace drnkcatm4=4 if drnkcatm==4 | drnkcatm==5
label define drnkcatm4 1 "abstainers" 2 "low risk" 3 "medium risk" 4 "high/very high risk"
label values drnkcatm4 drnkcatm4

//Drinking dcategories for women
gen drnkcatf=.
replace drnkcatf=1 if male==0 & currdyr1==0 //abstainers
replace drnkcatf=2 if male==0 & w23dailyethgram>0 & w23dailyethgram<=20 //low risk
replace drnkcatf=3 if male==0 & w23dailyethgram>20 & w23dailyethgram<=40 //medium risk
replace drnkcatf=4 if male==0 & w23dailyethgram>40 & w23dailyethgram<=60 //hi risk
replace drnkcatf=5 if male==0 & w23dailyethgram>60 & w23dailyethgram<.  //very hi risk
tab drnkcatf if male==0 & survey==2
tab drnkcatf if male==0 & survey==3
label define drnkcatf 1 "abstainers" 2 "low risk" 3 "medium risk" 4 "high risk" 5 "very high risk"
label values drnkcatf drnkcatf

gen drnkcatf4=.
replace drnkcatf4=1 if drnkcatf==1   
replace drnkcatf4=2 if drnkcatf==2 
replace drnkcatf4=3 if drnkcatf==3 
replace drnkcatf4=4 if drnkcatf==4 | drnkcatf==5
label define drnkcatf4 1 "abstainers" 2 "low risk" 3 "medium risk" 4 "high/very high risk"
label values drnkcatf4 drnkcatf4


////To compute means, SEs, and 95% CIs and construct Excel data file for them***

//Below are for NESARC Wave 2 (survey==2) similar to Wave 1 with some minor differences (e.g, weight variables)
//Use survey==3 instead to subset Wave 3

use "C:\Back up\CAMH\Data analysis\NESARC 2 to use.dta"

svyset psu [pweight=w2weight], strata(w2stratum) vce(linear) singleunit(centered) 

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
save "C:\Back up\CAMH\Deliverables\NESARC 2_white.dta", replace 
restore 

**black**
outtable, racevalue(2) outcome(coolp beerp winep liqp) sexvar(sex) agevar(age3) educvar(educ2) drinkvar(riskpttn3)	
preserve 
clear
svmat double r(table), names(col)
gen race4 = 2
save "C:\Back up\CAMH\Deliverables\NESARC 2_black.dta", replace 
restore 

**hisp***
outtable, racevalue(3) outcome(coolp beerp winep liqp) sexvar(sex) agevar(age3) educvar(educ2) drinkvar(riskpttn3)	
preserve 
clear
svmat double r(table), names(col)
gen race4 = 3
save "C:\Back up\CAMH\Deliverables\NESARC 2_hisp.dta", replace 
restore 

**others**
outtable, racevalue(4) outcome(coolp beerp winep liqp) sexvar(sex) agevar(age3) educvar(educ2) drinkvar(riskpttn3)		
preserve 
clear
svmat double r(table), names(col)
gen race4 = 4
save "C:\Back up\CAMH\Deliverables\NESARC 2_other.dta", replace 
restore 	

**Examine different set up of svy structure for CI estimates**
svyset psu [pweight=weight2], strata(stratum2) vce(linear) singleunit(centered) 
svy, subpop(if race4 == 4 & sex == 1 & age3 == 1 & educ2 == 1 & riskpttn3 == 2): mean coolp

svyset psu [pweight=weight], /*strata(stratum)*/ vce(linear) singleunit(centered)	
svy, subpop(if race4 == 4 & sex == 1 & age3 == 1 & educ2 == 1 & riskpttn3 == 2): mean coolp
	
	
***Here we can combine black/Hisp/others, as they are using the same variables***
***But we can't combine white with them*** 
	
use "C:\Back up\CAMH\Deliverables\NESARC 2_black.dta", clear 
append using "C:\Back up\CAMH\Deliverables\NESARC 2_hisp.dta"
append using "C:\Back up\CAMH\Deliverables\NESARC 2_other.dta"
save "C:\Back up\CAMH\Deliverables\NESARC 2_BlackHispOther.dta"

******To define BlackHispOther file

lab def sex 1 "male" 2 "female"
lab val sex sex

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

drop if riskpttn3==4

save "C:\Back up\CAMH\Deliverables\NESARC 2_BlackHispOther.dta", replace

******To define white file

use "C:\Back up\CAMH\Deliverables\NESARC 2_white.dta", clear 

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

drop if riskpttn4==5

save "C:\Back up\CAMH\Deliverables\NESARC 2_white.dta", replace

use "C:\Back up\CAMH\Deliverables\NESARC 2_white.dta"
export delimited using "C:\Back up\CAMH\Deliverables\Excel files\NESARC 2_white", nolabel datafmt 

use "C:\Back up\CAMH\Deliverables\NESARC 2_BlackHispOther.dta"
export delimited using "C:\Back up\CAMH\Deliverables\Excel files\NESARC 2_BlackHispOther", nolabel datafmt 




