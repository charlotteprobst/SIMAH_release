// NIH2020 project. 
// Process mortality data for years 2000 to 2018

/* This do file 
- reads in the raw mortality data 
- uses ICD 10 codes to generate cause of death categories
- aggregates deaths for each category by sociodemographic characteristics
- redistributes deaths with missing information for education (only applicable when 
  education is used)
*/

// Only run if changes were made to the CPS data or do files
/*
do "C:/Users/marie/Dropbox/NIH2020/Demography/2_do/1_CPS_March_read_merge_0018.do"
do "C:/Users/marie/Dropbox/NIH2020/Demography/2_do/2_read_cps_march_0018.do"
do "C:/Users/marie/Dropbox/NIH2020/Demography/2_do/3_aggregate_population_counts_0018.do"
*/

set more off

global dir `"C:/Users/marie/Dropbox/NIH2020/"'

cd "${dir}/Mortality/"

use "3_out data/mort_0018_complete.dta", clear


rename race race18 
gen race = .
replace race = 1 if race18 == 1
replace race = 2 if race18 == 2
replace race = 3 if race18 == 3
replace race = 4 if race18 == .
lab var race "Race/ethnicity"
label define racelab 1 "White" 2 "Black" 3 "Hispanic" 4 "Other"
label values race racelab
drop race18

/*
quietly {
    log using filename.txt, text replace
    noisily codebook
    log close
}
*/

drop if age_gp == .
keep if year == 2000 | year == 2015

keep  icd10 year race sex age_gp edclass
gen str1 xcode = icd10 


///////////////////////////////////////////////////////////////////////////////

// specific causes of death (using icd codes)

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

// specific causes of death (using icd codes)

//////////////////////////////////////////////////////////////////////////////
// Communicable Diseases

//<1 - 0% attributable to alcohol = allcause
gen com_allcause=0
forval i = 00(1)09 {
replace com_allcause=1 if icd10=="A`i'"
}
forval i = 20(1)38 {
replace com_allcause=1 if icd10=="A`i'"
}
forval i = 300(1)394 {
replace com_allcause=1 if icd10=="A`i'"
}
forval i = 398(1)469 {
replace com_allcause=1 if icd10=="A`i'"
}
forval i = 482(1)499 {
replace com_allcause=1 if icd10=="A`i'"
}
forval i = 65(1)69 {
replace com_allcause=1 if icd10=="A`i'"
}
forval i = 75(1)79 {
replace com_allcause=1 if icd10=="A`i'"
}
forval i = 80(1)99 {
replace com_allcause=1 if icd10=="A`i'"
}
forval i = 00(1)09 {
replace com_allcause=1 if icd10=="B`i'"
}
replace com_allcause=1 if icd10=="B10"
forval i = 15(1)17 {
replace com_allcause=1 if icd10=="B`i'"
}
replace com_allcause= 1 if icd10=="B19"
forval i = 250(1)331 {
replace com_allcause=1 if icd10=="B`i'"
}
forval i = 333(1)89 {
replace com_allcause=1 if icd10=="B`i'"
}
forval i = 91(1)99 {
replace com_allcause=1 if icd10=="B`i'"
}
forval i = 50(1)53 {
replace com_allcause=1 if icd10=="D`i'"
}
replace com_allcause=1  if icd10=="D649"
forval i = 00(1)02 {
replace com_allcause=1 if icd10=="E`i'"
}
forval i = 40(1)46 {
replace com_allcause=1 if icd10=="E`i'"
}
forval i = 50(1)64 {
replace com_allcause=1 if icd10=="E`i'"
}
forval i = 00(1)05 {
replace com_allcause=1 if icd10=="G`i'"
}
replace com_allcause=1 if icd10=="G14"
forval i = 65(1)66 {
replace com_allcause=1 if icd10=="H`i'"
}
forval i = 00(1)06 {
replace com_allcause=1 if icd10=="J`i'"
}
forval i = 17(1)18 {
replace com_allcause=1 if icd10=="J`i'"
}
replace com_allcause=1 if icd10=="J22"
replace com_allcause=1 if icd10=="N72"
forval i = 00(1)99 {
replace com_allcause=1 if icd10=="O`i'"
}
forval i = 00(1)22 {
replace com_allcause=1 if icd10=="P`i'"
}
forval i = 235(1)369 {
replace com_allcause=1 if icd10=="P`i'"
}
forval i = 371(1)969 {
replace com_allcause=1 if icd10=="P`i'"
}
forval i = 00(1)03 {
replace com_allcause=1 if icd10=="U`i'"
}
forval i = 05(1)49 {
replace com_allcause=1 if icd10=="U`i'"
}
forval i = 82(1)85 {
replace com_allcause=1 if icd10=="U`i'"
}

//1-29% attributable to alcohol = LR
gen com_LR=0
forval i = 15(1)19 {
replace com_LR=1 if icd10=="A`i'"
}
replace com_LR=1 if icd10=="A395"
replace com_LR=1 if icd10=="A481"
forval i = 50(1)64 {
replace com_LR=1 if icd10=="A`i'"
}
forval i = 70(1)74 {
replace com_LR=1 if icd10=="A`i'"
}
forval i = 20(1)24 {
replace com_LR=1 if icd10=="B`i'"
}
replace com_LR=1 if icd10=="B332"
replace com_LR=1 if icd10=="B90"
forval i = 06(1)16 {
replace com_LR=1 if icd10=="J`i'"
}
replace com_LR=1 if icd10=="J20"
replace com_LR=1 if icd10=="J21"
forval i = 70(1)71 {
replace com_LR=1 if icd10=="N`i'"
}
forval i = 73(1)74 {
replace com_LR=1 if icd10=="N`i'"
}
forval i = 230(1)234 {
replace com_LR=1 if icd10=="P`i'"
}
replace com_LR=1 if icd10=="P370"
replace com_LR=1 if icd10=="U04"

//30-99% attributable to alcohol
gen com_HR=0
replace com_HR=1 if icd10=="B18"

//////////////////////////////////////////////////////////////////////////////
//Noncommunicable disease

//0-1% attributable to alcohol
gen ncd_allcause=0
forval i = 30(1)31 {
replace ncd_allcause=1 if icd10=="C`i'"
}
forval i = 35(1)39 {
replace ncd_allcause=1 if icd10=="C`i'"
}
forval i = 40(1)49 {
replace ncd_allcause=1 if icd10=="C`i'"
}
forval i = 51(1)58 {
replace ncd_allcause=1 if icd10=="C`i'"
}
replace ncd_allcaus=1 if icd10=="C60"
forval i = 62(1)63 {
replace ncd_allcause=1 if icd10=="C`i'"
}
forval i = 64(1)97 {
replace ncd_allcause=1 if icd10=="C`i'"
}
forval i = 014(1)019 {
replace ncd_allcause=1 if icd10=="D`i'"
}
forval i = 024(1)049 {
replace ncd_allcause=1 if icd10=="D`i'"
}
forval i = 06(1)09 {
replace ncd_allcause=1 if icd10=="D`i'"
}
replace ncd_allcaus=1 if icd10=="D109"
forval i = 140(1)141 {
replace ncd_allcause=1 if icd10=="D`i'"
}
forval i = 144(1)239 {
replace ncd_allcause=1 if icd10=="D`i'"
}
forval i = 15(1)23 {
replace ncd_allcause=1 if icd10=="D`i'"
}
forval i = 25(1)36 {
replace ncd_allcause=1 if icd10=="D`i'"
}
replace ncd_allcaus=1 if icd10=="D370"
forval i = 376(1)377 {
replace ncd_allcause=1 if icd10=="D`i'"
}
forval i = 390(1)485 {
replace ncd_allcause=1 if icd10=="D`i'"
}
forval i = 487(1)489 {
replace ncd_allcause=1 if icd10=="D`i'"
}
forval i = 550(1)648 {
replace ncd_allcause=1 if icd10=="D`i'"
}
forval i = 650(1)867 {
replace ncd_allcause=1 if icd10=="D`i'"
}
forval i = 869(1)899 {
replace ncd_allcause=1 if icd10=="D`i'"
}
forval i = 03(1)07 {
replace ncd_allcause=1 if icd10=="E`i'"
}
forval i = 10(1)16 {
replace ncd_allcause=1 if icd10=="E`i'"
}
forval i = 200(1)243 {
replace ncd_allcause=1 if icd10=="E`i'"
}
forval i = 248(1)358 {
replace ncd_allcause=1 if icd10=="E`i'"
}
forval i = 65(1)68 {
replace ncd_allcause=1 if icd10=="E`i'"
}
forval i = 70(1)90 {
replace ncd_allcause=1 if icd10=="E`i'"
}
forval i = 00(1)09 {
replace ncd_allcause=1 if icd10=="F`i'"
}
forval i = 11(1)99 {
replace ncd_allcause=1 if icd10=="F`i'"
}
forval i = 06(1)09 {
replace ncd_allcause=1 if icd10=="G`i'"
}
forval i = 10(1)13 {
replace ncd_allcause=1 if icd10=="G`i'"
}
forval i = 20(1)26 {
replace ncd_allcause=1 if icd10=="G`i'"
}
forval i = 300(1)311 {
replace ncd_allcause=1 if icd10=="G`i'"
}
forval i = 318(1)328 {
replace ncd_allcause=1 if icd10=="G`i'"
}
forval i = 35(1)37 {
replace ncd_allcause=1 if icd10=="G`i'"
}
forval i = 43(1)44 {
replace ncd_allcause=1 if icd10=="G`i'"
}
replace ncd_allcause=1 if icd10=="G47"
forval i = 50(1)59 {
replace ncd_allcause=1 if icd10=="G`i'"
}
forval i = 600(1)620 {
replace ncd_allcause=1 if icd10=="G`i'"
}
forval i = 622(1)64{
replace ncd_allcause=1 if icd10=="G`i'"
}
forval i = 700(1)720 {
replace ncd_allcause=1 if icd10=="G`i'"
}
forval i = 722(1)737 {
replace ncd_allcause=1 if icd10=="G`i'"
}
forval i = 80(1)83 {
replace ncd_allcause=1 if icd10=="G`i'"
}
forval i = 90(1)99 {
replace ncd_allcause=1 if icd10=="G`i'"
}
forval i = 00(1)64 {
replace ncd_allcause=1 if icd10=="H`i'"
}
forval i = 67(1)95 {
replace ncd_allcause=1 if icd10=="H`i'"
}
forval i = 00(1)02 {
replace ncd_allcause=1 if icd10=="I`i'"
}
forval i = 00(1)02 {
replace ncd_allcause=1 if icd10=="I`i'"
}
forval i = 05(1)09 {
replace ncd_allcause=1 if icd10=="I`i'"
}
replace ncd_allcause=1 if icd10=="I10"
replace ncd_allcause=1 if icd10=="I12"
replace ncd_allcause=1 if icd10=="I15"
replace ncd_allcause=1 if icd10=="I131"
replace ncd_allcause=1 if icd10=="I139"
forval i = 26(1)28 {
replace ncd_allcause=1 if icd10=="I`i'"
}
forval i = 30(1)39 {
replace ncd_allcause=1 if icd10=="I`i'"
}
forval i = 44(1)47 {
replace ncd_allcause=1 if icd10=="I`i'"
}
replace ncd_allcause=1 if icd10=="I49"
replace ncd_allcause=1 if icd10=="I52"
forval i = 510(1)513 {
replace ncd_allcause=1 if icd10=="I`i'"
}
forval i = 516(1)519 {
replace ncd_allcause=1 if icd10=="I`i'"
}
forval i = 621(1)629{
replace ncd_allcause=1 if icd10=="I`i'"
}
forval i = 63(1)66 {
replace ncd_allcause=1 if icd10=="I`i'"
}
forval i = 672(1)679 {
replace ncd_allcause=1 if icd10=="I`i'"
}
forval i = 680(1)688 {
replace ncd_allcause=1 if icd10=="I`i'"
}
forval i = 693(1)698 {
replace ncd_allcause=1 if icd10=="I`i'"
}
forval i = 70(1)79 {
replace ncd_allcause=1 if icd10=="I`i'"
}
forval i = 80(1)83 {
replace ncd_allcause=1 if icd10=="I`i'"
}
forval i = 86(1)89 {
replace ncd_allcause=1 if icd10=="I`i'"
}
forval i = 95(1)97 {
replace ncd_allcause=1 if icd10=="I`i'"
}
replace ncd_allcause=1 if icd10=="I981"
forval i = 983(1)99 {
replace ncd_allcause=1 if icd10=="I`i'"
}
forval i = 30(1)99 {
replace ncd_allcause=1 if icd10=="J`i'"
}
forval i = 00(1)14 {
replace ncd_allcause=1 if icd10=="K`i'"
}
forval i = 20(1)291 {
replace ncd_allcause=1 if icd10=="K`i'"
}
forval i = 293(1)319 {
replace ncd_allcause=1 if icd10=="K`i'"
}
forval i = 35(1)64 {
replace ncd_allcause=1 if icd10=="K`i'"
}
forval i = 65(1)66 {
replace ncd_allcause=1 if icd10=="K`i'"
}
replace ncd_allcause=1 if icd10=="K678"
forval i = 710(1)712 {
replace ncd_allcause=1 if icd10=="K`i'"
}
replace ncd_allcause=1 if icd10=="K716"
replace ncd_allcause=1 if icd10=="K718"
replace ncd_allcause=1 if icd10=="K719"
forval i = 750(1)754 {
replace ncd_allcause=1 if icd10=="K`i'"
}
forval i = 761(1)765 {
replace ncd_allcause=1 if icd10=="K`i'"
}
replace ncd_allcause=1 if icd10=="K768"
replace ncd_allcause=1 if icd10=="K77"
forval i = 80(1)83 {
replace ncd_allcause=1 if icd10=="K`i'"
}
replace ncd_allcause=1 if icd10=="K87"
forval i = 90(1)92 {
replace ncd_allcause=1 if icd10=="K`i'"
}
forval i = 931(1)938 {
replace ncd_allcause=1 if icd10=="K`i'"
}
forval i = 00(1)99 {
replace ncd_allcause=1 if icd10=="L`i'"
}
forval i = 00(1)02 {
replace ncd_allcause=1 if icd10=="M`i'"
}
replace ncd_allcause=1 if icd10=="M030"
replace ncd_allcause=1 if icd10=="M032"
replace ncd_allcause=1 if icd10=="M036"
forval i = 05(1)48 {
replace ncd_allcause=1 if icd10=="M`i'"
}
forval i = 50(1)68 {
replace ncd_allcause=1 if icd10=="M`i'"
}
forval i = 70(1)72 {
replace ncd_allcause=1 if icd10=="M`i'"
}
replace ncd_allcause=1 if icd10=="M738"
forval i = 75(1)99 {
replace ncd_allcause=1 if icd10=="M`i'"
}
forval i = 00(1)51 {
replace ncd_allcause=1 if icd10=="N`i'"
}
forval i = 61(1)64 {
replace ncd_allcause=1 if icd10=="N`i'"
}
forval i = 75(1)99 {
replace ncd_allcause=1 if icd10=="N`i'"
}
forval i = 00(1)99 {
replace ncd_allcause=1 if icd10=="Q`i'"
}
forval i = 00(1)77 {
replace ncd_allcause=1 if icd10=="R`i'"
}
forval i = 781(1)99 {
replace ncd_allcause=1 if icd10=="R`i'"
}

//2-29% attributable to alcohol
gen ncd_LR=0
forval i = 00(1)26 {
replace ncd_LR=1 if icd10=="C`i'"
}
replace ncd_LR=1 if icd10=="C32"
forval i = 33(1)34 {
replace ncd_LR=1 if icd10=="C`i'"
}
replace ncd_LR=1 if icd10=="C50"
replace ncd_LR=1 if icd10=="C61"
forval i = 000(1)008 {
replace ncd_LR=1 if icd10=="D`i'"
}
forval i = 010(1)013 {
replace ncd_LR=1 if icd10=="D`i'"
}
forval i = 020(1)023 {
replace ncd_LR=1 if icd10=="D`i'"
}
replace ncd_LR=1 if icd10=="D05"
forval i = 100(1)107 {
replace ncd_LR=1 if icd10=="D`i'"
}
replace ncd_LR=1 if icd10=="D11"
replace ncd_LR=1 if icd10=="D12"
replace ncd_LR=1 if icd10=="D13"
forval i = 142(1)143 {
replace ncd_LR=1 if icd10=="D`i'"
}
replace ncd_LR=1 if icd10=="D24"
forval i = 371(1)375 {
replace ncd_LR=1 if icd10=="D`i'"
}
replace ncd_LR=1 if icd10=="D379"
replace ncd_LR=1 if icd10=="D38"
replace ncd_LR=1 if icd10=="D486"
replace ncd_LR=1 if icd10=="D868"
forval i = 40(1)41 {
replace ncd_LR=1 if icd10=="G`i'"
}
replace ncd_LR=1 if icd10=="G45"
replace ncd_LR=1 if icd10=="G46"
replace ncd_LR=1 if icd10=="I11"
replace ncd_LR=1 if icd10=="I130"
replace ncd_LR=1 if icd10=="I132"
forval i = 20(1)25 {
replace ncd_LR=1 if icd10=="I`i'"
}
replace ncd_LR=1 if icd10=="I40"
replace ncd_LR=1 if icd10=="I41"
forval i = 420(1)425 {
replace ncd_LR=1 if icd10=="I`i'"
}
forval i = 427(1)429 {
replace ncd_LR=1 if icd10=="I`i'"
}
replace ncd_LR=1 if icd10=="I43"
replace ncd_LR=1 if icd10=="I48"
replace ncd_LR=1 if icd10=="I50"
replace ncd_LR=1 if icd10=="I514"
replace ncd_LR=1 if icd10=="I515"
forval i = 600(1)620 {
replace ncd_LR=1 if icd10=="I`i'"
}
forval i = 670(1)671 {
replace ncd_LR=1 if icd10=="I`i'"
}
forval i = 681(1)682 {
replace ncd_LR=1 if icd10=="I`i'"
}
forval i = 690(1)692 {
replace ncd_LR=1 if icd10=="I`i'"
}
replace ncd_LR=1 if icd10=="I980"
forval i = 670(1)673 {
replace ncd_LR=1 if icd10=="K`i'"
}
forval i = 850(1)851 {
replace ncd_LR=1 if icd10=="K`i'"
}
forval i = 853(1)859 {
replace ncd_LR=1 if icd10=="K`i'"
}
forval i = 861(1)869 {
replace ncd_LR=1 if icd10=="K`i'"
}
replace ncd_LR=1 if icd10=="K930"
replace ncd_L=1 if icd10=="M031"
replace ncd_LR=1 if icd10=="M49"
replace ncd_LR=1 if icd10=="M730"
replace ncd_LR=1 if icd10=="M731"
replace ncd_LR=1 if icd10=="N60"
forval i = 41(1)42 {
replace ncd_LR=1 if icd10=="X`i'"
}
replace ncd_LR=1 if icd10=="X44"

//30-99% attributable to alcohol
gen ncd_HR=0
replace ncd_HR=1 if icd10=="I85"
replace ncd_HR=1 if icd10=="I982"
forval i = 713(1)715 {
replace ncd_HR=1 if icd10=="K`i'"
}
replace ncd_HR=1 if icd10=="K717"
forval i = 72(1)74 {
replace ncd_HR=1 if icd10=="K`i'"
}
forval i = 758(1)759 {
replace ncd_HR=1 if icd10=="K`i'"
}
replace ncd_HR=1 if icd10=="K760"
replace ncd_HR=1 if icd10=="K766"
replace ncd_HR=1 if icd10=="K767"
replace ncd_HR=1 if icd10=="K769"

//100% attributable to alcohol
gen ncd_AAF1=0
replace ncd_AAF1=1 if icd10=="E244"
replace ncd_AAF1=1 if icd10=="F10"
replace ncd_AAF1=1 if icd10=="G312"
replace ncd_AAF1=1 if icd10=="G621"
replace ncd_AAF1=1 if icd10=="G721"
replace ncd_AAF1=1 if icd10=="I426"
replace ncd_AAF1=1 if icd10=="K292"
replace ncd_AAF1=1 if icd10=="K70"
replace ncd_AAF1=1 if icd10=="K852"
replace ncd_AAF1=1 if icd10=="K860"
replace ncd_AAF1=1 if icd10=="R780"
replace ncd_AAF1=1 if icd10=="X45"


/////////////////////////////////////////////////////////////////////////
// Injury and external causes

//0-1% attributable to alcohol 
gen inj_allcause=0

forval i = 00(1)99 {
replace inj_allcause=1 if icd10=="S`i'"
}
forval i = 00(1)50 {
replace inj_allcause=1 if icd10=="T`i'"
}
forval i = 52(1)98 {
replace inj_allcause=1 if icd10=="T`i'"
}
forval i = 35(1)36 {
replace inj_allcause=1 if icd10=="Y`i'"
}
forval i = 40(1)84 {
replace inj_allcause=1 if icd10=="Y`i'"
}
forval i = 95(1)98 {
replace inj_allcause=1 if icd10=="Y`i'"
}

//2-29% attributable to alcohol
gen inj_LR=0

forval i = 00(1)99 {
replace inj_LR=1 if icd10=="V`i'"
}
forval i = 00(1)40 {
replace inj_LR=1 if icd10=="X`i'"
}
replace inj_LR=1 if icd10=="X43"
forval i = 46(1)59 {
replace inj_LR=1 if icd10=="X`i'"
}
forval i = 60(1)64 {
replace inj_LR=1 if icd10=="X`i'"
}
forval i = 66(1)84 {
replace inj_LR=1 if icd10=="X`i'"
}
forval i = 85(1)99 {
replace inj_LR=1 if icd10=="X`i'"
}
forval i = 00(1)09 {
replace inj_LR=1 if icd10=="Y`i'"
}
forval i = 10(1)14 {
replace inj_LR=1 if icd10=="Y`i'"
}
forval i = 16(1)34 {
replace inj_LR=1 if icd10=="Y`i'"
}
forval i = 85(1)89 {
replace inj_LR=1 if icd10=="Y`i'"
}

//100% attributable to alcohol
gen inj_AAF1=0

replace inj_AAF1=1 if icd10=="T51"
replace inj_AAF1=1 if icd10=="X65"
replace inj_AAF1=1 if icd10=="Y15"
replace inj_AAF1=1 if icd10=="Y90"
replace inj_AAF1=1 if icd10=="Y91"

////////////////////////////////////////////////////////////////////////////

// Factors influencing health status and contact with health services

//0-1% attributable to alcohol
gen fac_allcause=0

forval i = 00(1)03 {
replace fac_allcause=1 if icd10=="Z`i'"
}
forval i = 041(1)501 {
replace fac_allcause=1 if icd10=="Z`i'"
}
forval i = 503(1)713 {
replace fac_allcause=1 if icd10=="Z`i'"
}
forval i = 715(1)720 {
replace fac_allcause=1 if icd10=="Z`i'"
}
forval i = 722(1)810 {
replace fac_allcause=1 if icd10=="Z`i'"
}
forval i = 812(1)999 {
replace fac_allcause=1 if icd10=="Z`i'"
}
forval i = 00(1)03 {
replace fac_allcause=1 if icd10=="U`i'"
}
forval i = 05(1)49 {
replace fac_allcause=1 if icd10=="U`i'"
}
forval i = 82(1)85 {
replace fac_allcause=1 if icd10=="U`i'"
}

//2-29% attributable to alcohol
gen fac_LR=0

replace fac_LR=1 if icd10=="U04"

//100% attributable to alcohol
gen fac_AAF1=0

replace fac_AAF1=1 if icd10=="Z040"
replace fac_AAF1=1 if icd10=="Z502"
replace fac_AAF1=1 if icd10=="Z714"
replace fac_AAF1=1 if icd10=="Z721"
replace fac_AAF1=1 if icd10=="Z811"


save "3_out data/1_allethn_mortbycause_0018.dta", replace

///////////////////////////////////////////////////////////////////////////////

//sum by COD 

use "3_out data/1_allethn_mortbycause_0018.dta", clear

gen one=1
bysort age_gp sex edclass race year: egen Tmort = total(one)
bysort age_gp sex edclass race year: egen CD_NRmort = total(com_allcause)
bysort age_gp sex edclass race year: egen CD_LRmort = total(com_LR)
bysort age_gp sex edclass race year: egen CD_HRmort = total(com_HR)
bysort age_gp sex edclass race year: egen NCD_NRmort = total(ncd_allcause)
bysort age_gp sex edclass race year: egen NCD_LRmort = total(ncd_LR)
bysort age_gp sex edclass race year: egen NCD_HRmort = total(ncd_HR)
bysort age_gp sex edclass race year: egen NCD_AAF1mort = total(ncd_AAF1)
bysort age_gp sex edclass race year: egen Inj_NRmort = total(inj_allcause)
bysort age_gp sex edclass race year: egen Inj_LRmort = total(inj_LR)
bysort age_gp sex edclass race year: egen Inj_AAF1mort = total(inj_AAF1)
bysort age_gp sex edclass race year: egen HS_NRmort = total(fac_allcause)
bysort age_gp sex edclass race year: egen lowriskmort = total(fac_LR)
bysort age_gp sex edclass race year: egen HS_AAF1mort = total(fac_AAF1)


by age_gp sex edclass race year, sort: keep if _n==1
keep year sex age_gp edclass race *mort // from MK code

save "3_out data/2_allethn_sumCOD_0018.dta", replace

///////////////////////////////////////////////////////////////////////////////

//assigning deaths without education information

use "3_out data/2_allethn_sumCOD_0018.dta", clear

reshape wide Tmort CD_NRmort CD_LRmort CD_HRmort NCD_NRmort NCD_LRmort NCD_HRmort NCD_AAF1mort Inj_NRmort Inj_LRmort ////
	Inj_AAF1mort HS_NRmort lowriskmor HS_AAF1mort, i(year age_gp sex race) j(edclass)
sum *mort99

foreach i in T CD_NR CD_LR CD_HR NCD_NR NCD_LR NCD_HR NCD_AAF1 Inj_NR Inj_LR Inj_AAF1 HS_NR lowrisk HS_AAF1 {
gen MORT`i' = `i'mort1 + `i'mort2 + `i'mort3
	replace `i'mort1 = `i'mort1 + (`i'mort1/MORT`i') * `i'mort99 if MORT`i' != 0 
	replace `i'mort2 = `i'mort2 + (`i'mort2/MORT`i') * `i'mort99 if MORT`i' != 0 
	replace `i'mort3 = `i'mort3 + (`i'mort3/MORT`i') * `i'mort99 if MORT`i' != 0 
}

keep year sex race age_gp *mort1 *mort2 *mort3   

reshape long Tmort CD_NRmort CD_LRmort CD_HRmort NCD_NRmort NCD_LRmort NCD_HRmort NCD_AAF1mort Inj_NRmort Inj_LRmort ////
	Inj_AAF1mort HS_NRmort lowriskmort HS_AAF1mort, i(year age_gp sex race) j(edclass)

save "3_out data/allethn_sumCOD_0018_final.dta", replace

