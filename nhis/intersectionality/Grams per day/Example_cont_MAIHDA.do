use "G:\My Drive\RESEARCH ATTACHMENTS\Functional capacity & Intersectionality\Data and analysis\walking_speed_clean.dta", clear

*socio-dems: sex qual3 total_wealth_3 country_birth race

*controls: age height


*outcome: wlk_speed_WV9

*missing
foreach var of varlist sex qual3 total_wealth_3 country_birth race {
	drop if `var'==.
}


*to generate the intersections
egen intersections = group(sex qual3 total_wealth_3 country_birth race)
egen justoneintersections = tag(intersections)

//egen justone2 = tag(intersections1)

*generate constant for runmlwin
gen cons=1

*generate dummy variables for runmlwin to being able to calculate predicted effects:
foreach var of varlist sex qual3 total_wealth_3 country_birth race {
tabulate `var', generate(`var'_dum)
}

*maihda, null model
sort intersections id
runmlwin wlk_speed_WV9 cons, level2(intersections: cons) level1(id: cons) nopause
estat ic
display [RP2]var(cons)/([RP2]var(cons) + [RP1]var(cons)) //estimate icc/vpc
runmlwin wlk_speed_WV9 cons, level2(intersections: cons) level1(id: cons)  nopause mcmc(burnin(5000) chain(50000)) initsprevious
display [RP2]var(cons)/([RP2]var(cons) + [RP1]var(cons)) //estimate icc/vpc
}

*maihda, main effects model
sort intersections id
runmlwin wlk_speed_WV9 i.sex i.qual3 i.total_wealth_3 i.country_birth i.race cons, level2(intersections: cons) level1(id: cons) nopause
estat ic
display [RP2]var(cons)/([RP2]var(cons) + [RP1]var(cons)) //estimate icc/vpc
runmlwin wlk_speed_WV9 i.sex i.qual3 i.total_wealth_3 i.country_birth i.race cons, level2(intersections: cons) level1(id: cons)  nopause mcmc(burnin(5000) chain(50000)) initsprevious
display [RP2]var(cons)/([RP2]var(cons) + [RP1]var(cons)) //estimate icc/vpc


*to produce graphs
sort intersections id
runmlwin wlk_speed_WV9 i.sex i.qual3 i.total_wealth_3 i.country_birth i.race cons, level2(intersections: cons) level1(id: cons) nopause
estat ic
display [RP2]var(cons)/([RP2]var(cons) + [RP1]var(cons)) //estimate icc/vpc
runmlwin wlk_speed_WV9 i.sex i.qual3 i.total_wealth_3 i.country_birth i.race cons, level2(intersections: cons, residuals(u)) level1(id: cons)  nopause mcmc(burnin(5000) chain(50000)) initsprevious
display [RP2]var(cons)/([RP2]var(cons) + [RP1]var(cons)) //estimate icc/vpc

*quick and easy graph
keep if justoneintersections==1
sort intersections
gen graphorder = [_n]

serrbar u0 u0se graphorder, scale(1.96)

*graphs v2
sort intersections id
runmlwin wlk_speed_WV9 sex_dum1 sex_dum2 qual3_dum1 qual3_dum2 qual3_dum3 total_wealth_3_dum1 total_wealth_3_dum2 total_wealth_3_dum3 country_birth_dum1 country_birth_dum2 race_dum1 race_dum2 cons, level2(intersections: cons) level1(id: cons) nopause
estat ic
display [RP2]var(cons)/([RP2]var(cons) + [RP1]var(cons)) //estimate icc/vpc

runmlwin wlk_speed_WV9 sex_dum1 sex_dum2 qual3_dum1 qual3_dum2 qual3_dum3 total_wealth_3_dum1 total_wealth_3_dum2 total_wealth_3_dum3 country_birth_dum1 country_birth_dum2 race_dum1 cons, level2(intersections: cons, residuals(u)) level1(id: cons)  nopause mcmc(burnin(5000) chain(50000)) initsprevious
display [RP2]var(cons)/([RP2]var(cons) + [RP1]var(cons)) //estimate icc/vpc

*predicted effects for graph
predict yhat_wlk_speed_WV9
predict yhat_wlk_speed_WV9_se, stdp
gen mean=yhat_wlk_speed_WV9+u0
gen se=(sqrt((yhat_wlk_speed_WV9_se*yhat_wlk_speed_WV9_se)+(u0se*u0se)))/2

*to produce graphs
drop intersections justoneintersections
fillin sex qual3 total_wealth_3 country_birth race
egen intersections = group(sex qual3 total_wealth_3 country_birth race)
egen justoneintersections = tag(intersections)
keep if justoneintersections==1
sort intersections

separate mean, by(intersections) veryshortlabel

gen upper = mean + (1.96*se)
gen lower = mean - (1.96*se) 
separate upper, by(intersections) veryshortlabel
separate lower, by(intersections) veryshortlabel 

levelsof intersections, local(INTERSECTIONS)

twoway scatter mean intersections, legend(off) title("HbA1c, age 40-49") ytitle("mmol/mol") yscale(range(.1 1.3)) ylabel(.1(.1)1.3)  xlabel(0 10 20 30 40 50 60 70, nolabel notick) xtitle(Women								        	Men, margin(small)) cmissing(y) xline(75.5, lstyle(major_grid) lwidth(thick)) xline(0 10 20 30 40 50 60 70, lstyle(major_grid) lwidth(medthick)) scheme(plottig) 

twoway scatter mean se intersections4, title("HbA1c", size(medium)) ytitle("Predicted HbA1c per SD increase in GBP volatility (7 days)") scheme(plottig) yscale(range(-1500 2500)) ylabel(-1500(500)2500) xlabel(9.5 18.5 27.5 36.5 45.5 54.5 63.5 72.5 81.5 90.5 99.5 108.5, nolabel notick) xtitle(Women								        	Men, margin(small)) cmissing(y) xline(54.5, lstyle(major_grid) lwidth(thick)) xline(0.5 9.5 18.5 27.5 36.5 45.5 54.5 63.5 72.5 81.5 90.5 99.5 108.5, lstyle(major_grid) lwidth(medthick)) xmlabel(5 "White British" 14 "Chinese" 23 "Indian" 32 "Pakistani" 41 "Caribbean" 49.5 "African" 59 "White British" 68 "Chinese" 77 "Indian" 86 "Pakistani" 95 "Caribbean" 104 "African", nogrid notick angle(0)) || rcap upper lower intersections4, lc(black) legend(off) graphregion(margin(1 1 1 1))




foreach var of varlist hba1c_std chol_std hscrp_std sysval_std pulse_std bmival_std  {
quietly runmlwin `var' c.age c.age2 cons, level2(intersections1: cons) level1(id: cons) nopause
estat ic
display [RP2]var(cons)/([RP2]var(cons) + [RP1]var(cons)) //estimate icc/vpc
}