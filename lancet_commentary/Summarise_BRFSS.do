import delimited /Users/carolinkilian/Desktop/SIMAH_workplace/lancet_commentary/selected_brfss_full_2000_2022.csv, clear

* prepare data
drop if missing(sex_recode) | sex_recode == "."
drop if missing(education_summary) | education_summary == "."
drop if missing(gramsperday)

replace gramsperday = 200 if gramsperday > 200

gen alccat3 = 0
replace alccat3 = 1 if sex_recode == "Men" & gramsperday >= 60
replace alccat3 = 1 if sex_recode == "Women" & gramsperday >= 40

egen group = group(year sex_recode education_summary), label

* set svyset
egen stratum_new = group(year x_ststr)

svyset x_psu [pweight = final_sample_weight], singleunit(certainty) strata(stratum_new)

* get mean and SE 
svy: mean gramsperday, over(group)
svy: mean alccat3, over(group)
