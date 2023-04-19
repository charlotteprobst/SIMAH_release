set more off

global dir `"C:/Users/marie/Dropbox/NIH2020/SIMAH_workplace/"'

cd "${dir}/mortality/"


use "3_out data/mort_0021_complete.dta", clear

generate sample = runiformint(0, 10)
drop if sample != 1

save "3_out data/allethn_sumCOD_0021_10perc sample.dta", replace
outsheet using "${dir}mortality/3_out data/allethn_sumCOD_0021_10perc sample.csv" , comma replace
