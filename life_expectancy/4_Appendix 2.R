

# Generate Appendix 2
dMort_out <- read.csv("SIMAH_workplace/life_expectancy/2_out_data/dMort_0018.csv")
for (i in 1:length(v.totals)){
      dMort_out[, v.rates[i]] <- (dMort_out[, v.rates[i]]*100000)
}

class(dMort_out$TPop)
dMort_out$TPop <- round(dMort_out$TPop, 0)
v.keep = c('SES','Sex', 'Year', 'Age group', 'N population',
           "All cause", "Liver disease & cirrhosis",
           "Diabetes mellitus", "IHD & ischemic stroke", 
           "Hypertensive heart disease", 
           "Alcohol use disorder", "Unintentional injury*", 
           "MVA", "Suicide", "Rest",
           "All cause rate", "Liver disease & cirrhosis rate",
           "Diabetes mellitus rate", "IHD & ischemic stroke rate", 
           "Hypertensive heart disease rate", 
           "Alcohol use disorder rate", "Unintentional injury* rate", 
           "MVA rate", "Suicide rate", "Rest rate")
setnames(dMort_out, old = c('edclass', 'sex', 'year', 'age_gp', 'TPop',
                            "Tmort", "LVDCmort", "DMmort", "IHDmort", "HYPHDmort",
                            "AUDmort", "UIJmort", "MVACCmort", "IJmort", "RESTmort", 
                            v.rates), 
         new = v.keep)
dMort_out$SES <- recode(dMort_out$SES, "College" = "High", "SomeC" = "Middle",  "LEHS" = "Low")
dMort_out$Sex <- as.factor(dMort_out$Sex)
dMort_out$Sex <- recode(dMort_out$Sex, "1" = "Men", "2" = "Women")
dMort_out$`Age group` <- recode(dMort_out$`Age group`, "18" = "18-24",
                                "25" = "25-29",
                                "30" = "30-34",
                                "35" = "35-39",
                                "40" = "40-44",
                                "45" = "45-49",
                                "50" = "50-54",
                                "55" = "55-59",
                                "60" = "60-64",
                                "65" = "65-69",
                                "70" = "70-74",
                                "75" = "75-79",
                                "80" = "80+")
dMort_out <- dMort_out %>% select(v.keep)
write.csv(dMort_out, "SIMAH_workplace/mortality/3_out data/appendix2_rates_0018_final.csv", row.names = F)
remove(dMort_out, v.keep)
