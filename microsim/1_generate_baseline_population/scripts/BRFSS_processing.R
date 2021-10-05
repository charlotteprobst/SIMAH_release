#####BRFSS processing for micro-synthesis 

brfss <- read_csv("SIMAH_workplace/microsim/brfss_data/BRFSS_upshift_BMIUSA.csv") %>% filter(YEAR==2000)
brfss$RACE <- recode(brfss$RACE, "White"="WHI", "Black"="BLA", "Hispanic"="SPA", "Other"="OTH")
brfss$SEX <- recode(brfss$SEX, "Male"="M", "Female"="F")
brfss$EMPLOYED <- recode(brfss$EMPLOYED, "1"="employed", "0"="unemployed")
brfss$CHILDREN <- recode(brfss$CHILDREN, "1"="parent", "0"="notparent")
brfss$MARRIED <- recode(brfss$MARRIED, "1"="married", "0"="unmarried")
brfss$EDUCATION <- recode(brfss$EDUCATION, "1"="LEHS", "2"="SomeC", "3"="College")

##age categories 
brfss <- as.data.frame(brfss[,c("SEX", "AGE", "CHILDREN", "RACE", 
                                "EMPLOYED", "MARRIED", "EDUCATION", "BMI", "INCOMENEW", "DRINKINGSTATUS_NEW", "imputeddrinking", "alcgpd_new", 
                                "ALCDAYS2_shifted")])
agecats <- c("18-24","25-34","35-44","45-64","65+")

brfss$agecat <- cut(brfss$AGE,
                    breaks=c(0,24,34,44,64,80),
                    labels=c("18.24", "25.34", "35.44", "45.64","65."))

brfss$agecat <- cut(brfss$AGE,
                    breaks=c(0,24,34,44,54,64,79,100),
                    labels=c("18.24","25.34","35.44","45.54","55.64","65.79","80+"))

brfss <- brfss %>% filter(AGE<=79)

summary(as.factor(brfss$RACE))
summary(as.factor(brfss$agecat))
summary(as.factor(brfss$SEX))
summary(as.factor(brfss$EDUCATION))


# check that there is at least one BRFSS individual in each category in 2000 
nrow(brfss %>% group_by(RACE, SEX, EDUCATION, agecat) %>% tally())==120
