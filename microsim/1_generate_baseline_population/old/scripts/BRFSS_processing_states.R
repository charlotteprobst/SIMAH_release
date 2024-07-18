#####BRFSS processing for micro-synthesis 
brfss <- read_csv("SIMAH_workplace/microsim/brfssdata/BRFSS2000_states.csv")

brfss$RACE <- recode(brfss$RACE, "White"="WHI", "Black"="BLA", "Hispanic"="SPA", "Other"="OTH")
brfss$SEX <- recode(brfss$SEX, '1'="M", '2'="F")
brfss$EMPLOYED <- recode(brfss$EMPLOYED, "1"="employed", "0"="unemployed")
brfss$CHILDREN <- recode(brfss$CHILDREN, "1"="parent", "0"="notparent")
brfss$MARRIED <- recode(brfss$MARRIED, "1"="married", "0"="unmarried")
brfss$EDUCATION <- recode(brfss$EDUCATION, "1"="LEHS", "2"="SomeC", "3"="College")

##age categories 
brfss <- brfss %>% select(STATE, region, SEX, AGE, CHILDREN, RACE,
                          EMPLOYED, MARRIED, EDUCATION, BMI,
                          INCOMENEW, DRINKINGSTATUS, ALCGPD,
                          ALCDAYS2)

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
brfss$agecat <- factor(brfss$agecat)

selected <- brfss %>% filter(STATE==State) %>% 
  drop_na()

