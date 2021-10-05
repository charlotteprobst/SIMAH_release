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

dropping <- F

# dropping groups not in BRFSS approach 
if(dropping==T){
  summary <- selected %>% mutate(SEX = as.factor(SEX),
                                 agecat = as.factor(agecat),
                                 EDUCATION=as.factor(EDUCATION),
                                 RACE=as.factor(RACE)) %>% 
    group_by(SEX,agecat, EDUCATION, RACE, .drop=FALSE) %>% 
    tally() %>% 
    mutate(cat = paste(RACE, SEX, agecat, EDUCATION, sep="")) %>% ungroup() %>% 
    select(cat, n) %>% 
    pivot_wider(names_from=cat, values_from=n)
summary <- summary %>% 
  select(sort(tidyselect::peek_vars())) %>% mutate(var="BRFSS")

cons <- cons %>% 
  select(sort(tidyselect::peek_vars())) %>% mutate(var="census")

compare <- rbind(cons, summary) %>% 
  pivot_longer(cols=c(BLAF18.24College:WHIM75.79SomeC)) %>% 
  pivot_wider(names_from=var, values_from=value) %>% 
  filter(BRFSS!=0) %>% select(name,census) %>% 
  pivot_wider(names_from=name, values_from=census)
cons <- compare

brfss <- selected
}else{
  missing <- selected %>% mutate(SEX = as.factor(SEX),
                                 agecat = as.factor(agecat),
                                 EDUCATION=as.factor(EDUCATION),
                                 RACE=as.factor(RACE)) %>% 
    group_by(SEX,agecat, EDUCATION, RACE, .drop=FALSE) %>% 
    tally() %>% 
    mutate(cat = paste(RACE, SEX, agecat, EDUCATION, sep="")) %>% ungroup() %>% 
    select(cat, n) %>% filter(n==0)
  missingcats <- unique(missing$cat)
  division <- unique(selected$region)
  toreplace <- brfss %>% drop_na() %>% mutate(cat = paste(RACE, SEX, agecat, EDUCATION, sep="")) %>% 
    filter(region==division) %>% 
    filter(cat %in% missingcats) %>% select(-c(cat))
  selected <- rbind(toreplace, selected)
  
  missing <- selected %>% mutate(SEX = as.factor(SEX),
                                 agecat = as.factor(agecat),
                                 EDUCATION=as.factor(EDUCATION),
                                 RACE=as.factor(RACE)) %>% 
    group_by(SEX,agecat, EDUCATION, RACE, .drop=FALSE) %>% 
    tally() %>% 
    mutate(cat = paste(RACE, SEX, agecat, EDUCATION, sep="")) %>% ungroup() %>% 
    select(cat, n) %>% filter(n==0)
  missingcats <- unique(missing$cat)
  if(length(missingcats>=1)){
    toreplace <- brfss %>% drop_na() %>% mutate(cat=paste(RACE,SEX,agecat,EDUCATION,sep="")) %>% 
      filter(cat %in% missingcats) %>% select(-c(cat)) %>% sample_n(10)
    selected <- rbind(toreplace, selected)
  }
  brfss <- selected
  }
  

