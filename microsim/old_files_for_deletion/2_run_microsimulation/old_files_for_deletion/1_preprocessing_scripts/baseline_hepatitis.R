# first read in the chronic HBV/HCV data for the baseline year (1990)
# this is calculated from NHANES data in the repos/Hepatitis folder
HepPrevalence <- read.csv("SIMAH_workplace/microsim/1_input_data/HepatitisPrevalence.csv")
HepPrevalence$hepcat <- paste(HepPrevalence$SEX, HepPrevalence$RACE, HepPrevalence$AGECAT, sep="_")
HepPrevalence <- HepPrevalence %>% dplyr::select(hepcat, HBV, HCV)

# create category for binding with the microsim data 
basepop <- basepop %>% mutate(catage=cut(microsim.init.age,
                 breaks=c(0,24,34,44,64,100),
                 labels=c("18-24","25-34","35-44","45-64","65+")),
                 SEX=ifelse(microsim.init.sex=="f","F","M"))
basepop$hepcat <- paste(basepop$SEX,basepop$microsim.init.race, basepop$catage, sep="_")
basepop <- basepop%>% 
  dplyr::select(-c(catage,SEX))

basepop <- left_join(basepop, HepPrevalence)

summary(as.numeric(basepop$HBV))

# assign chronic hepatitis status for baseline
basepop$prob <- runif(nrow(basepop))
basepop$chronicB <- ifelse(basepop$prob<basepop$HBV, 1,0)
basepop$chronicC <- ifelse(basepop$prob<basepop$HCV, 1,0)

summary(as.factor(basepop$chronicB))/nrow(basepop)*100

summary(as.factor(basepop$chronicC))

basepop <- basepop %>% dplyr::select(-c(prob, HBV, HCV, hepcat))

# read in the HBV / HCV data 

HepB <- read_csv("SIMAH_workplace/microsim/1_input_data/Incidence_HepB.csv")
HepC <- read_csv("SIMAH_workplace/microsim/1_input_data/Incidence_HepC.csv")

# subset by location
# convert age cat to long form 

HepB$location_name <- ifelse(HepB$location_name=="United States of America","USA",
                             HepB$location_name)

HepC$location_name <- ifelse(HepC$location_name=="United States of America","USA",
                             HepC$location_name)

HepB <- HepB %>% 
  filter(location_name==SelectedState) %>% 
  gather(agecat, Incidence, `15 to 19`:`80 plus`) %>% mutate(HepB = Incidence*proportion) %>% 
  mutate(microsim.init.sex = recode(sex_name, "Male" = "m", "Female" ="f")) %>% 
  mutate(agecat = gsub(" to ", "-", agecat)) %>% 
  mutate(microsim.init.sex = as.factor(microsim.init.sex)) %>% 
  dplyr::select(microsim.init.sex, year, agecat, HepB)

HepC <- HepC %>% filter(location_name==SelectedState) %>% 
  gather(agecat, Incidence, `15 to 19`:`80 plus`) %>% mutate(HepC=Incidence*proportion) %>%
  mutate(microsim.init.sex=recode(sex_name, "Male"="m", "Female"="f")) %>% 
  mutate(agecat = gsub(" to ", "-", agecat)) %>% 
  mutate(microsim.init.sex=as.factor(microsim.init.sex)) %>% 
  dplyr::select(microsim.init.sex, year, agecat, HepC)

Hep <- left_join(HepB, HepC)
rm(HepB, HepC)

# read in the data on distributions of drinkers 
distribution <- read.csv("SIMAH_workplace/microsim/1_input_data/drinkers_distribution.csv")
names(distribution) <- gsub("X","",names(distribution))
names(distribution) <- gsub("\\.","",names(distribution))

# average drinking in population with HBV / HCV 41 g/day so subset the 40g/day distributions 
distribution <- distribution %>% dplyr::select(Distributionamongdrinkersin, Sex, `40gday`) %>% 
  mutate(`40gday` = str_trim(`40gday`, side=c("right")),
         `40gday`= as.numeric(`40gday`),
         percentage = `40gday` / 100) %>% rename(microsim.init.sex=Sex,
                                                   gpdcat = Distributionamongdrinkersin) %>% 
  dplyr::select(microsim.init.sex, gpdcat, percentage) %>% 
  mutate(gpdcat = gsub(" g/day","",gpdcat),
         gpdcat = gsub("g/day","",gpdcat),
         gpdcat = gsub("[[:space:]]","",gpdcat),
         gpdcat = str_trim(gpdcat, side=c("right")))

# assigned <- AssignAcuteHep(microsim, Hep, distribution, y)
# microsim <- basepop
# y <- 1984


