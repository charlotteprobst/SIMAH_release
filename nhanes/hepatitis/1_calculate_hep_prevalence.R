# SIMAH project 2022 
# analysis of NHANES survey data to get updated prevalence of chronic HBV / HCV to initialise simulation 
# (this is required as previous CASCADE version initialised in 1984 so the data used is out of date)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(tidyverse)
library(haven)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"

setwd(WorkingDirectory)

# read in the hepatitis data - from the 1999 - 2000 wave 
hep1999 <- read_xpt("SIMAH_workplace/nhanes/LAB02.XPT")
# codebook for this data is available online here: https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/LAB02.htm
# read in demographics data - needed to decompose prevalence by age, sex, race and education
demo1999 <- read_xpt("SIMAH_workplace/nhanes/DEMO.XPT")
# codebook for this data is available online here: https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.htm#DMDEDUC
hep1999 <- left_join(hep1999, demo1999) %>% mutate(year= "2001-2002") %>% 
  dplyr::select(year, LBXHBC, LBDHCV, DMDEDUC2, DMDEDUC3, DMDHRGND, RIDRETH1,
                RIDAGEYR, WTMEC2YR)

# combine with 2001 - 2002 wave for greater power 
hep2001 <- read_xpt("SIMAH_workplace/nhanes/L02_B.XPT")
# codebook here: https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/L02_B.htm
demo2001 <- read_xpt("SIMAH_workplace/nhanes/DEMO_B.XPT")
# codebook here: https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DEMO_B.htm
hep2001 <- left_join(hep2001, demo2001) %>% mutate(year = "2001-2002") %>% 
  dplyr::select(year, LBXHBC, LBDHCV, DMDEDUC2, DMDEDUC3, DMDHRGND, RIDRETH1,
                RIDAGEYR, WTMEC2YR)

hep <- rbind(hep1999, hep2001) %>%
mutate(ChronicHepB = ifelse(LBXHBC==1, 1,0),
         ChronicHepC = ifelse(LBDHCV==1, 1,0),
         EducAdult = ifelse(DMDEDUC2<=3, "LEHS",
                            ifelse(DMDEDUC2==4, "SomeC",
                                   ifelse(DMDEDUC2==5, "College", NA))),
         EducYouth = ifelse(DMDEDUC3<=66, "LEHS", NA),
         EducFinal = ifelse(is.na(EducAdult), EducYouth, EducAdult),
         Sex = ifelse(DMDHRGND==1, "m","f"),
         Race = ifelse(RIDRETH1<=2, "SPA",
                       ifelse(RIDRETH1==3, "WHI",
                              ifelse(RIDRETH1==4, "BLA", "OTH"))),
         Age = RIDAGEYR,
         Weight = WTMEC2YR) %>% dplyr::select(Age, Sex, Race, EducFinal, Weight, ChronicHepB, ChronicHepC) %>% 
  filter(Age<=79 & Age>=18) %>% 
  mutate(Agecat = cut(Age,
                      breaks=c(0,34,64,100),
                      labels=c("18-24","35-64","65+"))) %>% 
  drop_na(Sex, Agecat, Race, EducFinal, ChronicHepB, ChronicHepC)

hep %>% group_by(ChronicHepB) %>% 
  summarise(total=sum(Weight)) %>% ungroup() %>% 
  mutate(prevalence = total / sum(total)*100)

hep %>% group_by(ChronicHepC) %>% 
  summarise(total=sum(Weight)) %>% ungroup() %>% 
  mutate(prevalence = total / sum(total)*100)

prevalenceHepB <- hep %>% group_by(Sex, Agecat, Race, ChronicHepB) %>% 
  tally() %>% ungroup() %>% 
  group_by(Sex,Agecat, Race) %>% 
  mutate(PrevalenceHepB = n/sum(n)) %>% filter(ChronicHepB==1) %>% 
  dplyr::select(-c(ChronicHepB, n))

prevalenceHepC <- hep %>% group_by(Sex, Agecat, Race, ChronicHepC) %>% 
  tally() %>% ungroup() %>% 
  group_by(Sex, Agecat, Race) %>% 
  mutate(PrevalenceHepC = n/sum(n)) %>% filter(ChronicHepC==1) %>% 
  dplyr::select(-c(ChronicHepC, n))

# ggplot(data=prevalenceHepB, aes(x=Agecat, y=PrevalenceHepB, colour=EducFinal)) + geom_point() + 
#   facet_grid(cols=vars(Sex), rows=vars(Race))
# 
# ggplot(data=overallprevalence, aes(x=EducFinal, y=prevalencehepb, colour=Race, fill=Race)) + 
#   geom_bar(stat="identity", position="dodge") + 
#   facet_grid(rows=vars(Sex))

alldata <- expand.grid(Agecat = unique(hep$Agecat),
                       Sex = unique(hep$Sex),
                       Race = unique(hep$Race))

alldata <- left_join(alldata, prevalenceHepB)
alldata <- left_join(alldata, prevalenceHepC)

# set NAs to 0 prevalence
alldata$PrevalenceHepC[is.na(alldata$PrevalenceHepC)] <- 0

# save initial estimates of hepatitis b and c prevalence 
write.csv(alldata, "SIMAH_workplace/nhanes/hepatitis_b_c_prevalence.csv", row.names=F)
