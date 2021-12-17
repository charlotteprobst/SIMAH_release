updating_alcohol <- function(data, CDF,y, ...){
  
# y <- ifelse(y==1996, 1997, 
#             ifelse(y==2000, 2001, 
#                    ifelse(y==1998, 1999, y)))

CDFs <- brfssorig %>% filter(YEAR==y) %>% 
    mutate(BMIcat = cut(microsim.init.BMI, breaks=c(0,18,25,30,100),
                      labels=c("15-18","19-25","26-30","31+")),
         agecat = cut(microsim.init.age, breaks=c(0,24,34,44,54,64,74,100),
                      labels=c("18-24","25-34","35-44","45-54","55-64","65-74","75+"))) %>% 
  dplyr::select(YEAR, microsim.init.sex, BMIcat, agecat,
                microsim.init.age, microsim.init.alc.gpd)

data <- data %>% mutate(agecat = cut(microsim.init.age, breaks=c(0,24,34,44,54,64,74,100),
                                           labels=c("18-24","25-34","35-44","45-54","55-64","65-74","75+")),
                        BMIcat = cut(microsim.init.BMI, breaks=c(0,18,25,30,100),
                                     labels=c("15-18","19-25","26-30","31+"))) 

Nsmicrosim <- data %>% 
  group_by(microsim.init.sex, agecat,BMIcat) %>% tally(name="nmicrosim")
NsBRFSS <- CDFs %>% group_by(microsim.init.sex, agecat,BMIcat) %>% tally(name="nbrfss")
  # mutate(microsim.init.sex = recode(microsim.init.sex, "Male"="m","Female"="f"))
Ns <- left_join(Nsmicrosim, NsBRFSS, by=c("microsim.init.sex",
                                          "agecat","BMIcat")) %>% group_by(microsim.init.sex, agecat, BMIcat) %>% 
  mutate(max = ifelse(nmicrosim>nbrfss, "microsim","brfss"),
         which = ifelse(max=="microsim","brfss","microsim"),
         torank = ifelse(which=="microsim",nmicrosim,nbrfss),
         torank=nmicrosim) %>% 
  dplyr::select(microsim.init.sex, agecat, BMIcat, torank)

data <- left_join(data, Ns, by=c("microsim.init.sex",
                                 "agecat","BMIcat"))

data$cat <- paste(data$microsim.init.sex, data$agecat, data$BMIcat, sep="_")
distribution <- data %>% group_by(microsim.init.sex, agecat, BMIcat) %>% 
  mutate(rank = ntile(microsim.init.alc.gpd, torank)) %>% 
  dplyr::select(microsim.init.id, microsim.init.sex, agecat, BMIcat,
                microsim.init.alc.gpd, rank)

CDFs <- left_join(CDFs, Ns, by=c("microsim.init.sex",
                                 "agecat","BMIcat"))

brfss <- CDFs %>% mutate(cat = paste(microsim.init.sex, agecat, BMIcat, sep="_")) %>% 
                           filter(cat %in% unique(data$cat)) %>% 
  group_by(microsim.init.sex, agecat, BMIcat) %>% sample_n(torank, replace=T) %>% 
  mutate(rank = ntile(microsim.init.alc.gpd, torank)) %>% rename(BRFSSGPD = microsim.init.alc.gpd) %>% 
  dplyr::select(microsim.init.sex, agecat, 
                BMIcat,BRFSSGPD, rank)

distribution <- left_join(distribution, brfss, by=c("rank","microsim.init.sex","agecat","BMIcat")) %>% 
  mutate(cat = paste0(microsim.init.sex, agecat, BMIcat, rank))

distribution$quotient <- distribution$BRFSSGPD / distribution$microsim.init.alc.gpd
distribution$newGPD <- distribution$microsim.init.alc.gpd*distribution$quotient
distribution$newGPD <- ifelse(distribution$microsim.init.alc.gpd==0 & distribution$BRFSSGPD>0, distribution$BRFSSGPD,
                              ifelse(distribution$microsim.init.alc.gpd==0 & distribution$BRFSSGPD==0, 0,
                              distribution$newGPD))
distribution <- distribution %>% ungroup() %>%
  dplyr::select(microsim.init.id, newGPD)
return(distribution)
}

