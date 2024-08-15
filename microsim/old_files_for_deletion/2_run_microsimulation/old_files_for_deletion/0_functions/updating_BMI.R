updating_BMI <- function(data, CDF,y, ...){
  
CDFs <- brfssorig %>% filter(YEAR==y) %>% 
  mutate(
           alcCAT = cut(microsim.init.alc.gpd, breaks=c(-1,0,20,40,60,201),
                        labels=c("abstainer","0-20","21-40","41-60","61+")),
           agecat = cut(microsim.init.age, breaks=c(0,24,34,44,54,64,74,100),
                        labels=c("18-24","25-34","35-44","45-54","55-64","65-74","75+")))
  
data <- data %>% mutate(agecat = cut(microsim.init.age, breaks=c(0,24,34,44,54,64,74,100),
                                       labels=c("18-24","25-34","35-44","45-54","55-64","65-74","75+")),
                          alcCAT = cut(microsim.init.alc.gpd, breaks=c(-1,0,20,40,60,201),
                                       labels=c("abstainer","0-20","21-40","41-60","61+")))
  
Nsmicrosim <- data %>% 
    group_by(microsim.init.sex, agecat,alcCAT) %>% tally(name="nmicrosim")
NsBRFSS <- CDFs %>% group_by(microsim.init.sex, agecat,alcCAT) %>% tally(name="nbrfss") %>% 
    mutate(microsim.init.sex = recode(microsim.init.sex, "Male"="m","Female"="f"))
Ns <- inner_join(Nsmicrosim, NsBRFSS, by=c("microsim.init.sex",
                                          "agecat","alcCAT")) %>% group_by(microsim.init.sex, agecat, alcCAT) %>% 
    mutate(torank = nmicrosim) %>% ungroup() %>% 
    dplyr::select(microsim.init.sex, agecat, alcCAT, torank)
  
data <- left_join(data, Ns, by=c("microsim.init.sex",
                                 "agecat","alcCAT"))

data$cat <- paste(data$microsim.init.sex, data$agecat, data$alcCAT, sep="_")
  
distribution <- data %>% group_by(microsim.init.sex, agecat, alcCAT) %>% 
    mutate(rank = ntile(microsim.init.BMI, torank)) %>% dplyr::select(microsim.init.id, microsim.init.sex, agecat, alcCAT,
                                                                      microsim.init.BMI, rank)
  
CDFs <- left_join(CDFs, Ns, by=c("microsim.init.sex",
                                 "agecat","alcCAT"))
  
brfss <- CDFs %>% mutate(cat = paste(microsim.init.sex, agecat, alcCAT, sep="_")) %>% 
  filter(cat %in% unique(data$cat)) %>% group_by(microsim.init.sex, agecat, alcCAT) %>% sample_n(torank, replace=T) %>% 
    mutate(rank = ntile(microsim.init.BMI, torank)) %>% rename(BRFSSBMI= microsim.init.BMI) %>% dplyr::select(microsim.init.sex, agecat, 
                                                                                                 alcCAT,
                                                                                                 BRFSSBMI, rank)
  
distribution <- left_join(distribution, brfss, by=c("rank","microsim.init.sex","agecat","alcCAT"))
  
distribution$quotient <- distribution$BRFSSBMI / distribution$microsim.init.BMI
distribution$newBMI <- distribution$microsim.init.BMI*distribution$quotient

distribution <- distribution %>% ungroup() %>% 
    dplyr::select(microsim.init.id, newBMI)
  
# Ns <- data.frame(NmicrosimM = nrow(subset(data, microsim.init.sex=="m")),
#                    NmicrosimF = nrow(subset(data, microsim.init.sex=="f")),
#                    NbrfssM = nrow(subset(CDF, SEX=="m" & YEAR==y)),
#                    NbrfssF = nrow(subset(CDF, SEX=="f" & YEAR==y)),
#                    summicrosim = nrow(data),
#                    sumbrfss = nrow(subset(CDF, YEAR==y)))
# Ns$largestM <- names(which.max(Ns[c(1,3)]))
# Ns$largestF <- names(which.max(Ns[c(2,4)]))
#   
# Msample <- ifelse(Ns$largestM == "NmicrosimM", Ns$NbrfssM, Ns$NmicrosimM) 
# Fsample <- ifelse(Ns$largestF == "NmicrosimF", Ns$NbrfssF, Ns$NmicrosimF)
# 
# distributionM <- data.frame(microsim.init.id = 
#                               subset(data, microsim.init.sex=="m")$microsim.init.id,
#                             YEAR= y, SEX = "m", BMI = subset(data, microsim.init.sex=="m")$microsim.init.BMI) %>% 
#   mutate(rank = ntile(BMI, Msample))
# 
# distributionF <- data.frame(microsim.init.id = 
#                               subset(data, microsim.init.sex=="f")$microsim.init.id,
#                             YEAR= y, SEX = "f", BMI = subset(data, microsim.init.sex=="f")$microsim.init.BMI) %>% 
#   mutate(rank = ntile(BMI, Fsample))
# 
# distribution <- rbind(distributionM, distributionF)
# 
# brfssdist1 <- CDF %>% filter(YEAR==y, SEX=="m") %>% sample_n(Msample) %>% 
#   mutate(rank = ntile(BMI, Msample)) %>% dplyr::select(-prob) %>% 
#   rename(BRFSSBMI = BMI)
# 
# # brfssdist1 <- CDF %>% filter(YEAR==y, SEX=="m") %>% 
# #   mutate(rank = ntile(GPD, nM)) %>% dplyr::select(-prob) %>% 
# #   rename(BRFSSGPD = GPD)
# 
# brfssdist2 <- CDF %>% filter(YEAR==y, SEX=="f") %>% sample_n(Fsample) %>% 
#   mutate(rank = ntile(BMI, Fsample)) %>% dplyr::select(-prob) %>% 
#   rename(BRFSSBMI = BMI)
# 
# # brfssdist2 <- CDF %>% filter(YEAR==y, SEX=="f") %>% 
# #   mutate(rank = ntile(GPD, nF)) %>% dplyr::select(-c(prob)) %>% 
# #   rename(BRFSSGPD = GPD)
# 
# brfss <- rbind(brfssdist1, brfssdist2) %>% dplyr::select(-c(YEAR))
# 
# brfss %>% group_by(SEX) %>% summarise(mean(BRFSSBMI))
# 
# distribution <- left_join(distribution, brfss, by=c("rank","SEX"))
# summary(distribution)
# 
# distribution %>% group_by(SEX) %>% summarise(mean(BRFSSBMI))
# 
# distribution$quotient <- distribution$BRFSSBMI / distribution$BMI
# distribution$newBMI <- distribution$BMI*distribution$quotient
# distribution$newBMI[is.na(distribution$newBMI)] <- distribution$BRFSSBMI[is.na(distribution$newBMI)]
# summary(distribution)
# distribution %>% group_by(SEX) %>%
#   summarise(mean(newBMI))
# distribution <- distribution %>% 
#   dplyr::select(microsim.init.id, newBMI)
return(distribution)
}

