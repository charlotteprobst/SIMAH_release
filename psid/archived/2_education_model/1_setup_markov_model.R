setup_markov_model <- function(data,y){
  if(y==2009){
  data <- data %>% filter(year<=2009)
  }else{
    data <- data %>% filter(year>=2009)
  }
  data$educNUM <- ifelse(data$education<=11, 1,
                      ifelse(data$education==12, 2,
                         ifelse(data$education==13, 3,
                                ifelse(data$education==14,4,
                                       ifelse(data$education==15,5,
                                              ifelse(data$education>=16,6,NA))))))
  
  data <- data %>% drop_na(sex,age,education,race_new_unique, total_fam_income)

  # remove anyone with only one year of data- this gives an error in MSM 
  data <- data %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
    filter(totalobservations>1) %>% mutate(sex=factor(sex),
                                           sex=ifelse(sex=="female",1,0))
  # all data needs to be ordered by ID then year
  data <- as.data.frame(lapply(data, unlist))
  data <- data[order(data$newID, data$year),]
  data$sex <- as.factor(data$sex)
  data$highestEd <- data$education
  source("SIMAH_code/education_transitions/2_analysis/cleaning_education_function2.R")
  backIDs <- getIDs(data)
  data <- data[!data$newID %in% backIDs,]
  data <- data %>% filter(age>=16)
  data$age <- round(data$age, digits=0)
  data$agesq <- data$age^2
  data$racefinal2 <- as.character(data$race_new_unique)
  data$racefinal2 <- ifelse(data$racefinal2=="Asian/PI","other",data$racefinal2)
  data$racefinal2 <- ifelse(data$racefinal2=="Native","other",data$racefinal2)
  data$racefinal2 <- as.factor(data$racefinal2)
  # data <- data %>% filter(racefinal2!="other")
  data$racefinal2 <- relevel(data$racefinal2, ref = "white")
  data <- data.frame(data)
  data <- as.data.frame(lapply(data, unlist))
  # remove absorbing transitions - i.e. anyone after they have reached 16 
  toremove <- data %>% group_by(newID) %>% 
    filter(educNUM==6) %>% 
    mutate(minyear = min(year),
           keep = ifelse(minyear==year, 1,0)) %>% 
    dplyr::select(newID, year, keep)
  data <- left_join(data, toremove)
  data$keep[is.na(data$keep)] <- 1
  data <- data %>% filter(keep==1)
  # remove those with 1 observation or less 
  toremove <- data %>% group_by(newID) %>% 
    tally() %>% mutate(toremove = ifelse(n==1, 1,0)) %>% 
    filter(toremove==0)
  IDS <- unique(toremove$newID)
  data <- data %>% filter(newID %in% IDS)
  data$agescaled <- as.numeric(scale(data$age, center=T))
  data$incomescaled <- as.numeric(scale(data$total_fam_income, center=T))
  data$agesqscaled <- as.numeric(scale(data$agesq, center=T))
  data <- data[order(data$newID, data$year),]
    return(data)
}

run_markov_model_baseline <- function(data){
  # Q matrix of allowed instantaneous transitions - only allowed to transition between 1-2 and 2-3 (and staying the same)
  Q <- rbind( c(0.5, 0.5, 0, 0, 0),
              c(0, 0.5, 0.5, 0, 0),
              c(0, 0, 0.5, 0.5, 0),
              c(0, 0, 0, 0.5, 0.5),
              c(0, 0, 0, 0, 0.5))
  data$racefinal2 <- as.character(data$individualrace)
  data$racefinal2 <- ifelse(data$racefinal2=="Asian/PI","other",data$racefinal2)
  data$racefinal2 <- ifelse(data$racefinal2=="Native","other",data$racefinal2)
  data$racefinal2 <- as.factor(data$racefinal2)
  data$racefinal2 <- relevel(data$racefinal2, ref = "white")
  data$agescaled <- as.numeric(scale(data$age, center=T))
  data$agesq <- data$age^2
  data$agesqscaled <- as.numeric(scale(data$agesq, center=T)) 
  model <- msm(educNUM~year, newID, data=data, qmatrix=Q,
                       center=FALSE,
                       covariates=~agescaled + agesqscaled + sex + racefinal2 + onecollegeplus)
  return(model)
}

run_markov_model_parent <- function(data){
  # Q matrix of allowed instantaneous transitions - only allowed to transition between 1-2 and 2-3 (and staying the same)
  Q <- rbind( c(0.5, 0.5, 0, 0, 0),
              c(0, 0.5, 0.5, 0, 0),
              c(0, 0, 0.5, 0.5, 0),
              c(0, 0, 0, 0.5, 0.5),
              c(0, 0, 0, 0, 0.5))
  data$racefinal2 <- as.character(data$racefinal2)
  data$racefinal2 <- ifelse(data$racefinal2=="Asian/PI","other",data$racefinal2)
  data$racefinal2 <- ifelse(data$racefinal2=="Native","other",data$racefinal2)
  data$racefinal2 <- as.factor(data$racefinal2)
  data$racefinal2 <- relevel(data$racefinal2, ref = "white")
  data$agescaled <- as.numeric(scale(data$age, center=T))
  data$agesqscaled <- as.numeric(scale(data$agesq, center=T)) 
  model <- msm(educNUM~year, newID, data=data, qmatrix=Q,
               center=FALSE,
               covariates=~agescaled + agesqscaled + sex + racefinal2*oneCollegeplus)
  return(model)
  
}

extract_coefficients_6cat <- function(model, type, timeperiod, data){
  dat <- print(model)
  if(length(dat)>1){
  dat <- t(dat)
  dat <- data.frame(dat)
  dat$names <- row.names(dat)
  origsample <- length(unique(data$uniqueID))
  expandedsample <- length(unique(data$newID))
  dat <- dat %>% 
    mutate(Type = ifelse(grepl("Estimate",names),"Estimate",
                         ifelse(grepl(".L", names), "Lower",
                                ifelse(grepl(".U",names),"Upper",NA)))) %>%
    mutate(Variable = gsub(".Estimate","",names),
           Variable = gsub(".L","",Variable),
           Variable = gsub(".U","",Variable)) %>% filter(Variable!="base.Fixed") %>% 
    rename("LHS->LHS"=State.1...State.1,
           "LHS->HS"=State.1...State.2,
           "HS->HS"=State.2...State.2,
           "HS->SomeC1"=State.2...State.3,
           "SomeC1->SomeC1"=State.3...State.3,
           "SomeC1->SomeC2"=State.3...State.4,
           "SomeC2->SomeC2"=State.4...State.4,
           "SomeC2->SomeC3"=State.4...State.5,
           "SomeC3->SomeC3"=State.5...State.5,
           "SomeC3->College"=State.5...State.6) %>% select(-c(names)) %>% 
    pivot_wider(names_from=Type, values_from=c("LHS->LHS":"SomeC3->College")) %>% 
    pivot_longer(`LHS->LHS_Estimate`:`SomeC3->College_Upper`) %>% 
    separate(name, into=c("Transition","Type"),sep="_") %>% 
    filter(Variable!="agescaled") %>% filter(Variable!="agesqscaled") %>% 
    filter(Variable!="base") %>% drop_na() %>%
    mutate(model = type, time = timeperiod) %>%
    pivot_wider(names_from=Type, values_from=value) %>%
    group_by(Variable, Transition, model, time) %>%
    mutate(SE = (log(Upper) - log(Lower)) / 3.92,
      SD = SE * sqrt(expandedsample),
      newSE = SD / sqrt(origsample), 
      newLower = log(Estimate) - (newSE * 1.96),
      newUpper = log(Estimate) + (newSE * 1.96),
      newLower = round(exp(newLower), digits=2),
      newUpper = round(exp(newUpper), digits=2),
      newUpper = ifelse(newUpper>100, 100, newUpper))
  }
  return(dat)
}

extract_coefficients <- function(model, type, timeperiod, data){
  dat <- print(model)
  if(length(dat)>1){
    dat <- t(dat)
    dat <- data.frame(dat)
    dat$names <- row.names(dat)
    origsample <- length(unique(data$uniqueID))
    expandedsample <- length(unique(data$newID))
    dat <- dat %>% 
      mutate(Type = ifelse(grepl("Estimate",names),"Estimate",
                           ifelse(grepl(".L", names), "Lower",
                                  ifelse(grepl(".U",names),"Upper",NA)))) %>%
      mutate(Variable = gsub(".Estimate","",names),
             Variable = gsub(".L","",Variable),
             Variable = gsub(".U","",Variable)) %>% filter(Variable!="base.Fixed") %>% 
      rename("LEHS->LEHS"=State.1...State.1,
             "LEHS->SomeC1"=State.1...State.2,
             "SomeC1->SomeC1"=State.2...State.2,
             "SomeC1->SomeC2"=State.2...State.3,
             "SomeC2->SomeC2"=State.3...State.3,
             "SomeC2->SomeC3"=State.3...State.4,
             "SomeC3->SomeC3"=State.4...State.4,
             "SomeC3->College"=State.4...State.5) %>% select(-c(names)) %>% 
      pivot_wider(names_from=Type, values_from=c("LEHS->LEHS":"SomeC3->College")) %>% 
      pivot_longer(`LEHS->LEHS_Estimate`:`SomeC3->College_Upper`) %>% 
      separate(name, into=c("Transition","Type"),sep="_") %>% 
      filter(Variable!="agescaled") %>% filter(Variable!="agesqscaled") %>% 
      filter(Variable!="base") %>% drop_na() %>%
      mutate(model = type, time = timeperiod) %>%
      pivot_wider(names_from=Type, values_from=value) %>%
      group_by(Variable, Transition, model, time) %>%
      mutate(SE = (log(Upper) - log(Lower)) / 3.92,
             SD = SE * sqrt(expandedsample),
             newSE = SD / sqrt(origsample), 
             newLower = log(Estimate) - (newSE * 1.96),
             newUpper = log(Estimate) + (newSE * 1.96),
             newLower = round(exp(newLower), digits=2),
             newUpper = round(exp(newUpper), digits=2),
             newUpper = ifelse(newUpper>100, 100, newUpper))
  }
  return(dat)
}

bind_imputations <- function(coefs){
  for(i in 1:length(coefs)){
    coefs[[paste(i)]]$imp <- i
  }
 coefs <- coefs %>% bind_rows() %>% 
   mutate(SE2 = newSE^2) %>% 
    group_by(Variable, Transition, model, time) %>% 
    mutate(PooledEstimate = round(mean(Estimate),2),
           Vw = 1/20*(sum(SE2)),
           diffsq = ((Estimate-PooledEstimate)^2),
           Vb = (1/19)*sum(diffsq),
           Vtotal  = Vw + Vb + (Vb/20),
           SEpooled = sqrt(Vtotal)) %>% ungroup() %>% 
   group_by(Variable, Transition, model, time) %>% 
   summarise(Estimate = round(mean(Estimate),digits=2),
             oldLower = round(mean(Lower),digits=2),
             oldUpper = round(mean(Upper),digits=2),
             NewLower = round(exp(log(Estimate) - (1.96*SEpooled)),digits=2),
             NewUpper = round(exp(log(Estimate) + (1.96*SEpooled)),digits=2)) %>% 
   distinct()

 return(coefs)
}

extract_covariance <- function(model, type){
  covmat <- model$covmat
  covmat <- data.frame(covmat)
  if(type=="baseline"){
  # covmat <- covmat[c(1:52),c(5:56)]
  covnames <- c("base","agescaled","agesqscaled","sex1","racefinal2Asian/PI","racefinal2black",
                  "racefinal2hispanic","racefinal2Native",
                  "racefinal2other","sex1:racefinal2Asian/PI","sex1:racefinal2black",
                  "sex1:racefinal2hispanic","sex1:racefinal2Native","sex1:racefinal2other")  
  }else if(type=="parent"){
  covmat <- covmat[c(1:54),c(5:54)]
  covnames <- c("base","agescaled","agesqscaled","sex1","racefinal2Asian/PI","racefinal2black",
                "racefinal2hispanic","racefinal2Native",
                "racefinal2other","oneCollegeplus","sex1:racefinal2Asian/PI","sex1:racefinal2black",
                "sex1:racefinal2hispanic","sex1:racefinal2Native","sex1:racefinal2other")
  }
  transitions <- c("LEHS->SomeC1","SomeC1->SomeC2","SomeC2->SomeC3","SomeC3->College")
  namestransitions <- expand.grid(transitions,covnames) %>% 
    mutate(nametransition=paste(Var2,Var1,sep="_"))
  if(length(covmat<0)){
  names(covmat) <- namestransitions$nametransition
  # rownames(covmat) <- namestransitions$nametransition
  }
  return(covmat)
}

compare_missing_covariance <- function(cov, meancovs){
  subtracted = cov-meancovs
  return(subtracted)
}

calculate_AIC <- function(model){
  AIC <- AIC(model)
  AIC = data.frame(AIC)
  return(AIC)
}
