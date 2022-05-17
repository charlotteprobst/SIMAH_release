setup_markov_model <- function(data,y){
  if(y==2009){
  data <- data %>% filter(year<=2009)
  }else{
    data <- data %>% filter(year>=2011)
  }
  data <- data %>% drop_na(highestEd)
  data$educNUM <- ifelse(data$highestEd<=12, 1,
                         ifelse(data$highestEd==13, 2,
                                ifelse(data$highestEd==14,3,
                                       ifelse(data$highestEd==15,4,
                                              ifelse(data$highestEd>=16,5,NA)))))

  # remove anyone with only one year of data- this gives an error in MSM 
  data <- data %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
    filter(totalobservations>1) %>% mutate(sex=factor(sex),
                                           sex=ifelse(sex=="female",1,0))
  # all data needs to be ordered by ID then year
  data <- as.data.frame(lapply(data, unlist))
  data <- data[order(data$newID, data$year),]
  # 
  # data$racefinal <- ifelse(data$racefinal=="Native","other",
  #                          ifelse(data$racefinal=="Asian/PI","other",data$racefinal))
  data$racefinal <- as.factor(data$racefinal)
  data$racefinal <- relevel(data$racefinal, ref="white")
  data$educNUM <- ifelse(data$age==18 & data$educNUM>2, 1, data$educNUM)
  data$sex <- as.factor(data$sex)
  source("SIMAH_code/education_transitions/2_analysis/cleaning_education_function.R")
  backIDs <- getIDs(data)
  data <- data[!data$newID %in% backIDs,]

  data$agesq <- data$age^2
  data$agescaled <- as.numeric(scale(data$age, center=T))
  data$agesqscaled <- as.numeric(scale(data$agesq, center=T))
  data <- data.frame(data)
  return(data)
}

run_markov_model_baseline <- function(data){
  # Q matrix of allowed instantaneous transitions - only allowed to transition between 1-2 and 2-3 (and staying the same)
  Q <- rbind( c(0.5, 0.5, 0, 0, 0),
              c(0, 0.5, 0.5, 0, 0),
              c(0, 0, 0.5, 0.5, 0),
              c(0, 0, 0, 0.5, 0.5),
              c(0, 0, 0, 0, 0.5))
  data$agescaled <- as.numeric(scale(data$age, center=T))
  data$agesqscaled <- as.numeric(scale(data$agesq, center=T)) 
  model <- msm(educNUM~year, newID, data=data, qmatrix=Q,
                       center=FALSE,
                       covariates=~agescaled + agesqscaled + sex*racefinal2)
  return(model)
}

run_markov_model_parent <- function(data){
  # Q matrix of allowed instantaneous transitions - only allowed to transition between 1-2 and 2-3 (and staying the same)
  Q <- rbind( c(0.5, 0.5, 0, 0, 0),
              c(0, 0.5, 0.5, 0, 0),
              c(0, 0, 0.5, 0.5, 0),
              c(0, 0, 0, 0.5, 0.5),
              c(0, 0, 0, 0, 0.5))
  data$agescaled <- as.numeric(scale(data$age, center=T))
  data$agesqscaled <- as.numeric(scale(data$agesq, center=T)) 
  model <- msm(educNUM~year, newID, data=data, qmatrix=Q,
               center=FALSE,
               covariates=~agescaled + agesqscaled + sex*racefinal2 + oneCollegeplus)
  return(model)
  
}

extract_coefficients <- function(model){
  dat <- print(model)
  if(length(dat)>1){
  dat <- t(dat)
  dat <- data.frame(dat)
  dat$names <- row.names(dat)
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
    filter(Type=="Estimate")
  }
  return(dat)
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
}
