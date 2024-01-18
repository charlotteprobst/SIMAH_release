# resample the markov model parameters

samples <- newestimates %>% dplyr::select(-c(SampleNum, inflation, TimePeriod)) %>% 
  mutate_all(as.numeric)
estimates <- colMeans(samples)
cov <- cov(samples)
samples <- mvrnorm(n=nsamples, estimates, cov)
x <- model
sex <- c(0,1)
race <- c("white","black","hispanic","other")
age <- c("18","19","20","21","22-24","25+")
# every age sex race combination
combinations <- expand.grid(age,sex,race)
names(combinations) <- c("age","sex","race")
combinations <- data.frame(combinations)
options(digits=3)
combinations$cat <- paste(combinations$age, combinations$sex, combinations$race, sep="_")
# plist <- list()
# plist <- extract_for_estimates(estimates, combinations, x, setupQ, msm.fixdiag.qmatrix,
#                                msm.parse.covariates, MatrixExp)
options(scipen=999)

sampleList <- as.list(as.data.frame(t(samples)))
names(sampleList) <- 1:nrow(samples)
allsamples <- list()
for(k in 1:nrow(samples)){
  estimates <- sampleList[[k]]
  allsamples[[paste(k)]] <- extract_for_estimates(estimates, combinations, x, setupQ, msm.fixdiag.qmatrix,
                                                  msm.parse.covariates, MatrixExp)
  allsamples[[paste(k)]]$SampleNum <- k
}
allsamples <- do.call(rbind,allsamples)
TimePeriod <- "1999-2019"
inflation <- 1
allsamples <- allsamples %>% mutate(StateTo=parse_number(StateTo),
                                    sex = ifelse(sex==1,"female","male"),
                                    time = TimePeriod,
                                    inflation = inflation) %>% 
  dplyr::select(SampleNum, inflation, StateFrom, StateTo, time, age, sex, race, prob)
SampleNum <- 1:nrow(samples)
samples <- data.frame(cbind(SampleNum, inflation, TimePeriod, samples))
probs <- allsamples
estimates <- samples

transitionsList <- list()
for(i in 1:length(unique(estimates$SampleNum))){
  transitionsList[[paste(i)]] <- probs %>% filter(SampleNum==i) %>% 
    mutate(sex = ifelse(sex=="male", "m","f"),
           cat = paste(time,age, sex, race, "STATEFROM", StateFrom, sep="_")) %>% 
    group_by(cat) %>% mutate(cumsum=cumsum(prob)) %>% 
    dplyr::select(cat, StateTo, cumsum)
}