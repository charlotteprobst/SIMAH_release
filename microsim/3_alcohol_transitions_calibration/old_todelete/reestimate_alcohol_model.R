
data <- model$data$mf

data$time <- ifelse(data$`(time)`==0, 1,4)

data <- data %>% 
  mutate(age3 = as.factor(case_when(age7=="18-20" ~ "18-24", 
                          age7=="21-25" ~ "18-24",
                          age7=="26-29" ~ "25-64",
                          age7=="30-39" ~ "25-64",
                          age7=="40-49" ~ "25-64",
                          age7=="50-64" ~ "25-64",
                          age7=="65+" ~ "65+")))

qmatrix <- model$qmodel$qmatrix
qmatrix <- ifelse(qmatrix!=0, 0.5, qmatrix)

data <- data[order(data$`(subject)`, data$time),]

new_markov_model <- msm(`(state)`~time, `(subject)`, data=data, 
    qmatrix=abs(qmatrix), center=FALSE, 
    covariates=~female_w1+age3+edu3+race_w1, 
    control=list(trace=1, fnscale=100000, maxit=300))

pmatrix.msm(new_markov_model, 
covariates=list(female_w1="Women", age3="65+", edu3="Low",race_w1="Hispanic"))

pmatrix.msm(model, 
            covariates=list(female_w1="Women", age7="18-20", edu3="Low",race_w1="Hispanic"))


saveRDS(new_markov_model, "SIMAH_workplace/nesarc/Models/msm_CB_reestimation.RDS")
