extract_for_estimates <- function(estimates, combinations, model, setupQ, msm.fixdiag.qmatrix,
                                  msm.parse.covariates, MatrixExp){
  plist <- list()
  i <- combinations$cat[52]
  for(i in levels(as.factor(combinations$cat))){
    combination <- combinations %>% filter(cat==i)
    # covariates <- list(female_wave1.factor=as.character(combination$sex), 
    #                    age3.factor=as.character(combination$age), 
    #                    edu3.factor = as.character(combinations$educ),
    #                    race_wave1.factor=as.character(combination$race))
    x <- model
    covlist <- list(female_wave1.factorWomen = ifelse(combination$sex=="Women",1,0),
                    'age718-20'=ifelse(combination$age=="18-20",1,0),
                    'age721-25'=ifelse(combination$age=="21-25",1,0),
                    'age726-29'=ifelse(combination$age=="26-29",1,0),
                    'age730-39'=ifelse(combination$age=="30-39",1,0),
                    'age740-49'=ifelse(combination$age=="40-49",1,0),
                    'age750-64'=ifelse(combination$age=="50-64",1,0),
                    # 'age765+'=ifelse(combination$age=="65+",1,0),
                    'edu3.factorLow'=ifelse(combination$educ=="Low",1,0),
                    'edu3.factorMed'=ifelse(combination$educ=="Med",1,0),
                    'race_wave1.factor Black, non-Hispanic'=ifelse(combination$race=="Black, non-Hispanic",1,0),
                    'race_wave1.factorHispanic'=ifelse(combination$race=="Hispanic",1,0),
                    'race_wave1.factorOther, non-Hispanic'=ifelse(combination$race=="Other, non-Hispanic",1,0))

    # covlist <- msm.parse.covariates(x, covariates, x$qcmodel)
    ni <- x$qmodel$npars
    nc <- length(covlist)
    se <- lse <- fixed <- numeric(ni)
    Qmatrices <- setupQ(estimates,x)
    logest <- Qmatrices[[1]]
    for (j in seq_len(nc)) {
      logest <- logest + Qmatrices[[j + 1]] * covlist[[j]]
    }
    mat <- exp(logest)
    mat[mat==1] <- 0
    mat <- msm.fixdiag.qmatrix(mat)
    p <- MatrixExp(mat, 2)
    plist[[paste(i)]] <- p
    plist[[paste(i)]] <- data.frame(unclass(plist[[paste(i)]]))
    plist[[paste(i)]]$StateFrom <- 1:nrow(plist[[paste(i)]])
    plist[[paste(i)]] <- plist[[paste(i)]] %>% pivot_longer(cols=X1:X4,
                                                            names_to="StateTo", values_to="prob") %>% 
      mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
                                 endsWith(StateTo,"2") ~ "State 2",
                                 endsWith(StateTo,"3") ~ "State 3",
                                 endsWith(StateTo,"4") ~ "State 4",
                                 endsWith(StateTo,"5") ~ "State 5")) %>% 
      mutate(age = combination$age,
             sex = combination$sex,
             race= combination$race,
             educ=combination$educ)
  }
  plist <- do.call(rbind,plist)
  
  return(plist)
}
