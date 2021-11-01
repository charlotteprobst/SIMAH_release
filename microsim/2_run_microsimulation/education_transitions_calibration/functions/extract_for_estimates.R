extract_for_estimates <- function(estimates, combinations, model, setupQ, msm.fixdiag.qmatrix,
                                  msm.parse.covariates, MatrixExp){
  plist <- list()
  i <- combinations$cat[35]
  for(i in levels(as.factor(combinations$cat))){
    combination <- combinations %>% filter(cat==i)
    covariates <- list(sex=combination$sex, age=combination$age, racefinal=combination$race)
    x <- model
    covlist <- msm.parse.covariates(x, covariates, x$qcmodel)
    ni <- x$qmodel$npars
    nc <- length(covlist)
    se <- lse <- fixed <- numeric(ni)
    Qmatrices <- setupQ(estimates)
    logest <- Qmatrices[[1]]
    for (j in seq_len(nc)) {
      logest <- logest + Qmatrices[[j + 1]] * covlist[[j]]
    }
    mat <- exp(logest)
    mat[mat==1] <- 0
    mat <- msm.fixdiag.qmatrix(mat)
    p <- MatrixExp(mat, 1)
    plist[[paste(i)]] <- p
    plist[[paste(i)]] <- data.frame(unclass(plist[[paste(i)]]))
    plist[[paste(i)]]$StateFrom <- 1:nrow(plist[[paste(i)]])
    plist[[paste(i)]] <- plist[[paste(i)]] %>% pivot_longer(cols=X1:X5,
                                                            names_to="StateTo", values_to="prob") %>% 
      mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
                                 endsWith(StateTo,"2") ~ "State 2",
                                 endsWith(StateTo,"3") ~ "State 3",
                                 endsWith(StateTo,"4") ~ "State 4",
                                 endsWith(StateTo,"5") ~ "State 5")) %>% 
      mutate(age = combination$age,
             sex = combination$sex,
             race= combination$race)
  }
  plist <- do.call(rbind,plist)
  
  return(plist)
}
