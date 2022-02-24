setupQ <- function(estimates,model){
  Qmatrices <- list()
  template <- rbind(c(0,0,0,0,0),
                    c(0,0,0,0,0),
                    c(0,0,0,0,0),
                    c(0,0,0,0,0),
                    c(0,0,0,0,0))
  template <- rbind(c(0,0,0,0),
                    c(0,0,0,0),
                    c(0,0,0,0),
                    c(0,0,0,0))
  seq <- seq(1,78,6)
  for(i in seq){
    template1 <- template
    template1[1,2] <- estimates[i]
    template1[2,1] <- estimates[i+1]
    template1[2,3] <- estimates[i+2]
    template1[3,2] <- estimates[i+3]
    template1[3,4] <- estimates[i+4]
    template1[4,3] <- estimates[i+5]
    # template1[5,4] <- estimates[i+6]
    Qmatrices[[paste(i)]] <- template1
  }
  Qmatrices[[14]] <- Qmatrices[[1]]
  Qmatrices[[1]][1,2] <- log(abs(Qmatrices[[1]][1,2]))
  Qmatrices[[1]][2,1] <- log(abs(Qmatrices[[1]][2,1]))
  Qmatrices[[1]][2,3] <- log(abs(Qmatrices[[1]][2,3]))
  Qmatrices[[1]][3,2] <- log(abs(Qmatrices[[1]][3,2]))
  Qmatrices[[1]][3,4] <- log(abs(Qmatrices[[1]][3,4]))
  Qmatrices[[1]][4,3] <- log(abs(Qmatrices[[1]][4,3]))
  # Qmatrices[[1]][5,4] <- log(abs(Qmatrices[[1]][5,4]))
  names(Qmatrices) <- c("logbaseline","female_wave1.factorWomen","age3.factor30-49","age3.factor50+","edu3.factorLow",
                       "edu3.factorMed","race_wave1.factorBlack, non-Hispanic","race_wave1.factorHispanic","race_wave1.factorOther, non-Hispanic",
                       "baseline")
  names(Qmatrices) <- names(model$Qmatrices)
  return(Qmatrices)
}
