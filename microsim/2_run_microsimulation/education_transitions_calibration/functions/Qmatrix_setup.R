setupQ <- function(estimates){
  Qmatrices <- list()
  template <- rbind(c(0,0,0,0,0),
                    c(0,0,0,0,0),
                    c(0,0,0,0,0),
                    c(0,0,0,0,0),
                    c(0,0,0,0,0))
  seq <- c(1,5,9,13,17,21,25,29,33)
  for(i in seq){
    template1 <- template
    template1[1,2] <- estimates[i]
    template1[2,3] <- estimates[i+1]
    template1[3,4] <- estimates[i+2]
    template1[4,5] <- estimates[i+3]
    Qmatrices[[paste(i)]] <- template1
  }
  Qmatrices[[10]] <- Qmatrices[[1]]
  Qmatrices[[1]][1,2] <- log(abs(Qmatrices[[1]][1,2]))
  Qmatrices[[1]][2,3] <- log(abs(Qmatrices[[1]][2,3]))
  Qmatrices[[1]][3,4] <- log(abs(Qmatrices[[1]][3,4]))
  Qmatrices[[1]][4,5] <- log(abs(Qmatrices[[1]][4,5]))
  names(Qmatrices) <- c("logbaseline","sex1","racefinalblack","racefinalhispanic","racefinalother",
                       "agecen","sex1:racefinalblack","sex1:racefinalhispanic","sex1:racefinalother",
                       "baseline")
  return(Qmatrices)
}
