setupQ <- function(estimates){
  # Qmatrices <- list()
  # template <- rbind(c(0,0,0,0,0),
  #                   c(0,0,0,0,0),
  #                   c(0,0,0,0,0),
  #                   c(0,0,0,0,0),
  #                   c(0,0,0,0,0))
  # seq <- c(1,5,9,13,17,21,25,29,33)
  # for(i in seq){
  #   template1 <- Qmatrices[[i]]
  #   template1[1,2] <- estimates[i]
  #   template1[2,3] <- estimates[i+1]
  #   template1[3,4] <- estimates[i+2]
  #   template1[4,5] <- estimates[i+3]
  #   Qmatrices[[paste(i)]] <- template1
  # }
  # Qmatrices[[10]] <- Qmatrices[[1]]
  # Qmatrices[[1]][1,2] <- log(abs(Qmatrices[[1]][1,2]))
  # Qmatrices[[1]][2,3] <- log(abs(Qmatrices[[1]][2,3]))
  # Qmatrices[[1]][3,4] <- log(abs(Qmatrices[[1]][3,4]))
  # Qmatrices[[1]][4,5] <- log(abs(Qmatrices[[1]][4,5]))
  # names(Qmatrices) <- c("logbaseline","agecat19","agecat20","agecat21-25", "agecat26+","sex1","racefinal2black","racefinal2hispanic","racefinal2other",
  #                      "baseline")
  Qmatrices <- model$Qmatrices
  # use model Q matrix as template
  Qmatrices[[1]][1,2] <- log(estimates[1])
  Qmatrices[[1]][2,3] <- log(estimates[2])
  Qmatrices[[1]][3,4] <- log(estimates[3])
  Qmatrices[[1]][4,5] <- log(estimates[4])
  
  start_index <- 5 # Index in estimates to start from
  for (i in 2:length(Qmatrices)) {
    for (j in 1:(ncol(Qmatrices[[i]]) - 1)) {
      Qmatrices[[i]][j, j + 1] <- estimates[start_index]
      start_index <- start_index + 1
    }
  }
  
  Qmatrices[[10]][1,1] <- -abs(estimates[1])
  Qmatrices[[10]][1,2] <- abs(estimates[1])

  Qmatrices[[10]][2,2] <- -abs(estimates[2])
  Qmatrices[[10]][2,3] <- abs(estimates[2])
  
  Qmatrices[[10]][3,3] <- -abs(estimates[3])
  Qmatrices[[10]][3,4] <- abs(estimates[3])
  
  Qmatrices[[10]][4,4] <- -abs(estimates[4])
  Qmatrices[[10]][4,5] <- abs(estimates[4])
  
  return(Qmatrices)
}
