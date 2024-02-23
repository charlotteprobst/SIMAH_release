setupQ <- function(estimates){
  Qmatrices <- model$Qmatrices
  # use model Q matrix as template
  Qmatrices[[1]][1,2] <- log(estimates[1])
  Qmatrices[[1]][2,1] <- log(estimates[2])
  Qmatrices[[1]][2,3] <- log(estimates[3])
  Qmatrices[[1]][3,2] <- log(estimates[4])
  Qmatrices[[1]][3,4] <- log(estimates[5])
  Qmatrices[[1]][4,3] <- log(estimates[6])

  start_index <- 7 # Index in estimates to start from
  for (i in 2:length(Qmatrices)) {
    for (j in 1:(ncol(Qmatrices[[i]]) - 1)) {
      Qmatrices[[i]][j, j + 1] <- estimates[start_index]
      start_index <- start_index + 1
    }
  }
  length(Qmatrices)
  Qmatrices[[length(Qmatrices)]][1,1] <- -abs(estimates[1])
  Qmatrices[[length(Qmatrices)]][1,2] <- abs(estimates[1])

  Qmatrices[[length(Qmatrices)]][2,1] <- -abs(estimates[2])
  Qmatrices[[length(Qmatrices)]][2,3] <- abs(estimates[2])

  Qmatrices[[length(Qmatrices)]][3,3] <- -abs(estimates[3])
  Qmatrices[[length(Qmatrices)]][3,4] <- abs(estimates[3])

  Qmatrices[[length(Qmatrices)]][4,4] <- -abs(estimates[4])
  Qmatrices[[length(Qmatrices)]][4,5] <- abs(estimates[4])

  return(Qmatrices)
}
