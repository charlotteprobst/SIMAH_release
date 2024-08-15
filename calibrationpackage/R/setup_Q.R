#' function to setup the Q transition matrix for markov models for calibration
#' @param
#' @keywords microsimulation markov model
#' @export
#' @examples
#' setup_Q
setup_Q <- function(model, estimates){
  Qmatrices <- model$Qmatrices

  non_zero_positions <- data.frame(which(Qmatrices[[1]] != 0, arr.ind = TRUE))
  non_zero_positions <- non_zero_positions[order(non_zero_positions$row,non_zero_positions$col),]

  for (j in 1:nrow(non_zero_positions)) {
    for (p in 1:length(Qmatrices)) {
      Qmatrices[[p]][non_zero_positions$row[j], non_zero_positions$col[j]] <- estimates[j]
      j <- j+nrow(non_zero_positions)
    }
  }

  # start_index <- nrow(non_zero_positions)+1 # Index in estimates to start from
  # for (m in 2:(length(Qmatrices)-1)) {
  #   for (n in 1:(ncol(Qmatrices[[i]]) - 1)) {
  #     Qmatrices[[m]][n, n + 1] <- estimates[start_index]
  #     start_index <- start_index + 1
  #   }
  # }

  # set up the final element of the Q
  for(j in 1:nrow(non_zero_positions)){
    Qmatrices[[length(Qmatrices)]][non_zero_positions$row[j], non_zero_positions$col[j]] <- exp(estimates[j])
  }

  for(j in 1:nrow(Qmatrices[[1]])){
    Qmatrices[[length(Qmatrices)]][j,j] <- 0
    Qmatrices[[length(Qmatrices)]][j,j] <- -sum(Qmatrices[[length(Qmatrices)]][j,])
  }

  return(Qmatrices)
}
