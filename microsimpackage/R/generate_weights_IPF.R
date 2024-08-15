#' generates the IPF weights for each selected state
#'
#' @param
#' @keywords brfss
#' @export
#' @examples
#' setup_for_IPF
generate_weights_IPF <- function(data,selectedstate,cons){
  data <- data %>% filter(State==selectedstate)
  cons <- cons %>% filter(state==selectedstate) %>% dplyr::select(-state)
  individual <- data.frame(race=data$race, agecat=data$agecat, sex=data$sex, education=data$education)
  # race sex age education - make binary
  RSAE <- paste0(individual$race, individual$sex, individual$agecat, individual$education)

  m1 <- model.matrix(~RSAE-1)
  colnames(m1) <- sub("RSAE", "", colnames(m1))
  ind_cat <- as.data.frame(m1)

  ind_cat <- ind_cat[ , order(names(ind_cat))]
  cons <- cons[, order(names(cons))]
  # colnames(cons) == names(ind_cat)

  weights <- array(NA, dim=c(nrow(individual),nrow(cons)))
  ind_agg <- matrix(colSums(ind_cat), nrow(cons), ncol(cons), byrow = T)
  cons <- apply(cons, 2, as.numeric) # convert the constraints to 'numeric'
  ind_catt <- t(ind_cat) # transpose the dummy variables for ipfp
  x0 <- rep(1, nrow(individual)) # set the initial weights - 1

  cons[is.na(cons)] <- 0 #set any NA constraints to 0

  weights <- ipfp(cons, ind_catt, x0, maxit=100, v=T)
  # ind_agg <- colSums(weights*ind_cat)
  # max(abs(ind_agg-cons))

  weights <- int_trs(weights)

  weights_file <- data.frame(State=data$State, id=data$id, weight = weights)
  return(weights_file)
}

