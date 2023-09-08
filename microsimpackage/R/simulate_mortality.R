#' Assigns risk for specific causes and simulates mortality
#'
#' This function simulates mortality
#' @param
#' @keywords mortality
#' @export
#' @examples
#' simulate_mortality
simulate_mortality <- function(data, diseases){
  data <- as.data.frame(data)
  for (i in 1:length(diseases)) {
    disease <- diseases[i]
    RR_expr <- sym(paste0('RR_', quo_name(disease)))
    rate_expr <- sym(paste0('rate_', quo_name(disease)))
    risk_expr <- sym(paste0('risk_', quo_name(disease)))
    if (i==1) {
      data <- data %>%
        mutate(!!risk_expr := !!RR_expr * !!rate_expr)
    } else if (i>1) {
      prev_risk_expr <- sym(paste0("risk_", quo_name(diseases[i-1])))
      data <- data %>%
        mutate(!!risk_expr := !!RR_expr * !!rate_expr,
               !!risk_expr := !!risk_expr + !!prev_risk_expr) 
    }
  }

  data$prob <- runif(nrow(data))
  
  for (i in 1:length(diseases)) {
    disease <- diseases[i]
    risk_expr <- sym(paste0('risk_', quo_name(disease)))
    mort_expr <- sym(paste0('mort_', quo_name(disease)))
    if (i==1) {
      data <- data %>%
        mutate(!!mort_expr := ifelse(!!risk_expr > prob, 1, 0))
    } else if (i>1) {
      prev_risk_expr <- sym(paste0("risk_", quo_name(diseases[i-1])))
      data <- data %>%
        mutate(!!mort_expr := ifelse(!!risk_expr > prob & !!prev_risk_expr < prob, 1, 0))
    }
  }
  return(data)
}

