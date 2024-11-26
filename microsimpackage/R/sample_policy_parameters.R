#' Sample policy estimates
#'
#' This function simulates tax policy
#' @param
#' @keywords tax policy
#' @export 
#' @examples

sample_policy_parameters <- function(data = data, n_uncertainty = n_uncertainty) {

  #include superior foreach loop to loop through seeds
  out <- list()
  out <- foreach(i=1:length(unique(data$seed))) %do% {
    
    seed <- unique(data$seed)[i]
    set.seed(seed)
    
    temp <- data %>% filter(seed == seed) %>% 
      dplyr::select(seed, beverage, cons_elasticity, cons_elasticity_se) %>% slice(1)
    
    # reshape data
    pdat <- cbind(beverage = unlist(strsplit(as.character(temp$beverage), ",")),
                  cons_elasticity = unlist(strsplit((temp$cons_elasticity), ",")),
                  cons_elasticity_se = unlist(strsplit((temp$cons_elasticity_se), ","))) %>% as.data.frame() %>% 
      mutate(cons_elasticity = as.numeric(cons_elasticity),
             cons_elasticity_se = as.numeric(cons_elasticity_se))
    
    # sample consumption elasticities for uncertainty modelling
    elasticities <- list()
    elasticities <- foreach(k=1:nrow(pdat)) %do% {
      rnorm(n_uncertainty, m=pdat$cons_elasticity[k], sd=pdat$cons_elasticity_se[k])
    }
    
    do.call(cbind,elasticities) %>% as.data.frame() %>%
      # get scaled beverage-specific SE based on population mean and SE
      mutate(SE_beer = (pdat[pdat$beverage=="beer",]$cons_elasticity_se / pdat[pdat$beverage=="beer",]$cons_elasticity) * V1,
             SE_wine = (pdat[pdat$beverage=="wine",]$cons_elasticity_se / pdat[pdat$beverage=="wine",]$cons_elasticity) * V2,
             SE_spir = (pdat[pdat$beverage=="spirits",]$cons_elasticity_se / pdat[pdat$beverage=="spirits",]$cons_elasticity) * V3) %>% 
      # get required data format
      mutate(cons_elasticity = paste0(V1, ",", V2, ",", V3), 
             cons_elasticity_se = paste0(SE_beer, ",", SE_wine, ",", SE_spir),
             nunc = 1:n_uncertainty,
             seed = seed) %>% 
      dplyr::select(-c(V1,V2,V3, SE_beer, SE_wine, SE_spir))
    
  }
  
  out <- do.call(rbind,out)
  
  data <- data %>% dplyr::select(-c(cons_elasticity, cons_elasticity_se)) %>%
    left_join(., out, relationship = "many-to-many")
  
  return(data)
}
