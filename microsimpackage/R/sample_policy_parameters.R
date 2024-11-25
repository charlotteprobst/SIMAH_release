#' Sample policy estimates
#'
#' This function simulates tax policy
#' @param
#' @keywords tax policy
#' @export 
#' @examples

sample_policy_parameters <- function(data = data, n_uncertainty = n_uncertainty) {

  #include superior foreach loop to loop through groups of samplenum, seed, ed/alcmodels
  data <- data %>% group_by(samplenum, seed, educationmodel, alcoholmodel) %>%
    mutate(group = cur_group_id()) %>% ungroup()
  
  out <- list()
  out <- foreach(i=1:max(unique(data$group))) %do% {
    
    temp <- data %>% filter(group == i) %>% slice(1) %>% 
      dplyr::select(seed, group, beverage, cons_elasticity, cons_elasticity_se)
    
    set.seed(temp$seed)
    
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
      mutate(cons_elasticity = paste0(V1, ",", V2, ",", V3), 
             
             # FOR NOW: set se=0.08
             cons_elasticity_se = "0.081,0.0808,0.0806",
             nunc = 1:n_uncertainty,
             group = i) %>% 
      dplyr::select(-c(V1,V2,V3))
    
  }
  
  out <- do.call(rbind,out)
  
  data <- data %>% dplyr::select(-c(cons_elasticity, cons_elasticity_se)) %>%
    left_join(out, relationship = "many-to-many")
  
  return(data)
}
