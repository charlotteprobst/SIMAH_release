#' Allocate individuals a grams per day of consumption - based on categories
#'
#' @param
#' @keywords allocate alcohol
#' @export
#' @examples
#' allocate_gpd
allocate_gramsperday <- function(basepop){

distributions <- list()
basepop$cat <- paste0(basepop$AlcCAT, basepop$microsim.init.sex)
cats <- unique(basepop$cat)
cats <- cats[-c(1,5)]
for(i in unique(cats)){
  sub <- basepop %>% filter(cat==i)
  distribution <- fitdist(sub$microsim.init.alc.gpd, "lnorm")
  distributions[[paste(i)]] <- data.frame(mean = distribution$estimate[1],
                                          sd = distribution$estimate[2],
                                          cat = paste(i))
}
distributions <- distributions %>% do.call(rbind,.) %>%
  separate(cat, into=c("AlcCAT","microsim.init.sex"), sep=c(-1))

impute_gpd <- function(data){
  if(data$AlcCAT[1]!="Non-drinker"){
  data$newgpd <- rlnorm(nrow(data), mean=data$mean, sd=data$sd)
  data$newgpd <- ifelse(data$newgpd<0, 0, ifelse(data$newgpd>200, 200, data$newgpd))
  }else{
    data$newgpd <- 0
  }
  return(data)
}

basepop <- left_join(basepop,distributions, by=c("microsim.init.sex","AlcCAT")) %>%
  group_by(microsim.init.sex, AlcCAT) %>%
  do(impute_gpd(.)) %>%
  mutate(microsim.init.alc.gpd=newgpd) %>%
  dplyr::select(-c(cat,mean,sd,newgpd))
return(basepop)
}

