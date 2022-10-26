#' Allocate individuals a grams per day of consumption - based on categories
#'
#' @param
#' @keywords allocate alcohol
#' @export
#' @examples
#' allocate_gpd
allocate_gramsperday <- function(basepop, y, model, DataDirectory){
  prepdata <- basepop %>% filter(AlcCAT!="Non-drinker") %>%
    mutate(age_var = microsim.init.age, sex_recode = ifelse(microsim.init.sex=="m","Male","Female"),
           YEAR = y, education_summary = microsim.init.education, race_eth = ifelse(microsim.init.race=="BLA","Black",
                                                                                    ifelse(microsim.init.race=="WHI","White",
                                                                                           ifelse(microsim.init.race=="SPA","Hispanic",
                                                                                                  ifelse(microsim.init.race=="OTH","Other",NA)))),
           lambda = ifelse(sex_recode=="Male",0.06, -0.22))
  # lambda <- 0.129 #lambda for transforming consumption taken from M.Strong / D.Moyo script
  back.tran <- function(x, lambda){(lambda * x + 1) ^ (1/lambda)}

  women <- prepdata %>% filter(sex_recode=="Female")
  women$newgpd <- back.tran(predict(model[[1]], women), lambda=-0.22)
  women <- women %>% dplyr::select(microsim.init.id, newgpd)

  men <- prepdata %>% filter(sex_recode=="Male")
  men$newgpd <- back.tran(predict(model[[2]], men), lambda=0.06)
  men <- men %>% dplyr::select(microsim.init.id, newgpd)
  prepdata <- rbind(men, women)
  basepop <- left_join(basepop, prepdata)
  basepop$microsim.init.alc.gpd <- ifelse(basepop$AlcCAT=="Non-drinker", 0,
                                          basepop$newgpd)
  basepop$newgpd <- NULL
  return(basepop)
}

