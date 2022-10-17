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
                                                                                                  ifelse(microsim.init.race=="OTH","Other",NA)))))
  lambda <- 0.129 #lambda for transforming consumption taken from M.Strong / D.Moyo script
  back.tran <- function(x){(lambda * x + 1) ^ (1/lambda)}
  prepdata$newgpd <- back.tran(predict(model, prepdata))
  prepdata <- prepdata %>% dplyr::select(microsim.init.id, newgpd)
  basepop <- left_join(basepop, prepdata)
  basepop$microsim.init.alc.gpd <- ifelse(basepop$AlcCAT=="Non-drinker", 0,
                                          basepop$newgpd)
  basepop$newgpd <- NULL
  return(basepop)
}

