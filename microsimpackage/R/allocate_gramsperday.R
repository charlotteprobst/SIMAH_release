#' Allocate individuals a grams per day of consumption - based on categories
#'
#' @param
#' @keywords allocate alcohol
#' @export
#' @examples
#' allocate_gpd
allocate_gramsperday <- function(basepop, y, DataDirectory){
  model <- read_rds(paste0(DataDirectory, "CatContModel1.RDS"))
  model$coefficients
  prepdata <- basepop %>% filter(AlcCAT!="Non-drinker") %>%
    mutate(age_var = microsim.init.age, sex_recode = ifelse(microsim.init.sex=="m","Male","Female"),
           YEAR = y, education_summary = microsim.init.education, race_eth = microsim.init.race)
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

