#' function to summarise alcohol use output from microsimulation
#' @param
#' @keywords microsimulation
#' @export
#' @examples
#' summarise_alcohol_output
summarise_model_alcohol_output <- function(data){
  output <- data %>%
    #group into three age groups
    mutate(agecat = cut(microsim.init.age,
                          breaks=c(0,24,64,100),
                          labels=c("18-24","25-64","65+")),
           # set education to be SomeC for SomeC + College for 18-24 group
           microsim.init.education = ifelse(agecat=="18-24" & microsim.init.education=="College","SomeC",
                                            microsim.init.education),
           #group years into categories
           year_cat = cut(year,
                          breaks=c(0,2001,2004,2007,2010,2013,2016,2019,2021),
                          labels=c("2000-2001","2002-2004","2005-2007","2008-2010","2011-2013","2014-2016",
                                   "2017-2019","2020-2021"))
           ) %>%
    group_by(samplenum, seed, year, microsim.init.sex,microsim.init.race,agecat, microsim.init.education,AlcCAT, .drop=FALSE) %>%
    reframe(n=sum(n)) %>%
    ungroup() %>%
    group_by(samplenum, seed, year, microsim.init.sex,microsim.init.race, agecat, microsim.init.education) %>%
    mutate(propsimulation = n/sum(n),
           agecat = as.character(agecat)) %>%
    dplyr::select(-n)
  return(output)
}
