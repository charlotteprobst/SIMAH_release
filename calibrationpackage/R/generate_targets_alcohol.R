#' function to generate alcohol use calibration targets
#' @param
#' @keywords microsimulation markov model targest
#' @export
#' @examples
#' generate_targets_alcohol
generate_targets_alcohol <- function(data){
  targets <- data %>%
    #group into three age groups
    mutate(agecat = cut(age,
                          breaks=c(0,24,64,100),
                          labels=c("18-24","25-64","65+")),
           # set education to be SomeC for SomeC + College for 18-24 group
           education = ifelse(agecat=="18-24" & education=="College","SomeC",
                              education),
           #group years into categories
           # year_cat = cut(YEAR,
           #                breaks=c(0,2003,2006,2009,2012,2015,2018,2021),
           #                labels=c("2000-2003","2004-2006","2007-2009","2010-2012","2013-2015",
           #                         "2016-2018","2019-2021"),
            year_cat = cut(YEAR,
                           breaks=c(0,2001,2004,2007,2010,2013,2016,2019,2021),
                           labels=c("2000-2001","2002-2004","2005-2007","2008-2010","2011-2013","2014-2016",
                                                  "2017-2019","2020-2021"))
           ) %>%
    rename(year=YEAR) %>%
    group_by(year, sex,race,agecat, education,alc_cat, .drop=FALSE) %>% tally() %>%
    ungroup() %>%
    group_by(year, sex,race, agecat, education) %>%
    mutate(proptarget = n/sum(n),
           se = sqrt(proptarget * (1 - proptarget) / sum(n)),
           agecat = as.character(agecat)) %>%
    dplyr::select(-n) %>% drop_na()
  return(targets)
}
