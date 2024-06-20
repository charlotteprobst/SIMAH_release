#' summarises the data for alcohol categories
#'
#' @param
#' @keywords alcohol categories
#' @export
#' @examples
#' summarise alcohol cat data by grouping variables
generate_alcoholcat_summary <- function(data){ #add functionality for custom groupings later
  data <- data %>%
    mutate(
      agecat = cut(microsim.init.age,
                        breaks=c(0,24,64,100),
                        labels=c("18-24","25-64","65+")),
      year_cat = cut(year,
                     breaks=c(0,2001,2003,2005,2007,2009,2011,2013,2015,2017,2019,2021),
                     labels=c("2000-2001","2002-2003","2004-2005","2006-2007","2008-2009","2010-2011",
                              "2012-2013","2014-2015","2016-2017","2018-2019","2020-2021")),
      microsim.init.education = ifelse(agecat=="18-24" & microsim.init.education=="SomeC", "LEHS",
                                       microsim.init.education)) %>%
    # rename(year=YEAR) %>%
    group_by(year_cat, microsim.init.sex,microsim.init.race,agecat, microsim.init.education,AlcCAT, .drop=FALSE) %>%
    tally() %>%
    ungroup() %>%
    group_by(year_cat,microsim.init.sex,microsim.init.race, agecat, microsim.init.education) %>%
    mutate(proportion = n/sum(n),
           se = sqrt(proportion * (1 - proportion) / sum(n)),
           agecat = as.character(agecat),

           # microsim.init.race = recode(microsim.init.race,
           #                             "BLA"="Black","SPA"="Hispanic",
           #                             "WHI"="White","OTH"="Others"),
           microsim.init.education = factor(microsim.init.education,
                                            levels=c("LEHS","SomeC","College"))) %>%
    dplyr::select(-n) %>% drop_na()
return(data)
}
