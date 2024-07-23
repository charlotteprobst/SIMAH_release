# looking at age and cohort effects in the microsimulation 

alcohol <- do.call(rbind,alcohol)
year1984 <- alcohol %>% filter(year==1984) %>% mutate(type="microsim")

compare1984 <- brfssorig %>% filter(YEAR==1984) %>% 
  mutate(agegroup = cut(microsim.init.age, 
                        breaks=c(0,24,34,44,54,64,74,100),
                        labels=c("18-24","25-34","35-44","45-54","55-64","65-74","75+")),
         birthyear = YEAR-microsim.init.age,
         cohort = cut(birthyear,
                      breaks=c(0,1920,1925,1930,1935,1940,1945,1950,1955,
                               1960,1965,1970,1975,1980,1985,1990,2005),
                      labels=c("1900-1920","1921-1925","1926-1930","1931-1935","1936-1940",
                               "1941-1945","1946-1950","1951-1955","1956-1960","1961-1965",
                               "1966-1970","1971-1975","1976-1980","1981-1985","1986-1990",
                               "1991-2000"))) %>% 
  ungroup() %>% group_by(YEAR, microsim.init.sex,agegroup,cohort) %>% filter(microsim.init.alc.gpd!=0) %>% 
  summarise(meanGPD = mean(microsim.init.alc.gpd)) %>% mutate(type="BRFSS") %>% rename(year=YEAR)

compare1984 <- rbind(year1984, compare1984)


ggplot(data=compare1984, aes(x=agegroup, y=meanGPD, colour=type)) + geom_point() + 
  facet_grid(cols=vars(cohort), rows=vars(microsim.init.sex))

compare <- brfssorig %>% 
  mutate(agegroup = cut(microsim.init.age, 
                        breaks=c(0,24,34,44,54,64,74,100),
                        labels=c("18-24","25-34","35-44","45-54","55-64","65-74","75+")),
         birthyear = YEAR-microsim.init.age,
         cohort = cut(birthyear,
                      breaks=c(0,1920,1925,1930,1935,1940,1945,1950,1955,
                               1960,1965,1970,1975,1980,1985,1990,2005),
                      labels=c("1900-1920","1921-1925","1926-1930","1931-1935","1936-1940",
                               "1941-1945","1946-1950","1951-1955","1956-1960","1961-1965",
                               "1966-1970","1971-1975","1976-1980","1981-1985","1986-1990",
                               "1991-2000"))) %>% 
  ungroup() %>% group_by(YEAR, microsim.init.sex,agegroup,cohort) %>% filter(microsim.init.alc.gpd!=0) %>% 
  summarise(meanGPD = mean(microsim.init.alc.gpd)) %>% mutate(type="BRFSS") %>% rename(year=YEAR)

alcohol <- alcohol %>% mutate(type="microsim")
compare <- rbind(compare,alcohol)

men <- compare %>% filter(microsim.init.sex=="m")

ggplot(data=men, aes(x=year, y=meanGPD, colour=type)) + geom_line() + 
  facet_grid(cols=vars(cohort), rows=vars(agegroup)) + ylim(0,NA)

