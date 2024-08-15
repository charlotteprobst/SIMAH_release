###get the summary files to compare to the census ####
library(dplyr)
library(tidyr)
options(scipen=999)
source("running_scripts/microsim_functions.R")
SelectedState <- "USA"

summaryperyear <- list()

for(i in names(PopPerYear)){
  summaryperyear[[paste(i)]] <- PopPerYear[[paste(i)]] %>% 
    mutate(agecat=cut(microsim.init.age,
                      breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,100),
                      labels=c("18.24","25.29","30.34","35.39","40.44","45.49",
                               "50.54","55.59","60.64","65.69","70.74","75.79"))) %>% 
    group_by(microsim.init.sex, microsim.init.race, agecat) %>% tally() %>% mutate(year=i, n=n*(1/proportion),
                                                                                   datatype="microsim",
                                                                                   year=as.numeric(year))
}

summary2000 <- baseorig %>% mutate(agecat=cut(microsim.init.age,
                                              breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,100),
                                              labels=c("18.24","25.29","30.34","35.39","40.44","45.49",
                                                       "50.54","55.59","60.64","65.69","70.74","75.79"))) %>% 
  group_by(microsim.init.sex, microsim.init.race, agecat) %>% tally() %>% mutate(year=2000, n=n*(1/proportion),
                                                                                 datatype="microsim",
                                                                                 year=as.numeric(year))

summaryperyear <- data.frame(do.call(rbind, summaryperyear))

summaryperyear <- summaryperyear %>% filter(year!=2000)

summaryperyear <- rbind(summary2000, summaryperyear)


compare2000 <- read.csv("input_data/popcounts2000.csv") %>% filter(age<=79) %>% 
  select(-c(X)) %>% 
  rename(microsim.init.race = race,
         microsim.init.age = age, 
         microsim.init.sex= sex, 
         microsim.init.education=education,
         n=newcount) %>% 
  mutate(year=2000,
         datatype="census",
         agecat = cut(microsim.init.age,
                      breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,79,100),
                      labels=c("18.24","25.29","30.34","35.39","40.44","45.49",
                               "50.54","55.59","60.64","65.69","70.74","75.79","80"))) %>% 
  group_by(microsim.init.race, microsim.init.sex, agecat) %>% 
  summarise(n=sum(n)) %>% mutate(year=2000, datatype="census")

# compare2000 <- read.csv("input_data/education_constraints_long.csv") %>% group_by(sex, Race, age) %>% 
#   summarise(n=sum(newcount)) %>% mutate(datatype="census", year=2000) %>% 
#   rename(microsim.init.sex = sex, 
#          microsim.init.race = Race,
#          agecat = age)

compare2010 <- read.csv("input_data/overallcons2010.csv") %>% select(-c(X)) %>% pivot_longer(!STATE, names_to="cat", values_to="n") %>%
  filter(STATE=="USA") %>% 
  separate(cat, into=c("microsim.init.race","age","microsim.init.sex"),
           sep=c(3,5)) %>% mutate(age=as.numeric(age),
                                  agecat=cut(age,
                                             breaks=c(0,24,29,34,39,44,49,54,59,64,69,74,100),
                                             labels=c("18.24","25.29","30.34","35.39","40.44","45.49",
                                             "50.54","55.59","60.64","65.69","70.74","75.79"))) %>% 
  filter(age>=18) %>% filter(age<=79) %>% 
  group_by(microsim.init.sex, microsim.init.race, agecat) %>% summarise(n=sum(n)) %>% mutate(year=2010,
                                                                                             datatype="census")

compare <- rbind(summaryperyear, compare2000)
compare <- rbind(compare, compare2010)  
compare$microsim.init.race <- recode(compare$microsim.init.race, "BLA"="Black", "WHI"="White", "SPA"="Hispanic", "OTH"="Other")
compare$microsim.init.sex <- recode(compare$microsim.init.sex, "m"="Male", "f"="Female", "M"="Male","F"="Female")
compare$microsim.init.sex <- as.factor(compare$microsim.init.sex)
compare$microsim.init.race <- as.factor(compare$microsim.init.race)

####plot the output 

####THREE SEPARATE PLOTS ####
# comment this line in our out to get the population reported every year
# compare <- compare %>% filter(Year==1980 | Year==1990 | Year == 2000 | Year == 2010)

# compare$sum <- compare$sum*(1/proportion)

compare <- data.frame(compare)

compsex <- compare %>% 
  group_by(microsim.init.sex, year, datatype) %>% 
  summarise(sum=sum(n))

compsex$year <- as.numeric(as.character(compsex$year))
library(ggplot2)

psex <- ggplot(compsex, aes(sum))

bars <- compsex %>% 
  filter(datatype=="census")

points <- compsex %>% 
  filter(datatype=="microsim")
p1 <- psex + geom_bar(data=bars, aes(fill=datatype, x=year, y=sum, group=datatype), colour="darkblue", stat="identity", position="dodge") +
  geom_point(data=points, aes(x=year, y=sum, group=datatype, shape=datatype), colour="darkblue", size=2) + facet_grid(~microsim.init.sex) + 
  geom_line(data=points, aes(x=year, y=sum, group=datatype), colour="darkblue") + 
  scale_fill_manual(name="",values="white") + scale_shape_manual(name="", values=18) +
  theme_classic() + ylab("Total Population") + xlab("Year") +theme(legend.title=NULL, legend.margin = margin(-1,0,0,0, "cm")) +
  scale_y_continuous(expand=c(0,0), labels=function(x) 
    format(x, big.mark=",", scientific=FALSE))
p1

ggsave("plot_compare.png", p1, dpi=300, width=33, height=19, units="cm")

# , limits=c(0,max(points$sum)+max(points$sum)*0.2)

compage <- compare %>% 
  group_by(agecat, year, datatype) %>% 
  summarise(sum=sum(n))

compage$year <- as.numeric(as.character(compage$year))

page <- ggplot(compage, aes(sum))

bars <- compage %>% 
  filter(datatype=="census")

points <- compage %>% 
  filter(datatype=="microsim")

p2 <- page + geom_bar(data=bars, aes(fill=datatype, x=year, y=sum, group=datatype), colour="darkblue", stat="identity", position="dodge") +
  geom_point(data=points, aes(x=year, y=sum, group=datatype, shape=datatype), colour="darkblue", size=2) + facet_grid(~agecat) + 
  geom_line(data=points, aes(x=year, y=sum, group=datatype), colour="darkblue") + 
  scale_fill_manual(name="",values="white") + scale_shape_manual(name="", values=18) +
  theme_classic() + ylab("Total Population") + xlab("Year") +theme(legend.title=NULL, legend.margin = margin(-1,0,0,0, "cm"))

p2

comprace <- compare %>% 
  group_by(microsim.init.race, year, datatype) %>% 
  summarise(sum=sum(n))

comprace$year <- as.numeric(as.character(comprace$year))

prace <- ggplot(comprace, aes(sum))

bars <- comprace %>% 
  filter(datatype=="census")

points <- comprace %>% 
  filter(datatype=="microsim")

p3 <- prace + geom_bar(data=bars, aes(fill=datatype, x=year, y=sum, group=datatype), colour="darkblue", stat="identity", position="dodge") +
  geom_point(data=points, aes(x=year, y=sum, group=datatype, shape=datatype), colour="darkblue", size=2) + facet_grid(~microsim.init.race) + 
  geom_line(data=points, aes(x=year, y=sum, group=datatype), colour="darkblue") + 
  scale_fill_manual(name="",values="white") + scale_shape_manual(name="", values=18) + 
  theme_classic() + ylab("Total Population") + xlab("Year") +theme(legend.title=NULL, legend.margin = margin(-1,0,0,0, "cm")) +
  scale_y_continuous(expand=c(0,0), limits=c(0,max(points$sum)+max(points$sum)*0.2), labels=function(x) 
    format(x, big.mark=",", scientific=FALSE))

p3

library(gridExtra)

gridplot <- grid.arrange(p1, p2, p3)
gridplot


ggsave(paste(SelectedState, ".png"), plot=gridplot, width=33, height=19, units="cm", dpi=1500)
# do a detailed breakdown plot - by age/sex/ race 

compare <- compare %>% filter(Year==1990 | Year == 2000 | Year == 2010)

ggplot(data=compare, aes(x=age, y=sum, fill=as.factor(data))) + geom_bar(stat="identity", position="dodge") +
  facet_wrap(~sex+race+Year, scales="free", nrow=3)

# work out % differences for the paper 

age <- compare %>% group_by(Year, data, age) %>% 
  summarise(total=sum(sum))

age <- spread(age, data,total)
age$percentdiff <- ((age$microsimulation-age$census) / age$census)*100

sex <- compare %>% group_by(Year, data, sex) %>% 
  summarise(total=sum(sum))
sex <- spread(sex, data, total)
sex$percentdiff <- ((sex$microsimulation-sex$census)/sex$census)*100

race <- compare %>% group_by(Year, data, race) %>% 
  summarise(total=sum(sum))

race <- spread(race, data, total)
race$percentdiff <- ((race$microsimulation-race$census)/race$census)*100

# detailed % differences 
detailed <- compare %>% group_by(Year, data, agecat, race, sex) %>% 
  summarise(total=sum(sum))
detailed <- spread(detailed, data, total)
detailed$percentdiff <- ((detailed$microsimulation-detailed$census)/detailed$census)*100
