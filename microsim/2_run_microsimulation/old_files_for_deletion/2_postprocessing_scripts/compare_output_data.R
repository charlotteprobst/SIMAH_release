# Output <- readRDS(paste("3_output_data/PopSummary", SelectedState, ".RDS", sep=""))

PopCounts <- Output[[1]]

ACS <- read.csv("1_input_data/ACS_popcounts_unweighted.csv") %>% group_by(YEAR,SEX,RACE,
                                                               AGECAT) %>% 
  filter(STATE==SelectedState) %>% summarise(n=sum(n)) %>% mutate(data = "ACS") %>% 
  filter(AGECAT!="80+") %>% filter(YEAR>=2006) %>% mutate(n=n*proportion)

for(i in 2000:2018){
  PopCounts[[paste(i)]] <- PopCounts[[paste(i)]] %>% 
    mutate(YEAR= i,
           AGECAT = cut(microsim.init.age,
                        breaks=c(0,24,34,44,54,64,100),
                        labels=c("18-24","25-34","35-44","45-54",
                                 "55-64","65-79"))) %>% 
    group_by(YEAR, microsim.init.sex, microsim.init.race, AGECAT) %>% tally() %>% 
    rename(SEX = microsim.init.sex, RACE=microsim.init.race) %>% 
    mutate(data="microsim",
           SEX = recode(SEX, "m"="M","f"="F"),
           RACE = recode(RACE, "BLA"="Black", "SPA"="Hispanic","WHI"="White","OTH"="Other"))
}

PopCounts <- do.call(rbind, PopCounts)
names(PopCounts)
names(ACS)
PopCounts <- rbind(PopCounts, ACS)
unique(PopCounts$AGECAT)
unique(PopCounts$SEX)
unique(PopCounts$RACE)

census2000 <- read.csv("1_input_data/processed_agesexrace.csv") %>% filter(STATE==SelectedState) %>% 
  group_by(sex, race, agecat) %>% summarise(n=sum(count)) %>% mutate(YEAR=2000, data="Census") %>% 
  rename(SEX=sex, RACE=race, AGECAT=agecat) %>% 
  mutate(RACE = recode(RACE, "BLA"="Black", "SPA"="Hispanic","WHI"="White","OTH"="Other")) %>% 
  mutate(n=n*proportion)
#   
census2010 <- read.csv("1_input_data/processed_indagesexrace2010.csv") %>% filter(STATE==SelectedState) %>% 
  group_by(sex, race, agecat) %>% summarise(n=sum(count)) %>% mutate(YEAR=2010, data="Census") %>% 
  rename(SEX=sex, RACE=race, AGECAT=agecat) %>% mutate(RACE = recode(RACE, "BLA"="Black", "SPA"="Hispanic","WHI"="White","OTH"="Other")) %>% 
  mutate(n=n*proportion)

PopCounts <- rbind(PopCounts, census2000, census2010)
PopCounts$SEX <- recode(PopCounts$SEX, "M"="Men","F"="Women")
# PopCounts <- PopCounts %>% filter(YEAR<=2010)

Men <- PopCounts %>% filter(SEX=="Men")

plot1 <- ggplot(data=Men, aes(x=YEAR, y=n, group=data, colour=data, size=data)) + 
  geom_line() + facet_grid(rows=vars(RACE), cols=vars(AGECAT), scales="free") + geom_point() + 
  theme_bw() + ylim(0,NA) + scale_size_manual(values=c(1,2,1))
plot1

Women <- PopCounts %>% filter(SEX=="Women")

plot2 <- ggplot(data=Women, aes(x=YEAR, y=n, group=data, colour=data, size=data)) + 
  geom_line() + facet_grid(rows=vars(RACE), cols=vars(AGECAT), scales="free") + geom_point() + 
  theme_bw() + ylim(0,NA) + scale_size_manual(values=c(1,2,1))
plot2

ggsave(paste("3_plots/", SelectedState, "Men.png",sep=""), plot1, dpi=300, height=19, width=33, units="cm")
ggsave(paste("3_plots/", SelectedState, "Women.png",sep="") ,plot2, dpi=300, height=19, width=33, units="cm")



p1 <- psex + geom_bar(data=bars, aes(fill=data, x=YEAR, y=n, group=data), colour="darkblue", stat="identity", position="dodge") +
  geom_point(data=points, aes(x=YEAR, y=n, group=data, shape=data), colour="darkblue", size=2) + facet_grid(rows=vars(SEX), cols=vars(AGECAT)) + 
  geom_line(data=points, aes(x=YEAR, y=n, group=data), colour="darkblue") + 
  scale_fill_manual(name="",values="white") + scale_shape_manual(name="", values=18) + ggtitle("Kentucky") +
  theme_classic() + ylab("Total Population") + xlab("Year") +theme(legend.title=NULL, legend.margin = margin(-1,0,0,0, "cm")) +
  scale_y_continuous(expand=c(0,0), labels=function(x) 
    format(x, big.mark=",", scientific=FALSE))
p1
ggsave("3_plots/Kentucky.png",p1, dpi=300, height=19, width=33, units="cm")
