sampled_TPs <- read.csv("1_input_data/sampled_TP300.csv")
TPList <- list()
for(i in 1:nsamples){
  TPList[[i]] <- sampled_TPs %>% filter(SampleNum==i) %>% mutate(sex=ifelse(sex=="male","m","f"),
                                        cat=paste(time, age, sex, race, "STATEFROM",StateFrom, sep="_")) %>% 
    ungroup() %>% select(cat, StateTo, prob) %>% group_by(cat) %>% 
    mutate(cumsum=cumsum(prob)) %>% select(cat, StateTo, cumsum) %>% data.frame(.)
}

# just one model run 
# TP <- read.csv("input_data/TP_tunnel2.csv") %>% mutate(sex=ifelse(sex=="male","m","f"),
#                                                        StateFrom=parse_number(StateFrom),
#                                                        StateTo=parse_number(StateTo),
#                                                        cat = paste(Time, age, sex, racefinal, "STATEFROM", StateFrom, sep="_")) %>% 
#   ungroup() %>% select(cat, StateTo, Prob) %>% group_by(cat) %>% 
#   mutate(cumsum=cumsum(Prob)) %>% select(cat, StateTo, cumsum) %>% data.frame()
# 
# TPList <- list()

# TPlist <- list()
# for(i in 1:nsamples){
#   sample <- paste("sample",i,sep="")
#   TPlist[[i]] <- parameters %>% mutate(sex=ifelse(sex=="male","m","f"),
#                                                   cat=paste(time, age, sex, race, "STATEFROM",StateFrom, sep="_")) %>%
#     ungroup() %>%
#     select(cat, StateTo, sample) %>% group_by(cat, StateTo) %>% rename(prob = sample) %>%
#     group_by(cat) %>% mutate(cumsum=cumsum(prob)) %>% select(-c(prob)) %>% data.frame(.)
# }
