
sex <- c("m","f")
race <- c("white","black","hispanic","other")
education <- c("low","med","high")


basepop <- data.frame(uniqueID=1:100, sex = sample(sex))

nesarc <- data.frame(sex = sex)
nesarc$beer <- runif(2, min=0, max=1)
nesarc$wine <- runif(2, min=0, max=1)
nesarc$spirits <- runif(2, min=0, max=1)

basepop <- left_join(basepop, nesarc)

nesarc <- expand.grid(sex = sex, race=race, education=education)
nesarc$beer <- runif(2, min=0, max=1)
nesarc$wine <- runif(2, min=0, max=1)
nesarc$spirits <- runif(2, min=0, max=1)

nesarc <- nesarc %>% dplyr::select(sex, beer, wine,spirits) %>% 
  group_by(sex) %>% 
  summarise(beer=mean(beer), wine=mean(wine), spirits=mean(spirits))
