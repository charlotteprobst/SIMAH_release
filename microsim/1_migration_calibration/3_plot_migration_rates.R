library(tidyverse)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)
migration_rates <- read.csv("SIMAH_workplace/microsim/1_input_data/birth_migration_rates_USA.csv")

births <- migration_rates %>% dplyr::select(year,race,
                                            sex,birthrate) %>% 
  drop_na()

predicted <- expand.grid(year=2020:2025, sex=unique(births$sex),
                         race = unique(births$race),
                         birthrate=NA)

births <- rbind(births,predicted)

pred_function <- function(data){
  model <- lm(birthrate ~ year, data)
  data$predicted <- predict(model, data)
  return(data)
}

modelled <- births %>% 
  group_by(sex, race) %>% 
  do(pred_function(.)) %>% 
  pivot_longer(cols=c(birthrate, predicted)) %>% 
  mutate(name=factor(name, levels=c("birthrate","predicted")),
         sex = ifelse(sex=="m","Men","Women"),
         race = recode(race,
                                     "BLA"="Black",
                                     "WHI"="White","SPA"="Hispanic",
                                     "OTH"="Others"),
         value = ifelse(name=="predicted" & year<2018, NA, value))

ggplot(data=modelled, aes(x=year, y=value, linetype=name)) + 
  geom_line(size=1) + facet_grid(cols=vars(sex), 
                                 rows=vars(race), scales="free") +
  xlim(2000,2025) + theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank()) + ylim(0,NA)

ggsave("SIMAH_workplace/demography/plots/modelled_birthrates.png",
       dpi=300, width=33, height=19, units="cm")

migration <- migration_rates %>% dplyr::select(year,race,
                                            sex,agecat, 
                                            migrationinrate,
                                            migrationoutrate) %>% 
  drop_na() %>% 
  mutate( sex = ifelse(sex=="m","Men","Women"),
          race = recode(race,
                                      "BLA"="Black",
                                      "WHI"="White","SPA"="Hispanic",
                                      "OTH"="Others"))

ggplot(data=migration, aes(x=year, y=migrationinrate,colour=sex)) + 
  geom_line(size=1) + facet_grid(cols=vars(agecat), 
                                 rows=vars(race), scales="free") + 
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank()) + ylim(0,0.15)
ggsave("SIMAH_workplace/demography/plots/modelled_migrationinrates.png",
       dpi=300, width=33, height=19, units="cm")

ggplot(data=migration, aes(x=year, y=migrationoutrate,colour=sex)) + 
  geom_line(size=1) + facet_grid(cols=vars(agecat), 
                                 rows=vars(race), scales="free") + 
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank()) + ylim(0,0.15)
ggsave("SIMAH_workplace/demography/plots/modelled_migrationoutrates.png",
       dpi=300, width=33, height=19, units="cm")
