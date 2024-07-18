library(tidyverse)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)
migration_rates <- read.csv("SIMAH_workplace/microsim/1_input_data/birth_migration_rates_USA.csv")

births <- migration_rates %>% dplyr::select(year,microsim.init.race,
                                            microsim.init.sex,birthrate) %>% 
  drop_na()

predicted <- expand.grid(year=2020:2025, microsim.init.sex=unique(births$microsim.init.sex),
                         microsim.init.race = unique(births$microsim.init.race),
                         birthrate=NA)

births <- rbind(births,predicted)

pred_function <- function(data){
  model <- lm(birthrate ~ year, data)
  data$predicted <- predict(model, data)
  return(data)
}

modelled <- births %>% 
  group_by(microsim.init.sex, microsim.init.race) %>% 
  do(pred_function(.)) %>% 
  pivot_longer(cols=c(birthrate, predicted)) %>% 
  mutate(name=factor(name, levels=c("birthrate","predicted")),
         microsim.init.sex = ifelse(microsim.init.sex=="m","Men","Women"),
         microsim.init.race = recode(microsim.init.race,
                                     "BLA"="Black",
                                     "WHI"="White","SPA"="Hispanic",
                                     "OTH"="Others"),
         value = ifelse(name=="predicted" & year<2018, NA, value))

ggplot(data=modelled, aes(x=year, y=value, linetype=name)) + 
  geom_line(size=1) + facet_grid(cols=vars(microsim.init.sex), 
                                 rows=vars(microsim.init.race), scales="free") +
  xlim(2000,2025) + theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank()) + ylim(0,NA)

ggsave("SIMAH_workplace/demography/plots/modelled_birthrates.png",
       dpi=300, width=33, height=19, units="cm")

migration <- migration_rates %>% dplyr::select(year,microsim.init.race,
                                            microsim.init.sex,agecat, 
                                            migrationinrate,
                                            migrationoutrate) %>% 
  drop_na() %>% 
  mutate( microsim.init.sex = ifelse(microsim.init.sex=="m","Men","Women"),
          microsim.init.race = recode(microsim.init.race,
                                      "BLA"="Black",
                                      "WHI"="White","SPA"="Hispanic",
                                      "OTH"="Others"))

ggplot(data=migration, aes(x=year, y=migrationinrate,colour=microsim.init.sex)) + 
  geom_line(size=1) + facet_grid(cols=vars(agecat), 
                                 rows=vars(microsim.init.race), scales="free") + 
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank()) + ylim(0,0.15)
ggsave("SIMAH_workplace/demography/plots/modelled_migrationinrates.png",
       dpi=300, width=33, height=19, units="cm")

ggplot(data=migration, aes(x=year, y=migrationoutrate,colour=microsim.init.sex)) + 
  geom_line(size=1) + facet_grid(cols=vars(agecat), 
                                 rows=vars(microsim.init.race), scales="free") + 
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank()) + ylim(0,0.15)
ggsave("SIMAH_workplace/demography/plots/modelled_migrationoutrates.png",
       dpi=300, width=33, height=19, units="cm")
