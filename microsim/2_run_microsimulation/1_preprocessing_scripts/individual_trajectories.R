# correlations of year on year drinking 

Pops <- do.call(rbind, PopPerYear) %>% 
  dplyr::select(microsim.init.id, year, microsim.init.education, microsim.init.sex, microsim.init.alc.gpd)

YearonYear <- Pops %>% pivot_wider(names_from=year, values_from=microsim.init.alc.gpd)

cor.test(YearonYear$`2001`, YearonYear$`2002`)

cor.test(YearonYear$`2002`, YearonYear$`2003`)

cor.test(YearonYear$`2003`, YearonYear$`2004`)

cor.test(YearonYear$`2004`, YearonYear$`2005`)


sampled <- Pops %>% group_by(microsim.init.sex, microsim.init.education) %>% filter(year==2000) %>% 
  sample_n(5)

selected <- Pops %>% filter(microsim.init.id %in% sampled$microsim.init.id)

ggplot(data=selected, aes(x=year, y=microsim.init.alc.gpd, colour=as.factor(microsim.init.id))) + geom_line(size=1) +
  facet_grid(cols=vars(microsim.init.education),
             rows=vars(microsim.init.sex)) + theme_bw() + theme(legend.position="none")
ggsave("SIMAH_workplace/microsim/2_output_data/individual_trajectories_gpd.png", dpi=300,
       width=33, height=19, units="cm")


library(tidyverse)
library(MASS)
library(magrittr)
library(caTools)
library(party)
library(fitdistrplus)

dat <- readRDS("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_1984_2020_final.RDS") %>% filter(YEAR>=2000 & YEAR<=2005) %>% 
  filter(drinkingstatus==1) %>% filter(State=="USA") %>% 
  dplyr::select(YEAR, sex_recode, education_summary, gramsperday)

