# SIMAH project Sep 2023 
# analysing the run to run variability of education transitions
library(tidyverse)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)
# WorkingDirectory <- "/home/cbuckley/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/education_calibration/newage")

final_TPs <- read_rds(paste0(DataDirectory, "/transitionsList-9.RDS"))

for(i in 1:length(final_TPs)){
  final_TPs[[i]]$samplenum <- i
  final_TPs[[i]]$probability <- c(0, diff(final_TPs[[i]]$cumsum))
  # final_TPs[[i]]$probability <- ifelse(final_TPs[[i]]$probability<0, 0, final_TPs$probability)
}

TPs <- do.call(rbind, final_TPs)

TPs_new <- TPs %>% 
  mutate(cat = gsub("1999-2019_","", cat),
         cat = gsub("_STATEFROM", "", cat),
         agecat = case_when(grepl("18", cat) ~ "18",
                            grepl("19", cat) ~ "19",
                            grepl("20", cat) ~ "20",
                            grepl("21", cat) ~ "21",
                            grepl("22-24", cat) ~ "22-24",
                            grepl("25+", cat) ~ "25+"),
         sex = case_when(grepl("m", cat) ~ "Men",
                         grepl("f", cat) ~ "Women"),
         race = case_when(grepl("black", cat) ~ "Black",
                          grepl("white", cat) ~ "White",
                          grepl("hispanic", cat) ~ "Hispanic",
                          grepl("other", cat) ~ "Others"),
         cat = substr(cat, 7,20),
         StateFrom = parse_number(cat),
         probability = ifelse(probability<0, 0, probability)) %>% ungroup() %>% 
  dplyr::select(samplenum, StateFrom, StateTo,agecat, sex, race, probability)
           
# summarise the transition to going to college 
gotocollege <- TPs_new %>% 
  filter(StateFrom==1 & StateTo==2) %>% group_by(agecat, race, sex) %>% 
  summarise(min=min(probability), max=max(probability))

gotocollege %>%
  ggplot(aes(x = agecat)) +
  geom_linerange(aes(ymin = min, ymax = max, x = agecat),
                 size = 1.5) +
  geom_point(aes(y=max)) + 
  geom_point(aes(y=min)) + 
  coord_flip() +
  theme_bw(base_size = 16) +
  theme(axis.title.y = element_blank()) + 
  facet_grid(cols=vars(race), rows=vars(sex)) + 
  ggtitle("TP ranges - transition from LEHS to 1 year college") + 
  ylab("Transition probability")

ggsave("SIMAH_workplace/microsim/2_output_data/education_calibration/newage/TPrange_y1college.png",
       dpi=300, width=33, height=19, units="cm")

summary_range <- outputsummary %>% 
  group_by(YEAR, wave, SEX, EDUC, RACE) %>% 
  summarise(min=min(propsimulation), max=max(propsimulation),
            proptarget=mean(proptarget))

ggplot(data=subset(summary_range, SEX=="Men"), 
       aes(x=as.numeric(YEAR), colour=as.factor(EDUC))) + 
  # geom_line(linewidth=1) + 
  geom_ribbon(aes(ymin=min, ymax=max, colour=as.factor(EDUC), fill=as.factor(EDUC)),
              alpha=0.5) + 
  geom_line(aes(x=YEAR,y=proptarget,colour=as.factor(EDUC)), linewidth=1,
            linetype="dashed") +
  # geom_line(aes(x=YEAR,y=PSID_new), colour="purple",linewidth=1, linetype="dashed") +
  facet_grid(cols=vars(wave), rows=vars(RACE)) + 
  theme_bw() + 
  theme(legend.title=element_blank(),
        legend.position="bottom") +
  ggtitle("Men, all ages") + 
  xlab("Year") + ylim(0,1)

ggsave("SIMAH_workplace/microsim/2_output_data/education_calibration/newage/waves_range_men.png",
       dpi=300, width=33, height=19, units="cm")

# plot the best fitting model from the best wave
best <- outputsummary %>% filter(wave==9) %>% 
  filter(percentile==1) %>% 
  pivot_longer(prop:target) %>% 
  filter(AGECAT=="35-44" | AGECAT=="45-54")

ggplot(data=subset(best, SEX=="Men" & samplenum==4), 
       aes(x=as.numeric(YEAR), y=value, colour=as.factor(EDUC), linetype=as.factor(name))) + 
  geom_line(linewidth=1) + 
  # geom_line(aes(x=YEAR,y=target, colour=as.factor(EDUC)), linetype="dashed",linewidth=1) + 
  facet_grid(cols=vars(RACE), rows=vars(AGECAT)) + 
  scale_linetype_manual(values=c("dotdash","solid"), labels=c("simulation","target")) + 
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.title=element_blank()) + 
  # ggtitle("age 18-24") + 
  xlab("Year") +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) 
ggsave("SIMAH_workplace/microsim/2_output_data/education_calibration/best_setting35+.png",
       dpi=300, width=33, height=19, units="cm")
# plot range for final wave

ggplot(data=subset(outputsummary, SEX=="Women" & AGECAT=="18-24" & wave==9), 
       aes(x=as.numeric(YEAR), y=prop, colour=as.factor(samplenum))) + 
  geom_line(linewidth=1) + 
  geom_line(aes(x=YEAR,y=target), colour="darkblue",linewidth=1) +
  # geom_line(aes(x=YEAR,y=PSID_new), colour="purple",linewidth=1, linetype="dashed") +
  facet_grid(cols=vars(RACE), rows=vars(EDUC)) + 
  theme_bw() + 
  theme(legend.position = "none") + 
  ggtitle("Women, 18-24") + 
  xlab("Year") + ylim(0,1)
ggsave("SIMAH_workplace/microsim/2_output_data/education_calibration/posterior_women.png",
       dpi=300, width=33, height=19, units="cm")

pct_diff <- outputsummary %>% filter(wave==5) %>% 
  filter(percentile==1) %>% 
  mutate(pct_diff = abs(prop-target)/target,
         abs_diff = abs(prop-target))
