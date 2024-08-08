# plotting RSA results for policy experiments 
# creating plots for RSA based on alcohol policy experiments 
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(tidyverse)
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)
# now read in the policy effects
data <- read.csv("SIMAH_workplace/microsim/2_output_data/policy_experiments/processed_policy_data.csv")

# NEW VERSION - change in mortality compared to 0% condition 
policydata <- data %>%
  dplyr::select(year, percentreduction, cause, sex, education, simulated_rate, simulated_count) %>% 
  group_by(year, percentreduction, cause, sex, education) %>% 
  filter(year>=2013) %>% 
  pivot_wider(names_from=percentreduction, values_from=c(simulated_rate:simulated_count)) %>% 
  summarise(rate_difference = `simulated_rate_10.8%`- `simulated_rate_0%`,
         rate_lower = `simulated_rate_7.1%` - `simulated_rate_0%`,
         rate_upper = `simulated_rate_14.5%` - `simulated_rate_0%`,
         count_difference = `simulated_count_10.8%`-`simulated_count_0%`,
         count_lower = `simulated_count_7.1%`-`simulated_count_0%`,
         count_upper = `simulated_count_14.5%` - `simulated_count_0%`,
         pct_difference = (`simulated_count_10.8%`-`simulated_count_0%`)/`simulated_count_0%`,
         pct_lower = (`simulated_count_7.1%`-`simulated_count_0%`)/`simulated_count_0%`,
         pct_upper = (`simulated_count_14.5%` - `simulated_count_0%`)/`simulated_count_0%`) %>% 
  mutate(education=factor(education, levels=c("High school or less","Some college","College +"))) %>% 
    filter(education!="Some college")

title <-"Change in mortality from liver cirrhosis, suicide and alcohol use disorders following taxation policy (10.8% reduction in alcohol consumption)" 


ggplot(data=policydata, aes(x=year, y=pct_difference, colour=as.factor(sex))) + 
  geom_line(linewidth=1) + 
  # geom_ribbon(aes(ymin=count_lower, ymax=count_upper,fill=as.factor(sex)), colour=NA, alpha=0.2) +
  facet_grid(rows=vars(cause), cols=vars(education), scales="fixed") + 
  scale_colour_brewer(palette="Dark2", name="") + 
  scale_fill_brewer(palette="Dark2", name="") + 
  # scale_linetype_manual(values=c("solid","dashed"), name="") + 
  theme_bw() + 
  theme(legend.position="bottom",
        strip.background = element_rect(fill="white"),
        text = element_text(size=15),
        axis.text = element_text(size=15),
        plot.title=element_text(hjust=0.5, size=18)) +
  ylab("Percentage change in mortality") +
  xlab("") + 
  geom_vline(xintercept=2015, linetype="dashed") + 
  geom_hline(yintercept=0, linetype="dashed") + 
  scale_y_continuous(labels=scales::percent) +
  xlim(2013, 2019) + 
  labs(title = str_wrap(title, 90),
       caption = "Values are percentage changes in mortality compared to baseline model (0% change in consumption)") + 
  coord_cartesian(clip = 'on', xlim=c(2013, 2017), expand=TRUE)
ggsave("SIMAH_workplace/microsim/2_output_data/policy_experiments/policy_change_full.png", dpi=500, width=25, height=19, units="cm")

# extract values for reporting
reporting <- policydata %>% 
  filter(year==2015) %>% dplyr::select(cause, sex, education, count_difference, pct_difference)

# modelled overall reductions in mortality 
reporting <- policydata %>% 
  group_by(cause, sex, education) %>% 
  filter(year>=2015) %>% 
  summarise(totalreduction = sum(count_difference))


title <-"Absolute (total deaths) changes in mortality over the modelled period 2015 - 2019 following taxation policy" 

# draw a bar chart of the overall reductions in mortality 
ggplot(data=reporting, aes(x=education, y=totalreduction, fill=sex)) + 
  geom_bar(stat="identity", position="dodge", colour="black") + 
  facet_grid(rows=vars(cause)) + 
  scale_fill_brewer(palette="Dark2", name="") + 
  # scale_linetype_manual(values=c("solid","dashed"), name="") + 
  theme_bw() + 
  theme(legend.position="bottom",
        strip.background = element_rect(fill="white"),
        text = element_text(size=15),
        axis.text = element_text(size=15),
        plot.title=element_text(hjust=0.5, size=18)) + 
  xlab("Educational attainment") + 
  ylab("Absolute reduction in mortality") + 
  labs(title = str_wrap(title, 90))

ggsave("SIMAH_workplace/microsim/2_output_data/policy_experiments/policy_change_counts.png", 
       
       dpi=500, width=30, height=19, units="cm")

# OLD VERSION - change in mortality from previous year
# summarise as change in mortality 
policymodel <- data %>%
  group_by(year, percentreduction, cause, sex, education) %>% 
  filter(year>=2014) %>% 
  summarise(simulated_count = sum(simulated_count)) %>% 
  ungroup() %>% group_by(percentreduction, sex, cause, education) %>% 
  mutate(change_score = simulated_count-lag(simulated_count),
         change_pct = (simulated_count-lag(simulated_count))/lag(simulated_count),
         change_score = ifelse(is.na(change_score), 0, change_score),
         change_pct = ifelse(is.na(change_pct), 0, change_pct)) %>% 
  dplyr::select(-c(simulated_count,change_score)) %>% 
  # pivot_wider(names_from=percentreduction, values_from=change_pct) %>% 
  # rename('Baseline'="0%", "Policy experiment"="10.8%", "Min"="7.1%","Max"="14.5%") %>% 
  # pivot_longer(Baseline:`Policy experiment`) 
  mutate(education=factor(education, levels=c("High school or less","Some college","College +")),
         percentreduction = factor(percentreduction, levels=c("0%","7.1%","10.8%","14.5%"))) %>% 
  filter(education!="Some college")

# look at overall mortality rates  
# bar chart for 2015 showing high and low SES and sex
# three bars for each of COD 
# show negative percentage change
# show change based on expected mortality for that year
# show percentage change compared to the 0% condition
# could also do the absolute differences compared to the 0% condition

# version 1 of the plot - comparison to % change year n year 

ggplot(data=subset(policymodel, cause=="Alcohol use disorder"), aes(x=year, y=change_pct, colour=as.factor(percentreduction))) + 
  geom_line(linewidth=1) + 
  # geom_ribbon(aes(ymin=Min, ymax=Max,fill=as.factor(name)), colour=NA) +
  facet_grid(rows=vars(sex), cols=vars(education), scales="free") + 
  scale_colour_brewer(palette="Dark2", name="") + 
  scale_fill_brewer(palette="Dark2", name="") + 
  # scale_linetype_manual(values=c("solid","dashed"), name="") + 
  theme_bw() + 
  theme(legend.position="bottom",
        strip.background = element_rect(fill="white"),
        text = element_text(size=15),
        plot.title=element_text(hjust=0.5, size=15)) +
  ylab("Percentage change in mortality") +
  xlab("") + 
  geom_vline(xintercept=2015, linetype="dashed") + 
  scale_y_continuous(labels=scales::percent) +
  xlim(2014, 2019) + 
  ggtitle("Change in mortality from liver cirrhosis, suicide and alcohol use disorders following taxation policy")
# coord_cartesian(clip = 'on', xlim=c(2014, 2015.5), expand=TRUE)

ggsave("SIMAH_workplace/microsim/2_output_data/policy_experiments/policy_change_full.png", dpi=500, width=25, height=19, units="cm")

# by cause 
policymodel <- data %>%
  group_by(year, percentreduction, sex, education) %>% 
  filter(year>=2014) %>% 
  summarise(simulated_count = sum(simulated_count)) %>% 
  ungroup() %>% group_by(percentreduction, sex, education) %>% 
  mutate(change_score = simulated_count-lag(simulated_count),
         change_pct = (simulated_count-lag(simulated_count))/lag(simulated_count),
         change_score = ifelse(is.na(change_score), 0, change_score),
         change_pct = ifelse(is.na(change_pct), 0, change_pct)) %>% 
  dplyr::select(-c(simulated_count,change_score)) %>% 
  # pivot_wider(names_from=percentreduction, values_from=change_pct) %>% 
  # rename('Baseline'="0%", "Policy experiment"="10.8%", "Min"="7.1%","Max"="14.5%") %>% 
  # pivot_longer(Baseline:`Policy experiment`) 
  mutate(education=factor(education, levels=c("High school or less","Some college","College +")),
         percentreduction = factor(percentreduction, levels=c("0%","7.1%","10.8%","14.5%"))) %>% 
  filter(education!="Some college")

ggplot(data=policymodel, aes(x=year, y=change_pct, colour=as.factor(percentreduction))) + 
  geom_line(linewidth=1) + 
  # geom_ribbon(aes(ymin=Min, ymax=Max,fill=as.factor(name)), colour=NA) +
  facet_grid(rows=vars(sex), cols=vars(education), scales="free") + 
  scale_colour_brewer(palette="Dark2", name="") + 
  scale_fill_brewer(palette="Dark2", name="") + 
  # scale_linetype_manual(values=c("solid","dashed"), name="") + 
  theme_bw() + 
  theme(legend.position="bottom",
        strip.background = element_rect(fill="white"),
        text = element_text(size=15),
        plot.title=element_text(hjust=0.5, size=15)) +
  ylab("Percentage change in mortality") +
  xlab("") + 
  geom_vline(xintercept=2015, linetype="dashed") + 
  scale_y_continuous(labels=scales::percent) +
  xlim(2014, 2019) + 
  ggtitle("Change in mortality from liver cirrhosis, suicide and alcohol use disorders following taxation policy")
# coord_cartesian(clip = 'on', xlim=c(2014, 2015.5), expand=TRUE)




analysis <- data %>%
  group_by(year, percentreduction, sex) %>% 
  filter(year>=2015) %>% 
  summarise(simulated_count = sum(simulated_count)) %>% 
  ungroup() %>% group_by(percentreduction, sex) %>% 
  pivot_wider(names_from=percentreduction, values_from=simulated_count) %>% 
  mutate(overall_reduction = `10.8%`-`0%`,
         pct_reduction = (`10.8%`-`0%`)/`0%`,
         abs_lower = (`7.1%`-`0%`)/`0%`,
         abs_upper = (`14.5%`-`0%`)/`0%`) %>% 
  filter(year==2015)

