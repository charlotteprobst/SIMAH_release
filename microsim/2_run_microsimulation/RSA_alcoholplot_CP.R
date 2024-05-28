# plotting RSA results for policy experiments 
# creating plots for RSA based on alcohol policy experiments 
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(tidyverse)
WorkingDirectory <- "C:/Users/marie/Dropbox/NIH2020/Presentations/RSA 2023/graphs/"
setwd(WorkingDirectory)
# now read in the policy effects
data <- read.csv("processed_alcoholchange_data.csv") %>% 
      mutate(education = factor(education, levels=c("High school or less","Some college","College +")),
             percentreduction = factor(percentreduction, levels=c("0%","7.1%","10.8%","14.5%")), 
             year = year -1)

ggplot(data=data, aes(x=year, y=mean, colour=as.factor(percentreduction))) + 
      geom_line(linewidth=1) + 
      facet_grid(rows=vars(sex), cols=vars(education), scales="fixed") +
      expand_limits(y = 0) +
      scale_colour_manual(values=c("#C00000", "#B3E1D6", "#1A5464", "#2E8A95")) +
      theme_bw() + 
      theme(legend.position="bottom",
            strip.background = element_rect(fill="white"),
            text = element_text(size=25),
            axis.text = element_text(size=25),
            plot.title=element_text(hjust=0.5, size=25)) +
            ylab("Mean alcohol use (grams per day)") + 
   geom_vline(xintercept=2015, linetype="dashed") + 
   scale_x_continuous(breaks=c(2010, 2012, 2014, 2016, 2018), limits = c(2010,2018)) +
      xlab("Year") + labs(color = "Percent reduction")

ggsave("alcohol_change.png", dpi=500, width=36, height=19, units="cm")
