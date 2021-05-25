library(dplyr)
library(tidyr)
library(ggplot2)

k.wd <- c("C:/Users/Marie/Dropbox/NIH2020/Protocol_paper")
setwd(k.wd)

summary <- read.csv("output_data/1_mortality_rates_data_SES.csv")

data_graph <- subset(summary, cause!="REST" )
Data <- c("dashed", "solid")
Data <- Data[as.numeric(as.factor(data_graph$datatype))]
symbol <- c(16, 21)
symbol <- symbol[as.numeric(as.factor(data_graph$datatype))]
data_graph$edclass <- recode(data_graph$edclass, 
                             "LEHS" = "High school degree or less", 
                             "SomeC" = "Some college", 
                             "College" = "College degree or more")
data_graph$edclass <- factor(data_graph$edclass, 
                             levels = c("High school degree or less", 
                                        "Some college", 
                                        "College degree or more"))

data_graph$Sex <- recode(data_graph$Sex, "m" = "Men", "f" = "Women")

data_graph$cause <- recode(data_graph$cause, 
                                 "AUD" = "AUD",
                                 "LVDC" = "Liver C.",
                                 "IJ" = "Suicide",
                                 "MVACC" = "MVA", 
                                 "UIJ" = "Other UI",
                                 "IHD" = "IHD", 
                                 "HYPHD" = "HHD",
                                 "STR" = "Stroke",  
                                 "DM" = "Diabetes")
data_graph$cause <- factor(data_graph$cause, levels = c("AUD", "Liver C.", 
                                                        "Suicide", "MVA", 
                                                        "Other UI", "IHD", "HHD", 
                                                        "Stroke", "Diabetes"))
data_graph$datatype <- recode(data_graph$datatype, "microsim" = "Microsimulation", "target"= "Observed")

color.vec <- c("#132268", "#447a9e", "#93AEBF")
ggplot(data=data_graph, aes(x=year, y=rate, colour=edclass)) + 
  facet_grid(rows = vars(cause), cols = vars(Sex), scales = "free") +
  theme_light() +
  theme(strip.background = element_rect(fill = "white"), legend.position = "bottom",  legend.box = "horizontal", legend.box.just = "top") +
  theme(strip.text = element_text(colour = 'black'), text = element_text(size = 14)) +
  ylab("Age standardized mortality rate per 100,000") + xlab("Year") +
  ylim(0, NA) +
  scale_color_manual(values = color.vec) +
  geom_line(aes(linetype=datatype, size = datatype), alpha= .75) + 
  #geom_line(aes(linetype=datatype), size = 0.7, alpha= .7) + 
  scale_linetype_manual(values = c(1, 3, 3, 3)) +
  scale_size_manual(breaks=c("Microsimulation", "Observed"), values=c(1, 0.7, 0.7, 0.7)) +
  labs(color="Education", linetype = "Data type", size = "Data type") +
  guides(color = guide_legend(nrow = 3), linetype = guide_legend(nrow = 2), size = guide_legend(nrow = 2))
ggsave("graphs/1_mortality_rates_graph.jpeg", dpi=600, width=18, height=25, units="cm")
write.csv(data_graph, "output_data/1_out_mortality_rates_SES.csv")

