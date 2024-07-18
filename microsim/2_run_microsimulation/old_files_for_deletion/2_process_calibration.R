#####SIMAH project 2022 - script for running SIMAH microsimulation model
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(devtools)
library(roxygen2)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(purrr)
library(fitdistrplus)
library(lhs)
library(truncnorm)
library(data.table)
library(gridExtra)
library(foreach)
library(doParallel)
options(dplyr.summarise.inform = FALSE)

###set working directory to the main "SIMAH" folder in your directory 
# WorkingDirectory <- "U:/SIMAH/"
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"

DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/1_input_data/")

# load in microsim R package
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)

Output <- read_csv("SIMAH_workplace/microsim/2_output_data/SIMAH_calibration/Output_basecalibration.csv")

inflation_factor <- 200
diseases <- c("LVDC", "AUD", "IJ")
SelectedState <- "USA"

# summarise_mortality_output_calibration() function



plot1 <- ggplot(data=subset(summary_agest, sex=="Women"), aes(x=year, y=simulated, colour=as.factor(percentreduction))) +
  geom_line(size=1) +
  geom_line(aes(x=year, y=observed), colour="black") +
  facet_grid(cols=vars(cause),
             rows=vars(education), scales="free") +
  ylab("Age-st Mortality per 100,000") +
  xlab("") + theme_bw() + theme(legend.position = "bottom",
                                legend.title=element_blank(),
                                strip.background=element_rect(fill="white"),
                                text = element_text(size=18)) +
  ggtitle("Women") +
  scale_y_continuous(labels=scaleFUN, limits=c(0,NA)) +
  xlim(2014,2019)
# geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, colour=name, fill=name)) +
# scale_colour_manual(values=c("#93aebf","#132268"))
plot1

plot1 <- ggplot(data=subset(summary_agest, sex=="Men"), aes(x=year, y=simulated, colour=as.factor(percentreduction))) +
  geom_line() +
  geom_line(aes(x=year, y=observed), colour="black", size=1) +
  facet_grid(cols=vars(cause),
             rows=vars(education), scales="free") +
  ylab("Age-st Mortality per 100,000") +
  xlab("") + theme_bw() + theme(legend.position = "bottom",
                                legend.title=element_blank(),
                                strip.background=element_rect(fill="white"),
                                text = element_text(size=18)) +
  ggtitle("Women") +
  scale_y_continuous(labels=scaleFUN, limits=c(0,NA)) +
  xlim(2014,2019)
# geom_ribbon(aes(ymin=lower_ci, ymax=upper_ci, colour=name, fill=name)) +
# scale_colour_manual(values=c("#93aebf","#132268"))
plot1

analysis <- summary_agest %>% 
  filter(year>=2014) %>% 
  dplyr::select(percentreduction, cause, sex, education,
                year, simulated) %>% 
  filter(samplenum==1 | samplenum==2 | samplenum==3 | samplenum==4) %>% 
  pivot_wider(names_from=year, values_from=simulated) %>% 
  mutate(pct_reduction = (`2015`-`2014`)/`2014`) %>% 
  filter(samplenum==1 | samplenum==2) %>% 
  dplyr::select(percentreduction, cause, sex, education, pct_reduction, `2014`,`2015`) %>% 
  filter(sex=="Men" & education=="LEHS")
  
