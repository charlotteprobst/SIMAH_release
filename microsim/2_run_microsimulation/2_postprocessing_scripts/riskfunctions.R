# plotting for publication the risk functions
library(dplyr)
library(tidyr)
library(ggplot2) 

setwd("~/Google Drive/SIMAH Sheffield/")
riskfunctioninput <- read.csv("SIMAH_workplace/microsim/2_output_data/publication/riskfunctions.csv")

sample <- function(data){
  mean <- data$mean
  sd <- data$summarystat
  samples <- data.frame(sample=rnorm(100, mean,sd))
  samples$type = data$type
  samples$parameter = data$name
  samples$num <- 1:nrow(samples)
  return(samples)
}

list <- split(riskfunctioninput, seq(nrow(riskfunctioninput)))
samples <- lapply(list, sample) %>% do.call(rbind,.) %>% pivot_wider(names_from=parameter, 
                                                                     values_from=sample)
# adding PE for prior and posteriors 
PE <- riskfunctioninput %>% dplyr::select(type, name, mean) %>% 
  pivot_wider(names_from=name, values_from=mean) %>% 
  mutate(num=1,
         type=paste0(type,"PE")) %>% 
  dplyr::select(type,num,BETA_MALE_MORTALITY:BETA_HEPATITIS)

samples <- rbind(samples, PE)

source("SIMAH_code/microsim/2_run_microsimulation/1_functions/cirrhosis_functions.R")

alcgpd <- seq(from=0, to=150, by=0.5)
sex <- c("m","f")
types <- unique(samples$type)
newdata <- expand.grid(microsim.init.alc.gpd=alcgpd, 
                        microsim.init.sex=sex, type=types)
newdata <- left_join(newdata, samples) %>% 
  mutate_at(vars(BETA_MALE_MORTALITY:BETA_HEPATITIS), as.numeric)

newdata$RRHeavy <- ifelse(newdata$microsim.init.sex=="m",
                          exp(newdata$BETA_MALE_MORTALITY*newdata$microsim.init.alc.gpd),
                          ifelse(newdata$microsim.init.sex=="f",
                                 exp(newdata$BETA_FEMALE_MORTALITY*newdata$microsim.init.alc.gpd), NA))
newdata$RRHeavy <- ifelse(newdata$RRHeavy>100, 100, newdata$RRHeavy)
newdata$RRMetabolic = ifelse(newdata$microsim.init.sex=="m",
                             (newdata$METABOLIC_BETA1_MALE*((((newdata$microsim.init.alc.gpd+2)/1000)^-.5)-9.537024026) +
                                newdata$METABOLIC_BETA2_MALE*((((newdata$microsim.init.alc.gpd+2)/1000)^-.5)*
                                                        log((newdata$microsim.init.alc.gpd+2)/1000)+43.0154401)),
                             ifelse(newdata$microsim.init.sex=="f",
                                    (newdata$METABOLIC_BETA1_FEMALE*((newdata$microsim.init.alc.gpd+2)/100)^3-.0000696286 +
                                       newdata$METABOLIC_BETA2_FEMALE*((newdata$microsim.init.alc.gpd+2)/100)^3*
                                       log((newdata$microsim.init.alc.gpd+2)/100)+.0002221693),NA))
newdata$RRMetabolic <- ifelse(newdata$RRMetabolic<0, 0, newdata$RRMetabolic)
newdata$RRMetabolic <- exp(newdata$RRMetabolic)
newdata$RRMetabolic <- ifelse(newdata$RRMetabolic>100, 100, newdata$RRMetabolic)

newdata$RRHep <- exp(newdata$microsim.init.alc.gpd*newdata$BETA_HEPATITIS)

data <- newdata %>% group_by(microsim.init.alc.gpd, microsim.init.sex, type) %>% 
  summarise(max_RRHeavy = max(RRHeavy),
            min_RRHeavy = min(RRHeavy),
            max_RRMet = max(RRMetabolic),
            min_RRMet = min(RRMetabolic),
            max_RRHep = max(RRHep),
            min_RRHep = min(RRHep)) %>% 
  mutate(microsim.init.sex = ifelse(microsim.init.sex=="m","Men","Women")) %>% 
  pivot_longer(max_RRHeavy:min_RRHep) %>% 
  dplyr::select(microsim.init.alc.gpd, microsim.init.sex, type,
                name, value) %>% 
  separate(name, into=c("minormax","pathway")) %>% 
  pivot_wider(names_from=minormax, values_from=value) %>% 
  mutate(pathway = ifelse(pathway =="RRHeavy","Heavy alcohol use",
                          ifelse(pathway=="RRMet", "Metabolic interaction",
                                 "Hepatitis")))

PE <- data %>% filter(type=="priorPE" | type=="age specificPE" | 
                        type=="age standardizedPE") %>% 
  mutate(type=ifelse(type=="priorPE","prior",
                     ifelse(type=="age specificPE","age specific",
                            ifelse(type=="age standardizedPE","age standardized",NA)))) %>% 
  mutate(PE = max) %>% 
  dplyr::select(microsim.init.alc.gpd, microsim.init.sex, type, pathway, PE)

data <- data %>% filter(type!="priorPE") %>% filter(type!="age specificPE") %>% 
  filter(type!="age standardizedPE")

data <- left_join(data, PE) %>% 
  mutate(type = ifelse(type=="prior", "Prior belief",
                       ifelse(type=="age standardized","Posterior belief (age-standardized)",
                              "Posterior belief (age-specific)")),
    type=factor(type, levels=c("Prior belief","Posterior belief (age-standardized)",
                               "Posterior belief (age-specific)")),
         pathway = factor(pathway, levels=c("Heavy alcohol use","Metabolic interaction", "Hepatitis")))

men <- ggplot(data=subset(data,microsim.init.sex=="Men"), aes(x=microsim.init.alc.gpd)) + 
  geom_line(aes(x=microsim.init.alc.gpd, y=PE)) + 
  geom_ribbon(aes(ymin=min, ymax=max),fill='grey20', colour=NA, alpha=0.2) +
  facet_grid(cols=vars(type), rows=vars(pathway), scales="free") +
  theme_bw() +
  theme(legend.position="none") + 
  # geom_hline(yintercept=1, linetype="dashed") + 
  xlab("grams of alcohol per day") + theme(text = element_text(size=15)) + ggtitle("Men") +
  ylab("Relative Risk (RR)")

men

women <- ggplot(data=subset(data,microsim.init.sex=="Women"), aes(x=microsim.init.alc.gpd)) + 
  geom_line(aes(x=microsim.init.alc.gpd, y=PE)) + 
  geom_ribbon(aes(ymin=min, ymax=max),fill='grey20', colour=NA, alpha=0.2) + 
  facet_grid(cols=vars(type), rows=vars(pathway), scales="free") +
  theme_bw() +
  theme(legend.position="none") + 
  # geom_hline(yintercept=1, linetype="dashed") + 
  xlab("grams of alcohol per day") + theme(text = element_text(size=15)) + ggtitle("Women") +
  ylab("Relative Risk (RR)")
women

library(gridExtra)
combined <- grid.arrange(men,women)

ggsave("SIMAH_workplace/microsim/2_output_data/publication/riskfunctions.png", combined, dpi=500, width=23, height=31, units="cm")


# for paper -split these into the three pathways 
priors <- data %>% filter(type=="Prior belief")

HeavyUse <- priors %>% filter(pathway=="Heavy alcohol use")
HeavyUsePlot <- ggplot(data=HeavyUse, aes(x=microsim.init.alc.gpd)) + 
  geom_line(aes(x=microsim.init.alc.gpd, y=PE)) + 
  geom_ribbon(aes(ymin=min, ymax=max),fill='grey20', colour=NA, alpha=0.2) + 
  facet_grid(cols=vars(microsim.init.sex), scales="free") +
  theme_bw() +
  theme(legend.position="none",
        strip.background=element_rect(fill="white")) + 
  # geom_hline(yintercept=1, linetype="dashed") + 
  xlab("grams of alcohol per day") + theme(text = element_text(size=24)) + ggtitle("Pathway 1: If lifetime alcohol consumption > threshold (100kg)") +
  ylab("Relative Risk (RR)")
HeavyUsePlot
ggsave("SIMAH_workplace/microsim/2_output_data/publication/HeavyUseRisk.png", HeavyUsePlot, dpi=500, width=33, height=19, units="cm")

Metabolic <- priors %>% filter(pathway=="Metabolic interaction") %>% 
  mutate(max = ifelse(microsim.init.sex=="Men" & microsim.init.alc.gpd<=9,1.002860,max))

MetabolicPlot <- ggplot(data=Metabolic, aes(x=microsim.init.alc.gpd)) + 
  geom_line(aes(x=microsim.init.alc.gpd, y=PE)) + 
  geom_ribbon(aes(ymin=min, ymax=max),fill='grey20', colour=NA, alpha=0.2) + 
  facet_grid(cols=vars(microsim.init.sex), scales="free") +
  theme_bw() +
  theme(legend.position="none",
        strip.background=element_rect(fill="white")) + 
  # geom_hline(yintercept=1, linetype="dashed") + 
  xlab("grams of alcohol per day") + theme(text = element_text(size=24)) + ggtitle("Pathway 2: If BMI >= 30") +
  ylab("Relative Risk (RR)")
MetabolicPlot
ggsave("SIMAH_workplace/microsim/2_output_data/publication/MetabolicRisk.png", MetabolicPlot, dpi=500, width=33, height=19, units="cm")

Hepatitis <- priors %>% filter(pathway=="Hepatitis")

HepatitisPlot <- ggplot(data=Hepatitis, aes(x=microsim.init.alc.gpd)) + 
  geom_line(aes(x=microsim.init.alc.gpd, y=PE)) + 
  geom_ribbon(aes(ymin=min, ymax=max),fill='grey20', colour=NA, alpha=0.2) + 
  facet_grid(scales="free") +
  theme_bw() +
  theme(legend.position="none",
        strip.background=element_rect(fill="white")) + 
  # geom_hline(yintercept=1, linetype="dashed") + 
  xlab("grams of alcohol per day") + theme(text = element_text(size=24)) + ggtitle("Pathway 3: If chronic HBV or HCV") +
  ylab("Relative Risk (RR)")
HepatitisPlot
ggsave("SIMAH_workplace/microsim/2_output_data/publication/HepatitisRisk.png", HepatitisPlot, dpi=500, width=33, height=19, units="cm")

# posterior plot for presentation 
posteriors <- data %>% 
  filter(type=="Posterior belief (age-standardized)" | type=="Prior belief") %>% 
  mutate(max = ifelse(type=="Prior belief" & microsim.init.sex=="Men" & pathway=="Metabolic interaction" & 
                        microsim.init.alc.gpd<=9,1.002860,max))

PosteriorPlot <- ggplot(data=subset(posteriors,pathway=="Heavy alcohol use"), aes(x=microsim.init.alc.gpd)) + 
  geom_line(aes(x=microsim.init.alc.gpd, y=PE)) + 
  geom_ribbon(aes(ymin=min, ymax=max),fill='grey20', colour=NA, alpha=0.2) + 
  facet_grid(cols=vars(type), rows=vars(microsim.init.sex), scales="free") +
  theme_bw() +
  theme(legend.position="none",
        strip.background=element_rect(fill="white")) + 
  # geom_hline(yintercept=1, linetype="dashed") + 
  xlab("grams of alcohol per day") + theme(text = element_text(size=24)) + 
  ylab("Relative Risk (RR)")
PosteriorPlot
ggsave("SIMAH_workplace/microsim/2_output_data/publication/PosteriorPlot.png", PosteriorPlot, dpi=500, width=33, height=19, units="cm")

