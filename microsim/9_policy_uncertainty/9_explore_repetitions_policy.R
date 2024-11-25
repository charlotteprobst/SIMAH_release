#' Summarise mean alcohol consumption by policy setting and simulation run
#'
#' @param
#' @keywords alcohol postprocessing exploration
#' @export
#' @examples

# libraries 
library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(ggthemes)

options(digits = 4)

# load data file
WorkingDirectory <- "/Users/carolinkilian/Desktop/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/")
setwd(paste(DataDirectory))

# select one model version randomly 
# select <- sample(1:4, 1)
select <- 4  
data <- read.csv("2024-11-18/output-policy_alcohol_20rep_2024-11-18.csv") %>%
  filter(policymodel == select)

# set ggplot layout
#options(digits = 4)
  
#c4 <- c("#A9D8B6", "#487B79", "#1F4328", "#1482AB")
#c6 <- c("#FFD679", "#BFBFBF", "#A9D8B6", "#487B79", "#1F4328", "#1482AB")
  
ggtheme <- theme_bw() + theme(legend.position="right",
                              strip.background = element_rect(fill="white"),
                              panel.spacing = unit(1,"lines"),
                              text = element_text(size=20))
  
# MEAN BY MODEL REPETITIONS

# generate replication columns
columns <- c("nrep1", "nrep2", "nrep3", "nrep4", "nrep5",
             "nrep6", "nrep7", "nrep8", "nrep9", "nrep10",
             "nrep11", "nrep12", "nrep13", "nrep14", "nrep15",
             "nrep16", "nrep17", "nrep18", "nrep19", "nrep20")
add_columns <- function(data, columns){
  new <- rep(NA_character_, length(columns))
  names(new) <- columns
  mutate(data, !!!new)
}
data <- data %>% add_columns(columns)

# select seeds by repetition
pdat1 <- data %>% filter(seed == (sample(unique(data$seed), 1))) %>%
  mutate(nrep1 = "1") 
pdat2 <- data %>% filter(seed == (sample(unique(data[!data$seed %in% unique(pdat1$seed), ]$seed), 1))) %>%
  rbind(., pdat1) %>% mutate(nrep2 = "1") 
pdat3 <- data %>% filter(seed == (sample(unique(data[!data$seed %in% unique(pdat2$seed), ]$seed), 1))) %>%
  rbind(., pdat2) %>% mutate(nrep3 = "1") 
pdat4 <- data %>% filter(seed == (sample(unique(data[!data$seed %in% unique(pdat3$seed), ]$seed), 1))) %>%
  rbind(., pdat3) %>% mutate(nrep4 = "1") 
pdat5 <- data %>% filter(seed == (sample(unique(data[!data$seed %in% unique(pdat4$seed), ]$seed), 1))) %>%
  rbind(., pdat4) %>% mutate(nrep5 = "1") 
pdat6 <- data %>% filter(seed == (sample(unique(data[!data$seed %in% unique(pdat5$seed), ]$seed), 1))) %>%
  rbind(., pdat5) %>% mutate(nrep6 = "1") 
pdat7 <- data %>% filter(seed == (sample(unique(data[!data$seed %in% unique(pdat6$seed), ]$seed), 1))) %>%
  rbind(., pdat6) %>% mutate(nrep7 = "1") 
pdat8 <- data %>% filter(seed == (sample(unique(data[!data$seed %in% unique(pdat7$seed), ]$seed), 1))) %>%
  rbind(., pdat7) %>% mutate(nrep8 = "1") 
pdat9 <- data %>% filter(seed == (sample(unique(data[!data$seed %in% unique(pdat8$seed), ]$seed), 1))) %>%
  rbind(., pdat8) %>% mutate(nrep9 = "1") 
pdat10 <- data %>% filter(seed == (sample(unique(data[!data$seed %in% unique(pdat9$seed), ]$seed), 1))) %>%
  rbind(., pdat9) %>% mutate(nrep10 = "1") 
pdat11 <- data %>% filter(seed == (sample(unique(data[!data$seed %in% unique(pdat10$seed), ]$seed), 1))) %>%
  rbind(., pdat10) %>% mutate(nrep11 = "1") 
pdat12 <- data %>% filter(seed == (sample(unique(data[!data$seed %in% unique(pdat11$seed), ]$seed), 1))) %>%
  rbind(., pdat11) %>% mutate(nrep12 = "1") 
pdat13 <- data %>% filter(seed == (sample(unique(data[!data$seed %in% unique(pdat12$seed), ]$seed), 1))) %>%
  rbind(., pdat12) %>% mutate(nrep13 = "1") 
pdat14 <- data %>% filter(seed == (sample(unique(data[!data$seed %in% unique(pdat13$seed), ]$seed), 1))) %>%
  rbind(., pdat13) %>% mutate(nrep14 = "1") 
pdat15 <- data %>% filter(seed == (sample(unique(data[!data$seed %in% unique(pdat14$seed), ]$seed), 1))) %>%
  rbind(., pdat14) %>% mutate(nrep15 = "1") 
pdat16 <- data %>% filter(seed == (sample(unique(data[!data$seed %in% unique(pdat15$seed), ]$seed), 1))) %>%
  rbind(., pdat15) %>% mutate(nrep16 = "1") 
pdat17 <- data %>% filter(seed == (sample(unique(data[!data$seed %in% unique(pdat16$seed), ]$seed), 1))) %>%
  rbind(., pdat16) %>% mutate(nrep17 = "1") 
pdat18 <- data %>% filter(seed == (sample(unique(data[!data$seed %in% unique(pdat17$seed), ]$seed), 1))) %>%
  rbind(., pdat17) %>% mutate(nrep18 = "1") 
pdat19 <- data %>% filter(seed == (sample(unique(data[!data$seed %in% unique(pdat18$seed), ]$seed), 1))) %>%
  rbind(., pdat18) %>% mutate(nrep19 = "1") 

# mean across repetitions
pdat <- pdat19 %>% 
  pivot_longer(cols = columns, names_to = "nrep", values_to = "n") %>% 
  filter(!is.na(n)) 
table(pdat$seed, pdat$nrep)

pdat <- pdat %>% group_by(nrep, year, sex, education) %>%
  summarise(meangpd = mean(meansimulation)) %>% 
  mutate(nrep = factor(nrep, levels = c("nrep1", "nrep2", "nrep3", "nrep4", "nrep5",
                                        "nrep6", "nrep7", "nrep8", "nrep9", "nrep10",
                                        "nrep11", "nrep12", "nrep13", "nrep14", "nrep15",
                                        "nrep16", "nrep17", "nrep18", "nrep19", "nrep20")),
         sex = factor(sex, levels = c("m", "f"), labels = c("Men", "Women")),
         education = factor(education, levels = c("LEHS", "SomeC", "College")))

# plot 
ggplot(data = pdat, aes(x = year, y = meangpd, colour = nrep)) + 
  geom_line() +
  facet_grid(cols = vars(education), rows = vars(sex), scales = "free_y") + 
  ylab("Mean grams of alcohol per day") +
  scale_colour_viridis_d(direction = -1) + ggtheme + xlab("") + 
  ggtitle("Change in mean GPD across multiple simulation runs")
ggsave(paste0("policy_test_runs/NRep Simulation runs/", Sys.Date, "_ChangeMeanGPDByNRep.png"), height = 10, width = 15)

ggplot(data = pdat[pdat$sex == "Men" & pdat$education == "LEHS",], 
       aes(x = year, y = meangpd, colour = nrep)) + 
  geom_line() +
  ylab("Mean grams of alcohol per day") +
  scale_colour_viridis_d(direction = -1) + ggtheme + xlab("") + 
  ggtitle("Change in mean GPD across multiple simulation runs - Men/LEHS")

# deviation by repetition

nrep19 <- pdat %>% ungroup() %>% filter(nrep == "nrep19") %>% 
  rename("meangpd19" = "meangpd") %>% dplyr::select(-nrep)
pdat.dev <- pdat %>% filter(nrep != "nrep19") %>% 
  left_join(., nrep19) %>% 
  mutate(diff = meangpd19 - meangpd,
         nrep = as.numeric(gsub("nrep", "", nrep)))

ggplot(data = pdat.dev[pdat.dev$year == 2019,], 
       aes(x = nrep, y = diff)) + geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(cols = vars(education), rows = vars(sex), scales = "free_y") + 
  ylab("Deviation in grams of alcohol per day") +
  xlab("Number of simulation runs") + 
  scale_colour_viridis_d(direction = -1) + ggtheme + 
  ggtitle("Deviation in mean GPD by simulation run in 2019")
ggsave(paste0("policy_test_runs/NRep Simulation runs/", Sys.Date(), "_DeviationMeanGPDByNRep_2019.png"), height = 10, width = 15)
