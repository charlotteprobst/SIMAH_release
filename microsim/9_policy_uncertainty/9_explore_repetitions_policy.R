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
nunc <- data %>% pull(seed) %>% unique() %>% as.data.frame() %>% 
  mutate(nunc = 1:length(.)) %>% rename("seed" = ".")
data <- data %>% dplyr::select(-nunc) %>% left_join(., nunc)

n <- max(data$nunc)
columns <- c(paste0("nrep", 1:n))
add_columns <- function(data, columns){
  new <- rep(NA_character_, length(columns))
  names(new) <- columns
  mutate(data, !!!new)
}
data <- data %>% add_columns(columns)

# select nunc by repetition
temp <- list()
i <- 2
temp[[1]] <- data %>% filter(nunc == 1) %>%
  mutate(nrep1 = "1")
while(i <= length(columns)) {
  print(i)
  temp[[i]] <- data %>% filter(nunc == i) %>% 
    rbind(., temp[[i-1]]) %>% mutate(!!paste0("nrep", i) := "1")
  i <- i+1
}

pdat <- temp[[n]] %>% as.data.frame() %>%
  pivot_longer(cols = columns, names_to = "nrep", values_to = "n") %>% 
  filter(!is.na(n)) %>% 
  mutate(nrep = as.numeric(str_sub(nrep, 5, str_length(nrep))))
table(pdat$nunc, pdat$nrep)

pdat2 <- pdat %>% group_by(nrep, year, sex, education) %>%
  summarise(meangpd = mean(meansimulation)) %>% 
  mutate(nrep = as.factor(nrep),
         sex = factor(sex, levels = c("m", "f"), labels = c("Men", "Women")),
         education = factor(education, levels = c("LEHS", "SomeC", "College"),
                            labels = c("High school or less", "Some college", "College")))

# plot 
ggplot(data = pdat2, aes(x = year, y = meangpd, colour = nrep)) + 
  geom_line() +
  facet_grid(cols = vars(education), rows = vars(sex), scales = "free_y") + 
  ylab("Mean grams of alcohol per day") +
  scale_colour_viridis_d(direction = -1) + ggtheme + xlab("") + 
  ggtitle("Change in mean GPD across multiple simulation runs")
ggsave(paste0("policy_test_runs/NRep Simulation runs/", Sys.Date, "_ChangeMeanGPDByNUNC.png"), height = 10, width = 15)

ggplot(data = pdat2[pdat2$sex == "Men" & pdat2$education == "LEHS",], 
       aes(x = year, y = meangpd, colour = nrep)) + 
  geom_line() +
  ylab("Mean grams of alcohol per day") +
  scale_colour_viridis_d(direction = -1) + ggtheme + xlab("") + 
  ggtitle("Change in mean GPD across multiple simulation runs - Men/LEHS")

# deviation by repetition
nrepmax <- max(as.numeric(pdat2$nrep))
nrep <- pdat2 %>% ungroup() %>% filter(nrep == nrepmax) %>% 
  rename("meangpdMAX" = "meangpd") %>% dplyr::select(-nrep)
pdat.dev <- pdat2 %>% ungroup() %>% filter(nrep != nrepmax) %>% 
  left_join(., nrep) %>% 
  mutate(diff = meangpdMAX - meangpd)

ggplot(data = pdat.dev[pdat.dev$year == 2019,], 
       aes(x = as.numeric(nrep), y = diff)) + geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") + xlim(c(0, 20)) +
  facet_grid(cols = vars(education), rows = vars(sex), scales = "free_y") + 
  ylab("Deviation in grams of pure alcohol per day") +
  xlab("Number of simulation runs") + 
  ggtheme + ggtitle("")
ggsave(paste0("policy_test_runs/NRep Simulation runs/", Sys.Date(), "_DeviationMeanGPDByNRep_2019.png"), height = 10, width = 15)
