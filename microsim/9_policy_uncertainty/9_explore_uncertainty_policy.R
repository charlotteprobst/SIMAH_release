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
library(viridis)

# load data file
WorkingDirectory <- "/Users/carolinkilian/Desktop/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/")
setwd(paste(DataDirectory))

data <- readRDS("2024-11-14/output-policy_percentreduction_2024-11-14.rds") %>% 
  filter(setting == "standard") 

select <- data %>% pull(ID) %>% unique() %>% sample(., 0.01*length(.))
sdat <- data %>% filter(ID %in% select)

# set ggplot layout
ggtheme <- theme_bw() + theme(legend.position="right",
                              strip.background = element_rect(fill="white"),
                              panel.spacing = unit(1,"lines"),
                              text = element_text(size=14))

# Descriptive exploration of policy effect estimate 

pdat <- sdat %>% pivot_longer(cols = c("beergpd", "winegpd", "liqgpd",
                                       "beer_percentreduction", "wine_percentreduction", "liq_percentreduction"),
                              names_to = "var", values_to = "value") %>%
  mutate(beverage = factor(ifelse(var %like% "beer", "beer",
                                  ifelse(var %like% "wine", "wine",
                                         ifelse(var %like% "liq", "liqor", NA))),
                           levels = c("beer", "wine", "liqor")),
         out = ifelse(var %like% "gpd", "gpd",
                      ifelse(var %like% "percentreduction", "percentreduction", NA))) %>% dplyr::select(-var) %>%
  pivot_wider(id_cols = c(ID, nunc, beverage), names_from = "out", values_from = "value")

# median, min, max
pdat %>% group_by(beverage) %>%
  summarise(median = median(percentreduction),
            min = min(percentreduction), # some elasticities are < -1
            max = max(percentreduction)) # just a few elasticities are > 0

# distribution percent reduction
hist(pdat[pdat$beverage == "beer",]$percentreduction)
hist(pdat[pdat$beverage == "wine",]$percentreduction)
hist(pdat[pdat$beverage == "liqor",]$percentreduction)

# distribution of policy effect estimate by alc consumption across simulation runs
ggplot(pdat) + geom_point(aes(x = gpd, y = percentreduction, color = ID)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_hline(yintercept = -1, color = "#77131D", linetype = "dashed") +
  scale_color_viridis(discrete = F) + scale_x_continuous(trans='log10') + 
  facet_grid(rows = vars(beverage)) + guides(color="none") +
  xlab("Grams of pure alcohol per day (log-scale)") +
  ylab("Individual-level policy effect estimates") + 
  ggtheme + ggtitle("Scatterplot alcohol consumption level and individual-level policy effect estimates")
ggsave(paste0("policy_test_runs/NRep Simulation runs/", Sys.Date(), "_PercentRedGPD_.01_2001.png"), height = 15, width = 10)

# association between alc consumption and policy effect estimate
pdat2 <- pdat %>% group_by(ID, beverage) %>% 
  summarise(pr_mean = quantile(percentreduction, 0.5),
            pr_lci = quantile(percentreduction, 0.025),
            pr_uci = quantile(percentreduction, 0.975),
            gpd = mean(gpd))

ggplot(data = pdat2, aes(x = gpd)) + 
  geom_errorbar(aes(ymin = pr_lci, ymax = pr_uci), color = "grey") +
  geom_point(aes(y = pr_mean)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_hline(yintercept = -1, color = "#77131D", linetype = "dashed") +
  scale_x_continuous(trans='log10') + facet_grid(rows = vars(beverage)) + 
  xlab("Grams of pure alcohol per day (log-scale)") +
  ylab("Individual-level policy effect estimates") + 
  ggtheme + ggtitle("Association between alcohol consumption level and individual-level policy effect estimate")
ggsave(paste0("policy_test_runs/NRep Simulation runs/", Sys.Date(), "_UncertaintyPolicyEffect_.01_2001.png"), height = 15, width = 10)

# Population mean of policy effect across model runs

# generate replication columns
n <- max(sdat$nunc)
columns <- c(paste0("nrep", 1:n))
add_columns <- function(data, columns){
  new <- rep(NA_character_, length(columns))
  names(new) <- columns
  mutate(data, !!!new)
}
sdat <- sdat %>% add_columns(columns)

# select nunc by repetition
temp <- list()
i <- 2
temp[[1]] <- sdat %>% filter(nunc == 1) %>%
  mutate(nrep1 = "1")
while(i <= length(columns)) {
  print(i)
  temp[[i]] <- sdat %>% filter(nunc == i) %>% 
    rbind(., temp[[i-1]]) %>% mutate(!!paste0("nrep", i) := "1")
  i <- i+1
}

# mean across repetitions
pdat3 <- temp[[n]] %>% as.data.frame() %>%
  pivot_longer(cols = columns, names_to = "nrep", values_to = "n") %>% 
  filter(!is.na(n)) %>% 
  mutate(nrep = as.numeric(str_sub(nrep, 5, str_length(nrep))))
table(pdat3$nunc, pdat3$nrep)

pdat3 <- pdat3 %>% group_by(nrep) %>%
  summarise(mean_pr_beer = mean(beer_percentreduction),
            mean_pr_wine = mean(wine_percentreduction),
            mean_pr_liq = mean(liq_percentreduction)) %>% 
  pivot_longer(cols = c("mean_pr_beer", "mean_pr_wine", "mean_pr_liq"),
               names_to = "beverage", values_to = "mean_pr") %>% 
  mutate(beverage = factor(str_sub(beverage, 9, str_length(beverage)), 
                           levels = c("beer", "wine", "liq"),
                           labels = c("Beer", "Wine", "Spirits")),
         pr = ifelse(beverage == "Beer", -0.52,
                     ifelse(beverage == "Wine", -0.55,
                            ifelse(beverage == "Spirits", -0.6, NA))))

# plot 
ggplot(data = pdat3, aes(x = nrep)) + 
  geom_line(aes(y = mean_pr)) + geom_hline(aes(yintercept = pr), linetype = "dashed") +
  facet_grid(rows = vars(beverage)) + 
  ylab("Mean of own-price consumption elasticity") + xlab("Number of simulation runs") + 
  ggtheme + ggtitle("")
ggsave(paste0("policy_test_runs/NRep Simulation runs/", Sys.Date(), "_UncertaintyPolicyEffect_ChangeMeanByNRep_.01_2001.png"), height = 10, width = 15)
