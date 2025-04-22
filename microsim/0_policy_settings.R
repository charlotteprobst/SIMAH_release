# SIMAH project - script for setting up policy model settings

# general policy settings
policy <- 1
year_policy <- 2019
policy_int <- "price"

# define policy modelling scenarios 
# for price policites specific to beer, wine, spirits
scenarios <- cbind(policymodel = 1:5,
                   scenario = c("0,0,0",
                                "0.1,0.1,0.1",
                                "0.3,0.3,0.3",
                                "0.3,0.1,0.3",
                                "0.5,0.1,0.5")) %>% as.data.frame()

######################DO NOT EDIT BELOW HERE ##################################################

policy_setting <- read.csv(paste0(DataDirectory,"input_PricePolicy_sim.csv")) %>% 
  filter(policy %in% policy_int) %>% 
  mutate(beverage = as.factor(beverage))


